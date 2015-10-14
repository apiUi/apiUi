unit tacoInterface;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Classes
   , Controls
   , StrUtils
   , Xmlz
   , IdTCPClient
   ;

type
  TRtReturnType = (rtUndefined, rtReply, rtError);
  TTacoInterface = class;
  TOnHaveTacoMessage = procedure ( aTacoInterface: TTacoInterface
                                 ; aClientId, aMessage: AnsiString
                                 ) of Object;

  TTacoInterface = class(TComponent)
  private
    fClient: TIdTCPClient;
    fHost: String;
    fPort: Integer;
    fOnHaveTacoMessage: TOnHaveTacoMessage;
    fReqTime: TDateTime;
    fRspTime: TDateTime;
    fTacoReply: AnsiString;
    fReady: Boolean;
    fReturnType: TRtReturnType;
    fReply: AnsiString;
    function tacoString (aString: AnsiString): AnsiString;
    function tacoRequest (aRequest, aUserName, aPassword: AnsiString; aConfig: TXml): AnsiString;
    function tacoCopy (aString: AnsiString; var x: Integer): AnsiString;
    procedure EvaluateResponse;
  public
    UserName, Password: AnsiString;
    property Host: String read fHost write fHost;
    property Port: Integer read fPort write fPort;
    property ReturnType: TRtReturnType read fReturnType;
    procedure doTerminate;
    procedure Connect;
    procedure Disconnect;
    constructor Create(Owner: TComponent; aOnHaveTacoMessage: TOnHaveTacoMessage);
    constructor CreateFromXml(aXml: TXml; aOnHaveTacoMessage: TOnHaveTacoMessage);
    destructor Destroy; override;
    function RequestReply ( aRequest: AnsiString
                          ; aTimeOut: Integer
                          ; aConfigAsXml: TXml
                          ): AnsiString; Virtual;
    function AsXml: TXml;
  published
    property RequestTimestamp: TDateTime read fReqTime;
    property ReplyTimestamp: TDateTime read fRspTime;
  end;

implementation

uses SysUtils
   ;
{ TTacoInterface }

function TTacoInterface.AsXml: TXml;
begin

end;

procedure TTacoInterface.Connect;
begin

end;

constructor TTacoInterface.Create(Owner: TComponent;
  aOnHaveTacoMessage: TOnHaveTacoMessage);
begin
  inherited Create(Owner);
  fOnHaveTacoMessage := aOnHaveTacoMessage;
  fClient := TIdTCPClient.Create(nil);
end;

constructor TTacoInterface.CreateFromXml(aXml: TXml;
  aOnHaveTacoMessage: TOnHaveTacoMessage);
begin

end;

destructor TTacoInterface.Destroy;
begin
  fClient.Disconnect;
  fClient.Free;
  inherited;
end;

procedure TTacoInterface.Disconnect;
begin

end;

procedure TTacoInterface.doTerminate;
begin

end;


procedure TTacoInterface.EvaluateResponse;
var
  x: Integer;
begin
  fReply := '';
  if AnsiStartsStr ('<REPLY>', fTacoReply) then
  begin
    fReturnType := rtReply;
    x := 8;
    fReply := tacoCopy(fTacoReply, x);
    Exit;
  end;
  if AnsiStartsStr ('<ERR>', fTacoReply) then
  begin
    fReturnType := rtError;
    x := 6;
    fReply := tacoCopy(fTacoReply, x);
    raise Exception.Create('Taco server error: ' + fReply);
  end;
  raise Exception.Create('tacoInterface: Illegal Taco response: ' + fTacoReply);
end;

function TTacoInterface.RequestReply(aRequest: AnsiString; aTimeOut: Integer;
  aConfigAsXml: TXml): AnsiString;
var
  xHost: String;
  xPort: Integer;
begin
  fReturnType := rtUndefined;
  xHost := aConfigAsXml.Items.XmlValueByTag['Host'];
  xPort := aConfigAsXml.Items.XmlIntegerByTag['Port'];
  if fClient.Connected
  and (   (fClient.Host <> xHost)
       or (fClient.Port <> xPort)
      ) then
    fClient.Disconnect;
  if not fClient.Connected then
  begin
    fClient.Host := xHost;
    fClient.Port := xPort;
    fClient.Connect;
  end;
  fReady := False;
  fTacoReply := '';
  fClient.IOHandler.WriteLn(tacoRequest(aRequest, UserName, Password, aConfigAsXml));
  while not fReady do
  begin
    fTacoReply := fTacoReply + fClient.IOHandler.ReadString(1);
    fReady := (system.Length(fTacoReply) > 12)
          and (Copy (fTacoReply, Length (fTacoReply) - 12, 13) = '<END-OF-DATA>')
            ;
  end;
  EvaluateResponse;
  result := fReply;
end;

function TTacoInterface.tacoCopy(aString: AnsiString;
  var x: Integer): AnsiString;
var
  L, S: Integer;
begin
  if aString[x] <> '<' then
    raise Exception.Create('tacoInterface: Illegal Taco response: ' + aString);
  L := x;
  while (x <= Length (aString))
  and (aString [x] <> '>') do
    Inc (x);
  if x > Length (aString) then
    raise Exception.Create('tacoInterface: Illegal Taco response: ' + aString);
  S := StrToInt (Copy (aString, L + 1, x - L - 1));
  result := Copy (aString, x + 1, S);
  x := x + 1 + S;
end;

function TTacoInterface.tacoRequest(aRequest, aUserName, aPassword: AnsiString; aConfig: TXml): AnsiString;
var
  xXml, mqXml: TXml;
  s: String;
  x: Integer;
begin
  result := '<USER>'
          + tacoString (aUserName)
          + tacoString (aPassword)
          ;
  xXml := aConfig.FindUQXml('Taco.messageBroker.Pathsend');
  if Assigned (xXml)
  and (xXml.Checked) then
  begin
    result := result + '<SEND-S';
    s := xXml.Items.XmlValueByTag['TMF'];
    if s = 'Commit' then
      result := result + 'T';
    if s = 'Abort' then
      result := result + 'A';
    result := result + '>';
    result := result + tacoString (xXml.Items.XmlValueByTag['Monitor'])
                     + tacoString (xXml.Items.XmlValueByTag['Server'])
                     ;
 {}
  end
  else
  begin
    xXml := aConfig.FindUQXml('Taco.messageBroker.Mq');
    if Assigned (xXml)
    and xXml.Checked then
    begin
      mqXml := nil;
      for x := 0 to xXml.Items.Count - 1 do
        if xXml.Items.XmlItems[x].Checked then
          mqXml := xXml.Items.XmlItems[x];
      if not Assigned (mqXml) then
        raise Exception.Create('tacoInterface: Illegal MQ config');
      result := result
              + '<MQSEND>'
              + tacoString (mqXml.Items.XmlValueByTag['Manager'])
              + tacoString (mqXml.Items.XmlValueByTag['GetQueue'])
              + tacoString (mqXml.Items.XmlValueByTag['PutQueue'])
              + tacoString (mqXml.Items.XmlValueByTag['ReplyToQueue'])
              + tacoString (mqXml.Items.XmlValueByTag['TimeOut'])
              ;
    end;
  end;
  result := result + tacoString (aRequest) + '<END-OF-DATA>';
end;

function TTacoInterface.tacoString(aString: AnsiString): AnsiString;
begin
  result := '<'
          + IntToStr (Length (aString))
          + '>'
          + aString
          ;
end;

end.
