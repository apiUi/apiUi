program wsdlServer;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  , WsdlProjectz
  , wsdlcontrolz
  , xmlio
  , xmlz
  , xmlUtilz
  , Wsdlz
  , lazrichedit
  , FormIniFilez
  , virtualtreeview_package
  ;

type
  longOptsArrayType = array [0..1] of String;

const
  helpOpt = 'help';
  portOpt = 'port';
  longOpts: longOptsArrayType = ( helpOpt
                                , portOpt + ':'
                                );
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    se: TWsdlProject;
    sc: TWsdlControl;
    IniFile: TFormIniFile;
    function doDecryptString(aString: AnsiString): AnsiString;
    function doEncryptString(aString: AnsiString): AnsiString;
    procedure Notify(aString: String);
    function ClearLogCommand(aDoRaiseExceptions: Boolean): String;
    function ReactivateCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ActivateCommand(aActivate: Boolean);
    procedure OpenProjectCommand(aProject: String);
    procedure LogServerException(const Msg: String; aException: Boolean; E: Exception);
    procedure WriteHelp; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  x: Integer;
begin
  if ParamCount = 0 then
  begin
    WriteLn(ExeName, ' --', helpOpt, ' for more information');
    Terminate;
    Exit;
  end;

  ErrorMsg := CheckOptions('',longOpts);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('?',helpOpt) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('?',portOpt) then
  begin
    WriteLn('option ', portOpt, GetOptionValue('?', portOpt));
  end;

  se.projectFileName := ParamStr(1);
  if (Copy (se.projectFileName, 1, 1) = '-')  // switch as first argument ??
  or (not FileExists(se.projectFileName))
  then
  begin
    WriteLn ('First argument not a filename: ' + se.projectFileName);
    Terminate;
    Exit;
  end;
  OpenProjectCommand(se.projectFileName);
  if HasOption('?', portOpt) then
    sc.portNumber := StrToInt(GetOptionValue(portOpt));
  sc.Active := True;
  ActivateCommand(True);
  while not Terminated do
    Sleep (333);
  ActivateCommand(False);
  Terminate;
end;

procedure TMyApplication .Notify (aString : String );
begin
  WriteLn (ExeName, ' notify: ', aString);
end;

function TMyApplication.ClearLogCommand(aDoRaiseExceptions: Boolean): String;
begin
  raise Exception.Create('ClearLog: Not implemented (not required) in ' + ExeName);
end;

function TMyApplication.ReactivateCommand: String;
begin
  raise Exception.Create('Reactivate: Not implemented in ' + ExeName);
end;

function TMyApplication.QuitCommand(aDoRaiseExceptions: Boolean): String;
begin
  WriteLn (ExeName, ' received Quit command');
  Terminate;
end;

function TMyApplication.RestartCommand: String;
begin
  raise Exception.Create('Restart: Not implemented in ' + ExeName);
end;

function TMyApplication.ReloadDesignCommand: String;
begin
  OpenProjectCommand(se.projectFileName);
end;

procedure TMyApplication.ActivateCommand(aActivate: Boolean);
begin
  if aActivate <> se.IsActive then
  begin
    try
      se.Activate(aActivate);
      Notify (ifthen (se.IsActive, 'Is Active' , 'Is Inactive'));
    except
      on e: Exception do
        raise Exception.Create('Activate: ' + e.Message);
    end;
  end;
end;

procedure TMyApplication.OpenProjectCommand(aProject: String);
var
  wasActive: Boolean;
begin
  wasActive := se.IsActive;
  ActivateCommand(False);
  se.projectFileName := aProject;
  se.ProjectDesignFromString(ReadStringFromFile(aProject), aProject);
  ActivateCommand(wasActive);
end;

procedure TMyApplication .LogServerException (const Msg : String ;
  aException : Boolean ; E : Exception );
begin
  if aException then
    WriteLn (ExeName, ' exception', LineEnding, Msg, LineEnding, se.ExceptionStackListString(nil))
  else
    Notify (Msg);
end;

function TMyApplication.doDecryptString(aString: AnsiString): AnsiString;
begin
  result := IniFile.DecryptPassword(aString);
end;

function TMyApplication.doEncryptString(aString: AnsiString): AnsiString;
begin
  result := IniFile.EncryptPassword(aString);
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  _xmlUserName := GetUserName;
  _xmlProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  _xmlProgVersion := xmlio.GetVersion;
  _xmlLicensed := True;
  DecryptString := doDecryptString;
  EncryptString := doEncryptString;
  IniFile := TFormIniFile.Create;
  se := TWsdlProject.Create;
  se.Notify := Notify;
  se.doDisplayLog := False;
  sc := TWsdlControl.Create;
  sc.OnActivateEvent := ActivateCommand;
  sc.OnClearLogEvent := ClearLogCommand;
  sc.OnOpenProjectEvent := OpenProjectCommand;
  sc.OnReactivateEvent := ReactivateCommand;
  sc.OnQuitEvent := QuitCommand;
  sc.OnRestartEvent := RestartCommand;
  sc.OnReloadDesignEvent := ReloadDesignCommand;
end;

destructor TMyApplication.Destroy;
begin
  FreeAndNil(se);
  IniFile.Free;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  WriteLn (ExeName);
  WriteLn (ApplicationName, ' projectFileName [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn (ExeName, ' myProject.wsdlStub --port=6161');
  WriteLn ('');
  WriteLn ('This command will ...');
  WriteLn ('');
  WriteLn ('Switches');
  WriteLn ('  --', portOpt, '=');
  WriteLn ('     overrules the portnumber for the wsdlServer webservice');
  WriteLn ('  --', helpOpt);
  WriteLn ('     types this helpmessage');
  WriteLn ('');
end;

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

