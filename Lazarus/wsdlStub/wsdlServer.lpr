program wsdlServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  , WsdlProjectz , lazrichedit , virtualtreeview_package ;

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
  public
    se: TWsdlProject;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  x: Integer;
begin
  if ParamCount = 0 then
  begin
    WriteLn(ExeName, ' --help for more information');
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

  { add your program here }
  for x := 1 to ParamCount do
    WriteLn (ParamStr(x));

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  se := TWsdlProject.Create;
end;

destructor TMyApplication.Destroy;
begin
  FreeAndNil(se);
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  WriteLn (ExeName);
  WriteLn (ApplicationName, ' projectFileName [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn (ExeName, '  myProject.wsdlStub --port=3738');
  WriteLn ('');
  WriteLn ('This command will ...');
  WriteLn ('');
  WriteLn ('Switches');
  WriteLn ('  --port=');
  WriteLn ('     writes the error to an output file with ''Error'' appended to its name');
  WriteLn ('  --help');
  WriteLn ('     type this helpmessage');
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

