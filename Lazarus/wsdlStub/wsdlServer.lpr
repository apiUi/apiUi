program wsdlServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  , WsdlProjectz
  , xmlio
  , xmlz
  , xmlUtilz
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
    function ClearLogCommand(aDoRaiseExceptions: Boolean): String;
    function ReactivateCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ActivateCommand(aActivate: Boolean);
    procedure OpenProjectCommand(aProject: String);
  public
    se: TWsdlProject;
    IniFile: TFormIniFile;
    function doDecryptString(aString: AnsiString): AnsiString;
    function doEncryptString(aString: AnsiString): AnsiString;
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

  se.projectFileName := ParamStr(1);
  if (Copy (se.projectFileName, 1, 1) = '-')  // switch as first argument ??
  or (not FileExists(se.projectFileName))
  then
  begin
    WriteLn ('First argument not a filename: ' + se.projectFileName);
    Terminate;
    Exit;
  end;

  se.ProjectDesignFromString(ReadStringFromFile(se.projectFileName), se.projectFileName);
  if HasOption('?', portOpt) then
    MasterPortNumber := StrToInt(GetOptionValue(portOpt));
  ActivateCommand(True);
  while not Terminated
    Sleep (333);
  ActivateCommand(False);
  Terminate;
end;

function TMyApplication.ClearLogCommand(aDoRaiseExceptions: Boolean): String;
begin
  raise Exception.Create('ClearLog: Not implemented in ' + ExeName);
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
  raise Exception.Create('ReloadDesign: Not implemented in ' + ExeName);
end;

procedure TMyApplication.ActivateCommand(aActivate: Boolean);
begin
  if aActivate <> se.IsActive then
  begin
    try
      se.Activate(aActivate);
      WriteLn (ExeName, ifthen (se.IsActive, ': Active' , ': Inactive'));
    except
      on e: Exception do
        raise Exception.Create('Activate: ' + e.Message);
    end;
  end;
end;

procedure TMyApplication.OpenProjectCommand(aProject: String);
begin
  raise Exception.Create('OpenProject: Not implemented in ' + ExeName);
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
  DecryptString := @doDecryptString;
  EncryptString := @doEncryptString;
  IniFile := TFormIniFile.Create;
  se := TWsdlProject.Create;
  se.OnActivateEvent := @ActivateCommand;
  se.OnOpenProjectEvent := @OpenProjectCommand;
  se.OnClearLogEvent := @ClearLogCommand;
  se.OnReactivateEvent := @ReactivateCommand;
  se.OnQuitEvent := @QuitCommand;
  se.OnRestartEvent := @RestartCommand;
  se.OnReloadDesignEvent := @ReloadDesignCommand;
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
  WriteLn ('  --port=');
  WriteLn ('     overrules the portnumber for the wsdlServer webservice');
  WriteLn ('  --help');
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

