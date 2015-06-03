program wsdlServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

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
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  WriteLn (ExeName);
  WriteLn (ApplicationName, ' inputFolder outputFolder httpEndPoint SOAPAction [switches]');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example');
  WriteLn ('  httpFolderPoster "c:\data\Requests" "c:\data\Responses" "http://localhost:6060" "myServer"');
  WriteLn ('');
  WriteLn ('This command will scan the folder c:\data\Requests for files');
  WriteLn ('and post (http command) the content of each file to a server');
  WriteLn ('listening on http://localhost:6060 with myServer in the HTTP header');
  WriteLn ('The server responses will be written to the folder c:\data\Responses');
  WriteLn ('');
  WriteLn ('Switches');
  WriteLn ('  -c continues with next input files in case of errors.');
  WriteLn ('     writes the error to an output file with ''Error'' appended to its name');
  WriteLn ('  -s skips input files for which an output file already exists.');
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

