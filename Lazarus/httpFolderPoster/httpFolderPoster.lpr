program httpFolderPoster;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, abZipper, abUnzper, abZipTyp, IdHTTP,
  IdHeaderList, IdZLibCompressorBase, IdCompressorZLib, IdZLibHeaders,
  indycorelaz, indyprotocolslaz, indysystemlaz;
var
  doSkipExisting: Boolean;
  doContinueOnErrors: Boolean;
  doNotCompress: Boolean;
  fileNamePostfix: AnsiString;
  soapAction: String;

procedure StringToStream(const Str: AnsiString; const Stm: Classes.TStream);
var
  SS: Classes.TStringStream;  // used to copy AnsiString to stream
begin
  // Create stream onto string and copy it to given stream
  SS := Classes.TStringStream.Create(Str);
  try
    Stm.CopyFrom(SS, Length(Str));
  finally
    SS.Free;
  end;
end;

function ReadStringFromFile(aFileName: AnsiString): AnsiString;
begin
  with TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';  // Deallocates memory
      Free;
      raise;
    end;
    Free;
  end;
end;

function SendHttpMessage(aMessage, aEndPoint: String): String;
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  xResponse: String;
begin
  Result := '';
  HttpClient := TIdHTTP.Create;
  try
    HttpClient.Compressor := TidCompressorZLib.Create (nil);
    HttpRequest := TStringStream.Create ('');
    try
      try
        HttpClient.Request.CustomHeaders.Values ['SOAPAction'] := '"' + soapAction + '"';
      except
      end;
      HttpClient.Request.ContentType := 'text/xml';
      HttpClient.Request.CharSet := '';
      HttpRequest.WriteString (aMessage);
      HttpClient.ProxyParams.ProxyServer := '';
      HttpClient.ProxyParams.ProxyPort := 0;
      try
        result := HttpClient.Post(aEndPoint, HttpRequest);
      except
        on e: EIdHTTPProtocolException do
        begin
          result := e.ErrorMessage;
        end;
      end;
      if HttpClient.Connected then {in case server s-alive}
        HttpClient.Disconnect;
    finally
      FreeAndNil (HttpRequest);
    end;
  finally
    if Assigned (HttpClient.IOHandler) then
    begin
      HttpClient.IOHandler.Free;
      HttpClient.IOHandler := nil;
    end;
    if Assigned (HttpClient.Compressor) then
    begin
      HttpClient.Compressor.Free;
      HttpClient.Compressor := nil;
    end;
    FreeAndNil (HttpClient);
  end;
end;


procedure StringToFile(const Str, FileName: AnsiString);
var
  FS: Classes.TFileStream;  // stream used to write file
begin
  writeln ('writing: ' + FileName);
  // Create stream onto file and write to it
  FS := Classes.TFileStream.Create(FileName, Classes.fmCreate);
  try
    StringToStream(Str, FS);
  finally
    FS.Free;
  end;
end;

procedure postTxtFile (aInputFile, aOutputFile, aHttpEndPoint: AnsiString);
var
  xResultString: String;
begin
  writeln ('posting: ' + aInputFile);
  xResultString := SendHttpMessage (ReadStringFromFile (aInputFile), aHttpEndPoint);
  StringToFile(xResultString, aOutputFile);
  xResultString := '';
end;

procedure postFile (aFileName, aOutputFolder, aHttpEndPoint: AnsiString);
  function outName: String;
  var
    xExt, xName: String;
  begin
    xExt := ExtractFileExt(aFileName);
    xName := ExtractFileName(aFileName);
    xName := Copy (xName, 1, Length (xName) -  Length (xExt));
    xName := aOutputFolder
           + '\'
           + xName
           + fileNamePostfix
           + xExt
           ;
    result := ExpandFileName(xName);
  end;
var
  xOutputFileName: AnsiString;
begin
  xOutputFileName := outName;
  if doSkipExisting
  and FileExists(xOutputFileName) then
  begin
    writeln ('skipped: ' + aFileName);
    Exit;
  end;
  try
    postTxtFile (aFileName, xOutputFileName, aHttpEndPoint);
  except
    on e: exception do
      if doContinueOnErrors then
      begin
        StringToFile ( 'Exception in ' + aFileName + #10#13 + e.Message
                     , xOutputFileName + 'Error'
                     );
        WriteLn ( 'Exception: '
                +  e.Message
                );
      end
      else
        raise;
  end;
end;


procedure postFiles (aInputFolder, aOutputFolder, aHttpEndPoint: AnsiString);
var
  sccs: Integer;
  xSearchRec: TSearchRec;
begin
  sccs := FindFirst(aInputFolder + '\*.*', 0, xSearchRec);
  try
    while sccs = 0 do
    begin
      postFile( ExpandFileName (aInputFolder)
              + '\'
              + xSearchRec.Name
              , aOutputFolder
              , aHttpEndPoint
              );
      sccs := FindNext(xSearchRec);
    end;
  finally
    FindClose(xSearchRec);
  end;
end;


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
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hcs','');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help')
  or (ParamCount < 4) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  { add your program here }
  try
    doSkipExisting := HasOption('s', 'skipexisting');
    doContinueOnErrors := HasOption('c', 'continue');
    if not DirectoryExists(ParamStr(1)) then
      raise Exception.Create('Inputfolder does not exist: ' + ParamStr(1));
    if not DirectoryExists(ParamStr(2)) then
      raise Exception.Create('Outputfolder does not exist: ' + ParamStr(2));
    soapAction := ParamStr(4);
    postFiles (ParamStr(1), ParamStr(2), ParamStr(3));
  except
    on e: exception do
    begin
      WriteLn ( 'Exception: '
              +  e.Message
              );
      Terminate;
      Exit;
    end;
  end;
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
  { add your help code here }
  WriteLn (ExeName);
  WriteLn ('httpFolderPoster inputFolder outputFolder httpEndPoint SOAPAction [switches]');
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
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

