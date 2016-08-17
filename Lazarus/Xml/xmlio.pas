unit xmlio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function HttpPostDialog (aRequest, aUrl: String): String;
function PromptFolderName(aCaption, aStart: String): String;
function PrepareFileNameSpace(aMainFileName, aFileName: String): String;
function ReadStringFromFile (aFileName: String): String;
procedure SaveStringToFile (aFileName: String; aString: String);
function ExpandRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function ExtractRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function uncFilename (aFileName: String): String;
function GetHostName: String;
function GetUserName: String;
function GetVersion: String;
function resolveAliasses (aString: String; aAliasses: TStringList): String;
function StringHasRegExpr (aString, aExpr: String): String;

var
  PathPrefixes: TStringList;

implementation
uses StrUtils
   , LCLIntf, LCLType, LMessages
   , LazFileUtils
   , versiontypes, versionresource
   , idHTTP
   , idStack
   , LConvEncoding
   , RegExpr
   , Forms
   , Controls
   , PromptFolderUnit
   ;

function StringHasRegExpr (aString, aExpr: String): String;
begin
  result := '';
  if (aString <> '')
  and (aExpr <> '') then
  with TRegExpr.Create do
  try
    Expression := aExpr;
    if (Exec(aString)) then
      result := Match[0];
  finally
    Free;
  end;
end;

function HttpPostDialog (aRequest, aUrl: String): String;
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  xResponse: String;
  xStream: TMemoryStream;
begin
  Result := '';
  HttpClient := TIdHTTP.Create;
  try
    HttpRequest := TStringStream.Create ('');
    xStream := TMemoryStream.Create;
    try
      try
        HttpClient.Request.ContentType := 'text/xml';
        HttpClient.Request.CharSet := '';
        HttpRequest.WriteString (aRequest);
        HttpClient.ProxyParams.ProxyServer := '';
        HttpClient.ProxyParams.ProxyPort := 0;
        HttpClient.Post(aUrl, HttpRequest, xStream);
        SetLength(Result,xStream.Size);
        xStream.Position := 0;;
        xStream.Read(Pointer(Result)^,xStream.Size);
        if HttpClient.ResponseCode <> 200 then
          raise Exception.CreateFmt ( '%d: %s%s%s'
                                    , [ HttpClient.ResponseCode
                                      , HttpClient.ResponseText
                                      , LineEnding
                                      , Result
                                      ]
                                    );
      except
        on e: Exception do
          raise Exception.Create (e.Message);
      end;
    finally
      FreeAndNil (HttpRequest);
      FreeAndNil (xStream);
    end;
  finally
    if Assigned (HttpClient) then
    begin
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
    end;
    FreeAndNil (HttpClient);
  end;
end;

function PromptFolderName(aCaption, aStart: String): String;
var
  xForm: TPromptFolderForm;
begin
  Application.CreateForm(TPromptFolderForm, xForm);
  try
    xForm.Caption := aCaption;
    xForm.PromptEdit.Text := aStart;
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
      result := xForm.PromptEdit.Text
    else
      raise Exception.Create('no replcement specified');
  finally
    FreeAndNil(xForm);
  end;
end;

function PrepareFileNameSpace(aMainFileName, aFileName: String): String;
var
  xPrefix, xAlias, xSpec, xSep: String;
  x: Integer;
begin
  result := aFileName;
  with TRegExpr.Create do
  try
    Expression:= '^[A-Za-z0-9]+\:/[^/]';
    if Exec(aFileName) then
    begin
      xAlias := Copy (Match[0], 1, Length (Match[0]) - 3); // without ':/' and that other char that differs from '/'
      if Length (xAlias) > 1 then // migth be a windows driveletter
      begin
        xPrefix := PathPrefixes.Values[xAlias];
        xSpec := Copy (aFileName, Length(Match[0]), 1000);
        if xPrefix <> '' then
        begin
          result := xPrefix + xSpec;
          if not (AnsiStartsText('http://', result))
          and not (AnsiStartsText('https://', result)) then
            ForcePathDelims(result);
          Exit;
        end;
        xPrefix := '';
        xSep := '';
        for x := 0 to Length (aFileName) do
        begin
          if (aFileName[x] = '/')
          or (aFileName[x] = '\') then
          begin
            xPrefix := xPrefix + xSep;
            xSep := '..' + aFileName [x];
          end;
        end;
        xPrefix := PromptFolderName('Specify replacement for alias ' + xAlias, xPrefix) + '/';
        if not (AnsiStartsText('http://', xPrefix))
        and not (AnsiStartsText('https://', xPrefix)) then
          ForcePathDelims(xPrefix);
        PathPrefixes.Values[xAlias] := xPrefix;
        result := xPrefix + xSpec;
      end;
    end;
  finally
    free;
  end;
end;

function ExpandRelativeFileName(aMainFileName, aToRelateFileName: String): String;
  function _ExtractHttpPath(aFileName: String): String;
  var
    l, x: Integer;
  begin
    l := 0;
    for x := 1 to length (aFileName) do
      if aFileName [x] = '/' then
        l := x;
    result := Copy (aFileName, 1, l);
  end;
  function _ExpandHttpName (aFileName: String): String;
  var
    l, x: Integer;
  begin
    SetLength (result, Length (aFileName));
    x := 1;
    l := 0;
    while x <= Length (aFileName) do
    begin
      if aFileName [x] <> '.' then
      begin
        Inc (l);
        result [l] := aFileName [x];
        Inc (x);
      end
      else
      begin
        if (Copy (aFileName, x, 3) = '../') then
        begin
          Dec (l);
          while (l > 0) and (result [l] <> '/') do
            Dec (l);
          if l = 0 then
            raise Exception.Create ( 'Could not expand: '
                                   + aFileName
                                   );
          Inc (x, 3);
        end
        else
        begin
          if (Copy (aFileName, x, 2) = './') then
          begin
            Inc (x, 2);
          end
          else
          begin
            Inc (l);
            result [l] := aFileName [x];
            Inc (x);
          end;
        end;
      end;
    end;
    SetLength (Result, l);
  end;
var
  httpPath: String;
begin
  aToRelateFileName := PrepareFileNameSpace(aMainFileName, aToRelateFileName);
  if (AnsiStartsText('http://', aToRelateFileName))
  or (AnsiStartsText('https://', aToRelateFileName))
  or (AnsiStartsText('file://', aToRelateFileName))
{$ifdef UNIX}
  or (AnsiStartsText('/', aToRelateFileName))
{$endif}
{$ifdef WINDOWS}
  or (ExtractFileDrive(aToRelateFileName) <> '')
{$endif}
  then
  begin
    result := aToRelateFileName;
    exit;
  end;
  if (AnsiStartsText('\\', aToRelateFileName))
  then
  begin
    result := aToRelateFileName;
    exit;
  end;
  if (AnsiStartsText('http://', aMainFileName))
  or (AnsiStartsText('https://', aMainFileName))
  then
  begin
    httpPath := _ExtractHttpPath(aMainFileName);
    if (AnsiRightStr(httpPath, 1) = '/')
    and (AnsiLeftStr(aToRelateFileName, 1) = '/') then
      httpPath := AnsiLeftStr(httpPath, Length(httpPath) - 1);
    result := _ExpandHttpName (httpPath + aToRelateFileName);
  end
  else
    result := ExpandFileName (  ExtractFilePath(aMainFileName)
                              + aToRelateFileName
                             );
end;

function ExtractRelativeFileName(aMainFileName,
  aToRelateFileName: String): String;
var
  x: Integer;
  m: Integer;
  xSpec: String;
  xMainPath: String;
  xToRelatePath: String;
begin
  result := aToRelateFileName;
  if (aMainFileName = '')
  or (aToRelateFileName = '')
  or (AnsiStartsText ('http://', aToRelateFileName))
  then
    exit;
  if (aMainFileName = aToRelateFileName)
  then
  begin
    result := ExtractFileName(aToRelateFileName);
    exit;
  end;
  if ExtractFileDrive (aMainFileName) <> ExtractFileDrive (aToRelateFileName) then
    exit;
  // files on same drive and there is a difference
  xMainPath := ExtractFilePath(aMainFileName);
  xToRelatePath := ExtractFilePath(aToRelateFileName);
  if xMainPath = xToRelatePath then
  begin
    result := ExtractFileName(aToRelateFileName);
    exit;
  end;
  x := 1;
  // search for last common path delimiter
  m := 0;
  while (x <= Length (xMainPath))
  and (x <= Length (xToRelatePath))
  and (xMainPath [x] = xToRelatePath [x]) do
  begin
    if (IsPathDelimiter (xMainPath, x))
    and (IsPathDelimiter(xToRelatePath, x)) then
      m := x;
    Inc (x);
  end;
  Inc (m);
  x := m;
  result := '';
  while (m <= Length (xMainPath)) do
  begin
    if IsPathDelimiter (xMainPath, m) then
      result := result + '..\';
    Inc (m);
  end;
  while (x <= Length (aToRelateFileName)) do
  begin
    result := result + aToRelateFileName [x];
    Inc (x);
  end;
end;

function ReadStringFromFile (aFileName: String): String;
  function _GetURLAsString(aURL: string): string;
  var
    lHTTP: TIdHTTP;
    lStream: TStringStream;
  begin
    Result := '';
    lHTTP := TIdHTTP.Create(nil);
    lStream := TStringStream.Create(Result);
    try
      lHTTP.Get(aURL, lStream);
      lStream.Position := 0;
      Result := lStream.ReadString(lStream.Size);
    finally
      FreeAndNil(lHTTP);
      FreeAndNil(lStream);
    end;
  end;
begin
  if (AnsiStartsText('HTTP://', UpperCase(aFileName))) then
  begin
    result := _GetURLAsString (aFileName);
    exit;
  end;
  with TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(result, Size);
      Read(Pointer(result)^, Size);
      if GuessEncoding(result) <> EncodingUTF8 then
        Result := ConvertEncoding(result, GuessEncoding(result), EncodingUTF8);
    except
      Result := '';  // Deallocates memory
      Free;
      raise;
    end;
    Free;
  end;
end;

function uncFilename(aFileName: String): String;
begin
  if (Copy (aFileName, 1, 2) <> '\\')
  and (Copy (aFileName, 1, 2) <> '//') then
    result := ExpandUNCFileName(aFileName)
  else
    result := aFileName;
end;

function GetHostName: String;
begin
  TIdStack.IncUsage;
  try
    Result := LowerCase(GStack.HostName);
  finally
    TIdStack.DecUsage;
  end;
end;

function GetUserName: String;
begin
  {$IFDEF UNIX}
  result := GetEnvironmentVariable('USER');
  {$ELSE}
  result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
end;

function GetVersion: String;
var
  Stream: TResourceStream;
  vr: TVersionResource;
  fi: TVersionFixedInfo;
begin
  Result := '';
  try
  (* This raises an exception if version info has not been incorporated into the  *)
  (* binary (Lazarus Project -> Project Options -> Version Info -> Version        *)
  (* numbering).                                                                  *)
    Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vr:= TVersionResource.Create;
      try
        vr.SetCustomRawDataStream(Stream);
        fi:= vr.FixedInfo;
        result := Format('%d.%d.%d.%d', [fi.FileVersion[0], fi.FileVersion[1], fi.FileVersion[2], fi.FileVersion[3]]);
        vr.SetCustomRawDataStream(nil);
      finally
        vr.Free
      end;
    finally
     Stream.Free
    end
  except
    result := '(not available)';
  end
end;

function resolveAliasses (aString : String ; aAliasses : TStringList ): String ;
  const _regexp = '\$\{[_A-Za-z][A-Za-z0-9]*\}';
  function _resolv (aString: String; aSl: TStringList): String;
    function _trans (aString: String): String;
    var
      f, x: Integer;
    begin
      result := '';
      f := aSl.IndexOfName(aString);
      if f < 0 then
        raise Exception.Create('Alias not found:' + aString);
      if Assigned (aSl.Objects[f]) then
        raise Exception.Create('Circular reference for alias:' + aString);
      try
        aSl.Objects[f] := TObject (Pointer (1));
        try
          result := _resolv (aSl.ValueFromIndex[f], aSl);
        finally
          aSl.Objects[f] := nil;
        end;
      except
        on e: exception do
          raise Exception.CreateFmt('%s%s resolving %s', [e.Message, LineEnding, aString]);
      end;
    end;
  begin
    result := '';
    try
      with TRegExpr.Create do
      try
        Expression := _regexp;
        if Exec (aString) then
          result := Copy (aString, 1, MatchPos[0] - 1)
                  + _trans (Copy (Match[0], 3, Length (Match[0]) - 3)) // "${property}"
                  + _resolv (Copy (aString, MatchPos[0] + MatchLen[0], Length (aString)), aSl)
        else
          result := aString;
      finally
        Free;
      end;
    except
      on e: exception do
        raise Exception.CreateFmt('%s%s resolving %s', [e.Message, LineEnding, aString]);
    end;
  end;
var
  x: Integer;
  sl: TStringList;
begin
  result := aString;
  if not Assigned (aAliasses) then
    exit;
  if StringHasRegExpr(aString, _regexp) <> '' then
  begin
    sl := TStringList.Create;
    try
      sl.Text := aAliasses.Text; // need to work with a copy since we are gonna set TObjs
      result := _resolv (aString, sl);
    finally
      sl.Free;
    end;
  end;
end;


procedure SaveStringToFile (aFileName: String; aString: AnsiString);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.Size := Length (aString);
    S.Position := 0;
    S.WriteBuffer(aString[1], S.Size);
    S.SaveToFile(aFileName);
  finally
    S.Free;
  end;
end;

initialization
  PathPrefixes := TStringList.Create;
  PathPrefixes.Sorted := True;

finalization
  PathPrefixes.Free;

end.

