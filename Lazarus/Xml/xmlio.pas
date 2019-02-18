unit xmlio;

{$mode objfpc}{$H+}
{$I+}
interface

uses
  Classes, SysUtils;


function urlDecode(const S: String): String;
function urlEncode(const S: String): String;
function makeFileNameAllowed(aFileName: String): String;
function isFileNameAllowed (aFileName: String): Boolean;
procedure EraseAllFolderContent (aFolderName: String);
function HttpResponseCodeToText (aCode: Integer): String;
function HttpPostDialog (aRequest, aUrl: String): String;
function PromptFolderName(aCaption, aStart: String): String;
function PrepareFileNameSpace(aMainFileName, aFileName: String): String;
function ReadStringFromFile (aFileName: String): String;
procedure SaveStringToFile (aFileName: String; aString: String);
function ExpandRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function ExtractRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function uncFilename (aFileName: String): String;
function GetFileChangedTime (aFileName:string):TDateTime;
function GetHostName: String;
function GetUserName: String;
function GetVersion: String;
function resolveAliasses (aString: String; aDoDecriptPassword: Boolean = false): String;
function StringHasRegExpr (aString, aExpr: String): String;
function ExplodeStr(S, Delim: string; const List: Classes.TStrings;
  const AllowEmpty: Boolean = True; const Trim: Boolean = False): Integer;
function DecryptPassword(aPassword: AnsiString): AnsiString;
function EncryptPassword(aPassword: AnsiString): AnsiString;
function PosSubString (ss, ms: String; CaseSensitive, MatchWholeWord: Boolean): Integer;
function ReplaceStrings (OrgString, SrchString, RplString: String; CaseSensitive, MatchWholeWord: Boolean): String;
function ifthen(val:boolean;const iftrue:String; const iffalse:String='') :String;

const base64DocxStartStr = 'UEsDBB';
const base64PdfStartStr = 'JVBERi';
const base64RtfStartStr = 'e1xyd';

type TOnStringEvent = procedure (const Msg: String) of Object;
var
  PathPrefixes, ProjectAliasses: TStringList;
  ProjectContext: String;
  ProjectContexts: TObject;
  doTrackXmlIO: Boolean;
  OnNotify: TOnStringEvent;


implementation
uses StrUtils
   , LCLIntf, LCLType, LMessages
   , LazFileUtils
   , versiontypes, versionresource
   , idHTTP, IdCustomHTTPServer
   , IdSSLOpenSSL
   , idStack
   , LConvEncoding
   , base64
   , RegExpr
   , StringListListUnit
{$ifndef NoGUI}
   , Forms
   , Controls
   , PromptFolderUnit
{$endif}
   ;

procedure SjowMessage (aString: String);
begin
  if Assigned (OnNotify) then
    OnNotify (aString);
end;


function ifthen(val:boolean;const iftrue:String; const iffalse:String='') :String;
begin
  if val then result:=iftrue else result:=iffalse;
end;

function PosSubString (ss, ms: String; CaseSensitive, MatchWholeWord: Boolean): Integer;
var
  x, o: Integer;
  match: Boolean;
begin
  if not CaseSensitive then
  begin
    ss := UpperCase(ss);
    ms := UpperCase(ms);
  end;
  if not MatchWholeWord then
  begin
    result := Pos(ss, ms);
    exit;
  end;
  o := 1;
  x := Pos (ss, ms);
  match := False;
  while (not Match) and (x > 0) do
  begin
    Match := True;
    o := o + x - 1;
    if (o > 1) then
      if IsCharAlphanumeric(ms[o-1]) then
        Match := False;
    if ((o + Length (ss)) <= Length (ms)) then
      if IsCharAlphanumeric(ms[o+Length(ss)]) then
        Match := False;
    if not Match then
    begin
      o := o + Length (ss);
      x := Pos (ss, Copy (ms, o, Length (ms)));
    end;
  end;
  if Match then
    result := o
  else
    result := 0;
end;

function ReplaceStrings (OrgString, SrchString, RplString: String; CaseSensitive, MatchWholeWord: Boolean): String;
var
  x: Integer;
begin
  x := PosSubString(SrchString,OrgString,CaseSensitive,MatchWholeWord);
  if x > 0 then
  begin
    result := Copy (OrgString, 1, x - 1)
            + RplString
            + ReplaceStrings (Copy (OrgString, x + Length (SrchString), Length (OrgString))
                             ,SrchString
                             ,RplString
                             ,CaseSensitive
                             ,MatchWholeWord
                             );
  end
  else
    result := OrgString;
end;

function SimpleEncrypt(const Source: AnsiString): AnsiString;
var
  Index: Integer;
  EncryptionSeed: String;
begin
  EncryptionSeed := 'th^ruh54bdkjbkjb4k458&*';
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := AnsiChar((Ord(EncryptionSeed[Index mod Length(EncryptionSeed)]) xor Ord(Source[Index])));
end;

function DecryptPassword(aPassword: AnsiString): AnsiString;
begin
  if aPassword <> '' then
    result :=  SimpleEncrypt(DecodeStringBase64(aPassword))
  else
    result := '';
end;

function EncryptPassword(aPassword: AnsiString): AnsiString;
begin
  if aPassword <> '' then
    result := EncodeStringBase64 (SimpleEncrypt(aPassword))
  else
    result := '';
end;

function ExplodeStr(S, Delim: string; const List: Classes.TStrings;
  const AllowEmpty: Boolean = True; const Trim: Boolean = False): Integer;

  function SplitStr(S, Delim: string; out S1, S2: string): Boolean;
  var
    DelimPos: Integer;  // position of delimiter in source string
  begin
    // Find position of first occurence of delimter in string
    DelimPos := Pos(Delim, S);
    if DelimPos > 0 then
    begin
      // Delimiter found: do split and return True
      S1 := Copy(S, 1, DelimPos - 1);
      S2 := Copy(S, DelimPos + Length (Delim), MaxInt);
      Result := True;
    end
    else
    begin
      // Delimeter not found: return false and set S1 to whole string
      S1 := S;
      S2 := '';
      Result := False;
    end;
  end;
var
  Item: string;       // current delimted text
  Remainder: string;  // remaining unconsumed part of string

  procedure AddItem;
  begin
    // Adds optionally trimmed item to list if required
    if (Trim) then
      Item := SysUtils.Trim(Item);
    if (Item <> '') or AllowEmpty then
      List.Add(Item);
  end;

begin
  // Clear the list
  List.Clear;
  // Check we have some entries in the string
  if S <> '' then
  begin
    // Repeatedly split string until we have no more entries
    while SplitStr(S, Delim, Item, Remainder) do
    begin
      AddItem;
      S := Remainder;
    end;
    // Add any remaining item
    AddItem;
  end;
  Result := List.Count;
end;

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

function makeFileNameAllowed(aFileName: String): String;
var
  naChars: String;
  x: Integer;
begin
  result := aFileName;
  naChars := '\/:*?"<>|';
  for x := 1 to Length(Result) do
  begin
    if Pos (result [x], naChars) > 0 then
      result [x] := '_';
  end;
  if (Length(Result) > 0) then
  begin
    if (Result [Length(Result)] = '\')       // n.a. by Windows
    or (Result [Length(Result)] = ' ') then  // n.a. by Windows
      Result [Length(Result)] := '_';
  end;
end;

function isFileNameAllowed(aFileName: String): Boolean;
begin
  result := False;
  with TRegExpr.Create('^[^\\\/\:\*\?\"\<\>\|]*[^\\\/\:\*\?\"\<\>\|\. ]$') do
//                        ^\\\/\:\*\?\"\<\>\|\.      (second part because windows does not allow a dot or space as last char)
  try
    result := Exec(aFileName);
  finally
    Free;
  end;
end;

procedure EraseAllFolderContent(aFolderName: String);
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink {$endif unix};
var
  xFileInfo: TSearchRec;
  xCurSrcDir, xCurFilename: String;
  xFound: Boolean;
begin
  xCurSrcDir := LazFileUtils.CleanAndExpandDirectory(aFolderName);
  xFound := (LazFileUtils.FindFirstUTF8 ( xCurSrcDir
                                        +
                                        {$IFDEF WINDOWS}
                                        '*.*'
                                        {$ELSE}
                                        '*'
                                        {$ENDIF}
                                        , DeleteMask
                                        , xFileInfo
                                        ) = 0);
  while xFound do
  begin
    if (xFileInfo.Name <> '.')
    and (xFileInfo.Name <> '..')
    and (xFileInfo.Name <> '') then
    begin
      xCurFilename := xCurSrcDir + xFileInfo.Name;
      if ((xFileInfo.Attr and faDirectory) > 0)
      {$ifdef unix}
      and ((xFileInfo.Attr and faSymLink) = 0)
      {$endif unix}
      then
      begin
        EraseAllFolderContent(xCurFilename);
        if not LazFileUtils.RemoveDirUTF8(xCurFilename) then
          raise Exception.CreateFmt('Could not remove folder "%s"', [xCurFilename]);
      end
      else
      begin
        if not LazFileUtils.DeleteFileUTF8(xCurFilename) then
          raise Exception.CreateFmt('Could not delete file "%s"', [xCurFilename]);
      end;
    end;
    xFound := (LazFileUtils.FindNextUTF8(xFileInfo) = 0);
  end;
  LazFileUtils.FindCloseUTF8(xFileInfo);
end;

function HttpResponseCodeToText(aCode: Integer): String;
begin
  with TIdHTTPResponseInfo.Create(nil, nil, nil) do
  try
    ResponseNo := aCode;
    Result := ResponseText;
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
{$ifndef NoGUI}
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
{$else}
begin
  raise Exception.CreateFmt('%s%sno replcement specified%s%s', [aCaption, LineEnding, LineEnding, aStart]);
{$endif}
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
  or (AnsiStartsText ('https://', aToRelateFileName))
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
      result := result + '..' + DirectorySeparator;
    Inc (m);
  end;
  while (x <= Length (aToRelateFileName)) do
  begin
    result := result + aToRelateFileName [x];
    Inc (x);
  end;
end;

function ReadStringFromFile (aFileName: String): String;
  function _GetURLAsString(aURL: string; useSsl: Boolean): string;
  var
    lHTTP: TIdHTTP;
    lStream: TStringStream;
  begin
    Result := '';
    lHTTP := TIdHTTP.Create(nil);
    lStream := TStringStream.Create(Result);
    try
      if useSsl then
      begin
        lHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
        with (lHTTP.IOHandler as TIdSSLIOHandlerSocketOpenSSL) do
        begin
        {
          SSLOptions.CertFile := aOperation.sslCertificateFile;
          SSLOptions.KeyFile := aOperation.sslKeyFile;
          SSLOptions.RootCertFile := aOperation.sslRootCertificateFile;
        }
          SSLOptions.Method := sslvTLSv1_2;
          SSLOptions.Mode := sslmUnassigned;
          SSLOptions.VerifyMode := [];
        end;
      end;
      lHTTP.Get(aURL, lStream);
      lStream.Position := 0;
      Result := lStream.ReadString(lStream.Size);
    finally
      if Assigned (lHTTP)
      and Assigned (lHTTP.IOHandler) then
        lHTTP.IOHandler.Free;
      FreeAndNil(lHTTP);
      FreeAndNil(lStream);
    end;
  end;
begin
  aFileName := resolveAliasses(aFileName);
  if doTrackXmlIO then
    SjowMessage('ReadStringFromFile: ' + aFileName);
  if (AnsiStartsText('HTTP://', aFileName)) then
  begin
    result := _GetURLAsString (aFileName, false);
    exit;
  end;
  if (AnsiStartsText('HTTPS://', aFileName)) then
  begin
    result := _GetURLAsString (aFileName, true);
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
  result := aFileName;
{ does ot work snce lazarus 1.6 fpc 3.0
  if (Copy (aFileName, 1, 2) <> '\\')
  and (Copy (aFileName, 1, 2) <> '//') then
    result := ExpandUNCFileName(aFileName)
  else
    result := aFileName;
}
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

function resolveAliasses (aString : String; aDoDecriptPassword: Boolean = false): String ;
  const _regexp = '\$\{[^\{\}]+\}';
  function _resolv (aString: String; aSl: TStringList): String;
  var
    xHasExp: Boolean;
    function _isPwd (aString: String): Boolean;
    var
      xString: String;
    begin
      xString := UpperCase(aString);
      result := (RightStr(xString, 3) = 'PWD')
             or (RightStr(xString, 8) = 'PASSWORD')
              ;
    end;
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
          if aDoDecriptPassword
          and _isPwd(aString) then
            result := _resolv (DecryptPassword(aSl.ValueFromIndex[f]), aSl)
          else
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
    result := aString;
    try
      with TRegExpr.Create do
      try
        Expression := _regexp;
        while Exec (result) do
        begin
          result := Copy (result, 1, MatchPos[0] - 1)
                  + _trans (Copy (Match[0], 3, Length (Match[0]) - 3)) // "${property}"
                  + _resolv (Copy (result, MatchPos[0] + MatchLen[0], Length (result)), aSl)
                  ;
        end;
      finally
        Free;
      end;
    except
      on e: exception do
        raise Exception.CreateFmt('%s%s resolving %s', [e.Message, LineEnding, aString]);
    end;
  end;
var
  x, r, c: Integer;
  sl: TStringList;
begin
  result := aString;
  if Assigned (ProjectContexts)
  and (ProjectContext <> '') then
  begin
    if Pos ('${', aString) > 0 then
    begin
      with (ProjectContexts as TStringListList) do
      begin
        for r := 1 to RowCount - 1 do
        begin
          if CellValue[0, r] = ProjectContext then
          begin
            sl := TStringList.Create;
            try
              for c := 1 to ColCount - 1 do
                sl.Values[CellValue[c, 0]] := CellValue[c, r];
              sl.Values['context'] := ProjectContext;
              result := _resolv (result, sl);;
            finally
              sl.Free;
            end;
          end;
        end;
      end;
    end;
    exit;
  end;
  if not Assigned(ProjectAliasses) then
    Exit;
  if Pos ('${', result) > 0 then
  begin
    sl := TStringList.Create;
    try
      sl.Text := ProjectAliasses.Text; // need to work with a copy since we are gonna set TObjs
      result := _resolv (result, sl);
    finally
      sl.Free;
    end;
  end;
end;

function GetFileChangedTime (aFileName:string):TDateTime;
begin
  result := 0;
  try
    result := FileDateToDateTime (FileAge(aFileName));
  except
  end;
end;

procedure SaveStringToFile (aFileName: String; aString: AnsiString);
var
  S: TMemoryStream;
begin
  if doTrackXmlIO then
    SjowMessage('SaveStringToFile: ' + resolveAliasses(aFileName));
  S := TMemoryStream.Create;
  try
    S.Size := Length (aString);
    S.Position := 0;
    S.WriteBuffer(aString[1], S.Size);
    S.SaveToFile(resolveAliasses(aFileName));
  finally
    S.Free;
  end;
end;

function urlDecode(const S: String): String;
var
  Idx: Integer;   // loops thru chars in string
  pIdx: Integer; // remember pos of %
  Hex: string;    // string of hex characters
  Code: Integer;  // hex character code (-1 on error)
begin
  // Intialise result and string index
  Result := '';
  Idx := 1;
  // Loop thru string decoding each character
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        pIdx := Idx;
        // % should be followed by two hex digits - exception otherwise
        if Idx <= Length(S) - 2 then
        begin
          // there are sufficient digits - try to decode hex digits
          Hex := S[Idx+1] + S[Idx+2];
          Code := SysUtils.StrToIntDef('$' + Hex, -1);
          Inc(Idx, 2);
        end
        else
          // insufficient digits - error
          Code := -1;
        // check for error and raise exception if found
        if Code = -1 then
          raise SysUtils.EConvertError.Create(
            '(UrlDecode) Invalid hex digit in string: '''
            + Copy (S, pIdx, 3)
            + ''' (at position '
            + IntToStr (pIdx)
            + ')'
          );
        // decoded OK - add character to result
        Result := Result + Chr(Code);
      end;
      '+':
        // + is decoded as a space
        Result := Result + ' '
      else
        // All other characters pass thru unchanged
        Result := Result + S[Idx];
    end;
    Inc(Idx);
  end;
end;

function urlEncode(const S: String): String;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    case S[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.': Result := Result + S[Idx];
      ' ': Result := Result + '+';
      else Result := Result + '%' + SysUtils.IntToHex(Ord(S[Idx]), 2);
    end;
  end;
end;


initialization
  PathPrefixes := TStringList.Create;
  PathPrefixes.Sorted := True;
  ProjectAliasses := nil;

finalization
  PathPrefixes.Free;

end.

