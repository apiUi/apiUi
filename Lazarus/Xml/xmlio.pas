unit xmlio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function PromptFolderName(aCaption, aStart: String): String;
function PrepareFileNameSpace(aMainFileName, aFileName: String): String;
function ReadStringFromFile (aFileName: String): String;
procedure SaveStringToFile (aFileName: String; aString: String);
function ExpandRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function ExtractRelativeFileName(aMainFileName, aToRelateFileName: String): String;
function GetUserName: String;
function GetVersion: String;

var
  PathPrefixes: TStringList;

implementation
uses StrUtils
   , LCLIntf, LCLType, LMessages
   , versiontypes, versionresource
   , idHTTP
   , LConvEncoding
   , RegExpr
   , Forms
   , Controls
   , PromptUnit
   ;

function PromptFolderName(aCaption, aStart: String): String;
var
  xForm: TPromptForm;
begin
  Application.CreateForm(TPromptForm, xForm);
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
  xFound: Boolean;
  x: Integer;
begin
  result := aFileName;
  with TRegExpr.Create do
  try
    xFound := False;
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
          {$ifdef windows}
          if not (AnsiStartsText('http://', result))
          and not (AnsiStartsText('https://', result)) then
          begin
            for x := 1 to Length (result) do
              if result[x] = '/' then
                result[x] := '\';
          end;
          {$endif}
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
        xPrefix := PromptFolderName('Specify replacement for alias ' + xAlias, xPrefix);
        {$ifdef windows}
        if not (AnsiStartsText('http://', xPrefix))
        and not (AnsiStartsText('https://', xPrefix)) then
        begin
          for x := 1 to Length (xPrefix) do
            if xPrefix[x] = '/' then
              xPrefix[x] := '\';
        end;
        {$endif}
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
  if (AnsiStartsText('http://', aToRelateFileName))
  or (AnsiStartsText('https://', aToRelateFileName))
  or (AnsiStartsText('file://', aToRelateFileName))
  or (ExtractFileDrive(aToRelateFileName) <> '')
  then
  begin
    result := aToRelateFileName;
    exit;
  end;
  aToRelateFileName := PrepareFileNameSpace(aMainFileName, aToRelateFileName);
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

