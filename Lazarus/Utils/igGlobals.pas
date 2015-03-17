unit igGlobals;

{$MODE Delphi}
interface

uses SysUtils
   , Classes
   , ComCtrls
   , Controls
   , StdCtrls
   , ActnList
   , Dialogs
   , Grids
   , Graphics, FileUtil
   , Messages
   , Types
   , RichBox
//   , VirtualTrees
   ;

resourcestring
  S_REGEXP_LINK = '(?i)(FTP|HTTP|FILE|DOC)://([_a-z\d\-]+(\.[_a-z\d\-]+)+)((/[ _a-z\d\-\\\.]+)+)*(\?[a-z0-9=&]+)?';
//S_REGEXP_LINK = '(?i)(FILE)://[a-z]([_:/a-z0-9\.])+';
type
  TOnHaveDir = function ( Path: String
                        ): Boolean of Object;
  TOnHaveFile = function ( Path: String
                         ; SRec: TSearchRec
                         ): Boolean of Object;
  TOnHaveLinkEvent = procedure ( Sender: TObject
                               ; aLink: String
                               ) of Object;

var
  Height: Integer;
  Width: Integer;
  Name: String;
  LastTop: Integer;
  LastLeft: Integer;
  Top: Integer;
  Left: Integer;
  igLength: Integer;
  UserWorkingArea: String;
  UWASeparator: String;
  PictureClause: String;
  ReadOnly: Boolean;
  Lines: Integer;
  Columns: Integer;
  SkippingLines: Integer;
  SkippingColumns: Integer;
  Occurs: Integer;
  OffsetColumns: Integer;
  Value: String;
  Dim: Boolean;
  Redefines: Boolean;
  CopyMember: String;
  CopyLibrary: String;

type TOnHaveNoMatch = procedure ( Sender: TObject
                                ; Key: String
                                ; Obj: TObject
                                );
type TOnHaveMatch = procedure ( Sender: TObject
                              ; Key: String
                              ; LeftObj, RightObj: TObject
                              );
procedure BalancedLine ( Sender: TObject
                       ; leftObjs, rightObjs: TStringList
                       ; leftOnly: TOnHaveNoMatch
                       ; LeftAndRight: TOnHaveMatch
                       ; rightOnly: TOnHaveNoMatch
                       );
function PosSubString (ss, ms: String; CaseSensitive, MatchWholeWord: Boolean): Integer;
function ReplaceStrings (OrgString, SrchString, RplString: String; CaseSensitive, MatchWholeWord: Boolean): String;
function B64EncodeStream(const S: TMemoryStream): AnsiString;
function URLDecode(const S: AnsiString): AnsiString;
function URLEncode(const S: AnsiString): AnsiString;
function HEXDecode(const S: AnsiString): AnsiString;
function HEXEncode(const S: AnsiString): AnsiString;
function HEXRGBToColor(const S: string): TColor;

procedure MemoSetSelectedText (Memo: TLzRichEdit; Line: Integer; Column: Integer; Width: Integer);
function StringMatchesMask( S, mask: String; CaseSensitive, useRegExp: Boolean ): Boolean;
function SubStringMatch ( aString: String
                        ; var aOffset: Integer
                        ; aSubString: String
                        ): Boolean;
function ExtractCopyLibraryName (arg: String): String;
procedure InitScreenClause;
procedure FindDirs ( Path: String
                   ; Mask: String
                   ; Recursive: Boolean
                   ; OnHaveDir: TOnHaveDir
                   );
procedure FindFiles ( Path: String
                    ; Mask: String
                    ; Recursive: Boolean
                    ; OnHaveDir: TOnHaveDir
                    ; OnHaveFile: TOnHaveFile
                    );
function GetFileDateTimeAsString (FileName: String): String;
function aToolButtonUsed(Sender: TObject): Boolean;
function ReadB64StringFromFile (aFileName: String): String;
function ReadBinaryStringFromFile (aFileName: String): String;
function ReadStringFromFile (aFileName: String): String;
procedure SaveBinaryStringToFile (aFileName: String; aString: String);
procedure SaveStringToFile (aFileName: String; aString: AnsiString);
function ExpandRelativeFileName(aMainFileName,
  aToRelateFileName: String): String;
function ExtractRelativeFileName(aMainFileName,
  aToRelateFileName: String): String;
function EbcdicToAscii (const aString: String): String;
function AsciiToEbcdic (const aString: String): String;
function RestStr (aPrefix: String; aString: String): String;
function ThisCaption (aCaption: String): String;
function LastCaption (aCaption: String): String;
function NextFullCaption (aCaption: String): String;
procedure ModifyColorFor(ctrl: TWinControl);
procedure GridAddRow(aGrid: TStringGrid);
procedure GridInsertRow(aGrid: TStringGrid);
procedure GridDeleteRow(aGrid: TStringGrid);
procedure GridCopyMatrix (aGrid: TStringGrid; aText: String; aDoIncludeFixed: Boolean);
procedure GridPasteMatrix (aGrid: TStringGrid);
function GridDuplicateRowsExist ( aGrid: TStringGrid
                                ; aFirstColumn, aLastColumn: Integer
                                ; var aDuplicateRow: Integer
                                ; var aDuplicatedRow: Integer
                                ): Boolean;
function ExplodeStr(S: string; const Delim: Char; const List: Classes.TStrings;
  const AllowEmpty: Boolean = True; const Trim: Boolean = False): Integer;
function SplitStr(const S: string; Delim: Char; out S1, S2: string): Boolean;
function BoolToStr (aValue: Boolean): String;
function GetUserName: String;
function GetHostName: String;
function GenerateRandomId: String;
procedure MemoMouseDown(aMemo: TLzRichEdit; X, Y: Integer; aOnHaveLink: TOnHaveLinkEvent);
procedure MemoMouseMove(aMemo: TLzRichEdit; X, Y: Integer);
procedure MemoShowLinks (aMemo: TLzRichEdit);

implementation

uses StrUtils
   , Menus
   , TypInfo
   , ClipBrd
   , LCLIntf, LCLType, LMessages
   , Registry
   , RegExpr
   , hashUtilz
   , idHTTP
   , base64
   ;
procedure MemoMouseDown(aMemo: TLzRichEdit; X, Y: Integer; aOnHaveLink: TOnHaveLinkEvent);
var
  iCharIndex, z: Integer;
  Pt: TPoint;
  s: string;
  Rslt: Boolean;
  rx: TRegExpr;
begin
  {$ifdef windows}
  Pt := Point(X, Y);
  iCharIndex := aMemo.Perform(Messages.EM_CHARFROMPOS, 0, Integer(@Pt));
  if (iCharIndex < 0)
  or (iCharIndex = Length (s))
  then
    exit;
  Inc(iCharIndex);
  rx := TRegExpr.Create;
  try
    Rx.Expression := S_REGEXP_LINK;
    s := '';
    for z := 0 to aMemo.Lines.Count - 1 do
      s := s + aMemo.Lines.Strings[z] + ' ';
    Rslt := Rx.Exec(s);
    while Rslt and (Rx.MatchPos [0] <= iCharIndex) do
    begin
      if (Rx.MatchPos [0] + Rx.MatchLen [0] > iCharIndex) then
      begin
        aOnHaveLink (aMemo, Rx.Match[0]);
        exit;
      end;
      Rslt := Rx.ExecNext;
    end;
  finally
    rx.Free;
  end;
  {$else}
  raise Exception.Create ('only with ms windos');
  {$endif}
end;

procedure MemoMouseMove(aMemo: TLzRichEdit; X, Y: Integer);
var
  iCharIndex, z: Integer;
  Pt: TPoint;
  s: string;
  Rslt: Boolean;
  rx: TRegExpr;
begin
  {$ifdef windows}
  Pt := Point(X, Y);
  iCharIndex := aMemo.Perform(Messages.EM_CHARFROMPOS, 0, Integer(@Pt));
  if (iCharIndex < 0)
  or (iCharIndex = Length (s))
  then begin
    aMemo.Cursor := crArrow;
    exit;
  end;
  Inc(iCharIndex);
  s := '';
  for z := 0 to aMemo.Lines.Count - 1 do
    s := s + aMemo.Lines.Strings[z] + ' ';
  rx := TRegExpr.Create;
  try
    Rx.Expression := S_REGEXP_LINK;
    Rslt := Rx.Exec(s);
    while Rslt and (Rx.MatchPos [0] <= iCharIndex) do
    begin
      if (Rx.MatchPos [0] + Rx.MatchLen [0] > iCharIndex) then
      begin
        aMemo.Cursor := crHandPoint;
        exit;
      end;
      Rslt := Rx.ExecNext;
    end;
    aMemo.Cursor := crArrow;
  finally
    rx.Free;
  end;
  {$else}
  raise Exception.Create ('only with ms windos');
  {$endif}
end;

procedure MemoShowLinks (aMemo: TLzRichEdit);
var
  rx: TRegExpr;
  x: Integer;
  Rslt: Boolean;
  xS: String;
  swapPos, swapLen: Integer;
begin
  rx := TRegExpr.Create;
  try
    rx.Expression := S_REGEXP_LINK;
    {
    xs := '';
    for x := 0 to aMemo.Lines.Count - 1 do
      xs := xs + aMemo.Lines.Strings[x] + ' ';
      }
    xs := '';
    with TStringList.Create do
    try
      Text := aMemo.Text;
      for x := 0 to Count - 1 do
        xs := xs + Strings[x] + ' ';
    finally
      Free;
    end;
    swapPos := aMemo.SelStart;
    swapLen := aMemo.SelLength;
    aMemo.SelectAll;
    aMemo.SelAttributes.Color := clBlack;
    aMemo.SelAttributes.Style := [];
    Rslt := Rx.Exec(xs);
    while Rslt do
    begin
      aMemo.SelStart := Rx.MatchPos [0] - 1;
      aMemo.SelLength := Rx.MatchLen [0];
      aMemo.SelAttributes.Color := clBlue;
      aMemo.SelAttributes.Style := [fsUnderline];
      Rslt := Rx.ExecNext;
    end;
    aMemo.SelStart := swapPos;
    aMemo.SelLength := swapLen;
  finally
    rx.Free;
  end;
end;

function GenerateRandomId: String;
begin
  result := Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          + '-'
          + Copy (SHA1 (FloatToStr (Random)), 1, 4)
          ;
end;

function GetUserName: String;
begin
  {$IFDEF UNIX}
  result := GetEnvironmentVariable('USER');
  {$ELSE}
  result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
end;

function GetHostName: String;
begin
  result := 'gethostnameisnotyetimplemeted';
end;

{*********************************************}
{ Set Global Environment Function             }
{ Coder : Kingron,2002.8.6                    }
{ Bug Report : Kingron@163.net                }
{ Test OK For Windows 2000 Advance Server     }
{ Parameter:                                  }
{ Name : environment variable name            }
{ Value: environment variable Value           }
{ Ex: SetGlobalEnvironment('MyVar','OK')      }
{*********************************************}

function BoolToStr (aValue: Boolean): String;
begin
  if aValue then
    result := 'true'
  else
    result := 'false';
end;

function SplitStr(const S: string; Delim: Char; out S1, S2: string): Boolean;
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimter in string
  DelimPos := SysUtils.AnsiPos(Delim, S);
  if DelimPos > 0 then
  begin
    // Delimiter found: do split and return True
    S1 := Copy(S, 1, DelimPos - 1);
    S2 := Copy(S, DelimPos + 1, MaxInt);
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

function ExplodeStr(S: string; const Delim: Char; const List: Classes.TStrings;
  const AllowEmpty: Boolean = True; const Trim: Boolean = False): Integer;
var
  Item: string;       // current delimted text
  Remainder: string;  // remaining unconsumed part of string

  // ---------------------------------------------------------------------------
  procedure AddItem;
  begin
    // Adds optionally trimmed item to list if required
    if (Trim) then
      Item := SysUtils.Trim(Item);
    if (Item <> '') or AllowEmpty then
      List.Add(Item);
  end;
  // ---------------------------------------------------------------------------

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

procedure BalancedLine ( Sender: TObject
                       ; leftObjs, rightObjs: TStringList
                       ; leftOnly: TOnHaveNoMatch
                       ; LeftAndRight: TOnHaveMatch
                       ; rightOnly: TOnHaveNoMatch
                       );
var
  xl, xr: Integer;
begin
  xl := 0;
  xr := 0;
  while (xl < leftObjs.Count)
     or (xr < rightObjs.Count) do
  begin
    if (xl < leftObjs.Count)
    and (xr < rightObjs.Count) then
    begin
      if leftObjs.Strings[xl] = rightObjs.Strings[xr] then
      begin
        if Assigned (leftAndRight) then
          leftAndRight (Sender, leftObjs.Strings[xl], leftObjs.Objects[xl], rightObjs.Objects[xr]);
        Inc (xl);
        Inc (xr);
      end
      else
      begin
        if leftObjs.Strings[xl] < rightObjs.Strings[xr] then
        begin
          if Assigned (leftOnly) then
            leftOnly (Sender, leftObjs.Strings[xl], leftObjs.Objects[xl]);
          Inc (xl);
        end
        else
        begin
          if Assigned (rightOnly) then
            rightOnly (Sender, rightObjs.Strings[xr], rightObjs.Objects[xr]);
          Inc (xr);
        end;
      end;
    end
    else
    begin
      if (xl < leftObjs.Count) then
      begin
        if Assigned (leftOnly) then
          leftOnly (Sender, leftObjs.Strings[xl], leftObjs.Objects[xl]);
        Inc (xl);
      end
      else
      begin
        if Assigned (rightOnly) then
          rightOnly (Sender, rightObjs.Strings[xr], rightObjs.Objects[xr]);
        Inc (xr);
      end;
    end;
  end;
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

function GridDuplicateRowsExist ( aGrid: TStringGrid
                                ; aFirstColumn, aLastColumn: Integer
                                ; var aDuplicateRow: Integer
                                ; var aDuplicatedRow: Integer
                                ): Boolean;
var
  xLow: Integer;
  xHigh: Integer;
  xCol: Integer;
begin
  result := False;
  for xLow := aGrid.FixedRows to aGrid.RowCount - 1 do
  begin
    for xHigh := xLow + 1 to aGrid.RowCount - 1 do
    begin
      for xCol := aFirstColumn to aLastColumn do
      begin
        result := False;
        if aGrid.Cells [xCol, xLow] <> aGrid.Cells [xCol, xHigh] then
          break;
        result := True;
      end;
      if result then
      begin
        aDuplicateRow := xHigh;
        aDuplicatedRow := xLow;
        Exit;
      end;
    end; {xHigh}
  end; {xLow}
end;

procedure GridAddRow(aGrid: TStringGrid);
var
  xRow: Integer;
begin
  xRow := aGrid.Row;
  aGrid.RowCount := aGrid.RowCount + 1;
  aGrid.Row := aGrid.RowCount - 1;
  aGrid.Rows [aGrid.Row] := aGrid.Rows [xRow];
end;

procedure GridInsertRow(aGrid: TStringGrid);
var
  xRow: Integer;
begin
  aGrid.RowCount := aGrid.RowCount + 1;
  for xRow := aGrid.RowCount - 2 downto aGrid.Row do
    aGrid.Rows [xRow + 1] := aGrid.Rows [xRow];
end;

procedure GridDeleteRow(aGrid: TStringGrid);
var
  xRow: Integer;
begin
  if aGrid.RowCount > aGrid.FixedRows  + 1 then
  begin
    for xRow := aGrid.Row to aGrid.RowCount - 2 do
      aGrid.Rows [xRow] := aGrid.Rows [xRow + 1];
    aGrid.RowCount := aGrid.RowCount - 1;
  end;
end;

procedure GridCopyMatrix (aGrid: TStringGrid; aText: String; aDoIncludeFixed: Boolean);
var
  r: Integer;
  c: Integer;
  Left: Integer;
  Top: Integer;
  Right: Integer;
  Bottom: Integer;
  Separator: String;
  StringList: TStringList;
  CopyRow: String;
  procedure CopyColumnHeaders;
  var
    r, c: Integer;
  begin
    for r := 0 to aGrid.FixedRows - 1 do
    begin
      Separator := '';
      CopyRow := '';
      for c := 0 to aGrid.FixedCols - 1 do
      begin
        CopyRow := CopyRow + Separator + aGrid.Cells [c, r];
        Separator := #9; {separated by tabs}
      end;
      for c := Left to Right do
      begin
        CopyRow := CopyRow + Separator + aGrid.Cells [c, r];
        Separator := #9; {separated by tabs}
      end;
      StringList.Add(CopyRow);
    end; // for each fixed row
  end;
begin
  Left := aGrid.Selection.Left;
  Top := aGrid.Selection.Top;
  Right := aGrid.Selection.Right;
  Bottom := aGrid.Selection.Bottom;
  StringList := TStringList.Create;
  try
    if (Right = Left)
    and (Bottom = Top) then
      Clipboard.AsText := aGrid.Cells [Left, Top] {exact one cell selected}
    else
    begin
      if aText <> '' then
        StringList.Text := aText;
      if aDoIncludeFixed then
        CopyColumnHeaders;
      for r := Top to Bottom do
      begin
        Separator := '';
        CopyRow := '';
        if aDoIncludeFixed then
        begin
          for c := 0 to aGrid.FixedCols - 1 do
          begin
            CopyRow := CopyRow + Separator + aGrid.Cells [c, r];
            Separator := #9; {separated by tabs}
          end;
        end;
        for c := Left to Right do
        begin
          CopyRow := CopyRow + Separator + aGrid.Cells [c, r];
          Separator := #9; {separated by tabs}
        end;
        StringList.Add(CopyRow);
      end;
      ClipBoard.AsText := StringList.Text;
    end;
  finally
    StringList.Free;
  end;
end;

procedure GridPasteMatrix (aGrid: TStringGrid);
var
  Grect: TGridRect;
  S, CS, F: string;
  L, R, C: Integer;
  xRow: Integer;
  xCol: Integer;
  PasteRows: TStringList;
  PasteRow: String;
begin
  GRect := aGrid.Selection;
  L := GRect.Left;
  R := GRect.Top;
  PasteRows := TStringList.Create;
  try
    if not Clipboard.HasFormat(CF_TEXT) then
      raise Exception.Create('Clipboard does not contain text');
    PasteRows.Text := ClipBoard.AsText;
    for xRow := 0 to PasteRows.Count - 1 do
    begin
      PasteRow := PasteRows.Strings [xRow];
      CS := '';
      if R + xRow >= aGrid.RowCount then
        aGrid.RowCount := R + xRow + 1;
      xCol := 0;
      for C := 1 to Length (PasteRow) do
      begin
        if PasteRow [C] = #9 then
        begin
          if (L + xCol <= aGrid.ColCount - 1) then
            aGrid.Cells [L + xCol, R + xRow] := CS;
          CS := '';
          xCol := xCol + 1;
        end
        else
          CS := CS + PasteRow [C];
      end; {for each char in string}
      if (L + xCol <= aGrid.ColCount - 1) then
        aGrid.Cells [L + xCol, R + xRow] := CS;
    end; {for each row}
  finally
    PasteRows.Free;
  end;
end;

function LastCaption (aCaption: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aCaption) do
  begin
    if aCaption [x] = '.' then
      result := ''
    else
      result := result + aCaption [x];
  end;
end;

function ThisCaption (aCaption: String): String;
var
  dotPos: Integer;
begin
  dotPos := Pos ('.', aCaption);
  if dotPos = 0 then
    result := aCaption
  else
    result := LeftStr (aCaption, dotPos - 1);
end;

function NextFullCaption (aCaption: String): String;
var
  dotPos: Integer;
begin
  dotPos := Pos ('.', aCaption);
  if dotPos = 0 then
    result := ''
  else
    result := Copy (aCaption, dotPos + 1, Length (aCaption));
end;

function RestStr (aPrefix: String; aString: String): String;
begin
  if AnsiStartsStr (aPrefix, aString) then
    result := RightStr (aString, Length (aString) - Length (aPrefix))
  else
    result := aString;
end;

function AsciiToEbcdic (const aString: String): String;
  const Table: array [0..255] of Byte =
  ($00,$01,$02,$03,$37,$2D,$2E,$2F,$16,$05,$25,$0B,$0C,$0D,$0E,$0F
  ,$10,$11,$12,$13,$3C,$3D,$32,$26,$18,$19,$3F,$27,$1C,$1D,$1E,$1F
  ,$40,$5A,$7F,$7B,$5B,$6C,$50,$7D,$4D,$5D,$5C,$4E,$6B,$60,$4B,$61
  ,$F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$7A,$5E,$4C,$7E,$6E,$6F
  ,$7C,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$D1,$D2,$D3,$D4,$D5,$D6
  ,$D7,$D8,$D9,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$AD,$E0,$BD,$5F,$6D
  ,$79,$81,$82,$83,$84,$85,$86,$87,$88,$89,$91,$92,$93,$94,$95,$96
  ,$97,$98,$99,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$C0,$4F,$D0,$A1,$07
  ,$20,$21,$22,$23,$24,$15,$06,$17,$28,$29,$2A,$2B,$2C,$09,$0A,$1B
  ,$30,$31,$1A,$33,$34,$35,$36,$08,$38,$39,$3A,$3B,$04,$14,$3E,$FF
  ,$41,$AA,$4A,$B1,$9F,$B2,$6A,$B5,$BB,$B4,$9A,$8A,$B0,$CA,$AF,$BC
  ,$90,$8F,$EA,$FA,$BE,$A0,$B6,$B3,$9D,$DA,$9B,$8B,$B7,$B8,$B9,$AB
  ,$64,$65,$62,$66,$63,$67,$9E,$68,$74,$71,$72,$73,$78,$75,$76,$77
  ,$AC,$69,$ED,$EE,$EB,$EF,$EC,$BF,$80,$FD,$FE,$FB,$FC,$BA,$AE,$59
  ,$44,$45,$42,$46,$43,$47,$9C,$48,$54,$51,$52,$53,$58,$55,$56,$57
  ,$8C,$49,$CD,$CE,$CB,$CF,$CC,$E1,$70,$DD,$DE,$DB,$DC,$8D,$8E,$DF
  );
  function _AsciiByteToEbcdicByte(const c: Byte): Byte;
  begin
    result := Table[c];
  end; {_AsciiByteToEbcdicByte}
var
  x: Integer;
begin
  SetLength (result, Length (aString));
  for x := 1 to Length (aString) do
    result [x] := Char (_AsciiByteToEbcdicByte (Byte (aString [x])));
end;

function EbcdicToAscii (const aString: String): String;
  const Table: array [0..255] of Byte =
  ($00,$01,$02,$03,$9C,$09,$86,$7F,$97,$8D,$8E,$0B,$0C,$0D,$0E,$0F
  ,$10,$11,$12,$13,$9D,$85,$08,$87,$18,$19,$92,$8F,$1C,$1D,$1E,$1F
  ,$80,$81,$82,$83,$84,$0A,$17,$1B,$88,$89,$8A,$8B,$8C,$05,$06,$07
  ,$90,$91,$16,$93,$94,$95,$96,$04,$98,$99,$9A,$9B,$14,$15,$9E,$1A
  ,$20,$A0,$E2,$E4,$E0,$E1,$E3,$E5,$E7,$F1,$A2,$2E,$3C,$28,$2B,$7C
  ,$26,$E9,$EA,$EB,$E8,$ED,$EE,$EF,$EC,$DF,$21,$24,$2A,$29,$3B,$5E
  ,$2D,$2F,$C2,$C4,$C0,$C1,$C3,$C5,$C7,$D1,$A6,$2C,$25,$5F,$3E,$3F
  ,$F8,$C9,$CA,$CB,$C8,$CD,$CE,$CF,$CC,$60,$3A,$23,$40,$27,$3D,$22
  ,$D8,$61,$62,$63,$64,$65,$66,$67,$68,$69,$AB,$BB,$F0,$FD,$FE,$B1
  ,$B0,$6A,$6B,$6C,$6D,$6E,$6F,$70,$71,$72,$AA,$BA,$E6,$B8,$C6,$A4
  ,$B5,$7E,$73,$74,$75,$76,$77,$78,$79,$7A,$A1,$BF,$D0,$5B,$DE,$AE
  ,$AC,$A3,$A5,$B7,$A9,$A7,$B6,$BC,$BD,$BE,$DD,$A8,$AF,$5D,$B4,$D7
  ,$7B,$41,$42,$43,$44,$45,$46,$47,$48,$49,$AD,$F4,$F6,$F2,$F3,$F5
  ,$7D,$4A,$4B,$4C,$4D,$4E,$4F,$50,$51,$52,$B9,$FB,$FC,$F9,$FA,$FF
  ,$5C,$F7,$53,$54,$55,$56,$57,$58,$59,$5A,$B2,$D4,$D6,$D2,$D3,$D5
  ,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$B3,$DB,$DC,$D9,$DA,$9F
  );
  function _EbcdicByteToAsciiByte(const c: Byte): Byte;
  begin
    result := Table[c];
  end; {_EbcdicByteToAsciiByte}
var
  x: Integer;
begin
  SetLength (result, Length (aString));
  for x := 1 to Length (aString) do
    result [x] := Char (_EbcdicByteToAsciiByte (Byte (aString [x])));
end;

function ExpandRelativeFileName(aMainFileName,
  aToRelateFileName: String): String;
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
  or (ExtractFileDrive(aToRelateFileName) <> '')
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
    result := ExpandFileNameUTF8(ExtractFilePath(aMainFileName)
                              + aToRelateFileName
                             ); { *Converted from ExpandFileName* }
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

function ReadBinaryStringFromFile (aFileName: String): String;
var
  F: file;
  BytesRead: Integer;
  x: Integer;
  s: Integer;
  Buf: array[1..2048] of Char;
begin
  result := '';
  AssignFile (F, aFileName);
  Reset (F, 1);
  BlockRead(F, Buf, SizeOf(Buf), BytesRead);
  while (BytesRead > 0) do
  begin
    s := Length (Result);
    SetLength (result, s + BytesRead);
    for x := 1 to BytesRead do
      result [s + x] := Buf [x];
    BlockRead(F, Buf, SizeOf(Buf), BytesRead);
  end;
  CloseFile (F);
end;

procedure SaveBinaryStringToFile (aFileName: String; aString: String);
var
  F: file;
  Written: Integer;
begin
  AssignFile (F, aFileName);
  Rewrite (F, 1);
  BlockWrite (F, aString[1], Length (aString), Written);
  CloseFile (F);
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
  if (AnsiStartsText('http://', aFileName))
  or (AnsiStartsText('https://', aFileName)) then
  begin
    result := _GetURLAsString (aFileName);
    exit;
  end;
  with TStringList.Create do
  try
    LoadFromFile(aFileName);
    result := Text;
  finally
    Free;
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

const
  B64Table= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function B64EncodeStream(const S: TMemoryStream): AnsiString;
var
  i: integer;
  InBuf: array[0..2] of byte;
  OutBuf: array[0..3] of char;
  MemoryBuf: String;
begin
  SetLength (MemoryBuf, S.Size);
  S.Position := 0;
  S.ReadBuffer(MemoryBuf[1], S.Size);
  SetLength(Result,((S.Size+2) div 3)*4);
  for i:= 1 to ((S.Size+2) div 3) do
  begin
    if S.Size < (i*3) then
      Move(MemoryBuf[(i-1)*3+1],InBuf,S.Size -(i-1)*3)
    else
      Move(MemoryBuf[(i-1)*3+1],InBuf,3);
    OutBuf[0]:= B64Table[((InBuf[0] and $FC) shr 2) + 1];
    OutBuf[1]:= B64Table[(((InBuf[0] and $03) shl 4) or ((InBuf[1] and $F0) shr 4)) + 1];
    OutBuf[2]:= B64Table[(((InBuf[1] and $0F) shl 2) or ((InBuf[2] and $C0) shr 6)) + 1];
    OutBuf[3]:= B64Table[(InBuf[2] and $3F) + 1];
    Move(OutBuf,Result[(i-1)*4+1],4);
  end;
  if (S.Size mod 3)= 1 then
  begin
    Result[Length(Result)-1]:= '=';
    Result[Length(Result)]:= '=';
  end
  else if (S.Size mod 3)= 2 then
    Result[Length(Result)]:= '=';
  MemoryBuf := '';
end;

function ReadB64StringFromFile (aFileName: String): String;
var
  xStream: TMemoryStream;
  xString: String;
begin
  xStream := TMemoryStream.Create;
  try
    xStream.LoadFromFile(aFileName);
    {
    result := B64EncodeStream (xStream);
    }
    SetLength (xString, xStream.Size);
    xStream.Position := 0;
    xStream.ReadBuffer(xString[1], xStream.Size);
    result := EncodeStringBase64(xString);
  finally
    xStream.Free;
  end;
end;

function URLDecode(const S: AnsiString): AnsiString;
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

function URLEncode(const S: AnsiString): AnsiString;
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

function HEXDecode(const S: AnsiString): AnsiString;
var
  Idx: Integer;   // loops thru chars in string
  d: Integer; // index into result
  Hex: AnsiString;    // string of hex characters
  Code: Integer;  // hex character code (-1 on error)
begin
  if (system.Length (S) mod 2) <> 0 then
    raise Exception.Create('HECDecode: Invalid lenght for hex string');
  SetLength (Result, system.Length (S) div 2);
  Hex := '$12';
  Idx := 1;
  d := 1;
  // Loop thru string decoding hex character pairs
  while Idx <= Length(S) do
  begin
    Hex [2] := S [Idx];
    Hex [3] := S [Idx+1];
    Code := SysUtils.StrToIntDef(Hex, -1);
    // check for error and raise exception if found
    if Code = -1 then
      raise SysUtils.EConvertError.Create(
        '(HEXDecode) Invalid hex digit in string: '''
        + Copy (S, Idx, 2)
        + ''' (at position '
        + IntToStr (Idx)
        + ')'
      );
    // decoded OK - add character to result
    Result [d] := AnsiChar (Code);
    Inc(Idx, 2);
    Inc(d);
  end;
end;

function HEXEncode(const S: AnsiString): AnsiString;
var
  Idx: Integer; // loops thru characters in string
  d: Integer;
  st: AnsiString;
begin
  SetLength (Result, 2 * system.Length (S));
  d := 1;
  for Idx := 1 to Length(S) do
  begin
    st := SysUtils.IntToHex(Ord(S[Idx]), 2);
    Result [d] := st [1];
    Result [d+1] := st[2];
    Inc (d,2);
  end;
end;

function HEXRGBToColor(const S: string): TColor;
begin
  result := TColor (StrToInt ( '$'
                             + Copy (S, 5, 2)
                             + Copy (S, 3, 2)
                             + Copy (S, 1, 2)
                             )
                   );
end;

procedure MemoSetSelectedText (Memo: TLzRichEdit; Line: Integer; Column: Integer; Width: Integer);
var
  x: Integer;
  Offset: Integer;
begin
  if not (Memo is TCustomMemo) then
    raise Exception.Create ('First arg is not a TCustomMemo');
  if Line > Memo.Lines.Count then
    raise Exception.Create ('Line out of index for memo');
  Offset := Column - 1;
  x := 0;
  while (x < Line - 1) do
  begin
    Offset := Offset + system.Length (Memo.Lines[x]) + 2;
    Inc (x);
  end;
  Memo.SetFocus;
  Memo.SelStart := Offset;
  Memo.SelLength := Width;
end;

function _StringMatchesRegExp( S, aRegExp: String): Boolean;
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  try
    rx.Expression := aRegExp;
    result := rx.Exec(S);
  finally
    rx.Free;
  end;
end;

function _StringMatchesMask( S, mask: String): Boolean;
var
  sIndex, maskIndex: Integer;
begin
  Result := True; // blatant optimism
  sIndex := 1;
  maskIndex := 1;
  while (sIndex <= system.Length(S))
  and (maskIndex <= system.Length(mask)) do
  begin
    case mask[maskIndex] of
      '?':
      begin
        // matches any character
        Inc( sIndex );
        Inc( maskIndex );
      end; { case '?' }
      '*':
      begin
        // matches 0 or more characters, so need to check for
        // next character in mask
        Inc( maskIndex );
        if maskIndex > System.Length(mask) then
         // * at end matches rest of string
          Exit
        else
          if mask[maskindex] In ['*','?'] then
            raise Exception.Create('Invalid mask');
        while (sIndex <= System.Length(S)) do
        begin
          // look for mask character in S
          while (sIndex <= System.Length(S))
          and (S[sIndex] <> mask[maskIndex])
          do
            Inc( sIndex );
          if sIndex > System.Length(S) then begin
            // character not found, no match
            Result := false;
            Exit;
          end; { if }
          // if rest of string matches rest of mask then OK
          if _StringMatchesMask ( Copy (S, sIndex, system.Length (S))
                                , Copy (mask, maskIndex, system.Length (mask))
                                )
          then
            exit;
          // else try next
          Inc (sIndex);
        end; {while}
      end; { Case '*' }
    else
      if S[sIndex] = mask[maskIndex] then
      begin
        Inc(sIndex);
        Inc(maskIndex);
      end { if }
      else
      begin
        // no match
        result := False;
        exit;
      end;
    end; { Case }
  end; { while }
  // if we have not yet reached the end of the mask it might still be a
  // good match in case the rest of the mask is a '*'
  if maskIndex = System.Length (mask) then
    if mask [maskIndex] = '*' then
      exit;
  // if we have reached the end of both S and mask we have a complete
  // match, otherwise we only have a partial match
  if (sIndex <= System.Length(S))
  or (maskIndex <= System.Length(mask))
  then
    result := false;
end; { _StringMatchesMask }

function StringMatchesMask( S, mask: String; CaseSensitive, useRegExp: Boolean ): Boolean;
var
  sIndex, maskIndex: Integer;
begin
  if not CaseSensitive then
  begin
    S:= AnsiUpperCase(S);
    mask := AnsiUpperCase(mask);
  end; { if not CaseSensitive}
  if useRegExp then
    result := _StringMatchesRegExp (S, mask)
  else
    result := _StringMatchesMask (S, mask);
end;

function SubStringMatch ( aString: String
                        ; var aOffset: Integer
                        ; aSubString: String
                        ): Boolean;
begin
  result := (Copy ( aString
                  , aOffset
                  , system.Length (aSubString)
                  )
            = aSubString
            );
  if (result) then
    aOffset := aOffset + system.Length (aSubString);
end;

function ExtractCopyLibraryName (arg: String): String;
var
  L: Integer;
  x: Integer;
  LastDot: Integer;
begin
  result := arg;
  if (Copy (result, 1, 5) = '=LIB_') then
    result := Copy (result, 6, system.Length (result) - 5);
  L := system.Length (result);
  LastDot := 0;
  for x := 1 to L do
    if result [x] = '.' then
      LastDot := x;
  if LastDot > 0 then
    result := Copy (result, LastDot + 1, 80);
end;

procedure InitScreenClause;
begin
  Height := 24;
  Width := 80;
  Name := '';
  LastTop := Top;
  Top := 0;
  LastLeft := Left;
  Left := 0;
  igLength := 0;
  UserWorkingArea := '';
  UWASeparator := '';
  PictureClause := '';
  ReadOnly := True;
  Lines := 1;
  Columns := 1;
  SkippingLines := 0;
  SkippingColumns := 1;
  OffsetColumns := 0;
  Value := '';
  Dim := False;
  Redefines := False;
end;

procedure FindDirs ( Path: String
                   ; Mask: String
                   ; Recursive: Boolean
                   ; OnHaveDir: TOnHaveDir
                   );
  function RecurseFindDirs ( const Path: String
                           ; const Mask: String
                           ; Recursive: Boolean
                           ): Boolean;
  var
    SRec: TSearchRec;
    retval: Integer;
  begin
    Result := True; // be optimistic
    if Result then
    begin
      retval := FindFirstUTF8(Path + '*.*',faDirectory,SRec); { *Converted from FindFirst* }
      while (retval = 0)
      and result do
      begin
        if (SRec.Attr and faDirectory) <> 0 then
        begin
          if (SRec.Name <> '.')
          and (SRec.Name <> '..')
          then begin
            if Assigned (OnHaveDir) then
              Result := OnHaveDir (SRec.Name);
            if Result and Recursive then
              result := RecurseFindDirs (Path + SRec.Name + '\', Mask, Recursive);
          end;
        end;
        retval := FindNextUTF8(SRec ); { *Converted from FindNext* }
      end;
      FindCloseUTF8(SRec); { *Converted from FindClose* }
    end; {if Result}
  end; { function RecurseFindDirs }
begin
  if Path = '' Then
    GetDir(0, Path);
  if Path [Length (Path)] <> '\' then
    Path := Path + '\';
  if Mask = '' then
    Mask := '*.*';
  RecurseFindDirs (Path, Mask, Recursive);
end;

procedure FindFiles ( Path: String
                    ; Mask: String
                    ; Recursive: Boolean
                    ; OnHaveDir: TOnHaveDir
                    ; OnHaveFile: TOnHaveFile
                    );
  function RecurseFindFiles ( const Path: String
                            ; const Mask: String
                            ; Recursive: Boolean
                            ): Boolean;
  var
    SRec: TSearchRec;
    retval: Integer;
  begin
    Result := True; // be optimistic
    if Assigned (OnHaveDir) then
      Result := OnHaveDir (Path);
    if Result then
    begin
      retval := FindFirstUTF8(Path + Mask,faAnyFile,SRec); { *Converted from FindFirst* }
      while retval = 0 do
      begin
        if (SRec.Attr and (faDirectory or faVolumeID)) = 0 then
        begin
          if Assigned (OnHaveFile) then
          begin
            if not OnHaveFile (Path, SRec) then
            begin
              Result := False;
              Break;
            end;
          end;
        end;
        retval := FindNextUTF8(SRec); { *Converted from FindNext* }
      end;
      FindCloseUTF8(SRec); { *Converted from FindClose* }
    end;

    if Result
    and Recursive then
    begin
      retval := FindFirstUTF8(Path + '*.*',faDirectory,SRec); { *Converted from FindFirst* }
      while (retval = 0)
      and result do
      begin
        if (SRec.Attr and faDirectory) <> 0 then
        begin
          if (SRec.Name <> '.')
          and (SRec.Name <> '..')
          then
            result := RecurseFindFiles (Path + SRec.Name + '\', Mask, Recursive);
        end;
        retval := FindNextUTF8(SRec ); { *Converted from FindNext* }
      end;
      FindCloseUTF8(SRec); { *Converted from FindClose* }
    end; {if Recursive}
  end; { function RecurseFindFiles }
begin
  if Path = '' Then
    GetDir(0, Path);
  if Path [Length (Path)] <> '\' then
    Path := Path + '\';
  if Mask = '' then
    Mask := '*.*';
  RecurseFindFiles (Path, Mask, Recursive);
end;

function GetFileDateTimeAsString (FileName: String): String;
begin
  result := FormatDateTime ('yyyy-mm-dd hh:nn:ss', FileDateToDateTime (FileAgeUTF8(FileName) { *Converted from FileAge* }));
end;

function aToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
  begin
    result := ((Sender as TAction).ActionComponent is TToolbutton);
    if (Sender as TAction).ActionComponent is TMenuItem then
    begin
      result := not ((Sender as TAction).ActionComponent.GetParentComponent is TPopUpMenu);
    end;
  end;
end;

procedure ModifyFontsFor(ctrl: TWinControl);
  procedure ModifyFont(ctrl: TControl);
  var
    f: TFont;
  begin
    if IsPublishedProp(ctrl, 'Parentfont')
      and (GetOrdProp(ctrl, 'Parentfont') = Ord(false))
      and IsPublishedProp(ctrl, 'font')
      then begin
      f := TFont(GetObjectProp(ctrl, 'font', TFont));
      f.Name := 'Symbol';
    end;
  end;
var
  i: Integer;
begin
  ModifyFont(ctrl);
  for i := 0 to ctrl.controlcount - 1 do
    if ctrl.controls[i] is Twincontrol then
      ModifyFontsfor(TWincontrol(ctrl.controls[i]))
    else
      Modifyfont(ctrl.controls[i]);
end;

procedure ModifyColorFor(ctrl: TWinControl);
  procedure ModifyColor(ctrl: TWinControl);
  var
    f: TColor;
  begin
{    if IsPublishedProp(ctrl, 'ParentColor')
    and (GetOrdProp(ctrl, 'ParentColor') = Ord(false))
    and IsPublishedProp(ctrl, 'Color')
    then begin
    }
      ctrl.Brush.Color := clRed;
      {
    end;
    }
  end;
var
  i: Integer;
begin
  ModifyColor(ctrl);
  for i := 0 to ctrl.controlcount - 1 do
    if ctrl.controls[i] is Twincontrol then
      ModifyColorFor(TWincontrol(ctrl.controls[i]));
end;


end.

