{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
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
   , Graphics, LazFileUtils
   , Messages
   , Types
//   , VirtualTrees
   ;

resourcestring
  S_REGEXP_LINK = '(?i)(FTP|HTTPS?|FILE|DOC)://([_a-z\d\-]+(\.[_a-z\d\-]+)+)((/[ _a-z\d\-\\\.]+)+)*(\?[a-z0-9=&]+)?';
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
function B64EncodeStream(const S: TMemoryStream): AnsiString;
function URLDecode(const S: String): String;
function URLEncode(const S: String): String;
function HEXDecode(const S: AnsiString): AnsiString;
function HEXEncode(const S: AnsiString): AnsiString;
function HEXRGBToColor(const S: string): TColor;

procedure MemoSetSelectedText (Memo: TMemo; Line: Integer; Column: Integer; Width: Integer);
function StringMatchesMask( S, mask: String; CaseSensitive, useRegExp: Boolean ): Boolean;
function SubStringMatch ( aString: String
                        ; var aOffset: Integer
                        ; aSubString: String
                        ): Boolean;
function GetFileDateTimeAsString (FileName: String): String;

implementation

uses StrUtils
   , Menus
   , TypInfo
   , ClipBrd
   , LCLIntf
   , LCLType
   , LMessages
   , versiontypes
   , versionresource
   , Registry
   , RegExpr
   , HashUtilz
   , IdHTTP
   , base64
   ;


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

function URLDecode(const S: String): String;
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

function URLEncode(const S: String): String;
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

procedure MemoSetSelectedText (Memo: TMemo; Line: Integer; Column: Integer; Width: Integer);
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

function GetFileDateTimeAsString (FileName: String): String;
begin
  result := FormatDateTime ('yyyy-mm-dd hh:nn:ss', FileDateToDateTime (FileAgeUTF8(FileName) { *Converted from FileAge* }));
end;

end.

