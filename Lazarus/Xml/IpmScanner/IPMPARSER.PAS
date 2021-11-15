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
unit IPMPARSER;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes
   , ParserClasses
   , CustParser
   , Ipmz
   ;


type TIpmParser = class (TCustParser)
private
  OfIdString: String;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  destructor Destroy; override;
end;
const _BY = 257;
const _COMP = 258;
const _CONDITIONLEVEL = 259;
const _DEPENDING = 260;
const _DISPLAY = 261;
const _IGNORE = 262;
const _IN = 263;
const _INDEXED = 264;
const _INTEGER = 265;
const _IS = 266;
const _LEADING = 267;
const _NATIVE_2 = 268;
const _NATIVE_4 = 269;
const _NATIVE_8 = 270;
const _NOID = 271;
const _OCCURS = 272;
const _OF = 273;
const _ON = 274;
const _PICTURE = 275;
const _PICTURECLAUSE = 276;
const _REDEFINES = 277;
const _SEPARATE = 278;
const _SIGN = 279;
const _SPACE = 280;
const _SPACES = 281;
const _STRING = 282;
const _TERMINATOR = 283;
const _THROUGH = 284;
const _TIMES = 285;
const _TO = 286;
const _TRAILING = 287;
const _USAGE = 288;
const _VALUE = 289;
const _ZERO = 290;
const _ZEROES = 291;



implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TIpmParser.Prepare;
begin
  inherited Prepare;
end;

destructor TIpmParser.Destroy;
begin
  inherited Destroy;
end;

function TIpmParser.yylex: Integer;
begin
  result := inherited yylex;
end;

const yymaxdepth = 1024;

function TIpmParser.yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;
    x, y, z: Integer;
    SubString: String;

procedure yyaction ( yyruleno : Integer );
var
  x, y, z: Integer;
  SubString: String;
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         
         BaseIpmItem := nil;
         PreviousIpmItem := nil;
         CurrentIpmItem := nil;
         
       end;
   2 : begin
         yyval := yyv[yysp-1];
       end;
   3 : begin
       end;
   4 : begin
         yyval := yyv[yysp-0];
       end;
   5 : begin
         yyval := yyv[yysp-0];
       end;
   6 : begin
         yyval := yyv[yysp-1];
       end;
   7 : begin
         
         CurrentIpmItem := TIpmItem.Create;
         CurrentIpmItem.minOccurs := 1;
         CurrentIpmItem.Occurs := 1;
         
       end;
   8 : begin
         
         if BaseIpmItem = nil then
         BaseIpmItem := CurrentIpmItem
         else
         begin
         if (CurrentIpmItem.Redefines)
         and (CurrentIpmItem.Level = BaseIpmItem.Level) then
         begin
         CurrentIpmItem.Free;
         raise Exception.Create ('Unable to deal with redefine on first level');
         end;
         CurrentIpmItem.LinkToParent (PreviousIpmItem);
         end;
         PreviousIpmItem := CurrentIpmItem;
         
       end;
   9 : begin
         
         CurrentIpmItem.Name := yyv[yysp-3].TokenString;
         CurrentIpmItem.Level := yyv[yysp-4].yy.yyInteger;
         
       end;
  10 : begin
         
         CurrentIpmItem.Name := 'filler';
         CurrentIpmItem.Level := yyv[yysp-3].yy.yyInteger;
         
       end;
  11 : begin
       end;
  12 : begin
         yyval := yyv[yysp-0];
       end;
  13 : begin
         yyval := yyv[yysp-0];
       end;
  14 : begin
         yyval := yyv[yysp-1];
       end;
  15 : begin
         
         
       end;
  16 : begin
         
         CurrentIpmItem.Comp := True;
         
       end;
  17 : begin
         
         CurrentIpmItem.Display := True;
         
       end;
  18 : begin
         
         CurrentIpmItem.Comp := True;
         CurrentIpmItem.PictureClause := 'S9(04)';
         igLength := PictureClauseToLength (PictureClause);
         
       end;
  19 : begin
         
         CurrentIpmItem.Comp := True;
         CurrentIpmItem.PictureClause := 'S9(09)';
         igLength := PictureClauseToLength (PictureClause);
         
       end;
  20 : begin
         
         CurrentIpmItem.Comp := True;
         CurrentIpmItem.PictureClause := 'S9(18)';
         igLength := PictureClauseToLength (PictureClause);
         
       end;
  21 : begin
         yyval := yyv[yysp-2];
       end;
  22 : begin
         yyval := yyv[yysp-1];
       end;
  23 : begin
         
         CurrentIpmItem.PictureClause := yyv[yysp-0].TokenString;
         if (CurrentIpmItem.PictureClause [1] = 's')
         or (CurrentIpmItem.PictureClause [1] = 'S') then
         CurrentIpmItem.Signed := True;
         igLength := PictureClauseToLength (PictureClause);
         
       end;
  24 : begin
         
         CurrentIpmItem.Redefines := True;
         
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         yyval := yyv[yysp-1];
       end;
  27 : begin
         
         CurrentIpmItem.Occurs := yyv[yysp-1].yy.yyInteger;
         
       end;
  28 : begin
         
         CurrentIpmItem.minOccurs := yyv[yysp-3].yy.yyInteger;
         CurrentIpmItem.Occurs := yyv[yysp-1].yy.yyInteger;
         
       end;
  29 : begin
         yyval := yyv[yysp-2];
       end;
  30 : begin
         yyval := yyv[yysp-2];
       end;
  31 : begin
       end;
  32 : begin
         yyval := yyv[yysp-0];
       end;
  33 : begin
       end;
  34 : begin
         yyval := yyv[yysp-0];
       end;
  35 : begin
       end;
  36 : begin
         
         CurrentIpmItem.SignLeading := True;
         
       end;
  37 : begin
         
         CurrentIpmItem.SignLeading := False;
         
       end;
  38 : begin
       end;
  39 : begin
         yyval := yyv[yysp-0];
       end;
  40 : begin
       end;
  41 : begin
         
         CurrentIpmItem.SignSeparate := True;
         
       end;
  42 : begin
       end;
  43 : begin
         yyval := yyv[yysp-0];
       end;
  44 : begin
         yyval := yyv[yysp-0];
       end;
  45 : begin
         yyval := yyv[yysp-2];
       end;
  46 : begin
         
         UserWorkingArea := UserWorkingArea
         + UWASeparator
         + yyv[yysp-0].TokenString
         ;
         UWASeparator := ' of ';
         
       end;
  47 : begin
         yyval := yyv[yysp-0];
       end;
  48 : begin
         yyval := yyv[yysp-0];
       end;
  49 : begin
       end;
  50 : begin
         yyval := yyv[yysp-0];
       end;
  51 : begin
         yyval := yyv[yysp-0];
       end;
  52 : begin
         yyval := yyv[yysp-1];
       end;
  53 : begin
         yyval := yyv[yysp-5];
       end;
  54 : begin
         yyval := yyv[yysp-0];
       end;
  55 : begin
         yyval := yyv[yysp-1];
       end;
  56 : begin
         
         CurrentIpmItem.Level88Values.Add (yyv[yysp-0].yyString);
         
       end;
  57 : begin
         
         CurrentIpmItem.Level88Values.Add (yyv[yysp-0].TokenString);
         
       end;
  58 : begin
         yyval := yyv[yysp-2];
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 193;
yyngotos  = 45;
yynstates = 77;
yynrules  = 58;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -7 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
  ( sym: 265; act: 8 ),
{ 4: }
{ 5: }
  ( sym: 0; act: -4 ),
  ( sym: 265; act: -7 ),
{ 6: }
{ 7: }
{ 8: }
  ( sym: 258; act: 13 ),
  ( sym: 261; act: 14 ),
  ( sym: 268; act: 15 ),
  ( sym: 269; act: 16 ),
  ( sym: 270; act: 17 ),
  ( sym: 271; act: 18 ),
  ( sym: 272; act: 19 ),
  ( sym: 275; act: 20 ),
  ( sym: 277; act: 21 ),
  ( sym: 279; act: 22 ),
  ( sym: 288; act: 23 ),
  ( sym: 283; act: -11 ),
{ 9: }
{ 10: }
{ 11: }
  ( sym: 258; act: 13 ),
  ( sym: 261; act: 14 ),
  ( sym: 268; act: 15 ),
  ( sym: 269; act: 16 ),
  ( sym: 270; act: 17 ),
  ( sym: 272; act: 19 ),
  ( sym: 275; act: 20 ),
  ( sym: 277; act: 21 ),
  ( sym: 279; act: 22 ),
  ( sym: 288; act: 23 ),
  ( sym: 283; act: -12 ),
{ 12: }
  ( sym: 283; act: 25 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: 258; act: 13 ),
  ( sym: 261; act: 14 ),
  ( sym: 268; act: 15 ),
  ( sym: 269; act: 16 ),
  ( sym: 270; act: 17 ),
  ( sym: 272; act: 19 ),
  ( sym: 275; act: 20 ),
  ( sym: 277; act: 21 ),
  ( sym: 279; act: 22 ),
  ( sym: 288; act: 23 ),
  ( sym: 283; act: -11 ),
{ 19: }
  ( sym: 260; act: 29 ),
  ( sym: 264; act: 30 ),
  ( sym: 265; act: 31 ),
{ 20: }
  ( sym: 266; act: 33 ),
  ( sym: 276; act: -33 ),
{ 21: }
  ( sym: 271; act: 34 ),
{ 22: }
  ( sym: 267; act: 36 ),
  ( sym: 287; act: 37 ),
  ( sym: 258; act: -35 ),
  ( sym: 261; act: -35 ),
  ( sym: 268; act: -35 ),
  ( sym: 269; act: -35 ),
  ( sym: 270; act: -35 ),
  ( sym: 272; act: -35 ),
  ( sym: 275; act: -35 ),
  ( sym: 277; act: -35 ),
  ( sym: 278; act: -35 ),
  ( sym: 279; act: -35 ),
  ( sym: 283; act: -35 ),
  ( sym: 288; act: -35 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: 259; act: 41 ),
  ( sym: 0; act: -49 ),
  ( sym: 265; act: -49 ),
{ 26: }
  ( sym: 283; act: 42 ),
{ 27: }
{ 28: }
  ( sym: 260; act: 29 ),
  ( sym: 264; act: 30 ),
  ( sym: 265; act: 31 ),
  ( sym: 258; act: -22 ),
  ( sym: 261; act: -22 ),
  ( sym: 268; act: -22 ),
  ( sym: 269; act: -22 ),
  ( sym: 270; act: -22 ),
  ( sym: 272; act: -22 ),
  ( sym: 275; act: -22 ),
  ( sym: 277; act: -22 ),
  ( sym: 279; act: -22 ),
  ( sym: 283; act: -22 ),
  ( sym: 288; act: -22 ),
{ 29: }
  ( sym: 274; act: 45 ),
  ( sym: 271; act: -38 ),
{ 30: }
  ( sym: 257; act: 47 ),
  ( sym: 271; act: -31 ),
{ 31: }
  ( sym: 285; act: 49 ),
  ( sym: 286; act: 50 ),
  ( sym: 258; act: -42 ),
  ( sym: 260; act: -42 ),
  ( sym: 261; act: -42 ),
  ( sym: 264; act: -42 ),
  ( sym: 265; act: -42 ),
  ( sym: 268; act: -42 ),
  ( sym: 269; act: -42 ),
  ( sym: 270; act: -42 ),
  ( sym: 272; act: -42 ),
  ( sym: 275; act: -42 ),
  ( sym: 277; act: -42 ),
  ( sym: 279; act: -42 ),
  ( sym: 283; act: -42 ),
  ( sym: 288; act: -42 ),
{ 32: }
  ( sym: 276; act: 51 ),
{ 33: }
{ 34: }
{ 35: }
  ( sym: 278; act: 53 ),
  ( sym: 258; act: -40 ),
  ( sym: 261; act: -40 ),
  ( sym: 268; act: -40 ),
  ( sym: 269; act: -40 ),
  ( sym: 270; act: -40 ),
  ( sym: 272; act: -40 ),
  ( sym: 275; act: -40 ),
  ( sym: 277; act: -40 ),
  ( sym: 279; act: -40 ),
  ( sym: 283; act: -40 ),
  ( sym: 288; act: -40 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
  ( sym: 259; act: 41 ),
  ( sym: 0; act: -50 ),
  ( sym: 265; act: -50 ),
{ 40: }
{ 41: }
  ( sym: 271; act: 55 ),
{ 42: }
  ( sym: 259; act: 41 ),
  ( sym: 0; act: -49 ),
  ( sym: 265; act: -49 ),
{ 43: }
{ 44: }
  ( sym: 271; act: 59 ),
{ 45: }
{ 46: }
  ( sym: 271; act: 59 ),
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: 265; act: 61 ),
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
  ( sym: 289; act: 62 ),
{ 56: }
{ 57: }
{ 58: }
  ( sym: 263; act: 64 ),
  ( sym: 273; act: 65 ),
  ( sym: 258; act: -29 ),
  ( sym: 260; act: -29 ),
  ( sym: 261; act: -29 ),
  ( sym: 264; act: -29 ),
  ( sym: 265; act: -29 ),
  ( sym: 268; act: -29 ),
  ( sym: 269; act: -29 ),
  ( sym: 270; act: -29 ),
  ( sym: 272; act: -29 ),
  ( sym: 275; act: -29 ),
  ( sym: 277; act: -29 ),
  ( sym: 279; act: -29 ),
  ( sym: 283; act: -29 ),
  ( sym: 288; act: -29 ),
{ 59: }
{ 60: }
  ( sym: 263; act: 64 ),
  ( sym: 273; act: 65 ),
  ( sym: 258; act: -30 ),
  ( sym: 260; act: -30 ),
  ( sym: 261; act: -30 ),
  ( sym: 264; act: -30 ),
  ( sym: 265; act: -30 ),
  ( sym: 268; act: -30 ),
  ( sym: 269; act: -30 ),
  ( sym: 270; act: -30 ),
  ( sym: 272; act: -30 ),
  ( sym: 275; act: -30 ),
  ( sym: 277; act: -30 ),
  ( sym: 279; act: -30 ),
  ( sym: 283; act: -30 ),
  ( sym: 288; act: -30 ),
{ 61: }
  ( sym: 285; act: 49 ),
  ( sym: 258; act: -42 ),
  ( sym: 260; act: -42 ),
  ( sym: 261; act: -42 ),
  ( sym: 264; act: -42 ),
  ( sym: 265; act: -42 ),
  ( sym: 268; act: -42 ),
  ( sym: 269; act: -42 ),
  ( sym: 270; act: -42 ),
  ( sym: 272; act: -42 ),
  ( sym: 275; act: -42 ),
  ( sym: 277; act: -42 ),
  ( sym: 279; act: -42 ),
  ( sym: 283; act: -42 ),
  ( sym: 288; act: -42 ),
{ 62: }
  ( sym: 266; act: 33 ),
  ( sym: 265; act: -33 ),
  ( sym: 282; act: -33 ),
{ 63: }
  ( sym: 271; act: 59 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: 265; act: 71 ),
  ( sym: 282; act: 72 ),
{ 68: }
{ 69: }
  ( sym: 284; act: 73 ),
  ( sym: 265; act: -54 ),
  ( sym: 282; act: -54 ),
  ( sym: 283; act: -54 ),
{ 70: }
  ( sym: 265; act: 71 ),
  ( sym: 282; act: 72 ),
  ( sym: 283; act: 75 ),
{ 71: }
{ 72: }
{ 73: }
  ( sym: 265; act: 71 ),
  ( sym: 282; act: 72 ),
{ 74: }
  ( sym: 284; act: 73 ),
  ( sym: 265; act: -55 ),
  ( sym: 282; act: -55 ),
  ( sym: 283; act: -55 ),
{ 75: }
{ 76: }
  ( sym: 284; act: 73 ),
  ( sym: 265; act: -58 ),
  ( sym: 282; act: -58 ),
  ( sym: 283; act: -58 )
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -4; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -8; act: 3 ),
  ( sym: -6; act: 4 ),
  ( sym: -5; act: 5 ),
  ( sym: -3; act: 6 ),
{ 2: }
{ 3: }
  ( sym: -7; act: 7 ),
{ 4: }
{ 5: }
  ( sym: -8; act: 3 ),
  ( sym: -6; act: 9 ),
{ 6: }
{ 7: }
{ 8: }
  ( sym: -12; act: 10 ),
  ( sym: -11; act: 11 ),
  ( sym: -9; act: 12 ),
{ 9: }
{ 10: }
{ 11: }
  ( sym: -12; act: 24 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -12; act: 10 ),
  ( sym: -11; act: 11 ),
  ( sym: -9; act: 26 ),
{ 19: }
  ( sym: -17; act: 27 ),
  ( sym: -15; act: 28 ),
{ 20: }
  ( sym: -16; act: 32 ),
{ 21: }
{ 22: }
  ( sym: -13; act: 35 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: -25; act: 38 ),
  ( sym: -24; act: 39 ),
  ( sym: -10; act: 40 ),
{ 26: }
{ 27: }
{ 28: }
  ( sym: -17; act: 43 ),
{ 29: }
  ( sym: -19; act: 44 ),
{ 30: }
  ( sym: -21; act: 46 ),
{ 31: }
  ( sym: -18; act: 48 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -14; act: 52 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
  ( sym: -25; act: 54 ),
{ 40: }
{ 41: }
{ 42: }
  ( sym: -25; act: 38 ),
  ( sym: -24; act: 39 ),
  ( sym: -10; act: 56 ),
{ 43: }
{ 44: }
  ( sym: -22; act: 57 ),
  ( sym: -20; act: 58 ),
{ 45: }
{ 46: }
  ( sym: -22; act: 57 ),
  ( sym: -20; act: 60 ),
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
{ 57: }
{ 58: }
  ( sym: -23; act: 63 ),
{ 59: }
{ 60: }
  ( sym: -23; act: 63 ),
{ 61: }
  ( sym: -18; act: 66 ),
{ 62: }
  ( sym: -16; act: 67 ),
{ 63: }
  ( sym: -22; act: 68 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: -27; act: 69 ),
  ( sym: -26; act: 70 ),
{ 68: }
{ 69: }
{ 70: }
  ( sym: -27; act: 74 ),
{ 71: }
{ 72: }
{ 73: }
  ( sym: -27; act: 76 )
{ 74: }
{ 75: }
{ 76: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } -5,
{ 5: } 0,
{ 6: } -2,
{ 7: } -8,
{ 8: } 0,
{ 9: } -6,
{ 10: } -13,
{ 11: } 0,
{ 12: } 0,
{ 13: } -16,
{ 14: } -17,
{ 15: } -18,
{ 16: } -19,
{ 17: } -20,
{ 18: } 0,
{ 19: } 0,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -15,
{ 24: } -14,
{ 25: } 0,
{ 26: } 0,
{ 27: } -25,
{ 28: } 0,
{ 29: } 0,
{ 30: } 0,
{ 31: } 0,
{ 32: } 0,
{ 33: } -34,
{ 34: } -24,
{ 35: } 0,
{ 36: } -36,
{ 37: } -37,
{ 38: } -51,
{ 39: } 0,
{ 40: } -10,
{ 41: } 0,
{ 42: } 0,
{ 43: } -26,
{ 44: } 0,
{ 45: } -39,
{ 46: } 0,
{ 47: } -32,
{ 48: } -27,
{ 49: } -43,
{ 50: } 0,
{ 51: } -23,
{ 52: } -21,
{ 53: } -41,
{ 54: } -52,
{ 55: } 0,
{ 56: } -9,
{ 57: } -44,
{ 58: } 0,
{ 59: } -46,
{ 60: } 0,
{ 61: } 0,
{ 62: } 0,
{ 63: } 0,
{ 64: } -47,
{ 65: } -48,
{ 66: } -28,
{ 67: } 0,
{ 68: } -45,
{ 69: } 0,
{ 70: } 0,
{ 71: } -57,
{ 72: } -56,
{ 73: } 0,
{ 74: } 0,
{ 75: } -53,
{ 76: } 0
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 3,
{ 3: } 4,
{ 4: } 5,
{ 5: } 5,
{ 6: } 7,
{ 7: } 7,
{ 8: } 7,
{ 9: } 19,
{ 10: } 19,
{ 11: } 19,
{ 12: } 30,
{ 13: } 31,
{ 14: } 31,
{ 15: } 31,
{ 16: } 31,
{ 17: } 31,
{ 18: } 31,
{ 19: } 42,
{ 20: } 45,
{ 21: } 47,
{ 22: } 48,
{ 23: } 62,
{ 24: } 62,
{ 25: } 62,
{ 26: } 65,
{ 27: } 66,
{ 28: } 66,
{ 29: } 80,
{ 30: } 82,
{ 31: } 84,
{ 32: } 100,
{ 33: } 101,
{ 34: } 101,
{ 35: } 101,
{ 36: } 113,
{ 37: } 113,
{ 38: } 113,
{ 39: } 113,
{ 40: } 116,
{ 41: } 116,
{ 42: } 117,
{ 43: } 120,
{ 44: } 120,
{ 45: } 121,
{ 46: } 121,
{ 47: } 122,
{ 48: } 122,
{ 49: } 122,
{ 50: } 122,
{ 51: } 123,
{ 52: } 123,
{ 53: } 123,
{ 54: } 123,
{ 55: } 123,
{ 56: } 124,
{ 57: } 124,
{ 58: } 124,
{ 59: } 140,
{ 60: } 140,
{ 61: } 156,
{ 62: } 171,
{ 63: } 174,
{ 64: } 175,
{ 65: } 175,
{ 66: } 175,
{ 67: } 175,
{ 68: } 177,
{ 69: } 177,
{ 70: } 181,
{ 71: } 184,
{ 72: } 184,
{ 73: } 184,
{ 74: } 186,
{ 75: } 190,
{ 76: } 190
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 2,
{ 2: } 3,
{ 3: } 4,
{ 4: } 4,
{ 5: } 6,
{ 6: } 6,
{ 7: } 6,
{ 8: } 18,
{ 9: } 18,
{ 10: } 18,
{ 11: } 29,
{ 12: } 30,
{ 13: } 30,
{ 14: } 30,
{ 15: } 30,
{ 16: } 30,
{ 17: } 30,
{ 18: } 41,
{ 19: } 44,
{ 20: } 46,
{ 21: } 47,
{ 22: } 61,
{ 23: } 61,
{ 24: } 61,
{ 25: } 64,
{ 26: } 65,
{ 27: } 65,
{ 28: } 79,
{ 29: } 81,
{ 30: } 83,
{ 31: } 99,
{ 32: } 100,
{ 33: } 100,
{ 34: } 100,
{ 35: } 112,
{ 36: } 112,
{ 37: } 112,
{ 38: } 112,
{ 39: } 115,
{ 40: } 115,
{ 41: } 116,
{ 42: } 119,
{ 43: } 119,
{ 44: } 120,
{ 45: } 120,
{ 46: } 121,
{ 47: } 121,
{ 48: } 121,
{ 49: } 121,
{ 50: } 122,
{ 51: } 122,
{ 52: } 122,
{ 53: } 122,
{ 54: } 122,
{ 55: } 123,
{ 56: } 123,
{ 57: } 123,
{ 58: } 139,
{ 59: } 139,
{ 60: } 155,
{ 61: } 170,
{ 62: } 173,
{ 63: } 174,
{ 64: } 174,
{ 65: } 174,
{ 66: } 174,
{ 67: } 176,
{ 68: } 176,
{ 69: } 180,
{ 70: } 183,
{ 71: } 183,
{ 72: } 183,
{ 73: } 185,
{ 74: } 189,
{ 75: } 189,
{ 76: } 193
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 7,
{ 3: } 7,
{ 4: } 8,
{ 5: } 8,
{ 6: } 10,
{ 7: } 10,
{ 8: } 10,
{ 9: } 13,
{ 10: } 13,
{ 11: } 13,
{ 12: } 14,
{ 13: } 14,
{ 14: } 14,
{ 15: } 14,
{ 16: } 14,
{ 17: } 14,
{ 18: } 14,
{ 19: } 17,
{ 20: } 19,
{ 21: } 20,
{ 22: } 20,
{ 23: } 21,
{ 24: } 21,
{ 25: } 21,
{ 26: } 24,
{ 27: } 24,
{ 28: } 24,
{ 29: } 25,
{ 30: } 26,
{ 31: } 27,
{ 32: } 28,
{ 33: } 28,
{ 34: } 28,
{ 35: } 28,
{ 36: } 29,
{ 37: } 29,
{ 38: } 29,
{ 39: } 29,
{ 40: } 30,
{ 41: } 30,
{ 42: } 30,
{ 43: } 33,
{ 44: } 33,
{ 45: } 35,
{ 46: } 35,
{ 47: } 37,
{ 48: } 37,
{ 49: } 37,
{ 50: } 37,
{ 51: } 37,
{ 52: } 37,
{ 53: } 37,
{ 54: } 37,
{ 55: } 37,
{ 56: } 37,
{ 57: } 37,
{ 58: } 37,
{ 59: } 38,
{ 60: } 38,
{ 61: } 39,
{ 62: } 40,
{ 63: } 41,
{ 64: } 42,
{ 65: } 42,
{ 66: } 42,
{ 67: } 42,
{ 68: } 44,
{ 69: } 44,
{ 70: } 44,
{ 71: } 45,
{ 72: } 45,
{ 73: } 45,
{ 74: } 46,
{ 75: } 46,
{ 76: } 46
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 6,
{ 2: } 6,
{ 3: } 7,
{ 4: } 7,
{ 5: } 9,
{ 6: } 9,
{ 7: } 9,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 13,
{ 12: } 13,
{ 13: } 13,
{ 14: } 13,
{ 15: } 13,
{ 16: } 13,
{ 17: } 13,
{ 18: } 16,
{ 19: } 18,
{ 20: } 19,
{ 21: } 19,
{ 22: } 20,
{ 23: } 20,
{ 24: } 20,
{ 25: } 23,
{ 26: } 23,
{ 27: } 23,
{ 28: } 24,
{ 29: } 25,
{ 30: } 26,
{ 31: } 27,
{ 32: } 27,
{ 33: } 27,
{ 34: } 27,
{ 35: } 28,
{ 36: } 28,
{ 37: } 28,
{ 38: } 28,
{ 39: } 29,
{ 40: } 29,
{ 41: } 29,
{ 42: } 32,
{ 43: } 32,
{ 44: } 34,
{ 45: } 34,
{ 46: } 36,
{ 47: } 36,
{ 48: } 36,
{ 49: } 36,
{ 50: } 36,
{ 51: } 36,
{ 52: } 36,
{ 53: } 36,
{ 54: } 36,
{ 55: } 36,
{ 56: } 36,
{ 57: } 36,
{ 58: } 37,
{ 59: } 37,
{ 60: } 38,
{ 61: } 39,
{ 62: } 40,
{ 63: } 41,
{ 64: } 41,
{ 65: } 41,
{ 66: } 41,
{ 67: } 43,
{ 68: } 43,
{ 69: } 43,
{ 70: } 44,
{ 71: } 44,
{ 72: } 44,
{ 73: } 45,
{ 74: } 45,
{ 75: } 45,
{ 76: } 45
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -4 ),
{ 2: } ( len: 2; sym: -2 ),
{ 3: } ( len: 0; sym: -3 ),
{ 4: } ( len: 1; sym: -3 ),
{ 5: } ( len: 1; sym: -5 ),
{ 6: } ( len: 2; sym: -5 ),
{ 7: } ( len: 0; sym: -8 ),
{ 8: } ( len: 2; sym: -6 ),
{ 9: } ( len: 5; sym: -7 ),
{ 10: } ( len: 4; sym: -7 ),
{ 11: } ( len: 0; sym: -9 ),
{ 12: } ( len: 1; sym: -9 ),
{ 13: } ( len: 1; sym: -11 ),
{ 14: } ( len: 2; sym: -11 ),
{ 15: } ( len: 1; sym: -12 ),
{ 16: } ( len: 1; sym: -12 ),
{ 17: } ( len: 1; sym: -12 ),
{ 18: } ( len: 1; sym: -12 ),
{ 19: } ( len: 1; sym: -12 ),
{ 20: } ( len: 1; sym: -12 ),
{ 21: } ( len: 3; sym: -12 ),
{ 22: } ( len: 2; sym: -12 ),
{ 23: } ( len: 3; sym: -12 ),
{ 24: } ( len: 2; sym: -12 ),
{ 25: } ( len: 1; sym: -15 ),
{ 26: } ( len: 2; sym: -15 ),
{ 27: } ( len: 2; sym: -17 ),
{ 28: } ( len: 4; sym: -17 ),
{ 29: } ( len: 3; sym: -17 ),
{ 30: } ( len: 3; sym: -17 ),
{ 31: } ( len: 0; sym: -21 ),
{ 32: } ( len: 1; sym: -21 ),
{ 33: } ( len: 0; sym: -16 ),
{ 34: } ( len: 1; sym: -16 ),
{ 35: } ( len: 0; sym: -13 ),
{ 36: } ( len: 1; sym: -13 ),
{ 37: } ( len: 1; sym: -13 ),
{ 38: } ( len: 0; sym: -19 ),
{ 39: } ( len: 1; sym: -19 ),
{ 40: } ( len: 0; sym: -14 ),
{ 41: } ( len: 1; sym: -14 ),
{ 42: } ( len: 0; sym: -18 ),
{ 43: } ( len: 1; sym: -18 ),
{ 44: } ( len: 1; sym: -20 ),
{ 45: } ( len: 3; sym: -20 ),
{ 46: } ( len: 1; sym: -22 ),
{ 47: } ( len: 1; sym: -23 ),
{ 48: } ( len: 1; sym: -23 ),
{ 49: } ( len: 0; sym: -10 ),
{ 50: } ( len: 1; sym: -10 ),
{ 51: } ( len: 1; sym: -24 ),
{ 52: } ( len: 2; sym: -24 ),
{ 53: } ( len: 6; sym: -25 ),
{ 54: } ( len: 1; sym: -26 ),
{ 55: } ( len: 2; sym: -26 ),
{ 56: } ( len: 1; sym: -27 ),
{ 57: } ( len: 1; sym: -27 ),
{ 58: } ( len: 3; sym: -27 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);

end.
