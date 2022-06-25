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

unit proto3Parser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , xmlio, Xsdz, Wsdlz
   ;


{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
type TProto3Parser = class (TCustParser)
private
  OfIdString: String;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  wsdl: TWsdl;
  syntax_version, package_name, proto_reserved: string;
  reserved_statements, import_statements, option_statements: TJBStringList;
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  constructor Create (aOwner: TObject); override;
  destructor Destroy; override;
end;
const _LINEENDING = 257;
const _TERMINATOR = 258;
const _IS = 259;
const _DOT = 260;
const _COMMA = 261;
const _LT = 262;
const _GT = 263;
const _OPENPARENTHESIS = 264;
const _CLOSEPARENTHESIS = 265;
const _OPENBRACKET = 266;
const _CLOSEBRACKET = 267;
const _OPENBRACE = 268;
const _CLOSEBRACE = 269;
const _PLUS = 270;
const _MINUS = 271;
const _DECIMALLIT = 272;
const _OCTALLIT = 273;
const _HEXLIT = 274;
const _FLOATLIT = 275;
const _BOOLLIT = 276;
const _STRINGLIT = 277;
const _IDENT = 278;
const _FULLIDENT = 279;
const _BOOL = 280;
const _BYTES = 281;
const _DOUBLE = 282;
const _ENUM = 283;
const _ENUMTYPE = 284;
const _FIXED32 = 285;
const _FIXED64 = 286;
const _FLOAT = 287;
const _IMPORT = 288;
const _INT32 = 289;
const _INT64 = 290;
const _MAP = 291;
const _MAX = 292;
const _MESSAGE = 293;
const _MESSAGETYPE = 294;
const _ONEOF = 295;
const _OPTION = 296;
const _PACKAGE = 297;
const _PACKED = 298;
const _PUBLIC = 299;
const _REPEATED = 300;
const _RESERVED = 301;
const _RETURNS = 302;
const _RPC = 303;
const _SERVICE = 304;
const _SFIXED32 = 305;
const _SFIXED64 = 306;
const _SINT32 = 307;
const _SINT64 = 308;
const _STREAM = 309;
const _STRING = 310;
const _SYNTAX = 311;
const _TO = 312;
const _UINT32 = 313;
const _UINT64 = 314;
const _WEAK = 315;
const _IGNORE = 316;



implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TProto3Parser.Prepare;
begin
  reserved_statements.Clear;
  import_statements.Clear;
  option_statements.Clear;
end;

destructor TProto3Parser.Destroy;
begin
  reserved_statements.free;
  import_statements.free;
  option_statements.free;
  inherited Destroy;
end;

constructor TProto3Parser.Create(aOwner: TObject);
begin
  inherited Create (aOwner);
  reserved_statements := TJBStringList.Create;
  import_statements := TJBStringList.Create;
  option_statements := TJBStringList.Create;
end;

function TProto3Parser.yylex: Integer;
begin
  result := inherited yylex;
end;
const yymaxdepth = 1024;

function TProto3Parser.yyparse : Integer;

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
         
         ShowMessage ( 'syntax: ' + syntax_version + LineEnding
         + 'package: ' + package_name + LineEnding
         + 'Import: ' + import_statements.Text + LineEnding
         + 'Options: ' + option_statements.Text + LineEnding
         )
         
       end;
   2 : begin
         
         syntax_version := yyv[yysp-1].yyString;
         
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
         yyval := yyv[yysp-0];
       end;
   8 : begin
         yyval := yyv[yysp-0];
       end;
   9 : begin
         yyval := yyv[yysp-0];
       end;
  10 : begin
         yyval := yyv[yysp-0];
       end;
  11 : begin
         yyval := yyv[yysp-0];
       end;
  12 : begin
         
         import_statements.Add (yyv[yysp-1].yyString);
         
       end;
  13 : begin
       end;
  14 : begin
         yyval := yyv[yysp-0];
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
         
         package_name := yyv[yysp-1].yyString;
         
       end;
  17 : begin
         
         option_statements.Add (yyv[yysp-3].yyString + ': ' + yyv[yysp-1].yyString);
         
       end;
  18 : begin
         yyval := yyv[yysp-0];
       end;
  19 : begin
         
         yyval.yyString := '(' + yyv[yysp-2].yyString + ')' + yyv[yysp-0].yyString
         
       end;
  20 : begin
         
         yyval.yyString := '';
         
       end;
  21 : begin
         
         yyval.yyString := '.' + yyv[yysp-0].yyString;
         
       end;
  22 : begin
         yyval := yyv[yysp-0];
       end;
  23 : begin
         yyval := yyv[yysp-0];
       end;
  24 : begin
         yyval := yyv[yysp-0];
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         yyval := yyv[yysp-0];
       end;
  27 : begin
         yyval := yyv[yysp-0];
       end;
  28 : begin
         yyval := yyv[yysp-0];
       end;
  29 : begin
         yyval := yyv[yysp-0];
       end;
  30 : begin
         yyval := yyv[yysp-1];
       end;
  31 : begin
         yyval := yyv[yysp-1];
       end;
  32 : begin
         yyval := yyv[yysp-1];
       end;
  33 : begin
         yyval := yyv[yysp-1];
       end;
  34 : begin
         yyval := yyv[yysp-1];
       end;
  35 : begin
         yyval := yyv[yysp-1];
       end;
  36 : begin
         yyval := yyv[yysp-0];
       end;
  37 : begin
         yyval := yyv[yysp-0];
       end;
  38 : begin
         yyval := yyv[yysp-0];
       end;
  39 : begin
         yyval := yyv[yysp-1];
       end;
  40 : begin
         yyval := yyv[yysp-1];
       end;
  41 : begin
         yyval := yyv[yysp-0];
       end;
  42 : begin
         yyval := yyv[yysp-0];
       end;
  43 : begin
         yyval := yyv[yysp-0];
       end;
  44 : begin
         yyval := yyv[yysp-4];
       end;
  45 : begin
         yyval := yyv[yysp-0];
       end;
  46 : begin
         yyval := yyv[yysp-1];
       end;
  47 : begin
       end;
  48 : begin
         yyval := yyv[yysp-0];
       end;
  49 : begin
         yyval := yyv[yysp-0];
       end;
  50 : begin
         yyval := yyv[yysp-4];
       end;
  51 : begin
         yyval := yyv[yysp-4];
       end;
  52 : begin
       end;
  53 : begin
         yyval := yyv[yysp-4];
       end;
  54 : begin
         yyval := yyv[yysp-4];
       end;
  55 : begin
         yyval := yyv[yysp-0];
       end;
  56 : begin
         yyval := yyv[yysp-1];
       end;
  57 : begin
       end;
  58 : begin
         yyval := yyv[yysp-0];
       end;
  59 : begin
         yyval := yyv[yysp-0];
       end;
  60 : begin
         yyval := yyv[yysp-0];
       end;
  61 : begin
         yyval := yyv[yysp-0];
       end;
  62 : begin
         yyval := yyv[yysp-0];
       end;
  63 : begin
         yyval := yyv[yysp-0];
       end;
  64 : begin
         yyval := yyv[yysp-0];
       end;
  65 : begin
         yyval := yyv[yysp-0];
       end;
  66 : begin
         yyval := yyv[yysp-0];
       end;
  67 : begin
         yyval := yyv[yysp-1];
       end;
  68 : begin
         yyval := yyv[yysp-5];
       end;
  69 : begin
         yyval := yyv[yysp-0];
       end;
  70 : begin
         yyval := yyv[yysp-0];
       end;
  71 : begin
         yyval := yyv[yysp-0];
       end;
  72 : begin
         yyval := yyv[yysp-1];
       end;
  73 : begin
         yyval := yyv[yysp-0];
       end;
  74 : begin
         yyval := yyv[yysp-0];
       end;
  75 : begin
         yyval := yyv[yysp-0];
       end;
  76 : begin
         yyval := yyv[yysp-0];
       end;
  77 : begin
         yyval := yyv[yysp-0];
       end;
  78 : begin
         yyval := yyv[yysp-0];
       end;
  79 : begin
         yyval := yyv[yysp-0];
       end;
  80 : begin
         yyval := yyv[yysp-0];
       end;
  81 : begin
         yyval := yyv[yysp-0];
       end;
  82 : begin
         yyval := yyv[yysp-0];
       end;
  83 : begin
         yyval := yyv[yysp-0];
       end;
  84 : begin
         yyval := yyv[yysp-0];
       end;
  85 : begin
         yyval := yyv[yysp-0];
       end;
  86 : begin
         yyval := yyv[yysp-0];
       end;
  87 : begin
         yyval := yyv[yysp-0];
       end;
  88 : begin
       end;
  89 : begin
         yyval := yyv[yysp-2];
       end;
  90 : begin
         yyval := yyv[yysp-0];
       end;
  91 : begin
         yyval := yyv[yysp-2];
       end;
  92 : begin
         yyval := yyv[yysp-0];
       end;
  93 : begin
         yyval := yyv[yysp-4];
       end;
  94 : begin
         yyval := yyv[yysp-0];
       end;
  95 : begin
         yyval := yyv[yysp-1];
       end;
  96 : begin
       end;
  97 : begin
         yyval := yyv[yysp-0];
       end;
  98 : begin
         yyval := yyv[yysp-0];
       end;
  99 : begin
         yyval := yyv[yysp-10];
       end;
 100 : begin
         yyval := yyv[yysp-0];
       end;
 101 : begin
         yyval := yyv[yysp-0];
       end;
 102 : begin
         yyval := yyv[yysp-0];
       end;
 103 : begin
         yyval := yyv[yysp-0];
       end;
 104 : begin
         yyval := yyv[yysp-0];
       end;
 105 : begin
         yyval := yyv[yysp-0];
       end;
 106 : begin
         yyval := yyv[yysp-0];
       end;
 107 : begin
         yyval := yyv[yysp-0];
       end;
 108 : begin
         yyval := yyv[yysp-0];
       end;
 109 : begin
         yyval := yyv[yysp-0];
       end;
 110 : begin
         yyval := yyv[yysp-0];
       end;
 111 : begin
         yyval := yyv[yysp-0];
       end;
 112 : begin
         
         proto_reserved := '';
         
       end;
 113 : begin
         
         reserved_statements.Add (proto_reserved);
         
       end;
 114 : begin
         yyval := yyv[yysp-0];
       end;
 115 : begin
         yyval := yyv[yysp-0];
       end;
 116 : begin
         yyval := yyv[yysp-0];
       end;
 117 : begin
         yyval := yyv[yysp-2];
       end;
 118 : begin
         yyval := yyv[yysp-1];
       end;
 119 : begin
       end;
 120 : begin
         yyval := yyv[yysp-1];
       end;
 121 : begin
         yyval := yyv[yysp-0];
       end;
 122 : begin
         yyval := yyv[yysp-0];
       end;
 123 : begin
         
         proto_reserved := yyv[yysp-0].yyString;
         
       end;
 124 : begin
         
         proto_reserved := proto_reserved + ', ' + yyv[yysp-0].yyString;
         
       end;
 125 : begin
         yyval := yyv[yysp-4];
       end;
 126 : begin
       end;
 127 : begin
         yyval := yyv[yysp-0];
       end;
 128 : begin
         yyval := yyv[yysp-0];
       end;
 129 : begin
         yyval := yyv[yysp-1];
       end;
 130 : begin
         yyval := yyv[yysp-0];
       end;
 131 : begin
         yyval := yyv[yysp-0];
       end;
 132 : begin
         yyval := yyv[yysp-0];
       end;
 133 : begin
         yyval := yyv[yysp-9];
       end;
 134 : begin
         yyval := yyv[yysp-1];
       end;
 135 : begin
         yyval := yyv[yysp-0];
       end;
 136 : begin
         yyval := yyv[yysp-0];
       end;
 137 : begin
         yyval := yyv[yysp-1];
       end;
 138 : begin
         yyval := yyv[yysp-0];
       end;
 139 : begin
         yyval := yyv[yysp-0];
       end;
 140 : begin
         yyval := yyv[yysp-2];
       end;
 141 : begin
       end;
 142 : begin
         yyval := yyv[yysp-0];
       end;
 143 : begin
         yyval := yyv[yysp-0];
       end;
 144 : begin
         yyval := yyv[yysp-1];
       end;
 145 : begin
         yyval := yyv[yysp-0];
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

yynacts   = 331;
yyngotos  = 152;
yynstates = 224;
yynrules  = 145;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 311; act: 3 ),
{ 1: }
  ( sym: 258; act: 14 ),
  ( sym: 283; act: 15 ),
  ( sym: 288; act: 16 ),
  ( sym: 293; act: 17 ),
  ( sym: 296; act: 18 ),
  ( sym: 297; act: 19 ),
  ( sym: 304; act: 20 ),
  ( sym: 0; act: -3 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
  ( sym: 259; act: 21 ),
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
  ( sym: 258; act: 14 ),
  ( sym: 283; act: 15 ),
  ( sym: 288; act: 16 ),
  ( sym: 293; act: 17 ),
  ( sym: 296; act: 18 ),
  ( sym: 297; act: 19 ),
  ( sym: 304; act: 20 ),
  ( sym: 0; act: -4 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: 278; act: 23 ),
{ 16: }
  ( sym: 299; act: 25 ),
  ( sym: 315; act: 26 ),
  ( sym: 277; act: -13 ),
{ 17: }
  ( sym: 278; act: 27 ),
{ 18: }
  ( sym: 264; act: 30 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 19: }
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 20: }
  ( sym: 278; act: 34 ),
{ 21: }
  ( sym: 277; act: 35 ),
{ 22: }
{ 23: }
  ( sym: 268; act: 36 ),
{ 24: }
  ( sym: 277; act: 37 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: 268; act: 38 ),
{ 28: }
  ( sym: 259; act: 39 ),
{ 29: }
{ 30: }
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 31: }
{ 32: }
{ 33: }
  ( sym: 258; act: 41 ),
{ 34: }
  ( sym: 268; act: 42 ),
{ 35: }
  ( sym: 258; act: 43 ),
{ 36: }
  ( sym: 278; act: 48 ),
  ( sym: 296; act: 49 ),
  ( sym: 269; act: -47 ),
{ 37: }
  ( sym: 258; act: 50 ),
{ 38: }
  ( sym: 258; act: 65 ),
  ( sym: 260; act: 66 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 283; act: 15 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 291; act: 75 ),
  ( sym: 293; act: 17 ),
  ( sym: 295; act: 76 ),
  ( sym: 296; act: 18 ),
  ( sym: 300; act: 77 ),
  ( sym: 301; act: 78 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
  ( sym: 269; act: -57 ),
{ 39: }
  ( sym: 270; act: 91 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
  ( sym: 275; act: 95 ),
  ( sym: 276; act: 96 ),
  ( sym: 277; act: 97 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 40: }
  ( sym: 265; act: 98 ),
{ 41: }
{ 42: }
  ( sym: 258; act: 104 ),
  ( sym: 296; act: 18 ),
  ( sym: 303; act: 105 ),
  ( sym: 269; act: -126 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
  ( sym: 269; act: 107 ),
  ( sym: 278; act: 48 ),
  ( sym: 296; act: 49 ),
{ 48: }
  ( sym: 259; act: 108 ),
{ 49: }
  ( sym: 278; act: 109 ),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
  ( sym: 278; act: 110 ),
{ 54: }
{ 55: }
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
  ( sym: 258; act: 65 ),
  ( sym: 260; act: 66 ),
  ( sym: 269; act: 112 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 283; act: 15 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 291; act: 75 ),
  ( sym: 293; act: 17 ),
  ( sym: 295; act: 76 ),
  ( sym: 296; act: 18 ),
  ( sym: 300; act: 77 ),
  ( sym: 301; act: 78 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
  ( sym: 262; act: 114 ),
{ 76: }
  ( sym: 278; act: 115 ),
{ 77: }
  ( sym: 260; act: 66 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: 272; act: 118 ),
  ( sym: 273; act: 119 ),
  ( sym: 274; act: 120 ),
  ( sym: 275; act: 121 ),
{ 87: }
{ 88: }
{ 89: }
  ( sym: 258; act: 122 ),
{ 90: }
{ 91: }
  ( sym: 272; act: 123 ),
  ( sym: 273; act: 124 ),
  ( sym: 274; act: 125 ),
  ( sym: 275; act: 126 ),
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
  ( sym: 260; act: 128 ),
  ( sym: 259; act: -20 ),
{ 99: }
{ 100: }
{ 101: }
  ( sym: 258; act: 104 ),
  ( sym: 296; act: 18 ),
  ( sym: 303; act: 105 ),
  ( sym: 269; act: -127 ),
{ 102: }
  ( sym: 269; act: 130 ),
{ 103: }
{ 104: }
{ 105: }
  ( sym: 278; act: 131 ),
{ 106: }
{ 107: }
{ 108: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
{ 109: }
  ( sym: 259; act: 135 ),
{ 110: }
  ( sym: 259; act: 136 ),
{ 111: }
{ 112: }
{ 113: }
{ 114: }
  ( sym: 280; act: 138 ),
  ( sym: 285; act: 139 ),
  ( sym: 286; act: 140 ),
  ( sym: 289; act: 141 ),
  ( sym: 290; act: 142 ),
  ( sym: 305; act: 143 ),
  ( sym: 306; act: 144 ),
  ( sym: 307; act: 145 ),
  ( sym: 308; act: 146 ),
  ( sym: 310; act: 147 ),
  ( sym: 313; act: 148 ),
  ( sym: 314; act: 149 ),
{ 115: }
  ( sym: 268; act: 150 ),
{ 116: }
{ 117: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
  ( sym: 277; act: 156 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 129: }
{ 130: }
{ 131: }
  ( sym: 264; act: 158 ),
{ 132: }
  ( sym: 272; act: 118 ),
  ( sym: 273; act: 119 ),
  ( sym: 274; act: 120 ),
{ 133: }
  ( sym: 266; act: 160 ),
  ( sym: 258; act: -52 ),
{ 134: }
  ( sym: 272; act: 123 ),
  ( sym: 273; act: 124 ),
  ( sym: 274; act: 125 ),
{ 135: }
  ( sym: 276; act: 161 ),
{ 136: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
{ 137: }
  ( sym: 261; act: 163 ),
{ 138: }
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
{ 150: }
  ( sym: 260; act: 66 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 296; act: 18 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
  ( sym: 269; act: -96 ),
{ 151: }
{ 152: }
  ( sym: 261; act: 168 ),
  ( sym: 258; act: -115 ),
{ 153: }
  ( sym: 261; act: 169 ),
  ( sym: 258; act: -114 ),
{ 154: }
  ( sym: 258; act: 170 ),
{ 155: }
  ( sym: 312; act: 172 ),
  ( sym: 258; act: -119 ),
  ( sym: 261; act: -119 ),
{ 156: }
{ 157: }
{ 158: }
  ( sym: 260; act: 176 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 309; act: 177 ),
{ 159: }
  ( sym: 258; act: 178 ),
{ 160: }
  ( sym: 264; act: 30 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 161: }
  ( sym: 258; act: 180 ),
{ 162: }
  ( sym: 266; act: 182 ),
  ( sym: 258; act: -88 ),
{ 163: }
  ( sym: 260; act: 66 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
{ 164: }
{ 165: }
  ( sym: 260; act: 66 ),
  ( sym: 269; act: 185 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 280; act: 67 ),
  ( sym: 281; act: 68 ),
  ( sym: 282; act: 69 ),
  ( sym: 285; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 296; act: 18 ),
  ( sym: 305; act: 79 ),
  ( sym: 306; act: 80 ),
  ( sym: 307; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 313; act: 84 ),
  ( sym: 314; act: 85 ),
{ 166: }
{ 167: }
{ 168: }
  ( sym: 277; act: 186 ),
{ 169: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
{ 170: }
{ 171: }
{ 172: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
  ( sym: 292; act: 190 ),
{ 173: }
{ 174: }
  ( sym: 265; act: 191 ),
{ 175: }
{ 176: }
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 177: }
  ( sym: 260; act: 176 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 178: }
{ 179: }
  ( sym: 259; act: 194 ),
{ 180: }
{ 181: }
  ( sym: 258; act: 195 ),
{ 182: }
  ( sym: 296; act: 18 ),
{ 183: }
  ( sym: 263; act: 199 ),
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
  ( sym: 302; act: 200 ),
{ 192: }
{ 193: }
{ 194: }
  ( sym: 270; act: 91 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
  ( sym: 275; act: 95 ),
  ( sym: 276; act: 96 ),
  ( sym: 277; act: 97 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
{ 195: }
{ 196: }
{ 197: }
  ( sym: 261; act: 202 ),
  ( sym: 267; act: 203 ),
{ 198: }
{ 199: }
  ( sym: 278; act: 204 ),
{ 200: }
  ( sym: 264; act: 205 ),
{ 201: }
  ( sym: 267; act: 206 ),
{ 202: }
  ( sym: 296; act: 18 ),
{ 203: }
{ 204: }
  ( sym: 259; act: 208 ),
{ 205: }
  ( sym: 260; act: 176 ),
  ( sym: 278; act: 31 ),
  ( sym: 279; act: 32 ),
  ( sym: 309; act: 177 ),
{ 206: }
{ 207: }
{ 208: }
  ( sym: 270; act: 134 ),
  ( sym: 272; act: 92 ),
  ( sym: 273; act: 93 ),
  ( sym: 274; act: 94 ),
{ 209: }
  ( sym: 265; act: 211 ),
{ 210: }
  ( sym: 266; act: 182 ),
  ( sym: 258; act: -88 ),
{ 211: }
  ( sym: 258; act: 215 ),
  ( sym: 268; act: 216 ),
{ 212: }
  ( sym: 258; act: 217 ),
{ 213: }
{ 214: }
{ 215: }
{ 216: }
  ( sym: 296; act: 18 ),
  ( sym: 269; act: -141 ),
{ 217: }
{ 218: }
{ 219: }
  ( sym: 296; act: 18 ),
  ( sym: 269; act: -142 ),
{ 220: }
  ( sym: 269; act: 223 )
{ 221: }
{ 222: }
{ 223: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -3; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -21; act: 4 ),
  ( sym: -20; act: 5 ),
  ( sym: -19; act: 6 ),
  ( sym: -10; act: 7 ),
  ( sym: -9; act: 8 ),
  ( sym: -8; act: 9 ),
  ( sym: -7; act: 10 ),
  ( sym: -6; act: 11 ),
  ( sym: -5; act: 12 ),
  ( sym: -4; act: 13 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
  ( sym: -21; act: 4 ),
  ( sym: -20; act: 5 ),
  ( sym: -19; act: 6 ),
  ( sym: -10; act: 7 ),
  ( sym: -9; act: 8 ),
  ( sym: -8; act: 9 ),
  ( sym: -7; act: 10 ),
  ( sym: -6; act: 22 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
  ( sym: -11; act: 24 ),
{ 17: }
{ 18: }
  ( sym: -13; act: 28 ),
  ( sym: -12; act: 29 ),
{ 19: }
  ( sym: -12; act: 33 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
  ( sym: -12; act: 40 ),
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -25; act: 44 ),
  ( sym: -24; act: 45 ),
  ( sym: -23; act: 46 ),
  ( sym: -22; act: 47 ),
{ 37: }
{ 38: }
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 53 ),
  ( sym: -33; act: 54 ),
  ( sym: -32; act: 55 ),
  ( sym: -31; act: 56 ),
  ( sym: -30; act: 57 ),
  ( sym: -29; act: 58 ),
  ( sym: -28; act: 59 ),
  ( sym: -27; act: 60 ),
  ( sym: -20; act: 61 ),
  ( sym: -19; act: 62 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 64 ),
{ 39: }
  ( sym: -18; act: 86 ),
  ( sym: -17; act: 87 ),
  ( sym: -16; act: 88 ),
  ( sym: -14; act: 89 ),
  ( sym: -12; act: 90 ),
{ 40: }
{ 41: }
{ 42: }
  ( sym: -53; act: 99 ),
  ( sym: -52; act: 100 ),
  ( sym: -51; act: 101 ),
  ( sym: -50; act: 102 ),
  ( sym: -9; act: 103 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
  ( sym: -25; act: 44 ),
  ( sym: -24; act: 45 ),
  ( sym: -23; act: 106 ),
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
{ 59: }
{ 60: }
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 53 ),
  ( sym: -33; act: 54 ),
  ( sym: -32; act: 55 ),
  ( sym: -31; act: 56 ),
  ( sym: -30; act: 57 ),
  ( sym: -29; act: 58 ),
  ( sym: -28; act: 111 ),
  ( sym: -20; act: 61 ),
  ( sym: -19; act: 62 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 64 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: -12; act: 113 ),
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 53 ),
  ( sym: -33; act: 116 ),
  ( sym: -12; act: 63 ),
{ 78: }
  ( sym: -44; act: 117 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
  ( sym: -15; act: 127 ),
{ 99: }
{ 100: }
{ 101: }
  ( sym: -53; act: 99 ),
  ( sym: -52; act: 129 ),
  ( sym: -9; act: 103 ),
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 133 ),
{ 109: }
{ 110: }
{ 111: }
{ 112: }
{ 113: }
{ 114: }
  ( sym: -42; act: 137 ),
{ 115: }
{ 116: }
{ 117: }
  ( sym: -47; act: 151 ),
  ( sym: -46; act: 152 ),
  ( sym: -45; act: 153 ),
  ( sym: -43; act: 154 ),
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 155 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
  ( sym: -12; act: 157 ),
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  ( sym: -26; act: 159 ),
{ 134: }
{ 135: }
{ 136: }
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 162 ),
{ 137: }
{ 138: }
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
{ 150: }
  ( sym: -41; act: 164 ),
  ( sym: -40; act: 165 ),
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 53 ),
  ( sym: -33; act: 166 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 167 ),
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
  ( sym: -48; act: 171 ),
{ 156: }
{ 157: }
{ 158: }
  ( sym: -56; act: 173 ),
  ( sym: -54; act: 174 ),
  ( sym: -12; act: 175 ),
{ 159: }
{ 160: }
  ( sym: -13; act: 179 ),
  ( sym: -12; act: 29 ),
{ 161: }
{ 162: }
  ( sym: -35; act: 181 ),
{ 163: }
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 183 ),
  ( sym: -12; act: 63 ),
{ 164: }
{ 165: }
  ( sym: -41; act: 184 ),
  ( sym: -37; act: 51 ),
  ( sym: -36; act: 52 ),
  ( sym: -34; act: 53 ),
  ( sym: -33; act: 166 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 167 ),
{ 166: }
{ 167: }
{ 168: }
{ 169: }
  ( sym: -47; act: 187 ),
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 155 ),
{ 170: }
{ 171: }
{ 172: }
  ( sym: -49; act: 188 ),
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 189 ),
{ 173: }
{ 174: }
{ 175: }
{ 176: }
  ( sym: -12; act: 192 ),
{ 177: }
  ( sym: -56; act: 193 ),
  ( sym: -12; act: 175 ),
{ 178: }
{ 179: }
{ 180: }
{ 181: }
{ 182: }
  ( sym: -39; act: 196 ),
  ( sym: -38; act: 197 ),
  ( sym: -9; act: 198 ),
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
  ( sym: -18; act: 86 ),
  ( sym: -17; act: 87 ),
  ( sym: -16; act: 88 ),
  ( sym: -14; act: 201 ),
  ( sym: -12; act: 90 ),
{ 195: }
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
  ( sym: -39; act: 207 ),
  ( sym: -9; act: 198 ),
{ 203: }
{ 204: }
{ 205: }
  ( sym: -56; act: 173 ),
  ( sym: -54; act: 209 ),
  ( sym: -12; act: 175 ),
{ 206: }
{ 207: }
{ 208: }
  ( sym: -18; act: 132 ),
  ( sym: -16; act: 210 ),
{ 209: }
{ 210: }
  ( sym: -35; act: 212 ),
{ 211: }
  ( sym: -57; act: 213 ),
  ( sym: -55; act: 214 ),
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
  ( sym: -60; act: 218 ),
  ( sym: -59; act: 219 ),
  ( sym: -58; act: 220 ),
  ( sym: -9; act: 221 ),
{ 217: }
{ 218: }
{ 219: }
  ( sym: -60; act: 222 ),
  ( sym: -9; act: 221 )
{ 220: }
{ 221: }
{ 222: }
{ 223: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } -43,
{ 5: } -42,
{ 6: } -41,
{ 7: } -11,
{ 8: } -10,
{ 9: } -9,
{ 10: } -8,
{ 11: } -5,
{ 12: } 0,
{ 13: } -1,
{ 14: } -7,
{ 15: } 0,
{ 16: } 0,
{ 17: } 0,
{ 18: } 0,
{ 19: } 0,
{ 20: } 0,
{ 21: } 0,
{ 22: } -6,
{ 23: } 0,
{ 24: } 0,
{ 25: } -15,
{ 26: } -14,
{ 27: } 0,
{ 28: } 0,
{ 29: } -18,
{ 30: } 0,
{ 31: } -36,
{ 32: } -37,
{ 33: } 0,
{ 34: } 0,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
{ 38: } 0,
{ 39: } 0,
{ 40: } 0,
{ 41: } -16,
{ 42: } 0,
{ 43: } -2,
{ 44: } -49,
{ 45: } -48,
{ 46: } -45,
{ 47: } 0,
{ 48: } 0,
{ 49: } 0,
{ 50: } -12,
{ 51: } -70,
{ 52: } -69,
{ 53: } 0,
{ 54: } -66,
{ 55: } -64,
{ 56: } -63,
{ 57: } -62,
{ 58: } -58,
{ 59: } -55,
{ 60: } 0,
{ 61: } -60,
{ 62: } -59,
{ 63: } -71,
{ 64: } -61,
{ 65: } -65,
{ 66: } 0,
{ 67: } -85,
{ 68: } -87,
{ 69: } -73,
{ 70: } -81,
{ 71: } -82,
{ 72: } -74,
{ 73: } -75,
{ 74: } -76,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -112,
{ 79: } -83,
{ 80: } -84,
{ 81: } -79,
{ 82: } -80,
{ 83: } -86,
{ 84: } -77,
{ 85: } -78,
{ 86: } 0,
{ 87: } -24,
{ 88: } -23,
{ 89: } 0,
{ 90: } -22,
{ 91: } 0,
{ 92: } -27,
{ 93: } -28,
{ 94: } -29,
{ 95: } -38,
{ 96: } -26,
{ 97: } -25,
{ 98: } 0,
{ 99: } -132,
{ 100: } -128,
{ 101: } 0,
{ 102: } 0,
{ 103: } -131,
{ 104: } -130,
{ 105: } 0,
{ 106: } -46,
{ 107: } -44,
{ 108: } 0,
{ 109: } 0,
{ 110: } 0,
{ 111: } -56,
{ 112: } -54,
{ 113: } -72,
{ 114: } 0,
{ 115: } 0,
{ 116: } -67,
{ 117: } 0,
{ 118: } -33,
{ 119: } -34,
{ 120: } -35,
{ 121: } -40,
{ 122: } -17,
{ 123: } -30,
{ 124: } -31,
{ 125: } -32,
{ 126: } -39,
{ 127: } -19,
{ 128: } 0,
{ 129: } -129,
{ 130: } -125,
{ 131: } 0,
{ 132: } 0,
{ 133: } 0,
{ 134: } 0,
{ 135: } 0,
{ 136: } 0,
{ 137: } 0,
{ 138: } -110,
{ 139: } -106,
{ 140: } -107,
{ 141: } -100,
{ 142: } -101,
{ 143: } -108,
{ 144: } -109,
{ 145: } -104,
{ 146: } -105,
{ 147: } -111,
{ 148: } -102,
{ 149: } -103,
{ 150: } 0,
{ 151: } -116,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } -123,
{ 157: } -21,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } 0,
{ 162: } 0,
{ 163: } 0,
{ 164: } -94,
{ 165: } 0,
{ 166: } -98,
{ 167: } -97,
{ 168: } 0,
{ 169: } 0,
{ 170: } -113,
{ 171: } -118,
{ 172: } 0,
{ 173: } -135,
{ 174: } 0,
{ 175: } -136,
{ 176: } 0,
{ 177: } 0,
{ 178: } -51,
{ 179: } 0,
{ 180: } -50,
{ 181: } 0,
{ 182: } 0,
{ 183: } 0,
{ 184: } -95,
{ 185: } -93,
{ 186: } -124,
{ 187: } -117,
{ 188: } -120,
{ 189: } -122,
{ 190: } -121,
{ 191: } 0,
{ 192: } -137,
{ 193: } -134,
{ 194: } 0,
{ 195: } -68,
{ 196: } -90,
{ 197: } 0,
{ 198: } -92,
{ 199: } 0,
{ 200: } 0,
{ 201: } 0,
{ 202: } 0,
{ 203: } -89,
{ 204: } 0,
{ 205: } 0,
{ 206: } -53,
{ 207: } -91,
{ 208: } 0,
{ 209: } 0,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } -138,
{ 214: } -133,
{ 215: } -139,
{ 216: } 0,
{ 217: } -99,
{ 218: } -143,
{ 219: } 0,
{ 220: } 0,
{ 221: } -145,
{ 222: } -144,
{ 223: } -140
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 10,
{ 3: } 11,
{ 4: } 12,
{ 5: } 12,
{ 6: } 12,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 12,
{ 12: } 12,
{ 13: } 20,
{ 14: } 20,
{ 15: } 20,
{ 16: } 21,
{ 17: } 24,
{ 18: } 25,
{ 19: } 28,
{ 20: } 30,
{ 21: } 31,
{ 22: } 32,
{ 23: } 32,
{ 24: } 33,
{ 25: } 34,
{ 26: } 34,
{ 27: } 34,
{ 28: } 35,
{ 29: } 36,
{ 30: } 36,
{ 31: } 38,
{ 32: } 38,
{ 33: } 38,
{ 34: } 39,
{ 35: } 40,
{ 36: } 41,
{ 37: } 44,
{ 38: } 45,
{ 39: } 72,
{ 40: } 81,
{ 41: } 82,
{ 42: } 82,
{ 43: } 86,
{ 44: } 86,
{ 45: } 86,
{ 46: } 86,
{ 47: } 86,
{ 48: } 89,
{ 49: } 90,
{ 50: } 91,
{ 51: } 91,
{ 52: } 91,
{ 53: } 91,
{ 54: } 92,
{ 55: } 92,
{ 56: } 92,
{ 57: } 92,
{ 58: } 92,
{ 59: } 92,
{ 60: } 92,
{ 61: } 119,
{ 62: } 119,
{ 63: } 119,
{ 64: } 119,
{ 65: } 119,
{ 66: } 119,
{ 67: } 121,
{ 68: } 121,
{ 69: } 121,
{ 70: } 121,
{ 71: } 121,
{ 72: } 121,
{ 73: } 121,
{ 74: } 121,
{ 75: } 121,
{ 76: } 122,
{ 77: } 123,
{ 78: } 141,
{ 79: } 141,
{ 80: } 141,
{ 81: } 141,
{ 82: } 141,
{ 83: } 141,
{ 84: } 141,
{ 85: } 141,
{ 86: } 141,
{ 87: } 145,
{ 88: } 145,
{ 89: } 145,
{ 90: } 146,
{ 91: } 146,
{ 92: } 150,
{ 93: } 150,
{ 94: } 150,
{ 95: } 150,
{ 96: } 150,
{ 97: } 150,
{ 98: } 150,
{ 99: } 152,
{ 100: } 152,
{ 101: } 152,
{ 102: } 156,
{ 103: } 157,
{ 104: } 157,
{ 105: } 157,
{ 106: } 158,
{ 107: } 158,
{ 108: } 158,
{ 109: } 162,
{ 110: } 163,
{ 111: } 164,
{ 112: } 164,
{ 113: } 164,
{ 114: } 164,
{ 115: } 176,
{ 116: } 177,
{ 117: } 177,
{ 118: } 182,
{ 119: } 182,
{ 120: } 182,
{ 121: } 182,
{ 122: } 182,
{ 123: } 182,
{ 124: } 182,
{ 125: } 182,
{ 126: } 182,
{ 127: } 182,
{ 128: } 182,
{ 129: } 184,
{ 130: } 184,
{ 131: } 184,
{ 132: } 185,
{ 133: } 188,
{ 134: } 190,
{ 135: } 193,
{ 136: } 194,
{ 137: } 198,
{ 138: } 199,
{ 139: } 199,
{ 140: } 199,
{ 141: } 199,
{ 142: } 199,
{ 143: } 199,
{ 144: } 199,
{ 145: } 199,
{ 146: } 199,
{ 147: } 199,
{ 148: } 199,
{ 149: } 199,
{ 150: } 199,
{ 151: } 219,
{ 152: } 219,
{ 153: } 221,
{ 154: } 223,
{ 155: } 224,
{ 156: } 227,
{ 157: } 227,
{ 158: } 227,
{ 159: } 231,
{ 160: } 232,
{ 161: } 235,
{ 162: } 236,
{ 163: } 238,
{ 164: } 256,
{ 165: } 256,
{ 166: } 276,
{ 167: } 276,
{ 168: } 276,
{ 169: } 277,
{ 170: } 281,
{ 171: } 281,
{ 172: } 281,
{ 173: } 286,
{ 174: } 286,
{ 175: } 287,
{ 176: } 287,
{ 177: } 289,
{ 178: } 292,
{ 179: } 292,
{ 180: } 293,
{ 181: } 293,
{ 182: } 294,
{ 183: } 295,
{ 184: } 296,
{ 185: } 296,
{ 186: } 296,
{ 187: } 296,
{ 188: } 296,
{ 189: } 296,
{ 190: } 296,
{ 191: } 296,
{ 192: } 297,
{ 193: } 297,
{ 194: } 297,
{ 195: } 306,
{ 196: } 306,
{ 197: } 306,
{ 198: } 308,
{ 199: } 308,
{ 200: } 309,
{ 201: } 310,
{ 202: } 311,
{ 203: } 312,
{ 204: } 312,
{ 205: } 313,
{ 206: } 317,
{ 207: } 317,
{ 208: } 317,
{ 209: } 321,
{ 210: } 322,
{ 211: } 324,
{ 212: } 326,
{ 213: } 327,
{ 214: } 327,
{ 215: } 327,
{ 216: } 327,
{ 217: } 329,
{ 218: } 329,
{ 219: } 329,
{ 220: } 331,
{ 221: } 332,
{ 222: } 332,
{ 223: } 332
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 9,
{ 2: } 10,
{ 3: } 11,
{ 4: } 11,
{ 5: } 11,
{ 6: } 11,
{ 7: } 11,
{ 8: } 11,
{ 9: } 11,
{ 10: } 11,
{ 11: } 11,
{ 12: } 19,
{ 13: } 19,
{ 14: } 19,
{ 15: } 20,
{ 16: } 23,
{ 17: } 24,
{ 18: } 27,
{ 19: } 29,
{ 20: } 30,
{ 21: } 31,
{ 22: } 31,
{ 23: } 32,
{ 24: } 33,
{ 25: } 33,
{ 26: } 33,
{ 27: } 34,
{ 28: } 35,
{ 29: } 35,
{ 30: } 37,
{ 31: } 37,
{ 32: } 37,
{ 33: } 38,
{ 34: } 39,
{ 35: } 40,
{ 36: } 43,
{ 37: } 44,
{ 38: } 71,
{ 39: } 80,
{ 40: } 81,
{ 41: } 81,
{ 42: } 85,
{ 43: } 85,
{ 44: } 85,
{ 45: } 85,
{ 46: } 85,
{ 47: } 88,
{ 48: } 89,
{ 49: } 90,
{ 50: } 90,
{ 51: } 90,
{ 52: } 90,
{ 53: } 91,
{ 54: } 91,
{ 55: } 91,
{ 56: } 91,
{ 57: } 91,
{ 58: } 91,
{ 59: } 91,
{ 60: } 118,
{ 61: } 118,
{ 62: } 118,
{ 63: } 118,
{ 64: } 118,
{ 65: } 118,
{ 66: } 120,
{ 67: } 120,
{ 68: } 120,
{ 69: } 120,
{ 70: } 120,
{ 71: } 120,
{ 72: } 120,
{ 73: } 120,
{ 74: } 120,
{ 75: } 121,
{ 76: } 122,
{ 77: } 140,
{ 78: } 140,
{ 79: } 140,
{ 80: } 140,
{ 81: } 140,
{ 82: } 140,
{ 83: } 140,
{ 84: } 140,
{ 85: } 140,
{ 86: } 144,
{ 87: } 144,
{ 88: } 144,
{ 89: } 145,
{ 90: } 145,
{ 91: } 149,
{ 92: } 149,
{ 93: } 149,
{ 94: } 149,
{ 95: } 149,
{ 96: } 149,
{ 97: } 149,
{ 98: } 151,
{ 99: } 151,
{ 100: } 151,
{ 101: } 155,
{ 102: } 156,
{ 103: } 156,
{ 104: } 156,
{ 105: } 157,
{ 106: } 157,
{ 107: } 157,
{ 108: } 161,
{ 109: } 162,
{ 110: } 163,
{ 111: } 163,
{ 112: } 163,
{ 113: } 163,
{ 114: } 175,
{ 115: } 176,
{ 116: } 176,
{ 117: } 181,
{ 118: } 181,
{ 119: } 181,
{ 120: } 181,
{ 121: } 181,
{ 122: } 181,
{ 123: } 181,
{ 124: } 181,
{ 125: } 181,
{ 126: } 181,
{ 127: } 181,
{ 128: } 183,
{ 129: } 183,
{ 130: } 183,
{ 131: } 184,
{ 132: } 187,
{ 133: } 189,
{ 134: } 192,
{ 135: } 193,
{ 136: } 197,
{ 137: } 198,
{ 138: } 198,
{ 139: } 198,
{ 140: } 198,
{ 141: } 198,
{ 142: } 198,
{ 143: } 198,
{ 144: } 198,
{ 145: } 198,
{ 146: } 198,
{ 147: } 198,
{ 148: } 198,
{ 149: } 198,
{ 150: } 218,
{ 151: } 218,
{ 152: } 220,
{ 153: } 222,
{ 154: } 223,
{ 155: } 226,
{ 156: } 226,
{ 157: } 226,
{ 158: } 230,
{ 159: } 231,
{ 160: } 234,
{ 161: } 235,
{ 162: } 237,
{ 163: } 255,
{ 164: } 255,
{ 165: } 275,
{ 166: } 275,
{ 167: } 275,
{ 168: } 276,
{ 169: } 280,
{ 170: } 280,
{ 171: } 280,
{ 172: } 285,
{ 173: } 285,
{ 174: } 286,
{ 175: } 286,
{ 176: } 288,
{ 177: } 291,
{ 178: } 291,
{ 179: } 292,
{ 180: } 292,
{ 181: } 293,
{ 182: } 294,
{ 183: } 295,
{ 184: } 295,
{ 185: } 295,
{ 186: } 295,
{ 187: } 295,
{ 188: } 295,
{ 189: } 295,
{ 190: } 295,
{ 191: } 296,
{ 192: } 296,
{ 193: } 296,
{ 194: } 305,
{ 195: } 305,
{ 196: } 305,
{ 197: } 307,
{ 198: } 307,
{ 199: } 308,
{ 200: } 309,
{ 201: } 310,
{ 202: } 311,
{ 203: } 311,
{ 204: } 312,
{ 205: } 316,
{ 206: } 316,
{ 207: } 316,
{ 208: } 320,
{ 209: } 321,
{ 210: } 323,
{ 211: } 325,
{ 212: } 326,
{ 213: } 326,
{ 214: } 326,
{ 215: } 326,
{ 216: } 328,
{ 217: } 328,
{ 218: } 328,
{ 219: } 330,
{ 220: } 331,
{ 221: } 331,
{ 222: } 331,
{ 223: } 331
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 13,
{ 3: } 13,
{ 4: } 13,
{ 5: } 13,
{ 6: } 13,
{ 7: } 13,
{ 8: } 13,
{ 9: } 13,
{ 10: } 13,
{ 11: } 13,
{ 12: } 13,
{ 13: } 21,
{ 14: } 21,
{ 15: } 21,
{ 16: } 21,
{ 17: } 22,
{ 18: } 22,
{ 19: } 24,
{ 20: } 25,
{ 21: } 25,
{ 22: } 25,
{ 23: } 25,
{ 24: } 25,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 25,
{ 29: } 25,
{ 30: } 25,
{ 31: } 26,
{ 32: } 26,
{ 33: } 26,
{ 34: } 26,
{ 35: } 26,
{ 36: } 26,
{ 37: } 30,
{ 38: } 30,
{ 39: } 44,
{ 40: } 49,
{ 41: } 49,
{ 42: } 49,
{ 43: } 54,
{ 44: } 54,
{ 45: } 54,
{ 46: } 54,
{ 47: } 54,
{ 48: } 57,
{ 49: } 57,
{ 50: } 57,
{ 51: } 57,
{ 52: } 57,
{ 53: } 57,
{ 54: } 57,
{ 55: } 57,
{ 56: } 57,
{ 57: } 57,
{ 58: } 57,
{ 59: } 57,
{ 60: } 57,
{ 61: } 70,
{ 62: } 70,
{ 63: } 70,
{ 64: } 70,
{ 65: } 70,
{ 66: } 70,
{ 67: } 71,
{ 68: } 71,
{ 69: } 71,
{ 70: } 71,
{ 71: } 71,
{ 72: } 71,
{ 73: } 71,
{ 74: } 71,
{ 75: } 71,
{ 76: } 71,
{ 77: } 71,
{ 78: } 76,
{ 79: } 77,
{ 80: } 77,
{ 81: } 77,
{ 82: } 77,
{ 83: } 77,
{ 84: } 77,
{ 85: } 77,
{ 86: } 77,
{ 87: } 77,
{ 88: } 77,
{ 89: } 77,
{ 90: } 77,
{ 91: } 77,
{ 92: } 77,
{ 93: } 77,
{ 94: } 77,
{ 95: } 77,
{ 96: } 77,
{ 97: } 77,
{ 98: } 77,
{ 99: } 78,
{ 100: } 78,
{ 101: } 78,
{ 102: } 81,
{ 103: } 81,
{ 104: } 81,
{ 105: } 81,
{ 106: } 81,
{ 107: } 81,
{ 108: } 81,
{ 109: } 83,
{ 110: } 83,
{ 111: } 83,
{ 112: } 83,
{ 113: } 83,
{ 114: } 83,
{ 115: } 84,
{ 116: } 84,
{ 117: } 84,
{ 118: } 90,
{ 119: } 90,
{ 120: } 90,
{ 121: } 90,
{ 122: } 90,
{ 123: } 90,
{ 124: } 90,
{ 125: } 90,
{ 126: } 90,
{ 127: } 90,
{ 128: } 90,
{ 129: } 91,
{ 130: } 91,
{ 131: } 91,
{ 132: } 91,
{ 133: } 91,
{ 134: } 92,
{ 135: } 92,
{ 136: } 92,
{ 137: } 94,
{ 138: } 94,
{ 139: } 94,
{ 140: } 94,
{ 141: } 94,
{ 142: } 94,
{ 143: } 94,
{ 144: } 94,
{ 145: } 94,
{ 146: } 94,
{ 147: } 94,
{ 148: } 94,
{ 149: } 94,
{ 150: } 94,
{ 151: } 102,
{ 152: } 102,
{ 153: } 102,
{ 154: } 102,
{ 155: } 102,
{ 156: } 103,
{ 157: } 103,
{ 158: } 103,
{ 159: } 106,
{ 160: } 106,
{ 161: } 108,
{ 162: } 108,
{ 163: } 109,
{ 164: } 113,
{ 165: } 113,
{ 166: } 120,
{ 167: } 120,
{ 168: } 120,
{ 169: } 120,
{ 170: } 123,
{ 171: } 123,
{ 172: } 123,
{ 173: } 126,
{ 174: } 126,
{ 175: } 126,
{ 176: } 126,
{ 177: } 127,
{ 178: } 129,
{ 179: } 129,
{ 180: } 129,
{ 181: } 129,
{ 182: } 129,
{ 183: } 132,
{ 184: } 132,
{ 185: } 132,
{ 186: } 132,
{ 187: } 132,
{ 188: } 132,
{ 189: } 132,
{ 190: } 132,
{ 191: } 132,
{ 192: } 132,
{ 193: } 132,
{ 194: } 132,
{ 195: } 137,
{ 196: } 137,
{ 197: } 137,
{ 198: } 137,
{ 199: } 137,
{ 200: } 137,
{ 201: } 137,
{ 202: } 137,
{ 203: } 139,
{ 204: } 139,
{ 205: } 139,
{ 206: } 142,
{ 207: } 142,
{ 208: } 142,
{ 209: } 144,
{ 210: } 144,
{ 211: } 145,
{ 212: } 147,
{ 213: } 147,
{ 214: } 147,
{ 215: } 147,
{ 216: } 147,
{ 217: } 151,
{ 218: } 151,
{ 219: } 151,
{ 220: } 153,
{ 221: } 153,
{ 222: } 153,
{ 223: } 153
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 12,
{ 2: } 12,
{ 3: } 12,
{ 4: } 12,
{ 5: } 12,
{ 6: } 12,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 12,
{ 12: } 20,
{ 13: } 20,
{ 14: } 20,
{ 15: } 20,
{ 16: } 21,
{ 17: } 21,
{ 18: } 23,
{ 19: } 24,
{ 20: } 24,
{ 21: } 24,
{ 22: } 24,
{ 23: } 24,
{ 24: } 24,
{ 25: } 24,
{ 26: } 24,
{ 27: } 24,
{ 28: } 24,
{ 29: } 24,
{ 30: } 25,
{ 31: } 25,
{ 32: } 25,
{ 33: } 25,
{ 34: } 25,
{ 35: } 25,
{ 36: } 29,
{ 37: } 29,
{ 38: } 43,
{ 39: } 48,
{ 40: } 48,
{ 41: } 48,
{ 42: } 53,
{ 43: } 53,
{ 44: } 53,
{ 45: } 53,
{ 46: } 53,
{ 47: } 56,
{ 48: } 56,
{ 49: } 56,
{ 50: } 56,
{ 51: } 56,
{ 52: } 56,
{ 53: } 56,
{ 54: } 56,
{ 55: } 56,
{ 56: } 56,
{ 57: } 56,
{ 58: } 56,
{ 59: } 56,
{ 60: } 69,
{ 61: } 69,
{ 62: } 69,
{ 63: } 69,
{ 64: } 69,
{ 65: } 69,
{ 66: } 70,
{ 67: } 70,
{ 68: } 70,
{ 69: } 70,
{ 70: } 70,
{ 71: } 70,
{ 72: } 70,
{ 73: } 70,
{ 74: } 70,
{ 75: } 70,
{ 76: } 70,
{ 77: } 75,
{ 78: } 76,
{ 79: } 76,
{ 80: } 76,
{ 81: } 76,
{ 82: } 76,
{ 83: } 76,
{ 84: } 76,
{ 85: } 76,
{ 86: } 76,
{ 87: } 76,
{ 88: } 76,
{ 89: } 76,
{ 90: } 76,
{ 91: } 76,
{ 92: } 76,
{ 93: } 76,
{ 94: } 76,
{ 95: } 76,
{ 96: } 76,
{ 97: } 76,
{ 98: } 77,
{ 99: } 77,
{ 100: } 77,
{ 101: } 80,
{ 102: } 80,
{ 103: } 80,
{ 104: } 80,
{ 105: } 80,
{ 106: } 80,
{ 107: } 80,
{ 108: } 82,
{ 109: } 82,
{ 110: } 82,
{ 111: } 82,
{ 112: } 82,
{ 113: } 82,
{ 114: } 83,
{ 115: } 83,
{ 116: } 83,
{ 117: } 89,
{ 118: } 89,
{ 119: } 89,
{ 120: } 89,
{ 121: } 89,
{ 122: } 89,
{ 123: } 89,
{ 124: } 89,
{ 125: } 89,
{ 126: } 89,
{ 127: } 89,
{ 128: } 90,
{ 129: } 90,
{ 130: } 90,
{ 131: } 90,
{ 132: } 90,
{ 133: } 91,
{ 134: } 91,
{ 135: } 91,
{ 136: } 93,
{ 137: } 93,
{ 138: } 93,
{ 139: } 93,
{ 140: } 93,
{ 141: } 93,
{ 142: } 93,
{ 143: } 93,
{ 144: } 93,
{ 145: } 93,
{ 146: } 93,
{ 147: } 93,
{ 148: } 93,
{ 149: } 93,
{ 150: } 101,
{ 151: } 101,
{ 152: } 101,
{ 153: } 101,
{ 154: } 101,
{ 155: } 102,
{ 156: } 102,
{ 157: } 102,
{ 158: } 105,
{ 159: } 105,
{ 160: } 107,
{ 161: } 107,
{ 162: } 108,
{ 163: } 112,
{ 164: } 112,
{ 165: } 119,
{ 166: } 119,
{ 167: } 119,
{ 168: } 119,
{ 169: } 122,
{ 170: } 122,
{ 171: } 122,
{ 172: } 125,
{ 173: } 125,
{ 174: } 125,
{ 175: } 125,
{ 176: } 126,
{ 177: } 128,
{ 178: } 128,
{ 179: } 128,
{ 180: } 128,
{ 181: } 128,
{ 182: } 131,
{ 183: } 131,
{ 184: } 131,
{ 185: } 131,
{ 186: } 131,
{ 187: } 131,
{ 188: } 131,
{ 189: } 131,
{ 190: } 131,
{ 191: } 131,
{ 192: } 131,
{ 193: } 131,
{ 194: } 136,
{ 195: } 136,
{ 196: } 136,
{ 197: } 136,
{ 198: } 136,
{ 199: } 136,
{ 200: } 136,
{ 201: } 136,
{ 202: } 138,
{ 203: } 138,
{ 204: } 138,
{ 205: } 141,
{ 206: } 141,
{ 207: } 141,
{ 208: } 143,
{ 209: } 143,
{ 210: } 144,
{ 211: } 146,
{ 212: } 146,
{ 213: } 146,
{ 214: } 146,
{ 215: } 146,
{ 216: } 150,
{ 217: } 150,
{ 218: } 150,
{ 219: } 152,
{ 220: } 152,
{ 221: } 152,
{ 222: } 152,
{ 223: } 152
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 2; sym: -2 ),
{ 2: } ( len: 4; sym: -3 ),
{ 3: } ( len: 0; sym: -4 ),
{ 4: } ( len: 1; sym: -4 ),
{ 5: } ( len: 1; sym: -5 ),
{ 6: } ( len: 2; sym: -5 ),
{ 7: } ( len: 1; sym: -6 ),
{ 8: } ( len: 1; sym: -6 ),
{ 9: } ( len: 1; sym: -6 ),
{ 10: } ( len: 1; sym: -6 ),
{ 11: } ( len: 1; sym: -6 ),
{ 12: } ( len: 4; sym: -7 ),
{ 13: } ( len: 0; sym: -11 ),
{ 14: } ( len: 1; sym: -11 ),
{ 15: } ( len: 1; sym: -11 ),
{ 16: } ( len: 3; sym: -8 ),
{ 17: } ( len: 5; sym: -9 ),
{ 18: } ( len: 1; sym: -13 ),
{ 19: } ( len: 4; sym: -13 ),
{ 20: } ( len: 0; sym: -15 ),
{ 21: } ( len: 2; sym: -15 ),
{ 22: } ( len: 1; sym: -14 ),
{ 23: } ( len: 1; sym: -14 ),
{ 24: } ( len: 1; sym: -14 ),
{ 25: } ( len: 1; sym: -14 ),
{ 26: } ( len: 1; sym: -14 ),
{ 27: } ( len: 1; sym: -16 ),
{ 28: } ( len: 1; sym: -16 ),
{ 29: } ( len: 1; sym: -16 ),
{ 30: } ( len: 2; sym: -16 ),
{ 31: } ( len: 2; sym: -16 ),
{ 32: } ( len: 2; sym: -16 ),
{ 33: } ( len: 2; sym: -16 ),
{ 34: } ( len: 2; sym: -16 ),
{ 35: } ( len: 2; sym: -16 ),
{ 36: } ( len: 1; sym: -12 ),
{ 37: } ( len: 1; sym: -12 ),
{ 38: } ( len: 1; sym: -17 ),
{ 39: } ( len: 2; sym: -17 ),
{ 40: } ( len: 2; sym: -17 ),
{ 41: } ( len: 1; sym: -10 ),
{ 42: } ( len: 1; sym: -10 ),
{ 43: } ( len: 1; sym: -10 ),
{ 44: } ( len: 5; sym: -19 ),
{ 45: } ( len: 1; sym: -22 ),
{ 46: } ( len: 2; sym: -22 ),
{ 47: } ( len: 0; sym: -23 ),
{ 48: } ( len: 1; sym: -23 ),
{ 49: } ( len: 1; sym: -23 ),
{ 50: } ( len: 5; sym: -24 ),
{ 51: } ( len: 5; sym: -25 ),
{ 52: } ( len: 0; sym: -26 ),
{ 53: } ( len: 5; sym: -26 ),
{ 54: } ( len: 5; sym: -20 ),
{ 55: } ( len: 1; sym: -27 ),
{ 56: } ( len: 2; sym: -27 ),
{ 57: } ( len: 0; sym: -28 ),
{ 58: } ( len: 1; sym: -28 ),
{ 59: } ( len: 1; sym: -28 ),
{ 60: } ( len: 1; sym: -28 ),
{ 61: } ( len: 1; sym: -28 ),
{ 62: } ( len: 1; sym: -28 ),
{ 63: } ( len: 1; sym: -28 ),
{ 64: } ( len: 1; sym: -28 ),
{ 65: } ( len: 1; sym: -28 ),
{ 66: } ( len: 1; sym: -29 ),
{ 67: } ( len: 2; sym: -29 ),
{ 68: } ( len: 6; sym: -33 ),
{ 69: } ( len: 1; sym: -34 ),
{ 70: } ( len: 1; sym: -34 ),
{ 71: } ( len: 1; sym: -37 ),
{ 72: } ( len: 2; sym: -37 ),
{ 73: } ( len: 1; sym: -36 ),
{ 74: } ( len: 1; sym: -36 ),
{ 75: } ( len: 1; sym: -36 ),
{ 76: } ( len: 1; sym: -36 ),
{ 77: } ( len: 1; sym: -36 ),
{ 78: } ( len: 1; sym: -36 ),
{ 79: } ( len: 1; sym: -36 ),
{ 80: } ( len: 1; sym: -36 ),
{ 81: } ( len: 1; sym: -36 ),
{ 82: } ( len: 1; sym: -36 ),
{ 83: } ( len: 1; sym: -36 ),
{ 84: } ( len: 1; sym: -36 ),
{ 85: } ( len: 1; sym: -36 ),
{ 86: } ( len: 1; sym: -36 ),
{ 87: } ( len: 1; sym: -36 ),
{ 88: } ( len: 0; sym: -35 ),
{ 89: } ( len: 3; sym: -35 ),
{ 90: } ( len: 1; sym: -38 ),
{ 91: } ( len: 3; sym: -38 ),
{ 92: } ( len: 1; sym: -39 ),
{ 93: } ( len: 5; sym: -30 ),
{ 94: } ( len: 1; sym: -40 ),
{ 95: } ( len: 2; sym: -40 ),
{ 96: } ( len: 0; sym: -41 ),
{ 97: } ( len: 1; sym: -41 ),
{ 98: } ( len: 1; sym: -41 ),
{ 99: } ( len: 11; sym: -31 ),
{ 100: } ( len: 1; sym: -42 ),
{ 101: } ( len: 1; sym: -42 ),
{ 102: } ( len: 1; sym: -42 ),
{ 103: } ( len: 1; sym: -42 ),
{ 104: } ( len: 1; sym: -42 ),
{ 105: } ( len: 1; sym: -42 ),
{ 106: } ( len: 1; sym: -42 ),
{ 107: } ( len: 1; sym: -42 ),
{ 108: } ( len: 1; sym: -42 ),
{ 109: } ( len: 1; sym: -42 ),
{ 110: } ( len: 1; sym: -42 ),
{ 111: } ( len: 1; sym: -42 ),
{ 112: } ( len: 0; sym: -44 ),
{ 113: } ( len: 4; sym: -32 ),
{ 114: } ( len: 1; sym: -43 ),
{ 115: } ( len: 1; sym: -43 ),
{ 116: } ( len: 1; sym: -45 ),
{ 117: } ( len: 3; sym: -45 ),
{ 118: } ( len: 2; sym: -47 ),
{ 119: } ( len: 0; sym: -48 ),
{ 120: } ( len: 2; sym: -48 ),
{ 121: } ( len: 1; sym: -49 ),
{ 122: } ( len: 1; sym: -49 ),
{ 123: } ( len: 1; sym: -46 ),
{ 124: } ( len: 3; sym: -46 ),
{ 125: } ( len: 5; sym: -21 ),
{ 126: } ( len: 0; sym: -50 ),
{ 127: } ( len: 1; sym: -50 ),
{ 128: } ( len: 1; sym: -51 ),
{ 129: } ( len: 2; sym: -51 ),
{ 130: } ( len: 1; sym: -52 ),
{ 131: } ( len: 1; sym: -52 ),
{ 132: } ( len: 1; sym: -52 ),
{ 133: } ( len: 10; sym: -53 ),
{ 134: } ( len: 2; sym: -54 ),
{ 135: } ( len: 1; sym: -54 ),
{ 136: } ( len: 1; sym: -56 ),
{ 137: } ( len: 2; sym: -56 ),
{ 138: } ( len: 1; sym: -55 ),
{ 139: } ( len: 1; sym: -55 ),
{ 140: } ( len: 3; sym: -57 ),
{ 141: } ( len: 0; sym: -58 ),
{ 142: } ( len: 1; sym: -58 ),
{ 143: } ( len: 1; sym: -59 ),
{ 144: } ( len: 2; sym: -59 ),
{ 145: } ( len: 1; sym: -60 )
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
