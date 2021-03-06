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

unit proto3Scanner;

interface
uses CustScanner, Classes, proto3Parser;

type TProto3Scanner = class (TCustScanner)
published
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnOutput: TNotifyEvent read FOnOutput write FOnOutput;
  property OnEcho: TNotifyEvent read FOnEcho write FOnEcho;
  property OnDefaultAction: TNotifyEvent read FOnDefaultAction write FOnDefaultAction;
  property OnError: TOnScannerErrorEvent read FOnError write FOnError;
public
  function yylex: Integer; override;
end;

implementation
  uses SysUtils;

const InitState = 2;
const IgnoreState = 4;








function TProto3Scanner.yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                                return (_TERMINATOR);
  2:
                                return (_LINEENDING);
  3:
                                return (_IS);
  4:
                                return (_OPENPARENTHESIS);
  5:
                                return (_CLOSEPARENTHESIS);
  6:
                                return (_OPENBRACKET);
  7:
                                return (_CLOSEBRACKET);
  8:
                                return (_OPENBRACE);
  9:
                                return (_CLOSEBRACE);
  10:
                                return (_COMMA);
  11:
                                return (_PLUS);
  12:
                                return (_MINUS);
  13:
                       		return (_DECIMALLIT);
  14:
                     		return (_OCTALLIT);
  15:
                   		return (_HEXLIT);
  16:
                                return (_FLOATLIT);
  17:
                                return (_BOOLLIT);
  18:
                                return (_STRINGLIT);
  19:
                                return (_IDENT);
  20:
                                return (_FULLIDENT);
  21:
                                return (_IGNORE);
  22:
                                return (_IGNORE);
  23:
                                return (_DOT);
  24:
                                return (_LT);
  25:
                                return (_GT);
  26:
                                begin
                                  start (IgnoreState);
                                  return (_IGNORE);
                                end;
  27:
                                begin
                                  start (InitState);
                                  return (_IGNORE);
                                end;
  28:
                                return (_IGNORE);
  29:
                                return (_LINEENDING);

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 63;
yynmatches = 63;
yyntrans   = 142;
yynstates  = 62;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  { 3: }
  { 4: }
  { 5: }
  { 6: }
  1,
  { 7: }
  2,
  { 8: }
  3,
  { 9: }
  4,
  { 10: }
  5,
  { 11: }
  6,
  { 12: }
  7,
  { 13: }
  8,
  { 14: }
  9,
  { 15: }
  10,
  { 16: }
  11,
  { 17: }
  12,
  { 18: }
  13,
  { 19: }
  14,
  { 20: }
  23,
  { 21: }
  19,
  20,
  { 22: }
  19,
  20,
  { 23: }
  19,
  20,
  { 24: }
  19,
  20,
  { 25: }
  19,
  20,
  { 26: }
  { 27: }
  { 28: }
  19,
  20,
  { 29: }
  21,
  { 30: }
  { 31: }
  24,
  { 32: }
  25,
  { 33: }
  28,
  { 34: }
  28,
  { 35: }
  29,
  { 36: }
  { 37: }
  { 38: }
  14,
  { 39: }
  15,
  { 40: }
  { 41: }
  { 42: }
  { 43: }
  16,
  { 44: }
  { 45: }
  19,
  20,
  { 46: }
  19,
  20,
  { 47: }
  19,
  20,
  { 48: }
  19,
  20,
  { 49: }
  18,
  { 50: }
  22,
  { 51: }
  26,
  { 52: }
  27,
  { 53: }
  { 54: }
  16,
  { 55: }
  20,
  { 56: }
  16,
  19,
  20,
  { 57: }
  19,
  20,
  { 58: }
  19,
  20,
  { 59: }
  16,
  { 60: }
  17,
  19,
  20,
  { 61: }
  19,
  20
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  1,
{ 7: }
  2,
{ 8: }
  3,
{ 9: }
  4,
{ 10: }
  5,
{ 11: }
  6,
{ 12: }
  7,
{ 13: }
  8,
{ 14: }
  9,
{ 15: }
  10,
{ 16: }
  11,
{ 17: }
  12,
{ 18: }
  13,
{ 19: }
  14,
{ 20: }
  23,
{ 21: }
  19,
  20,
{ 22: }
  19,
  20,
{ 23: }
  19,
  20,
{ 24: }
  19,
  20,
{ 25: }
  19,
  20,
{ 26: }
{ 27: }
{ 28: }
  19,
  20,
{ 29: }
  21,
{ 30: }
{ 31: }
  24,
{ 32: }
  25,
{ 33: }
  28,
{ 34: }
  28,
{ 35: }
  29,
{ 36: }
{ 37: }
{ 38: }
  14,
{ 39: }
  15,
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  16,
{ 44: }
{ 45: }
  19,
  20,
{ 46: }
  19,
  20,
{ 47: }
  19,
  20,
{ 48: }
  19,
  20,
{ 49: }
  18,
{ 50: }
  22,
{ 51: }
  26,
{ 52: }
  27,
{ 53: }
{ 54: }
  16,
{ 55: }
  20,
{ 56: }
  16,
  19,
  20,
{ 57: }
  19,
  20,
{ 58: }
  19,
  20,
{ 59: }
  16,
{ 60: }
  17,
  19,
  20,
{ 61: }
  19,
  20
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
{ 1: }
{ 2: }
  ( cc: [ #9,' ' ]; s: 29),
  ( cc: [ #10 ]; s: 7),
  ( cc: [ '"' ]; s: 27),
  ( cc: [ '''' ]; s: 26),
  ( cc: [ '(' ]; s: 9),
  ( cc: [ ')' ]; s: 10),
  ( cc: [ '+' ]; s: 16),
  ( cc: [ ',' ]; s: 15),
  ( cc: [ '-' ]; s: 17),
  ( cc: [ '.' ]; s: 20),
  ( cc: [ '/' ]; s: 30),
  ( cc: [ '0' ]; s: 19),
  ( cc: [ '1'..'9' ]; s: 18),
  ( cc: [ ';' ]; s: 6),
  ( cc: [ '<' ]; s: 31),
  ( cc: [ '=' ]; s: 8),
  ( cc: [ '>' ]; s: 32),
  ( cc: [ 'A'..'D','F'..'Z','a'..'d','g','h','j'..'m',
            'o'..'s','u'..'z' ]; s: 28),
  ( cc: [ 'E','e' ]; s: 21),
  ( cc: [ '[' ]; s: 11),
  ( cc: [ ']' ]; s: 12),
  ( cc: [ 'f' ]; s: 25),
  ( cc: [ 'i' ]; s: 22),
  ( cc: [ 'n' ]; s: 23),
  ( cc: [ 't' ]; s: 24),
  ( cc: [ '{' ]; s: 13),
  ( cc: [ '}' ]; s: 14),
{ 3: }
  ( cc: [ #9,' ' ]; s: 29),
  ( cc: [ #10 ]; s: 7),
  ( cc: [ '"' ]; s: 27),
  ( cc: [ '''' ]; s: 26),
  ( cc: [ '(' ]; s: 9),
  ( cc: [ ')' ]; s: 10),
  ( cc: [ '+' ]; s: 16),
  ( cc: [ ',' ]; s: 15),
  ( cc: [ '-' ]; s: 17),
  ( cc: [ '.' ]; s: 20),
  ( cc: [ '/' ]; s: 30),
  ( cc: [ '0' ]; s: 19),
  ( cc: [ '1'..'9' ]; s: 18),
  ( cc: [ ';' ]; s: 6),
  ( cc: [ '<' ]; s: 31),
  ( cc: [ '=' ]; s: 8),
  ( cc: [ '>' ]; s: 32),
  ( cc: [ 'A'..'D','F'..'Z','a'..'d','g','h','j'..'m',
            'o'..'s','u'..'z' ]; s: 28),
  ( cc: [ 'E','e' ]; s: 21),
  ( cc: [ '[' ]; s: 11),
  ( cc: [ ']' ]; s: 12),
  ( cc: [ 'f' ]; s: 25),
  ( cc: [ 'i' ]; s: 22),
  ( cc: [ 'n' ]; s: 23),
  ( cc: [ 't' ]; s: 24),
  ( cc: [ '{' ]; s: 13),
  ( cc: [ '}' ]; s: 14),
{ 4: }
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 34),
  ( cc: [ #10 ]; s: 35),
  ( cc: [ '*' ]; s: 33),
{ 5: }
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 34),
  ( cc: [ #10 ]; s: 35),
  ( cc: [ '*' ]; s: 33),
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'9' ]; s: 18),
  ( cc: [ 'E','e' ]; s: 37),
{ 19: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'7' ]; s: 38),
  ( cc: [ '8','9' ]; s: 40),
  ( cc: [ 'E','e' ]; s: 37),
  ( cc: [ 'X','x' ]; s: 39),
{ 20: }
  ( cc: [ '0'..'9' ]; s: 41),
  ( cc: [ 'E','e' ]; s: 42),
{ 21: }
  ( cc: [ '+','-','|' ]; s: 43),
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 28),
{ 22: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 28),
  ( cc: [ 'n' ]; s: 45),
{ 23: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 28),
  ( cc: [ 'a' ]; s: 46),
{ 24: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 28),
  ( cc: [ 'r' ]; s: 47),
{ 25: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 28),
  ( cc: [ 'a' ]; s: 48),
{ 26: }
  ( cc: [ #1..'&','('..#255 ]; s: 26),
  ( cc: [ '''' ]; s: 49),
{ 27: }
  ( cc: [ #1..'!','#'..#255 ]; s: 27),
  ( cc: [ '"' ]; s: 49),
{ 28: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 28),
{ 29: }
  ( cc: [ #9,' ' ]; s: 29),
{ 30: }
  ( cc: [ '*' ]; s: 51),
  ( cc: [ '/' ]; s: 50),
{ 31: }
{ 32: }
{ 33: }
  ( cc: [ '/' ]; s: 52),
{ 34: }
{ 35: }
{ 36: }
  ( cc: [ '0'..'9' ]; s: 36),
  ( cc: [ 'E','e' ]; s: 53),
{ 37: }
  ( cc: [ '+','-','|' ]; s: 43),
{ 38: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'7' ]; s: 38),
  ( cc: [ '8','9' ]; s: 40),
  ( cc: [ 'E','e' ]; s: 37),
{ 39: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 39),
{ 40: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'9' ]; s: 40),
  ( cc: [ 'E','e' ]; s: 37),
{ 41: }
  ( cc: [ '0'..'9' ]; s: 41),
  ( cc: [ 'E','e' ]; s: 42),
{ 42: }
  ( cc: [ '+','-','|' ]; s: 54),
{ 43: }
  ( cc: [ '0'..'9' ]; s: 43),
{ 44: }
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 55),
{ 45: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 28),
  ( cc: [ 'f' ]; s: 56),
{ 46: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 28),
  ( cc: [ 'n' ]; s: 56),
{ 47: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 28),
  ( cc: [ 'u' ]; s: 57),
{ 48: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'k','m'..'z' ]; s: 28),
  ( cc: [ 'l' ]; s: 58),
{ 49: }
{ 50: }
  ( cc: [ #1..#9,#11..#255 ]; s: 50),
{ 51: }
{ 52: }
{ 53: }
  ( cc: [ '+','-','|' ]; s: 59),
{ 54: }
  ( cc: [ '0'..'9' ]; s: 54),
{ 55: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 55),
{ 56: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 28),
{ 57: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 28),
  ( cc: [ 'e' ]; s: 60),
{ 58: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 28),
  ( cc: [ 's' ]; s: 61),
{ 59: }
  ( cc: [ '0'..'9' ]; s: 59),
{ 60: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 28),
{ 61: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 28),
  ( cc: [ 'e' ]; s: 60)
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 2,
{ 8: } 3,
{ 9: } 4,
{ 10: } 5,
{ 11: } 6,
{ 12: } 7,
{ 13: } 8,
{ 14: } 9,
{ 15: } 10,
{ 16: } 11,
{ 17: } 12,
{ 18: } 13,
{ 19: } 14,
{ 20: } 15,
{ 21: } 16,
{ 22: } 18,
{ 23: } 20,
{ 24: } 22,
{ 25: } 24,
{ 26: } 26,
{ 27: } 26,
{ 28: } 26,
{ 29: } 28,
{ 30: } 29,
{ 31: } 29,
{ 32: } 30,
{ 33: } 31,
{ 34: } 32,
{ 35: } 33,
{ 36: } 34,
{ 37: } 34,
{ 38: } 34,
{ 39: } 35,
{ 40: } 36,
{ 41: } 36,
{ 42: } 36,
{ 43: } 36,
{ 44: } 37,
{ 45: } 37,
{ 46: } 39,
{ 47: } 41,
{ 48: } 43,
{ 49: } 45,
{ 50: } 46,
{ 51: } 47,
{ 52: } 48,
{ 53: } 49,
{ 54: } 49,
{ 55: } 50,
{ 56: } 51,
{ 57: } 54,
{ 58: } 56,
{ 59: } 58,
{ 60: } 59,
{ 61: } 62
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 1,
{ 7: } 2,
{ 8: } 3,
{ 9: } 4,
{ 10: } 5,
{ 11: } 6,
{ 12: } 7,
{ 13: } 8,
{ 14: } 9,
{ 15: } 10,
{ 16: } 11,
{ 17: } 12,
{ 18: } 13,
{ 19: } 14,
{ 20: } 15,
{ 21: } 17,
{ 22: } 19,
{ 23: } 21,
{ 24: } 23,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 27,
{ 29: } 28,
{ 30: } 28,
{ 31: } 29,
{ 32: } 30,
{ 33: } 31,
{ 34: } 32,
{ 35: } 33,
{ 36: } 33,
{ 37: } 33,
{ 38: } 34,
{ 39: } 35,
{ 40: } 35,
{ 41: } 35,
{ 42: } 35,
{ 43: } 36,
{ 44: } 36,
{ 45: } 38,
{ 46: } 40,
{ 47: } 42,
{ 48: } 44,
{ 49: } 45,
{ 50: } 46,
{ 51: } 47,
{ 52: } 48,
{ 53: } 48,
{ 54: } 49,
{ 55: } 50,
{ 56: } 53,
{ 57: } 55,
{ 58: } 57,
{ 59: } 58,
{ 60: } 61,
{ 61: } 63
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 2,
{ 8: } 3,
{ 9: } 4,
{ 10: } 5,
{ 11: } 6,
{ 12: } 7,
{ 13: } 8,
{ 14: } 9,
{ 15: } 10,
{ 16: } 11,
{ 17: } 12,
{ 18: } 13,
{ 19: } 14,
{ 20: } 15,
{ 21: } 16,
{ 22: } 18,
{ 23: } 20,
{ 24: } 22,
{ 25: } 24,
{ 26: } 26,
{ 27: } 26,
{ 28: } 26,
{ 29: } 28,
{ 30: } 29,
{ 31: } 29,
{ 32: } 30,
{ 33: } 31,
{ 34: } 32,
{ 35: } 33,
{ 36: } 34,
{ 37: } 34,
{ 38: } 34,
{ 39: } 35,
{ 40: } 36,
{ 41: } 36,
{ 42: } 36,
{ 43: } 36,
{ 44: } 37,
{ 45: } 37,
{ 46: } 39,
{ 47: } 41,
{ 48: } 43,
{ 49: } 45,
{ 50: } 46,
{ 51: } 47,
{ 52: } 48,
{ 53: } 49,
{ 54: } 49,
{ 55: } 50,
{ 56: } 51,
{ 57: } 54,
{ 58: } 56,
{ 59: } 58,
{ 60: } 59,
{ 61: } 62
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 1,
{ 7: } 2,
{ 8: } 3,
{ 9: } 4,
{ 10: } 5,
{ 11: } 6,
{ 12: } 7,
{ 13: } 8,
{ 14: } 9,
{ 15: } 10,
{ 16: } 11,
{ 17: } 12,
{ 18: } 13,
{ 19: } 14,
{ 20: } 15,
{ 21: } 17,
{ 22: } 19,
{ 23: } 21,
{ 24: } 23,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 27,
{ 29: } 28,
{ 30: } 28,
{ 31: } 29,
{ 32: } 30,
{ 33: } 31,
{ 34: } 32,
{ 35: } 33,
{ 36: } 33,
{ 37: } 33,
{ 38: } 34,
{ 39: } 35,
{ 40: } 35,
{ 41: } 35,
{ 42: } 35,
{ 43: } 36,
{ 44: } 36,
{ 45: } 38,
{ 46: } 40,
{ 47: } 42,
{ 48: } 44,
{ 49: } 45,
{ 50: } 46,
{ 51: } 47,
{ 52: } 48,
{ 53: } 48,
{ 54: } 49,
{ 55: } 50,
{ 56: } 53,
{ 57: } 55,
{ 58: } 57,
{ 59: } 58,
{ 60: } 61,
{ 61: } 63
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 28,
{ 4: } 55,
{ 5: } 58,
{ 6: } 61,
{ 7: } 61,
{ 8: } 61,
{ 9: } 61,
{ 10: } 61,
{ 11: } 61,
{ 12: } 61,
{ 13: } 61,
{ 14: } 61,
{ 15: } 61,
{ 16: } 61,
{ 17: } 61,
{ 18: } 61,
{ 19: } 64,
{ 20: } 69,
{ 21: } 71,
{ 22: } 74,
{ 23: } 77,
{ 24: } 80,
{ 25: } 83,
{ 26: } 86,
{ 27: } 88,
{ 28: } 90,
{ 29: } 92,
{ 30: } 93,
{ 31: } 95,
{ 32: } 95,
{ 33: } 95,
{ 34: } 96,
{ 35: } 96,
{ 36: } 96,
{ 37: } 98,
{ 38: } 99,
{ 39: } 103,
{ 40: } 104,
{ 41: } 107,
{ 42: } 109,
{ 43: } 110,
{ 44: } 111,
{ 45: } 112,
{ 46: } 115,
{ 47: } 118,
{ 48: } 121,
{ 49: } 124,
{ 50: } 124,
{ 51: } 125,
{ 52: } 125,
{ 53: } 125,
{ 54: } 126,
{ 55: } 127,
{ 56: } 129,
{ 57: } 131,
{ 58: } 134,
{ 59: } 137,
{ 60: } 138,
{ 61: } 140
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 27,
{ 3: } 54,
{ 4: } 57,
{ 5: } 60,
{ 6: } 60,
{ 7: } 60,
{ 8: } 60,
{ 9: } 60,
{ 10: } 60,
{ 11: } 60,
{ 12: } 60,
{ 13: } 60,
{ 14: } 60,
{ 15: } 60,
{ 16: } 60,
{ 17: } 60,
{ 18: } 63,
{ 19: } 68,
{ 20: } 70,
{ 21: } 73,
{ 22: } 76,
{ 23: } 79,
{ 24: } 82,
{ 25: } 85,
{ 26: } 87,
{ 27: } 89,
{ 28: } 91,
{ 29: } 92,
{ 30: } 94,
{ 31: } 94,
{ 32: } 94,
{ 33: } 95,
{ 34: } 95,
{ 35: } 95,
{ 36: } 97,
{ 37: } 98,
{ 38: } 102,
{ 39: } 103,
{ 40: } 106,
{ 41: } 108,
{ 42: } 109,
{ 43: } 110,
{ 44: } 111,
{ 45: } 114,
{ 46: } 117,
{ 47: } 120,
{ 48: } 123,
{ 49: } 123,
{ 50: } 124,
{ 51: } 124,
{ 52: } 124,
{ 53: } 125,
{ 54: } 126,
{ 55: } 128,
{ 56: } 130,
{ 57: } 133,
{ 58: } 136,
{ 59: } 137,
{ 60: } 139,
{ 61: } 142
);


var yyn : Integer;

label start_, scan, action;

begin

start_:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap then
    begin
      yyclear;
      return (0);
    end;

  if not yydone then goto start_;

  yylex := FToken;

end(*yylex*);


end.
