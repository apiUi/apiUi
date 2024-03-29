
unit XmlScanner;

interface
uses CustScanner, Classes, XmlParser;

type TXmlScanner = class (TCustScanner)
published
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnOutput: TNotifyEvent read FOnOutput write FOnOutput;
  property OnEcho: TNotifyEvent read FOnEcho write FOnEcho;
  property OnDefaultAction: TNotifyEvent read FOnDefaultAction write FOnDefaultAction;
  property OnError: TOnScannerErrorEvent read FOnError write FOnError;
public
  function yylex: Integer; override;
end;

var
  popState: Integer;

implementation
  uses SysUtils;

const InitState = 2;
const AttributeState = 4;
const ValueState = 6;
const CDataState = 8;
const IgnoreState = 10;


function TXmlScanner.yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                              begin
                                start (CDataState);
                                return (_CDATATAG);
                              end;
  2:
                              begin
                                popState := InitState;
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
  3:
                              begin
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
  4:
                              return (_ENDTAG);
  5:
                              return (_SLASHTAG);
  6:
                              begin
                                start (AttributeState);
                                return (_TAG);
                              end;
  7:
                              return (_IGNORE);
  8:
                              return (_ESCAPECHAR);
  9:
                              return (_VALUE);

  10:
                               begin
                                popState := ValueState;
                                start (IgnoreState);
                                return (_IGNORE);
                              end;
  11:
                              begin
                                start (CDataState);
                                return (_CDATATAG);
                              end;
  12:
                              begin
                                start (InitState);
                                return (_SLASHTAG);
                              end;
  13:
                              begin
                                start (AttributeState);
                                return (_TAG);
                              end;
  14:
                              return (_VALUE);
  15:
                              return (_ESCAPECHAR);
  16:
                              return (_VALUE);

  17:
                              begin
                                start (InitState);
                                return (_CDATAENDTAG);
                              end;
  18:
                              return (_CDATA);
  19:
                              return (_CDATA);
  20:
                              begin
                                start (InitState);
                                return (_IGNORE);
                              end;
  21:
                              begin
                                start (popState);
                                return (_IGNORE);
                              end;
  22:
                              return (_IGNORE);
  23:
                              return (_IGNORE);
  24:
                              return (_IS);
  25:
                              return (_ATTRIBUTENAME);
  26:
                              return (_ATTRIBUTEVALUE);
  27:
                              begin
                                start (ValueState);
                                return (_ENDTAG);
                              end;
  28:
                              begin
                                start (InitState);
                                return (_EMPTYENDTAG);
                              end;
  29:
                              return (_IGNORE);
  30:
                              return (_IGNORE);

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 54;
yynmatches = 54;
yyntrans   = 125;
yynstates  = 85;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
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
  9,
  { 13: }
  9,
  { 14: }
  4,
  9,
  { 15: }
  7,
  { 16: }
  9,
  { 17: }
  9,
  { 18: }
  24,
  29,
  { 19: }
  25,
  29,
  { 20: }
  29,
  { 21: }
  29,
  { 22: }
  29,
  { 23: }
  27,
  29,
  { 24: }
  29,
  { 25: }
  29,
  { 26: }
  30,
  { 27: }
  16,
  { 28: }
  16,
  { 29: }
  14,
  { 30: }
  16,
  { 31: }
  16,
  { 32: }
  18,
  { 33: }
  18,
  { 34: }
  19,
  { 35: }
  22,
  { 36: }
  22,
  { 37: }
  22,
  { 38: }
  23,
  { 39: }
  { 40: }
  3,
  { 41: }
  5,
  6,
  { 42: }
  6,
  { 43: }
  { 44: }
  4,
  { 45: }
  { 46: }
  25,
  { 47: }
  { 48: }
  26,
  { 49: }
  { 50: }
  { 51: }
  27,
  { 52: }
  28,
  { 53: }
  { 54: }
  12,
  13,
  { 55: }
  13,
  { 56: }
  { 57: }
  { 58: }
  { 59: }
  20,
  { 60: }
  { 61: }
  { 62: }
  { 63: }
  5,
  { 64: }
  8,
  { 65: }
  { 66: }
  { 67: }
  12,
  { 68: }
  15,
  { 69: }
  17,
  { 70: }
  21,
  { 71: }
  { 72: }
  2,
  { 73: }
  10,
  { 74: }
  { 75: }
  { 76: }
  { 77: }
  { 78: }
  { 79: }
  { 80: }
  { 81: }
  { 82: }
  { 83: }
  1,
  { 84: }
  11
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
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
  9,
{ 13: }
  9,
{ 14: }
  4,
  9,
{ 15: }
  7,
{ 16: }
  9,
{ 17: }
  9,
{ 18: }
  24,
  29,
{ 19: }
  25,
  29,
{ 20: }
  29,
{ 21: }
  29,
{ 22: }
  29,
{ 23: }
  27,
  29,
{ 24: }
  29,
{ 25: }
  29,
{ 26: }
  30,
{ 27: }
  16,
{ 28: }
  16,
{ 29: }
  14,
{ 30: }
  16,
{ 31: }
  16,
{ 32: }
  18,
{ 33: }
  18,
{ 34: }
  19,
{ 35: }
  22,
{ 36: }
  22,
{ 37: }
  22,
{ 38: }
  23,
{ 39: }
{ 40: }
  3,
{ 41: }
  5,
  6,
{ 42: }
  6,
{ 43: }
{ 44: }
  4,
{ 45: }
{ 46: }
  25,
{ 47: }
{ 48: }
  26,
{ 49: }
{ 50: }
{ 51: }
  27,
{ 52: }
  28,
{ 53: }
{ 54: }
  12,
  13,
{ 55: }
  13,
{ 56: }
{ 57: }
{ 58: }
{ 59: }
  20,
{ 60: }
{ 61: }
{ 62: }
{ 63: }
  5,
{ 64: }
  8,
{ 65: }
{ 66: }
{ 67: }
  12,
{ 68: }
  15,
{ 69: }
  17,
{ 70: }
  21,
{ 71: }
{ 72: }
  2,
{ 73: }
  10,
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
  1,
{ 84: }
  11
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
{ 1: }
{ 2: }
  ( cc: [ #1..#8,#11..#31,'!'..'%',''''..';','=','?'..#255 ]; s: 17),
  ( cc: [ #9,' ' ]; s: 13),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '>' ]; s: 14),
{ 3: }
  ( cc: [ #1..#8,#11..#31,'!'..'%',''''..';','=','?'..#255 ]; s: 17),
  ( cc: [ #9,' ' ]; s: 13),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '>' ]; s: 14),
{ 4: }
  ( cc: [ #1..#8,#11..#31,'!','#'..'&','('..'.','0'..'<',
            '?','@','['..'`','{'..#255 ]; s: 25),
  ( cc: [ #9,' ' ]; s: 22),
  ( cc: [ #10 ]; s: 26),
  ( cc: [ '"' ]; s: 20),
  ( cc: [ '''' ]; s: 21),
  ( cc: [ '/' ]; s: 24),
  ( cc: [ '=' ]; s: 18),
  ( cc: [ '>' ]; s: 23),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 19),
{ 5: }
  ( cc: [ #1..#8,#11..#31,'!','#'..'&','('..'.','0'..'<',
            '?','@','['..'`','{'..#255 ]; s: 25),
  ( cc: [ #9,' ' ]; s: 22),
  ( cc: [ #10 ]; s: 26),
  ( cc: [ '"' ]; s: 20),
  ( cc: [ '''' ]; s: 21),
  ( cc: [ '/' ]; s: 24),
  ( cc: [ '=' ]; s: 18),
  ( cc: [ '>' ]; s: 23),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 19),
{ 6: }
  ( cc: [ #1..#8,#11..#31,'!'..'%',''''..';','='..#255 ]; s: 31),
  ( cc: [ #9,' ' ]; s: 28),
  ( cc: [ #10 ]; s: 29),
  ( cc: [ '&' ]; s: 30),
  ( cc: [ '<' ]; s: 27),
{ 7: }
  ( cc: [ #1..#8,#11..#31,'!'..'%',''''..';','='..#255 ]; s: 31),
  ( cc: [ #9,' ' ]; s: 28),
  ( cc: [ #10 ]; s: 29),
  ( cc: [ '&' ]; s: 30),
  ( cc: [ '<' ]; s: 27),
{ 8: }
  ( cc: [ #1..#9,#11..'\','^'..#255 ]; s: 33),
  ( cc: [ #10 ]; s: 34),
  ( cc: [ ']' ]; s: 32),
{ 9: }
  ( cc: [ #1..#9,#11..'\','^'..#255 ]; s: 33),
  ( cc: [ #10 ]; s: 34),
  ( cc: [ ']' ]; s: 32),
{ 10: }
  ( cc: [ #1..#9,#11..',','.'..'>','@'..#255 ]; s: 37),
  ( cc: [ #10 ]; s: 38),
  ( cc: [ '-' ]; s: 36),
  ( cc: [ '?' ]; s: 35),
{ 11: }
  ( cc: [ #1..#9,#11..',','.'..'>','@'..#255 ]; s: 37),
  ( cc: [ #10 ]; s: 38),
  ( cc: [ '-' ]; s: 36),
  ( cc: [ '?' ]; s: 35),
{ 12: }
  ( cc: [ #1..' ','"'..'.','0'..'>','@'..#255 ]; s: 42),
  ( cc: [ '!' ]; s: 39),
  ( cc: [ '/' ]; s: 41),
  ( cc: [ '?' ]; s: 40),
{ 13: }
  ( cc: [ #9,' ' ]; s: 43),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '>' ]; s: 44),
{ 14: }
{ 15: }
  ( cc: [ #9,' ' ]; s: 15),
{ 16: }
  ( cc: [ #1..':','<'..#255 ]; s: 45),
{ 17: }
{ 18: }
{ 19: }
  ( cc: [ #1..#9,#11..#31,'!'..'<','>'..#255 ]; s: 46),
{ 20: }
  ( cc: [ #1..'!','#'..#255 ]; s: 47),
  ( cc: [ '"' ]; s: 48),
{ 21: }
  ( cc: [ #1..'&','('..#255 ]; s: 49),
  ( cc: [ '''' ]; s: 48),
{ 22: }
  ( cc: [ #9,' ' ]; s: 50),
  ( cc: [ '>' ]; s: 51),
{ 23: }
{ 24: }
  ( cc: [ '>' ]; s: 52),
{ 25: }
{ 26: }
{ 27: }
  ( cc: [ #1..' ','"'..'.','0'..'>','@'..#255 ]; s: 55),
  ( cc: [ '!' ]; s: 53),
  ( cc: [ '/' ]; s: 54),
{ 28: }
  ( cc: [ #9,' ' ]; s: 56),
  ( cc: [ #10 ]; s: 29),
{ 29: }
  ( cc: [ #9,' ' ]; s: 29),
{ 30: }
  ( cc: [ #1..':','<'..#255 ]; s: 57),
{ 31: }
{ 32: }
  ( cc: [ ']' ]; s: 58),
{ 33: }
{ 34: }
{ 35: }
  ( cc: [ '>' ]; s: 59),
{ 36: }
  ( cc: [ '-' ]; s: 60),
{ 37: }
{ 38: }
{ 39: }
  ( cc: [ '-' ]; s: 62),
  ( cc: [ '[' ]; s: 61),
{ 40: }
{ 41: }
  ( cc: [ #1..#8,#11..#31,'!'..'.','0'..'=','?'..#255 ]; s: 41),
  ( cc: [ #10,'/' ]; s: 63),
{ 42: }
  ( cc: [ #1..#8,#11..#31,'!'..'.','0'..'=','?'..#255 ]; s: 42),
{ 43: }
  ( cc: [ #9,' ' ]; s: 43),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '>' ]; s: 44),
{ 44: }
{ 45: }
  ( cc: [ #1..':','<'..#255 ]; s: 45),
  ( cc: [ ';' ]; s: 64),
{ 46: }
  ( cc: [ #1..#9,#11..#31,'!'..'<','>'..#255 ]; s: 46),
{ 47: }
  ( cc: [ #1..'!','#'..#255 ]; s: 47),
  ( cc: [ '"' ]; s: 48),
{ 48: }
{ 49: }
  ( cc: [ #1..'&','('..#255 ]; s: 49),
  ( cc: [ '''' ]; s: 48),
{ 50: }
  ( cc: [ #9,' ' ]; s: 50),
  ( cc: [ '>' ]; s: 51),
{ 51: }
{ 52: }
{ 53: }
  ( cc: [ '-' ]; s: 65),
  ( cc: [ '[' ]; s: 66),
{ 54: }
  ( cc: [ #1..#8,#11..#31,'!'..'.','0'..'=','?'..#255 ]; s: 54),
  ( cc: [ #10,'/' ]; s: 67),
{ 55: }
  ( cc: [ #1..#8,#11..#31,'!'..'.','0'..'=','?'..#255 ]; s: 55),
{ 56: }
  ( cc: [ #9,' ' ]; s: 56),
  ( cc: [ #10 ]; s: 29),
{ 57: }
  ( cc: [ #1..':','<'..#255 ]; s: 57),
  ( cc: [ ';' ]; s: 68),
{ 58: }
  ( cc: [ '>' ]; s: 69),
{ 59: }
{ 60: }
  ( cc: [ '>' ]; s: 70),
{ 61: }
  ( cc: [ 'C' ]; s: 71),
{ 62: }
  ( cc: [ '-' ]; s: 72),
{ 63: }
  ( cc: [ #1..#8,#10..#31,'!'..'=','?'..#255 ]; s: 63),
{ 64: }
{ 65: }
  ( cc: [ '-' ]; s: 73),
{ 66: }
  ( cc: [ 'C' ]; s: 74),
{ 67: }
  ( cc: [ #1..#8,#10..#31,'!'..'=','?'..#255 ]; s: 67),
{ 68: }
{ 69: }
{ 70: }
{ 71: }
  ( cc: [ 'D' ]; s: 75),
{ 72: }
{ 73: }
{ 74: }
  ( cc: [ 'D' ]; s: 76),
{ 75: }
  ( cc: [ 'A' ]; s: 77),
{ 76: }
  ( cc: [ 'A' ]; s: 78),
{ 77: }
  ( cc: [ 'T' ]; s: 79),
{ 78: }
  ( cc: [ 'T' ]; s: 80),
{ 79: }
  ( cc: [ 'A' ]; s: 81),
{ 80: }
  ( cc: [ 'A' ]; s: 82),
{ 81: }
  ( cc: [ '[' ]; s: 83),
{ 82: }
  ( cc: [ '[' ]; s: 84)
{ 83: }
{ 84: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 1,
{ 8: } 1,
{ 9: } 1,
{ 10: } 1,
{ 11: } 1,
{ 12: } 1,
{ 13: } 2,
{ 14: } 3,
{ 15: } 5,
{ 16: } 6,
{ 17: } 7,
{ 18: } 8,
{ 19: } 10,
{ 20: } 12,
{ 21: } 13,
{ 22: } 14,
{ 23: } 15,
{ 24: } 17,
{ 25: } 18,
{ 26: } 19,
{ 27: } 20,
{ 28: } 21,
{ 29: } 22,
{ 30: } 23,
{ 31: } 24,
{ 32: } 25,
{ 33: } 26,
{ 34: } 27,
{ 35: } 28,
{ 36: } 29,
{ 37: } 30,
{ 38: } 31,
{ 39: } 32,
{ 40: } 32,
{ 41: } 33,
{ 42: } 35,
{ 43: } 36,
{ 44: } 36,
{ 45: } 37,
{ 46: } 37,
{ 47: } 38,
{ 48: } 38,
{ 49: } 39,
{ 50: } 39,
{ 51: } 39,
{ 52: } 40,
{ 53: } 41,
{ 54: } 41,
{ 55: } 43,
{ 56: } 44,
{ 57: } 44,
{ 58: } 44,
{ 59: } 44,
{ 60: } 45,
{ 61: } 45,
{ 62: } 45,
{ 63: } 45,
{ 64: } 46,
{ 65: } 47,
{ 66: } 47,
{ 67: } 47,
{ 68: } 48,
{ 69: } 49,
{ 70: } 50,
{ 71: } 51,
{ 72: } 51,
{ 73: } 52,
{ 74: } 53,
{ 75: } 53,
{ 76: } 53,
{ 77: } 53,
{ 78: } 53,
{ 79: } 53,
{ 80: } 53,
{ 81: } 53,
{ 82: } 53,
{ 83: } 53,
{ 84: } 54
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 0,
{ 7: } 0,
{ 8: } 0,
{ 9: } 0,
{ 10: } 0,
{ 11: } 0,
{ 12: } 1,
{ 13: } 2,
{ 14: } 4,
{ 15: } 5,
{ 16: } 6,
{ 17: } 7,
{ 18: } 9,
{ 19: } 11,
{ 20: } 12,
{ 21: } 13,
{ 22: } 14,
{ 23: } 16,
{ 24: } 17,
{ 25: } 18,
{ 26: } 19,
{ 27: } 20,
{ 28: } 21,
{ 29: } 22,
{ 30: } 23,
{ 31: } 24,
{ 32: } 25,
{ 33: } 26,
{ 34: } 27,
{ 35: } 28,
{ 36: } 29,
{ 37: } 30,
{ 38: } 31,
{ 39: } 31,
{ 40: } 32,
{ 41: } 34,
{ 42: } 35,
{ 43: } 35,
{ 44: } 36,
{ 45: } 36,
{ 46: } 37,
{ 47: } 37,
{ 48: } 38,
{ 49: } 38,
{ 50: } 38,
{ 51: } 39,
{ 52: } 40,
{ 53: } 40,
{ 54: } 42,
{ 55: } 43,
{ 56: } 43,
{ 57: } 43,
{ 58: } 43,
{ 59: } 44,
{ 60: } 44,
{ 61: } 44,
{ 62: } 44,
{ 63: } 45,
{ 64: } 46,
{ 65: } 46,
{ 66: } 46,
{ 67: } 47,
{ 68: } 48,
{ 69: } 49,
{ 70: } 50,
{ 71: } 50,
{ 72: } 51,
{ 73: } 52,
{ 74: } 52,
{ 75: } 52,
{ 76: } 52,
{ 77: } 52,
{ 78: } 52,
{ 79: } 52,
{ 80: } 52,
{ 81: } 52,
{ 82: } 52,
{ 83: } 53,
{ 84: } 54
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 1,
{ 8: } 1,
{ 9: } 1,
{ 10: } 1,
{ 11: } 1,
{ 12: } 1,
{ 13: } 2,
{ 14: } 3,
{ 15: } 5,
{ 16: } 6,
{ 17: } 7,
{ 18: } 8,
{ 19: } 10,
{ 20: } 12,
{ 21: } 13,
{ 22: } 14,
{ 23: } 15,
{ 24: } 17,
{ 25: } 18,
{ 26: } 19,
{ 27: } 20,
{ 28: } 21,
{ 29: } 22,
{ 30: } 23,
{ 31: } 24,
{ 32: } 25,
{ 33: } 26,
{ 34: } 27,
{ 35: } 28,
{ 36: } 29,
{ 37: } 30,
{ 38: } 31,
{ 39: } 32,
{ 40: } 32,
{ 41: } 33,
{ 42: } 35,
{ 43: } 36,
{ 44: } 36,
{ 45: } 37,
{ 46: } 37,
{ 47: } 38,
{ 48: } 38,
{ 49: } 39,
{ 50: } 39,
{ 51: } 39,
{ 52: } 40,
{ 53: } 41,
{ 54: } 41,
{ 55: } 43,
{ 56: } 44,
{ 57: } 44,
{ 58: } 44,
{ 59: } 44,
{ 60: } 45,
{ 61: } 45,
{ 62: } 45,
{ 63: } 45,
{ 64: } 46,
{ 65: } 47,
{ 66: } 47,
{ 67: } 47,
{ 68: } 48,
{ 69: } 49,
{ 70: } 50,
{ 71: } 51,
{ 72: } 51,
{ 73: } 52,
{ 74: } 53,
{ 75: } 53,
{ 76: } 53,
{ 77: } 53,
{ 78: } 53,
{ 79: } 53,
{ 80: } 53,
{ 81: } 53,
{ 82: } 53,
{ 83: } 53,
{ 84: } 54
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 0,
{ 7: } 0,
{ 8: } 0,
{ 9: } 0,
{ 10: } 0,
{ 11: } 0,
{ 12: } 1,
{ 13: } 2,
{ 14: } 4,
{ 15: } 5,
{ 16: } 6,
{ 17: } 7,
{ 18: } 9,
{ 19: } 11,
{ 20: } 12,
{ 21: } 13,
{ 22: } 14,
{ 23: } 16,
{ 24: } 17,
{ 25: } 18,
{ 26: } 19,
{ 27: } 20,
{ 28: } 21,
{ 29: } 22,
{ 30: } 23,
{ 31: } 24,
{ 32: } 25,
{ 33: } 26,
{ 34: } 27,
{ 35: } 28,
{ 36: } 29,
{ 37: } 30,
{ 38: } 31,
{ 39: } 31,
{ 40: } 32,
{ 41: } 34,
{ 42: } 35,
{ 43: } 35,
{ 44: } 36,
{ 45: } 36,
{ 46: } 37,
{ 47: } 37,
{ 48: } 38,
{ 49: } 38,
{ 50: } 38,
{ 51: } 39,
{ 52: } 40,
{ 53: } 40,
{ 54: } 42,
{ 55: } 43,
{ 56: } 43,
{ 57: } 43,
{ 58: } 43,
{ 59: } 44,
{ 60: } 44,
{ 61: } 44,
{ 62: } 44,
{ 63: } 45,
{ 64: } 46,
{ 65: } 46,
{ 66: } 46,
{ 67: } 47,
{ 68: } 48,
{ 69: } 49,
{ 70: } 50,
{ 71: } 50,
{ 72: } 51,
{ 73: } 52,
{ 74: } 52,
{ 75: } 52,
{ 76: } 52,
{ 77: } 52,
{ 78: } 52,
{ 79: } 52,
{ 80: } 52,
{ 81: } 52,
{ 82: } 52,
{ 83: } 53,
{ 84: } 54
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 7,
{ 4: } 13,
{ 5: } 22,
{ 6: } 31,
{ 7: } 36,
{ 8: } 41,
{ 9: } 44,
{ 10: } 47,
{ 11: } 51,
{ 12: } 55,
{ 13: } 59,
{ 14: } 62,
{ 15: } 62,
{ 16: } 63,
{ 17: } 64,
{ 18: } 64,
{ 19: } 64,
{ 20: } 65,
{ 21: } 67,
{ 22: } 69,
{ 23: } 71,
{ 24: } 71,
{ 25: } 72,
{ 26: } 72,
{ 27: } 72,
{ 28: } 75,
{ 29: } 77,
{ 30: } 78,
{ 31: } 79,
{ 32: } 79,
{ 33: } 80,
{ 34: } 80,
{ 35: } 80,
{ 36: } 81,
{ 37: } 82,
{ 38: } 82,
{ 39: } 82,
{ 40: } 84,
{ 41: } 84,
{ 42: } 86,
{ 43: } 87,
{ 44: } 90,
{ 45: } 90,
{ 46: } 92,
{ 47: } 93,
{ 48: } 95,
{ 49: } 95,
{ 50: } 97,
{ 51: } 99,
{ 52: } 99,
{ 53: } 99,
{ 54: } 101,
{ 55: } 103,
{ 56: } 104,
{ 57: } 106,
{ 58: } 108,
{ 59: } 109,
{ 60: } 109,
{ 61: } 110,
{ 62: } 111,
{ 63: } 112,
{ 64: } 113,
{ 65: } 113,
{ 66: } 114,
{ 67: } 115,
{ 68: } 116,
{ 69: } 116,
{ 70: } 116,
{ 71: } 116,
{ 72: } 117,
{ 73: } 117,
{ 74: } 117,
{ 75: } 118,
{ 76: } 119,
{ 77: } 120,
{ 78: } 121,
{ 79: } 122,
{ 80: } 123,
{ 81: } 124,
{ 82: } 125,
{ 83: } 126,
{ 84: } 126
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 6,
{ 3: } 12,
{ 4: } 21,
{ 5: } 30,
{ 6: } 35,
{ 7: } 40,
{ 8: } 43,
{ 9: } 46,
{ 10: } 50,
{ 11: } 54,
{ 12: } 58,
{ 13: } 61,
{ 14: } 61,
{ 15: } 62,
{ 16: } 63,
{ 17: } 63,
{ 18: } 63,
{ 19: } 64,
{ 20: } 66,
{ 21: } 68,
{ 22: } 70,
{ 23: } 70,
{ 24: } 71,
{ 25: } 71,
{ 26: } 71,
{ 27: } 74,
{ 28: } 76,
{ 29: } 77,
{ 30: } 78,
{ 31: } 78,
{ 32: } 79,
{ 33: } 79,
{ 34: } 79,
{ 35: } 80,
{ 36: } 81,
{ 37: } 81,
{ 38: } 81,
{ 39: } 83,
{ 40: } 83,
{ 41: } 85,
{ 42: } 86,
{ 43: } 89,
{ 44: } 89,
{ 45: } 91,
{ 46: } 92,
{ 47: } 94,
{ 48: } 94,
{ 49: } 96,
{ 50: } 98,
{ 51: } 98,
{ 52: } 98,
{ 53: } 100,
{ 54: } 102,
{ 55: } 103,
{ 56: } 105,
{ 57: } 107,
{ 58: } 108,
{ 59: } 108,
{ 60: } 109,
{ 61: } 110,
{ 62: } 111,
{ 63: } 112,
{ 64: } 112,
{ 65: } 113,
{ 66: } 114,
{ 67: } 115,
{ 68: } 115,
{ 69: } 115,
{ 70: } 115,
{ 71: } 116,
{ 72: } 116,
{ 73: } 116,
{ 74: } 117,
{ 75: } 118,
{ 76: } 119,
{ 77: } 120,
{ 78: } 121,
{ 79: } 122,
{ 80: } 123,
{ 81: } 124,
{ 82: } 125,
{ 83: } 125,
{ 84: } 125
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
