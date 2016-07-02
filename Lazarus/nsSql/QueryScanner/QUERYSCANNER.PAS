
unit QueryScanner;

interface
uses CustScanner, Classes;
const _DEFINE_NAME = 257;
const _WHITESPACE = 258;
const _STRING = 259;
const _SELECTSTAR = 260;
const _SEPARATOR = 261;
const _ANY = 262;

type TQueryScanner = class (TCustScanner)
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



function TQueryScanner.yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                return (_STRING);
  2:
                return (_STRING);
  3:
                return (_ANY);
  4:
                return (_DEFINE_NAME);
  5:
                return (_WHITESPACE);
  6:
                return (_SELECTSTAR);
  7:
                return (_SEPARATOR);
  8:
                return (_ANY);

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 20;
yynmatches = 20;
yyntrans   = 87;
yynstates  = 40;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  5,
  { 1: }
  5,
  { 2: }
  8,
  { 3: }
  8,
  { 4: }
  8,
  { 5: }
  5,
  8,
  { 6: }
  5,
  { 7: }
  8,
  { 8: }
  8,
  { 9: }
  7,
  8,
  { 10: }
  8,
  { 11: }
  { 12: }
  { 13: }
  1,
  { 14: }
  { 15: }
  { 16: }
  2,
  { 17: }
  4,
  { 18: }
  { 19: }
  { 20: }
  { 21: }
  { 22: }
  { 23: }
  { 24: }
  { 25: }
  1,
  { 26: }
  2,
  { 27: }
  3,
  { 28: }
  { 29: }
  { 30: }
  { 31: }
  { 32: }
  { 33: }
  { 34: }
  { 35: }
  { 36: }
  { 37: }
  { 38: }
  { 39: }
  6
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
  5,
{ 1: }
  5,
{ 2: }
  8,
{ 3: }
  8,
{ 4: }
  8,
{ 5: }
  5,
  8,
{ 6: }
  5,
{ 7: }
  8,
{ 8: }
  8,
{ 9: }
  7,
  8,
{ 10: }
  8,
{ 11: }
{ 12: }
{ 13: }
  1,
{ 14: }
{ 15: }
{ 16: }
  2,
{ 17: }
  4,
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  1,
{ 26: }
  2,
{ 27: }
  3,
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
  6
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'!','#'..'&','('..',',
            '.'..':','<','>'..'R','T'..'r','t'..#255 ]; s: 10),
  ( cc: [ #9,#12,' ' ]; s: 5),
  ( cc: [ #10 ]; s: 6),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '''' ]; s: 2),
  ( cc: [ '-' ]; s: 7),
  ( cc: [ ';' ]; s: 9),
  ( cc: [ '=' ]; s: 4),
  ( cc: [ 'S','s' ]; s: 8),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'!','#'..'&','('..',',
            '.'..':','<','>'..'R','T'..'r','t'..#255 ]; s: 10),
  ( cc: [ #9,#12,' ' ]; s: 5),
  ( cc: [ #10 ]; s: 6),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '''' ]; s: 2),
  ( cc: [ '-' ]; s: 7),
  ( cc: [ ';' ]; s: 9),
  ( cc: [ '=' ]; s: 4),
  ( cc: [ 'S','s' ]; s: 8),
{ 2: }
  ( cc: [ #1..#9,#11..'&','('..#255 ]; s: 11),
  ( cc: [ #10 ]; s: 12),
  ( cc: [ '''' ]; s: 13),
{ 3: }
  ( cc: [ #1..#9,#11..'!','#'..#255 ]; s: 14),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '"' ]; s: 16),
{ 4: }
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 17),
{ 5: }
  ( cc: [ #9,#10,#12,' ' ]; s: 6),
  ( cc: [ '-' ]; s: 18),
{ 6: }
  ( cc: [ #9,#10,#12,' ' ]; s: 6),
  ( cc: [ '-' ]; s: 18),
{ 7: }
  ( cc: [ '-' ]; s: 19),
{ 8: }
  ( cc: [ 'E','e' ]; s: 20),
{ 9: }
{ 10: }
{ 11: }
  ( cc: [ #1..#9,#11..'&','('..#255 ]; s: 11),
  ( cc: [ #10 ]; s: 12),
  ( cc: [ '''' ]; s: 13),
{ 12: }
  ( cc: [ #1..#9,#11..'&','('..',','.'..#255 ]; s: 11),
  ( cc: [ #10 ]; s: 12),
  ( cc: [ '''' ]; s: 13),
  ( cc: [ '-' ]; s: 21),
{ 13: }
  ( cc: [ '''' ]; s: 11),
{ 14: }
  ( cc: [ #1..#9,#11..'!','#'..#255 ]; s: 14),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '"' ]; s: 16),
{ 15: }
  ( cc: [ #1..#9,#11..'!','#'..',','.'..#255 ]; s: 14),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '"' ]; s: 16),
  ( cc: [ '-' ]; s: 22),
{ 16: }
  ( cc: [ '"' ]; s: 14),
{ 17: }
  ( cc: [ '-','0'..'9','A'..'Z','_','a'..'z' ]; s: 17),
  ( cc: [ '.' ]; s: 23),
{ 18: }
  ( cc: [ '-' ]; s: 19),
{ 19: }
  ( cc: [ #1..#9,#11..#255 ]; s: 19),
  ( cc: [ #10 ]; s: 6),
{ 20: }
  ( cc: [ 'L','l' ]; s: 24),
{ 21: }
  ( cc: [ #1..#8,#11,#13..#31,'!'..'&','('..#255 ]; s: 11),
  ( cc: [ #9,#12,' ' ]; s: 21),
  ( cc: [ #10 ]; s: 12),
  ( cc: [ '''' ]; s: 25),
{ 22: }
  ( cc: [ #1..#8,#11,#13..#31,'!','#'..#255 ]; s: 14),
  ( cc: [ #9,#12,' ' ]; s: 22),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '"' ]; s: 26),
{ 23: }
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 27),
{ 24: }
  ( cc: [ 'E','e' ]; s: 28),
{ 25: }
  ( cc: [ #1..#9,#11..'&','('..#255 ]; s: 11),
  ( cc: [ #10 ]; s: 12),
  ( cc: [ '''' ]; s: 25),
{ 26: }
  ( cc: [ #1..#9,#11..'!','#'..#255 ]; s: 14),
  ( cc: [ #10 ]; s: 15),
  ( cc: [ '"' ]; s: 26),
{ 27: }
  ( cc: [ '-','0'..'9','A'..'Z','_','a'..'z' ]; s: 27),
{ 28: }
  ( cc: [ 'C','c' ]; s: 29),
{ 29: }
  ( cc: [ 'T','t' ]; s: 30),
{ 30: }
  ( cc: [ #9,#10,#12,' ' ]; s: 30),
  ( cc: [ '*' ]; s: 32),
  ( cc: [ '-' ]; s: 31),
{ 31: }
  ( cc: [ '-' ]; s: 33),
{ 32: }
  ( cc: [ #9,#10,#12,' ' ]; s: 32),
  ( cc: [ '-' ]; s: 34),
  ( cc: [ 'F','f' ]; s: 35),
{ 33: }
  ( cc: [ #1..#9,#11..#255 ]; s: 33),
  ( cc: [ #10 ]; s: 30),
{ 34: }
  ( cc: [ '-' ]; s: 36),
{ 35: }
  ( cc: [ 'R','r' ]; s: 37),
{ 36: }
  ( cc: [ #1..#9,#11..#255 ]; s: 36),
  ( cc: [ #10 ]; s: 32),
{ 37: }
  ( cc: [ 'O','o' ]; s: 38),
{ 38: }
  ( cc: [ 'M','m' ]; s: 39)
{ 39: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 4,
{ 4: } 5,
{ 5: } 6,
{ 6: } 8,
{ 7: } 9,
{ 8: } 10,
{ 9: } 11,
{ 10: } 13,
{ 11: } 14,
{ 12: } 14,
{ 13: } 14,
{ 14: } 15,
{ 15: } 15,
{ 16: } 15,
{ 17: } 16,
{ 18: } 17,
{ 19: } 17,
{ 20: } 17,
{ 21: } 17,
{ 22: } 17,
{ 23: } 17,
{ 24: } 17,
{ 25: } 17,
{ 26: } 18,
{ 27: } 19,
{ 28: } 20,
{ 29: } 20,
{ 30: } 20,
{ 31: } 20,
{ 32: } 20,
{ 33: } 20,
{ 34: } 20,
{ 35: } 20,
{ 36: } 20,
{ 37: } 20,
{ 38: } 20,
{ 39: } 20
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 4,
{ 4: } 5,
{ 5: } 7,
{ 6: } 8,
{ 7: } 9,
{ 8: } 10,
{ 9: } 12,
{ 10: } 13,
{ 11: } 13,
{ 12: } 13,
{ 13: } 14,
{ 14: } 14,
{ 15: } 14,
{ 16: } 15,
{ 17: } 16,
{ 18: } 16,
{ 19: } 16,
{ 20: } 16,
{ 21: } 16,
{ 22: } 16,
{ 23: } 16,
{ 24: } 16,
{ 25: } 17,
{ 26: } 18,
{ 27: } 19,
{ 28: } 19,
{ 29: } 19,
{ 30: } 19,
{ 31: } 19,
{ 32: } 19,
{ 33: } 19,
{ 34: } 19,
{ 35: } 19,
{ 36: } 19,
{ 37: } 19,
{ 38: } 19,
{ 39: } 20
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 4,
{ 4: } 5,
{ 5: } 6,
{ 6: } 8,
{ 7: } 9,
{ 8: } 10,
{ 9: } 11,
{ 10: } 13,
{ 11: } 14,
{ 12: } 14,
{ 13: } 14,
{ 14: } 15,
{ 15: } 15,
{ 16: } 15,
{ 17: } 16,
{ 18: } 17,
{ 19: } 17,
{ 20: } 17,
{ 21: } 17,
{ 22: } 17,
{ 23: } 17,
{ 24: } 17,
{ 25: } 17,
{ 26: } 18,
{ 27: } 19,
{ 28: } 20,
{ 29: } 20,
{ 30: } 20,
{ 31: } 20,
{ 32: } 20,
{ 33: } 20,
{ 34: } 20,
{ 35: } 20,
{ 36: } 20,
{ 37: } 20,
{ 38: } 20,
{ 39: } 20
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 4,
{ 4: } 5,
{ 5: } 7,
{ 6: } 8,
{ 7: } 9,
{ 8: } 10,
{ 9: } 12,
{ 10: } 13,
{ 11: } 13,
{ 12: } 13,
{ 13: } 14,
{ 14: } 14,
{ 15: } 14,
{ 16: } 15,
{ 17: } 16,
{ 18: } 16,
{ 19: } 16,
{ 20: } 16,
{ 21: } 16,
{ 22: } 16,
{ 23: } 16,
{ 24: } 16,
{ 25: } 17,
{ 26: } 18,
{ 27: } 19,
{ 28: } 19,
{ 29: } 19,
{ 30: } 19,
{ 31: } 19,
{ 32: } 19,
{ 33: } 19,
{ 34: } 19,
{ 35: } 19,
{ 36: } 19,
{ 37: } 19,
{ 38: } 19,
{ 39: } 20
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 10,
{ 2: } 19,
{ 3: } 22,
{ 4: } 25,
{ 5: } 26,
{ 6: } 28,
{ 7: } 30,
{ 8: } 31,
{ 9: } 32,
{ 10: } 32,
{ 11: } 32,
{ 12: } 35,
{ 13: } 39,
{ 14: } 40,
{ 15: } 43,
{ 16: } 47,
{ 17: } 48,
{ 18: } 50,
{ 19: } 51,
{ 20: } 53,
{ 21: } 54,
{ 22: } 58,
{ 23: } 62,
{ 24: } 63,
{ 25: } 64,
{ 26: } 67,
{ 27: } 70,
{ 28: } 71,
{ 29: } 72,
{ 30: } 73,
{ 31: } 76,
{ 32: } 77,
{ 33: } 80,
{ 34: } 82,
{ 35: } 83,
{ 36: } 84,
{ 37: } 86,
{ 38: } 87,
{ 39: } 88
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 9,
{ 1: } 18,
{ 2: } 21,
{ 3: } 24,
{ 4: } 25,
{ 5: } 27,
{ 6: } 29,
{ 7: } 30,
{ 8: } 31,
{ 9: } 31,
{ 10: } 31,
{ 11: } 34,
{ 12: } 38,
{ 13: } 39,
{ 14: } 42,
{ 15: } 46,
{ 16: } 47,
{ 17: } 49,
{ 18: } 50,
{ 19: } 52,
{ 20: } 53,
{ 21: } 57,
{ 22: } 61,
{ 23: } 62,
{ 24: } 63,
{ 25: } 66,
{ 26: } 69,
{ 27: } 70,
{ 28: } 71,
{ 29: } 72,
{ 30: } 75,
{ 31: } 76,
{ 32: } 79,
{ 33: } 81,
{ 34: } 82,
{ 35: } 83,
{ 36: } 85,
{ 37: } 86,
{ 38: } 87,
{ 39: } 87
);


var yyn : Integer;

label startlabel, scan, action;

begin

startlabel:

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

  if not yydone then goto startlabel;

  yylex := FToken;

end(*yylex*);


end.
