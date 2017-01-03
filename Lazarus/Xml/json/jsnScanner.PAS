
unit jsnScanner;

interface
uses CustScanner, Classes, jsnParser;

type TjsnScanner = class (TCustScanner)
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



function TjsnScanner.yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                              return (_STRING);
  2:
                              return (_IGNORE);
  3:
                              return (_COLON);
  4:
                              return (_COMMA);
  5:
                              return (_LEFT_SQUARE_BRACKET);
  6:
                              return (_RIGHT_SQUARE_BRACKET);
  7:
                              return (_LEFT_CURLY_BRACKET);
  8:
                              return (_RIGHT_CURLY_BRACKET);
  9:
                              return (_NUMBER);
  10:
                              return (_FALSE);
  11:
                              return (_NULL);
  12:
                              return (_TRUE);

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 17;
yynmatches = 17;
yyntrans   = 57;
yynstates  = 36;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  9,
  { 3: }
  9,
  { 4: }
  { 5: }
  { 6: }
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
  { 16: }
  { 17: }
  { 18: }
  { 19: }
  { 20: }
  { 21: }
  1,
  { 22: }
  9,
  { 23: }
  { 24: }
  9,
  { 25: }
  { 26: }
  { 27: }
  { 28: }
  1,
  { 29: }
  { 30: }
  { 31: }
  { 32: }
  { 33: }
  11,
  { 34: }
  12,
  { 35: }
  10
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  9,
{ 3: }
  9,
{ 4: }
{ 5: }
{ 6: }
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
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  1,
{ 22: }
  9,
{ 23: }
{ 24: }
  9,
{ 25: }
{ 26: }
{ 27: }
{ 28: }
  1,
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  11,
{ 34: }
  12,
{ 35: }
  10
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
{ 1: }
{ 2: }
  ( cc: [ #9,#10,#13,' ' ]; s: 7),
  ( cc: [ '"' ]; s: 6),
  ( cc: [ ',' ]; s: 9),
  ( cc: [ '.' ]; s: 15),
  ( cc: [ '0'..'9' ]; s: 14),
  ( cc: [ ':' ]; s: 8),
  ( cc: [ 'E','e' ]; s: 16),
  ( cc: [ '[' ]; s: 10),
  ( cc: [ ']' ]; s: 11),
  ( cc: [ 'f' ]; s: 17),
  ( cc: [ 'n' ]; s: 18),
  ( cc: [ 't' ]; s: 19),
  ( cc: [ '{' ]; s: 12),
  ( cc: [ '}' ]; s: 13),
{ 3: }
  ( cc: [ #9,#10,#13,' ' ]; s: 7),
  ( cc: [ '"' ]; s: 6),
  ( cc: [ ',' ]; s: 9),
  ( cc: [ '.' ]; s: 15),
  ( cc: [ '0'..'9' ]; s: 14),
  ( cc: [ ':' ]; s: 8),
  ( cc: [ 'E','e' ]; s: 16),
  ( cc: [ '[' ]; s: 10),
  ( cc: [ ']' ]; s: 11),
  ( cc: [ 'f' ]; s: 17),
  ( cc: [ 'n' ]; s: 18),
  ( cc: [ 't' ]; s: 19),
  ( cc: [ '{' ]; s: 12),
  ( cc: [ '}' ]; s: 13),
{ 4: }
{ 5: }
{ 6: }
  ( cc: [ #1..'!','#'..'[',']'..#255 ]; s: 6),
  ( cc: [ '"' ]; s: 21),
  ( cc: [ '\' ]; s: 20),
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
  ( cc: [ '.' ]; s: 15),
  ( cc: [ '0'..'9' ]; s: 14),
  ( cc: [ 'E','e' ]; s: 16),
{ 15: }
  ( cc: [ '0'..'9' ]; s: 22),
{ 16: }
  ( cc: [ '+','-' ]; s: 23),
  ( cc: [ '0'..'9' ]; s: 24),
{ 17: }
  ( cc: [ 'a' ]; s: 25),
{ 18: }
  ( cc: [ 'u' ]; s: 26),
{ 19: }
  ( cc: [ 'r' ]; s: 27),
{ 20: }
  ( cc: [ #1..'!','#'..'[',']'..#255 ]; s: 6),
  ( cc: [ '"' ]; s: 28),
  ( cc: [ '\' ]; s: 20),
{ 21: }
{ 22: }
  ( cc: [ '0'..'9' ]; s: 22),
  ( cc: [ 'E','e' ]; s: 16),
{ 23: }
  ( cc: [ '0'..'9' ]; s: 24),
{ 24: }
  ( cc: [ '0'..'9' ]; s: 24),
{ 25: }
  ( cc: [ 'l' ]; s: 29),
{ 26: }
  ( cc: [ 'l' ]; s: 30),
{ 27: }
  ( cc: [ 'u' ]; s: 31),
{ 28: }
  ( cc: [ #1..'!','#'..'[',']'..#255 ]; s: 6),
  ( cc: [ '"' ]; s: 21),
  ( cc: [ '\' ]; s: 20),
{ 29: }
  ( cc: [ 's' ]; s: 32),
{ 30: }
  ( cc: [ 'l' ]; s: 33),
{ 31: }
  ( cc: [ 'e' ]; s: 34),
{ 32: }
  ( cc: [ 'e' ]; s: 35)
{ 33: }
{ 34: }
{ 35: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 2,
{ 4: } 3,
{ 5: } 3,
{ 6: } 3,
{ 7: } 3,
{ 8: } 4,
{ 9: } 5,
{ 10: } 6,
{ 11: } 7,
{ 12: } 8,
{ 13: } 9,
{ 14: } 10,
{ 15: } 11,
{ 16: } 11,
{ 17: } 11,
{ 18: } 11,
{ 19: } 11,
{ 20: } 11,
{ 21: } 11,
{ 22: } 12,
{ 23: } 13,
{ 24: } 13,
{ 25: } 14,
{ 26: } 14,
{ 27: } 14,
{ 28: } 14,
{ 29: } 15,
{ 30: } 15,
{ 31: } 15,
{ 32: } 15,
{ 33: } 15,
{ 34: } 16,
{ 35: } 17
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 1,
{ 3: } 2,
{ 4: } 2,
{ 5: } 2,
{ 6: } 2,
{ 7: } 3,
{ 8: } 4,
{ 9: } 5,
{ 10: } 6,
{ 11: } 7,
{ 12: } 8,
{ 13: } 9,
{ 14: } 10,
{ 15: } 10,
{ 16: } 10,
{ 17: } 10,
{ 18: } 10,
{ 19: } 10,
{ 20: } 10,
{ 21: } 11,
{ 22: } 12,
{ 23: } 12,
{ 24: } 13,
{ 25: } 13,
{ 26: } 13,
{ 27: } 13,
{ 28: } 14,
{ 29: } 14,
{ 30: } 14,
{ 31: } 14,
{ 32: } 14,
{ 33: } 15,
{ 34: } 16,
{ 35: } 17
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 2,
{ 4: } 3,
{ 5: } 3,
{ 6: } 3,
{ 7: } 3,
{ 8: } 4,
{ 9: } 5,
{ 10: } 6,
{ 11: } 7,
{ 12: } 8,
{ 13: } 9,
{ 14: } 10,
{ 15: } 11,
{ 16: } 11,
{ 17: } 11,
{ 18: } 11,
{ 19: } 11,
{ 20: } 11,
{ 21: } 11,
{ 22: } 12,
{ 23: } 13,
{ 24: } 13,
{ 25: } 14,
{ 26: } 14,
{ 27: } 14,
{ 28: } 14,
{ 29: } 15,
{ 30: } 15,
{ 31: } 15,
{ 32: } 15,
{ 33: } 15,
{ 34: } 16,
{ 35: } 17
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 1,
{ 3: } 2,
{ 4: } 2,
{ 5: } 2,
{ 6: } 2,
{ 7: } 3,
{ 8: } 4,
{ 9: } 5,
{ 10: } 6,
{ 11: } 7,
{ 12: } 8,
{ 13: } 9,
{ 14: } 10,
{ 15: } 10,
{ 16: } 10,
{ 17: } 10,
{ 18: } 10,
{ 19: } 10,
{ 20: } 10,
{ 21: } 11,
{ 22: } 12,
{ 23: } 12,
{ 24: } 13,
{ 25: } 13,
{ 26: } 13,
{ 27: } 13,
{ 28: } 14,
{ 29: } 14,
{ 30: } 14,
{ 31: } 14,
{ 32: } 14,
{ 33: } 15,
{ 34: } 16,
{ 35: } 17
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 15,
{ 4: } 29,
{ 5: } 29,
{ 6: } 29,
{ 7: } 32,
{ 8: } 32,
{ 9: } 32,
{ 10: } 32,
{ 11: } 32,
{ 12: } 32,
{ 13: } 32,
{ 14: } 32,
{ 15: } 35,
{ 16: } 36,
{ 17: } 38,
{ 18: } 39,
{ 19: } 40,
{ 20: } 41,
{ 21: } 44,
{ 22: } 44,
{ 23: } 46,
{ 24: } 47,
{ 25: } 48,
{ 26: } 49,
{ 27: } 50,
{ 28: } 51,
{ 29: } 54,
{ 30: } 55,
{ 31: } 56,
{ 32: } 57,
{ 33: } 58,
{ 34: } 58,
{ 35: } 58
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 14,
{ 3: } 28,
{ 4: } 28,
{ 5: } 28,
{ 6: } 31,
{ 7: } 31,
{ 8: } 31,
{ 9: } 31,
{ 10: } 31,
{ 11: } 31,
{ 12: } 31,
{ 13: } 31,
{ 14: } 34,
{ 15: } 35,
{ 16: } 37,
{ 17: } 38,
{ 18: } 39,
{ 19: } 40,
{ 20: } 43,
{ 21: } 43,
{ 22: } 45,
{ 23: } 46,
{ 24: } 47,
{ 25: } 48,
{ 26: } 49,
{ 27: } 50,
{ 28: } 53,
{ 29: } 54,
{ 30: } 55,
{ 31: } 56,
{ 32: } 57,
{ 33: } 57,
{ 34: } 57,
{ 35: } 57
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
