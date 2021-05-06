unit yamlParser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , igGlobals
   , Xmlz
   , Xsdz
   , Bind
   ;


type TyamlParser = class (TCustParser)
private
  CurrentXml, PreviousXml: TXml;
  OfIdString: String;
  function StringDecode(aString: String): String;
  procedure AddToParent (aXml, aCurrentXml: TXml);
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  Xml: TXml;
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  destructor Destroy; override;
end;
const _COMMENT = 257;
const _LINECONT = 258;
const _NAME = 259;
const _NEWLINE = 260;
const _WHITESPACE = 261;
const _INDENT = 262;
const _HYPHENINDENT = 263;
const _VALUE = 264;
const _ARRAYVALUE = 265;
const _PIPE = 266;
const _LEFT_SQUARE_BRACKET = 267;
const _LEFT_CURLY_BRACKET = 268;
const _RIGHT_SQUARE_BRACKET = 269;
const _RIGHT_CURLY_BRACKET = 270;
const _COLON = 271;
const _COMMA = 272;
const _STRING = 273;
const _NUMBER = 274;
const _FALSE = 275;
const _NULL = 276;
const _TRUE = 277;
const _IS = 278;
const _IGNORE = 279;
const _REPLCRANDSTRIP = 280;
const _REPLCRANDKEEP = 281;
const _REPLCRANDCLIP = 282;
const _KEEPCRANDSTRIP = 283;
const _KEEPCRANDKEEP = 284;
const _KEEPCRANDCLIP = 285;
const _NOTUSEDLASTONE = 286;



implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TyamlParser.Prepare;
begin
  inherited Prepare;
end;

destructor TyamlParser.Destroy;
begin
  inherited Destroy;
end;

function TyamlParser.yylex: Integer;
begin
  result := inherited yylex;
end;

function TyamlParser.StringDecode(aString: String): String;
  function hexVal (aChar: Char): Word;
  begin
    result := StrToInt ('$0' + aChar);
  end;

var
  p, x, n: Integer;
  s: String;
  w: Word;
  b: Char;
begin
  p := Pos ('\', aString);
  if p < 1 then
  begin
    result := aString;
    Exit;
  end;
  x := p + 1;
  n := p + 2;
  case aString [x] of
    'b': s := #8;
    't': s := #9;
    'n': s := #10;
    'f': s := #12;
    'r': s := #13;
    'u':
      begin
        w := (hexVal(aString[n    ]) shl 12)
           + (hexVal(aString[n + 1]) shl  8)
           + (hexVal(aString[n + 2]) shl  4)
           + (hexVal(aString[n + 3])       )
           ;
        b := Char(w);
        s := b;
        n := n + 4;
      end;
    else
      s := aString [x];
  end;
  result := Copy (aString, 1, p - 1)
          + s
          + StringDecode(Copy (aString, n, Length(aString)))
          ;
end;

procedure TyamlParser.AddToParent (aXml, aCurrentXml: TXml);
var
  pLevel, cLevel: Integer;
begin
  cLevel := aCurrentXml.Tag;
  pLevel := aXml.Tag;
  while cLevel <= pLevel do
  begin
    aXml := aXml.Parent as TXml;
    pLevel := aXml.Tag;
  end;
  aXml.AddXml(aCurrentXml);
end;

const yymaxdepth = 1024;

function TyamlParser.yyparse : Integer;

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
         
         PreviousXml := nil;
         Xml.Checked := True;
         Xml.jsonType := jsonObject;
         Xml.Name := 'yaml';
         PushInteger (Xml.Tag);
         Xml.Tag := -1;
         
       end;
   2 : begin
         
         Xml.Tag := PopInteger;
         
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
       end;
   9 : begin
         yyval := yyv[yysp-0];
       end;
  10 : begin
         yyval := yyv[yysp-0];
       end;
  11 : begin
         yyval := yyv[yysp-1];
       end;
  12 : begin
         
         CurrentXml := TXml.Create;
         CurrentXml.Checked := True;
         CurrentXml.Tag := 0;
         CurrentXml.jsonType := jsonObject;
         
       end;
  13 : begin
         
         if not Assigned (PreviousXml) then
         Xml.AddXml (CurrentXml)
         else
         AddToParent (PreviousXml, CurrentXml);
         PreviousXml := CurrentXml;
         
       end;
  14 : begin
         
         CurrentXml.Name := yyv[yysp-1].yyString;
         CurrentXml.Tag := yyv[yysp-1].yy.yyInteger;
         
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
         yyval := yyv[yysp-0];
       end;
  17 : begin
         
         CurrentXml.Name := '_';
         CurrentXml.jsonType := jsonArrayValue;
         CurrentXml.Tag := yyv[yysp-0].yy.yyInteger;
         
       end;
  18 : begin
         
         CurrentXml.Value := yyv[yysp-0].yyString;
         CurrentXml.jsonType := jsonString;
         CurrentXml.Tag := yyv[yysp-0].yy.yyInteger;
         
       end;
  19 : begin
       end;
  20 : begin
         yyval := yyv[yysp-0];
       end;
  21 : begin
         
         CurrentXml.Value := yyv[yysp-0].yyString;
         CurrentXml.jsonType := jsonString;
         CurrentXml.Tag := yyv[yysp-0].yy.yyInteger;
         
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

yynacts   = 27;
yyngotos  = 17;
yynstates = 22;
yynrules  = 21;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 257; act: 6 ),
  ( sym: 0; act: -3 ),
  ( sym: 259; act: -3 ),
  ( sym: 263; act: -3 ),
  ( sym: 265; act: -3 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
{ 4: }
  ( sym: 257; act: 6 ),
  ( sym: 0; act: -4 ),
  ( sym: 259; act: -4 ),
  ( sym: 263; act: -4 ),
  ( sym: 265; act: -4 ),
{ 5: }
  ( sym: 0; act: -8 ),
  ( sym: 259; act: -12 ),
  ( sym: 263; act: -12 ),
  ( sym: 265; act: -12 ),
{ 6: }
{ 7: }
{ 8: }
  ( sym: 259; act: 15 ),
  ( sym: 263; act: 16 ),
  ( sym: 265; act: 17 ),
{ 9: }
{ 10: }
  ( sym: 0; act: -9 ),
  ( sym: 259; act: -12 ),
  ( sym: 263; act: -12 ),
  ( sym: 265; act: -12 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: 264; act: 21 ),
  ( sym: 0; act: -19 ),
  ( sym: 259; act: -19 ),
  ( sym: 263; act: -19 ),
  ( sym: 265; act: -19 )
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -4; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -7; act: 3 ),
  ( sym: -6; act: 4 ),
  ( sym: -3; act: 5 ),
{ 2: }
{ 3: }
{ 4: }
  ( sym: -7; act: 7 ),
{ 5: }
  ( sym: -11; act: 8 ),
  ( sym: -9; act: 9 ),
  ( sym: -8; act: 10 ),
  ( sym: -5; act: 11 ),
{ 6: }
{ 7: }
{ 8: }
  ( sym: -14; act: 12 ),
  ( sym: -13; act: 13 ),
  ( sym: -10; act: 14 ),
{ 9: }
{ 10: }
  ( sym: -11; act: 8 ),
  ( sym: -9; act: 18 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: -15; act: 19 ),
  ( sym: -12; act: 20 )
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } 0,
{ 3: } -5,
{ 4: } 0,
{ 5: } 0,
{ 6: } -7,
{ 7: } -6,
{ 8: } 0,
{ 9: } -10,
{ 10: } 0,
{ 11: } -2,
{ 12: } -16,
{ 13: } -15,
{ 14: } -13,
{ 15: } 0,
{ 16: } -17,
{ 17: } -18,
{ 18: } -11,
{ 19: } -20,
{ 20: } -14,
{ 21: } -21
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 6,
{ 3: } 7,
{ 4: } 7,
{ 5: } 12,
{ 6: } 16,
{ 7: } 16,
{ 8: } 16,
{ 9: } 19,
{ 10: } 19,
{ 11: } 23,
{ 12: } 23,
{ 13: } 23,
{ 14: } 23,
{ 15: } 23,
{ 16: } 28,
{ 17: } 28,
{ 18: } 28,
{ 19: } 28,
{ 20: } 28,
{ 21: } 28
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 5,
{ 2: } 6,
{ 3: } 6,
{ 4: } 11,
{ 5: } 15,
{ 6: } 15,
{ 7: } 15,
{ 8: } 18,
{ 9: } 18,
{ 10: } 22,
{ 11: } 22,
{ 12: } 22,
{ 13: } 22,
{ 14: } 22,
{ 15: } 27,
{ 16: } 27,
{ 17: } 27,
{ 18: } 27,
{ 19: } 27,
{ 20: } 27,
{ 21: } 27
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 6,
{ 3: } 6,
{ 4: } 6,
{ 5: } 7,
{ 6: } 11,
{ 7: } 11,
{ 8: } 11,
{ 9: } 14,
{ 10: } 14,
{ 11: } 16,
{ 12: } 16,
{ 13: } 16,
{ 14: } 16,
{ 15: } 16,
{ 16: } 18,
{ 17: } 18,
{ 18: } 18,
{ 19: } 18,
{ 20: } 18,
{ 21: } 18
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 5,
{ 2: } 5,
{ 3: } 5,
{ 4: } 6,
{ 5: } 10,
{ 6: } 10,
{ 7: } 10,
{ 8: } 13,
{ 9: } 13,
{ 10: } 15,
{ 11: } 15,
{ 12: } 15,
{ 13: } 15,
{ 14: } 15,
{ 15: } 17,
{ 16: } 17,
{ 17: } 17,
{ 18: } 17,
{ 19: } 17,
{ 20: } 17,
{ 21: } 17
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -4 ),
{ 2: } ( len: 3; sym: -2 ),
{ 3: } ( len: 0; sym: -3 ),
{ 4: } ( len: 1; sym: -3 ),
{ 5: } ( len: 1; sym: -6 ),
{ 6: } ( len: 2; sym: -6 ),
{ 7: } ( len: 1; sym: -7 ),
{ 8: } ( len: 0; sym: -5 ),
{ 9: } ( len: 1; sym: -5 ),
{ 10: } ( len: 1; sym: -8 ),
{ 11: } ( len: 2; sym: -8 ),
{ 12: } ( len: 0; sym: -11 ),
{ 13: } ( len: 2; sym: -9 ),
{ 14: } ( len: 2; sym: -10 ),
{ 15: } ( len: 1; sym: -10 ),
{ 16: } ( len: 1; sym: -10 ),
{ 17: } ( len: 1; sym: -13 ),
{ 18: } ( len: 1; sym: -14 ),
{ 19: } ( len: 0; sym: -12 ),
{ 20: } ( len: 1; sym: -12 ),
{ 21: } ( len: 1; sym: -15 )
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
