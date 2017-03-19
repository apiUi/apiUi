unit yamlParser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , igGlobals
   , Xmlz
   , Xsdz
   ;


type TyamlParser = class (TCustParser)
private
  ParentXml: TXml;
  OfIdString: String;
  function StringDecode(aString: String): String;
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
const _NAME = 258;
const _NEWLINE = 259;
const _WHITESPACE = 260;
const _INDENT = 261;
const _HYPHENINDENT = 262;
const _VALUE = 263;
const _PIPE = 264;
const _LEFT_SQUARE_BRACKET = 265;
const _LEFT_CURLY_BRACKET = 266;
const _RIGHT_SQUARE_BRACKET = 267;
const _RIGHT_CURLY_BRACKET = 268;
const _COLON = 269;
const _COMMA = 270;
const _STRING = 271;
const _NUMBER = 272;
const _FALSE = 273;
const _NULL = 274;
const _TRUE = 275;
const _IS = 276;
const _IGNORE = 277;
const _NOTUSEDLASTONE = 278;



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
         
         ParentXml := nil;
         Xml.Checked := True;
         Xml.jsonType := jsonObject;
         Xml.Name := '';
         
       end;
   2 : begin
         
         
       end;
   3 : begin
       end;
   4 : begin
         yyval := yyv[yysp-2];
       end;
   5 : begin
       end;
   6 : begin
         
         if Trim (yyv[yysp-0].TokenString) <> '' then
         raise Exception.Create ( '%Illegal: ['
         + yyv[yysp-0].TokenString
         + ']['
         + IntToStr (yyv[yysp-0].LineNumber)
         + ':'
         + IntToStr (yyv[yysp-0].ColumnNumber)
         + ']'
         );
         
       end;
   7 : begin
         yyval := yyv[yysp-0];
       end;
   8 : begin
         yyval := yyv[yysp-1];
       end;
   9 : begin
         yyval := yyv[yysp-0];
       end;
  10 : begin
         
         Xml.jsonType := jsonObject;
         if not Assigned (ParentXml) then
         Xml.Name := 'json';
         
       end;
  11 : begin
         
         
       end;
  12 : begin
         yyval := yyv[yysp-4];
       end;
  13 : begin
         yyval := yyv[yysp-0];
       end;
  14 : begin
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
         yyval := yyv[yysp-0];
       end;
  17 : begin
         yyval := yyv[yysp-2];
       end;
  18 : begin
         
         PushObject (ParentXml);
         PushObject (Xml);
         ParentXml := Xml;
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         
       end;
  19 : begin
         
         Xml := PopObject as TXml;
         ParentXml := PopObject as TXml;
         
       end;
  20 : begin
         
         Xml.Name := Copy (yyv[yysp-1].TokenString, 2, Length (yyv[yysp-1].TokenString) - 2);
         
       end;
  21 : begin
         yyval := yyv[yysp-3];
       end;
  22 : begin
         
         Xml.Value := StringDecode(Copy (yyv[yysp-0].TokenString, 2, Length (yyv[yysp-0].TokenString) - 2));
         Xml.jsonType := jsonString;
         
       end;
  23 : begin
         
         Xml.Value := yyv[yysp-0].TokenString;
         Xml.jsonType := jsonNumber;
         
       end;
  24 : begin
         yyval := yyv[yysp-0];
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         
         Xml.Value := 'true';
         Xml.jsonType := jsonBoolean;
         
       end;
  27 : begin
         
         Xml.Value := 'false';
         Xml.jsonType := jsonBoolean;
         
       end;
  28 : begin
         
         Xml.Checked := False;
         
       end;
  29 : begin
         
         Xml.jsonType := jsonArray;
         if not Assigned (ParentXml) then
         Xml.Name := 'json';
         
       end;
  30 : begin
         yyval := yyv[yysp-3];
       end;
  31 : begin
       end;
  32 : begin
         yyval := yyv[yysp-0];
       end;
  33 : begin
         yyval := yyv[yysp-0];
       end;
  34 : begin
         yyval := yyv[yysp-2];
       end;
  35 : begin
         
         PushObject (ParentXml);
         ParentXml := Xml;
         PushObject (Xml);
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         Xml.Name := ParentXml.Name + '__Value';
         Xml.Name := '_';
         Xml.jsonType := jsonArrayValue;
         
       end;
  36 : begin
         
         Xml := PopObject as TXml;
         ParentXml := PopObject as TXml;
         
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

yynacts   = 42;
yyngotos  = 33;
yynstates = 44;
yynrules  = 36;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 263; act: 5 ),
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -5 ),
  ( sym: 266; act: -5 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
  ( sym: 265; act: 9 ),
  ( sym: 266; act: 10 ),
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
  ( sym: 263; act: 5 ),
  ( sym: 266; act: 10 ),
  ( sym: 0; act: -5 ),
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
  ( sym: 267; act: -31 ),
  ( sym: 265; act: -35 ),
  ( sym: 266; act: -35 ),
  ( sym: 271; act: -35 ),
  ( sym: 272; act: -35 ),
  ( sym: 273; act: -35 ),
  ( sym: 274; act: -35 ),
  ( sym: 275; act: -35 ),
{ 14: }
  ( sym: 268; act: -14 ),
  ( sym: 271; act: -18 ),
{ 15: }
  ( sym: 265; act: 9 ),
  ( sym: 266; act: 10 ),
  ( sym: 271; act: 27 ),
  ( sym: 272; act: 28 ),
  ( sym: 273; act: 29 ),
  ( sym: 274; act: 30 ),
  ( sym: 275; act: 31 ),
{ 16: }
{ 17: }
  ( sym: 270; act: 32 ),
  ( sym: 267; act: -32 ),
{ 18: }
  ( sym: 267; act: 33 ),
{ 19: }
  ( sym: 271; act: 35 ),
{ 20: }
{ 21: }
  ( sym: 270; act: 36 ),
  ( sym: 268; act: -15 ),
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 269; act: 39 ),
{ 36: }
{ 37: }
  ( sym: 268; act: 41 ),
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: 265; act: 9 ),
  ( sym: 266; act: 10 ),
  ( sym: 271; act: 27 ),
  ( sym: 272; act: 28 ),
  ( sym: 273; act: 29 ),
  ( sym: 274; act: 30 ),
  ( sym: 275; act: 31 )
{ 43: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -4; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -5; act: 3 ),
  ( sym: -3; act: 4 ),
{ 2: }
{ 3: }
  ( sym: -8; act: 6 ),
  ( sym: -7; act: 7 ),
  ( sym: -6; act: 8 ),
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
  ( sym: -7; act: 11 ),
  ( sym: -5; act: 12 ),
{ 9: }
  ( sym: -20; act: 13 ),
{ 10: }
  ( sym: -10; act: 14 ),
{ 11: }
{ 12: }
{ 13: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 16 ),
  ( sym: -21; act: 17 ),
  ( sym: -19; act: 18 ),
{ 14: }
  ( sym: -16; act: 19 ),
  ( sym: -14; act: 20 ),
  ( sym: -13; act: 21 ),
  ( sym: -12; act: 22 ),
  ( sym: -9; act: 23 ),
{ 15: }
  ( sym: -17; act: 24 ),
  ( sym: -8; act: 25 ),
  ( sym: -7; act: 26 ),
{ 16: }
{ 17: }
{ 18: }
{ 19: }
  ( sym: -15; act: 34 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
  ( sym: -11; act: 37 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 38 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -16; act: 19 ),
  ( sym: -14; act: 40 ),
{ 37: }
{ 38: }
{ 39: }
  ( sym: -18; act: 42 ),
{ 40: }
{ 41: }
{ 42: }
  ( sym: -17; act: 43 ),
  ( sym: -8; act: 25 ),
  ( sym: -7; act: 26 )
{ 43: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } -2,
{ 5: } -6,
{ 6: } -9,
{ 7: } -7,
{ 8: } 0,
{ 9: } -29,
{ 10: } -10,
{ 11: } -8,
{ 12: } -4,
{ 13: } 0,
{ 14: } 0,
{ 15: } 0,
{ 16: } -33,
{ 17: } 0,
{ 18: } 0,
{ 19: } 0,
{ 20: } -16,
{ 21: } 0,
{ 22: } -13,
{ 23: } -11,
{ 24: } -36,
{ 25: } -25,
{ 26: } -24,
{ 27: } -22,
{ 28: } -23,
{ 29: } -27,
{ 30: } -28,
{ 31: } -26,
{ 32: } -35,
{ 33: } -30,
{ 34: } -19,
{ 35: } 0,
{ 36: } -18,
{ 37: } 0,
{ 38: } -34,
{ 39: } -20,
{ 40: } -17,
{ 41: } -12,
{ 42: } 0,
{ 43: } -21
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 5,
{ 3: } 6,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 8,
{ 8: } 8,
{ 9: } 11,
{ 10: } 11,
{ 11: } 11,
{ 12: } 11,
{ 13: } 11,
{ 14: } 19,
{ 15: } 21,
{ 16: } 28,
{ 17: } 28,
{ 18: } 30,
{ 19: } 31,
{ 20: } 32,
{ 21: } 32,
{ 22: } 34,
{ 23: } 34,
{ 24: } 34,
{ 25: } 34,
{ 26: } 34,
{ 27: } 34,
{ 28: } 34,
{ 29: } 34,
{ 30: } 34,
{ 31: } 34,
{ 32: } 34,
{ 33: } 34,
{ 34: } 34,
{ 35: } 34,
{ 36: } 35,
{ 37: } 35,
{ 38: } 36,
{ 39: } 36,
{ 40: } 36,
{ 41: } 36,
{ 42: } 36,
{ 43: } 43
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 4,
{ 2: } 5,
{ 3: } 7,
{ 4: } 7,
{ 5: } 7,
{ 6: } 7,
{ 7: } 7,
{ 8: } 10,
{ 9: } 10,
{ 10: } 10,
{ 11: } 10,
{ 12: } 10,
{ 13: } 18,
{ 14: } 20,
{ 15: } 27,
{ 16: } 27,
{ 17: } 29,
{ 18: } 30,
{ 19: } 31,
{ 20: } 31,
{ 21: } 33,
{ 22: } 33,
{ 23: } 33,
{ 24: } 33,
{ 25: } 33,
{ 26: } 33,
{ 27: } 33,
{ 28: } 33,
{ 29: } 33,
{ 30: } 33,
{ 31: } 33,
{ 32: } 33,
{ 33: } 33,
{ 34: } 33,
{ 35: } 34,
{ 36: } 34,
{ 37: } 35,
{ 38: } 35,
{ 39: } 35,
{ 40: } 35,
{ 41: } 35,
{ 42: } 42,
{ 43: } 42
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 5,
{ 3: } 5,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 8,
{ 8: } 8,
{ 9: } 10,
{ 10: } 11,
{ 11: } 12,
{ 12: } 12,
{ 13: } 12,
{ 14: } 16,
{ 15: } 21,
{ 16: } 24,
{ 17: } 24,
{ 18: } 24,
{ 19: } 24,
{ 20: } 25,
{ 21: } 25,
{ 22: } 25,
{ 23: } 25,
{ 24: } 26,
{ 25: } 26,
{ 26: } 26,
{ 27: } 26,
{ 28: } 26,
{ 29: } 26,
{ 30: } 26,
{ 31: } 26,
{ 32: } 26,
{ 33: } 28,
{ 34: } 28,
{ 35: } 28,
{ 36: } 28,
{ 37: } 30,
{ 38: } 30,
{ 39: } 30,
{ 40: } 31,
{ 41: } 31,
{ 42: } 31,
{ 43: } 34
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 4,
{ 2: } 4,
{ 3: } 7,
{ 4: } 7,
{ 5: } 7,
{ 6: } 7,
{ 7: } 7,
{ 8: } 9,
{ 9: } 10,
{ 10: } 11,
{ 11: } 11,
{ 12: } 11,
{ 13: } 15,
{ 14: } 20,
{ 15: } 23,
{ 16: } 23,
{ 17: } 23,
{ 18: } 23,
{ 19: } 24,
{ 20: } 24,
{ 21: } 24,
{ 22: } 24,
{ 23: } 25,
{ 24: } 25,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 25,
{ 29: } 25,
{ 30: } 25,
{ 31: } 25,
{ 32: } 27,
{ 33: } 27,
{ 34: } 27,
{ 35: } 27,
{ 36: } 29,
{ 37: } 29,
{ 38: } 29,
{ 39: } 30,
{ 40: } 30,
{ 41: } 30,
{ 42: } 33,
{ 43: } 33
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -4 ),
{ 2: } ( len: 2; sym: -2 ),
{ 3: } ( len: 0; sym: -3 ),
{ 4: } ( len: 3; sym: -3 ),
{ 5: } ( len: 0; sym: -5 ),
{ 6: } ( len: 1; sym: -5 ),
{ 7: } ( len: 1; sym: -6 ),
{ 8: } ( len: 2; sym: -6 ),
{ 9: } ( len: 1; sym: -6 ),
{ 10: } ( len: 0; sym: -10 ),
{ 11: } ( len: 0; sym: -11 ),
{ 12: } ( len: 5; sym: -7 ),
{ 13: } ( len: 1; sym: -9 ),
{ 14: } ( len: 0; sym: -12 ),
{ 15: } ( len: 1; sym: -12 ),
{ 16: } ( len: 1; sym: -13 ),
{ 17: } ( len: 3; sym: -13 ),
{ 18: } ( len: 0; sym: -16 ),
{ 19: } ( len: 2; sym: -14 ),
{ 20: } ( len: 0; sym: -18 ),
{ 21: } ( len: 4; sym: -15 ),
{ 22: } ( len: 1; sym: -17 ),
{ 23: } ( len: 1; sym: -17 ),
{ 24: } ( len: 1; sym: -17 ),
{ 25: } ( len: 1; sym: -17 ),
{ 26: } ( len: 1; sym: -17 ),
{ 27: } ( len: 1; sym: -17 ),
{ 28: } ( len: 1; sym: -17 ),
{ 29: } ( len: 0; sym: -20 ),
{ 30: } ( len: 4; sym: -8 ),
{ 31: } ( len: 0; sym: -19 ),
{ 32: } ( len: 1; sym: -19 ),
{ 33: } ( len: 1; sym: -21 ),
{ 34: } ( len: 3; sym: -21 ),
{ 35: } ( len: 0; sym: -23 ),
{ 36: } ( len: 2; sym: -22 )
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
