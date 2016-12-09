unit jsnParser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , igGlobals
   , Xmlz
   , Xsdz
   ;


type TjsnParser = class (TCustParser)
private
  ParentXml: TXml;
  OfIdString: String;
  FOnHaveScanned: TOnHaveScannedEvent;
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
const _LEFT_SQUARE_BRACKET = 257;
const _LEFT_CURLY_BRACKET = 258;
const _RIGHT_SQUARE_BRACKET = 259;
const _RIGHT_CURLY_BRACKET = 260;
const _COLON = 261;
const _COMMA = 262;
const _STRING = 263;
const _NUMBER = 264;
const _FALSE = 265;
const _NULL = 266;
const _TRUE = 267;
const _IS = 268;
const _IGNORE = 269;



implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TjsnParser.Prepare;
begin
  inherited Prepare;
end;

destructor TjsnParser.Destroy;
begin
  inherited Destroy;
end;

function TjsnParser.yylex: Integer;
begin
  result := inherited yylex;
end;

const yymaxdepth = 1024;

function TjsnParser.yyparse : Integer;

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
         
         Xml.jsonType := jsonArray;
         if not Assigned (ParentXml) then
         Xml.Name := 'json';
         
       end;
  10 : begin
         yyval := yyv[yysp-3];
       end;
  11 : begin
         
         Xml.jsonType := jsonObject;
         if not Assigned (ParentXml) then
         Xml.Name := 'json';
         
       end;
  12 : begin
         
         
       end;
  13 : begin
         yyval := yyv[yysp-4];
       end;
  14 : begin
         yyval := yyv[yysp-0];
       end;
  15 : begin
       end;
  16 : begin
         yyval := yyv[yysp-0];
       end;
  17 : begin
         yyval := yyv[yysp-0];
       end;
  18 : begin
         yyval := yyv[yysp-2];
       end;
  19 : begin
         
         PushObject (ParentXml);
         PushObject (Xml);
         ParentXml := Xml;
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         
       end;
  20 : begin
         
         Xml := PopObject as TXml;
         ParentXml := PopObject as TXml;
         
       end;
  21 : begin
         
         Xml.Name := Copy (yyv[yysp-1].TokenString, 2, Length (yyv[yysp-1].TokenString) - 2);
         
       end;
  22 : begin
         yyval := yyv[yysp-3];
       end;
  23 : begin
         
         Xml.Value := Copy (yyv[yysp-0].TokenString, 2, Length (yyv[yysp-0].TokenString) - 2);
         Xml.jsonType := jsonString;
         
       end;
  24 : begin
         
         Xml.Value := yyv[yysp-0].TokenString;
         Xml.jsonType := jsonNumber;
         
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         yyval := yyv[yysp-0];
       end;
  27 : begin
         
         Xml.Value := 'true';
         Xml.jsonType := jsonBoolean;
         
       end;
  28 : begin
         
         Xml.Value := 'false';
         Xml.jsonType := jsonBoolean;
         
       end;
  29 : begin
         
         Xml.Checked := False;
         
       end;
  30 : begin
         
         Xml.jsonType := jsonArray;
         
       end;
  31 : begin
         yyval := yyv[yysp-3];
       end;
  32 : begin
       end;
  33 : begin
         yyval := yyv[yysp-0];
       end;
  34 : begin
         yyval := yyv[yysp-0];
       end;
  35 : begin
         yyval := yyv[yysp-2];
       end;
  36 : begin
         
         PushObject (ParentXml);
         ParentXml := Xml;
         PushObject (Xml);
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         Xml.Name := ParentXml.Name + '__Value';
         Xml.jsonType := jsonArrayValue;
         
       end;
  37 : begin
         
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

yynacts   = 49;
yyngotos  = 39;
yynstates = 47;
yynrules  = 37;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: -3 ),
  ( sym: 257; act: -5 ),
  ( sym: 258; act: -5 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
{ 4: }
  ( sym: 257; act: 8 ),
  ( sym: 258; act: 9 ),
{ 5: }
{ 6: }
{ 7: }
  ( sym: 258; act: 9 ),
  ( sym: 0; act: -5 ),
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
  ( sym: 259; act: -32 ),
  ( sym: 257; act: -36 ),
  ( sym: 258; act: -36 ),
  ( sym: 263; act: -36 ),
  ( sym: 264; act: -36 ),
  ( sym: 265; act: -36 ),
  ( sym: 266; act: -36 ),
  ( sym: 267; act: -36 ),
{ 13: }
  ( sym: 260; act: -15 ),
  ( sym: 263; act: -19 ),
{ 14: }
  ( sym: 257; act: 26 ),
  ( sym: 258; act: 9 ),
  ( sym: 263; act: 27 ),
  ( sym: 264; act: 28 ),
  ( sym: 265; act: 29 ),
  ( sym: 266; act: 30 ),
  ( sym: 267; act: 31 ),
{ 15: }
{ 16: }
  ( sym: 262; act: 32 ),
  ( sym: 259; act: -33 ),
{ 17: }
  ( sym: 259; act: 33 ),
{ 18: }
  ( sym: 263; act: 35 ),
{ 19: }
{ 20: }
  ( sym: 262; act: 36 ),
  ( sym: 260; act: -16 ),
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
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 261; act: 40 ),
{ 36: }
{ 37: }
  ( sym: 260; act: 42 ),
{ 38: }
  ( sym: 259; act: -32 ),
  ( sym: 257; act: -36 ),
  ( sym: 258; act: -36 ),
  ( sym: 263; act: -36 ),
  ( sym: 264; act: -36 ),
  ( sym: 265; act: -36 ),
  ( sym: 266; act: -36 ),
  ( sym: 267; act: -36 ),
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  ( sym: 259; act: 45 ),
{ 44: }
  ( sym: 257; act: 26 ),
  ( sym: 258; act: 9 ),
  ( sym: 263; act: 27 ),
  ( sym: 264; act: 28 ),
  ( sym: 265; act: 29 ),
  ( sym: 266; act: 30 ),
  ( sym: 267; act: 31 )
{ 45: }
{ 46: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -4; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -7; act: 3 ),
  ( sym: -5; act: 4 ),
  ( sym: -3; act: 5 ),
{ 2: }
{ 3: }
{ 4: }
  ( sym: -8; act: 6 ),
  ( sym: -6; act: 7 ),
{ 5: }
{ 6: }
{ 7: }
  ( sym: -8; act: 10 ),
  ( sym: -7; act: 3 ),
  ( sym: -5; act: 11 ),
{ 8: }
  ( sym: -10; act: 12 ),
{ 9: }
  ( sym: -12; act: 13 ),
{ 10: }
{ 11: }
{ 12: }
  ( sym: -25; act: 14 ),
  ( sym: -24; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -9; act: 17 ),
{ 13: }
  ( sym: -18; act: 18 ),
  ( sym: -16; act: 19 ),
  ( sym: -15; act: 20 ),
  ( sym: -14; act: 21 ),
  ( sym: -11; act: 22 ),
{ 14: }
  ( sym: -21; act: 23 ),
  ( sym: -19; act: 24 ),
  ( sym: -8; act: 25 ),
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -17; act: 34 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: -13; act: 37 ),
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( sym: -22; act: 38 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
  ( sym: -25; act: 14 ),
  ( sym: -24; act: 39 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -18; act: 18 ),
  ( sym: -16; act: 41 ),
{ 37: }
{ 38: }
  ( sym: -25; act: 14 ),
  ( sym: -24; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -9; act: 43 ),
{ 39: }
{ 40: }
  ( sym: -20; act: 44 ),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
  ( sym: -21; act: 23 ),
  ( sym: -19; act: 46 ),
  ( sym: -8; act: 25 )
{ 45: }
{ 46: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } 0,
{ 3: } -6,
{ 4: } 0,
{ 5: } -2,
{ 6: } -7,
{ 7: } 0,
{ 8: } -9,
{ 9: } -11,
{ 10: } -8,
{ 11: } -4,
{ 12: } 0,
{ 13: } 0,
{ 14: } 0,
{ 15: } -34,
{ 16: } 0,
{ 17: } 0,
{ 18: } 0,
{ 19: } -17,
{ 20: } 0,
{ 21: } -14,
{ 22: } -12,
{ 23: } -26,
{ 24: } -37,
{ 25: } -25,
{ 26: } -30,
{ 27: } -23,
{ 28: } -24,
{ 29: } -28,
{ 30: } -29,
{ 31: } -27,
{ 32: } -36,
{ 33: } -10,
{ 34: } -20,
{ 35: } 0,
{ 36: } -19,
{ 37: } 0,
{ 38: } 0,
{ 39: } -35,
{ 40: } -21,
{ 41: } -18,
{ 42: } -13,
{ 43: } 0,
{ 44: } 0,
{ 45: } -31,
{ 46: } -22
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 4,
{ 3: } 5,
{ 4: } 5,
{ 5: } 7,
{ 6: } 7,
{ 7: } 7,
{ 8: } 9,
{ 9: } 9,
{ 10: } 9,
{ 11: } 9,
{ 12: } 9,
{ 13: } 17,
{ 14: } 19,
{ 15: } 26,
{ 16: } 26,
{ 17: } 28,
{ 18: } 29,
{ 19: } 30,
{ 20: } 30,
{ 21: } 32,
{ 22: } 32,
{ 23: } 32,
{ 24: } 32,
{ 25: } 32,
{ 26: } 32,
{ 27: } 32,
{ 28: } 32,
{ 29: } 32,
{ 30: } 32,
{ 31: } 32,
{ 32: } 32,
{ 33: } 32,
{ 34: } 32,
{ 35: } 32,
{ 36: } 33,
{ 37: } 33,
{ 38: } 34,
{ 39: } 42,
{ 40: } 42,
{ 41: } 42,
{ 42: } 42,
{ 43: } 42,
{ 44: } 43,
{ 45: } 50,
{ 46: } 50
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 3,
{ 2: } 4,
{ 3: } 4,
{ 4: } 6,
{ 5: } 6,
{ 6: } 6,
{ 7: } 8,
{ 8: } 8,
{ 9: } 8,
{ 10: } 8,
{ 11: } 8,
{ 12: } 16,
{ 13: } 18,
{ 14: } 25,
{ 15: } 25,
{ 16: } 27,
{ 17: } 28,
{ 18: } 29,
{ 19: } 29,
{ 20: } 31,
{ 21: } 31,
{ 22: } 31,
{ 23: } 31,
{ 24: } 31,
{ 25: } 31,
{ 26: } 31,
{ 27: } 31,
{ 28: } 31,
{ 29: } 31,
{ 30: } 31,
{ 31: } 31,
{ 32: } 31,
{ 33: } 31,
{ 34: } 31,
{ 35: } 32,
{ 36: } 32,
{ 37: } 33,
{ 38: } 41,
{ 39: } 41,
{ 40: } 41,
{ 41: } 41,
{ 42: } 41,
{ 43: } 42,
{ 44: } 49,
{ 45: } 49,
{ 46: } 49
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 6,
{ 3: } 6,
{ 4: } 6,
{ 5: } 8,
{ 6: } 8,
{ 7: } 8,
{ 8: } 11,
{ 9: } 12,
{ 10: } 13,
{ 11: } 13,
{ 12: } 13,
{ 13: } 17,
{ 14: } 22,
{ 15: } 25,
{ 16: } 25,
{ 17: } 25,
{ 18: } 25,
{ 19: } 26,
{ 20: } 26,
{ 21: } 26,
{ 22: } 26,
{ 23: } 27,
{ 24: } 27,
{ 25: } 27,
{ 26: } 27,
{ 27: } 28,
{ 28: } 28,
{ 29: } 28,
{ 30: } 28,
{ 31: } 28,
{ 32: } 28,
{ 33: } 30,
{ 34: } 30,
{ 35: } 30,
{ 36: } 30,
{ 37: } 32,
{ 38: } 32,
{ 39: } 36,
{ 40: } 36,
{ 41: } 37,
{ 42: } 37,
{ 43: } 37,
{ 44: } 37,
{ 45: } 40,
{ 46: } 40
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 5,
{ 2: } 5,
{ 3: } 5,
{ 4: } 7,
{ 5: } 7,
{ 6: } 7,
{ 7: } 10,
{ 8: } 11,
{ 9: } 12,
{ 10: } 12,
{ 11: } 12,
{ 12: } 16,
{ 13: } 21,
{ 14: } 24,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 25,
{ 19: } 25,
{ 20: } 25,
{ 21: } 25,
{ 22: } 26,
{ 23: } 26,
{ 24: } 26,
{ 25: } 26,
{ 26: } 27,
{ 27: } 27,
{ 28: } 27,
{ 29: } 27,
{ 30: } 27,
{ 31: } 27,
{ 32: } 29,
{ 33: } 29,
{ 34: } 29,
{ 35: } 29,
{ 36: } 31,
{ 37: } 31,
{ 38: } 35,
{ 39: } 35,
{ 40: } 36,
{ 41: } 36,
{ 42: } 36,
{ 43: } 36,
{ 44: } 39,
{ 45: } 39,
{ 46: } 39
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
{ 9: } ( len: 0; sym: -10 ),
{ 10: } ( len: 4; sym: -6 ),
{ 11: } ( len: 0; sym: -12 ),
{ 12: } ( len: 0; sym: -13 ),
{ 13: } ( len: 5; sym: -8 ),
{ 14: } ( len: 1; sym: -11 ),
{ 15: } ( len: 0; sym: -14 ),
{ 16: } ( len: 1; sym: -14 ),
{ 17: } ( len: 1; sym: -15 ),
{ 18: } ( len: 3; sym: -15 ),
{ 19: } ( len: 0; sym: -18 ),
{ 20: } ( len: 2; sym: -16 ),
{ 21: } ( len: 0; sym: -20 ),
{ 22: } ( len: 4; sym: -17 ),
{ 23: } ( len: 1; sym: -19 ),
{ 24: } ( len: 1; sym: -19 ),
{ 25: } ( len: 1; sym: -19 ),
{ 26: } ( len: 1; sym: -19 ),
{ 27: } ( len: 1; sym: -19 ),
{ 28: } ( len: 1; sym: -19 ),
{ 29: } ( len: 1; sym: -19 ),
{ 30: } ( len: 0; sym: -22 ),
{ 31: } ( len: 4; sym: -21 ),
{ 32: } ( len: 0; sym: -9 ),
{ 33: } ( len: 1; sym: -9 ),
{ 34: } ( len: 1; sym: -23 ),
{ 35: } ( len: 3; sym: -23 ),
{ 36: } ( len: 0; sym: -25 ),
{ 37: } ( len: 2; sym: -24 )
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
