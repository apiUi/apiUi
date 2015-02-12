unit jsonParser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , igGlobals
   , Xmlz
   , Xsdz
   ;


type TjsonParser = class (TCustParser)
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

procedure TjsonParser.Prepare;
begin
  inherited Prepare;
end;

destructor TjsonParser.Destroy;
begin
  inherited Destroy;
end;

function TjsonParser.yylex: Integer;
begin
  result := inherited yylex;
end;

const yymaxdepth = 1024;

function TjsonParser.yyparse : Integer;

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
         
         Xml.jsonType := jsonObject;
         if not Assigned (ParentXml) then
         Xml.Name := 'json';
         
       end;
  10 : begin
         
         
       end;
  11 : begin
         yyval := yyv[yysp-4];
       end;
  12 : begin
         yyval := yyv[yysp-0];
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
         yyval := yyv[yysp-2];
       end;
  17 : begin
         
         PushObject (ParentXml);
         PushObject (Xml);
         ParentXml := Xml;
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         
       end;
  18 : begin
         
         Xml := PopObject as TXml;
         ParentXml := PopObject as TXml;
         
       end;
  19 : begin
         
         Xml.Name := Copy (yyv[yysp-1].TokenString, 2, Length (yyv[yysp-1].TokenString) - 2);
         
       end;
  20 : begin
         yyval := yyv[yysp-3];
       end;
  21 : begin
         
         Xml.Value := Copy (yyv[yysp-0].TokenString, 2, Length (yyv[yysp-0].TokenString) - 2);
         Xml.jsonType := jsonString;
         
       end;
  22 : begin
         
         Xml.Value := yyv[yysp-0].TokenString;
         Xml.jsonType := jsonNumber;
         
       end;
  23 : begin
         yyval := yyv[yysp-0];
       end;
  24 : begin
         yyval := yyv[yysp-0];
       end;
  25 : begin
         
         Xml.Value := 'true';
         Xml.jsonType := jsonBoolean;
         
       end;
  26 : begin
         
         Xml.Value := 'false';
         Xml.jsonType := jsonBoolean;
         
       end;
  27 : begin
         
         Xml.Checked := False;
         
       end;
  28 : begin
         
         Xml.jsonType := jsonArray;
         
       end;
  29 : begin
         yyval := yyv[yysp-3];
       end;
  30 : begin
       end;
  31 : begin
         yyval := yyv[yysp-0];
       end;
  32 : begin
         yyval := yyv[yysp-0];
       end;
  33 : begin
         yyval := yyv[yysp-2];
       end;
  34 : begin
         
         PushObject (ParentXml);
         ParentXml := Xml;
         PushObject (Xml);
         Xml := ParentXml.AddXml (TXml.Create);
         Xml.Checked := True;
         Xml.Name := ParentXml.Name + '__Value';
         Xml.jsonType := jsonArrayValue;
         
       end;
  35 : begin
         
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

yynacts   = 38;
yyngotos  = 34;
yynstates = 43;
yynrules  = 35;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: -3 ),
  ( sym: 258; act: -5 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
{ 4: }
  ( sym: 258; act: 8 ),
{ 5: }
{ 6: }
{ 7: }
  ( sym: 258; act: 8 ),
  ( sym: 0; act: -5 ),
{ 8: }
{ 9: }
{ 10: }
{ 11: }
  ( sym: 260; act: -13 ),
  ( sym: 263; act: -17 ),
{ 12: }
  ( sym: 263; act: 18 ),
{ 13: }
{ 14: }
  ( sym: 262; act: 19 ),
  ( sym: 260; act: -14 ),
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: 261; act: 21 ),
{ 19: }
{ 20: }
  ( sym: 260; act: 23 ),
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( sym: 257; act: 28 ),
  ( sym: 258; act: 8 ),
  ( sym: 263; act: 29 ),
  ( sym: 264; act: 30 ),
  ( sym: 265; act: 31 ),
  ( sym: 266; act: 32 ),
  ( sym: 267; act: 33 ),
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
  ( sym: 259; act: -30 ),
  ( sym: 257; act: -34 ),
  ( sym: 258; act: -34 ),
  ( sym: 263; act: -34 ),
  ( sym: 264; act: -34 ),
  ( sym: 265; act: -34 ),
  ( sym: 266; act: -34 ),
  ( sym: 267; act: -34 ),
{ 35: }
  ( sym: 257; act: 28 ),
  ( sym: 258; act: 8 ),
  ( sym: 263; act: 29 ),
  ( sym: 264; act: 30 ),
  ( sym: 265; act: 31 ),
  ( sym: 266; act: 32 ),
  ( sym: 267; act: 33 ),
{ 36: }
{ 37: }
  ( sym: 262; act: 40 ),
  ( sym: 259; act: -31 ),
{ 38: }
  ( sym: 259; act: 41 )
{ 39: }
{ 40: }
{ 41: }
{ 42: }
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
  ( sym: -8; act: 9 ),
  ( sym: -7; act: 3 ),
  ( sym: -5; act: 10 ),
{ 8: }
  ( sym: -10; act: 11 ),
{ 9: }
{ 10: }
{ 11: }
  ( sym: -16; act: 12 ),
  ( sym: -14; act: 13 ),
  ( sym: -13; act: 14 ),
  ( sym: -12; act: 15 ),
  ( sym: -9; act: 16 ),
{ 12: }
  ( sym: -15; act: 17 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
  ( sym: -11; act: 20 ),
{ 17: }
{ 18: }
{ 19: }
  ( sym: -16; act: 12 ),
  ( sym: -14; act: 22 ),
{ 20: }
{ 21: }
  ( sym: -18; act: 24 ),
{ 22: }
{ 23: }
{ 24: }
  ( sym: -19; act: 25 ),
  ( sym: -17; act: 26 ),
  ( sym: -8; act: 27 ),
{ 25: }
{ 26: }
{ 27: }
{ 28: }
  ( sym: -21; act: 34 ),
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
  ( sym: -24; act: 35 ),
  ( sym: -23; act: 36 ),
  ( sym: -22; act: 37 ),
  ( sym: -20; act: 38 ),
{ 35: }
  ( sym: -19; act: 25 ),
  ( sym: -17; act: 39 ),
  ( sym: -8; act: 27 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
  ( sym: -24; act: 35 ),
  ( sym: -23; act: 42 )
{ 41: }
{ 42: }
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
{ 9: } -8,
{ 10: } -4,
{ 11: } 0,
{ 12: } 0,
{ 13: } -15,
{ 14: } 0,
{ 15: } -12,
{ 16: } -10,
{ 17: } -18,
{ 18: } 0,
{ 19: } -17,
{ 20: } 0,
{ 21: } -19,
{ 22: } -16,
{ 23: } -11,
{ 24: } 0,
{ 25: } -24,
{ 26: } -20,
{ 27: } -23,
{ 28: } -28,
{ 29: } -21,
{ 30: } -22,
{ 31: } -26,
{ 32: } -27,
{ 33: } -25,
{ 34: } 0,
{ 35: } 0,
{ 36: } -32,
{ 37: } 0,
{ 38: } 0,
{ 39: } -35,
{ 40: } -34,
{ 41: } -29,
{ 42: } -33
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 3,
{ 3: } 4,
{ 4: } 4,
{ 5: } 5,
{ 6: } 5,
{ 7: } 5,
{ 8: } 7,
{ 9: } 7,
{ 10: } 7,
{ 11: } 7,
{ 12: } 9,
{ 13: } 10,
{ 14: } 10,
{ 15: } 12,
{ 16: } 12,
{ 17: } 12,
{ 18: } 12,
{ 19: } 13,
{ 20: } 13,
{ 21: } 14,
{ 22: } 14,
{ 23: } 14,
{ 24: } 14,
{ 25: } 21,
{ 26: } 21,
{ 27: } 21,
{ 28: } 21,
{ 29: } 21,
{ 30: } 21,
{ 31: } 21,
{ 32: } 21,
{ 33: } 21,
{ 34: } 21,
{ 35: } 29,
{ 36: } 36,
{ 37: } 36,
{ 38: } 38,
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 4,
{ 5: } 4,
{ 6: } 4,
{ 7: } 6,
{ 8: } 6,
{ 9: } 6,
{ 10: } 6,
{ 11: } 8,
{ 12: } 9,
{ 13: } 9,
{ 14: } 11,
{ 15: } 11,
{ 16: } 11,
{ 17: } 11,
{ 18: } 12,
{ 19: } 12,
{ 20: } 13,
{ 21: } 13,
{ 22: } 13,
{ 23: } 13,
{ 24: } 20,
{ 25: } 20,
{ 26: } 20,
{ 27: } 20,
{ 28: } 20,
{ 29: } 20,
{ 30: } 20,
{ 31: } 20,
{ 32: } 20,
{ 33: } 20,
{ 34: } 28,
{ 35: } 35,
{ 36: } 35,
{ 37: } 37,
{ 38: } 38,
{ 39: } 38,
{ 40: } 38,
{ 41: } 38,
{ 42: } 38
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
{ 10: } 12,
{ 11: } 12,
{ 12: } 17,
{ 13: } 18,
{ 14: } 18,
{ 15: } 18,
{ 16: } 18,
{ 17: } 19,
{ 18: } 19,
{ 19: } 19,
{ 20: } 21,
{ 21: } 21,
{ 22: } 22,
{ 23: } 22,
{ 24: } 22,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 25,
{ 29: } 26,
{ 30: } 26,
{ 31: } 26,
{ 32: } 26,
{ 33: } 26,
{ 34: } 26,
{ 35: } 30,
{ 36: } 33,
{ 37: } 33,
{ 38: } 33,
{ 39: } 33,
{ 40: } 33,
{ 41: } 35,
{ 42: } 35
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
{ 9: } 11,
{ 10: } 11,
{ 11: } 16,
{ 12: } 17,
{ 13: } 17,
{ 14: } 17,
{ 15: } 17,
{ 16: } 18,
{ 17: } 18,
{ 18: } 18,
{ 19: } 20,
{ 20: } 20,
{ 21: } 21,
{ 22: } 21,
{ 23: } 21,
{ 24: } 24,
{ 25: } 24,
{ 26: } 24,
{ 27: } 24,
{ 28: } 25,
{ 29: } 25,
{ 30: } 25,
{ 31: } 25,
{ 32: } 25,
{ 33: } 25,
{ 34: } 29,
{ 35: } 32,
{ 36: } 32,
{ 37: } 32,
{ 38: } 32,
{ 39: } 32,
{ 40: } 34,
{ 41: } 34,
{ 42: } 34
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
{ 10: } ( len: 0; sym: -11 ),
{ 11: } ( len: 5; sym: -8 ),
{ 12: } ( len: 1; sym: -9 ),
{ 13: } ( len: 0; sym: -12 ),
{ 14: } ( len: 1; sym: -12 ),
{ 15: } ( len: 1; sym: -13 ),
{ 16: } ( len: 3; sym: -13 ),
{ 17: } ( len: 0; sym: -16 ),
{ 18: } ( len: 2; sym: -14 ),
{ 19: } ( len: 0; sym: -18 ),
{ 20: } ( len: 4; sym: -15 ),
{ 21: } ( len: 1; sym: -17 ),
{ 22: } ( len: 1; sym: -17 ),
{ 23: } ( len: 1; sym: -17 ),
{ 24: } ( len: 1; sym: -17 ),
{ 25: } ( len: 1; sym: -17 ),
{ 26: } ( len: 1; sym: -17 ),
{ 27: } ( len: 1; sym: -17 ),
{ 28: } ( len: 0; sym: -21 ),
{ 29: } ( len: 4; sym: -19 ),
{ 30: } ( len: 0; sym: -20 ),
{ 31: } ( len: 1; sym: -20 ),
{ 32: } ( len: 1; sym: -22 ),
{ 33: } ( len: 3; sym: -22 ),
{ 34: } ( len: 0; sym: -24 ),
{ 35: } ( len: 2; sym: -23 )
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
