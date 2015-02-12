unit XmlParser;

interface
uses Classes
   , ParserClasses
   , CustParser
   , igGlobals
   , Xmlz
   ;


type TXmlParser = class (TCustParser)
private
  CurrentXml: TXml;
  ParentXml: TXml;
  OfIdString: String;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  BaseXml: TXml;
  procedure AddAttribute (aName: String; aValue: String);
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  destructor Destroy; override;
end;
const _ATTRIBUTENAME = 257;
const _ATTRIBUTEVALUE = 258;
const _CDATA = 259;
const _CDATAENDTAG = 260;
const _CDATATAG = 261;
const _EMPTYENDTAG = 262;
const _ENDTAG = 263;
const _ESCAPECHAR = 264;
const _IGNORE = 265;
const _IS = 266;
const _SLASHTAG = 267;
const _TAG = 268;
const _VALUE = 269;



implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TXmlParser.AddAttribute (aName: String; aValue: String);
var
  xAttribute: TXmlAttribute;
begin
  xAttribute := TXmlAttribute.Create;
  xAttribute.Name := aName;
  xAttribute.Value := xmlDecodeXml(Copy (aValue, 2, Length (aValue) - 2));
  xAttribute.Checked := True;
  CurrentXml.AddAttribute (xAttribute);
end;

procedure TXmlParser.Prepare;
begin
  inherited Prepare;
end;

destructor TXmlParser.Destroy;
begin
  inherited Destroy;
end;

function TXmlParser.yylex: Integer;
begin
  result := inherited yylex;
end;

const yymaxdepth = 1024;

function TXmlParser.yyparse : Integer;

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
         CurrentXml := nil;
         BaseXml := nil;
         
       end;
   2 : begin
         yyval := yyv[yysp-1];
       end;
   3 : begin
       end;
   4 : begin
         yyval := yyv[yysp-2];
       end;
   5 : begin
       end;
   6 : begin
         
         if (Trim (yyv[yysp-0].TokenString) <> '')
         and (Trim (yyv[yysp-0].TokenString) <> BOM) then
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
         
         PushObject (CurrentXml);
         CurrentXml := TXml.Create;
         CurrentXml.Checked := True;
         CurrentXml.Parent := ParentXml;
         if BaseXml = nil then
         BaseXml := CurrentXml;
         if ParentXml <> nil then
         (ParentXml.Items as TXmlList).AddObject ('', CurrentXml);
         
       end;
  10 : begin
         
         if (CurrentXml.Items.Count > 0)
         and (CurrentXml.Value <> '')
         and (Trim (CurrentXml.Value) = '') then
         CurrentXml.Value := '';
         CurrentXml := PopObject as TXml;
         
       end;
  11 : begin
         
         CurrentXml.TagName := Copy ( yyv[yysp-0].TokenString
         , 2
         , system.Length (yyv[yysp-0].TokenString) - 1
         );
         
       end;
  12 : begin
         yyval := yyv[yysp-3];
       end;
  13 : begin
         
         CurrentXml.CData := True;
         
       end;
  14 : begin
         
         if CurrentXml.TagName <> Copy ( yyv[yysp-1].TokenString
         , 3
         , system.Length (yyv[yysp-1].TokenString) - 2
         )
         then
         raise Exception.Create ( CurrentXml.TagName
         + ': Unmatched EndTag ('
         + yyv[yysp-1].TokenString
         + ')'
         );
         
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
       end;
  17 : begin
         yyval := yyv[yysp-0];
       end;
  18 : begin
         yyval := yyv[yysp-0];
       end;
  19 : begin
         yyval := yyv[yysp-1];
       end;
  20 : begin
         
         AddAttribute (yyv[yysp-2].TokenString, yyv[yysp-0].TokenString);
         
       end;
  21 : begin
       end;
  22 : begin
         yyval := yyv[yysp-0];
       end;
  23 : begin
         yyval := yyv[yysp-0];
       end;
  24 : begin
         yyval := yyv[yysp-1];
       end;
  25 : begin
         
         CurrentXml.Value := CurrentXml.Value + yyv[yysp-0].TokenString;
         
       end;
  26 : begin
         yyval := yyv[yysp-0];
       end;
  27 : begin
         
         PushObject (ParentXml);
         ParentXml := CurrentXml;
         
       end;
  28 : begin
         
         ParentXml := PopObject as TXml;
         
       end;
  29 : begin
       end;
  30 : begin
         yyval := yyv[yysp-0];
       end;
  31 : begin
         yyval := yyv[yysp-0];
       end;
  32 : begin
         yyval := yyv[yysp-1];
       end;
  33 : begin
         
         CurrentXml.Value := CurrentXml.Value + yyv[yysp-0].TokenString;
         
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

yynacts   = 36;
yyngotos  = 31;
yynstates = 41;
yynrules  = 33;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 269; act: 5 ),
  ( sym: 0; act: -3 ),
  ( sym: 261; act: -5 ),
  ( sym: 268; act: -5 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  ( sym: 261; act: 10 ),
  ( sym: 268; act: 11 ),
{ 7: }
{ 8: }
  ( sym: 269; act: 5 ),
  ( sym: 0; act: -5 ),
  ( sym: 261; act: -9 ),
  ( sym: 268; act: -9 ),
{ 9: }
{ 10: }
  ( sym: 259; act: 17 ),
  ( sym: 260; act: -29 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: 259; act: 17 ),
  ( sym: 260; act: -30 ),
{ 16: }
  ( sym: 260; act: 20 ),
{ 17: }
{ 18: }
  ( sym: 257; act: 24 ),
  ( sym: 262; act: -16 ),
  ( sym: 263; act: -16 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: 257; act: 24 ),
  ( sym: 262; act: -17 ),
  ( sym: 263; act: -17 ),
{ 23: }
  ( sym: 262; act: 27 ),
  ( sym: 263; act: 28 ),
{ 24: }
  ( sym: 266; act: 29 ),
{ 25: }
{ 26: }
{ 27: }
{ 28: }
  ( sym: 269; act: 35 ),
  ( sym: 267; act: -21 ),
  ( sym: 261; act: -27 ),
  ( sym: 268; act: -27 ),
{ 29: }
  ( sym: 258; act: 36 ),
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: 269; act: 35 ),
  ( sym: 267; act: -22 ),
  ( sym: 261; act: -27 ),
  ( sym: 268; act: -27 ),
{ 34: }
  ( sym: 267; act: 39 ),
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
  ( sym: 263; act: 40 )
{ 40: }
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
  ( sym: -9; act: 6 ),
  ( sym: -7; act: 7 ),
  ( sym: -6; act: 8 ),
{ 4: }
{ 5: }
{ 6: }
  ( sym: -8; act: 9 ),
{ 7: }
{ 8: }
  ( sym: -9; act: 6 ),
  ( sym: -7; act: 12 ),
  ( sym: -5; act: 13 ),
{ 9: }
{ 10: }
  ( sym: -22; act: 14 ),
  ( sym: -21; act: 15 ),
  ( sym: -13; act: 16 ),
{ 11: }
  ( sym: -11; act: 18 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: -22; act: 19 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: -16; act: 21 ),
  ( sym: -15; act: 22 ),
  ( sym: -10; act: 23 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: -16; act: 25 ),
{ 23: }
  ( sym: -12; act: 26 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
  ( sym: -20; act: 30 ),
  ( sym: -19; act: 31 ),
  ( sym: -18; act: 32 ),
  ( sym: -17; act: 33 ),
  ( sym: -14; act: 34 ),
{ 29: }
{ 30: }
  ( sym: -9; act: 6 ),
  ( sym: -7; act: 37 ),
{ 31: }
{ 32: }
{ 33: }
  ( sym: -20; act: 30 ),
  ( sym: -19; act: 31 ),
  ( sym: -18; act: 38 )
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } 0,
{ 3: } -9,
{ 4: } -2,
{ 5: } -6,
{ 6: } 0,
{ 7: } -7,
{ 8: } 0,
{ 9: } -10,
{ 10: } 0,
{ 11: } -11,
{ 12: } -8,
{ 13: } -4,
{ 14: } -31,
{ 15: } 0,
{ 16: } 0,
{ 17: } -33,
{ 18: } 0,
{ 19: } -32,
{ 20: } -13,
{ 21: } -18,
{ 22: } 0,
{ 23: } 0,
{ 24: } 0,
{ 25: } -19,
{ 26: } -12,
{ 27: } -15,
{ 28: } 0,
{ 29: } 0,
{ 30: } -9,
{ 31: } -26,
{ 32: } -23,
{ 33: } 0,
{ 34: } 0,
{ 35: } -25,
{ 36: } -20,
{ 37: } -28,
{ 38: } -24,
{ 39: } 0,
{ 40: } -14
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 5,
{ 3: } 6,
{ 4: } 6,
{ 5: } 6,
{ 6: } 6,
{ 7: } 8,
{ 8: } 8,
{ 9: } 12,
{ 10: } 12,
{ 11: } 14,
{ 12: } 14,
{ 13: } 14,
{ 14: } 14,
{ 15: } 14,
{ 16: } 16,
{ 17: } 17,
{ 18: } 17,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 20,
{ 23: } 23,
{ 24: } 25,
{ 25: } 26,
{ 26: } 26,
{ 27: } 26,
{ 28: } 26,
{ 29: } 30,
{ 30: } 31,
{ 31: } 31,
{ 32: } 31,
{ 33: } 31,
{ 34: } 35,
{ 35: } 36,
{ 36: } 36,
{ 37: } 36,
{ 38: } 36,
{ 39: } 36,
{ 40: } 37
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 4,
{ 2: } 5,
{ 3: } 5,
{ 4: } 5,
{ 5: } 5,
{ 6: } 7,
{ 7: } 7,
{ 8: } 11,
{ 9: } 11,
{ 10: } 13,
{ 11: } 13,
{ 12: } 13,
{ 13: } 13,
{ 14: } 13,
{ 15: } 15,
{ 16: } 16,
{ 17: } 16,
{ 18: } 19,
{ 19: } 19,
{ 20: } 19,
{ 21: } 19,
{ 22: } 22,
{ 23: } 24,
{ 24: } 25,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 29,
{ 29: } 30,
{ 30: } 30,
{ 31: } 30,
{ 32: } 30,
{ 33: } 34,
{ 34: } 35,
{ 35: } 35,
{ 36: } 35,
{ 37: } 35,
{ 38: } 35,
{ 39: } 36,
{ 40: } 36
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 5,
{ 3: } 5,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 9,
{ 8: } 9,
{ 9: } 12,
{ 10: } 12,
{ 11: } 15,
{ 12: } 16,
{ 13: } 16,
{ 14: } 16,
{ 15: } 16,
{ 16: } 17,
{ 17: } 17,
{ 18: } 17,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 20,
{ 23: } 21,
{ 24: } 22,
{ 25: } 22,
{ 26: } 22,
{ 27: } 22,
{ 28: } 22,
{ 29: } 27,
{ 30: } 27,
{ 31: } 29,
{ 32: } 29,
{ 33: } 29,
{ 34: } 32,
{ 35: } 32,
{ 36: } 32,
{ 37: } 32,
{ 38: } 32,
{ 39: } 32,
{ 40: } 32
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 4,
{ 2: } 4,
{ 3: } 7,
{ 4: } 7,
{ 5: } 7,
{ 6: } 8,
{ 7: } 8,
{ 8: } 11,
{ 9: } 11,
{ 10: } 14,
{ 11: } 15,
{ 12: } 15,
{ 13: } 15,
{ 14: } 15,
{ 15: } 16,
{ 16: } 16,
{ 17: } 16,
{ 18: } 19,
{ 19: } 19,
{ 20: } 19,
{ 21: } 19,
{ 22: } 20,
{ 23: } 21,
{ 24: } 21,
{ 25: } 21,
{ 26: } 21,
{ 27: } 21,
{ 28: } 26,
{ 29: } 26,
{ 30: } 28,
{ 31: } 28,
{ 32: } 28,
{ 33: } 31,
{ 34: } 31,
{ 35: } 31,
{ 36: } 31,
{ 37: } 31,
{ 38: } 31,
{ 39: } 31,
{ 40: } 31
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
{ 9: } ( len: 0; sym: -9 ),
{ 10: } ( len: 2; sym: -7 ),
{ 11: } ( len: 0; sym: -11 ),
{ 12: } ( len: 4; sym: -8 ),
{ 13: } ( len: 3; sym: -8 ),
{ 14: } ( len: 4; sym: -12 ),
{ 15: } ( len: 1; sym: -12 ),
{ 16: } ( len: 0; sym: -10 ),
{ 17: } ( len: 1; sym: -10 ),
{ 18: } ( len: 1; sym: -15 ),
{ 19: } ( len: 2; sym: -15 ),
{ 20: } ( len: 3; sym: -16 ),
{ 21: } ( len: 0; sym: -14 ),
{ 22: } ( len: 1; sym: -14 ),
{ 23: } ( len: 1; sym: -17 ),
{ 24: } ( len: 2; sym: -17 ),
{ 25: } ( len: 1; sym: -18 ),
{ 26: } ( len: 1; sym: -18 ),
{ 27: } ( len: 0; sym: -20 ),
{ 28: } ( len: 2; sym: -19 ),
{ 29: } ( len: 0; sym: -13 ),
{ 30: } ( len: 1; sym: -13 ),
{ 31: } ( len: 1; sym: -21 ),
{ 32: } ( len: 2; sym: -21 ),
{ 33: } ( len: 1; sym: -22 )
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
