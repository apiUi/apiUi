%{
{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
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
%}

%token _COMMENT
%token _LINECONT
%token _NAME
%token _NEWLINE
%token _WHITESPACE
%token _INDENT
%token _HYPHENINDENT
%token _VALUE
%token _ARRAYVALUE
%token _PIPE
%token _LEFT_SQUARE_BRACKET
%token _LEFT_CURLY_BRACKET
%token _RIGHT_SQUARE_BRACKET
%token _RIGHT_CURLY_BRACKET
%token _COLON
%token _COMMA
%token _STRING
%token _NUMBER
%token _FALSE
%token _NULL
%token _TRUE
%token _IS
%token _IGNORE
%token _REPLCRANDSTRIP
%token _REPLCRANDKEEP
%token _REPLCRANDCLIP
%token _KEEPCRANDSTRIP
%token _KEEPCRANDKEEP
%token _KEEPCRANDCLIP
%token _NOTUSEDLASTONE


%%
%{

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

%}

start:
      {
        PreviousXml := nil;
        Xml.Checked := True;
        Xml.jsonType := jsonObject;
        Xml.Name := 'yaml';
        PushInteger (Xml.Tag);
        Xml.Tag := -1;
      }
      OptionalComments
      OptionalYamlObjects
      {
        Xml.Tag := PopInteger;
      }
    ;

OptionalComments:
      /* void */
    | Comments
    ;

Comments:
      Comment
    | Comments Comment
    ;
Comment:
      _COMMENT
    ;

OptionalYamlObjects:
      /* void */
    | YamlObjects
    ;

YamlObjects:
      YamlObject
    | YamlObjects YamlObject
    ;

YamlObject:
      {
        CurrentXml := TXml.Create;
        CurrentXml.Checked := True;
        CurrentXml.Tag := 0;
        CurrentXml.jsonType := jsonObject;
      }
      NameValuePair
      {
        if not Assigned (PreviousXml) then
          Xml.AddXml (CurrentXml)
        else
          AddToParent (PreviousXml, CurrentXml);
        PreviousXml := CurrentXml;
      }
    ;

NameValuePair:
      _NAME OptionalValue
      {
        CurrentXml.Name := $1.yyString;
        CurrentXml.Tag := $1.yy.yyInteger;
      }
    | arrayEntry
    | arrayValueWithoutName
    ;

arrayEntry:
      _HYPHENINDENT
      {
        CurrentXml.Name := '_';
        CurrentXml.jsonType := jsonArrayValue;
        CurrentXml.Tag := $1.yy.yyInteger;
      }
    ;

arrayValueWithoutName:
      _ARRAYVALUE
      {
        CurrentXml.Value := $1.yyString;
        CurrentXml.jsonType := jsonString;
        CurrentXml.Tag := $1.yy.yyInteger;
      }
    ;


OptionalValue:
      /* void */
    | Value
    ;

Value:
      _VALUE
      {
        CurrentXml.Value := $1.yyString;
        CurrentXml.jsonType := jsonString;
        CurrentXml.Tag := $1.yy.yyInteger;
      }
    ;


