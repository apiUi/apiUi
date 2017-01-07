%{
type TJsnParser = class (TCustParser)
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
%}

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

%%
%{

implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TJsnParser.Prepare;
begin
  inherited Prepare;
end;

destructor TJsnParser.Destroy;
begin
  inherited Destroy;
end;

function TJsnParser.yylex: Integer;
begin
  result := inherited yylex;
end;

function TJsnParser.StringDecode(aString: String): String;
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


%}

start:
      {
        ParentXml := nil;
        Xml.Checked := True;
        Xml.jsonType := jsonObject;
        Xml.Name := '';
      }
      OptionalJsonObjects
      {
      }
    ;

OptionalJsonObjects:
      /* void */
    | OptionalIgnoredSpace JsonObjects OptionalIgnoredSpace
    ;

OptionalIgnoredSpace:
      /* void */
    | _VALUE
      {
        if Trim ($1.TokenString) <> '' then
          raise Exception.Create ( '%Illegal: ['
                                 + $1.TokenString
                                 + ']['
                                 + IntToStr ($1.LineNumber)
                                 + ':'
                                 + IntToStr ($1.ColumnNumber)
                                 + ']'
                                 );
      }
    ;


JsonObjects:
      JsonObject
    | JsonObjects JsonObject
    | JsonArray
    ;

JsonObject:
      _LEFT_CURLY_BRACKET
      {
         Xml.jsonType := jsonObject;
         if not Assigned (ParentXml) then
           Xml.Name := 'json';
     }
      Object
      {
      }
      _RIGHT_CURLY_BRACKET
    ;

Object:
      OptionalListOfMembers
    ;

OptionalListOfMembers:
      /* void */
    | ListOfMembers
    ;

ListOfMembers:
      Member
    | ListOfMembers _COMMA Member
    ;

Member:
      {
        PushObject (ParentXml);
        PushObject (Xml);
        ParentXml := Xml;
        Xml := ParentXml.AddXml (TXml.Create);
        Xml.Checked := True;
      }
      NameValuePair
      {
        Xml := PopObject as TXml;
        ParentXml := PopObject as TXml;
      }
    ;

NameValuePair:
      _STRING _COLON
      {
        Xml.Name := Copy ($1.TokenString, 2, Length ($1.TokenString) - 2);
      }
      Value
    ;

Value:
      _STRING
      {
        Xml.Value := StringDecode(Copy ($1.TokenString, 2, Length ($1.TokenString) - 2));
        Xml.jsonType := jsonString;
      }
    | _NUMBER
      {
        Xml.Value := $1.TokenString;
        Xml.jsonType := jsonNumber;
      }
    | JsonObject
    | JsonArray
    | _TRUE
      {
        Xml.Value := 'true';
        Xml.jsonType := jsonBoolean;
      }
    | _FALSE
      {
        Xml.Value := 'false';
        Xml.jsonType := jsonBoolean;
      }
    | _NULL
      {
        Xml.Checked := False;
      }
    ;

JsonArray:
      _LEFT_SQUARE_BRACKET
      {
         Xml.jsonType := jsonArray;
         if not Assigned (ParentXml) then
           Xml.Name := 'json';
      }
      optionalArrayValues
      _RIGHT_SQUARE_BRACKET
    ;

optionalArrayValues:
      /* void */
    | ArrayValues
    ;

ArrayValues:
      ArrayValue
    | ArrayValues _COMMA ArrayValue
    ;

ArrayValue:
      {
        PushObject (ParentXml);
        ParentXml := Xml;
        PushObject (Xml);
        Xml := ParentXml.AddXml (TXml.Create);
        Xml.Checked := True;
        Xml.Name := ParentXml.Name + '__Value';
        Xml.Name := '';
        Xml.jsonType := jsonArrayValue;
      }
      Value
      {
        Xml := PopObject as TXml;
        ParentXml := PopObject as TXml;
      }
    ;
