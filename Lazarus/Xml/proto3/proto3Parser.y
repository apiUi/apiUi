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
type TProto3Parser = class (TCustParser)
private
  OfIdString: String;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  wsdl: TWsdl;
  syntax_version, package_name, proto_reserved: string;
  reserved_statements, import_statements, option_statements: TJBStringList;
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  constructor Create (aOwner: TObject); override;
  destructor Destroy; override;
end;
%}

%token _LINEENDING
%token _TERMINATOR
%token _IS
%token _DOT
%token _COMMA
%token _LT
%token _GT
%token _OPENPARENTHESIS
%token _CLOSEPARENTHESIS
%token _OPENBRACKET
%token _CLOSEBRACKET
%token _OPENBRACE
%token _CLOSEBRACE
%token _PLUS
%token _MINUS
%token _DECIMALLIT
%token _OCTALLIT
%token _HEXLIT
%token _FLOATLIT
%token _BOOLLIT
%token _STRINGLIT
%token _IDENT
%token _FULLIDENT
%token _BOOL
%token _BYTES
%token _DOUBLE
%token _ENUM
%token _ENUMTYPE
%token _FIXED32
%token _FIXED64
%token _FLOAT
%token _IMPORT
%token _INT32
%token _INT64
%token _MAP
%token _MAX
%token _MESSAGE
%token _MESSAGETYPE
%token _ONEOF
%token _OPTION
%token _PACKAGE
%token _PACKED
%token _PUBLIC
%token _REPEATED
%token _RESERVED
%token _RETURNS
%token _RPC
%token _SERVICE
%token _SFIXED32
%token _SFIXED64
%token _SINT32
%token _SINT64
%token _STREAM
%token _STRING
%token _SYNTAX
%token _TO
%token _UINT32
%token _UINT64
%token _WEAK

%token _IGNORE

%%
%{

implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TProto3Parser.Prepare;
begin
  reserved_statements.Clear;
  import_statements.Clear;
  option_statements.Clear;
end;

destructor TProto3Parser.Destroy;
begin
  reserved_statements.free;
  import_statements.free;
  option_statements.free;
  inherited Destroy;
end;

constructor TProto3Parser.Create(aOwner: TObject);
begin
  inherited Create (aOwner);
  reserved_statements := TJBStringList.Create;
  import_statements := TJBStringList.Create;
  option_statements := TJBStringList.Create;
end;

function TProto3Parser.yylex: Integer;
begin
  result := inherited yylex;
end;
%}

start:
      Syntax OptionalStatementList
      {
        ShowMessage ( 'syntax: ' + syntax_version + LineEnding
                    + 'package: ' + package_name + LineEnding
                    + 'Import: ' + import_statements.Text + LineEnding
                    + 'Options: ' + option_statements.Text + LineEnding
                    )
      }
    ;

Syntax:
       _SYNTAX _IS _STRINGLIT _TERMINATOR
       {
         syntax_version := $3.yyString;
       }
       ;

OptionalStatementList:
          /* void */
        | StatementList
        ;

StatementList:
          Statement
        | StatementList Statement
        ;

Statement:
           _TERMINATOR
         | ImportStatement
         | PackageStatement
         | OptionStatement
         | TopLevelDef
         ;

ImportStatement:
           _IMPORT ImportOption _STRINGLIT  _TERMINATOR
           {
             import_statements.Add ($3.yyString);
           }
         ;

ImportOption:
           /* void */
         | _WEAK
         | _PUBLIC
         ;

PackageStatement:
           _PACKAGE Identifier _TERMINATOR
           {
             package_name := $2.yyString;
           }
         ;

OptionStatement:
           _OPTION OptionName _IS Constant _TERMINATOR
           {
             option_statements.Add ($2.yyString + ': ' + $4.yyString);
           }
         ;
OptionName:
           Identifier
         /* optionName = ( ident | "(" fullIdent ")" ) { "." ident } */
         | _OPENPARENTHESIS
           Identifier
           _CLOSEPARENTHESIS
           OptionalDotItentifier
           {
             $$.yyString := '(' + $2.yyString + ')' + $4.yyString
           }
         ;
OptionalDotItentifier:
           /* void */
           {
             $$.yyString := '';
           }
         | _DOT Identifier
           {
             $$.yyString := '.' + $2.yyString;
           }
         ;

Constant:
           Identifier
         | Integer
         | Float
         | _STRINGLIT
         | _BOOLLIT
         ;
Integer:
           _DECIMALLIT
         | _OCTALLIT
         | _HEXLIT
         | _PLUS _DECIMALLIT
         | _PLUS _OCTALLIT
         | _PLUS _HEXLIT
         | _MIN _DECIMALLIT
         | _MIN _OCTALLIT
         | _MIN _HEXLIT
         ;

Identifier:
           _IDENT
         | _FULLIDENT
         ;
Float:
           _FLOATLIT
         | _PLUS _FLOATLIT
         | _MIN _FLOATLIT
         ;

TopLevelDef:
           Enum
         | Message
         | Service
         ;

/*---
enum = "enum" enumName enumBody
enumBody = "{" { option | enumField | emptyStatement } "}"
enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
enumValueOption = optionName "=" constant
---*/
Enum:
           _ENUM _IDENT _OPENBRACE EnumItemList _CLOSEBRACE
         ;
EnumItemList:
           EnumItem
         | EnumItemList EnumItem
         ;
EnumItem:
           /* void */
         | EnumOption
         | EnumField
         ;
EnumOption:
           _OPTION _IDENT _IS _BOOLLIT _TERMINATOR
         ;
EnumField:
           _IDENT _IS Integer OptionalEnumValueOption _TERMINATOR
         ;
OptionalEnumValueOption:
           /* void */
         | _OPENBRACKET OptionName _IS Constant _CLOSEBRACKET
         ;

/*----
message = "message" messageName messageBody
messageBody = "{" { field | enum | message | option | oneof | mapField |
reserved | emptyStatement } "}"
----*/

Message:
           _MESSAGE _IDENT _OPENBRACE MessageItemList _CLOSEBRACE
         ;
MessageItemList:
           MessageItem
         | MessageItemList MessageItem
         ;
MessageItem:
           /* void */
         | Field
         | Enum
         | Message
         | OptionStatement
         | OneOfStatement
         | MapField
         | ReservedStatement
         | _TERMINATOR
         ;
/*
field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
fieldOptions = fieldOption { ","  fieldOption }
fieldOption = optionName "=" constant
*/
Field:
           FieldSpec
         | _REPEATED FieldSpec
         ;
FieldSpec:
           FieldType _IDENT _IS Integer OptionalFieldOptions _TERMINATOR
         ;
FieldType:
           BuiltInFieldType
         | Reference
         ;
Reference:
           Identifier
         | _DOT Identifier
         ;
BuiltInFieldType:
           _DOUBLE
         | _FLOAT
         | _INT32
         | _INT64
         | _UINT32
         | _UINT64
         | _SINT32
         | _SINT64
         | _FIXED32
         | _FIXED64
         | _SFIXED32
         | _SFIXED64
         | _BOOL
         | _STRING
         | _BYTES
         ;

OptionalFieldOptions:
           /* void */
         | _OPENBRACKET FieldOptionList _CLOSEBRACKET
         ;
FieldOptionList:
           FieldOption
         | FieldOptionList _COMMA FieldOption
         ;
FieldOption:
           OptionStatement
         ;

/*--
oneof = "oneof" oneofName "{" { option | oneofField | emptyStatement } "}"
oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
--*/
OneOfStatement:
           _ONEOF _IDENT _OPENBRACE OneOfItemList _CLOSEBRACE
         ;
OneOfItemList:
           OneOfItem
         | OneOfItemList  OneOfItem
         ;
OneOfItem:
           /* void */
         | OptionStatement
         | FieldSpec
         ;

MapField:
           _MAP _LT KeyType _COMMA FieldType _GT _IDENT _IS Integer OptionalFieldOptions _TERMINATOR
         ;
KeyType:
           _INT32
         | _INT64
         | _UINT32
         | _UINT64
         | _SINT32
         | _SINT64
         | _FIXED32
         | _FIXED64
         | _SFIXED32
         | _SFIXED64
         | _BOOL
         | _STRING
         ;

ReservedStatement:
           _RESERVED
           {
             proto_reserved := '';
           }
           ReservedSpec _TERMINATOR
           {
             reserved_statements.Add (proto_reserved);
           }
         ;

ReservedSpec:
           IntegerRanges
         | FieldNames
         ;

IntegerRanges:
           IntegerRange
         | IntegerRanges _COMMA IntegerRange
         ;

IntegerRange:
           Integer OptionalToIntegerRange
         ;

OptionalToIntegerRange:
           /* void */
         | _TO MaxOrInteger
         ;

MaxOrInteger:
           _MAX
         | Integer
         ;
FieldNames:
           _STRINGLIT
           {
             proto_reserved := $1.yyString;
           }
         | FieldNames _COMMA _STRINGLIT
           {
             proto_reserved := proto_reserved + ', ' + $3.yyString;
           }
         ;
/*---
service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
---*/

Service:
           _SERVICE _IDENT _OPENBRACE OptionalServiceItemList _CLOSEBRACE
         ;
OptionalServiceItemList:
           /* void */
         | ServiceItemList
         ;
ServiceItemList:
           ServiceItem
         | ServiceItemList ServiceItem
         ;
ServiceItem:
           _TERMINATOR
         | OptionStatement
         | RpcStatement
         ;
RpcStatement:
           _RPC _IDENT _OPENPARENTHESIS MessageSpec _CLOSEPARENTHESIS
           _RETURNS _OPENPARENTHESIS MessageSpec _CLOSEPARENTHESIS
           RpcOptionsBlockOrTerminator
         ;
MessageSpec:
           _STREAM MessageType
         | MessageType
         ;
MessageType:
           Identifier
         | _DOT Identifier
         ;
RpcOptionsBlockOrTerminator:
           RpcOptionsBlock
         |  _TERMINATOR
         ;
RpcOptionsBlock:
           _OPENBRACE RpcOptionalOptionsList _CLOSEBRACE
           ;
RpcOptionalOptionsList:
           /* void */
         | RpcOptionsList
         ;
RpcOptionsList:
           RpcOption
         | RpcOptionsList RpcOption
         ;
RpcOption:
           OptionStatement
         ;
