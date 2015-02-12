%{
type TIpmParser = class (TCustParser)
private
  OfIdString: String;
  FOnHaveScanned: TOnHaveScannedEvent;
published
  property OnHaveScanned: TOnHaveScannedEvent read FOnHaveScanned write FOnHaveScanned;
  property OnError: TOnErrorEvent read FOnError write FOnError;
public
  function yylex: Integer; override;
  function yyparse: Integer; override;
  procedure Prepare; override;
  destructor Destroy; override;
end;
%}

%token _BY
%token _COMP
%token _CONDITIONLEVEL
%token _DEPENDING
%token _DISPLAY
%token _IGNORE
%token _IN
%token _INDEXED
%token _INTEGER
%token _IS
%token _LEADING
%token _NATIVE_2
%token _NATIVE_4
%token _NATIVE_8
%token _NOID
%token _OCCURS
%token _OF
%token _ON
%token _PICTURE
%token _PICTURECLAUSE
%token _REDEFINES
%token _SEPARATE
%token _SIGN
%token _SPACE
%token _SPACES
%token _STRING
%token _TERMINATOR
%token _THROUGH
%token _TIMES
%token _TO
%token _TRAILING
%token _USAGE
%token _VALUE
%token _ZERO
%token _ZEROES

%%
%{

implementation
  (* local definitions: *)
uses SysUtils, Dialogs
   ;

procedure TIpmParser.Prepare;
begin
  inherited Prepare;
end;

destructor TIpmParser.Destroy;
begin
  inherited Destroy;
end;

function TIpmParser.yylex: Integer;
begin
  result := inherited yylex;
end;

%}

start:
      {
        BaseIpmItem := nil;
        PreviousIpmItem := nil;
        CurrentIpmItem := nil;
      }
      OptionalIpmSpecs
    ;

OptionalIpmSpecs:
      /* void */
    | IpmSpecs
    ;

IpmSpecs:
      IpmSpec
    | IpmSpecs IpmSpec
    ;

IpmSpec:
      {
        CurrentIpmItem := TIpmItem.Create;
        CurrentIpmItem.minOccurs := 1;
        CurrentIpmItem.Occurs := 1;
      }
      IpmClause
      {
        if BaseIpmItem = nil then
          BaseIpmItem := CurrentIpmItem
        else
        begin
          if (CurrentIpmItem.Redefines)
          and (CurrentIpmItem.Level = BaseIpmItem.Level) then
          begin
            CurrentIpmItem.Free;
            raise Exception.Create ('Unable to deal with redefine on first level');
          end;
          CurrentIpmItem.LinkToParent (PreviousIpmItem);
        end;
        PreviousIpmItem := CurrentIpmItem;
      }
    ;

IpmClause:
      _INTEGER _NOID OptionalAttributes _TERMINATOR OptionalConditionNames
      {
        CurrentIpmItem.Name := $2.TokenString;
        CurrentIpmItem.Level := $1.yy.yyInteger;
      }
    | _INTEGER OptionalAttributes _TERMINATOR OptionalConditionNames
      {
        CurrentIpmItem.Name := 'filler';
        CurrentIpmItem.Level := $1.yy.yyInteger;
      }
    ;

OptionalAttributes:
      /* void */
    | Attributes
    ;

Attributes:
      Attribute
    | Attributes Attribute
    ;

Attribute:
      _USAGE
      {
      }
    | _COMP
      {
        CurrentIpmItem.Comp := True;
      }
    | _DISPLAY
      {
        CurrentIpmItem.Display := True;
      }
    | _NATIVE_2
      {
        CurrentIpmItem.Comp := True;
        CurrentIpmItem.PictureClause := 'S9(04)';
        igLength := PictureClauseToLength (PictureClause);
      }
    | _NATIVE_4
      {
        CurrentIpmItem.Comp := True;
        CurrentIpmItem.PictureClause := 'S9(09)';
        igLength := PictureClauseToLength (PictureClause);
      }
    | _NATIVE_8
      {
        CurrentIpmItem.Comp := True;
        CurrentIpmItem.PictureClause := 'S9(18)';
        igLength := PictureClauseToLength (PictureClause);
      }
    | _SIGN OptionalLeadingTrailing OptionalSeparate
    | _OCCURS OccursPhrases
    | _PICTURE OptionalIs _PICTURECLAUSE
      {
        CurrentIpmItem.PictureClause := $3.TokenString;
        if (CurrentIpmItem.PictureClause [1] = 's')
        or (CurrentIpmItem.PictureClause [1] = 'S') then
          CurrentIpmItem.Signed := True;
        igLength := PictureClauseToLength (PictureClause);
      }
    | _REDEFINES _NOID
      {
        CurrentIpmItem.Redefines := True;
      }
    ;

OccursPhrases:
      OccursPhrase
    | OccursPhrases OccursPhrase
    ;

OccursPhrase:
      _INTEGER OptionalTimes
      {
        CurrentIpmItem.Occurs := $1.yy.yyInteger;
      }
    | _INTEGER _TO _INTEGER OptionalTimes
      {
        CurrentIpmItem.minOccurs := $1.yy.yyInteger;
        CurrentIpmItem.Occurs := $3.yy.yyInteger;
      }
    | _DEPENDING OptionalOn DataName
    | _INDEXED OptionalBy DataName
    ;

OptionalBy:
      /* void */
    | _BY
    ;

OptionalIs:
      /* void */
    | _IS
    ;

OptionalLeadingTrailing:
      /* void */
    | _LEADING
      {
        CurrentIpmItem.SignLeading := True;
      }
    | _TRAILING
      {
        CurrentIpmItem.SignLeading := False;
      }
    ;

OptionalOn:
      /* void */
    | _ON
    ;

OptionalSeparate:
      /* void */
    | _SEPARATE
      {
        CurrentIpmItem.SignSeparate := True;
      }
    ;

OptionalTimes:
      /* void */
    | _TIMES
    ;

DataName:
      ElementaryDataName
    | DataName InOrOf ElementaryDataName
    ;

ElementaryDataName:
      _NOID
      {
        UserWorkingArea := UserWorkingArea
                         + UWASeparator
                         + $1.TokenString
                         ;
        UWASeparator := ' of ';
      }
    ;

InOrOf:
      _IN
    | _OF
    ;

OptionalConditionNames:
      /* void */
    | ConditionNames
    ;

ConditionNames:
      ConditionName
    | ConditionNames ConditionName
    ;

ConditionName:
      _CONDITIONLEVEL _NOID _VALUE OptionalIs ConditionValues _TERMINATOR
    ;

ConditionValues:
      ConditionValue
    | ConditionValues ConditionValue
    ;

ConditionValue:
      _STRING
      {
        CurrentIpmItem.Level88Values.Add ($1.yyString);
      }
    | _INTEGER
      {
        CurrentIpmItem.Level88Values.Add ($1.TokenString);
      }
    | ConditionValue _THROUGH ConditionValue
    ;

