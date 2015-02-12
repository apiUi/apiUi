unit XpQuery;

interface

uses Classes, Bind, ADOdb
  ;

type TXpQueryVerb = (xqvSelect, xqvInsert, xqvUpdate, xqvDelete);

type TXpQuery = class (TADOQuery)
public
  QueryVerb: TXpQueryVerb;
  SqlStrings: TStringList;
  BindList: TBindList;
  ParamList: TBindList;
  InsertValuesString: String;
  InsertValuesSeparator: String;
  procedure xpPrep;
  constructor Create (AComponent: TComponent); override;
  destructor Destroy; override;
end;

type TXpQueryList = class (TStringList)
protected
  function GetXpQuery (Index: integer): TXpQuery;
public
  property XpQueries [Index: integer]: TXpQuery read GetXpQuery;
  procedure Clear; override;
end;


implementation

constructor TXpQuery.Create (AComponent: TComponent);
begin
  inherited Create (AComponent);
  SqlStrings := TStringList.Create;
  BindList := TBindList.Create;
  ParamList := TBindList.Create;
end;

destructor TXpQuery.Destroy;
begin
  BindList.Free;
  ParamList.Free;
  SqlStrings.Clear;
  SqlStrings.Free;
  inherited Destroy;
end;

function TXpQueryList.GetXpQuery (Index: integer): TXpQuery;
begin
  result := TXpQuery (Objects [index]);
end;

procedure TXpQueryList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    XpQueries [x].Free;
  inherited Clear;
end;

procedure TXpQuery.xpPrep;
begin
  if not Connection.Connected then
    Connection.Connected := True;
  SQL.Text := SqlStrings.Text;
  Prepared := True;
end;

end.
