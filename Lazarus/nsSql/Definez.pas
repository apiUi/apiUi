unit Definez;

interface
uses Classes
   , Dialogs
  ;

type TSqlVerb = (sqlSelect, sqlInsert, sqlUpdate, sqlDelete);

type
  TSqlData = class (TObject)
private
public
  Null: Boolean;
  Value: String;
  destructor Destroy; override;
end;

type
  TSqlDataList = class (TStringList)
protected
  function GetSqlData (Index: integer): TSqlData;
public
  property SqlDatas [Index: integer]: TSqlData read GetSqlData;
end;

type
  TSqlRow = class (TObject)
private
public
  SqlDatas: TSqlDataList;
  constructor Create;
  destructor Destroy; override;
end;

type
  TSqlRowList = class (TStringList)
protected
  function GetSqlRow (Index: integer): TSqlRow;
public
  property SqlRows [Index: integer]: TSqlRow read GetSqlRow;
end;

type
  TSqlQueryResult = class (TObject)
private
  SqlRows: TSqlRowList;
  function GetSqlData (aCol, aRow: Integer): TSqlData;
public
  property SqlData [aCol, aRow: Integer]: TSqlData read GetSqlData;
  constructor Create;
  destructor Destroy; override;
end;

type
  TColumn = class(TObject)
private
    function GetIsDefaultAllowed: Boolean;
    function GetIsNullAllowed: Boolean;
    function GetIsUpdatable: Boolean;
    function GetIsSysKey: Boolean;
    function getNullableAsText: String;
public
  ColName: String;
  ColClass: String;
  ColSize: Integer;
  NullAllowed: Boolean;
  DataType: Integer;
  Precision: Integer;
  Scale: Integer;
  DataTimeStartField: Integer;
  DateTimeEndField: Integer;
  DateTimeQualifier: String;
  KeySeqNumber: Integer;
  DefaultClass: String;
  DefaultValue: String;
  PictureClause: String;
  Value: String;
  OriginalValue: String;
  UseDefault: Boolean;
  UseNull: Boolean;
  OriginalUseNull: Boolean;
  DoUpdate: Boolean;
  property IsDefaultAllowed: Boolean read GetIsDefaultAllowed;
  property IsNullAllowed: Boolean read GetIsNullAllowed;
  property IsUpdatable: Boolean read GetIsUpdatable;
  property IsSysKey: Boolean read GetIsSysKey;
  property NullableAsText: String read getNullableAsText;
  function SqlValuePresentation (aValue: String): String;
  constructor Create;
end;

type TColumnList = class (TStringList)
  private
    function GetColumn(Index: integer): TColumn;
  protected
  public
    property Columns [Index: integer]: TColumn read GetColumn;
    function FindColumn (ColumnName: String): TColumn;
    procedure Clear; override;
end;


type
  TDefine = class(TObject)
private
    function GetInsertQuery(LineSep: String): String;
    function GetUpdateQuery(LineSep: String): String;
    function GetDeleteQuery(LineSep: String): String;
    function GetHasPrimaryKey: Boolean;
    function GetSelectStarColumns: String;
    function getWhereClause(LineSep: String): String;
public
  DefineName: String;
  FileName: String;
  Columns: TColumnList;
  ColClassKnown: Boolean;
  property InsertQuery[LineSep: String]: String read GetInsertQuery;
  property UpdateQuery[LineSep: String]: String read GetUpdateQuery;
  property DeleteQuery[LineSep: String]: String read GetDeleteQuery;
  property SelectStarColums: String read GetSelectStarColumns;
  property HasPrimaryKey: Boolean read GetHasPrimaryKey;
  property WhereClause[LineSep: String]: String read getWhereClause;
  constructor Create;
  destructor Destroy; override;
end;


type TDefineList = class (TStringList)
protected
  function GetDefine (Index: integer): TDefine;
public
  property Defines [Index: integer]: TDefine read GetDefine;
  function FindDefine (DefineName: String): TDefine;
  procedure Clear; override;
end;

type
  TQuery = class(TObject)
  public
    Text: String;
    SqlSelectStarFrom, SingleFullTableQuery: Boolean;
    NumberOfViewsInQuery: Integer;
    DefineName: String;
    LastDefine: TDefine;
    SubmitTimestamp, ResultTimestamp: TDateTime;
  end;

var
  Defines: TDefineList;

implementation

uses
  SysUtils
;

function TDefineList.FindDefine (DefineName: String): TDefine;
var
  x: Integer;
  Found: Boolean;
begin
  Found := Find (LowerCase (DefineName), x);
  if Found then
  begin
    result := Defines [x];
  end
  else
  begin
    result := nil;
  end;
end;

function TDefineList.GetDefine (Index: integer): TDefine;
begin
  result := TDefine (Objects [index]);
end;

procedure TDefineList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Defines [x].Free;
  inherited Clear;
end;

{ TColumnList }

procedure TColumnList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Columns [x].Free;
  inherited;
end;

function TColumnList.FindColumn(ColumnName: String): TColumn;
var
  x: Integer;
  Found: Boolean;
begin
  Found := Find (LowerCase (ColumnName), x);
  if Found then
  begin
    result := Columns [x];
  end
  else
  begin
    result := nil;
  end;
end;

function TColumnList.GetColumn(Index: integer): TColumn;
begin
  result := TColumn (Objects [index]);
end;

{ TDefine }

constructor TDefine.Create;
begin
  inherited Create;
  Columns := TColumnList.Create;
  ColClassKnown := False;
end;

destructor TDefine.Destroy;
begin
  Columns.Clear;
  Columns.Free;
  inherited;
end;

function TDefine.GetDeleteQuery(LineSep: String): String;
begin
  result := 'delete from '
          + DefineName
          + LineSep
          + WhereClause [LineSep]
          ;
end;

function TDefine.GetHasPrimaryKey: Boolean;
var
  x: Integer;
begin
  result := False;
  for x := 0 to Columns.Count - 1 do
  begin
    if Columns.Columns [x].KeySeqNumber > -1 then
    begin
      result := True;
      exit;
    end;
  end;
end;

function TDefine.GetInsertQuery(LineSep: String): String;
var
  Sep: String;
  qColumn: TColumn;
  x: Integer;
begin
  result := 'insert into ' + DefineName;
  Sep := '( ';
  for x := 0 to Columns.Count - 1 do
  begin
    qColumn := Columns.Columns [x];
    if (not qColumn.UseDefault)
    and (not qColumn.IsSysKey) then
    begin
      result := result + Sep + qColumn.ColName + LineSep;
      Sep := ', ';
    end;
  end;
  result := result + ') values';
  Sep := '( ';
  for x := 0 to Columns.Count - 1 do
  begin
    qColumn := Columns.Columns [x];
    if (not qColumn.UseDefault)
    and (not qColumn.IsSysKey) then
    begin
      Result := Result + Sep + qColumn.SqlValuePresentation (qColumn.Value) + LineSep;
      Sep := ', ';
    end;
  end;
  Result := Result + ')' + LineSep;
end;

function TDefine.GetSelectStarColumns: String;
var
  x: Integer;
  xHasSysKeyColumn: Boolean;
  xSeparator: String;
begin
  xHasSysKeyColumn := False;
  for x := 0 to Columns.Count - 1 do
    if Columns.Columns[x].IsSysKey then
      xHasSysKeyColumn := True;
  if xHasSysKeyColumn then
  begin
    result := '';
    xSeparator := '';
    for x := 0 to Columns.Count - 1 do
    begin
      result := result + xSeparator + Columns.Columns[x].ColName;
      xSeparator := ', ';
    end;
  end
  else
    result := '*';
end;

function TDefine.GetUpdateQuery(LineSep: String): String;
var
  qStrings: TStringList;
  Sep: String;
  qColumn: TColumn;
  x: Integer;
begin
  result := '';
  result := result + 'update ' + DefineName + LineSep;
  Sep := 'set ';
  for x := 0 to Columns.Count - 1 do
  begin
    qColumn := Columns.Columns [x];
    if (qColumn.IsUpdatable)
    and (qColumn.DoUpdate) then
    begin
      result := result
              + Sep
              + qColumn.ColName
              + ' = '
              + qColumn.SqlValuePresentation (qColumn.Value)
              ;
      Sep := '  , ';
    end;
  end;
  result := result + LineSep + WhereClause[LineSep];
end;

function TDefine.getWhereClause(LineSep: String): String;
var
  Sep: String;
  qColumn: TColumn;
  x: Integer;
begin
  result := '';
  Sep := 'where ';
  for x := 0 to Columns.Count - 1 do
  begin
    qColumn := Columns.Columns [x];
    if (not qColumn.IsUpdatable)
    or (not HasPrimaryKey) then
    begin
      if qColumn.OriginalUseNull then
        result := result
                + Sep
                + qColumn.ColName
                + ' is null'
      else
        result := result
                + Sep
                + qColumn.ColName
                + ' = '
                + qColumn.SqlValuePresentation (qColumn.OriginalValue)
                ;
      result := result + LineSep; {append newline}
      Sep := 'and ';
    end; {if to be included in where clause}
  end; {for each column}
end;

{ TColumn }

constructor TColumn.Create;
begin
  {Set some values in case server doesn't deliver}
  NullAllowed := False;
  DefaultClass := 'N';
  DefaultValue := '%Default value locally not known';
  PictureClause := 'Unknown';

  UseDefault := False;
  UseNull := False;
  DoUpdate := False;
end;

function TColumn.GetIsDefaultAllowed: Boolean;
begin
  result := (DefaultClass <> 'n')
        and (not IsSysKey);
end;

function TColumn.GetIsNullAllowed: Boolean;
begin
  result := NullAllowed;
end;

function TColumn.GetIsSysKey: Boolean;
begin
  result := (LowerCase (ColClass) = 's');
end;

function TColumn.GetIsUpdatable: Boolean;
begin
  result := (KeySeqNumber < 0)
        and (not IsSysKey)
          ;
end;

function TColumn.getNullableAsText: String;
begin
  if NullAllowed then
    result := 'Yes'
  else
    result := 'No';
end;

function TColumn.SqlValuePresentation (aValue: String): String;
  function _fValue (aValue: String): String;
  {decimalpoint is copied as a dot (.)}
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      if aValue [x] = DecimalSeparator then
        result := result + '.'
      else
        result := result + aValue [x];
    end;
  end;
  function _qValue (aValue: String): String;
  {single quotes are added twice}
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      result := result + aValue [x];
      if aValue [x] = '''' then
        result := result + '''';
    end;
  end;
begin
  if UseNull then
    result := 'Null'
  else
  begin
    if (   (Precision = 0)
        or (DateTimeQualifier <> '')
       )
    then
      Result := '''' + _qValue (aValue) + ''''
    else
    begin
      if aValue = '' then
        Result := '0'
      else
        Result := _fValue (aValue);
    end;
    if (DateTimeQualifier <> '') then
    begin
      if Uppercase (aValue) = 'CURRENT' then
        result := 'Current'
      else
      begin
        if DataType = 192 then
          Result := 'DateTime ' + Result + ' ' + DateTimeQualifier
        else
          Result := 'Interval ' + Result + ' ' + DateTimeQualifier;
      end;
    end;
  end; {not null}
end;

{ TSqlDataList }

function TSqlDataList.GetSqlData(Index: integer): TSqlData;
begin
  result := TSqlData (Objects [index]);
end;

{ TSqlRowList }

function TSqlRowList.GetSqlRow(Index: integer): TSqlRow;
begin
  result := TSqlRow (Objects [index]);
end;

{ TSqlQueryResult }

constructor TSqlQueryResult.Create;
begin
  inherited;
  SqlRows := TSqlRowList.Create;
end;

destructor TSqlQueryResult.Destroy;
begin
  SqlRows.Clear;
  SqlRows.Free;
  inherited;
end;

function TSqlQueryResult.GetSqlData(aCol, aRow: Integer): TSqlData;
var
  SqlRow: TSqlRow;
begin
  if aRow < 0 then
    raise Exception.Create ('SqlQueryResult.GetData: Row must be at least 0');
  if not (aRow < SqlRows.Count) then
    raise Exception.Create ('SqlQueryResult.GetData: Row out of range');
  SqlRow := SqlRows.SqlRows [aRow];
  if aCol < 0 then
    raise Exception.Create ('SqlQueryResult.GetData: Col must be at least 0');
  if not (aCol < SqlRow.SqlDatas.Count) then
    raise Exception.Create ('SqlQueryResult.GetData: Col out of range');
  result := SqlRow.SqlDatas.SqlDatas [aCol];
end;

{ TSqlRow }

constructor TSqlRow.Create;
begin
  inherited;
  SqlDatas := TSqlDataList.Create;
end;

destructor TSqlRow.Destroy;
begin
  SqlDatas.Clear;
  SqlDatas.Free;
  inherited;
end;

{ TSqlData }

destructor TSqlData.Destroy;
begin
  ShowMessage ('SqlData destroyed');
  inherited;
end;

initialization
  Defines := TDefineList.Create;
  Defines.Sorted := True;
finalization
  Defines.Clear;
  Defines.Free;

end.
