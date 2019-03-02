unit StringListListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Xmlz;

type

  { TStringListList }

  TStringListList = class (TStringList)
  private
    fColCount: Integer;
    fRowCount: Integer;
    function getCell(aCol, aRow: String): String;
    function getCellObject(aCol, aRow: Integer): TObject;
    procedure setCellObject(aCol, aRow: Integer; AValue: TObject);
    procedure setRowText(Index: integer; AValue: String);
    procedure SetStringList(Index: integer; const Value: TStringList);
    procedure setColCount(const Value: Integer);
    procedure setRowCount(const Value: Integer);
    function getRowText(Index: integer): String;
    function getCellValue(aCol, aRow: Integer): String;
    procedure setCellValue(aCol, aRow: Integer; const Value: String);
  protected
    function GetStringList (Index: integer): TStringList;
  public
    property RowCount: Integer read fRowCount write setRowCount;
    property ColCount: Integer read fColCount write setColCount;
    property CellValue [aCol, aRow: Integer]: String read getCellValue write setCellValue;
    property CellObject [aCol, aRow: Integer]: TObject read getCellObject write setCellObject;
    property Cell [aCol, aRow: String]: String read getCell;
    property RowText [Index: integer]: String read getRowText write setRowText;
    property StringLists [Index: integer]: TStringList read GetStringList write SetStringList;
    function AsXml: TXml;
    procedure FromXml (aXml: TXml);
    procedure CopyFrom (aGrid: TStringListList);
    constructor Create (aGrid: TStringListList); Overload;
  end
  ;


implementation

uses math
   ;
{ TStringListList }

function TStringListList.getCellValue(aCol, aRow: Integer): String;
begin
  result := StringLists [aRow].Strings [aCol];
end;

function TStringListList.getRowText(Index: integer): String;
begin
  result := StringLists[Index].Text;
end;

function TStringListList.GetStringList(Index: integer): TStringList;
begin
  try
    result := Objects [Index] as TStringList;
  except
    result := nil;
  end;
end;

function TStringListList.AsXml: TXml;
var
  r, c: Integer;
begin
  result := TXml.CreateAsString('grid', '');
  with result do
  begin
    for r := 0 to RowCount - 1 do
    with AddXml(TXml.CreateAsString('row','')) do
    begin
      for c := 0 to ColCount - 1 do
        with AddXml(TXml.CreateAsString('col', CellValue[c, r])) do
          if Assigned (CellObject[c, r]) then
            AddAttribute(TXmlAttribute.CreateAsInteger('attributes', QWord(CellObject[c, r])));
    end;
  end;
end;

procedure TStringListList.FromXml(aXml: TXml);
var
  r, c, mc: Integer;
  qw: QWord;
begin
  if not Assigned (aXml) then
    raise Exception.Create('procedure TStringListList.FromXml(aXml: TXml): nil arg');
  mc := 0;
  for r := 0 to aXml.Items.Count - 1 do
    with aXml.Items.XmlItems[r] do
      mc := max (mc, Items.Count);
  RowCount := aXml.Items.Count;
  ColCount := mc;
  for r := 0 to aXml.Items.Count - 1 do
    with aXml.Items.XmlItems[r] do
      for c := 0 to Items.Count - 1 do
      begin
        CellValue[c, r] := Items.XmlItems[c].Value;
        qw := StrToInt64Def(Items.XmlItems[c].AttributeValueByTagDef['attributes', '0'], 0);
        CellObject[c, r] := TObject (qw);
      end;
end;

procedure TStringListList.CopyFrom(aGrid: TStringListList);
var
  c, r: Integer;
begin
  RowCount := aGrid.RowCount;
  ColCount := aGrid.ColCount;
  for r := 0 to RowCount - 1 do
    for c := 0 to ColCount - 1 do
    begin
      CellValue [c, r] := aGrid.CellValue [c, r];
      CellObject [c, r] := aGrid.CellObject [c, r];
    end;
end;

constructor TStringListList.Create(aGrid: TStringListList);
begin
  CopyFrom(aGrid);
end;

procedure TStringListList.setCellValue(aCol, aRow: Integer;
  const Value: String);
begin
  StringLists [aRow].Strings [aCol] := Value;
end;

procedure TStringListList.setColCount(const Value: Integer);
var
  r, c: Integer;
  sl: TStringList;
begin
  for r := 0 to fRowCount - 1 do
  begin
    sl := StringLists [r];
    c := min (Value, fColCount);
    while (c < fColCount) do
    begin
      sl.Delete (Value);
      Inc (c);
    end;
    while (c < Value) do
    begin
      sl.Add ('');
      Inc (c);
    end;
  end;
  fColCount := Value;
end;

procedure TStringListList.setRowCount(const Value: Integer);
var
  r, c: Integer;
  sl: TStringList;
begin
  r := min (Value, fRowCount);
  while (r < fRowCount) do
  begin
    StringLists [Value].Free;
    Delete(Value);
    Inc (r);
  end;
  while (r < Value) do
  begin
    sl := TStringList.Create;
    for c := 0 to fColCount do
      sl.Add ('');
    AddObject('', sl);
    Inc (r);
  end;
  fRowCount := Value;
end;

procedure TStringListList.SetStringList(Index: integer;
  const Value: TStringList);
begin

end;

procedure TStringListList.setRowText(Index: integer; AValue: String);
begin
  StringLists[Index].Text := AValue;
end;

function TStringListList.getCell(aCol, aRow: String): String;
var
  r, rr, c, cc: Integer;
begin
  result := '';
  rr := -1;
  r := 0;
  while (r < RowCount) and (rr = -1) do
  begin
    if CellValue[0, r] = aRow then
      rr := r;
    Inc (r);
  end;
  if rr = -1 then
    raise Exception.Create('Row not found: ' + aRow);
  cc := -1;
  c := 0;
  while (c < ColCount) and (cc = -1) do
  begin
    if CellValue[c, 0] = aCol then
      cc := c;
    Inc (c);
  end;
  if cc = -1 then
    raise Exception.Create('Column not found: ' + aCol);
  result := CellValue[cc, rr];
end;

function TStringListList.getCellObject(aCol, aRow: Integer): TObject;
begin
  result := StringLists [aRow].Objects [aCol];
end;

procedure TStringListList.setCellObject(aCol, aRow: Integer; AValue: TObject);
begin
  StringLists [aRow].Objects [aCol] := AValue;
end;


end.

