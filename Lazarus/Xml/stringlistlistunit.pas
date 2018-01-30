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
    property RowText [Index: integer]: String read getRowText write setRowText;
    property StringLists [Index: integer]: TStringList read GetStringList write SetStringList;
    function AsXml: TXml;
    procedure FromXml (aXml: TXml);
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
        AddXml(TXml.CreateAsString('col', CellValue[c, r]));
    end;
  end;
end;

procedure TStringListList.FromXml(aXml: TXml);
var
  r, c, mc: Integer;
begin
  if not Assigned (aXml) then Exit;
  mc := 0;
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


end.

