{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit XpQuery;

interface

uses Classes, Bind, sqldb, mssqlconn, {$ifndef win64}oracleconnection, {$endif}odbcconn, SQLite3Conn, xmlio
  ;

type TXpQueryVerb = (xqvSelect, xqvInsert, xqvUpdate, xqvDelete);

type TXpQuery = class (TSqlQuery)
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
  UsePrimaryKeyAsKey := False;
  ParseSQL := False;
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
  if not DataBase.Connected then
    DataBase.Connected := True;
  SQL.Text := SqlStrings.Text;
  Prepare;
end;

end.
