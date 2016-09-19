unit ViewSqlRowUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ComCtrls, ToolWin, StdCtrls, ExtCtrls
  , FormIniFilez
  , Definez
  , Menus
  , Grids, DBCtrls
  ;

type
  TViewSqlRowForm = class(TForm)
    ActionList1: TActionList;
    CopyToClibBoardAction: TAction;
    ToolBar: TToolBar;
    DataGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InitMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    IniFile: TFormIniFile;
    LineNumber: Integer;
    QueryRunning: Boolean;
    procedure ScreenEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ShowScreen;
  public
    Viewer: String;
    Headers: TStrings;
    Values: TStrings;
    StartWithColumn: Integer;
  end;

var
  ViewSqlRowForm: TViewSqlRowForm;

implementation

{$R *.lfm}

uses ErrorFound
   , igGlobals
   ;

const ValueColumn = 1;

type
  TGridCracker = Class( TStringgrid );

procedure TViewSqlRowForm.ShowScreen;
var
  x: Integer;
  xColumn: TColumn;
begin
  DataGrid.RowCount := Headers.Count + 1 - StartWithColumn;
  DataGrid.ColWidths [0] := 4 + 20 * Canvas.TextWidth ('X');
  DataGrid.Cells [0, 0] := 'Column';
  DataGrid.Cells [ValueColumn, 0] := 'Value';
  for x := 0 to Headers.Count - 1 do
  begin
    DataGrid.Cells [0, x + 1] := Headers.Strings [x + StartWithColumn];
    DataGrid.Cells [1, x + 1] := Values.Strings [x + StartWithColumn];
  end;
end;

procedure TViewSqlRowForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  DataGrid.ColWidths [ValueColumn] := IniFile.IntegerByNameDef ['ValueColumnWidth', DataGrid.ColWidths [ValueColumn]];
end;

procedure TViewSqlRowForm.FormDestroy(Sender: TObject);
begin
  IniFile.IntegerByName ['ValueColumnWidth']:= DataGrid.ColWidths [ValueColumn];
  IniFile.Save;
  IniFile.Free;
end;

procedure TViewSqlRowForm.ScreenEditMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  xColumn: TColumn;
begin
end;

procedure TViewSqlRowForm.InitMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
{  ScreenBar.SimpleText := ''; }
end;

procedure TViewSqlRowForm.FormShow(Sender: TObject);
var
  CanSelect: Boolean;
begin
  ShowScreen;
  if Assigned (DataGrid.OnSelectCell) then
    DataGrid.OnSelectCell (nil, DataGrid.Col, DataGrid.Row, CanSelect);
end;

end.
