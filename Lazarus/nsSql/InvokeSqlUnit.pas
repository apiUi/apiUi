unit InvokeSqlUnit;

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
  TInvokeSqlForm = class(TForm)
    DataGrid: TStringGrid;
    Panel2: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InitMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    IniFile: TFormIniFile;
    LineNumber: Integer;
    QueryRunning: Boolean;
    procedure ScreenEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ShowScreen;
  public
    Viewer: String;
    Define: TDefine;
  end;

var
  InvokeSqlForm: TInvokeSqlForm;

implementation

{$R *.lfm}

uses ErrorFound
   , igGlobals
   ;

const UseDefaultColumn = 4;
const UseNullColumn = 5;
const ValueColumn = 6;

type
  TGridCracker = Class( TStringgrid );

procedure TInvokeSqlForm.ShowScreen;
var
  x: Integer;
  xColumn: TColumn;
begin
  Caption := 'Table/View: '
           + Define.DefineName
           + ' ('
           + Define.FileName
           + ')';
  DataGrid.RowCount := Define.Columns.Count + 1;
  DataGrid.Cells [0, 0] := 'ColNr';
  DataGrid.Cells [1, 0] := 'Column';
  DataGrid.Cells [2, 0] := 'Prim';
  DataGrid.Cells [3, 0] := 'PictureClause';
  DataGrid.Cells [4, 0] := 'Default';
  DataGrid.Cells [5, 0] := 'Nullable';
  DataGrid.Cells [6, 0] := 'ColClass';
  for x := 0 to Define.Columns.Count - 1 do
  begin
    DataGrid.Cells [0, x + 1] := IntToStr (x + 1);
    DataGrid.Cells [1, x + 1] := Define.Columns.Columns [x].ColName;
    if Define.Columns.Columns [x].KeySeqNumber > -1 then
      DataGrid.Cells [2, x + 1] := IntToStr (Define.Columns.Columns [x].KeySeqNumber)
    else
      DataGrid.Cells [2, x + 1] := '';
    DataGrid.Cells [3, x + 1] := Define.Columns.Columns [x].PictureClause;
    DataGrid.Cells [4, x + 1] := Define.Columns.Columns [x].DefaultValue;
    DataGrid.Cells [5, x + 1] := Define.Columns.Columns [x].NullableAsText;
    DataGrid.Cells [6, x + 1] := Define.Columns.Columns [x].ColClass;
  end;
end;

procedure TInvokeSqlForm.FormCreate(Sender: TObject);
  procedure _GetColumnWidths (aKey: String);
  var
    x: Integer;
  begin
    for x := 0 to DataGrid.ColCount - 1 do
      DataGrid.ColWidths [x] :=
        IniFile.IntegerByNameDef [ aKey + 'Width0' + IntToStr (x + 1)
                                 , DataGrid.ColWidths [x]
                                 ];

  end;
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  _GetColumnWidths ('InfoColunn');
end;

procedure TInvokeSqlForm.FormDestroy(Sender: TObject);
  procedure _SaveColumnWidths (aKey: String);
  var
    x: Integer;
  begin
    for x := 0 to DataGrid.ColCount - 1 do
      IniFile.IntegerByName[aKey + 'Width0' + IntToStr (x + 1)] := DataGrid.ColWidths [x];
  end;
begin
  _SaveColumnWidths ('InfoColunn');
  IniFile.Save;
  IniFile.Free;
end;

procedure TInvokeSqlForm.ScreenEditMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  xColumn: TColumn;
begin
end;

procedure TInvokeSqlForm.InitMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
{  ScreenBar.SimpleText := ''; }
end;

procedure TInvokeSqlForm.FormShow(Sender: TObject);
var
  CanSelect: Boolean;
begin
  ShowScreen;
  if Assigned (DataGrid.OnSelectCell) then
    DataGrid.OnSelectCell (nil, DataGrid.Col, DataGrid.Row, CanSelect);
end;

procedure TInvokeSqlForm.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Column: Integer;
  Row: Integer;
begin
  DataGrid.MouseToCell(X, Y, Column, Row);
  if Row < DataGrid.FixedRows then
    exit;
end;

procedure TInvokeSqlForm.DataGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  myRect: TGridRect;
  xGrid: TStringGrid;
begin
  xGrid := Sender as TStringGrid;
  if (Key = ord ('A'))
  and (Shift = [ssCtrl])
  then
  begin
    myRect.Left := xGrid.FixedCols;
    myRect.Top := xGrid.FixedRows;
    myRect.Right := xGrid.ColCount - 1;
    myRect.Bottom := xGrid.RowCount - 1;
    xGrid.Selection := myRect;
  end;
  if (Key = ord ('C'))
  and (Shift = [ssCtrl])
  then
  begin
    GridCopyMatrix (xGrid, '', True);
  end;
end;

end.
