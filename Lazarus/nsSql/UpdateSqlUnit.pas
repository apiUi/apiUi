unit UpdateSqlUnit;

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

  { TUpdateSqlForm }

  TUpdateSqlForm = class(TForm)
    ActionList1: TActionList;
    Button2 : TButton ;
    CopyToClibBoardAction: TAction;
    mainImageList : TImageList ;
    ToolBar: TToolBar;
    GenerateAction: TAction;
    ToolButton1: TToolButton;
    DataGrid: TStringGrid;
    UpdateAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    SaveQueryAction: TAction;
    SqlSaveDialog: TSaveDialog;
    CheckAllForUpdateAction: TAction;
    DataGridPopupMenu: TPopupMenu;
    Checkallforupdate1: TMenuItem;
    UnCheckAllForUpdate: TAction;
    Uncheckallforupdate1: TMenuItem;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InitMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure GenerateActionExecute(Sender: TObject);
    procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DataGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DataGridKeyPress(Sender: TObject; var Key: Char);
    procedure FormHide(Sender: TObject);
    procedure UpdateActionExecute(Sender: TObject);
    procedure SaveQueryActionExecute(Sender: TObject);
    procedure GenerateActionUpdate(Sender: TObject);
    procedure SaveQueryActionUpdate(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckAllForUpdateActionExecute(Sender: TObject);
    procedure UnCheckAllForUpdateExecute(Sender: TObject);
  private
    IniFile: TFormIniFile;
    LineNumber: Integer;
    QueryRunning: Boolean;
    procedure ScreenEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SaveValues;
    procedure SaveValue (aRow: Integer);
  private
    FCheck, FNoCheck: TBitmap;
    procedure ToggleUseNull(acol, arow: Integer);
    procedure ToggleDoUpdate(acol, arow: Integer);
    procedure FillDataGridValue (aRow: Integer);
    procedure ShowScreen;
  public
    Viewer: String;
    Define: TDefine;
  end;

var
  UpdateSqlForm: TUpdateSqlForm;

implementation

{$R *.lfm}

uses ErrorFound
   , igGlobals
   ;

const DoUpdateColumn = 3;
const UseNullColumn = 4;
const ValueColumn = 5;

type
  TGridCracker = Class( TStringgrid );

procedure TUpdateSqlForm.ShowScreen;
var
  myRect: TGridRect;
  x: Integer;
  xColumn: TColumn;
begin
  DataGrid.RowCount := Define.Columns.Count + 1;
  DataGrid.ColWidths [0] := 4 + 20 * Canvas.TextWidth ('X');
  DataGrid.Cells [0, 0] := 'Column';
  DataGrid.ColWidths [0] := 4 + 20 * Canvas.TextWidth ('X');
  DataGrid.Cells [1, 0] := 'Prim';
  DataGrid.ColWidths [1] := 4 + Length (DataGrid.Cells [1, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [2, 0] := 'PictureClause';
  DataGrid.ColWidths [2] := 4 + Length (DataGrid.Cells [2, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [DoUpdateColumn, 0] := 'Update';
  DataGrid.ColWidths [DoUpdateColumn] := 4 + Length (DataGrid.Cells [DoUpdateColumn, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [UseNullColumn, 0] := 'UseNull';
  DataGrid.ColWidths [UseNullColumn] := 4 + Length (DataGrid.Cells [UseNullColumn, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [ValueColumn, 0] := 'Value';
  for x := 0 to Define.Columns.Count - 1 do
  begin
    DataGrid.Cells [0, x + 1] := Define.Columns.Columns [x].ColName;
    if Define.Columns.Columns [x].KeySeqNumber > -1 then
      DataGrid.Cells [1, x + 1] := IntToStr (Define.Columns.Columns [x].KeySeqNumber)
    else
      DataGrid.Cells [1, x + 1] := '';
    DataGrid.Cells [2, x + 1] := Define.Columns.Columns [x].PictureClause;
    FillDataGridValue(x + 1);
  end;
  myRect.Left := DataGrid.FixedCols;
  myRect.Top := DataGrid.FixedRows;
  myRect.Right := myRect.Left;
  myRect.Bottom := myRect.Top;
  DataGrid.Selection := myRect;
end;

procedure TUpdateSqlForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  DataGrid.ColWidths [ValueColumn]
    := IniFile.IntegerByNameDef['ValueColumnWidth', DataGrid.ColWidths [ValueColumn]];
  FCheck:= TBitmap.Create;
  FNoCheck:= TBitmap.Create;
  mainImageList.GetBitmap(90, FCheck);
  mainImageList.GetBitmap(91, FNoCheck);
end;

procedure TUpdateSqlForm.FormDestroy(Sender: TObject);
begin
  IniFile.IntegerByName[ 'ValueColumnWidth'] := DataGrid.ColWidths [ValueColumn];
  IniFile.Save;
  IniFile.Free;
  FNoCheck.Free;
  FCheck.Free;
end;

procedure TUpdateSqlForm.ScreenEditMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  xColumn: TColumn;
begin
end;

procedure TUpdateSqlForm.InitMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
{  ScreenBar.SimpleText := ''; }
end;

procedure TUpdateSqlForm.FormShow(Sender: TObject);
var
  CanSelect: Boolean;
begin
  ShowScreen;
  if Assigned (DataGrid.OnSelectCell) then
    DataGrid.OnSelectCell (nil, DataGrid.Col, DataGrid.Row, CanSelect);
end;

procedure TUpdateSqlForm.GenerateActionExecute(Sender: TObject);
var
  x: Integer;
  Sep: String;
  QuoteIt: String;
begin
  SaveValues;
  if True then
  begin
    ShowMessage (Define.UpdateQuery[LineEnding]);
  end;
end;

procedure TUpdateSqlForm.ToggleUseNull(acol, arow: Integer);
begin
  if (Define.Columns.Columns [aRow - 1].IsUpdatable)
  and Define.Columns.Columns [aRow - 1].IsNullAllowed then
  begin
    Define.Columns.Columns [aRow - 1].UseNull
      := not Define.Columns.Columns [aRow - 1].UseNull;
    if Define.Columns.Columns [aRow - 1].UseNull then
      Define.Columns.Columns [aRow - 1].Value := DataGrid.Cells [ValueColumn, aRow];
  end;
  FillDataGridValue (aRow);
  TGridCracker (DataGrid).InvalidateCell ( aCol, aRow );
  TGridCracker (DataGrid).InvalidateCell ( ValueColumn, aRow );
end;

procedure TUpdateSqlForm.ToggleDoUpdate(acol, arow: Integer);
begin
  if (Define.Columns.Columns [aRow - 1].IsUpdatable) then
  begin
    Define.Columns.Columns [aRow - 1].DoUpdate
      := not Define.Columns.Columns [aRow - 1].DoUpdate;
    if Define.Columns.Columns [aRow - 1].DoUpdate then
      Define.Columns.Columns [aRow - 1].Value := DataGrid.Cells [ValueColumn, aRow];
  end;
  FillDataGridValue (aRow);
  TGridCracker (DataGrid).InvalidateCell ( aCol, aRow );
  TGridCracker (DataGrid).InvalidateCell ( UseNullColumn, aRow );
  TGridCracker (DataGrid).InvalidateCell ( ValueColumn, aRow );
end;

procedure TUpdateSqlForm.DataGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  grid: TStringgrid;
  xDrawCheckBox: Boolean;
  xBoolean: Boolean;
begin
  if aRow < DataGrid.FixedRows then
    exit;
  grid:= Sender As TStringgrid;
  xDrawCheckBox := False;
  case aCol of
    DoUpdateColumn:
      begin
        xDrawCheckBox := (Define.Columns.Columns [aRow - 1].IsUpdatable);
        xBoolean := Define.Columns.Columns [aRow - 1].DoUpdate;
      end;
    UseNullColumn:
      begin
        xDrawCheckBox := (Define.Columns.Columns [aRow - 1].IsNullAllowed)
                     and (Define.Columns.Columns [aRow - 1].DoUpdate);
        xBoolean := Define.Columns.Columns [aRow - 1].UseNull;
      end;
    ValueColumn:
      begin
        if (not Define.Columns.Columns [aRow - 1].IsUpdatable)
        or (Define.Columns.Columns [aRow - 1].UseNull)
        or (not Define.Columns.Columns [aRow - 1].DoUpdate)
        then begin
          grid.Canvas.Brush.Color := Color;
          grid.Canvas.FillRect( rect );
          grid.Canvas.TextRect
            ( Rect
            , Rect.Left + grid.Canvas.TextWidth ('l')
            , Rect.Top + (Rect.Bottom - Rect.Top - grid.Canvas.TextHeight('X')) div 2
            , DataGrid.Cells [aCol, aRow]
            );
        end;
      end;
  end;
  if xDrawCheckBox then
  begin
    With grid.Canvas Do
    Begin
      if not (gdFixed in State) then
      begin
        brush.color := $E0E0E0;
        Fillrect( rect );
      end;
      If xBoolean Then
        Draw( (rect.right + rect.left - FCheck.width) div 2,
              (rect.bottom + rect.top - FCheck.height) div 2,
              FCheck )
      Else
        Draw( (rect.right + rect.left - FNoCheck.width) div 2,
              (rect.bottom + rect.top - FNoCheck.height) div 2,
              FNoCheck )
    End;
  End;
end;

procedure TUpdateSqlForm.DataGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (aRow >= DataGrid.FixedRows)
  and (aCol = ValueColumn)
  and Assigned (Define)
  and (Define.Columns.Columns [aRow - 1].IsUpdatable)
  and (not Define.Columns.Columns [aRow - 1].UseNull)
  and (Define.Columns.Columns [aRow - 1].DoUpdate)
  then
    DataGrid.Options := DataGrid.Options + [ goEditing ]
  else
    DataGrid.Options := DataGrid.Options - [ goEditing ];
end;

procedure TUpdateSqlForm.DataGridKeyPress(Sender: TObject; var Key: Char);
begin
{
  If (Key = VK_RETURN) Then
    With Sender As Tstringgrid Do
      if (Col = DataGrid.ColCount - 2)
      and (Row > 0)
      Then Begin
        ToggleUseDefault( Col, row );
        Key := #0;
      End;
}
  if (Define.Columns.Columns [DataGrid.Row - 1].Precision > 0)
  and (Define.Columns.Columns [DataGrid.Row - 1].DateTimeQualifier = '')
  then begin {only accept numeric key or controlkey}
    if not (key in [ '0'..'9', #1..#26 , '.', '-']) then
      Key := #0;
  end;
end;

procedure TUpdateSqlForm.FormHide(Sender: TObject);
begin
  SaveValues;
end;

procedure TUpdateSqlForm.SaveValue(aRow: Integer);
begin
  if (not Define.Columns.Columns [aRow - 1].UseNull)
  then
    Define.Columns.Columns [aRow - 1].Value := DataGrid.Cells [ValueColumn, aRow];
end;

procedure TUpdateSqlForm.SaveValues;
var
  xRow: Integer;
begin
  for xRow := 1 to Define.Columns.Count do
  begin
    SaveValue(xRow);
  end;
end;

procedure TUpdateSqlForm.SaveQueryActionExecute(Sender: TObject);
begin
  SqlSaveDialog.Title := 'Save SQL query to file';
  SqlSaveDialog.DefaultExt := 'SQL';
  SqlSaveDialog.Filter := 'SQL File (*.SQL)|*.SQL';
  if SqlSaveDialog.Execute = True then
  begin
    SaveStringToFile (SqlSaveDialog.FileName, Define.UpdateQuery [LineEnding]);
  end;
end;

procedure TUpdateSqlForm.UpdateActionExecute(Sender: TObject);
begin
  SaveValues;
  ModalResult := mrOk;
end;

procedure TUpdateSqlForm.GenerateActionUpdate(Sender: TObject);
begin
  GenerateAction.Enabled := not QueryRunning;
end;

procedure TUpdateSqlForm.SaveQueryActionUpdate(Sender: TObject);
begin
  SaveQueryAction.Enabled := not QueryRunning;
end;

procedure TUpdateSqlForm.FillDataGridValue(aRow: Integer);
begin
  if (Define.Columns.Columns [aRow - 1].DoUpdate) then
  begin
    if (Define.Columns.Columns [aRow - 1].UseNull)
    and (Define.Columns.Columns [aRow - 1].NullAllowed) then
      DataGrid.Cells [ValueColumn, aRow] := 'Null'
    else
      DataGrid.Cells [ValueColumn, aRow] :=
        Define.Columns.Columns [aRow - 1].Value;
  end
  else
  begin
    DataGrid.Cells [ValueColumn, aRow] :=
      Define.Columns.Columns [aRow - 1].OriginalValue;
  end;
end;

procedure TUpdateSqlForm.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Column: Integer;
  Row: Integer;
begin
  DataGrid.MouseToCell(X, Y, Column, Row);
  if Row < DataGrid.FixedRows then
    exit;
  case Column of
    DoUpdateColumn:
      Begin
        DataGrid.Row := Row;
        ToggleDoUpdate(Column, Row);
      end;
    UseNullColumn:
      Begin
        DataGrid.Row := Row;
        ToggleUseNull(Column, Row);
      end;
  end;
end;

procedure TUpdateSqlForm.CheckAllForUpdateActionExecute(Sender: TObject);
var
  x: Integer;
begin
  for x := 0 to Define.Columns.Count - 1 do
  begin
    if (Define.Columns.Columns [x].IsUpdatable) then
    begin
      Define.Columns.Columns [x].DoUpdate := True;
    end;
    FillDataGridValue (x + 1);
  end;
  DataGrid.Invalidate;
end;

procedure TUpdateSqlForm.UnCheckAllForUpdateExecute(Sender: TObject);
var
  x: Integer;
begin
  for x := 0 to Define.Columns.Count - 1 do
  begin
    if (Define.Columns.Columns [x].IsUpdatable) then
    begin
      Define.Columns.Columns [x].DoUpdate := False;
    end;
    FillDataGridValue (x + 1);
  end;
  DataGrid.Invalidate;
end;

end.
