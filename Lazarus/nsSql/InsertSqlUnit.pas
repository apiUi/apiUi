unit InsertSqlUnit;

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
  TInsertSqlForm = class(TForm)
    ActionList1: TActionList;
    CopyToClibBoardAction: TAction;
    ToolBar: TToolBar;
    GenerateAction: TAction;
    ToolButton1: TToolButton;
    DataGrid: TStringGrid;
    InsertAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    SaveQueryAction: TAction;
    SqlSaveDialog: TSaveDialog;
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
    procedure InsertActionExecute(Sender: TObject);
    procedure SaveQueryActionExecute(Sender: TObject);
    procedure GenerateActionUpdate(Sender: TObject);
    procedure SaveQueryActionUpdate(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    IniFile: TFormIniFile;
    QueryRunning: Boolean;
    procedure SaveValues;
    procedure SaveValue (aRow: Integer);
  private
    FCheck, FNoCheck: TBitmap;
    procedure ToggleUseDefault(acol, arow: Integer);
    procedure ToggleUseNull(acol, arow: Integer);
    procedure FillDataGridValue (aRow: Integer);
    procedure ShowScreen;
  public
    Viewer: String;
    Define: TDefine;
  end;

var
  InsertSqlForm: TInsertSqlForm;

implementation

{$R *.lfm}

uses ErrorFound
   , igGlobals
   ;

const UseDefaultColumn = 3;
const UseNullColumn = 4;
const ValueColumn = 5;

type
  TGridCracker = Class( TStringgrid );

procedure TInsertSqlForm.ShowScreen;
var
  x: Integer;
begin
  DataGrid.RowCount := Define.Columns.Count + 1;
  DataGrid.ColWidths [0] := 4 + 20 * Canvas.TextWidth ('X');
  DataGrid.Cells [0, 0] := 'Column';
  DataGrid.ColWidths [0] := 4 + 20 * Canvas.TextWidth ('X');
  DataGrid.Cells [1, 0] := 'Prim';
  DataGrid.ColWidths [1] := 4 + Length (DataGrid.Cells [1, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [2, 0] := 'PictureClause';
  DataGrid.ColWidths [2] := 4 + Length (DataGrid.Cells [2, 0]) * Canvas.TextWidth ('X');
  DataGrid.Cells [UseDefaultColumn, 0] := 'UseDflt';
  DataGrid.ColWidths [UseDefaultColumn] := 4 + Length (DataGrid.Cells [UseDefaultColumn, 0]) * Canvas.TextWidth ('X');
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
end;

procedure TInsertSqlForm.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  FCheck:= TBitmap.Create;
  FNoCheck:= TBitmap.Create;
  bmp:= TBitmap.create;
  {
  try
    bmp.handle := LoadBitmap( 0, PChar(OBM_CHECKBOXES ));
    // bmp now has a 4x3 bitmap of divers state images
    // used by checkboxes and radiobuttons
    With FNoCheck Do Begin
      // the first subimage is the unchecked box
      width := bmp.width div 4;
      height := bmp.height div 3;
      canvas.copyrect
      ( canvas.cliprect
      , bmp.canvas
      , canvas.cliprect
      );
    End;
    With FCheck Do Begin
      // the second subimage is the checked box
      width := bmp.width div 4;
      height := bmp.height div 3;
      canvas.copyrect
      ( canvas.cliprect
      , bmp.canvas
      , rect( width, 0, 2*width, height )
      );
    End;
  finally
    bmp.free
  end;
  }
end;

procedure TInsertSqlForm.FormDestroy(Sender: TObject);
begin
  IniFile.WriteInteger ( 'ScreenForm'
                       , 'ValueWidth'
                       , DataGrid.ColWidths [ValueColumn]
                       );
  IniFile.Save;
  IniFile.Free;
  FNoCheck.Free;
  FCheck.Free;
end;

procedure TInsertSqlForm.InitMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
{  ScreenBar.SimpleText := ''; }
end;

procedure TInsertSqlForm.FormShow(Sender: TObject);
var
  CanSelect: Boolean;
begin
  ShowScreen;
  if Assigned (DataGrid.OnSelectCell) then
    DataGrid.OnSelectCell (nil, DataGrid.Col, DataGrid.Row, CanSelect);
end;

procedure TInsertSqlForm.GenerateActionExecute(Sender: TObject);
begin
  SaveValues;
  if True then {if generate Insert}
  begin
    Application.CreateForm(TipmInfoForm, ipmInfoForm);
    try
      ipmInfoForm.Caption := 'IpmGun - Generated SQL insert statement';
      ipmInfoForm.Memo.Lines.Text := Define.InsertQuery;
      ipmInfoForm.ShowModal;
    finally
      FreeAndNil (ipmInfoForm);
    end;
  end;
end;

procedure TInsertSqlForm.ToggleUseNull(acol, arow: Integer);
begin
  if (not Define.Columns.Columns [aRow - 1].UseDefault)
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

procedure TInsertSqlForm.ToggleUseDefault(acol, arow: Integer);
begin
  if Define.Columns.Columns [aRow - 1].IsDefaultAllowed then
    Define.Columns.Columns [aRow - 1].UseDefault
      := not Define.Columns.Columns [aRow - 1].UseDefault;
  if Define.Columns.Columns [aRow - 1].UseDefault
  and (not Define.Columns.Columns [aRow - 1].UseNull)
  then
    Define.Columns.Columns [aRow - 1].Value := DataGrid.Cells [ValueColumn, aRow];
  FillDataGridValue (aRow);
  TGridCracker (DataGrid).InvalidateCell ( UseDefaultColumn, aRow );
  TGridCracker (DataGrid).InvalidateCell ( UseNullColumn, aRow );
  TGridCracker (DataGrid).InvalidateCell ( ValueColumn, aRow );
end;

procedure TInsertSqlForm.DataGridDrawCell(Sender: TObject; ACol,
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
    UseDefaultColumn:
      begin
        xDrawCheckBox := Define.Columns.Columns [aRow - 1].IsDefaultAllowed;
        xBoolean := Define.Columns.Columns [aRow - 1].UseDefault;
      end;
    UseNullColumn:
      begin
        xDrawCheckBox := (not Define.Columns.Columns [aRow - 1].UseDefault)
                     and (Define.Columns.Columns [aRow - 1].IsNullAllowed);
        xBoolean := Define.Columns.Columns [aRow - 1].UseNull;
      end;
    ValueColumn:
      begin
        if (Define.Columns.Columns [aRow - 1].UseDefault)
        or (Define.Columns.Columns [aRow - 1].UseNull)
        or (UpperCase (Define.Columns.Columns [aRow - 1].ColClass) = 'S')
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

procedure TInsertSqlForm.DataGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (aCol = ValueColumn)
  and (not Define.Columns.Columns [aRow - 1].UseDefault)
  and (not Define.Columns.Columns [aRow - 1].UseNull)
  and (UpperCase (Define.Columns.Columns [aRow - 1].ColClass) <> 'S')
  then
    DataGrid.Options := DataGrid.Options + [ goEditing ]
  else
    DataGrid.Options := DataGrid.Options - [ goEditing ];
end;

procedure TInsertSqlForm.DataGridKeyPress(Sender: TObject; var Key: Char);
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

procedure TInsertSqlForm.FormHide(Sender: TObject);
begin
  SaveValues;
end;

procedure TInsertSqlForm.SaveValue(aRow: Integer);
begin
  if (not Define.Columns.Columns [aRow - 1].UseDefault)
  and (not Define.Columns.Columns [aRow - 1].UseNull)
  then
    Define.Columns.Columns [aRow - 1].Value := DataGrid.Cells [ValueColumn, aRow];
end;

procedure TInsertSqlForm.SaveValues;
var
  xRow: Integer;
begin
  for xRow := 1 to Define.Columns.Count do
  begin
    SaveValue(xRow);
  end;
end;

procedure TInsertSqlForm.SaveQueryActionExecute(Sender: TObject);
begin
  SqlSaveDialog.Title := 'Save SQL query to file';
  SqlSaveDialog.DefaultExt := 'SQL';
  SqlSaveDialog.Filter := 'SQL File (*.SQL)|*.SQL';
  SqlSaveDialog.FileName := ConsoleForm.SqlFileName;
  if SqlSaveDialog.Execute = True then
  begin
    ConsoleForm.SqlFileName := SqlSaveDialog.FileName;
    SaveStringToFile (SqlSaveDialog.FileName, Define.InsertQuery);
  end;
end;

procedure TInsertSqlForm.InsertActionExecute(Sender: TObject);
begin
  QueryRunning := True;
  InsertAction.Enabled := False;
  DataGrid.Enabled := False;
  SaveValues;
  ConsoleForm.BooleanDoSqlQuery (Define.InsertQuery);
  ToolBar.Invalidate;
  ConsoleForm.WaitUntilEndOfDialog;
  InsertAction.Enabled := True;
  DataGrid.Enabled := True;
  QueryRunning := False;
  if ConsoleForm.SqlSuccess then
    ConsoleForm.InsertDataGridRow;
  DataGrid.SetFocus;
end;

procedure TInsertSqlForm.GenerateActionUpdate(Sender: TObject);
begin
  GenerateAction.Enabled := not QueryRunning;
end;

procedure TInsertSqlForm.SaveQueryActionUpdate(Sender: TObject);
begin
  SaveQueryAction.Enabled := not QueryRunning;
end;

procedure TInsertSqlForm.FillDataGridValue(aRow: Integer);
begin
  if Define.Columns.Columns [aRow - 1].UseDefault then
    DataGrid.Cells [ValueColumn, aRow] :=
      Define.Columns.Columns [aRow - 1].DefaultValue
  else
  begin
    if Define.Columns.Columns [aRow - 1].UseNull then
      DataGrid.Cells [ValueColumn, aRow] := 'Null'
    else
    begin
      if UpperCase (Define.Columns.Columns [aRow - 1].ColClass) = 'S' then
        DataGrid.Cells [ValueColumn, aRow] := 'Syskey'
      else
        DataGrid.Cells [ValueColumn, aRow] :=
          Define.Columns.Columns [aRow - 1].Value;
    end;
  end;
end;

procedure TInsertSqlForm.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Column: Integer;
  Row: Integer;
begin
  DataGrid.MouseToCell(X, Y, Column, Row);
  if Row < DataGrid.FixedRows then
    exit;
  case Column of
    UseDefaultColumn:
      Begin
        DataGrid.Row := Row;
        ToggleUseDefault(Column, Row);
      end;
    UseNullColumn:
      Begin
        DataGrid.Row := Row;
        ToggleUseNull(Column, Row);
      end;
  end;
end;

end.
