{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit EditContextsUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  StdCtrls, ActnList, ExtCtrls, Buttons, ComCtrls, FormIniFilez,
  StringListListUnit, xmlio, VirtualTrees;
type

  PGridTreeRec = ^TGridTreeRec;

  TGridTreeRec = record
    Row: Integer;
  end;

  { TEditContextsForm }

  TEditContextsForm = class(TForm)
    GridView: TVirtualStringTree;
    Label1: TLabel;
    mainImageList1: TImageList;
    MenuItem6: TMenuItem;
    SetAsOnetimerAction: TAction;
    MenuItem5: TMenuItem;
    SetPasswordAction: TAction;
    CancelButton: TBitBtn;
    ContextComboBox: TComboBox;
    mainImageList: TImageList;
    OkButton: TBitBtn;
    Panel2: TPanel;
    RemovePropertyAction: TAction;
    RemoveContextAction: TAction;
    AddPropertyAction: TAction;
    AddContextAction: TAction;
    ActionList1: TActionList;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure AddContextActionExecute(Sender: TObject);
    procedure AddPropertyActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridViewAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure GridViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure GridViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure GridViewCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure GridViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure GridViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure GridViewHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure GridViewHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure GridViewHeaderImageClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GridViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure GridViewNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure GridViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure RemoveContextActionExecute(Sender: TObject);
    procedure RemoveContextActionUpdate(Sender: TObject);
    procedure RemovePropertyActionExecute(Sender: TObject);
    procedure RemovePropertyActionUpdate(Sender: TObject);
    procedure SetAsOnetimerActionExecute(Sender: TObject);
    procedure SetAsOnetimerActionUpdate(Sender: TObject);
    procedure SetPasswordActionExecute(Sender: TObject);
    procedure SetPasswordActionUpdate(Sender: TObject);
  private
    ColWidths: TJBStringList;
    procedure GetColAndRow(var aCol, aRow: Integer);
    function isPassWordColumn (aColumn: Integer): Boolean;
    function isOneTimerColumn (aColumn: Integer): Boolean;
    function BooleanPromptDialog (aCaption: String): Boolean;
    procedure PopulateContextComboBox;
    procedure SetColumnImageIndex (aColumn: Integer);
  public
    Contexts: TStringListList;
    isChanged: Boolean;
    { public declarations }
  end;

var
  EditContextsForm: TEditContextsForm;

implementation

{$R *.lfm}
uses PromptUnit
   , LCLType
   ;


{ TPasswordEditLink }
type
TPasswordEditLink = class(TStringEditLink, IVTEditLink)
public
  constructor Create; override;
end;

{ TPasswordEditLink }

constructor TPasswordEditLink.Create;
begin
  inherited Create;
  Self.Edit.PasswordChar := '*';
end;

{ TEditContextsForm }

procedure TEditContextsForm.AddContextActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
  r, c: Integer;
  p: PGridTreeRec;
begin
  Application.CreateForm(TPromptForm, xForm);
  with xForm do
  try
    Caption := 'Name for new Context';
    PromptEdit.Text := '';
    Numeric := False;
    Pattern := '[A-Za-z0-9\.,]+';
    ShowModal;
    if ModalResult = mrOk then
    begin
      Contexts.RowCount := Contexts.RowCount + 1;
      Contexts.CellValue[0, Contexts.RowCount - 1] := PromptEdit.Text;
      p := GridView.GetNodeData(GridView.AddChild(nil));
      p.Row := Contexts.RowCount - 1;
      GridView.Invalidate;
      PopulateContextComboBox;
      isChanged := True;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.AddPropertyActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
begin
  Application.CreateForm(TPromptForm, xForm);
  with xForm do
  try
    Caption := 'Name for new Property';
    PromptEdit.Text := '';
    Numeric := False;
    Pattern := '[A-Za-z0-9\.,]+';
    ShowModal;
    if ModalResult = mrOk then with GridView do
    begin
      if PromptEdit.Text = 'context' then
        raise Exception.Create('"context" not allowed as property name');
      Contexts.ColCount := Contexts.ColCount + 1;
      with Header.Columns.Add do
      begin
        Text := PromptEdit.Text;
      end;
      Contexts.CellValue [Contexts.ColCount - 1, 0] := PromptEdit.Text;
      Invalidate;
      FocusedColumn := Contexts.ColCount - 1;
      SetColumnImageIndex(FocusedColumn);
      isChanged := True;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  c: Integer;
begin
  with TFormIniFile.Create(self, False) do // here instead of at formdestroy because of references to contetxts
  try
    for c := 1 to Contexts.ColCount - 1 do
      ColWidths.Values[Contexts.CellValue[c, 0]] := IntToStr(GridView.Header.Columns[c].Width);
    StringByName['ColWidths'] := ColWidths.Text;
    Save;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormCreate(Sender: TObject);
begin
  ColWidths := TJBStringList.Create;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    ColWidths.Text := StringByName['ColWidths'];
  finally
    Free;
  end;
  GridView.NodeDataSize := SizeOf(TGridTreeRec);
  isChanged := False;
end;

procedure TEditContextsForm.FormShow(Sender: TObject);
var
  c, r, x: Integer;
  xNode: PVirtualNode;
  xData: PGridTreeRec;
begin
  with GridView do
  begin
    Clear;
    Header.Columns.Clear;
    while Header.Columns.Count < Contexts.ColCount do
      Header.Columns.Add;
    for c := 0 to Contexts.ColCount - 1 do
    begin
      with Header.Columns[c] do
      begin
        Text := Contexts.CellValue [c, 0];
        if c = 0 then // contexts
          Font.Style := Font.Style + [fsBold];
        if self.ColWidths.IndexOfName(Contexts.CellValue[c, 0]) > -1 then
          Width := StrToInt(self.ColWidths.Values[Contexts.CellValue[c, 0]]);
        SetColumnImageIndex(c);
      end;
    end;
    for r := 1 to Contexts.RowCount - 1 do
    begin
      xNode := AddChild(nil);
      xData := GetNodeData(xNode);
      xData.Row := r;
    end;
    PopulateContextComboBox;
    isChanged := False;
  end;
end;

procedure TEditContextsForm.GridViewAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if hpeBackground in Elements then
  begin
    PaintInfo.TargetCanvas.Brush.Color := clBtnFace;
    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
    if Assigned(PaintInfo.Column) then with PaintInfo.TargetCanvas do
    begin
      Brush.Color := clBtnText;
      Pen.Style := psDot;
      FrameRect(PaintInfo.PaintRectangle);
    end;
  end;
end;

procedure TEditContextsForm.GridViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  with TargetCanvas do
  begin
    Brush.Style := bsSolid;
    if Assigned (Node)
    and (Column > 0) then
        Brush.Color := clWhite;
    FillRect(CellRect);
  end;
end;

procedure TEditContextsForm.GridViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  GridView.FocusedColumn := Column;
end;

procedure TEditContextsForm.GridViewCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  if isPassWordColumn(Column) then
    EditLink := TPasswordEditLink.Create
  else
    EditLink := TStringEditLink.Create;
end;

procedure TEditContextsForm.GridViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column > 0);
end;

procedure TEditContextsForm.GridViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  p: PGridTreeRec;
begin
  p := GridView.GetNodeData(Node);
  if isPassWordColumn(Column) then
    CellText := '*****'
  else
    CellText := Contexts.CellValue[Column, p.Row];
end;

procedure TEditContextsForm.GridViewHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  GridView.FocusedColumn := HitInfo.Column;
end;

procedure TEditContextsForm.GridViewHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TEditContextsForm.GridViewHeaderImageClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  GridView.FocusedColumn := Column;
  PopupMenu1.PopUp;
end;

procedure TEditContextsForm.GridViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    (Sender as TVirtualStringTree).EditNode ( (Sender as TVirtualStringTree).FocusedNode
                                            , (Sender as TVirtualStringTree).FocusedColumn)
                                            ;
    Key := 0;
  end;
end;

procedure TEditContextsForm.GridViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  p: PGridTreeRec;
begin
  p := GridView.GetNodeData(Node);
  if isPassWordColumn(Column) then
    Contexts.CellValue[Column, p.Row] := EncryptPassword (NewText)
  else
    Contexts.CellValue[Column, p.Row] := NewText;
  if not isOneTimerColumn(Column) then
    isChanged := True;
end;

procedure TEditContextsForm.GridViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Assigned (Node)
  and (Column > 0) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
end;

procedure TEditContextsForm.PopupMenu1Popup(Sender: TObject);
begin
  RemovePropertyActionUpdate(nil);
  RemoveContextActionUpdate(nil);
  SetPasswordActionUpdate(nil);
  SetAsOnetimerActionUpdate(nil);
end;

procedure TEditContextsForm.RemoveContextActionExecute(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  with GridView do
  begin
    GetColAndRow (aCol, aRow);
    if BooleanPromptDialog('Remove context ' + Contexts.CellValue [0, aRow]) then
    begin
      GridView.Clear;
      Contexts.Delete(aRow);
      FormShow(nil);
      isChanged := True;
    end;
  end;
end;

procedure TEditContextsForm.RemoveContextActionUpdate(Sender: TObject);
begin
  RemoveContextAction.Enabled := (GridView.GetNodeData(GridView.FocusedNode) <> Pointer(0));
end;

procedure TEditContextsForm.RemovePropertyActionExecute(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  with Contexts do
  begin
    aCol := 0; aRow := 0; // avoid warning
    GetColAndRow (aCol, aRow);
    if BooleanPromptDialog('Remove property ' + CellValue [aCol, 0]) then
    begin
      GridView.Clear;
      Contexts.DeleteCol(aCol);
      FormShow(nil);
      if aCol < ColCount - 1 then
        GridView.FocusedColumn := aCol
      else
        GridView.FocusedColumn := ColCount - 1;
      isChanged := True;
    end;
  end;
end;

procedure TEditContextsForm.RemovePropertyActionUpdate(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  GetColAndRow (aCol, aRow);
  RemovePropertyAction.Enabled := (aCol > 0);
end;

procedure TEditContextsForm.SetAsOnetimerActionExecute(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  GetColAndRow (aCol, aRow);
  with Contexts do
  begin
    CellObject[aCol, 0] := TObject ((QWord(CellObject[aCol, 0]) xor xmlio.OneTimeContextsOptionValue));
    SetColumnImageIndex(aCol);
    isChanged := True;
  end;
end;

procedure TEditContextsForm.SetAsOnetimerActionUpdate(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  GetColAndRow (aCol, aRow);
  SetAsOnetimerAction.Enabled := True;
  SetAsOnetimerAction.Checked := isOneTimerColumn (aCol);
  SetAsOnetimerAction.Caption := 'Do not save values for column ' + Contexts.CellValue[aCol, 0] + ' to project';
end;

procedure TEditContextsForm.SetPasswordActionExecute(Sender: TObject);
var
  aCol, aRow: Integer;
  r: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  GetColAndRow (aCol, aRow);
  with Contexts do
  begin
    CellObject[aCol, 0] := TObject ((QWord(CellObject[aCol, 0]) xor xmlio.PasswordContextsOptionValue));
    for r := 1 to RowCount - 1 do
      CellValue[aCol, r] := '';
    SetColumnImageIndex(aCol);
  end;
  GridView.Invalidate;
  isChanged := True;
end;

procedure TEditContextsForm.SetPasswordActionUpdate(Sender: TObject);
var
  aCol, aRow: Integer;
begin
  aCol := 0; aRow := 0; // avoid warning
  GetColAndRow (aCol, aRow);
  SetPasswordAction.Enabled := True;
  SetPasswordAction.Checked := isPassWordColumn (aCol);
  SetPasswordAction.Caption := 'Encrypt values for column ' + Contexts.CellValue[aCol, 0] + ' (switching clears columndata)';
end;

procedure TEditContextsForm.GetColAndRow(var aCol, aRow: Integer);
var
  p: PGridTreeRec;
begin
  aRow := 0;
  p := GridView.GetNodeData(GridView.FocusedNode);
  if Assigned (p) then
    aRow := p^.Row;
  aCol := GridView.FocusedColumn;
end;

function TEditContextsForm.isPassWordColumn(aColumn: Integer): Boolean;
begin
  with Contexts do
    result := ((QWord(CellObject[aColumn, 0]) AND xmlio.PasswordContextsOptionValue) = xmlio.PasswordContextsOptionValue);
end;

function TEditContextsForm.isOneTimerColumn(aColumn: Integer): Boolean;
begin
  with Contexts do
    result := ((QWord(CellObject[aColumn, 0]) AND xmlio.OneTimeContextsOptionValue) = xmlio.OneTimeContextsOptionValue);
end;

function TEditContextsForm.BooleanPromptDialog(aCaption: String): Boolean;
begin
  result := (MessageDlg(aCaption, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TEditContextsForm.PopulateContextComboBox;
var
  r: Integer;
begin
  ContextComboBox.Items.Clear;
  for r := 1 to Contexts.RowCount - 1 do
    ContextComboBox.Items.Add (Contexts.CellValue[0, r]);
end;

procedure TEditContextsForm.SetColumnImageIndex(aColumn: Integer);
begin
  if aColumn > 0 then
    GridView.Header.Columns[aColumn].ImageIndex := QWord (Contexts.CellObject[aColumn, 0])
  else
    GridView.Header.Columns[aColumn].ImageIndex := -1;
end;

end.

