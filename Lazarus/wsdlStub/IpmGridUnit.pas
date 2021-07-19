{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit IpmGridUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormIniFilez, StdCtrls, ExtCtrls, xmlio, Ipmz, VirtualTrees, ComCtrls,
  ActnList, Menus;

type
  TIpmGridForm = class(TForm)
    Panel2: TPanel;
    Grid: TVirtualStringTree;
    GridStatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ImageList: TImageList;
    ActionList: TActionList;
    ToolButton1: TToolButton;
    CloseAction: TAction;
    ToggleShowNillsAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    GridPopupMenu: TPopupMenu;
    ShowInGridMenuItem: TMenuItem;
    ShowInGridAction: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    CreateHtmlReportAction: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    CopySpreadSheetFormatAction: TAction;
    MainSplitter: TSplitter;
    PropertiesPanel: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    PropertiesListView: TListView;
    EnumerationsListView: TListView;
    DataTypeDocumentationMemo: TMemo;
    ToolButton8: TToolButton;
    ShowPropertiesAction: TAction;
    ReadAction: TAction;
    SaveAction: TAction;
    CopyAction: TAction;
    PasteAction: TAction;
    CopyColbolAction: TAction;
    PasteCobolAction: TAction;
    GenerateCobolWsAction: TAction;
    ReadFromFileMenuItem: TMenuItem;
    SaveAction1: TMenuItem;
    N1: TMenuItem;
    CopyAction1: TMenuItem;
    PasteMenuItem: TMenuItem;
    CopyColbolAction1: TMenuItem;
    PasteCobolMenuItem: TMenuItem;
    N4: TMenuItem;
    GenerateCobolWsAction1: TMenuItem;
    N5: TMenuItem;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    procedure PasteCobolActionUpdate(Sender: TObject);
    procedure PasteActionUpdate(Sender: TObject);
    procedure ReadActionUpdate(Sender: TObject);
    procedure GenerateCobolWsActionExecute(Sender: TObject);
    procedure PasteCobolActionExecute(Sender: TObject);
    procedure CopyColbolActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure ReadActionExecute(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure ShowPropertiesActionExecute(Sender: TObject);
    procedure EditInPopUpActionExecute(Sender: TObject);
    procedure SetNilActionExecute(Sender: TObject);
    procedure CopySpreadSheetFormatActionExecute(Sender: TObject);
    procedure CreateHtmlReportActionExecute(Sender: TObject);
    procedure GridPopupMenuPopup(Sender: TObject);
    procedure ShowInGridActionExecute(Sender: TObject);
    procedure ToggleShowNillsActionExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure GridExit(Sender: TObject);
    procedure GridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure GridHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure GridPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure GridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    Ipms: TIpmItemList;
    nCols, nRows: Integer;
    Lists: TJBStringList;
    ColumnWidths: TJBStringList;
    fGridForm: TIpmGridForm;
    fStubChanged: Boolean;
    fReadOnly: Boolean;
    fStringList: TJBStringList;
    procedure ListProperties(aListView: TListView; aIpm: TIpmItem);
    function getPropertiesVisible: Boolean;
    procedure setPropertiesVisible(const Value: Boolean);
    function CreateHtmlReport: String;
    function getFocusedIpm: TIpmItem;
    procedure ShowHtml(aCaption, aInfoString: String);
    procedure setFocusedIpm(const Value: TIpmItem);
    procedure ShowHideColumns;
    function getGroupVisible(col: Integer): Boolean;
    procedure setGroupVisible(col: Integer; const Value: Boolean);
    procedure setColumnVisible(col: Integer; const Value: Boolean);
    function getColumnVisible(col: Integer): Boolean;
    function RowSpan (aIpm: TIpmItem): Integer;
    procedure CleanUp;
    procedure ShowGrid (aFocusIpm: TIpmItem);
    function getDoShowNills: Boolean;
    procedure CheckValueAgainstXsd (aXml: TIpmItem);
    function getColumnSpan(col: Integer): Integer;
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    procedure setCell(col, row: Integer; const Value: TIpmItem);
    function getCell(col, row: Integer): TIpmItem;
    procedure SetNodesVisibilty;
    procedure HaveString (aString: String);
    property FocusedIpm: TIpmItem read getFocusedIpm write setFocusedIpm;
    property Cell [col, row: Integer]: TIpmItem read getCell write setCell;
    property ColumnSpan [col: Integer]: Integer read getColumnSpan;
    property GroupVisible [col: Integer]: Boolean read getGroupVisible write setGroupVisible;
    property ColumnVisible [col: Integer]: Boolean read getColumnVisible write setColumnVisible;
  public
    Ipm: TIpmItem;
    doConfirmRemovals: Boolean;
    property doShowNills: Boolean read getDoShowNills;
    property isReadOnly: Boolean read getReadOnly write setReadOnly;
    property PropertiesVisible: Boolean read getPropertiesVisible write setPropertiesVisible;
    property stubChanged: Boolean read fStubChanged;
  end;

type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Row: Integer;
    Ipms: TIpmItemList;
  end;

var
  IpmGridForm: TIpmGridForm;

const xmlGridMaxBom = 2;

implementation

uses ClipBrd
   , StrUtils
   , xmlUtilz
   , Bind
   , Xmlz
   , EditValueUnit
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TIpmGridForm.FormCreate(Sender: TObject);
begin
  Ipms := TIpmItemList.Create;
  Grid.NodeDataSize := SizeOf(TTreeRec);
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    ToggleShowNillsAction.Checked := BooleanByNameDef ['doShowNillsInXmlGrid', True];
    PropertiesVisible := BooleanByNameDef ['PropertiesVisible', False];
    Lists := TJBStringList.Create;
    ColumnWidths := TJBStringList.Create;
    ColumnWidths.Text := StringByName['DataGridColumnWidths'];
  finally
    Free;
  end;
end;

procedure TIpmGridForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  for x := Grid.Header.Columns.Count - 1  downto 0 do
    ColumnWidths.Values [Grid.Header.Columns.Items[x].Text]
    := IntToStr (Grid.Header.Columns.Items[x].Width);
  with TFormIniFile.Create(self, False) do
  try
    BooleanByName ['doShowNillsInXmlGrid'] := doShowNills;
    BooleanByName ['PropertiesVisible'] := PropertiesVisible;
    StringByName['DataGridColumnWidths'] := ColumnWidths.Text;
    Save;
  finally
    Free;
  end;
  Lists.Clear;
  Lists.Free;
  Ipms.Free;
  ColumnWidths.Free;
end;

procedure TIpmGridForm.FormShow(Sender: TObject);
  procedure _CreateColumns (aIpm: TIpmItem);
  var
    x: Integer;
    xVTColumn: TVirtualTreeColumn;
  begin
    if aIpm.Occurrence > 1 then Exit;
    try
      xVTColumn := Grid.Header.Columns.Add;
      with xVTColumn do
      begin
        Text := aIpm.Name;
        Width :=
          StrToIntDef ( ColumnWidths.Values [Text]
                      , Width
                      );
          if aIpm.Items.Count > 0 then
          begin
{
            if aIpm.InitialCollapsed then
              ImageIndex := 26
            else
}
              ImageIndex := 27;
            Margin := 0;
          end;
        Tag := Grid.Header.Columns.Count;
      end;
      Ipms.AddObject('', aIpm);
{
      aXsd._Processed := True;
}
      for x := 0 to aIpm.Items.Count - 1 do
        _CreateColumns (aIpm.Items.IpmItems [x]);
      xVTColumn.Tag := Grid.Header.Columns.Count - xVTColumn.Tag;
    finally
    end;
  end;
begin
  _CreateColumns (Ipm);
  ShowGrid (Ipm);
end;

function TIpmGridForm.getCell(col, row: Integer): TIpmItem;
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  result := xData.Ipms.IpmItems[col];
  result := xData.Ipms.IpmItems[col];
end;

procedure TIpmGridForm.setCell(col, row: Integer; const Value: TIpmItem);
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  xData.Ipms.IpmItems[col] := Value;
end;

function TIpmGridForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TIpmGridForm.setReadOnly(const Value: Boolean);
begin
  fReadOnly := Value;
end;

procedure TIpmGridForm.GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  xIpm: TIpmItem;
  xData: PTreeRec;
begin
  xData := Grid.GetNodeData(Node);
  if not Assigned (xData) then Exit;
  xIpm := Cell [Column, xData.Row];
  if xIpm.Checked then
    CheckValueAgainstXsd(xIpm);
end;

procedure TIpmGridForm.GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  xIpm: TIpmItem;
  xData: PTreeRec;
begin
  Allowed := False;
  if fReadOnly then Exit;
  xData := Grid.GetNodeData(Node);
  if not Assigned (xData) then Exit;
  xIpm := Cell [Column, xData.Row];
  Allowed := Assigned (xIpm)
         and (xIpm.Items.Count = 0)
           ;
end;

procedure TIpmGridForm.GridExit(Sender: TObject);
begin
  Grid.EndEditNode;
end;

procedure TIpmGridForm.GridFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xData: PTreeRec;
  xIpm: TIpmItem;
begin
  GridStatusBar.Panels [0].Text := '';
  EnumerationsListView.Clear;
  if Assigned (Node) then
  begin
    xData := Sender.GetNodeData(Node);
    xIpm := Cell [Column, xData.Row];
    ListProperties(PropertiesListView, xIpm);
{
    XmlUtil.ListXsdEnumerations(InWsdlEnumerationsListView, xIpm, nil);
    XmlUtil.ListXsdDocumentation(DataTypeDocumentationMemo, xIpm, nil);
}
    if Assigned (xIpm) then
    begin
      GridStatusBar.Panels [0].Text := xIpm.FullCaption;
    end;
  end
  else
  begin
    PropertiesListView.Clear;
    DataTypeDocumentationMemo.Clear;
  end;
  Grid.Invalidate;
end;

procedure TIpmGridForm.GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  xData: PTreeRec;
  xIpm: TIpmItem;
begin
  xData := Grid.GetNodeData(Node);
  xIpm := xData.Ipms.IpmItems[Column];
  if not Assigned (xIpm) then
    CellText := ''
  else
  begin
    if xIpm.Items.Count > 0 then
      CellText := '[Group]'
    else
      CellText := xIpm.Value;
  end;
end;

procedure TIpmGridForm.GridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  xIpm: TipmItem;
begin
  xIpm := Ipms.IpmItems [HitInfo.Column];
  if xIpm.Items.Count = 0 then Exit;
  GroupVisible [HitInfo.Column] := not GroupVisible [HitInfo.Column];
  ShowHideColumns;
  Grid.FocusedColumn := HitInfo.Column;
end;

procedure TIpmGridForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN)
  or (Key = VK_F2)
  then
    Grid.OnDblClick (Sender);
end;

procedure TIpmGridForm.GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var
  xIpm: TIpmItem;
  xData: PTreeRec;
begin
  xData := Grid.GetNodeData(Node);
  if not Assigned (xData) then Exit;
  xIpm := Cell [Column, xData.Row];
  if Assigned (xIpm) then
  begin
    if NewText = '&nil' then
    begin
      if xIpm.Checked then
      begin
        xIpm.Checked := False;
        fStubChanged := True;
      end;
    end
    else
    begin
      if (NewText <> xIpm.Value)
      or (not xIpm.CheckedAllUp) then
      begin
        xIpm.Value := NewText;
        xIpm.Checked := True;
        fStubChanged := True;
      end;
    end;
  end;
end;

procedure TIpmGridForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TIpmGridForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TIpmGridForm.SetNodesVisibilty;
var
  xNode: PVirtualNode;
  xData: PTreeRec;
  xVisible: Boolean;
  c: Integer;
begin
  xNode := Grid.GetFirst;
  while Assigned (xNode) do
  begin
    xData := Grid.GetNodeData(xNode);
    xVisible := False;
    c := 0;
    while (not xVisible)
    and (c < xData.Ipms.Count) do
    begin
      xVisible := Assigned (xData.Ipms.IpmItems[c])
              and (   doShowNills
                   or (xData.Ipms.IpmItems[c].Value <> '')
                  )
              and (coVisible in Grid.Header.Columns.Items[c].Options);
      Inc (c);
    end;
    Grid.IsVisible [xNode] := xVisible;
    xNode := Grid.GetNext(xNode);
  end;
end;

function TIpmGridForm.getColumnSpan(col: Integer): Integer;
begin
  try
    result := Grid.Header.Columns.Items[col].Tag;
  except
    result := 0;
  end;
end;

procedure TIpmGridForm.CheckValueAgainstXsd(aXml: TIpmItem);
begin
  { TODO : implementeren }
{
  if Assigned (XmlAttr) then
  begin
    if not XmlAttr.IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
  end
  else
  begin
    if not aXml.IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
  end;
}
end;

procedure TIpmGridForm.ToggleShowNillsActionExecute(Sender: TObject);
begin
  ToggleShowNillsAction.Checked := not ToggleShowNillsAction.Checked;
  ShowGrid (FocusedIpm);
end;

function TIpmGridForm.getDoShowNills: Boolean;
begin
  result := ToggleShowNillsAction.Checked;
end;

procedure TIpmGridForm.ShowGrid (aFocusIpm: TIpmItem);
  function _nCols (aIpm: TIpmItem): Integer;
  var
    x: Integer;
  begin
    result := 1;
    for x := 0 to aIpm.Items.Count - 1 do
      result := result + _nCols (aIpm.Items.IpmItems[x]);
  end;
  function _nRows (aIpm: TIpmItem): Integer;
  var
    e, n, mx: Integer;
  begin
    if aIpm.Items.Count = 0 then
      result := 1
    else
    begin
      mx := 1;
      n := 0;
      for e := 0 to aIpm.Items.Count - 1 do
      begin
        n := aIpm.Items.IpmItems[e].Occurs * _nRows (aIpm.Items.IpmItems[e]);
        if n > mx then
          mx := n;
      end;
      result := mx;
    end;
  end;
  procedure _LinkIpm (aIpm: TIpmItem; aCol, aRow: Integer);
  var
    x, sCol, nCol, nRow: Integer;
  begin
    try
      Cell [aCol, aRow] := aIpm;
      aCol := aCol + 1;
      sCol := aCol;
      nCol := aCol;
      nRow := aRow;
      for x := 0 to aIpm.Items.Count - 1 do
      begin
        if aIpm.Items.IpmItems[x].Occurrence > 1 then
          nCol := sCol
        else
        begin
          sCol := nCol;
          nRow := aRow;
        end;
        _LinkIpm (aIpm.Items.IpmItems[x], nCol, nRow);
        nRow := nRow + _nRows(aIpm.Items.IpmItems[x]);
        nCol := nCol + ColumnSpan [nCol] + 1;
      end;
    finally
    end;
  end;
var
  x, y, xCol: Integer;
  xData: PTreeRec;
begin
  Grid.BeginUpdate;
  try
    CleanUp;
    if Assigned (Ipm) then
    begin
      Caption := Ipm.FullCaption;
      nCols := Grid.Header.Columns.Count;
      nRows := _nRows (Ipm);
      // first clean up
      for y := 0 to nRows - 1 do
      begin
        xData := Grid.GetNodeData(Grid.AddChild(nil));
        Lists.AddObject('', TObject (xData));
        xData.Row := y;
        xData.Ipms := TIpmItemList.Create;
        for x := 0 to nCols - 1 do
          xData.Ipms.AddObject('', nil);
      end;
      xCol := 0;
      _LinkIpm (Ipm, xCol, 0);
    end;
    ShowHideColumns;
    FocusedIpm := aFocusIpm;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TIpmGridForm.CleanUp;
var
  x: Integer;
begin
  for x := 0 to Lists.Count - 1 do
    PTreeRec(Lists.Objects[x]).Ipms.Free;
  Lists.Clear;
  Grid.Clear;
end;

function TIpmGridForm.RowSpan(aIpm: TIpmItem): Integer;
var
  e, x, n, mx: Integer;
begin
  if (not doShowNills)
  and (not aIpm.CheckedAllUp) then
  begin
    result := 0;
    exit;
  end;
  if aIpm.Items.Count = 0 then
    result := 1
  else
  begin
    mx := 1;
    n := 0;
    for e := 0 to aIpm.Items.Count - 1 do
    begin
      n := 0;
      for x := 0 to aIpm.Items.Count - 1 do
        if (aIpm.Items.IpmItems[x] = aIpm.Items.IpmItems [e]) then
          n := n + RowSpan (aIpm.Items.IpmItems[x]);
      if n > mx then
        mx := n;
    end;
    result := mx;
  end;
end;

function TIpmGridForm.getColumnVisible(col: Integer): Boolean;
begin
  result := coVisible in Grid.Header.Columns.Items[col].Options;
end;

procedure TIpmGridForm.setColumnVisible(col: Integer; const Value: Boolean);
begin
  if (Value) then
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options + [coVisible]
  else
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options - [coVisible];
end;

function TIpmGridForm.getGroupVisible(col: Integer): Boolean;
begin
  result := Grid.Header.Columns[col].ImageIndex = 27;
end;

procedure TIpmGridForm.setGroupVisible(col: Integer; const Value: Boolean);
begin
  if Value then
    Grid.Header.Columns[col].ImageIndex := 27
  else
    Grid.Header.Columns[col].ImageIndex := 26;
end;

procedure TIpmGridForm.ShowHideColumns;
  procedure _ShowHideColumn (aIpm: TIpmItem; var aCol: Integer; aVisible: Boolean);
  var
    e, xCol: Integer;
  begin
    try
      if aIpm.Occurrence > 1 then Exit;
      xCol := aCol;
      ColumnVisible [aCol] := aVisible;
      Inc (aCol);
      for e := 0 to aIpm.Items.Count - 1 do
        _ShowHideColumn (aIpm.Items.IpmItems [e], aCol, (aVisible and GroupVisible[xCol]));
    finally
    end;
  end;
var
  xCol: Integer;
begin
  xCol := 0;
  _ShowHideColumn(Ipm, xCol, True);
  SetNodesVisibilty;
end;

procedure TIpmGridForm.ShowInGridActionExecute(Sender: TObject);
begin
  Application.CreateForm(TIpmGridForm, fGridForm);
  try
    fGridForm.isReadOnly := isReadOnly;
    fGridForm.Ipm := FocusedIpm;
    fGridForm.ShowModal;
    if fGridForm.StubChanged then
      ShowGrid (FocusedIpm);
    fStubChanged := fStubChanged or fGridForm.StubChanged;
  finally
    FreeAndNil (fGridForm);
  end;
end;

procedure TIpmGridForm.GridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if (Node = Grid.GetFirst)
  and (Column = 0) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;
end;

procedure TIpmGridForm.GridPopupMenuPopup(Sender: TObject);
begin
  if not Assigned (FocusedIpm) then
    raise Exception.Create('Context menu disabled when not on a used cell');
end;

function TIpmGridForm.getFocusedIpm: TIpmItem;
begin
  result := nil;
  if Assigned (Grid.FocusedNode) then
  begin
    result := Cell [Grid.FocusedColumn, PTreeRec(Grid.GetNodeData(Grid.FocusedNode)).Row];
  end;
end;

procedure TIpmGridForm.setFocusedIpm(const Value: TIpmItem);
var
  c: Integer;
  xNode: PVirtualNode;
  xData: PTreeRec;
  xFound: Boolean;
begin
  xNode := Grid.GetFirst;
  xFound := False;
  while Assigned (xNode)
  and (not xFound) do
  begin
    xData := Grid.GetNodeData(xNode);
    for c := 0 to Grid.Header.Columns.Count - 1 do
    begin
      if xData.Ipms.IpmItems[c] = Value then
      begin
        xFound := True;
        Grid.FocusedNode := xNode;
        Grid.FocusedColumn := c;
        Break;
      end;
    end;
    xNode := Grid.GetNext(xNode);
  end;
end;

procedure TIpmGridForm.ShowHtml(aCaption, aInfoString: String);
begin
  if AnsiLeftStr(aInfoString, 6) <> '<html>' then
    raise Exception.Create('No HTML content');
  ShowMessage(aInfoString);
end;

procedure TIpmGridForm.CreateHtmlReportActionExecute(Sender: TObject);
begin
  ShowHtml ('', CreateHtmlReport);
end;

function TIpmGridForm.CreateHtmlReport: String;
  function _Headers: TXml;
  var
    col: Integer;
  begin
    result := TXml.CreateAsString('tr', '');
    for col := 0 to Grid.Header.Columns.Count - 1 do
      if ColumnVisible [col] then
        with result.AddXml (TXml.CreateAsString('td', '')) do
          AddXml (TXml.CreateAsString('strong', Grid.Header.Columns[col].Text));
  end;
  function _Details(aNode: PVirtualNode): TXml;
  var
    col: TColumnIndex;
    xText: String;
  begin
    result := TXml.CreateAsString('tr', '');
    with result.AddXml(TXml.CreateAsString('tr', '')) do
    begin
      for col := 0 to Grid.Header.Columns.Count - 1 do
      begin
        if ColumnVisible [col] then
        begin
          xText := ''; // to avoid compiler warning
          GridGetText(Grid, aNode, col, ttNormal, xText);
          AddXml (TXml.CreateAsString('td', xText));
        end;
      end;
    end;
  end;
var
  htmlXml: TXml;
  tableXml: TXml;
  xNode: PVirtualNode;
begin
  htmlXml := TXml.CreateAsString('html', '');
  try
    tableXml := htmlXml.AddXml(TXml.CreateAsString('table', ''));
    tableXml.AddAttribute(TXmlAttribute.CreateAsString('border', '1'));
    tableXml.AddXml(_Headers);
    xNode := Grid.GetFirst;
    while Assigned (xNode) do
    begin
      if Grid.IsVisible [xNode] then
        tableXml.AddXml(_Details(xNode));
      xNode := Grid.GetNext(xNode);
    end;
    result := htmlXml.asHtmlString;
  finally
    htmlXml.Free;
  end;
end;

procedure TIpmGridForm.CopySpreadSheetFormatActionExecute(Sender: TObject);
  function _Columns (aNode: PVirtualNode): String;
  var
    col: TColumnIndex;
    xText: String;
    xSep: String;
  begin
    result := '';
    xSep := '';
    for col := 0 to Grid.Header.Columns.Count - 1 do
    begin
      if ColumnVisible [col] then
      begin
        xText := ''; // to avoid compiler warning
        GridGetText(Grid, aNode, col, ttNormal,xText);
        result := result + xSep + xText;
        xSep := #9;
      end;
    end;
  end;
var
  xNode: PVirtualNode;
  s, xSep: String;
  c: Integer;
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    xNode := Grid.GetFirst;
    s := '';
    xSep := '';
    for c := 0 to Grid.Header.Columns.Count - 1 do
      if ColumnVisible [c] then
      begin
        s := s + xSep + Grid.Header.Columns[c].Text;
        xSep := #9;
      end;
    while Assigned (xNode) do
    begin
      if Grid.IsVisible [xNode] then
        s := s + #$D#$A + _Columns (xNode);
      xNode := Grid.GetNext(xNode);
    end;
    Clipboard.AsText := s;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TIpmGridForm.SetNilActionExecute(Sender: TObject);
  procedure _Uncheck (aIpm: TIpmItem);
  var
    x: Integer;
  begin
    aIpm.Checked := False;
    for x := 0 to aIpm.Items.Count - 1 do
      _Uncheck (aIpm.Items.IpmItems[x]);
  end;
var
  xIpm: TIpmItem;
begin
  if isReadOnly then
    raise Exception.Create ('not allowed because form is in read-only mode');
  xIpm := FocusedIpm;
  if Assigned (xIpm) then
  begin
    xIpm.Checked := not xIpm.CheckedAllUp;
{
    if not xIpm.Checked then
      _Uncheck (xIpm);
}
    fStubChanged := True;
    ShowGrid (xIpm);
  end;
end;

procedure TIpmGridForm.EditInPopUpActionExecute(Sender: TObject);
begin
{
  if (not isReadOnly)
  and XmlUtil.isEditSupported (FocusedIpm, nil) then
  begin
    if XmlUtil.editValue (FocusedIpm, nil, isReadOnly) then
    begin
      GridNewText ( Grid
                  , Grid.FocusedNode
                  , Grid.FocusedColumn
                  , XmlUtil.NewValue
                  );
    end;
  end;
}
end;

procedure TIpmGridForm.ShowPropertiesActionExecute(Sender: TObject);
begin
  PropertiesVisible := not PropertiesVisible;
end;

function TIpmGridForm.getPropertiesVisible: Boolean;
begin
  result := ShowPropertiesAction.Checked;
end;

procedure TIpmGridForm.setPropertiesVisible(const Value: Boolean);
begin
  ShowPropertiesAction.Checked := Value;
  PropertiesPanel.Visible := Value;
  MainSplitter.Visible := Value;
end;

procedure TIpmGridForm.ListProperties(aListView: TListView; aIpm: TIpmItem);
  procedure AddProperty (aKey: String; aValue: String);
  var
    ListItem: TListItem;
  begin
    if aValue <> '' then
    begin
      ListItem := aListView.Items.Add;
      ListItem.Caption := aKey;
      ListItem.SubItems.Add(aValue);
    end;
  end;
  procedure AddBooleanProperty (aKey: String; aValue: Boolean);
  begin
    if aValue then
      AddProperty(aKey, 'true')
    else
      AddProperty(aKey, 'false');
  end;
  procedure AddIntegerProperty (aKey: String; aValue: Integer);
  begin
    AddProperty(aKey, IntToStr (aValue))
  end;
begin
  aListView.Clear;
  if not Assigned (aIpm) then Exit;
  AddProperty('Name', aIpm.Name);
  AddIntegerProperty('Level', aIpm.Level);
  AddBooleanProperty('Group', aIpm.Group);
  AddIntegerProperty('Occurs', aIpm.Occurs);
  AddIntegerProperty('Occurrence', aIpm.Occurrence);
  AddIntegerProperty('Occurs', aIpm.Occurs);
  AddIntegerProperty('minOccurs', aIpm.minOccurs);
  AddBooleanProperty('Numeric', aIpm.Numeric);
  AddBooleanProperty('HasComp', aIpm.HasComp);
  AddBooleanProperty('Comp', aIpm.Comp);
  AddBooleanProperty('Display', aIpm.Display);
  AddIntegerProperty('InputLength', aIpm.InputLength);
  AddIntegerProperty('Bytes', aIpm.Bytes);
  AddIntegerProperty('Offset', aIpm.Offset);
  AddIntegerProperty('Precision', aIpm.Precision);
  AddProperty('PictureClause', aIpm.PictureClause);
  AddBooleanProperty('Signed', aIpm.Signed);
  AddBooleanProperty('SignLeading', aIpm.SignLeading);
  AddBooleanProperty('SignSeparate', aIpm.SignSeparate);
end;

procedure TIpmGridForm.GridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  xData: PTreeRec;
  xIpm: TIpmItem;
begin
  xData := Grid.GetNodeData(Node);
  xIpm := xData.Ipms.IpmItems[Column];
  if (not Assigned (xIpm)) then
  begin
    with TargetCanvas do
    begin
      Brush.Style := bsSolid;
//    Brush.Color := $CFFFFF;
      Brush.Color := $AAFFFF;
      FillRect( CellRect );
    end;
  end
  else
  begin
    if (    Assigned (FocusedIpm)
        and FocusedIpm.IsAncestorOf (xIpm)
       )
    or xIpm.IsAncestorOf (FocusedIpm) then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := $D6E890;
        FillRect( CellRect );
      end;
    end;
  end;
end;

procedure TIpmGridForm.GridDblClick(Sender: TObject);
begin
  if (not Assigned (FocusedIpm))
  or (FocusedIpm.Group)
    then Exit;
  Application.CreateForm(TEditValueForm, EditValueForm);
  try
    EditValueForm.Ipm := FocusedIpm;
    EditValueForm.ReadOnly := isReadOnly;
    EditValueForm.ShowModal;
    if EditValueForm.ModalResult = mrOk then
    begin
      Grid.InvalidateNode (Grid.FocusedNode);
    end;
  finally
    FreeAndNil (EditValueForm);
  end;
end;

procedure TIpmGridForm.ReadActionExecute(Sender: TObject);
var
  aXml: TXml;
begin
  if not Assigned (FocusedIpm) then Exit;
  if IsReadOnly then Exit;
  OpenFileDialog.Title := 'Read data for node: ' + FocusedIpm.FullCaption;
  OpenFileDialog.DefaultExt := 'XML';
  OpenFileDialog.Filter := 'XML File (*.XML)|*.XML';
  if FocusedIpm.XmlFileName <> '' then
    OpenFileDialog.FileName := FocusedIpm.XmlFileName;
  if OpenFileDialog.Execute = True then
  begin
    aXml := TXml.Create;
    try
      FocusedIpm.XmlFileName := OpenFileDialog.FileName;
      aXml.LoadFromFile (FocusedIpm.XmlFileName, nil, nil);
      FocusedIpm.ResetLoaded (True);
      aXml.TagName := FocusedIpm.Name; // Ignore name conflict at first level
      FocusedIpm.LoadValues (aXml);
      Grid.Invalidate;
    finally
      FreeAndNil (aXml);
    end;
  end;
end;

procedure TIpmGridForm.SaveActionExecute(Sender: TObject);
begin
  SaveFileDialog.Title := 'Save data from node: ' + FocusedIpm.FullCaption;
  if FocusedIpm.XmlFileName <> '' then
    SaveFileDialog.FileName := FocusedIpm.XmlFileName;
  SaveFileDialog.DefaultExt := 'XML';
  SaveFileDialog.Filter := 'XML File (*.XML)|*.XML';
  if SaveFileDialog.Execute = True then
  begin
    FocusedIpm.XmlFileName := SaveFileDialog.FileName;
    fStringList := TJBStringList.Create;
    try
      FocusedIpm.BuildXML (HaveString, 0, False);
      fStringList.SaveToFile (FocusedIpm.XmlFileName);
    finally
      fStringList.Free;
    end;
  end;
end;

procedure TIpmGridForm.HaveString(aString: String);
begin
  fStringList.Add (aString);
end;

procedure TIpmGridForm.CopyActionExecute(Sender: TObject);
begin
  if not Assigned (FocusedIpm) then Exit;
  fStringList := TJBStringList.Create;
  try
    FocusedIpm.BuildXML (HaveString, 0, False);
    Clipboard.AsText := fStringList.Text;
  finally
    fStringList.Free;
  end;
end;

procedure TIpmGridForm.PasteActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned (FocusedIpm) then Exit;
  if IsReadOnly then Exit;
  xXml := TXml.Create;
  try
    xXml.LoadFromString(ClipBoard.AsText, nil);
    FocusedIpm.ResetLoaded (True);
    xXml.TagName := FocusedIpm.Name; // Ignore name conflict at first level
    FocusedIpm.LoadValues (xXml);
    Grid.Invalidate;
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TIpmGridForm.CopyColbolActionExecute(Sender: TObject);
begin
  if not Assigned (FocusedIpm) then Exit;
  ClipBoard.AsText := FocusedIpm.ValuesToBuffer(nil);
end;

procedure TIpmGridForm.PasteCobolActionExecute(Sender: TObject);
begin
  if not Assigned (FocusedIpm) then Exit;
  if IsReadOnly then Exit;
  FocusedIpm.BufferToValues(nil, ClipBoard.AsText);
  Grid.Invalidate;
end;

procedure TIpmGridForm.GenerateCobolWsActionExecute(Sender: TObject);
begin
  if not Assigned (FocusedIpm) then Exit;
  fStringList := TJBStringList.Create;
  try
    FocusedIpm.BuildCobolWS (HaveString);
    xmlUtilz.ShowText('IpmGun - Cobol Working-Storage with Values', fStringList.Text);
  finally
    fStringList.Free;
  end;
end;

procedure TIpmGridForm.ReadActionUpdate(Sender: TObject);
begin
  ReadAction.Enabled := not isReadOnly;
end;

procedure TIpmGridForm.PasteActionUpdate(Sender: TObject);
begin
  PasteAction.Enabled := not isReadOnly;
end;

procedure TIpmGridForm.PasteCobolActionUpdate(Sender: TObject);
begin
  PasteCobolAction.Enabled := not isReadOnly;
end;

end.
