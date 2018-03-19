unit A2BXmlGridUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages , SysUtils , Variants , Classes , Graphics , Controls , Forms ,
  Dialogs , FormIniFilez , StdCtrls , ExtCtrls , Xsdz , A2BXmlz , Xmlz, VirtualTrees ,
  ComCtrls , ImgList , ToolWin , ActnList , Menus , Bind
{$IFnDEF FPC}
  , OleCtrls
  , SHDocVw
{$ENDIF}
  ;

type

  { TA2BXmlGridForm }

  TA2BXmlGridForm = class(TForm)
    Panel2: TPanel;
    Grid: TVirtualStringTree;
    ToolBar1: TToolBar;
    ImageList: TImageList;
    ActionList: TActionList;
    ToggleShowEmptyRowsAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    GridPopupMenu: TPopupMenu;
    ShowInGridMenuItem: TMenuItem;
    ShowInGridAction: TAction;
    CreateHtmlReportAction: TAction;
    ToolButton6: TToolButton;
    CopySpreadSheetFormatAction: TAction;
    SetNilAction: TAction;
    N1: TMenuItem;
    SetNilMenuItem: TMenuItem;
    EditInPopUpAction: TAction;
    EditInPopUpMenuItem: TMenuItem;
    AddAction: TAction;
    AddMenuItem: TMenuItem;
    DeleteAction: TAction;
    DeleteMenuItem: TMenuItem;
    N2: TMenuItem;
    CopyToClipBoardAction: TAction;
    PasteFromClipboardAction: TAction;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    N3: TMenuItem;
    PopulateMenuItem: TMenuItem;
    All1: TMenuItem;
    Required1: TMenuItem;
    ValidateMenuItem: TMenuItem;
    ZoomasXMLMenuItem: TMenuItem;
    MainSplitter: TSplitter;
    ToolButton8: TToolButton;
    ShowPropertiesAction: TAction;
    ToggleShowEmptyColunsAction: TAction;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    SetNotNiAction: TAction;
    SetNotNilMenuItem: TMenuItem;
    ZoomMenu: TMenuItem;
    Base641: TMenuItem;
    HTML1: TMenuItem;
    PDFBase641: TMenuItem;
    ext1: TMenuItem;
    XML1: TMenuItem;
    ToggleShowAttributeColumnsAction: TAction;
    ToolButton11: TToolButton;
    FindAction: TAction;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    FindNextAction: TAction;
    ToolButton14: TToolButton;
    Action1: TAction;
    ZoomAction: TAction;
    ZoomBestTryMenuItem: TMenuItem;
    Panel1: TPanel;
    OkButton: TButton;
    StatusPanel: TPanel;
    CleanAction: TAction;
    CleanMenuItem: TMenuItem;
    procedure GridAfterCellPaint (Sender : TBaseVirtualTree ;
      TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
      const CellRect : TRect );
    procedure GridInitNode (Sender : TBaseVirtualTree ; ParentNode ,
      Node : PVirtualNode ; var InitialStates : TVirtualNodeInitStates );
    procedure ZoomActionExecute(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure GridGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure ToggleShowAttributeColumnsActionExecute(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure PDFBase641Click(Sender: TObject);
    procedure HTML1Click(Sender: TObject);
    procedure Base641Click(Sender: TObject);
    procedure ext1Click(Sender: TObject);
    procedure SetNotNiActionExecute(Sender: TObject);
    procedure ToggleShowEmptyColunsActionExecute(Sender: TObject);
    procedure ShowPropertiesActionExecute(Sender: TObject);
    procedure ViewAsXMLMenuItemClick(Sender: TObject);
    procedure ValidateMenuItemClick(Sender: TObject);
    procedure Required1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure PasteFromClipboardActionExecute(Sender: TObject);
    procedure CopyToClipBoardActionExecute(Sender: TObject);
    procedure GridExit(Sender: TObject);
    procedure GridPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure SetNilActionExecute(Sender: TObject);
    procedure CopySpreadSheetFormatActionExecute(Sender: TObject);
    procedure CreateHtmlReportActionExecute(Sender: TObject);
    procedure GridPopupMenuPopup(Sender: TObject);
    procedure ToggleShowEmptyRowsActionExecute(Sender: TObject);
    procedure GridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GridColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure GridGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CancelButtonClick(Sender: TObject);
    procedure CleanActionExecute(Sender: TObject);
  private
    headerClicked: Boolean;
    grid_x, grid_y: Integer;
    IniFile: TFormIniFile;
    nCols, nRows: Integer;
    Lists: TStringList;
    ColumnWidths: TStringList;
    fStubChanged: Boolean;
    fReadOnly: Boolean;
    fXsdDescr: TXsdDescr;
    function inImageArea: Boolean;
    procedure NextVisibleCell (var aColumn: TColumnIndex; var aNode: PVirtualNode);
    function getIsAttributeColumn(col: Integer): Boolean;
    procedure setIsAttributeColumn(col: Integer; const Value: Boolean);
    function getDoShowAttributeColumns: Boolean;
    function XmlGetColor (aBind, aRefBind: TA2BXml; aBValue: Boolean): TColor;
    function getBindColNode(acol: Integer; aNode: PVirtualNode): TA2BXml;
    procedure setBindColNode(acol: Integer; aNode: PVirtualNode; const Value: TA2BXml);
    function getDoShowEmptyColumns: Boolean;
    function getXsdPropertiesVisible: Boolean;
    procedure setXsdPropertiesVisible(const Value: Boolean);
    function CreateHtmlReport: String;
    function getFocusedBind: TA2BXml;
    procedure ShowHtml(aCaption, aInfoString: String);
    procedure setFocusedBind(const Value: TA2BXml);
    procedure ShowHideColumns;
    function getGroupExpanded(col: Integer): Boolean;
    procedure setGroupExpanded(col: Integer; const Value: Boolean);
    procedure setColumnVisible(col: Integer; const Value: Boolean);
    function getColumnVisible(col: Integer): Boolean;
    procedure CleanUp;
    procedure ShowGrid (aFocusBind: TA2BXml);
    function getDoShowEmptyRows: Boolean;
    function getColumnSpan(col: Integer): Integer;
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    procedure setCell(col, row: Integer; const Value: TA2BXml);
    function getCell(col, row: Integer): TA2BXml;
    function getImageIndex (aBind: TA2BXml): Integer;
    procedure SetNodesVisibilty;
    procedure SetXmlsChecked (aCol: Integer; aChecked: Boolean);
    procedure SaveSettings;
    property FocusedBind: TA2BXml read getFocusedBind write setFocusedBind;
    property a2bCell [col, row: Integer]: TA2BXml read getCell write setCell;
    property BindColNode [acol: Integer; aNode: PVirtualNode]: TA2BXml read getBindColNode write setBindColNode;
    property ColumnSpan [col: Integer]: Integer read getColumnSpan;
    property GroupExpanded [col: Integer]: Boolean read getGroupExpanded write setGroupExpanded;
    property isAttributeColumn [col: Integer]: Boolean read getIsAttributeColumn write setIsAttributeColumn;
    property ColumnVisible [col: Integer]: Boolean read getColumnVisible write setColumnVisible;
  public
    ignoreDifferencesOn, checkValueAgainst, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TStringList;
    Xml: TA2BXml;
    doConfirmRemovals: Boolean;
    initialExpandStyle: TBindExpandStyle;
    property doShowAttributeColumns: Boolean read getDoShowAttributeColumns;
    property doShowEmptyColumns: Boolean read getDoShowEmptyColumns;
    property doShowEmptyRows: Boolean read getDoShowEmptyRows;
    property XsdPropertiesVisible: Boolean read getXsdPropertiesVisible write setXsdPropertiesVisible;
    property stubChanged: Boolean read fStubChanged;
  end;

const
  iiGreenBullet = 20;
  iiRedBullet = 21;
  iiOrangeBullet = 53;
  iiRedCross = 56;
  iiOrangeCross = 54;
  iiRedPlus = 3;
  iiOrangePlus = 55;
  iiTransparant = 57;

type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Row: Integer;
    Binds: TBindableList;
    a2b: PTreeRec;
  end;
  TPasswordEditLink = class(TStringEditLink)
  public
    constructor Create; override;
  end;

const xmlGridMaxBom = 10;

implementation

uses igGlobals
   , FindRegExpDialog
   , Registry
   , ClipBrd
   {$IFnDEF FPC}
   , ShowHtmlUnit
   {$ENDIF}
   , StrUtils
   , xmlUtilz
   , a2bStringListUnit
   , ShowA2BXmlUnit
   , EditRegExpUnit
   , htmlXmlUtilz
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TA2BXmlGridForm.FormCreate(Sender: TObject);
begin
  Grid.NodeDataSize := SizeOf(TTreeRec);
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  ToggleShowAttributeColumnsAction.Checked := IniFile.BooleanByNameDef ['doShowAttributeColumns', True];
  ToggleShowEmptyColunsAction.Checked := IniFile.BooleanByNameDef ['doShowEmptyColumns', True];
  ToggleShowEmptyRowsAction.Checked := IniFile.BooleanByNameDef ['doShowEmptyRows', True];
  XsdPropertiesVisible := IniFile.BooleanByNameDef ['XsdPropertiesVisible', True];
  Lists := TStringList.Create;
  ColumnWidths := TStringList.Create;
  ColumnWidths.Text := IniFile.StringByName['DataGridColumnWidths'];
  fXsdDescr := nil;
end;

procedure TA2BXmlGridForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  SaveSettings;
  IniFile.Save;
  IniFile.Free;
  Lists.Clear;
  Lists.Free;
  ColumnWidths.Free;
  if Assigned (fXsdDescr) then
  begin
    fXsdDescr.Free;
    Xml.ForgetXsd;
  end;
end;

procedure TA2BXmlGridForm.FormShow(Sender: TObject);
  procedure _CreateAttrColumns (aXsdAtt: TXsdAttr);
  begin
    with Grid.Header.Columns.Add do
    begin
      Text := aXsdAtt.Name;
      Width :=
        StrToIntDef ( ColumnWidths.Values [Text]
                    , Width
                    );
      Tag := 0;
      isAttributeColumn [Grid.Header.Columns.Count - 1] := True;
    end;
  end;
  procedure _CreateColumns (aLevel: Integer; aXsd: TXsd);
  var
    x: Integer;
    xVTColumn: TVirtualTreeColumn;
  begin
    if not Assigned(aXsd) then
      Exit;
    if aXsd.sType._DepthBillOfMaterial >= xmlGridMaxBom then
      exit;
    if aLevel > xsdMaxDepthXmlGen then
      exit;
    Inc (aXsd.sType._DepthBillOfMaterial);
    try
      xVTColumn := Grid.Header.Columns.Add;
      with xVTColumn do
      begin
        Text := aXsd.ElementName;
        if (aXsd.sType.ElementDefs.Count > 0)
     {  or (aXsd.sType.AttributeDefs.Count > 0) } then
        begin
          Width := 22;
          Options := Options - [coResizable];
          if aXsd.InitialCollapsed then
            ImageIndex := 27
          else
            ImageIndex := 28;
        end
        else
        begin
          Width :=
            StrToIntDef ( ColumnWidths.Values [Text]
                        , Width
                        );
          Options := Options + [coResizable];
        end;
        Tag := Grid.Header.Columns.Count;
      end;
      for x := 0 to aXsd.sType.AttributeDefs.Count - 1 do
        _CreateAttrColumns (aXsd.sType.AttributeDefs.XsdAttrs [x]);
      for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
        _CreateColumns (aLevel + 1, aXsd.sType.ElementDefs.Xsds [x]);
      xVTColumn.Tag := Grid.Header.Columns.Count - xVTColumn.Tag;
    finally
      Dec (aXsd.sType._DepthBillOfMaterial);
    end;
  end;
begin
  if not Assigned (Xml.Xsd) then
  begin
    fXsdDescr := TXsdDescr.Create;
    CreateXsdFromXml(fXsdDescr, Xml, True);
  end;
  _CreateColumns (0, Xml.Xsd);
  ShowGrid (Xml);
  Screen.Cursor := crDefault;
end;

function TA2BXmlGridForm.getCell(col, row: Integer): TA2BXml;
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  result := xData.Binds.Bindables[col] as TA2BXml;
end;

function TA2BXmlGridForm .getImageIndex (aBind : TA2BXml ): Integer ;
  function _if(aCond: Boolean; trueIndex, falseIndex: Integer): Integer;
  begin
    if aCond then
      result := trueIndex
    else
      result := falseIndex;
  end;
begin
  if aBind.Differs
  or aBind.ThisOneDiffers then
    case aBind.ChangeKind of
      ckAdd:    result := _if (aBind.Ignored, iiOrangePlus, iiRedPlus);
      ckDelete: result := _if (aBind.Ignored, iiOrangeCross, iiRedCross);
    else
      result := _if (aBind.Ignored, iiOrangeBullet, iiRedBullet);
    end
  else
    result := iiGreenBullet;
end;

procedure TA2BXmlGridForm.setCell(col, row: Integer; const Value: TA2BXml);
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  xData.Binds.Bindables[col] := Value;
end;

function TA2BXmlGridForm.getBindColNode(acol: Integer; aNode: PVirtualNode): TA2BXml;
begin
  try
    result := PTreeRec (Grid.GetNodeData(aNode)).Binds.Bindables[acol] as TA2BXml;
  except
    result := nil;
  end;
end;

procedure TA2BXmlGridForm.setBindColNode(acol: Integer; aNode: PVirtualNode;
  const Value: TA2BXml);
var
  xData: PTreeRec;
begin
  xData := Grid.GetNodeData(aNode);
  xData.Binds.Bindables[acol] := Value;
end;

function TA2BXmlGridForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TA2BXmlGridForm.setReadOnly(const Value: Boolean);
begin
  fReadOnly := Value;
  Grid.ParentColor := Value;
  if not Value then
  begin
    Grid.Color := clWindow;
    Grid.Colors.GridLineColor := clBtnFace;
  end
  else
    Grid.Colors.GridLineColor := clBtnHighlight;
end;

procedure TA2BXmlGridForm.GridColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
//  Sender.FocusedColumn := Column;
end;

procedure TA2BXmlGridForm.GridFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xData: PTreeRec;
  xBind: TA2BXml;
begin
  StatusPanel.Caption := '';
  xBind := BindColNode [Column, Node];
  if xBind is TA2BXml then
    StatusPanel.Caption := (xBind as TA2BXml).FullCaption;
  Grid.Invalidate;
end;

procedure TA2BXmlGridForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aCol: Integer;
begin
  if (Key = VK_RETURN) then
  begin
    (Sender as TVirtualStringTree).EditNode
      ( (Sender as TVirtualStringTree).FocusedNode
      , (Sender as TVirtualStringTree).FocusedColumn
      );
  end;
{
  if (Key = VK_LEFT) then
  begin
    aCol := (Sender as TVirtualStringTree).FocusedColumn - 1;
    try
      while not ColumnVisible [aCol] do
        Dec (aCol);
    (Sender as TVirtualStringTree).FocusedColumn := aCol;
    except
    end;
  end;
  if (Key = VK_RIGHT) then
  begin
    aCol := (Sender as TVirtualStringTree).FocusedColumn + 1;
    try
      while not ColumnVisible [aCol] do
        Inc (aCol);
    (Sender as TVirtualStringTree).FocusedColumn := aCol;
    except
    end;
  end;
}
end;

procedure TA2BXmlGridForm.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  grid_x := X;
  grid_y := Y;
end;

procedure TA2BXmlGridForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TA2BXmlGridForm.GridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ColumnSpan [Column] = 0 then Exit;
  GroupExpanded [Column] := not GroupExpanded [Column];
  ShowHideColumns;
  Grid.FocusedColumn := Column;
  headerClicked := True;
end;

procedure TA2BXmlGridForm.SetNodesVisibilty;
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
    and (c < xData.Binds.Count) do
    begin
      xVisible := Assigned (xData.Binds.Bindables[c])
              and (   doShowEmptyRows
                   or xData.Binds.Bindables[c].CheckedAllUp
                  )
              and (coVisible in Grid.Header.Columns.Items[c].Options);
      Inc (c);
    end;
    Grid.IsVisible [xNode] := xVisible;
    xNode := Grid.GetNext(xNode);
  end;
end;

function TA2BXmlGridForm.getColumnSpan(col: Integer): Integer;
begin
  try
    result := Grid.Header.Columns.Items[col].Tag;
  except
    result := 0;
  end;
end;

procedure TA2BXmlGridForm.CancelButtonClick(Sender: TObject);
begin
  fStubChanged := False;
  ModalResult := mrCancel;
end;

procedure TA2BXmlGridForm.ToggleShowEmptyRowsActionExecute(Sender: TObject);
begin
  ToggleShowEmptyRowsAction.Checked := not ToggleShowEmptyRowsAction.Checked;
  ShowGrid (FocusedBind);
end;

function TA2BXmlGridForm.getDoShowEmptyRows: Boolean;
begin
  result := ToggleShowEmptyRowsAction.Checked;
end;

procedure TA2BXmlGridForm.ShowGrid (aFocusBind: TA2BXml);
  function _nRows (aXml: TA2BXml): Integer;
  var
    e, x, n, mx: Integer;
  begin
{
    if (not doShowNills)
    and (not aXml.CheckedAllUp) then
    begin
      result := 0;
      exit;
    end;
}
    if aXml.TypeDef.ElementDefs.Count = 0 then
      result := 1
    else
    begin
      mx := 1;
      n := 0;
      for e := 0 to aXml.TypeDef.ElementDefs.Count - 1 do
      begin
        n := 0;
        for x := 0 to aXml.Items.Count - 1 do
          if (aXml.Items.XmlItems[x].Xsd = aXml.TypeDef.ElementDefs.Xsds [e]) then
            n := n + _nRows (aXml.Items.XmlItems[x] as TA2BXml);
        if n > mx then
          mx := n;
      end;
      result := mx;
    end;
  end;
  procedure _LinkXml (aLevel: Integer; aXml: TA2BXml; aCol, aRow: Integer);
  var
    e, x, r, sCol: Integer;
  begin
    if aXml.TypeDef._DepthBillOfMaterial >= xmlGridMaxBom then
      exit;
    if aLevel > xsdMaxDepthXmlGen then Exit;

    Inc (aXml.TypeDef._DepthBillOfMaterial);
    try
      if aCol > ColumnSpan[0] then Exit;
//      if doShowNills
//      or aXml.CheckedAllUp then
      a2bCell [aCol, aRow] := aXml;
      aCol := aCol + 1;
      if ColumnSpan [aCol - 1] > 0 then // recursive and depth > max
      begin
        for e := 0 to aXml.TypeDef.ElementDefs.Count - 1 do
        begin
          sCol := aCol;
          r := aRow;
          for x := 0 to aXml.Items.Count - 1 do
          begin
            if{ (   doShowNills
                or aXml.Items.XmlItems[x].CheckedAllUp
               )
            and} (aXml.Items.XmlItems[x].Xsd = aXml.TypeDef.ElementDefs.Xsds[e]) then
            begin
              aCol := sCol;
              _LinkXml (aLevel + 1, aXml.Items.XmlItems[x] as TA2BXml, aCol, r);
              r := r + _nRows(aXml.Items.XmlItems[x] as TA2BXml);
            end
          end;
          aCol := sCol + ColumnSpan [sCol] + 1;
        end;
      end;
    finally
      Dec (aXml.TypeDef._DepthBillOfMaterial);
    end;
  end;
var
  x, y, xCol: Integer;
  xNode: PVirtualNode;
  xData: PTreeRec;
begin
  Grid.BeginUpdate;
  try
    CleanUp;
    if Assigned (Xml)
    and Assigned (Xml.Xsd) then
    begin
      Caption := Xml.FullCaption;
      nCols := Grid.Header.Columns.Count;
      nRows := _nRows (Xml);
      for y := 0 to nRows - 1 do
      begin
        xData := Grid.GetNodeData(Grid.AddChild(nil));
        Lists.AddObject('', TObject (xData));
        xData.Row := y;
        xData.Binds := TBindableList.Create;
        for x := 0 to nCols - 1 do
          xData.Binds.AddObject('', nil);
        xData.a2b := Pointer (1);
{
        xData.a2b := Grid.GetNodeData(Grid.AddChild(nil));
        xData.a2b.Binds := xData.Binds;
        xData.a2b.Row := xData.Row;
        xData.a2b.a2b := nil;
}
      end;
      xCol := 0;
      _LinkXml (0, Xml, xCol, 0);
    end;
    ShowHideColumns;
    FocusedBind := aFocusBind;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TA2BXmlGridForm.CleanActionExecute(Sender: TObject);
begin
  if not (FocusedBind is TA2BXml) then Exit;
  (FocusedBind as TA2BXml).Clean(1, xsdMaxDepthBillOfMaterials);
  ShowGrid (FocusedBind);
  fStubChanged := True;
end;

procedure TA2BXmlGridForm.CleanUp;
var
  x: Integer;
  xData: PTreeRec;
begin
  for x := 0 to Lists.Count - 1 do
    PTreeRec(Lists.Objects[x]).Binds.Free;
  Lists.Clear;
  Grid.Clear;
end;

function TA2BXmlGridForm.getColumnVisible(col: Integer): Boolean;
begin
  result := coVisible in Grid.Header.Columns.Items[col].Options;
end;

procedure TA2BXmlGridForm.setColumnVisible(col: Integer; const Value: Boolean);
begin
  if (Value) then
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options + [coVisible]
  else
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options - [coVisible];
end;

function TA2BXmlGridForm.getGroupExpanded(col: Integer): Boolean;
begin
  result := Grid.Header.Columns[col].ImageIndex = 28;
end;

procedure TA2BXmlGridForm.setGroupExpanded(col: Integer; const Value: Boolean);
begin
  if Value then
    Grid.Header.Columns[col].ImageIndex := 28
  else
    Grid.Header.Columns[col].ImageIndex := 27;
end;

procedure TA2BXmlGridForm.ShowHideColumns;
  procedure _ShowHideColumn (aLevel: Integer; aXsd: TXsd; var aCol: Integer; aVisible: Boolean);
    function _ColumnNotEmpty (aCol: Integer): Boolean;
    var
      xNode: PVirtualNode;
      xBind: TA2BXml;
    begin
      result := False;
      xNode := Grid.GetFirst;
      while Assigned (xNode)
      and (not result) do
      begin
        xBind := BindColNode [aCol, xNode];
        result := (Assigned (xBind) and xBind.CheckedAllUp);
        xNode := Grid.GetNext(xNode);
      end;
    end;
  var
    e, xCol: Integer;
  begin
    if aXsd.sType._DepthBillOfMaterial >= xmlGridMaxBom then Exit;
    if aLevel > xsdMaxDepthXmlGen then Exit;
    Inc (aXsd.sType._DepthBillOfMaterial);
    try
      xCol := aCol;
      ColumnVisible [aCol] := aVisible
                          and (   (doShowEmptyColumns)
                               or (_ColumnNotEmpty (aCol))
                              )
                            ;
      Inc (aCol);
      for e := 0 to aXsd.sType.AttributeDefs.Count - 1 do
      begin
        ColumnVisible [aCol] := aVisible
                            and GroupExpanded[xCol]
                            and (   (doShowEmptyColumns)
                                 or (_ColumnNotEmpty (aCol))
                                )
                            and (doShowAttributeColumns)
                              ;
        Inc (aCol);
      end;
      for e := 0 to aXsd.sType.ElementDefs.Count - 1 do
        _ShowHideColumn (aLevel + 1,aXsd.sType.ElementDefs.Xsds [e], aCol, (aVisible and GroupExpanded[xCol]));
    finally
      Dec (aXsd.sType._DepthBillOfMaterial);
    end;
  end;
var
  xCol: Integer;
begin
  xCol := 0;
  _ShowHideColumn(0, Xml.Xsd, xCol, True);
  SetNodesVisibilty;
end;

procedure TA2BXmlGridForm.GridPopupMenuPopup(Sender: TObject);
var
  xSingleSelected: Boolean;
begin
  xSingleSelected := (Grid.SelectedCount < 2);
  CopyMenuItem.Enabled := xSingleSelected
                      and Assigned (FocusedBind)
                        ;
  ShowInGridMenuItem.Enabled := Assigned (FocusedBind)
                            and (FocusedBind is TA2BXml)
                            and (FocusedBind <> Xml)
                            and Assigned ((FocusedBind as TA2BXml).Xsd)
                            and ((FocusedBind as TA2BXml).TypeDef.ElementDefs.Count > 0)
                            and xSingleSelected
                              ;
  ZoomBestTryMenuItem.Enabled := xSingleSelected
                             and Assigned (FocusedBind)
                               ;
  ZoomMenu.Enabled := xSingleSelected
                  and Assigned (FocusedBind)
                    ;
  ZoomasXmlMenuItem.Enabled := xSingleSelected
                           and Assigned (FocusedBind)
                           and (FocusedBind is TA2BXml)
                             ;
  ValidateMenuItem.Enabled := xSingleSelected
                          and Assigned (FocusedBind)
                          and (   (    (FocusedBind is TA2BXml)
                                   and (Assigned ((FocusedBind as TA2BXml).Xsd))
                                   and (not (FocusedBind as TA2BXml).TypeDef._Ficticious)
                                  )
                              );
end;

function TA2BXmlGridForm.getFocusedBind: TA2BXml;
begin
  result := nil;
  if Assigned (Grid.FocusedNode) then
  begin
    result := a2bCell [Grid.FocusedColumn, PTreeRec(Grid.GetNodeData(Grid.FocusedNode)).Row];
  end;
end;

procedure TA2BXmlGridForm.setFocusedBind(const Value: TA2BXml);
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
      if xData.Binds.Bindables[c] = Value then
      begin
        xFound := True;
        Grid.FocusedNode := xNode;
        Grid.FocusedColumn := c;
        Grid.ScrollIntoView(c, True);
        Break;
      end;
    end;
    xNode := Grid.GetNext(xNode);
  end;
end;

procedure TA2BXmlGridForm.ShowHtml(aCaption, aInfoString: String);
begin
  if AnsiLeftStr(aInfoString, 6) <> '<html>' then
    raise Exception.Create('No HTML content');
  {$IFnDEF FPC}
      end;
  Application.CreateForm(TShowHtmlForm, ShowHtmlForm);
  try
    ShowHtmlForm.Caption := aCaption;
    ShowHtmlForm.Html := aInfoString;
    ShowHtmlForm.ShowModal;
  finally
    FreeAndNil (ShowHtmlForm);
  end;
  {$endif}
end;

procedure TA2BXmlGridForm.CreateHtmlReportActionExecute(Sender: TObject);
begin
//  ShowHtml ('', Grid.ContentToHTML(tstVisible, Xml.FullIndexCaption));
  ShowHtml (Xml.FullIndexCaption, CreateHtmlReport);
end;

function TA2BXmlGridForm.CreateHtmlReport: String;
  function _ColorToHtml (aColor: TColor): String;
  var
    s: String;
  begin
    s := IntToHex(aColor, 6);
{
    result := '#' + Copy (s, 5, 2) + Copy (s, 1, 2) + Copy (s, 3, 2);
    result := '#' + Copy (s, 1, 2) + Copy (s, 3, 2) + Copy (s, 5, 2);
    result := '#' + Copy (s, 1, 2) + Copy (s, 5, 2) + Copy (s, 3, 2);
    result := '#' + Copy (s, 3, 2) + Copy (s, 5, 2) + Copy (s, 1, 2);
    result := '#' + Copy (s, 3, 2) + Copy (s, 1, 2) + Copy (s, 5, 2);
}
    result := '#' + Copy (s, 5, 2) + Copy (s, 3, 2) + Copy (s, 1, 2);
  end;
  function _Headers(aXsd: TXsd): TA2BXml;
  var
    col: Integer;
  begin
    result := TA2BXml.CreateAsString('tr', '');
    for col := 0 to Grid.Header.Columns.Count - 1 do
      if ColumnVisible [col] then
        with result.AddXml (TA2BXml.CreateAsString('td', '')) do
          AddXml (TA2BXml.CreateAsString('strong', Grid.Header.Columns[col].Text));
  end;
  function _Details(aNode: PVirtualNode): TXml;
  var
    col: Integer;
    xText: String;
  begin
    xText := '';
    result := TXml.CreateAsString('tr', '');
    with result do
    begin
      for col := 0 to Grid.Header.Columns.Count - 1 do
      begin
        if ColumnVisible [col] then
        begin
          GridGetText(Grid, aNode, col, ttNormal,xText);
{
          if xText = '' then
            xText := '_';
}
          with AddXml (TXml.CreateAsString('td', '')) do
          begin
            AddAttribute
              (TXmlAttribute.CreateAsString
                ( 'bgcolor'
                , _ColorToHtml(XmlGetColor (BindColNode[col, aNode], nil, false))
                )
              );
            AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
            AddXml (TA2BXml.CreateAsString('b', xText));
          end;
        end;
      end;
    end;
  end;
var
  htmlXml: TXml;
  tableXml: TXml;
  xNode: PVirtualNode;
  c, r, x, y, z: Integer;
begin
  htmlXml := TXml.CreateAsString('html', '');
  try
    tableXml := htmlXml.AddXml(TXml.CreateAsString('table', ''));
    tableXml.AddAttribute(TXmlAttribute.CreateAsString('border', '1'));
    tableXml.AddXml(_Headers (Xml.Xsd));
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

procedure TA2BXmlGridForm.CopySpreadSheetFormatActionExecute(Sender: TObject);
  function _Columns (aNode: PVirtualNode): String;
  var
    col: Integer;
    xText: String;
    xSep: String;
  begin
    result := '';
    xSep := '';
    xText := '';
    for col := 0 to Grid.Header.Columns.Count - 1 do
    begin
      if ColumnVisible [col] then
      begin
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
begin
  XmlUtil.PushCursor(crHourGlass);
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
    XmlUtil.PopCursor;
  end;
end;

procedure TA2BXmlGridForm.SetXmlsChecked (aCol: Integer; aChecked: Boolean);
var
  xNode: PVirtualNode;
begin
  xNode := Grid.GetFirstSelected;
  while Assigned (xNode) do
  begin
    if Assigned (BindColNode [aCol, xNode]) then
      BindColNode [aCol, xNode].Checked := aChecked;
    xNode := Grid.GetNextSelected(xNode);
  end;
end;

procedure TA2BXmlGridForm.SetNilActionExecute(Sender: TObject);
begin
end;

procedure TA2BXmlGridForm.GridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TA2BXml;
  xXml: TA2BXml;
  Xml: TA2BXml;
  XmlAttr: TXmlAttribute;
  xChecked: Boolean;
begin
  if (Node = Grid.GetFirst)
  and (Column = 0) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;
  xBind := BindColNode [Column, Node];
  if not Assigned (xBind) then Exit;

  xXml := xBind as TA2BXml;
  if (not Assigned (xXml.Xsd)) then
  begin
    TargetCanvas.Font.Color := clRed {clLtGray}	;
    exit;
  end;
  try
    if Assigned (xXml.Xsd)
    and (StrToIntDef (xXml.Xsd.minOccurs, 0) > 0)
    and Assigned (xXml.Parent)
    and Assigned (TA2BXml(xXml.Parent).Xsd)
    and (TA2BXml(xXml.Parent).TypeDef.ContentModel <> 'Choice')
    then
    begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if xXml.Parent.CheckedAllUp then
        if not xXml.Checked then
          TargetCanvas.Font.Color := clRed {clLtGray}	;
    end;
  except
    ShowMessage (xXml.Xsd.minOccurs);
  end;
end;

procedure TA2BXmlGridForm.GridExit(Sender: TObject);
begin
  Grid.EndEditNode;
end;

procedure TA2BXmlGridForm.CopyToClipBoardActionExecute(Sender: TObject);
begin
  XmlUtil.CopyToClipboard(FocusedBind);
end;

procedure TA2BXmlGridForm.PasteFromClipboardActionExecute(Sender: TObject);
begin
end;

procedure TA2BXmlGridForm.All1Click(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TA2BXml;
begin
  xNode := Grid.GetFirstSelected;
  while Assigned (xNode) do
  begin
    xBind := BindColNode[Grid.FocusedColumn, xNode];
    XmlUtil.Populate(xBind, xvAll);
    xNode := Grid.GetNextSelected(xNode);
  end;
  ShowGrid (FocusedBind);
  fStubChanged := True;
end;

procedure TA2BXmlGridForm.Required1Click(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TA2BXml;
begin
  xNode := Grid.GetFirstSelected;
  while Assigned (xNode) do
  begin
    xBind := BindColNode[Grid.FocusedColumn, xNode];
    XmlUtil.Populate(xBind, xvRequired);
    xNode := Grid.GetNextSelected(xNode);
  end;
  ShowGrid (FocusedBind);
  fStubChanged := True;
end;

procedure TA2BXmlGridForm.ValidateMenuItemClick(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TA2BXml;
begin
  xNode := Grid.GetFirstSelected;
  while Assigned (xNode) do
  begin
    xBind := BindColNode[Grid.FocusedColumn, xNode];
    XmlUtil.Validate(xBind);
    xNode := Grid.GetNextSelected(xNode);
  end;
end;

procedure TA2BXmlGridForm.ViewAsXMLMenuItemClick(Sender: TObject);
begin
  XmlUtil.ViewAsXml(FocusedBind, True);
end;

procedure TA2BXmlGridForm.ShowPropertiesActionExecute(Sender: TObject);
begin
  XsdPropertiesVisible := not XsdPropertiesVisible;
end;

function TA2BXmlGridForm.getXsdPropertiesVisible: Boolean;
begin
  result := ShowPropertiesAction.Checked;
end;

procedure TA2BXmlGridForm.setXsdPropertiesVisible(const Value: Boolean);
begin
  ShowPropertiesAction.Checked := Value;
  MainSplitter.Visible := Value;
end;

procedure TA2BXmlGridForm.ToggleShowEmptyColunsActionExecute(Sender: TObject);
begin
  ToggleShowEmptyColunsAction.Checked := not ToggleShowEmptyColunsAction.Checked;
  ShowHideColumns;
end;

function TA2BXmlGridForm.getDoShowEmptyColumns: Boolean;
begin
  result := ToggleShowEmptyColunsAction.Checked;
end;

procedure TA2BXmlGridForm.SetNotNiActionExecute(Sender: TObject);
begin
end;

function TA2BXmlGridForm.XmlGetColor(aBind, aRefBind: TA2BXml; aBValue: Boolean): TColor;
begin
  if (not Assigned (aBind)) then
  begin
    result := { $BFF5BF } $CCFFCC;
    Exit;
  end;
{
  if aRefBind.IsAncestorOf (aBind)
  or aBind.IsAncestorOf (aRefBind) then
  begin
    if aBValue then
      result := $E3E3BB
    else
      result := $E3E3AA;
    Exit;
  end;
}
  if not aBValue then
    result := Color
  else
    result := clLtGray;
  Result := Color;
end;

procedure TA2BXmlGridForm.ext1Click(Sender: TObject);
begin
  XmlUtil.ZoomAsText(FocusedBind, True);
end;

procedure TA2BXmlGridForm.Base641Click(Sender: TObject);
begin
  XmlUtil.ZoomAsBase64(FocusedBind);
end;

procedure TA2BXmlGridForm.HTML1Click(Sender: TObject);
begin
  {$ifdef SHDOCVW}
  XmlUtil.ZoomAsHtml(FocusedBind);
  {$endif}
end;

function TA2BXmlGridForm.inImageArea: Boolean;
var
  xNode: PVirtualNode;
  xRect: TRect;
  xImageIndex: Integer;
  xGosthed: Boolean;
begin
  xGosthed := False;
  result := False;
  xImageIndex := -1;
  if Assigned (Grid.FocusedNode) then
  begin
    xRect := Grid.GetDisplayRect(Grid.FocusedNode, Grid.FocusedColumn, False, False, False);
    if ((Grid_X - xRect.Left) < 20) then
      GridGetImageIndex ( Grid
                        , Grid.FocusedNode
                        , ikNormal
                        , Grid.FocusedColumn
                        , xGosthed
                        , xImageIndex
                        );
  end;
  result := (xImageIndex > -1);
end;

procedure TA2BXmlGridForm.PDFBase641Click(Sender: TObject);
begin
  XmlUtil.ZoomAsPDF(FocusedBind);
end;

procedure TA2BXmlGridForm.XML1Click(Sender: TObject);
begin
  if Assigned (FocusedBind)
  and (FocusedBind is TA2BXml) then
  begin
    SaveSettings;
    if ((FocusedBind as TA2BXml).Items.Count > 0) then
      XmlUtil.ViewAsXml(FocusedBind, True)
    else
      XmlUtil.ZoomAsXml(FocusedBind, True);
  end;
end;

procedure TA2BXmlGridForm.ToggleShowAttributeColumnsActionExecute(Sender: TObject);
begin
  ToggleShowAttributeColumnsAction.Checked := not ToggleShowAttributeColumnsAction.Checked;
  ShowHideColumns;
end;

function TA2BXmlGridForm.getDoShowAttributeColumns: Boolean;
begin
  result := ToggleShowAttributeColumnsAction.Checked;
end;

function TA2BXmlGridForm.getIsAttributeColumn(col: Integer): Boolean;
begin
  result := (Grid.Header.Columns.Items [col].ImageIndex = 16);
end;

procedure TA2BXmlGridForm.setIsAttributeColumn(col: Integer; const Value: Boolean);
begin
  Grid.Header.Columns.Items [col].ImageIndex := 16;
end;

procedure TA2BXmlGridForm.GridGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: string);
begin
  HintText := Grid.Header.Columns[Column].Text;
end;

procedure TA2BXmlGridForm.GridGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xData: PTreeRec;
  xBind: TA2BXml;
begin
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column] as TA2BXml;
  case Kind of
    ikNormal, ikSelected:
    begin
      if Assigned (xBind) then
      begin
        if Assigned (xData.a2b) then
          ImageIndex := getImageIndex(xBind)
        else
          ImageIndex := iiTransparant;
      end;
      ImageIndex := iiTransparant;
    end;
  end;
end;

procedure TA2BXmlGridForm.GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  xData: PTreeRec;
  xBind: TA2BXml;
  x: Integer;
  xValue: String;
begin
  CellText := '';
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column] as TA2BXml;
  if Assigned (xBind) then
  begin
    if Assigned (xData.a2b) then
      xValue := xBind.Value + LineEnding + xBind.bValue
    else
      xValue := xBind.bValue;
    if xBind is TA2BXml then
    begin
      with xBind as TA2BXml do
      begin
        if TypeDef.ElementDefs.Count > 0 then
          CellText := ''
        else
        begin
          if (xBind is TA2BXml)
          and (xBind.Name <> 'passwordType')
          and Assigned((xBind as TA2BXml).TypeDef)
          and ((xBind as TA2BXml).TypeDef.Name = 'passwordType')
          and (xValue <> '') then
            CellText := '**********'
          else
          begin
            if _xmlLicensed then
              CellText := xValue
            else
            begin
              for x := 1 to Length (xValue) do
                if x < 5 then
                  CellText := CellText + xValue [x]
                else
                  CellText := CellText + '*';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TA2BXmlGridForm.GridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  xData: PTreeRec;
begin
  xData := Grid.GetNodeData(Node);
  with TargetCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := XmlGetColor(BindColNode [Column, Node], FocusedBind, (xData.a2b = nil));
    FillRect( CellRect );
  end;
end;

procedure TA2BXmlGridForm.GridClick(Sender: TObject);
var
  xChanged: Boolean;
  xBind: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  if headerClicked then
  begin
    headerClicked:=False;
    Exit;
  end;
  if (Assigned(Grid.FocusedNode)) then
  begin
    xBind := FocusedBind;
    if inImageArea then
    begin
      SaveSettings;
      if xBind.Items.Count > 0 then
      begin
        Application.CreateForm(TShowA2BXmlForm, xForm);
        try
          xForm.Caption := 'Differences in requests';
          xForm.ignoreDifferencesOn := ignoreDifferencesOn;
          xForm.ignoreAddingOn := ignoreAddingon;
          xForm.ignoreRemovingOn := ignoreRemovingOn;
          xForm.ignoreOrderOn := ignoreOrderOn;
          xForm.regressionSortColumns := regressionSortColumns;
          xForm.Xml := xBind;
          xForm.ShowModal;
          if xForm.RefreshNeeded then
            ShowGrid (Xml);
        finally
          FreeAndNil (xForm);
        end;
      end;
    end;
  end;
end;

procedure TA2BXmlGridForm.SaveSettings;
var
  x: Integer;
begin
  for x := Grid.Header.Columns.Count - 1  downto 0 do
    if ColumnSpan [x] = 0 then
      ColumnWidths.Values [Grid.Header.Columns.Items[x].Text]
      := IntToStr (Grid.Header.Columns.Items[x].Width);
  IniFile.BooleanByName ['doShowAttributeColumns'] := doShowAttributeColumns;
  IniFile.BooleanByName ['doShowEmptyColumns'] := doShowEmptyColumns;
  IniFile.BooleanByName ['doShowEmptyRows'] := doShowEmptyRows;
  IniFile.BooleanByName ['XsdPropertiesVisible'] := XsdPropertiesVisible;
  IniFile.StringByName['DataGridColumnWidths'] := ColumnWidths.Text;
end;

procedure TA2BXmlGridForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  xNode: PVirtualNode;
  xBind: TA2BXml;
  xColumn: TColumnIndex;
  mSelect: Boolean;
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    xNode := Grid.FocusedNode;
    xColumn := Grid.FocusedColumn;
    FindDlg.Caption := 'Find Tag';
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      xmlUtil.SearchString := FindDlg.SearchEdit.Text;
      xmlUtil.SearchUseRegexp := FindDlg.RegularExpressionCheckBox.Checked;
      xmlUtil.SearchScope := FindDlg.ScopeRadioGroup.ItemIndex;
      xmlUtil.SearchIn := FindDlg.SearchInRadioGroup.ItemIndex;
      Found := False;
      if (xmlUtil.SearchScope = 1) then // or search entire scope
        xNode := nil;
      NextVisibleCell(xColumn, xNode);
      while not (xNode = nil)
      and not Found do
      begin
        xBind := BindColNode [xColumn, xNode];
        if Assigned (xBind) then
        begin
          if xmlUtil.SearchIn = 0 then // search tag
            Found := StringMatchesMask ( xBind.Name
                                       , xmlUtil.SearchString
                                       , False
                                       , xmlUtil.SearchUseRegExp
                                       )
          else // search description
            Found := StringMatchesMask ( xBind.Value
                                       , xmlUtil.SearchString
                                       , False
                                       , xmlUtil.SearchUseRegExp
                                       );
        end;
        if not Found then
          NextVisibleCell (xColumn, xNode);
      end;
      if not Found then
        ShowMessage (xmlUtil.SearchString + ' not found')
      else
      begin
        mSelect := (toMultiSelect in Grid.TreeOptions.SelectionOptions);
        Grid.TreeOptions.SelectionOptions := Grid.TreeOptions.SelectionOptions - [toMultiSelect];
        if mSelect then
          Grid.TreeOptions.SelectionOptions := Grid.TreeOptions.SelectionOptions + [toMultiSelect];
        Grid.FocusedColumn := xColumn;
        Grid.FocusedNode := xNode;
        Grid.FocusedColumn := xColumn;
        Grid.Selected [xNode] := True;
      end;
    end;
  finally
    FreeAndNil (FindDlg);
  end;
end;

procedure TA2BXmlGridForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  xNode: PVirtualNode;
  xColumn: TColumnIndex;
  xBind: TA2BXml;
  mSelect: Boolean;
begin
  if True then
  begin
    Found := False;
    xNode := Grid.FocusedNode;
    xColumn := Grid.FocusedColumn;
    NextVisibleCell(xColumn, xNode);
    while Assigned (xNode)
    and not Found do
    begin
      xBind := BindColNode [xColumn, xNode];
      if Assigned (xBind) then
      begin
        if xmlUtil.SearchIn = 0 then // search tag
          Found := StringMatchesMask ( xBind.Name
                                     , xmlUtil.SearchString
                                     , False
                                     , xmlUtil.SearchUseRegExp
                                     )
        else // search description
          Found := StringMatchesMask ( xBind.Value
                                     , xmlUtil.SearchString
                                     , False
                                     , xmlUtil.SearchUseRegExp
                                     )
      end;
      if not Found then
        NextVisibleCell(xColumn, xNode);
    end;
    if not Found then
      ShowMessage (xmlUtil.SearchString + ' not found')
    else
    begin
      mSelect := (toMultiSelect in Grid.TreeOptions.SelectionOptions);
      Grid.TreeOptions.SelectionOptions := Grid.TreeOptions.SelectionOptions - [toMultiSelect];
      if mSelect then
        Grid.TreeOptions.SelectionOptions := Grid.TreeOptions.SelectionOptions + [toMultiSelect];
      Grid.FocusedColumn := xColumn;
      Grid.FocusedNode := xNode;
      Grid.FocusedColumn := xColumn;
      Grid.Selected [xNode] := True;
    end;
  end;
end;

procedure TA2BXmlGridForm.NextVisibleCell(var aColumn: TColumnIndex;
  var aNode: PVirtualNode);
begin
  if Assigned (aNode) then
  begin
    Inc (aColumn);
    if aColumn >= Grid.Header.Columns.Count then
    begin
      aNode := Grid.GetNextVisible(aNode);
      aColumn := 0;
    end;
  end
  else
  begin
    aNode := Grid.GetFirstVisible;
    aColumn := 0;
  end;
  while Assigned (aNode)
  and not (coVisible in Grid.Header.Columns.Items[aColumn].Options) do
  begin
    Inc (aColumn);
    if aColumn >= Grid.Header.Columns.Count then
    begin
      aNode := Grid.GetNextVisible(aNode);
      aColumn := 0;
    end;
  end;
end;

procedure TA2BXmlGridForm.Action1Execute(Sender: TObject);
begin
  ShowHtml (Xml.FullIndexCaption, '<html>' + Grid.ContentToHTML(tstVisible, 'Jan Test') + '</html>');
end;

procedure TA2BXmlGridForm.ZoomActionExecute(Sender: TObject);
begin
  xmlUtil.presentString (FocusedBind.FullCaption, FocusedBind.Value);
end;

procedure TA2BXmlGridForm .GridAfterCellPaint (Sender : TBaseVirtualTree ;
  TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
  const CellRect : TRect );
var
  xData: PTreeRec;
  xBind: TA2BXml;
  r: TRect;
begin
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column] as TA2BXml;
  if xBind is TA2BXml then
  begin
    r := Sender.GetDisplayRect(Node, Column, true);
    ImageList.Draw(TargetCanvas, r.Left - 20, CellRect.Top, getImageIndex(xBind));
  end;
end;

procedure TA2BXmlGridForm .GridInitNode (Sender : TBaseVirtualTree ;
  ParentNode , Node : PVirtualNode ; var InitialStates : TVirtualNodeInitStates
  );
begin
  Include (InitialStates, ivsMultiline);
end;

{ TPasswordEditLink }

constructor TPasswordEditLink.Create;
begin
  inherited;
  Self.Edit.PasswordChar := '*';
end;

end.
