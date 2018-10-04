unit XmlGridUnit;

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
  Dialogs , FormIniFilez , StdCtrls , ExtCtrls , Xsdz , Xmlz , VirtualTrees ,
  ComCtrls , ImgList , ToolWin , ActnList , Menus, IpHtml , Bind
{$IFnDEF FPC}
  , OleCtrls
  , SHDocVw
{$ENDIF}
  ;

type

  { TXmlGridForm }

  TXmlGridForm = class(TForm)
    DocumentationViewer: TIpHtmlPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel2: TPanel;
    Grid: TVirtualStringTree;
    Panel4: TPanel;
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
    PropertiesPanel: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    InWsdlPropertiesListView: TListView;
    InWSdlEnumerationsListView: TListView;
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
    CancelButton: TButton;
    StatusPanel: TPanel;
    CleanAction: TAction;
    CleanMenuItem: TMenuItem;
    procedure DocumentationViewerHotClick(Sender: TObject);
    procedure GridAfterCellPaint (Sender : TBaseVirtualTree ;
      TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
      const CellRect : TRect );
    procedure GridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuItem2Click(Sender: TObject);
    procedure OkButtonClick (Sender : TObject );
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
    procedure DeleteActionExecute(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure GridExit(Sender: TObject);
    procedure EditInPopUpActionExecute(Sender: TObject);
    procedure GridPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure SetNilActionExecute(Sender: TObject);
    procedure CopySpreadSheetFormatActionExecute(Sender: TObject);
    procedure CreateHtmlReportActionExecute(Sender: TObject);
    procedure GridPopupMenuPopup(Sender: TObject);
    procedure ShowInGridActionExecute(Sender: TObject);
    procedure ToggleShowEmptyRowsActionExecute(Sender: TObject);
    procedure GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GridColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure GridGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure GridCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
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
    fGridForm: TXmlGridForm;
    fStubChanged: Boolean;
    fReadOnly: Boolean;
    function inImageArea: Boolean;
    procedure NextVisibleCell (var aColumn: TColumnIndex; var aNode: PVirtualNode);
    function getIsAttributeColumn(col: Integer): Boolean;
    procedure setIsAttributeColumn(col: Integer; const Value: Boolean);
    function getDoShowAttributeColumns: Boolean;
    function XmlGetColor (aBind, aRefBind: TCustomBindable): TColor;
    function getBindColNode(acol: Integer; aNode: PVirtualNode): TCustomBindable;
    procedure setBindColNode(acol: Integer; aNode: PVirtualNode; const Value: TCustomBindable);
    function getDoShowEmptyColumns: Boolean;
    function getXsdPropertiesVisible: Boolean;
    procedure setXsdPropertiesVisible(const Value: Boolean);
    function CreateHtmlReport: String;
    function getFocusedBind: TCustomBindable;
    procedure ShowHtml(aCaption, aInfoString: String);
    procedure setFocusedBind(const Value: TCustomBindable);
    procedure ShowHideColumns;
    function getGroupExpanded(col: Integer): Boolean;
    procedure setGroupExpanded(col: Integer; const Value: Boolean);
    procedure setColumnVisible(col: Integer; const Value: Boolean);
    function getColumnVisible(col: Integer): Boolean;
    procedure CleanUp;
    procedure ShowGrid (aFocusBind: TCustomBindable);
    function getDoShowEmptyRows: Boolean;
    procedure CheckValueAgainstXsd (aXml: TXml);
    function getColumnSpan(col: Integer): Integer;
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    procedure setCell(col, row: Integer; const Value: TCustomBindable);
    function getCell(col, row: Integer): TCustomBindable;
    procedure SetNodesVisibilty;
    procedure SetXmlsChecked (aCol: Integer; aChecked: Boolean);
    procedure SaveSettings;
    property FocusedBind: TCustomBindable read getFocusedBind write setFocusedBind;
    property Cell [col, row: Integer]: TCustomBindable read getCell write setCell;
    property BindColNode [acol: Integer; aNode: PVirtualNode]: TCustomBindable read getBindColNode write setBindColNode;
    property ColumnSpan [col: Integer]: Integer read getColumnSpan;
    property GroupExpanded [col: Integer]: Boolean read getGroupExpanded write setGroupExpanded;
    property isAttributeColumn [col: Integer]: Boolean read getIsAttributeColumn write setIsAttributeColumn;
    property ColumnVisible [col: Integer]: Boolean read getColumnVisible write setColumnVisible;
  public
    Xml: TXml;
    doConfirmRemovals: Boolean;
    doShowCancelButton: Boolean;
    initialExpandStyle: TBindExpandStyle;
    ValidateDuplicatesOn: String;
    ProgName: String;
    property doShowAttributeColumns: Boolean read getDoShowAttributeColumns;
    property doShowEmptyColumns: Boolean read getDoShowEmptyColumns;
    property doShowEmptyRows: Boolean read getDoShowEmptyRows;
    property isReadOnly: Boolean read getReadOnly write setReadOnly;
    property XsdPropertiesVisible: Boolean read getXsdPropertiesVisible write setXsdPropertiesVisible;
    property stubChanged: Boolean read fStubChanged;
  end;

type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Row: Integer;
    Binds: TBindableList;
  end;
  TPasswordEditLink = class(TStringEditLink)
  public
    constructor Create; override;
  end;

var
  XmlGridForm: TXmlGridForm;

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
   , PromptUnit
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TXmlGridForm.FormCreate(Sender: TObject);
begin
  ProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  DocumentationViewer.Color := Self.Color;
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
end;

procedure TXmlGridForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  SaveSettings;
  IniFile.Save;
  IniFile.Free;
  Lists.Clear;
  Lists.Free;
  ColumnWidths.Free;
end;

procedure TXmlGridForm.FormShow(Sender: TObject);
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
  CancelButton.Visible := doShowCancelButton;
  if isReadOnly then
  begin
    OkButton.Caption := '&Close';
    CancelButton.Visible := False;
  end;
  if not CancelButton.Visible then
  begin
    CancelButton.Cancel := False;
    OkButton.Cancel := True;
  end;
  _CreateColumns (0, Xml.Xsd);
  ShowGrid (Xml);
  Screen.Cursor := crDefault;
end;

function TXmlGridForm.getCell(col, row: Integer): TCustomBindable;
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  result := xData.Binds.Bindables[col];
end;

procedure TXmlGridForm.setCell(col, row: Integer; const Value: TCustomBindable);
var
  xData: PTreeRec;
begin
  xData := PTreeRec(Lists.Objects[row]);
  xData.Binds.Bindables[col] := Value;
end;

function TXmlGridForm.getBindColNode(acol: Integer; aNode: PVirtualNode): TCustomBindable;
begin
  try
    result := PTreeRec (Grid.GetNodeData(aNode)).Binds.Bindables[acol];
  except
    result := nil;
  end;
end;

procedure TXmlGridForm.setBindColNode(acol: Integer; aNode: PVirtualNode;
  const Value: TCustomBindable);
var
  xData: PTreeRec;
begin
  xData := Grid.GetNodeData(aNode);
  xData.Binds.Bindables[acol] := Value;
end;

function TXmlGridForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TXmlGridForm.setReadOnly(const Value: Boolean);
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

procedure TXmlGridForm.GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  xCol, xRow: Integer;
  xData: PTreeRec;
  xBind: TCustomBindable;
begin
  Allowed := False;
//if fReadOnly then Exit;
  xData := Grid.GetNodeData(Node);
  if not Assigned (xData) then Exit;
  xBind := Cell [Column, xData.Row];
  if not Assigned (xBind) then Exit;
  if xBind is TXml then
    Allowed := Assigned ((xBind as TXml).Xsd)
           and ((xBind as TXml).TypeDef.ElementDefs.Count = 0)
  else
    Allowed := (xBind is TXmlAttribute)
             ;
end;

procedure TXmlGridForm.GridColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
//  Sender.FocusedColumn := Column;
end;

procedure TXmlGridForm.GridCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  Bind: TCustomBindable;
begin
  Bind := BindColNode [Column, Node];
  if (Bind is TXml)
  and Assigned ((Bind as TXml).Xsd)
  and ((Bind as TXml).TypeDef.ElementDefs.Count = 0)
  and ((Bind as TXml).TypeDef.Name = 'passwordType')
  then
    EditLink := TPasswordEditLink.Create
  else
    EditLink := TStringEditLink.Create;
end;

procedure TXmlGridForm.GridFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xData: PTreeRec;
  xBind: TCustomBindable;
begin
  StatusPanel.Caption := '';
  xBind := BindColNode [Column, Node];
  XmlUtil.ListXsdProperties(InWsdlPropertiesListView, xBind);
  XmlUtil.ListXsdEnumerations(InWsdlEnumerationsListView, xBind);
  XmlUtil.ListXsdDocumentation(DocumentationViewer, xBind, False, False);
  if xBind is TXml then
    StatusPanel.Caption := (xBind as TXml).FullCaption;
  Grid.Invalidate;
end;

procedure TXmlGridForm.GridKeyDown(Sender: TObject; var Key: Word;
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

procedure TXmlGridForm.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  grid_x := X;
  grid_y := Y;
end;

procedure TXmlGridForm.GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
  xData: PTreeRec;
begin
  if fReadOnly then Exit;
  xNode := Grid.GetFirst;
  while Assigned (xNode) do
  begin
    if Grid.Selected [xNode] then
    begin
      xData := Grid.GetNodeData(xNode);
      if Assigned (xData) then
      begin
        xBind := Cell [Column, xData.Row];
        if Assigned (xBind) then
        begin
          if NewText = '&nil' then
          begin
            if xBind.Checked then
            begin
              xBind.Checked := False;
              fStubChanged := True;
            end;
          end
          else
          begin
            if (NewText <> xBind.Value)
            or (not xBind.CheckedAllUp) then
            begin
              xBind.Value := NewText;
              xBind.Checked := True;
              fStubChanged := True;
            end;
          end;
        end;
      end;
      Grid.InvalidateNode(xNode);
    end;
    xNode := Grid.GetNext(xNode);
  end;
end;

procedure TXmlGridForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TXmlGridForm.SetNodesVisibilty;
var
  xNode: PVirtualNode;
  xData: PTreeRec;
  xVisible: Boolean;
  r, c: Integer;
begin
  xNode := Grid.GetFirst;
  while Assigned (xNode) do
  begin
    xData := Grid.GetNodeData(xNode);
    xVisible := False;
    r := xData.Row;
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

function TXmlGridForm.getColumnSpan(col: Integer): Integer;
begin
  try
    result := Grid.Header.Columns.Items[col].Tag;
  except
    result := 0;
  end;
end;

procedure TXmlGridForm.CancelButtonClick(Sender: TObject);
begin
  fStubChanged := False;
  ModalResult := mrCancel;
end;

procedure TXmlGridForm.CheckValueAgainstXsd(aXml: TXml);
var
  xMessage: String;
begin
{
  if Assigned (XmlAttr) then
  begin
    if not XmlAttr.IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
  end
  else
  begin
}
    if not aXml.IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
{
  end;
}
end;

procedure TXmlGridForm.GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  xBind: TCustomBindable;
  xData: PTreeRec;
begin
  if fReadOnly then Exit;
  xData := Grid.GetNodeData(Node);
  if not Assigned (xData) then Exit;
  xBind := Cell [Column, xData.Row];
  if xBind is TXml then
    if (xBind as TXml).Checked then
      CheckValueAgainstXsd(xBind as TXml);
end;

procedure TXmlGridForm.ToggleShowEmptyRowsActionExecute(Sender: TObject);
begin
  ToggleShowEmptyRowsAction.Checked := not ToggleShowEmptyRowsAction.Checked;
  ShowGrid (FocusedBind);
end;

function TXmlGridForm.getDoShowEmptyRows: Boolean;
begin
  result := ToggleShowEmptyRowsAction.Checked;
end;

procedure TXmlGridForm.ShowGrid (aFocusBind: TCustomBindable);
  function _nRows (aXml: TXml): Integer;
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
            n := n + _nRows (aXml.Items.XmlItems[x]);
        if n > mx then
          mx := n;
      end;
      result := mx;
    end;
  end;
  procedure _LinkXml (aLevel: Integer; aXml: TXml; aCol, aRow: Integer);
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
      Cell [aCol, aRow] := aXml;
      aCol := aCol + 1;
      if ColumnSpan [aCol - 1] > 0 then // recursive and depth > max
      begin
        for e := 0 to aXml.TypeDef.AttributeDefs.Count - 1 do
        begin
          for x := 0 to aXml.Attributes.Count - 1 do
            if aXml.TypeDef.AttributeDefs.XsdAttrs[e] = aXml.Attributes.XmlAttributes[x].XsdAttr then
              Cell [aCol, aRow] := aXml.Attributes.XmlAttributes[x];
          Inc (aCol);
        end;
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
              _LinkXml (aLevel + 1, aXml.Items.XmlItems[x], aCol, r);
              r := r + _nRows(aXml.Items.XmlItems[x]);
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

procedure TXmlGridForm.CleanActionExecute(Sender: TObject);
begin
  if not (FocusedBind is TXml) then Exit;
  (FocusedBind as TXml).Clean(1, xsdMaxDepthBillOfMaterials);
  ShowGrid (FocusedBind);
  fStubChanged := True;
end;

procedure TXmlGridForm.CleanUp;
var
  x: Integer;
  xData: PTreeRec;
begin
  for x := 0 to Lists.Count - 1 do
    PTreeRec(Lists.Objects[x]).Binds.Free;
  Lists.Clear;
  Grid.Clear;
end;

function TXmlGridForm.getColumnVisible(col: Integer): Boolean;
begin
  result := coVisible in Grid.Header.Columns.Items[col].Options;
end;

procedure TXmlGridForm.setColumnVisible(col: Integer; const Value: Boolean);
begin
  if (Value) then
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options + [coVisible]
  else
    Grid.Header.Columns[col].Options := Grid.Header.Columns[Col].Options - [coVisible];
end;

function TXmlGridForm.getGroupExpanded(col: Integer): Boolean;
begin
  result := Grid.Header.Columns[col].ImageIndex = 28;
end;

procedure TXmlGridForm.setGroupExpanded(col: Integer; const Value: Boolean);
begin
  if Value then
    Grid.Header.Columns[col].ImageIndex := 28
  else
    Grid.Header.Columns[col].ImageIndex := 27;
end;

procedure TXmlGridForm.ShowHideColumns;
  procedure _ShowHideColumn (aLevel: Integer; aXsd: TXsd; var aCol: Integer; aVisible: Boolean);
    function _ColumnNotEmpty (aCol: Integer): Boolean;
    var
      xNode: PVirtualNode;
      xBind: TCustomBindable;
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

procedure TXmlGridForm.ShowInGridActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not (FocusedBind is TXml) then Exit;
  Application.CreateForm(TXmlGridForm, fGridForm);
  try
    fGridForm.isReadOnly := isReadOnly;
    fGridForm.doConfirmRemovals := doConfirmRemovals;
    fGridForm.doShowCancelButton := doShowCancelButton;
    fGridForm.initialExpandStyle := initialExpandStyle;
    if isReadOnly then
    begin
      fGridForm.Xml := FocusedBind as TXml;
      fGridForm.ShowModal;
    end
    else
    begin
      xXml.LoadValues(FocusedBind as TXml, True, True);
      try
        xXml.Checked := True;
        xXml.Name := (FocusedBind as TXml).Name;
        fGridForm.Xml := xXml;
        fGridForm.ShowModal;
        if fGridForm.StubChanged then
        begin
          (FocusedBind as TXml).ResetValues;
          (FocusedBind as TXml).LoadValues(xXml, True, True);
          ShowGrid (FocusedBind);
        end;
      finally
        FreeAndNil(xXml);
      end;
    end;
    fStubChanged := fStubChanged or fGridForm.StubChanged;
  finally
    FreeAndNil (fGridForm);
  end;
end;

procedure TXmlGridForm.GridPopupMenuPopup(Sender: TObject);
var
  xSingleSelected: Boolean;
begin
  xSingleSelected := (Grid.SelectedCount < 2);
  AddMenuItem.Enabled := (not isReadOnly)
                     and xSingleSelected
                     and (FocusedBind <> Xml)
                     and XmlUtil.isAddAllowed(FocusedBind, false);
  DeleteMenuItem.Enabled := (not isReadOnly)
                        and xSingleSelected
                        and (FocusedBind <> Xml)
                        and XmlUtil.isDeleteAllowed(FocusedBind, false);
  CleanMenuItem.Enabled := (not isReadOnly)
                       and xSingleSelected
                       and (FocusedBind is TXml)
                       ;
  SetNilMenuItem.Enabled := (not isReadOnly)
                        and (   (not xSingleSelected)
                             or Assigned (FocusedBind)
                            )
                          ;
  SetNotNilMenuItem.Enabled := SetNilMenuItem.Enabled;
  CopyMenuItem.Enabled := xSingleSelected
                      and Assigned (FocusedBind)
                        ;
  PasteMenuItem.Enabled := (not isReadOnly)
                       and xSingleSelected
                       and Assigned (FocusedBind)
                         ;
  PasteMenuItem.Enabled := (not isReadOnly)
                       and Assigned (FocusedBind)
                         ;
  PopulateMenuItem.Enabled := (not isReadOnly);
  EditInPopUpMenuItem.Enabled := XmlUtil.isEditSupported(FocusedBind)
                             and (not isReadOnly)
                             and xSingleSelected
                             ;
  ShowInGridMenuItem.Enabled := Assigned (FocusedBind)
                            and (FocusedBind is Txml)
                            and (FocusedBind <> Xml)
                            and Assigned ((FocusedBind as TXml).Xsd)
                            and ((FocusedBind as TXml).TypeDef.ElementDefs.Count > 0)
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
                           and (FocusedBind is TXml)
                             ;
  ValidateMenuItem.Enabled := xSingleSelected
                          and Assigned (FocusedBind)
                          and (   (    (FocusedBind is TXml)
                                   and (Assigned ((FocusedBind as TXml).Xsd))
                                   and (not (FocusedBind as TXml).TypeDef._Ficticious)
                                  )
                               or (    (FocusedBind is TXmlAttribute)
                                   and (Assigned ((FocusedBind as TXmlAttribute).XsdAttr))
                                   and (not (FocusedBind as TXmlAttribute).XsdAttr._Ficticious)
                                  )
                              );
end;

function TXmlGridForm.getFocusedBind: TCustomBindable;
begin
  result := nil;
  if Assigned (Grid.FocusedNode) then
  begin
    result := Cell [Grid.FocusedColumn, PTreeRec(Grid.GetNodeData(Grid.FocusedNode)).Row];
  end;
end;

procedure TXmlGridForm.setFocusedBind(const Value: TCustomBindable);
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

procedure TXmlGridForm.ShowHtml(aCaption, aInfoString: String);
begin
  if AnsiLeftStr(aInfoString, 6) <> '<html>' then
    raise Exception.Create('No HTML content');
  {$IFnDEF FPC}
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

procedure TXmlGridForm.CreateHtmlReportActionExecute(Sender: TObject);
begin
//  ShowHtml ('', Grid.ContentToHTML(tstVisible, Xml.FullIndexCaption));
  ShowHtml (Xml.FullIndexCaption, CreateHtmlReport);
end;

function TXmlGridForm.CreateHtmlReport: String;
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
  function _Headers(aXsd: TXsd): TXml;
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
    col: Integer;
    xText: String;
  begin
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
                , _ColorToHtml(XmlGetColor (BindColNode[col, aNode], nil))
                )
              );
            AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
            AddXml (TXml.CreateAsString('b', xText));
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

procedure TXmlGridForm.CopySpreadSheetFormatActionExecute(Sender: TObject);
  function _Columns (aNode: PVirtualNode): String;
  var
    col: Integer;
    xText: String;
    xSep: String;
  begin
    result := '';
    xSep := '';
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

procedure TXmlGridForm.SetXmlsChecked (aCol: Integer; aChecked: Boolean);
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

procedure TXmlGridForm.SetNilActionExecute(Sender: TObject);
begin
  if isReadOnly then
    raise Exception.Create ('not allowed because form is in read-only mode');
  SetXmlsChecked(Grid.FocusedColumn, False);
  fStubChanged := True;
  ShowGrid (FocusedBind);
end;

procedure TXmlGridForm.GridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TCustomBindable;
  xXml: TXml;
  Xml: TXml;
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
  if xBind is TXmlAttribute then Exit;

  xXml := xBind as TXml;
  if (not Assigned (xXml.Xsd)) then
  begin
    TargetCanvas.Font.Color := clRed {clLtGray}	;
    exit;
  end;
  try
    if Assigned (xXml.Xsd)
    and (StrToIntDef (xXml.Xsd.minOccurs, 0) > 0)
    and Assigned (xXml.Parent)
    and Assigned (TXml(xXml.Parent).Xsd)
    and (TXml(xXml.Parent).TypeDef.ContentModel <> 'Choice')
    then
    begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if xXml.Parent.CheckedAllUp then
        if not xXml.Checked then
          TargetCanvas.Font.Color := clRed {clLtGray}	;
    end;
    if xXml.Checked
    and xXml.isValueLink then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
  except
    ShowMessage (xXml.Xsd.minOccurs);
  end;
end;

procedure TXmlGridForm.EditInPopUpActionExecute(Sender: TObject);
begin
  if (not isReadOnly)
  and XmlUtil.isEditSupported (FocusedBind) then
  begin
    if XmlUtil.editXml (FocusedBind, True, isReadOnly) then
    begin
      GridNewText ( Grid
                  , Grid.FocusedNode
                  , Grid.FocusedColumn
                  , XmlUtil.NewValue
                  );
    end;
  end;
end;

procedure TXmlGridForm.GridExit(Sender: TObject);
begin
  Grid.EndEditNode;
end;

procedure TXmlGridForm.AddActionExecute(Sender: TObject);
begin
  ShowGrid(AddSibbling(FocusedBind as TXml));
  fStubChanged := True;
end;

procedure TXmlGridForm.DeleteActionExecute(Sender: TObject);
begin
  if FocusedBind as TXml = Xml then
    raise Exception.Create('Not allowed to delete root');
  ShowGrid (xmlUtil.Delete(FocusedBind as TXml));
  fStubChanged := True;
end;

procedure TXmlGridForm.CopyToClipBoardActionExecute(Sender: TObject);
begin
  XmlUtil.CopyToClipboard(FocusedBind);
end;

procedure TXmlGridForm.PasteFromClipboardActionExecute(Sender: TObject);
begin
  xmlUtil.PasteFromClipboard(FocusedBind);
  ShowGrid (FocusedBind);
  fStubChanged := True;
end;

procedure TXmlGridForm.All1Click(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
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

procedure TXmlGridForm.Required1Click(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
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

procedure TXmlGridForm.ValidateMenuItemClick(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  xNode := Grid.GetFirstSelected;
  while Assigned (xNode) do
  begin
    xBind := BindColNode[Grid.FocusedColumn, xNode];
    XmlUtil.Validate(xBind);
    xNode := Grid.GetNextSelected(xNode);
  end;
end;

procedure TXmlGridForm.ViewAsXMLMenuItemClick(Sender: TObject);
begin
  XmlUtil.ViewAsXml(FocusedBind, isReadOnly);
end;

procedure TXmlGridForm.ShowPropertiesActionExecute(Sender: TObject);
begin
  XsdPropertiesVisible := not XsdPropertiesVisible;
end;

function TXmlGridForm.getXsdPropertiesVisible: Boolean;
begin
  result := ShowPropertiesAction.Checked;
end;

procedure TXmlGridForm.setXsdPropertiesVisible(const Value: Boolean);
begin
  ShowPropertiesAction.Checked := Value;
  PropertiesPanel.Visible := Value;
  MainSplitter.Visible := Value;
end;

procedure TXmlGridForm.ToggleShowEmptyColunsActionExecute(Sender: TObject);
begin
  ToggleShowEmptyColunsAction.Checked := not ToggleShowEmptyColunsAction.Checked;
  ShowHideColumns;
end;

function TXmlGridForm.getDoShowEmptyColumns: Boolean;
begin
  result := ToggleShowEmptyColunsAction.Checked;
end;

procedure TXmlGridForm.SetNotNiActionExecute(Sender: TObject);
begin
  if isReadOnly then
    raise Exception.Create ('not allowed because form is in read-only mode');
  SetXmlsChecked(Grid.FocusedColumn, True);
  fStubChanged := True;
  ShowGrid (FocusedBind);
end;

function TXmlGridForm.XmlGetColor(aBind, aRefBind: TCustomBindable): TColor;
begin
  if (not Assigned (aBind)) then
  begin
    result := { $BFF5BF } $CCFFCC;
    Exit;
  end;
  if not aBind.CheckedAllUp then
  begin
    result := $AAFFFF;
    Exit;
  end;
  if aRefBind.IsAncestorOf (aBind)
  or aBind.IsAncestorOf (aRefBind) then
  begin
    result := { $D6E890  $DEE9D0  $EDEDE8 } $E3E3AA;
    Exit;
  end;
  if isReadOnly then
  begin
    if Assigned (aRefBind)
    and (aBind <> aRefBind)
    and (aBind.Value <> '')
    and (aBind.Value = aRefBind.Value) then
      result := { $C0C0C0 } clLime
    else
      result := Color;
  end
  else
    result := clWhite;
end;

procedure TXmlGridForm.ext1Click(Sender: TObject);
begin
  XmlUtil.ZoomAsText(FocusedBind, isReadOnly);
end;

procedure TXmlGridForm.Base641Click(Sender: TObject);
begin
  XmlUtil.ZoomAsBase64(FocusedBind);
end;

procedure TXmlGridForm.HTML1Click(Sender: TObject);
begin
  {$ifdef SHDOCVW}
  XmlUtil.ZoomAsHtml(FocusedBind);
  {$endif}
end;

function TXmlGridForm.inImageArea: Boolean;
var
  xNode: PVirtualNode;
  xRect: TRect;
  xImageIndex: Integer;
  xGosthed: Boolean;
begin
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

procedure TXmlGridForm.PDFBase641Click(Sender: TObject);
begin
  XmlUtil.ZoomAsPDF(FocusedBind);
end;

procedure TXmlGridForm.XML1Click(Sender: TObject);
begin
  if Assigned (FocusedBind)
  and (FocusedBind is TXml) then
  begin
    SaveSettings;
    if ((FocusedBind as TXml).Items.Count > 0) then
      XmlUtil.ViewAsXml(FocusedBind, isReadOnly)
    else
      XmlUtil.ZoomAsXml(FocusedBind, isReadOnly);
  end;
end;

procedure TXmlGridForm.ToggleShowAttributeColumnsActionExecute(Sender: TObject);
begin
  ToggleShowAttributeColumnsAction.Checked := not ToggleShowAttributeColumnsAction.Checked;
  ShowHideColumns;
end;

function TXmlGridForm.getDoShowAttributeColumns: Boolean;
begin
  result := ToggleShowAttributeColumnsAction.Checked;
end;

function TXmlGridForm.getIsAttributeColumn(col: Integer): Boolean;
begin
  result := (Grid.Header.Columns.Items [col].ImageIndex = 16);
end;

procedure TXmlGridForm.setIsAttributeColumn(col: Integer; const Value: Boolean);
begin
  Grid.Header.Columns.Items [col].ImageIndex := 16;
end;

procedure TXmlGridForm.GridGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: string);
begin
  HintText := Grid.Header.Columns[Column].Text;
end;

procedure TXmlGridForm.GridGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xData: PTreeRec;
  xBind: TCustomBindable;
begin
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column];
  case Kind of
    ikNormal, ikSelected:
    begin
      if xBind is TXmlAttribute then
        ImageIndex := -1
      else
      begin
        if XmlUtil.isExtendAdviced (xBind) then
        begin
          ImageIndex := 45;
          exit;
        end;
        if xmlUtil.isBoolean(xBind) then
        begin
          if (xBind.Value = 'true')
          or (xBind.Value = '1') then
            ImageIndex := 41
          else
            ImageIndex := 40;
          exit;
        end;
        if XmlUtil.isDateTime (xBind) then
        begin
          ImageIndex := 42;
          exit;
        end;
        if XmlUtil.isDate (xBind) then
        begin
          ImageIndex := 43;
          exit;
        end;
        if XmlUtil.isTime (xBind) then
        begin
          ImageIndex := 44;
          exit;
        end;
        if XmlUtil.isEditSupported (xBind) then
        begin
          ImageIndex := 26;
          exit;
        end;
       if (xBind is TXml)
        and ((xBind as TXml).TypeDef.ElementDefs.Count > 0) then
        begin
          if XmlUtil.isGridAdviced (xBind) then
            ImageIndex := 32
          else
          begin
            if XmlUtil.isTreeAdviced (xBind) then
              ImageIndex := 37
            else
              ImageIndex := -1;
          end;
        end;
      end;
    end; {Kind in ikNormal, ikSelected}
  end; {case Kind}
end;

procedure TXmlGridForm.GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  xData: PTreeRec;
  xBind: TCustomBindable;
  x: Integer;
begin
  CellText := '';
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column];
  if Assigned (xBind) then
  begin
    if xBind is TXml then
    begin
      with xBind as TXml do
      begin
        if TypeDef.ElementDefs.Count > 0 then
          CellText := ''
        else
        begin
          if (xBind is TXml)
          and (xBind.Name <> 'passwordType')
          and Assigned((xBind as TXml).TypeDef)
          and ((xBind as TXml).TypeDef.Name = 'passwordType')
          and (xBind.Value <> '') then
            CellText := '**********'
          else
          begin
            if _xmlLicensed
            or (not isReadOnly) then
              CellText := xBind.Value
            else
            begin
              for x := 1 to Length (xBind.Value) do
                if x < 5 then
                  CellText := CellText + xBind.Value [x]
                else
                  CellText := CellText + '*';
            end;
          end;
        end;
      end;
    end;
    if xBind is TXmlAttribute then
      CellText := (xBind as TXmlAttribute).Value;
  end;
end;

procedure TXmlGridForm.GridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  function _decColor (aColor: TColor): TColor;
  begin
    result := aColor;
    if Sender.Selected[Node] then Result := DecColor(Result, 6);
    if Sender.FocusedNode = Node then Result := DecColor(Result, 9);
  end;
begin
  with TargetCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := _decColor(XmlGetColor(BindColNode [Column, Node], FocusedBind));
    FillRect( CellRect );
  end;
end;

procedure TXmlGridForm.GridClick(Sender: TObject);
var
  xChanged: Boolean;
  xBind: TCustomBindable;
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
      if xmlUtil.isExtendAdviced(xBind) then
      begin
        (xBind as TXml).ExtendRecursivity;
        ShowGrid(xBind);
      end
      else
      begin
        xChanged := xmlUtil.editXml (xBind, True, isReadOnly);
        if xChanged then
          ShowGrid (xBind);
        fStubChanged := fStubChanged or xChanged;
      end;
    end
    else
    begin
      Grid.EditNode(Grid.FocusedNode,Grid.FocusedColumn);
    end;
  end;
end;

procedure TXmlGridForm.SaveSettings;
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

procedure TXmlGridForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  xNode: PVirtualNode;
  xBind: TCustomBindable;
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

procedure TXmlGridForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  xNode: PVirtualNode;
  xColumn: TColumnIndex;
  xBind: TCustomBindable;
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

procedure TXmlGridForm.NextVisibleCell(var aColumn: TColumnIndex;
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

procedure TXmlGridForm.Action1Execute(Sender: TObject);
begin
  ShowHtml (Xml.FullIndexCaption, '<html>' + Grid.ContentToHTML(tstVisible, 'Jan Test') + '</html>');
end;

procedure TXmlGridForm.ZoomActionExecute(Sender: TObject);
begin
  xmlUtil.presentString (FocusedBind.FullCaption, FocusedBind.Value);
end;

procedure TXmlGridForm.DocumentationViewerHotClick(Sender: TObject);
begin
  OpenUrl(DocumentationViewer.HotURL);
end;

procedure TXmlGridForm .GridAfterCellPaint (Sender : TBaseVirtualTree ;
  TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
  const CellRect : TRect );
var
  xData: PTreeRec;
  xBind: TCustomBindable;
  r: TRect;
begin
  xData := Grid.GetNodeData(Node);
  xBind := xData.Binds.Bindables[Column];
  if xBind is TXml then with xBind as TXml do
  begin
    if Assigned (Xsd)
    and (Xsd.maxOccurs <> '1') then
    begin
      r := Sender.GetDisplayRect(Node, Column, true);
      ImageList.Draw(TargetCanvas, r.Right - 17, CellRect.Top, 52);
    end;
  end;
end;

procedure TXmlGridForm.GridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ColumnSpan [Column] = 0 then Exit;
  GroupExpanded [Column] := not GroupExpanded [Column];
  ShowHideColumns;
  Grid.FocusedColumn := Column;
//headerClicked := True;
end;

procedure TXmlGridForm.MenuItem2Click(Sender: TObject);
var
  n, w: Integer;
begin
  n := Grid.FocusedColumn;
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Column width';
    PromptForm.PromptEdit.Text := IntToStr (Grid.Header.Columns[n].Width);
    PromptForm.Numeric := True;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
      Grid.Header.Columns[n].Width := StrToInt(PromptForm.PromptEdit.Text);
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TXmlGridForm .OkButtonClick (Sender : TObject );
var
  oBind, dBind: TCustomBindable;
begin
  oBind := nil;
  dBind := nil;
  ModalResult := mrOk;
  if ValidateDuplicatesOn <> '' then
    if not Xml.hasNoDuplicatesOn(ValidateDuplicatesOn, True, oBind, dBind) then
    begin
      ModalResult := mrNone;
      ShowMessageFmt('Dusplicate found on %s: %s', [ValidateDuplicatesOn,
        dBind.Value]);
    end;
end;

{ TPasswordEditLink }

constructor TPasswordEditLink.Create;
begin
  inherited;
  Self.Edit.PasswordChar := '*';
end;

end.
