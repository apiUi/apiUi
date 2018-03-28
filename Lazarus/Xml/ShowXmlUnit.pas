unit ShowXmlUnit;

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
  SysUtils , Classes , Graphics , Forms , Controls , Buttons ,StdCtrls,
  ComCtrls , ExtCtrls , VirtualTrees , Bind , Xmlz , Ipmz , Dialogs ,
  FormIniFilez , ActnList , Menus, IpHtml
{$IFnDEF FPC}
  , OleCtrls
  , SHDocVw
{$ENDIF}
  , Express
  ;

type

  { TShowXmlForm }

  TShowXmlForm = class(TForm)
    CancelButton : TBitBtn ;
    DocumentationViewer: TIpHtmlPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel4: TPanel;
    TreeView: TVirtualStringTree;
    ActionList1: TActionList;
    WriteXmlAction: TAction;
    FindAction: TAction;
    FindNextAction: TAction;
    CopyAction: TAction;
    FullExpandAction: TAction;
    FullCollapseAction: TAction;
    ToolBar1: TToolBar;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton6: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    SaveFileDialog: TSaveDialog;
    TreeViewPopupMenu: TPopupMenu;
    CopyMenuItem: TMenuItem;
    FullExpandMenuItem: TMenuItem;
    FullCollapseMenuItem: TMenuItem;
    NodeCopyAction: TAction;
    NodeFullExpandAction: TAction;
    NodeFullCollapseAction: TAction;
    NodeWriteXmlAction: TAction;
    ActionImageList: TImageList;
    N1: TMenuItem;
    ZoomAsMenuItem: TMenuItem;
    PDFBase641: TMenuItem;
    ZoomAsTextMenuItem: TMenuItem;
    ZoomAsXmlMenuItem: TMenuItem;
    ZoomAsHTMLMenuItem: TMenuItem;
    ViewAsGridMenuItem: TMenuItem;
    B1: TMenuItem;
    Panel2: TPanel;
    Splitter1: TSplitter;
    XsdPropertiesListView: TListView;
    Splitter2: TSplitter;
    PopulateMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    AddAction: TAction;
    AddMenuItem: TMenuItem;
    DeleteAction: TAction;
    DeleteMenuItem: TMenuItem;
    N2: TMenuItem;
    PasteAction: TAction;
    N3: TMenuItem;
    All1: TMenuItem;
    Required1: TMenuItem;
    EditInPopUpMenuItem: TMenuItem;
    ViewinTreeMenuItem: TMenuItem;
    ZoomMenuItem: TMenuItem;
    Panel3: TPanel;
    genDocumentaionAction: TAction;
    CompareAction: TAction;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    ToggleHideAction: TAction;
    HideAction1: TMenuItem;
    ToggleHideButton: TToolButton;
    ToolButton11: TToolButton;
    ToggleShowHiddenAction: TAction;
    ToggleHideXmlNsAction: TAction;
    ToolButton10: TToolButton;
    ExpandLevelAction: TAction;
    ExpandLevelMenuItem: TMenuItem;
    CleanAction: TAction;
    CleanActionMenuItem: TMenuItem;
    ZoomasAssignment1: TMenuItem;
    procedure Button1Click (Sender : TObject );
    procedure DocumentationViewerClick (Sender : TObject );
    procedure DocumentationViewerMouseMove (Sender : TObject ;
      Shift : TShiftState ; X , Y : Integer );
    procedure DocumentationViewerHotClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure TreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
      TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
      const CellRect : TRect );
    procedure TreeViewDblClick(Sender: TObject);
    procedure ZoomMenuItemClick(Sender: TObject);
    procedure ViewinTreeMenuItemClick(Sender: TObject);
    procedure EditInPopUpMenuItemClick(Sender: TObject);
    procedure Required1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure TreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewExit(Sender: TObject);
    procedure TreeViewEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure B1Click(Sender: TObject);
    procedure ViewAsGridMenuItemClick(Sender: TObject);
    procedure ZoomAsHTMLMenuItemClick(Sender: TObject);
    procedure ZoomAsXmlMenuItemClick(Sender: TObject);
    procedure ZoomAsTextMenuItemClick(Sender: TObject);
    procedure PDFBase641Click(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure WriteXmlActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure FullExpandActionExecute(Sender: TObject);
    procedure FullCollapseActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure TreeViewPopupMenuPopup(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure TreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure TreeViewCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure genDocumentaionActionExecute(Sender: TObject);
    procedure XsdDocumentationMemoMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure XsdDocumentationMemoMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure CompareActionUpdate(Sender: TObject);
    procedure CompareActionExecute(Sender: TObject);
    procedure ToggleHideActionUpdate(Sender: TObject);
    procedure ToggleHideActionExecute(Sender: TObject);
    procedure ToggleShowHiddenActionExecute(Sender: TObject);
    procedure ToggleHideXmlNsActionExecute(Sender: TObject);
    procedure ExpandLevelActionExecute(Sender: TObject);
    procedure CleanActionExecute(Sender: TObject);
    procedure ZoomasAssignment1Click(Sender: TObject);
  private
    fIsChanged: Boolean;
    fReadOnly: Boolean;
    fBind: TCustomBindable;
    FileName: String;
    FileContents: TStringList;
    fIsCheckedOnly: Boolean;
    fdoEnableCompare: Boolean;
    fHideNodes: TStringList;
    function getDoHideNodes: Boolean;
    procedure setDoHideNodes(const Value: Boolean);
    function getDoHideXmlNs: Boolean;
    procedure setDoHideXmlNs(const Value: Boolean);
    property doHideXmlNs: Boolean read getDoHideXmlNs write setDoHideXmlNs;
    property doHideNodes: Boolean read getDoHideNodes write setDoHideNodes;
    procedure HaveLink(Sender: TObject; aLink: String);
    procedure FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
    function InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
    procedure ShowBind(Bind: TCustomBindable; aNode: PVirtualNode);
    procedure UpdateTreeViewNode(aTreeView: TVirtualStringTree;
      aNode: PVirtualNode);
    procedure SetBindNodeCheckBox(aTreeView: TVirtualStringTree;
      aBind: TCustomBindable; aNode: PVirtualNode);
    function getReadOnly: Boolean;
    function setVisibility(aBind: TCustomBindable): Boolean;
    procedure setReadOnly(const Value: Boolean);
    procedure HaveString(aString: String);
    function ToolButtonUsed(Sender: TObject): Boolean;
    procedure SetBind(aBind: TCustomBindable);
    function SelectedBind: TCustomBindable;
    procedure RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
    procedure setIsChanged(const Value: Boolean);
    procedure setdoEnableCompare(const Value: Boolean);
  public
    ignoreDifferencesOn, ignoreAddingon, ignoreRemovingOn: TStringList;
    doConfirmRemovals: Boolean;
    doShowCancelButton: Boolean;
    initialExpandStyle: TBindExpandStyle;
    ValidateDuplicatesOn: String;
    InitialFocusOn: String;
    ProgName: String;
    property doEnableCompare: Boolean read fdoEnableCompare write
      setdoEnableCompare;
    property isChanged: Boolean read fIsChanged write setIsChanged;
    property isReadOnly: Boolean read getReadOnly write setReadOnly;
    property isCheckedOnly: Boolean read fIsCheckedOnly write fIsCheckedOnly;
    property Bind: TCustomBindable read fBind write SetBind;
    function NodeToBind(aNode: PVirtualNode): TCustomBindable;
  end;

  TPasswordEditLink = class(TStringEditLink, IVTEditLink)
  public
    property Node: PVirtualNode read fNode;
    function BeginEdit: Boolean; virtual; stdcall;
    constructor Create; override;
  end;

var
  ShowXmlForm: TShowXmlForm;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  FindRegExpDialog, igGlobals, ClipBrd, Xsdz, xmlUtilz, XmlGridUnit,
  RegExpr, StrUtils, A2BXmlz, ShowA2BXmlUnit, PromptUnit;

const
  treeTagColumn = 0;

const
  treeValueColumn = 2;

const
  treeButtonColumn = 1;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  PBindTreeRec = ^TBindTreeRec;

  TBindTreeRec = record
    Bind: TCustomBindable;
  end;

procedure TShowXmlForm.FormCreate(Sender: TObject);
var
  wBttn: Integer;
begin
  fHideNodes := TStringList.Create;
  fHideNodes.Sorted := true;
  ProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  wBttn := TreeView.Header.Columns[treeButtonColumn].Width;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    fHideNodes.Text := StringByName['hideNodes'];
    doHideNodes := BooleanByName['doHideNodes'];
    doHideXmlNs := BooleanByName['doHideXmlNs'];
  finally
    Free;
  end;
  TreeView.Header.Columns[treeButtonColumn].Width := wBttn;
  TreeView.NodeDataSize := SizeOf(TBindTreeRec);
  TreeView.RootNodeCount := 0;
  FileContents := TStringList.Create;
  isChanged := False;
  doConfirmRemovals := True;
end;

procedure TShowXmlForm.TreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  xBind: TCustomBindable;
begin
  Allowed := False;
  // if fReadOnly then Exit;
  if (Column <> treeValueColumn) then
    Exit;
  xBind := NodeToBind(Node);
  Allowed := xmlUtil.isEditAllowed(xBind);
  if Allowed
  and (xBind is TXml)
  and Assigned((xBind as TXml).Xsd)
  and ((xBind as TXml).TypeDef.ElementDefs.Count = 0)
  and ((xBind as TXml).TypeDef.Name = 'passwordType') then
    xBind.Value := DecryptString(xBind.Value);
end;

procedure TShowXmlForm.ShowBind(Bind: TCustomBindable; aNode: PVirtualNode);
var
  ChildNode: PVirtualNode;
  AttributeNode: PVirtualNode;
  Data: PBindTreeRec;
  X: Integer;
begin
  if isCheckedOnly and (not Bind.Checked) then
    Exit;
  ChildNode := TreeView.AddChild(aNode);
  Data := TreeView.GetNodeData(ChildNode);
  Data.Bind := Bind;
  SetBindNodeCheckBox(TreeView, Bind, ChildNode);
  if Bind is TXml then
  begin
    for X := 0 to (Bind as TXml).Attributes.Count - 1 do
    begin
      if (not isCheckedOnly) or ((Bind as TXml).Attributes.XmlAttributes[X])
        .Checked then
      begin
        AttributeNode := TreeView.AddChild(ChildNode);
        Data := TreeView.GetNodeData(AttributeNode);
        AttributeNode.CheckType := ctCheckBox;
        Data.Bind := (Bind as TXml).Attributes.XmlAttributes[X];
        if (Bind as TXml).Attributes.XmlAttributes[X].Checked then
          AttributeNode.CheckState := csCheckedNormal
        else
          AttributeNode.CheckState := csUnCheckedNormal;
      end;
    end;
  end;
  for X := 0 to Bind.Children.Count - 1 do
  begin
    ShowBind(Bind.Children.Bindables[X], ChildNode);
  end;
end;

procedure TShowXmlForm.SetBind(aBind: TCustomBindable);
var
  theNode: PVirtualNode;
begin
  fBind := aBind;
  TreeView.Clear;
  if aBind = nil then
    Exit;
  ShowBind(aBind, nil);
  theNode := TreeView.GetFirst;
  if xmlUtil.doExpandFull then
    TreeView.FullExpand(theNode)
  else
  begin
    TreeView.FullCollapse(theNode);
    TreeView.Expanded[theNode] := True;
  end;
  TreeView.Selected[theNode] := True;
  TreeView.FocusedNode := theNode;
end;

procedure TShowXmlForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['hideNodes'] := fHideNodes.Text;
    BooleanByName['doHideNodes'] := doHideNodes;
    BooleanByName['doHideXmlNs'] := doHideXmlNs;
    Save;
  finally
    Free;
  end;
  fHideNodes.Clear;
  FreeAndNil(fHideNodes);
  FileContents.Free;
end;

function TShowXmlForm.NodeToBind(aNode: PVirtualNode): TCustomBindable;
var
  Data: PBindTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    Data := TreeView.GetNodeData(aNode);
    if Assigned(Data) then
      result := Data.Bind;
  end;
end;

procedure TShowXmlForm.OkButtonClick(Sender: TObject);
var
  oBind, dBind: TCustomBindable;
begin
  oBind := nil;
  dBind := nil;
  TreeView.EndEditNode;
  ModalResult := mrOk;
  if ValidateDuplicatesOn <> '' then
    if not Bind.hasNoDuplicatesOn(ValidateDuplicatesOn, True, oBind, dBind) then
    begin
      ModalResult := mrNone;
      ShowMessageFmt('Dusplicate found on %s: %s', [ValidateDuplicatesOn,
        dBind.Value]);
    end;
end;

procedure TShowXmlForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  theNode: PVirtualNode;
  Selected: Boolean;
begin
  theNode := TreeView.FocusedNode;
  Selected := (theNode <> nil) and (TreeView.Selected[theNode] = True);

  WriteXmlAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeWriteXmlAction.Enabled := Selected;

  CopyAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeCopyAction.Enabled := Selected;

  FindAction.Enabled := (TreeView.RootNodeCount > 0);
  FindNextAction.Enabled := (Selected) and (xmlUtil.SearchString <> '');
  FullExpandAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullExpandAction.Enabled := Selected and (theNode.ChildCount > 0);

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and (theNode.ChildCount > 0);

  Handled := True;
end;

procedure TShowXmlForm.WriteXmlActionExecute(Sender: TObject);
var
  Bind: TCustomBindable;
begin
  if ToolButtonUsed(Sender) then
    Bind := NodeToBind(TreeView.GetFirst)
  else
    Bind := NodeToBind(TreeView.FocusedNode);
  if Bind is TXml then
  begin
    SaveFileDialog.InitialDir := FileName;
    SaveFileDialog.DefaultExt := 'XML';
    SaveFileDialog.Filter := 'XML File (*.XML)|*.XML';
    SaveFileDialog.Title := 'Save ' + (Bind as TXml)
      .TagName + ' information as...';
    if SaveFileDialog.Execute = True then
    begin
      FileName := SaveFileDialog.FileName;
      FileContents.Text := (Bind as TXml).StreamXml(False, False, 0, False,
        False);
      FileContents.SaveToFile(FileName);
      FileContents.Clear;
    end;
  end;
end;

procedure TShowXmlForm.HaveLink(Sender: TObject; aLink: String);
var
  xAnsiLink: String;
begin
  xAnsiLink := aLink;
   OpenDocument(PChar(xAnsiLink)); { *Converted from ShellExecute* }
end;

procedure TShowXmlForm.XsdDocumentationMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//RichMemoMouseDown(Sender as TRichEdit, X, Y, HaveLink);
end;

procedure TShowXmlForm.XsdDocumentationMemoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
//  RichMemoMouseMove(Sender as TRichEdit, X, Y);
end;

function TShowXmlForm.SelectedBind: TCustomBindable;
begin
  result := NodeToBind(TreeView.FocusedNode);
end;

procedure TShowXmlForm.CompareActionExecute(Sender: TObject);
var
  xA2B: TA2BXml;
  aXml, bXml: TXml;
  xForm: TShowA2BXmlForm;
begin
  with TOpenDialog.Create(nil) do
    try
      DefaultExt := 'XML';
      Filter := 'XML File (*.XML)|*.XML';
      Title := 'Compare with XML file';
      if Execute then
      begin
        a2bInitialize;
        try
          bXml := TXml.Create;
          bXml.LoadFromFile(FileName, nil);
          if bXml.Name = '' then
          begin
            bXml.Free;
            raise Exception.Create('Could not parse ' + FileName + ' as XML');
          end;
          if NameWithoutPrefix(bXml.Name) <> NameWithoutPrefix
            ((Bind as TXml).Name) then
          begin
            bXml.Free;
            raise Exception.Create('Root elements differ');
          end;
          aXml := TXml.Create;
          aXml.Name := (Bind as TXml).Name;
          aXml.LoadValues(Bind as TXml, True, False);
          try
            xA2B := TA2BXml.CreateA2B('', '', aXml, bXml, Nil, Nil);
            xA2B.Ignore(ignoreDifferencesOn, ignoreAddingon, ignoreRemovingOn);
          finally
            FreeAndNil(aXml);
            FreeAndNil(bXml);
          end;
          try
            Application.CreateForm(TShowA2BXmlForm, xForm);
            try
              xForm.Caption := 'Differences in XML content';
              xForm.ignoreDifferencesOn := ignoreDifferencesOn;
              xForm.ignoreAddingon := ignoreAddingon;
              xForm.ignoreRemovingOn := ignoreRemovingOn;
              xForm.Xml := xA2B;
              xForm.ShowModal;
              if xForm.RefreshNeeded then
                FormShow(nil);
            finally
              FreeAndNil(xForm);
            end;
          finally
            FreeAndNil(xA2B);
          end;
        finally
          a2bUninitialize;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TShowXmlForm.CompareActionUpdate(Sender: TObject);
begin
  CompareAction.Enabled := doEnableCompare;
end;

procedure TShowXmlForm.CopyActionExecute(Sender: TObject);
begin
  if ToolButtonUsed(Sender) then
    xmlUtil.CopyToClipboard(Bind)
  else
    xmlUtil.CopyToClipboard(SelectedBind);
end;

procedure TShowXmlForm.FullExpandActionExecute(Sender: TObject);
var
  theNode: PVirtualNode;
begin
  if ToolButtonUsed(Sender) then
    theNode := TreeView.GetFirst
  else
    theNode := TreeView.FocusedNode;
  if theNode = nil then
    Exit;
  TreeView.FullExpand(theNode);
end;

procedure TShowXmlForm.FullCollapseActionExecute(Sender: TObject);
var
  theNode: PVirtualNode;
begin
  if ToolButtonUsed(Sender) then
    theNode := TreeView.GetFirst
  else
    theNode := TreeView.FocusedNode;
  if theNode = nil then
    Exit;
  TreeView.FullCollapse(theNode);
end;

procedure TShowXmlForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Bind: TCustomBindable;
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    FindDlg.Caption := 'Find Tag';
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      xmlUtil.SearchString := FindDlg.SearchEdit.Text;
      xmlUtil.SearchScope := FindDlg.ScopeRadioGroup.ItemIndex;
      xmlUtil.SearchIn := FindDlg.SearchInRadioGroup.ItemIndex;
      xmlUtil.SearchUseRegExp := FindDlg.RegularExpressionCheckBox.Checked;
      Found := False;
      if xmlUtil.SearchScope = 0 then // Search from next object
        CurItem := TreeView.GetNext(TreeView.FocusedNode);
      if (CurItem = nil) // if next object is nil
        or (xmlUtil.SearchScope = 1) then // or search entire scope
        CurItem := TreeView.GetFirst; // search from begin
      while not(CurItem = nil) and not Found do
      begin
        Bind := NodeToBind(CurItem);
        if xmlUtil.SearchIn = 0 then // search tag
          Found := StringMatchesMask(Bind.Name, xmlUtil.SearchString, False,
            xmlUtil.SearchUseRegExp)
        else // search description
          Found := StringMatchesMask(Bind.Value, xmlUtil.SearchString, False,
            xmlUtil.SearchUseRegExp);
        if not Found then
          CurItem := TreeView.GetNext(CurItem);
      end;
      if not Found then
        ShowMessage(xmlUtil.SearchString + ' not found')
      else
      begin
        TreeView.FocusedNode := CurItem;
        TreeView.Selected[CurItem] := True;
        {
          TreeView.OnChange (TreeView, CurItem);
          }
      end;
    end;
  finally
    FreeAndNil(FindDlg);
  end;
end;

procedure TShowXmlForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  Bind: TCustomBindable;
begin
  if True then
  begin
    Found := False;
    CurNode := TreeView.GetNext(TreeView.FocusedNode);
    while not(CurNode = nil) and not Found do
    begin
      Bind := NodeToBind(CurNode);
      if xmlUtil.SearchIn = 0 then // search tag
        Found := StringMatchesMask(Bind.Name, xmlUtil.SearchString, False,
          xmlUtil.SearchUseRegExp)
      else // search description
        Found := StringMatchesMask(Bind.Value, xmlUtil.SearchString, False,
          xmlUtil.SearchUseRegExp);
      if not Found then
        CurNode := TreeView.GetNext(CurNode);
    end;
    if not Found then
      ShowMessage(xmlUtil.SearchString + ' not found')
    else
    begin
      TreeView.FocusedNode := CurNode;
      TreeView.Selected[CurNode] := True;
    end;
  end;
end;

procedure TShowXmlForm.TreeViewPopupMenuPopup(Sender: TObject);
var
  Bind: TCustomBindable;
  f: Integer;
begin
  if TreeView.FocusedNode = nil then
    Raise Exception.Create('no item selected');
  Bind := SelectedBind;
  AddMenuItem.Enabled := (not isReadOnly) and (Bind is TXml) and Assigned
    (Bind.Parent) and Assigned((Bind as TXml).Xsd) and
    ((Bind as TXml).Xsd.maxOccurs <> '1');
  DeleteMenuItem.Enabled := (not isReadOnly) and (Bind is TXml) and Assigned
    ((Bind as TXml).Xsd) and ((Bind as TXml).Xsd.maxOccurs <> '1') and
    ((Bind as TXml).IndexOfRepeatableItem >= 1)
    and Assigned(Bind.Parent)
    and False // As long as it access violates and I do not see why??
    ;
  CleanActionMenuItem.Enabled := (not isReadOnly) and (Bind is TXml);
  ExpandLevelMenuItem.Enabled := (not isReadOnly) and (Bind is TXml) and XmlUtil.isExtendAdviced(Bind);
  PasteMenuItem.Enabled := not isReadOnly;
  PopulateMenuItem.Enabled := not isReadOnly;
  FullExpandMenuItem.Enabled := (not (Bind is TXmlAttribute)) and (Bind.Children.Count > 0);
  // FullExpandMenuItem.Caption := 'Full expand  '+ (Bind as TXml).TagName;
  FullCollapseMenuItem.Enabled := (not (Bind is TXmlAttribute)) and (Bind.Children.Count > 0);
  // FullCollapseMenuItem.Caption := 'Full collapse  ' + (Bind as TXml).TagName;
  // ViewAsGridMenuItem.Caption := 'View ' + (Bind as TXml).TagName + ' in a grid';
  ViewAsGridMenuItem.Enabled := (Bind is TXml) and (Bind.Children.Count > 0);
  // ZoomAsMenuItem.Caption := 'Zoom ' + (Bind as TXml).TagName + ' as';
  ZoomMenuItem.Enabled := (Bind.Children.Count = 0);
  ZoomAsMenuItem.Enabled := (Bind.Children.Count = 0);
  EditInPopUpMenuItem.Enabled := (not isReadOnly);
  if fHideNodes.Find(Bind.FullCaption, f) then
    ToggleHideAction.Caption := 'Unhide ' + Bind.FullCaption
  else
    ToggleHideAction.Caption := 'Hide ' + Bind.FullCaption;
end;

function TShowXmlForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolButton);
end;

procedure TShowXmlForm.HaveString(aString: String);
begin
  FileContents.Add(aString);
end;

procedure TShowXmlForm.ToggleHideActionExecute(Sender: TObject);
var
  f: Integer;
  xBind: TCustomBindable;
begin
  xBind := SelectedBind;
  if fHideNodes.Find(xBind.FullCaption, f) then
    fHideNodes.Delete(f)
  else
    fHideNodes.Add(xBind.FullCaption);
  RevalidateXmlTreeView(TreeView);
end;

procedure TShowXmlForm.ToggleHideActionUpdate(Sender: TObject);
begin
  ToggleHideAction.Enabled := Assigned(TreeView.FocusedNode);
end;

procedure TShowXmlForm.ToggleHideXmlNsActionExecute(Sender: TObject);
begin
  doHideXmlNs := not doHideXmlNs;
  RevalidateXmlTreeView(TreeView);
end;

procedure TShowXmlForm.ToggleShowHiddenActionExecute(Sender: TObject);
begin
  doHideNodes := not doHideNodes;
  RevalidateXmlTreeView(TreeView);
end;

procedure TShowXmlForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TShowXmlForm.TreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Bind: TCustomBindable;
begin
  Bind := NodeToBind(Node);
  case Column of
    treeTagColumn:
      begin
        Bind.Name := NewText;
        isChanged := True;
        RevalidateXmlTreeView(TreeView);
      end;
    treeValueColumn:
      begin
        if (Bind is TXml)
        and not (Bind as TXml).CustomCheck (NewText) then
          Exit;
        if (Bind is TXmlAttribute) or (Bind is TXml) then
        begin
          if NewText <> Bind.Value then
          begin
            if fReadOnly then
            begin
              ShowMessage('Change not accepted because form is readonly');
              Exit;
            end;
            if NewText = '&nil' then
            begin
              if Bind.Checked then
              begin
                Bind.Checked := False;
                isChanged := True;
              end;
            end
            else
            begin
              if (NewText <> Bind.Value) or (not Bind.CheckedAllup) then
              begin
                Bind.Value := NewText;
                Bind.Checked := True;
                isChanged := True;
              end;
            end;
          end;
        end;
        if (Bind is TIpmItem) then
        begin
          if NewText <> Bind.Value then
          begin
            if fReadOnly then
            begin
              ShowMessage('Change not accepted because form is readonly');
              Exit;
            end;
            Bind.Value := NewText;
            isChanged := True;
          end;
        end;
      end; // treeValueColumn
  end;
  RevalidateXmlTreeView(Sender as TVirtualStringTree);
end;

procedure TShowXmlForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowXmlForm.TreeViewCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  Bind: TCustomBindable;
begin
  Bind := NodeToBind(Node);
  if (Bind is TXml) and Assigned((Bind as TXml).Xsd) and
    ((Bind as TXml).TypeDef.ElementDefs.Count = 0) and
    ((Bind as TXml).TypeDef.Name = 'passwordType') then
  begin
    EditLink := TPasswordEditLink.Create;
  end
  else
    EditLink := TStringEditLink.Create;
end;

procedure TShowXmlForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xBind: TCustomBindable;
begin
  Sender.Selected[Sender.FocusedNode] := True;
  xBind := SelectedBind;
  try
    xmlUtil.ListXsdProperties(XsdPropertiesListView, xBind);
  except
  end;
  try
    xmlUtil.ListXsdDocumentation(DocumentationViewer, xBind, False, False);
  except
  end;
  TreeView.Invalidate;
end;

procedure TShowXmlForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TShowXmlForm.CancelButtonClick(Sender: TObject);
begin
  isChanged := False;
  ModalResult := mrCancel;
end;

procedure TShowXmlForm.CleanActionExecute(Sender: TObject);
begin
  if not(SelectedBind is TXml) then
    Exit;
  try
    (SelectedBind as TXml).Clean(1, xsdMaxDepthBillOfMaterials);
    UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
    isChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.CloseActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TShowXmlForm.PDFBase641Click(Sender: TObject);
begin
  xmlUtil.ZoomAsPDF(SelectedBind);
end;

procedure TShowXmlForm.ZoomAsTextMenuItemClick(Sender: TObject);
var
  xAllowEdit: Boolean;
begin
  TreeViewEditing(TreeView, TreeView.FocusedNode, treeValueColumn, xAllowEdit);
  xmlUtil.ZoomAsText(SelectedBind, not xAllowEdit);
  if xAllowEdit then
    TreeViewNewText(TreeView, TreeView.FocusedNode, treeValueColumn,
      xmlUtil.NewValue);
end;

procedure TShowXmlForm.ZoomAsXmlMenuItemClick(Sender: TObject);
begin
  xmlUtil.ZoomAsXml(SelectedBind, isReadOnly);
end;

procedure TShowXmlForm.ZoomasAssignment1Click(Sender: TObject);
begin
  if not (SelectedBind is TXml) then
    raise Exception.Create('Only allowd on XML elements');
  XmlUtil.ShowInfoForm('Zoom as Assignment', (SelectedBind as TXml).asAssignments);
end;

procedure TShowXmlForm.ZoomAsHTMLMenuItemClick(Sender: TObject);
begin
  {$ifdef SHDOCVW}
  xmlUtil.ZoomAsHtml(SelectedBind);
  {$endif}
end;

procedure TShowXmlForm.ViewAsGridMenuItemClick(Sender: TObject);
var
  Bind: TCustomBindable;
  xXml, dXml: TXml;
  xXsd: TXsd;
  xForm: TXmlGridForm;
  xXsdPropertiesVisible: Boolean;
  xXsdDescr: TXsdDescr;
begin
  Bind := SelectedBind;
  if Bind is TXmlAttribute then
    raise Exception.Create('function disabled on XML attributes');
  if not(Bind is TXml) then
    raise Exception.Create('No element selected');
  xXml := Bind as TXml;
  if xXml.Items.Count = 0 then
    raise Exception.Create('Function only enabled on groups');
  XmlUtil.PushCursor(crHourGlass);
  try
    if not Assigned(xXml.Xsd) then
    begin
      xXsdDescr := TXsdDescr.Create;
      try
        xXsd := CreateXsdFromXml(xXsdDescr, xXml, False);
        dXml := TXml.Create(-10000, xXsd);
        dXml.LoadValues(xXml, False);
        try
          Application.CreateForm(TXmlGridForm, xForm);
          xXsdPropertiesVisible := xForm.XsdPropertiesVisible;
          try
            xForm.isReadOnly := isReadOnly;
            xForm.Xml := dXml;
            xForm.XsdPropertiesVisible := False;
            xForm.ShowPropertiesAction.Enabled := False;
            xForm.doConfirmRemovals := doConfirmRemovals;
            xForm.doShowCancelButton := doShowCancelButton;
            xForm.initialExpandStyle := initialExpandStyle;
            xForm.ShowModal;
            isChanged := isChanged or xForm.stubChanged;
          finally
            xForm.XsdPropertiesVisible := xXsdPropertiesVisible;
            FreeAndNil(xForm);
          end;
        finally
          dXml.Free;
        end;
      finally
        xXsdDescr.Free;
      end;
    end
    else
    begin
      Application.CreateForm(TXmlGridForm, xForm);
      try
        xForm.isReadOnly := True;
        xForm.Xml := xXml;
        xForm.ShowModal;
        if xForm.stubChanged then
          RevalidateXmlTreeView(TreeView);
        isChanged := isChanged or xForm.stubChanged;
      finally
        FreeAndNil(xForm);
      end;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TShowXmlForm.B1Click(Sender: TObject);
begin
  xmlUtil.ZoomAsBase64(SelectedBind);
end;

procedure TShowXmlForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TShowXmlForm.FormShow(Sender: TObject);
var
  xNode: PVirtualNode;
  xBind, xParent: TCustomBindable;
  xFound: Boolean;
begin
  if InitialFocusOn = '' then
  begin
    case initialExpandStyle of
      esAll:
        ;
      esOne:
        begin
          TreeView.FullCollapse(TreeView.GetFirst);
          TreeView.Expanded[TreeView.GetFirst] := True;
        end;
      esUsed:
        begin
          TreeView.FullCollapse(TreeView.GetFirst);
          xNode := TreeView.GetFirst;
          while Assigned(xNode) do
          begin
            if xNode.CheckState = csCheckedNormal then
              TreeView.Expanded[xNode] := True;
            xNode := TreeView.GetNext(xNode);
          end;
        end;
    end;
  end
  else
  begin
    xParent := Bind.Parent;
    Bind.Parent := nil;
    try
      xFound := False;
      xNode := TreeView.GetFirst;
      while Assigned(xNode) and not xFound do
      begin
        xBind := NodeToBind(xNode);
        if (xBind.FullCaption = InitialFocusOn) then
        begin
          xFound := True;
          TreeView.FullCollapse(TreeView.GetFirst);
          TreeView.FocusedNode := xNode;
          TreeView.FullExpand(xNode);
          TreeView.SetFocus;
        end;
        xNode := TreeView.GetNext(xNode);
      end;
    finally
      Bind.Parent := xParent;
    end;
  end;
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
  RevalidateXmlTreeView(TreeView);
  TreeViewFocusChanged(TreeView, TreeView.FocusedNode, TreeView.FocusedColumn);
  DocumentationViewer.Color := Self.Color;
  Screen.Cursor := crDefault;
end;

procedure TShowXmlForm.genDocumentaionActionExecute(Sender: TObject);
var
  refKey: Integer;

  function StringMatchesRegExpr (aString, aExpr: String): Boolean;
  begin
    result := False;
    with TRegExpr.Create do
    try
      Expression := '^(' + aExpr + ')$';  // bol and eol: must match entire string
      if (Exec(aString)) then
        result := True;
    finally
      Free;
    end;
  end;

  function _name(aXml: TXml): String;
  begin
    if Assigned(aXml.Parent) then
      result := _name(aXml.Parent as TXml) + '_' + aXml.Name
    else
      result := aXml.Name;
  end;
  procedure _refLinks(aXml: TXml; aString: String);
    procedure _docLink(aString: String); // relative link
    begin
      with aXml.AddXml(TXml.CreateAsString('a', Copy(aString, 7,
            Length(aString)))) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('href',
            ReplaceText(Copy(aString, 7, Length(aString)), '\', '/')));
        AddAttribute(TXmlAttribute.CreateAsString('target', '_top'));
      end;
    end;
    function _stdLink(aString: String): TXml;
    begin
      result := aXml.AddXml(TXml.CreateAsString('a', aString));
      with result do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('href', aString));
        AddAttribute(TXmlAttribute.CreateAsString('target', '_top'));
      end;
    end;

  var
    rx: TRegExpr;
    rslt: Boolean;
    p: Integer;
  begin
    rx := TRegExpr.Create;
    try
      rx.Expression := S_XML_REGEXP_LINK;
      p := 1;
      rslt := rx.Exec(aString);
      while rslt do
      begin
        if rx.MatchPos[0] > p then
          aXml.AddXml(TXml.CreateAsString('a',
              Copy(aString, p, rx.MatchPos[0] - p)));
        if uppercase(Copy(rx.Match[0], 1, 6)) = 'DOC://' then
          _docLink(rx.Match[0])
        else
          _stdLink(rx.Match[0]);
        p := rx.MatchPos[0] + rx.MatchLen[0];
        rslt := rx.ExecNext;
      end;
      if p < Length(aString) then
        aXml.AddXml(TXml.CreateAsString('a', Copy(aString, p, Length(aString)))
          );
    finally
      rx.Free;
    end;
  end;

  procedure _toc(aDtlFile: String; aIndent: Integer; aXml: TXml; sXml: TXml);
    function _indent(X: Integer): String;
    begin
      result := '';
      while X > 0 do
      begin
        result := result + '_';
        Dec(X);
      end;
    end;

  var
    X: Integer;
  begin
    with aXml.AddXml(TXml.CreateAsString('a', '')) do
    begin
      AddXml(TXml.CreateAsString('', _indent(aIndent)));
      with AddXml(TXml.CreateAsString('a', sXml.Name)) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('href',
            aDtlFile + '#' + _name(sXml) + '_' + IntToStr(refKey)));
        Inc(refKey);
        AddAttribute(TXmlAttribute.CreateAsString('target', 'dtl'));
      end;
    end;
    aXml.AddXml(TXml.CreateAsString('br', ''));
    for X := 0 to sXml.Items.Count - 1 do
      _toc(aDtlFile, aIndent + 4, aXml, sXml.Items.XmlItems[X]);
  end;

  procedure _dtl(aXml: TXml; sXml: TXml);
  var
    X: Integer;
    xXml, dXml: TXml;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      aXml.Name := 'dl';
      with aXml.AddXml(TXml.CreateAsString('dt', '')) do
        with AddXml(TXml.CreateAsString('strong', '')) do
          with AddXml(TXml.CreateAsString('a', sXml.Name)) do
            AddAttribute(TXmlAttribute.CreateAsString('name',
                _name(sXml) + '_' + IntToStr(refKey)));
      Inc(refKey);
      sl.Text := sXml.Xsd.Documentation.Text;
      while (sl.Count > 0)
      and StringMatchesRegExpr(sl.Strings[0], '[ \t]*') do
        sl.Delete(0);
      while (sl.Count > 0)
      and StringMatchesRegExpr(sl.Strings[sl.Count - 1], '[ \t]*') do
        sl.Delete(sl.Count - 1);
      for X := 0 to sXml.TypeDef.Enumerations.Count - 1 do
        sl.Add(sXml.TypeDef.Enumerations.Strings[X] + ' = ' +
            (sXml.TypeDef.Enumerations.Objects[X] as TXsdEnumeration)
            .Annotation);
      xXml := aXml.AddXml(TXml.CreateAsString('dd', ''));
      _refLinks(xXml, sl.Text);
      for X := 0 to sXml.Items.Count - 1 do
      begin
        dXml := xXml.AddXml(TXml.Create);
        _dtl(dXml, sXml.Items.XmlItems[X]);
      end;
    finally
      sl.Free;
    end;
  end;

var
  xXml, dXml: TXml;
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView.FocusedNode);
  if not(xBind is TXml) then
    Exit;
  if not Assigned((xBind as TXml).Xsd) then
    Exit;
  xXml := TXml.Create;
  try
    xXml.Name := 'html';
    with xXml.AddXml(TXml.CreateAsString('frameset', '')) do
    begin
      AddAttribute(TXmlAttribute.CreateAsString('rows', '8%,92%'));
      with AddXml(TXml.CreateAsString('frame', '')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('name', 'top'));
        AddAttribute(TXmlAttribute.CreateAsString('src',
            _xmlProgName + '_' + xBind.Name + '_hlptop.htm'));
      end;
      with AddXml(TXml.CreateAsString('frameset', '')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('cols', '20%,80%'));
        with AddXml(TXml.CreateAsString('frame', '')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('name', 'toc'));
          AddAttribute(TXmlAttribute.CreateAsString('src',
              _xmlProgName + '_' + xBind.Name + '_hlptoc.htm'));
        end;
        with AddXml(TXml.CreateAsString('frame', '')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('name', 'dtl'));
          AddAttribute(TXmlAttribute.CreateAsString('src',
              _xmlProgName + '_' + xBind.Name + '_hlpdtl.htm'));
        end;
      end;
    end;
    SaveStringToFile(ExtractFilePath(ParamStr(0))
        + '\Documentation\' + _xmlProgName + '_' + xBind.Name + '_hlp.htm',
      xXml.asHtmlString);
    xXml.Items.Clear;
    xXml.Name := 'html';
    with xXml.AddXml(TXml.CreateAsString('body', '')) do
    begin
      with AddXml(TXml.CreateAsString('p', '')) do
      begin
        with AddXml(TXml.CreateAsString('a', 'wsdlStub')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('href', 'wsdlStub.htm'));
          AddAttribute(TXmlAttribute.CreateAsString('target', '_top'));
        end;
        AddXml(TXml.CreateAsString('a', ' - ' + xBind.Name));
      end;
    end;
    SaveStringToFile(ExtractFilePath(ParamStr(0))
        + '\Documentation\' + _xmlProgName + '_' + xBind.Name + '_hlptop.htm',
      xXml.asHtmlString);
    xXml.Items.Clear;
    xXml.Name := 'html';
    dXml := xXml.AddXml(TXml.CreateAsString('body', ''));
    refKey := 0;
    _toc(_xmlProgName + '_' + xBind.Name + '_hlpdtl.htm', 0, dXml,
      (xBind as TXml));
    SaveStringToFile(ExtractFilePath(ParamStr(0))
        + '\Documentation\' + _xmlProgName + '_' + xBind.Name + '_hlptoc.htm',
      xXml.asHtmlString);
    xXml.Items.Clear;
    refKey := 0;
    xXml.Name := 'html';
    with xXml.AddXml(TXml.CreateAsString('body', '')) do
    begin
      dXml := AddXml(TXml.Create);
      _dtl(dXml, (xBind as TXml));
    end;
    SaveStringToFile(ExtractFilePath(ParamStr(0))
        + '\Documentation\' + _xmlProgName + '_' + xBind.Name + '_hlpdtl.htm',
      xXml.asHtmlString);
    xXml.Items.Clear;
  finally
    xXml.Free;
  end;
end;

function TShowXmlForm.getDoHideNodes: Boolean;
begin
  result := not ToggleShowHiddenAction.Checked;
end;

function TShowXmlForm.getDoHideXmlNs: Boolean;
begin
  result := not ToggleHideXmlNsAction.Checked;
end;

function TShowXmlForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TShowXmlForm.setdoEnableCompare(const Value: Boolean);
begin
  fdoEnableCompare := Value;
  CompareAction.Visible := Value;
end;

procedure TShowXmlForm.setDoHideNodes(const Value: Boolean);
begin
  ToggleShowHiddenAction.Checked := not Value;
  RevalidateXmlTreeView(TreeView);
end;

procedure TShowXmlForm.setDoHideXmlNs(const Value: Boolean);
begin
  ToggleHideXmlNsAction.Checked := not Value;
  if doHideXmlNs then
    ToggleHideXmlNsAction.ImageIndex := 92
  else
    ToggleHideXmlNsAction.ImageIndex := 91;
end;

procedure TShowXmlForm.setIsChanged(const Value: Boolean);
begin
  fIsChanged := Value;
  // CancelButton.Enabled := Value;
end;

procedure TShowXmlForm.setReadOnly(const Value: Boolean);
begin
  fReadOnly := Value;
  TreeView.ParentColor := Value;
  if not Value then
  begin
    TreeView.Color := clWindow;
    TreeView.Colors.GridLineColor := clBtnFace;
  end
  else
    TreeView.Colors.GridLineColor := clBtnHighlight;
end;

function TShowXmlForm.setVisibility(aBind: TCustomBindable): Boolean;
var
  f: Integer;
begin
  result := (   (not Assigned(fHideNodes))
             or (not DoHideNodes)
             or (not fHideNodes.Find(aBind.FullCaption, f))
            )
        and (   (not doHideXmlNs)
             or (not (aBind is TXmlAttribute))
             or (    (LeftStr (aBind.Name, 6) <> 'xmlns:')
                 and (aBind.Name <> 'xmlns')
                 and (aBind.Name <> 'xsi:type')      { TODO : other prefixes for schema-insta... }
                )
            )
          ;
end;

procedure TShowXmlForm.TreeViewEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
  if xmlUtil.isPasswordType(NodeToBind(TreeView.FocusedNode)) then
    NodeToBind(TreeView.FocusedNode).Value := EncryptString
      (NodeToBind(TreeView.FocusedNode).Value);
end;

procedure TShowXmlForm.TreeViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xBind: TCustomBindable;
begin
  if fReadOnly then
    Exit;
  xBind := NodeToBind(Node);
  if xmlUtil.isPasswordType(xBind) then
    xBind.Value := EncryptString(xBind.Value);
  if Column = treeValueColumn then
    xmlUtil.CheckValidity(xBind);
end;

procedure TShowXmlForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure TShowXmlForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Bind: TCustomBindable;
  f: Integer;
begin
  Bind := NodeToBind(Node);
  case Kind of
    ikNormal, ikSelected:
      begin
        case Column of
          treeTagColumn:
            begin
              if not doHideNodes then
              begin
                if fHideNodes.Find(Bind.FullCaption, f) then
                  ImageIndex := 111;
              end;
            end;
          treeButtonColumn:
            begin
              if Bind is TXmlAttribute then
                ImageIndex := -1;
              if (Bind is TXml) or (Bind is TXmlAttribute) or
                (Bind is TIpmItem) then
              begin
                if xmlUtil.isExtendAdviced(Bind) then
                begin
                  ImageIndex := 105;
                  Exit;
                end;
                if xmlUtil.isBoolean(Bind) then
                begin
                  if (Bind.Value = 'true') or (Bind.Value = '1') then
                    ImageIndex := 100
                  else
                    ImageIndex := 99;
                  Exit;
                end;
                if xmlUtil.isDateTime(Bind) then
                begin
                  ImageIndex := 101;
                  Exit;
                end;
                if xmlUtil.isDate(Bind) then
                begin
                  ImageIndex := 102;
                  Exit;
                end;
                if xmlUtil.isTime(Bind) then
                begin
                  ImageIndex := 103;
                  Exit;
                end;
                if xmlUtil.isEditSupported(Bind) then
                begin
                  ImageIndex := 97;
                  Exit;
                end;
                if xmlUtil.isGridAdviced(Bind) then
                begin
                  ImageIndex := 36;
                  Exit;
                end;
                if xmlUtil.isTreeAdviced(Bind) then
                begin
                  ImageIndex := 98;
                  Exit;
                end;
                ImageIndex := -1;
              end;
            end;
        end; { case column }
      end; { Kind in ikNormal, ikSelected }
  end; { case Kind }
end;

procedure TShowXmlForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Bind: TCustomBindable;
  X: Integer;
begin
  CellText := '';
  case Column of
    treeTagColumn:
      begin
        Bind := NodeToBind(Node);
        if Assigned(Bind) then
          if Bind is TXmlAttribute then
            CellText := '@' + Bind.Name
          else
            CellText := xmlUtil.BindCaption(Bind);
      end;
    treeValueColumn:
      begin
        Bind := NodeToBind(Node);
        if Assigned(Bind) then
        begin
          try
          if (Bind is TXml)
          and (Bind.Name <> 'passwordType')
          and Assigned ((Bind as TXml).TypeDef)
          and ((Bind as TXml).TypeDef.Name = 'passwordType')
          and (Bind.Value <> '') then
            CellText := '**********'
            // CellText := Bind.Value
          else
          begin
            if _xmlLicensed or (not isReadOnly) then
              CellText := Bind.Value
            else
            begin
              for X := 1 to Length(Bind.Value) do
                if X < 5 then
                  CellText := CellText + Bind.Value[X]
                else
                  CellText := CellText + '*';
            end;
          end;

          except
            SjowMessage(bind.Name);
          end;
        end;
      end;
  end;
end;

procedure TShowXmlForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then with (Sender as TVirtualStringTree) do
    EditNode (FocusedNode, FocusedColumn);
end;

procedure TShowXmlForm.RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
var
  xNode: PVirtualNode;
  Bind: TCustomBindable;
begin
  xNode := aTreeView.GetFirst; // search from begin
  while not(xNode = nil) do
  begin
    Bind := NodeToBind(xNode);
    if Bind is TXmlAttribute then
    begin
      if (Bind as TXmlAttribute).Checked then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
      aTreeView.IsVisible[xNode] := setVisibility(Bind);
    end
    else
    begin
      if Bind is TXml then
      begin
        aTreeView.IsVisible[xNode] := setVisibility(Bind);
        if ((Bind as TXml).Checked) then
          xNode.CheckState := csCheckedNormal
        else
          xNode.CheckState := csUnCheckedNormal;
      end;
    end;
    xNode := aTreeView.GetNext(xNode);
  end;
  with aTreeView do
  begin
    // IsVisible [GetNextSibling (GetFirst)] := WsdlOperation.CheckExpectedValues;
    Invalidate;
  end;
end;

procedure TShowXmlForm.TreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if (Node = (Sender as TVirtualStringTree).GetFirst) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    Exit;
  end;
  with NodeToBind(Node) do
  begin
{$ifndef NoGUI}
    Font(TargetCanvas.Font);
{$endif}
    if (Column = treeValueColumn)
    and isValueLink then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
  end;
end;

procedure TShowXmlForm.SetBindNodeCheckBox(aTreeView: TVirtualStringTree;
  aBind: TCustomBindable; aNode: PVirtualNode);
begin
  if (aBind is TXmlAttribute) or (aBind is TXml) then
  begin
    if (Assigned(aBind.Parent))
    and (Assigned((aBind.Parent as TXml).Xsd)) then
    begin
      if not (    (aBind is TXml)
              and Assigned ((aBind as TXml).Xsd)
              and (aBind as TXml).Xsd.isCheckboxDisabled
             ) then
      begin
        if (aBind.Parent as TXml).TypeDef.ContentModel = 'Choice' then
          aNode.CheckType := ctRadioButton
        else
          aNode.CheckType := ctCheckBox;
        if aBind.Checked then
          aNode.CheckState := csCheckedNormal
        else
          aNode.CheckState := csUnCheckedNormal;
      end;
    end
    else
    begin
      aNode.CheckType := ctCheckBox;
      aNode.CheckState := csCheckedNormal;
    end;
  end;
end;

procedure TShowXmlForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  Bind, refBind: TCustomBindable;
begin
  refBind := NodeToBind(TreeView.FocusedNode);
  Bind := NodeToBind(Node);
  with TargetCanvas do
  begin
    Brush.Style := bsSolid;
    if isReadOnly and (Column = treeValueColumn) and Assigned(refBind) and
      (refBind <> Bind) and (refBind.Value = Bind.Value) and
      (refBind.Value <> '') then
      Brush.Color := clLime
    else
      Brush.Color := NodeToBind(Node).bgColor(isReadOnly, Column);
    FillRect(CellRect);
  end;
end;

procedure TShowXmlForm.TreeViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Node);
  if Assigned(xBind) then
  begin
    isChanged := True;
    xBind.Checked := (Node.CheckState = csCheckedNormal);
    if (not xBind.Checked) then
    begin
      if xmlUtil.doCollapseOnUncheck then
        TreeView.FullCollapse(Node)
    end
    else
    begin
      if xBind is TXml then with xBind as TXml do
      begin
        if Assigned (TypeDef) then
        begin
          if (TypeDef.BaseDataTypeName = 'boolean')
          and (Value = '') then
            Value := 'false';
        end;
      end;
      if xmlUtil.doExpandOnCheck then
        TreeView.FullExpand(Node);
    end;
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.TreeViewChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := (not isReadOnly) and (xmlUtil.isCheckAllowed(NodeToBind(Node)));
end;

procedure TShowXmlForm.TreeViewClick(Sender: TObject);
var
  xChanged: Boolean;
begin
  if (TreeView.FocusedColumn = treeButtonColumn) then
  begin
    if XmlUtil.isExtendAdviced (SelectedBind) then
      ExpandLevelActionExecute(nil)
    else
    begin
      xChanged := xmlUtil.editXml(SelectedBind, True, isReadOnly);
      if xChanged then
      begin
        UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
        RevalidateXmlTreeView(TreeView);
      end;
      isChanged := isChanged or xChanged;
      TreeView.FocusedColumn := treeValueColumn;
    end
  end
  else
  begin
    if (Assigned((Sender as TVirtualStringTree).FocusedNode)) and
      ((Sender as TVirtualStringTree).FocusedColumn = treeValueColumn) then
  (Sender as TVirtualStringTree)
      .EditNode((Sender as TVirtualStringTree).FocusedNode,
        (Sender as TVirtualStringTree).FocusedColumn);
  end;
end;

procedure TShowXmlForm.UpdateTreeViewNode(aTreeView: TVirtualStringTree;
  aNode: PVirtualNode);
var
  X: Integer;
  xAttributeNode: PVirtualNode;
  xData: PBindTreeRec;
  xExpanded: Boolean;
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(aNode);
  if xBind is TXmlAttribute then
  begin
    if xBind.Checked then
      aNode.CheckState := csCheckedNormal
    else
      aNode.CheckState := csUnCheckedNormal;
    aTreeView.InvalidateNode(aNode);
    Exit;
  end;
  xExpanded := aTreeView.Expanded[aNode];
  aTreeView.DeleteChildren(aNode);
  SetBindNodeCheckBox(aTreeView, xBind as TXml, aNode);
  if xBind is TXml then
  begin
    for X := 0 to (xBind as TXml).Attributes.Count - 1 do
    begin
      xAttributeNode := aTreeView.AddChild(aNode);
      xData := aTreeView.GetNodeData(xAttributeNode);
      xData.Bind := (xBind as TXml).Attributes.XmlAttributes[X];
      xAttributeNode.CheckType := ctCheckBox;
      if (xBind as TXml).Attributes.XmlAttributes[X].Checked then
        xAttributeNode.CheckState := csCheckedNormal
      else
        xAttributeNode.CheckState := csUnCheckedNormal;
    end;
  end;
  for X := 0 to xBind.Children.Count - 1 do
  begin
    ShowBind(xBind.Children.Bindables[X], aNode);
  end;
  if xExpanded then
    aTreeView.FullExpand(aNode);
end;

procedure TShowXmlForm.AddActionExecute(Sender: TObject);
var
  xXml: TXml; { new created }
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView.FocusedNode);
  if not(xBind is TXml) then
    Exit;
  xXml := AddSibbling(xBind as TXml);
  if Assigned(xXml) then
  begin
    TreeView.FocusedNode := InsertXmlNode(TreeView.FocusedNode, xXml);
    TreeView.FocusedColumn := 0;
    TreeView.Expanded[TreeView.FocusedNode] := True;
    TreeView.Invalidate;
    isChanged := True;
  end;
end;

function TShowXmlForm.InsertXmlNode(aNode: PVirtualNode;
  aXml: TXml): PVirtualNode;
begin
  result := TreeView.InsertNode(aNode, amInsertAfter);
  FinishXmlNode(result, aXml);
end;

procedure TShowXmlForm.FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
var
  attrNode: PVirtualNode;
  Data: PBindTreeRec;
  I: Integer;
begin
  Data := TreeView.GetNodeData(aNode);
  Data.Bind := aXml;
  if Assigned((aXml.Parent as TXml).Xsd) then
  begin
    if (aXml.Parent as TXml).TypeDef.ContentModel = 'Choice' then
      aNode.CheckType := ctRadioButton
    else
      aNode.CheckType := ctCheckBox;
    if aXml.Checked then
      aNode.CheckState := csCheckedNormal
    else
      aNode.CheckState := csUnCheckedNormal;
  end
  else
  begin
    aNode.CheckType := ctRadioButton;
    aNode.CheckState := csCheckedNormal;
  end;
  for I := 0 to aXml.Attributes.Count - 1 do
  begin
    attrNode := TreeView.AddChild(aNode);
    attrNode.CheckType := ctCheckBox;
    if aXml.Attributes.XmlAttributes[I].Checked then
      attrNode.CheckState := csCheckedNormal
    else
      attrNode.CheckState := csUnCheckedNormal;
    Data := TreeView.GetNodeData(attrNode);
    Data.Bind := aXml.Attributes.XmlAttributes[I];
  end;
  for I := 0 to aXml.Items.Count - 1 do
    FinishXmlNode(TreeView.AddChild(aNode), aXml.Items.XmlItems[I]);
end;

procedure TShowXmlForm.DeleteActionExecute(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := SelectedBind;
  if xBind = Bind then
    raise Exception.Create('Not allowed to delete root');
  if xBind is TIpmItem then
    raise Exception.Create('Not possible to delete Cobol items');
  TreeView.BeginUpdate;
  try
    TreeView.DeleteNode(TreeView.FocusedNode, True);
    xmlUtil.Delete(xBind as TXml);
    TreeView.Invalidate;
    isChanged := True;
  finally
    TreeView.EndUpdate;
  end;
end;

procedure TShowXmlForm.PasteActionExecute(Sender: TObject);
begin
  if not(SelectedBind is TXml) then
    Exit;
  try
    xmlUtil.PasteFromClipboard(SelectedBind);
    UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
    isChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.All1Click(Sender: TObject);
begin
  try
    xmlUtil.Populate(SelectedBind, xvAll);
    UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
    isChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.Required1Click(Sender: TObject);
begin
  try
    xmlUtil.Populate(SelectedBind, xvRequired);
    UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
    isChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.EditInPopUpMenuItemClick(Sender: TObject);
begin
  if (not isReadOnly) and xmlUtil.isEditSupported(SelectedBind) then
  begin
    if xmlUtil.editXml(SelectedBind, True, isReadOnly) then
    begin
      TreeViewNewText(TreeView, TreeView.FocusedNode, treeValueColumn,
        xmlUtil.NewValue);
    end;
  end;
end;

procedure TShowXmlForm.ExpandLevelActionExecute(Sender: TObject);
begin
  if (SelectedBind is TXml)
  and XmlUtil.isExtendAdviced(SelectedBind as TXml) then
  begin
    try
      (SelectedBind as TXml).ExtendRecursivity;
      UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
      TreeView.FullExpand(TreeView.FocusedNode);
    finally
      RevalidateXmlTreeView(TreeView);
    end;
  end;
end;

procedure TShowXmlForm.ViewinTreeMenuItemClick(Sender: TObject);
begin
  try
    xmlUtil.ViewAsXml(SelectedBind, isReadOnly);
    UpdateTreeViewNode(TreeView, TreeView.FocusedNode);
    isChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
  end;
end;

procedure TShowXmlForm.ZoomMenuItemClick(Sender: TObject);
begin
  xmlUtil.presentString(SelectedBind.FullCaption, SelectedBind.Value);
end;

procedure TShowXmlForm .Button1Click (Sender : TObject );
begin
end;

procedure TShowXmlForm .DocumentationViewerClick (Sender : TObject );
begin
end;


procedure TShowXmlForm .DocumentationViewerMouseMove (Sender : TObject ;
  Shift : TShiftState ; X , Y : Integer );
begin
end;

procedure TShowXmlForm.DocumentationViewerHotClick(Sender: TObject);
begin
  OpenUrl(DocumentationViewer.HotURL);
end;

procedure TShowXmlForm.MenuItem2Click(Sender: TObject);
var
  n, w: Integer;
begin
  n := TreeView.FocusedColumn;
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Column width';
    PromptForm.PromptEdit.Text := IntToStr (TreeView.Header.Columns[n].Width);
    PromptForm.Numeric := True;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
      TreeView.Header.Columns[n].Width := StrToInt(PromptForm.PromptEdit.Text);
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TShowXmlForm .TreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
  TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
  const CellRect : TRect );
var
  Bind: TCustomBindable;
  r: TRect;
begin
  case Column of
  treeTagColumn:
    begin
      Bind := NodeToBind(Node);
      if Assigned(Bind)
      and (Bind is TXml) then with Bind as TXml do
      begin
        if Assigned (Xsd)
        and (Xsd.maxOccurs <> '1') then
        begin
          r := Sender.GetDisplayRect(Node, Column, true);
          ActionImageList.Draw(TargetCanvas, r.Right - 16, CellRect.Top, 115);
        end;
      end;
    end;
  end;
end;

procedure TShowXmlForm.TreeViewDblClick(Sender: TObject);
begin
  xmlUtil.presentString(SelectedBind.FullCaption, SelectedBind.Value);
end;

{ TPasswordEditLink }

function TPasswordEditLink.BeginEdit: Boolean;
begin
  result := Inherited;
  {
    if result then
    Edit.Text := DecryptString (ShowXmlForm.NodeToBind(Node).Value);
  }
end;

constructor TPasswordEditLink.Create;
begin
  inherited;
  Self.Edit.PasswordChar := '*';
end;

end.
