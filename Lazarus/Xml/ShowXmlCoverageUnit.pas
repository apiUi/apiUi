unit ShowXmlCoverageUnit;

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
  SysUtils
   , Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, VirtualTrees
   , Bind
   , Xmlz
   , Ipmz
   , Dialogs
   , FormIniFilez, ToolWin, ActnList, Menus, ImgList,
  Express , RichBox
   ;

type

  { TShowXmlCoverageForm }

  TShowXmlCoverageForm = class(TForm)
    DocumentationMemo : TlzRichEdit ;
    Panel1: TPanel;
    TreeView: TVirtualStringTree;
    ActionList1: TActionList;
    FindAction: TAction;
    FindNextAction: TAction;
    FullExpandAction: TAction;
    FullCollapseAction: TAction;
    ToolBar1: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton6: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    SaveFileDialog: TSaveDialog;
    TreeViewPopupMenu: TPopupMenu;
    FullExpandMenuItem: TMenuItem;
    FullCollapseMenuItem: TMenuItem;
    NodeFullExpandAction: TAction;
    NodeFullCollapseAction: TAction;
    ActionImageList: TImageList;
    Panel2: TPanel;
    Splitter1: TSplitter;
    XsdPropertiesListView: TListView;
    Splitter2: TSplitter;
    Panel3: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    ToolButton5: TToolButton;
    ToggleIgnoreAction: TAction;
    oggleIgnoreAction1: TMenuItem;
    N1: TMenuItem;
    downAction: TAction;
    upAction: TAction;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToggleShowIgnoredsAction: TAction;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    AsHtmlAction: TAction;
    procedure DocumentationMemoClick (Sender : TObject );
    procedure ZoomMenuItemClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewExit(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure B1Click(Sender: TObject);
    procedure ZoomAsHTMLMenuItemClick(Sender: TObject);
    procedure PDFBase641Click(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
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
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure XsdDocumentationMemoMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure XsdDocumentationMemoMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToggleIgnoreActionExecute(Sender: TObject);
    procedure downActionExecute(Sender: TObject);
    procedure upActionExecute(Sender: TObject);
    procedure TreeViewGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure ToggleShowIgnoredsActionExecute(Sender: TObject);
    procedure ToggleShowIgnoredsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure AsHtmlActionExecute(Sender: TObject);
    procedure AsHtmlActionHint(var HintStr: string; var CanShow: Boolean);
  private
    fIsChanged: Boolean;
    fReadOnly: Boolean;
    fBind: TXmlCvrg;
    FileContents: TStringList;
    fdoEnableCompare: Boolean;
    procedure SearchMissing (aDown: Boolean);
    procedure HaveLink(Sender: TObject; aLink: String);
    procedure ShowBind (Bind: TXmlCvrg; aNode: PVirtualNode);
    procedure SetBindNodeCheckBox ( aTreeView: TVirtualStringTree
                                  ; aBind: TXmlCvrg
                                  ; aNode: PVirtualNode
                                  );
    function getReadOnly: Boolean;
    procedure HaveString (aString: String);
    function ToolButtonUsed (Sender: TObject): Boolean;
    procedure SetBind (aBind: TXmlCvrg);
    function SelectedBind: TXmlCvrg;
    procedure RevalidateXmlTreeView (aTreeView: TVirtualStringTree);
    procedure setIsChanged(const Value: Boolean);
    procedure setdoEnableCompare(const Value: Boolean);
    function getDoShowIgnoreds: Boolean;
    procedure setDoShowIgnoreds(const Value: Boolean);
  public
    ignoreDifferencesOn: TStringList;
    doConfirmRemovals: Boolean;
    doShowCancelButton: Boolean;
    initialExpandStyle: TBindExpandStyle;
    property doShowIgnoreds: Boolean read getDoShowIgnoreds write setDoShowIgnoreds;
    property doEnableCompare: Boolean read fdoEnableCompare write setdoEnableCompare;
    property Bind: TXmlCvrg read fBind write SetBind;
    property Changed: Boolean read fIsChanged write fIsChanged;
    function NodeToBind (aNode: PVirtualNode): TXmlCvrg;
    procedure RevalidateView;
  end;

  TPasswordEditLink = class(TStringEditLink, IVTEditLink)
  public
    property Node: PVirtualNode read fNode;
    function BeginEdit: Boolean; override; stdcall;
    constructor Create; override;
  end;


var
  ShowXmlCoverageForm: TShowXmlCoverageForm;
  GreenColor, OrangeColor, RedColor: TColor;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  FindRegExpDialog
   , igGlobals
   , ClipBrd
   , Xsdz
   , xmlUtilz
   , XmlGridUnit
   , RegExpr
   , StrUtils
   , A2BXmlz
   , ShowA2BXmlUnit
{$ifndef fpc}
   , ShowHtmlUnit
{$endif}
   ;

const treeTagColumn = 0;
const treeButtonColumn = 1;
const treeHasIgnoredColumn = 2;
const treePercentageColumn = 3;
const treeCoverageColumn = 4;
const treeNumberColumn = 5;
const treeNilColumn = 6;
const treeEmptyColumn = 7;
const treeDistinctColumn = 8;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
type
  PDataTreeRec = ^TDataTreeRec;
  TDataTreeRec = record
    Bind: TXmlCvrg;
  end;

procedure TShowXmlCoverageForm.FormCreate(Sender: TObject);
var
  wBttn: Integer;
begin
  wBttn := TreeView.Header.Columns [treeButtonColumn].Width;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    doShowIgnoreds := BooleanByNameDef['doShowIgnoreds', False];
  finally
    Free;
  end;
  TreeView.Header.Columns [treeButtonColumn].Width := wBttn;
  TreeView.Header.Columns [treeHasIgnoredColumn].Width := wBttn;
  TreeView.NodeDataSize := SizeOf(TDataTreeRec);
  TreeView.RootNodeCount := 0;
  FileContents := TStringList.Create;
  doConfirmRemovals := True;
  TreeView.Colors.GridLineColor := clBtnHighlight;
  DocumentationMemo.Color := self.Color;
end;

procedure TShowXmlCoverageForm.ShowBind (Bind: TXmlCvrg; aNode: PVirtualNode);
var
  ChildNode: PVirtualNode;
  AttributeNode: PVirtualNode;
  Data: PDataTreeRec;
  x: Integer;
begin
  ChildNode := TreeView.AddChild(aNode);
  Data := TreeView.GetNodeData(ChildNode);
  Data.Bind := Bind;
  SetBindNodeCheckBox(TreeView, Bind, ChildNode);
  for x := 0 to Bind.Children.Count - 1 do
  begin
    ShowBind (Bind.XmlItems[x], ChildNode);
  end;
end;

procedure TShowXmlCoverageForm.SetBind(aBind: TXmlCvrg);
var
  theNode: PVirtualNode;
begin
  fBind := aBind;
  TreeView.Clear;
  if aBind = nil then
    exit;
  ShowBind (aBind, nil);
  theNode := TreeView.GetFirst;
  if xmlUtil.doExpandFull then
    TreeView.FullExpand (theNode)
  else
  begin
    TreeView.FullCollapse (theNode);
    TreeView.Expanded [theNode] := True;
  end;
  TreeView.Selected [theNode] := True;
  TreeView.FocusedNode := theNode;
end;

procedure TShowXmlCoverageForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
    BooleanByName['doShowIgnoreds'] := doShowIgnoreds;
  finally
    Free;
  end;
  FileContents.Free;
end;

function TShowXmlCoverageForm.NodeToBind (aNode: PVirtualNode): TXmlCvrg;
var
  Data: PDataTreeRec;
begin
  result := nil;
  if Assigned (aNode) then
  begin
    Data := TreeView.GetNodeData(aNode);
    if Assigned (Data) then
      result := Data.Bind;
  end;
end;

procedure TShowXmlCoverageForm.OkButtonClick(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure TShowXmlCoverageForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  theNode: PVirtualNode;
  Selected: Boolean;
begin
  theNode := TreeView.FocusedNode;
  Selected := (theNode <> nil) and (TreeView.Selected [theNode] = True);
  FindAction.Enabled := (TreeView.RootNodeCount > 0);
  FindNextAction.Enabled := (Selected)
                        and (xmlUtil.SearchString <> '')
                          ;
  FullExpandAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullExpandAction.Enabled := Selected and (theNode.ChildCount > 0);

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and (theNode.ChildCount > 0);

  Handled := True;
end;

procedure TShowXmlCoverageForm.HaveLink(Sender: TObject; aLink: String);
var
  xAnsiLink: String;
begin
  xAnsiLink := aLink;
   OpenDocument(PChar ( xAnsiLink )
               ); { *Converted from ShellExecute* }
end;

procedure TShowXmlCoverageForm.XsdDocumentationMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MemoMouseDown(Sender as TLzRichEdit, X, Y, HaveLink);
end;

procedure TShowXmlCoverageForm.XsdDocumentationMemoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MemoMouseMove(Sender as TLzRichEdit, X, Y);
end;

procedure TShowXmlCoverageForm.SearchMissing(aDown: Boolean);
var
  xNode: PVirtualNode;
  xData: PDataTreeRec;
begin
  if not Assigned (TreeView.FocusedNode) then
    Raise Exception.Create ('Only possible when a node has focus');
  if aDown then
    xNode := TreeView.GetNext(TreeView.FocusedNode)
  else
    xNode := TreeView.GetPrevious(TreeView.FocusedNode);
  if Assigned (xNode) then
    xData := TreeView.GetNodeData(xNode);
  while Assigned (xNode)
  and (   xData.Bind.isIgnored
       or (xData.Bind.GreenCounter > 0)
      )
  do
  begin
    if aDown then
      xNode := TreeView.GetNext(xNode)
    else
      xNode := TreeView.GetPrevious(xNode);
    if Assigned (xNode) then
      xData := TreeView.GetNodeData(xNode);
  end;
  if not Assigned (xNode) then
    raise Exception.Create('not found');
  TreeView.Selected [xNode] := True;
  TreeView.FocusedNode := xNode;
end;

function TShowXmlCoverageForm.SelectedBind: TXmlCvrg;
var
  xAttr: TXmlAttribute;
begin
  result := NodeToBind(TreeView.FocusedNode);
end;

procedure TShowXmlCoverageForm.FullExpandActionExecute(Sender: TObject);
var
  theNode: PVirtualNode;
begin
  if ToolButtonUsed(Sender) then
    theNode := TreeView.GetFirst
  else
    theNode := TreeView.FocusedNode;
  if theNode = nil then
    exit;
  TreeView.FullExpand (theNode);
end;

procedure TShowXmlCoverageForm.FullCollapseActionExecute(Sender: TObject);
var
  theNode: PVirtualNode;
begin
  if ToolButtonUsed(Sender) then
    theNode := TreeView.GetFirst
  else
    theNode := TreeView.FocusedNode;
  if theNode = nil then
    exit;
  TreeView.FullCollapse (theNode);
end;

procedure TShowXmlCoverageForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Bind: TXmlCvrg;
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
        CurItem := TreeView.GetNext (TreeView.FocusedNode);
      if (CurItem = nil) // if next object is nil
      or (xmlUtil.SearchScope = 1) then // or search entire scope
        CurItem := TreeView.GetFirst; // search from begin
      while not (CurItem = nil)
      and not Found do
      begin
        Bind := NodeToBind (CurItem);
        if xmlUtil.SearchIn = 0 then // search tag
          Found := StringMatchesMask ( Bind.Name
                                     , xmlUtil.SearchString
                                     , False
                                     , xmlUtil.SearchUseRegExp
                                     )
        else // search description
          Found := StringMatchesMask ( Bind.Value
                                     , xmlUtil.SearchString
                                     , False
                                     , xmlUtil.SearchUseRegExp
                                     );
        if not Found then
          CurItem := TreeView.GetNext (CurItem);
      end;
      if not Found then
        ShowMessage (xmlUtil.SearchString + ' not found')
      else
      begin
        TreeView.FocusedNode := CurItem;
        TreeView.Selected [CurItem] := True;
        {
        TreeView.OnChange (TreeView, CurItem);
        }
      end;
    end;
  finally
    FreeAndNil (FindDlg);
  end;
end;

procedure TShowXmlCoverageForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  Bind: TXmlCvrg;
begin
  if True then
  begin
    Found := False;
    CurNode := TreeView.GetNext (TreeView.FocusedNode);
    while not (CurNode = nil)
    and not Found do
    begin
      Bind := NodeToBind (CurNode);
      if xmlUtil.SearchIn = 0 then // search tag
        Found := StringMatchesMask ( Bind.Name
                                   , xmlUtil.SearchString
                                   , False
                                   , xmlUtil.SearchUseRegExp
                                   )
      else // search description
        Found := StringMatchesMask ( Bind.Value
                                   , xmlUtil.SearchString
                                   , False
                                   , xmlUtil.SearchUseRegExp
                                   );
      if not Found then
        CurNode := TreeView.GetNext (CurNode);
    end;
    if not Found then
      ShowMessage (xmlUtil.SearchString + ' not found')
    else
    begin
      TreeView.FocusedNode := CurNode;
      TreeView.Selected [CurNode] := True;
    end;
  end;
end;

procedure TShowXmlCoverageForm.TreeViewPopupMenuPopup(Sender: TObject);
var
  xBind: TXmlCvrg;
begin
  if TreeView.FocusedNode = nil then
    Raise Exception.Create ('no item selected');
  xBind := SelectedBind;
  ToggleIgnoreAction.Enabled := True;
  if xBind.Ignore then
    ToggleIgnoreAction.Caption := 'Take coverage into account'
  else
    ToggleIgnoreAction.Caption := 'Ignore coverage';
  if (not Assigned (xBind.Parent))
  and (not xBind.Ignore) then
    ToggleIgnoreAction.Caption := ToggleIgnoreAction.Caption + ' (unusual at root)';
  FullExpandMenuItem.Enabled := (xBind.Children.Count > 0);
//  FullExpandMenuItem.Caption := 'Full expand  '+ (xBind as TXml).TagName;
  FullCollapseMenuItem.Enabled := (xBind.Children.Count > 0);
//  FullCollapseMenuItem.Caption := 'Full collapse  ' + (xBind as TXml).TagName;
//  ViewAsGridMenuItem.Caption := 'View ' + (xBind as TXml).TagName + ' in a grid';
end;

procedure TShowXmlCoverageForm.upActionExecute(Sender: TObject);
begin
  SearchMissing(False);
end;

procedure TShowXmlCoverageForm.ToggleIgnoreActionExecute(Sender: TObject);
var
  xBind: TXmlCvrg;
begin
  xBind := SelectedBind;
  xBind.Ignore := not xBind.isIgnored;
  Changed := True;
  RevalidateView;
end;

procedure TShowXmlCoverageForm.ToggleShowIgnoredsActionExecute(Sender: TObject);
begin
  doShowIgnoreds := not doShowIgnoreds;
  RevalidateView;
end;

procedure TShowXmlCoverageForm.ToggleShowIgnoredsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if doShowIgnoreds then
    HintStr := 'Hide ignored elements'
  else
    HintStr := 'Show ignored elemets';
end;

function TShowXmlCoverageForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolbutton);
end;

procedure TShowXmlCoverageForm.HaveString(aString: String);
begin
  FileContents.Add (aString);
end;

procedure TShowXmlCoverageForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TShowXmlCoverageForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowXmlCoverageForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xData: PTreeRec;
  xBind: TXmlCvrg;
begin
  Sender.Selected [Sender.FocusedNode] := True;
  xBind := SelectedBind;
  try XmlUtil.ListXsdProperties(XsdPropertiesListView, xBind); except end;
  try XmlUtil.ListXsdDocumentation(DocumentationMemo, xBind, False, False); except end;
  TreeView.Invalidate;
end;

procedure TShowXmlCoverageForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TShowXmlCoverageForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TShowXmlCoverageForm.CloseActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TShowXmlCoverageForm.downActionExecute(Sender: TObject);
begin
  SearchMissing(True);
end;

procedure TShowXmlCoverageForm.PDFBase641Click(Sender: TObject);
begin
  xmlUtil.ZoomAsPDF(SelectedBind);
end;

procedure TShowXmlCoverageForm.ZoomAsHTMLMenuItemClick(Sender: TObject);
begin
  ShowMessage ('XmlUtil.ZoomAsHtml(SelectedBind): not yet implemeted');
//  XmlUtil.ZoomAsHtml(SelectedBind);
end;

procedure TShowXmlCoverageForm.AsHtmlActionExecute(Sender: TObject);
  function indent (x: Integer): String;
  begin
    result := '';
    while x > 0 do
    begin
      result := result + '_';
      Dec (x);
    end;
  end;
  function vTop (aXml: TXml): TXml;
  begin
    result := aXml;
    aXml.AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
  end;
  function nbsp (aText: String): String;
  begin
    result := '_' + aText + '_';
  end;
  function CenteredCell (aText: String): TXml;
  begin
    result := TXml.CreateAsString('td', '');
    with result do
    begin
      AddAttribute(TXmlAttribute.CreateAsString('align', 'center'));
      AddXml (TXml.CreateAsString('a', nbsp(aText)));
    end;
  end;
  function hBarChart(aWidth: Integer; aCvrg: TXmlCvrg): TXml;
  var
    green, red, Denominator: Integer;
  begin
    Denominator := aCvrg.GreenCounter + aCvrg.RedCounter;
    if doShowIgnoreds then
      Inc (Denominator, aCvrg.OrangeCounter);
    Green := Round( aWidth
                   * (aCvrg.GreenCounter / Denominator)
                  );
    Red := aWidth - Green;
    if Green = 0 then
      Inc (Red, 4);
    if Red = 0 then
      Inc (Green, 4);
    result := TXml.CreateAsString('td', '');
    with result do
      with AddXml (TXml.CreateAsString('table', '')) do
        with AddXml (TXml.CreateAsString('tr', '')) do
        begin
          if Green > 0 then
            with AddXml (TXml.CreateAsString('td', '_')) do
              AddAttribute(TXmlAttribute.CreateAsString('style', Format('background-color:green;width:%d', [green])));
          if Red > 0 then
            with AddXml (TXml.CreateAsString('td', '_')) do
              AddAttribute(TXmlAttribute.CreateAsString('style', Format('background-color:red;width:%d', [red])));
        end;
  end;
  procedure details (aXml: TXml; aBind: TXmlCvrg; aIndent: Integer);
  var
    x: Integer;
    prefix: String;
  begin
    if (not doShowIgnoreds) and aBind.isIgnored  then Exit;
    with aXml.AddXml(TXml.CreateAsString('tr', '')) do
    begin
      with AddXml (TXml.CreateAsString('td', '')) do
        AddXml (TXml.CreateAsString('a', indent(aIndent) + aBind.displayName));
      with AddXml (CenteredCell(aBind.DisplayCoverage(doShowIgnoreds))) do
        if aBind.hasIgnored then
          AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'orange'));
      AddXml (CenteredCell(aBind.DisplayPercentage(doShowIgnoreds)));
      AddXml (hBarChart(300, aBind));
      AddXml (CenteredCell(aBind.displayCount));
      AddXml (CenteredCell(aBind.displayNilCount));
      AddXml (CenteredCell(aBind.displayEmptyCount));
      AddXml (CenteredCell(aBind.displayDistinctValueCounter));
    end;
    for x := 0 to aBind.Items.Count - 1 do
      details (aXml, aBind.Items.XmlItems[x] as TXmlCvrg, aIndent + 2);
  end;
var
  tableXml, xXml: TXml;
  xNode: PVirtualNode;
  xData: PDataTreeRec;
  xRow: Integer;
begin
  xXml := TXml.CreateAsString('html', '');
  try
    XmlUtil.PushCursor(crHourGlass);
    try
      tableXml := xXml.AddXml (TXml.CreateAsString('table', ''));
      with tableXml do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('border', '1'));
        AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'lightgray'));
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', 'wsdlStub - Coverage report')) do
            AddAttribute(TXmlAttribute.CreateAsString('colspan', '1'));
          with AddXml (TXml.CreateAsString('td', DateTimeToStr(now))) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('colspan', '7'));
            AddAttribute(TXmlAttribute.CreateAsString('align', 'right'));
          end;
        end;
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', '_')) do
             AddAttribute(TXmlAttribute.CreateAsString('colspan', '8'));
        end;
{}
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Tag '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('Coverage')));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('%')));
          with AddXml (TXml.CreateAsString('td', '')) do
          begin
            AddAttribute (TXmlAttribute.CreateAsString('width', '100px'));
            AddXml (TXml.CreateAsString('b', nbsp('')));
          end;
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('#')));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('# nil')));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('# empty')));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', nbsp('# distinct')));
        end;
        details (tableXml, self.Bind, 0);
      end;
    finally
      XmlUtil.PopCursor;
    end;
    XmlUtil.presentAsHTML('wsdlStub - Coverage report', xXml.asHtmlString);
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TShowXmlCoverageForm.AsHtmlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Show report as HTML';
end;

procedure TShowXmlCoverageForm.B1Click(Sender: TObject);
begin
  xmlUtil.ZoomAsBase64(SelectedBind);
end;

procedure TShowXmlCoverageForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TShowXmlCoverageForm.FormShow(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  GreenColor := RGB(0, 255, 0);
  OrangeColor := RGB(255, 155, 0);
  RedColor := RGB(255, 0, 0);
  case initialExpandStyle of
    esAll: ;
    esOne:
      begin
        TreeView.FullCollapse(TreeView.GetFirst);
        TreeView.Expanded [TreeView.GetFirst] := True;
      end;
    esUsed:
      begin
        TreeView.FullCollapse(TreeView.GetFirst);
        xNode := TreeView.GetFirst;
        while Assigned (xNode) do
        begin
          if xNode.CheckState = csCheckedNormal then
            TreeView.Expanded [xNode] := True;
          xNode := TreeView.GetNext(xNode);
        end;
      end;
  end;
  CancelButton.Visible := doShowCancelButton;
  begin
    OkButton.Caption := '&Close';
    CancelButton.Visible := False;
  end;
  if not CancelButton.Visible then
  begin
    CancelButton.Cancel := False;
    OkButton.Cancel := True;
  end;
  RevalidateView;
end;

function TShowXmlCoverageForm.getDoShowIgnoreds: Boolean;
begin
  result := ToggleShowIgnoredsAction.Checked;
end;

function TShowXmlCoverageForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TShowXmlCoverageForm.setdoEnableCompare(const Value: Boolean);
begin
end;

procedure TShowXmlCoverageForm.setDoShowIgnoreds(const Value: Boolean);
begin
  ToggleShowIgnoredsAction.Checked := Value;
end;

procedure TShowXmlCoverageForm.setIsChanged(const Value: Boolean);
begin
  fIsChanged := Value;
//CancelButton.Enabled := Value;
end;

procedure TShowXmlCoverageForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure TShowXmlCoverageForm.TreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Bind, Parent: TXmlCvrg;
  xName, pName: String;
begin
  HintText := '';
  xName := '';
  pName := '';
  Bind := NodeToBind(Node);
  if Assigned (Bind) then
  begin
    xName := Bind.Name;
    Parent := TXmlCvrg(Bind.Parent);
    if Assigned (Parent) then
      pName := Parent.Name;
  end;
  case Column of
    treeTagColumn: HintText := 'Outline';
    treePercentageColumn: HintText := 'Coverage percentage for ' + xName;
    treeCoverageColumn: HintText := 'Coverage';
    treeNumberColumn: HintText := 'Total number of occurrences for ' + xName;
    treeNilColumn: HintText := Format ('Number of %s items with missing %s', [pName, xName]);
    treeEmptyColumn: HintText := Format ('Number of empty %s elements', [xName]);
    treeDistinctColumn: HintText := Format ('Number of distinct values for %s (nil and empty excluded)', [xName]);
  end;
end;

procedure TShowXmlCoverageForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Bind: TXmlCvrg;
begin
  Bind := NodeToBind(Node);
  case Kind of
    ikNormal, ikSelected:
    begin
      case Column of
        treeTagColumn:
        begin
          if Bind.isIgnored then
            ImageIndex := 112
          else
          begin
            if Bind.RedCounter = 0 then
              ImageIndex := 111
            else
              ImageIndex := 113;
          end;
        end;
        treeButtonColumn:
        begin
          if True then
          begin
            if XmlUtil.isExtendAdviced (Bind) then
            begin
              ImageIndex := 105;
              exit;
            end;
            if XmlUtil.isTreeAdviced (Bind) then
            begin
              ImageIndex := 98;
              exit;
            end;
            ImageIndex := -1;
          end;
        end;
        treeHasIgnoredColumn: if Bind.hasIgnored then ImageIndex := 112;
      end; {case column}
    end; {Kind in ikNormal, ikSelected}
  end; {case Kind}
end;

procedure TShowXmlCoverageForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Bind: TXmlCvrg;
begin
  CellText := '';
  Bind := NodeToBind (Node);
  if Assigned (Bind) then
  begin
    case Column of
      treeTagColumn: CellText := Bind.displayName;
      treePercentageColumn: CellText := Bind.DisplayPercentage(doShowIgnoreds);
      treeCoverageColumn: CellText := Bind.DisplayCoverage(doShowIgnoreds);
      treeNumberColumn: CellText := Bind.displayCount;
      treeNilColumn: CellText := Bind.displayNilCount;
      treeEmptyColumn: CellText := Bind.displayEmptyCount;
      treeDistinctColumn: CellText := Bind.displayDistinctValueCounter;
    end;
  end;
end;

procedure TShowXmlCoverageForm.RevalidateView;
var
  xNode: PVirtualNode;
  xBind: TXmlCvrg;
begin
  Bind.CalculateCoverage;
  xNode := TreeView.GetFirst; // search from begin
  while Assigned (xNode) do
  begin
    xBind := NodeToBind (xNode);
    TreeView.IsVisible[xNode] := (   not xBind.isIgnored
                                  or doShowIgnoreds
                                 );
    xNode := TreeView.GetNext (xNode);
  end;
  TreeView.Invalidate;
end;

procedure TShowXmlCoverageForm.RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
var
  xNode: PVirtualNode;
  Bind: TXmlCvrg;
begin
  xNode := aTreeView.GetFirst; // search from begin
  while not (xNode = nil) do
  begin
    Bind := NodeToBind (xNode);
    if Assigned (Bind) then
    begin
      if (Bind.Counter > 0) then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    xNode := aTreeView.GetNext (xNode);
  end;
  with aTreeView do
  begin
//  IsVisible [GetNextSibling (GetFirst)] := WsdlOperation.CheckExpectedValues;
    Invalidate;
  end;
end;

procedure TShowXmlCoverageForm.SetBindNodeCheckBox(aTreeView: TVirtualStringTree;
  aBind: TXmlCvrg; aNode: PVirtualNode);
begin
  aNode.CheckType := ctNone;
{}{
  if aBind.Counter > 0 then
    aNode.CheckState := csCheckedNormal
  else
    aNode.CheckState := csUnCheckedNormal;
  {}
end;

procedure TShowXmlCoverageForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  xRect: TRect;
  Bind: TXmlCvrg;
  Left, Green, Orange, Red, Denominator: Integer;
begin
  Bind := NodeToBind(Node);
  if Assigned (Bind) then
  begin
    Denominator := Bind.GreenCounter + Bind.RedCounter;
    if doShowIgnoreds then
      Inc (Denominator, Bind.OrangeCounter);
    if Column = treeCoverageColumn then
    begin
      Green := Round( (CellRect.Right - CellRect.Left - 2)
                     * (Bind.GreenCounter / Denominator)
                    );
      if doShowIgnoreds then
        Orange := Round( (CellRect.Right - CellRect.Left - 2)
                        * (Bind.OrangeCounter / Denominator)
                       )
      else
        Orange := 0;
      red := CellRect.Right - CellRect.Left - 2 - Green - Orange;
      left := CellRect.Left + 1;
      if Green > 0 then
      begin
        xRect := Rect ( Left
                      , CellRect.Top + 1
                      , Left + Green
                      , CellRect.Bottom - 1
                      );
        TargetCanvas.Brush.Color := GreenColor;
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.FillRect( xRect );
        Inc (Left, Green);
      end;
      if Orange > 0 then
      begin
        xRect := Rect ( Left
                      , CellRect.Top + 1
                      , Left + Orange
                      , CellRect.Bottom - 1
                      );
        TargetCanvas.Brush.Color := OrangeColor;
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.FillRect( xRect );
        Inc (Left, Orange);
      end;
      if Red > 0 then
      begin
        xRect := Rect ( Left
                      , CellRect.Top + 1
                      , Left + Red
                      , CellRect.Bottom - 1
                      );
        TargetCanvas.Brush.Color := RedColor;
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.FillRect( xRect );
        Inc (Left, Red);
      end;
    end;
  end;
end;

procedure TShowXmlCoverageForm.TreeViewClick(Sender: TObject);
var
  xForm: TShowXmlCoverageForm;
  xBind: TXmlCvrg;
begin
  xBind := NodeToBind(TreeView.FocusedNode);
  if (TreeView.FocusedColumn = treeButtonColumn)
  and (xBind.Items.Count > 0)
  and (xBind <> Bind)
  then
  begin
    Application.CreateForm(TShowXmlCoverageForm, xForm);
    try
      xForm.Bind := xBind;
      xForm.Caption := Caption + ' : ' + xBind.Name;
      xForm.initialExpandStyle := initialExpandStyle;
      xForm.ShowModal;
      RevalidateView;
    finally
      xForm.Free;
    end;
  end;
end;

procedure TShowXmlCoverageForm.ZoomMenuItemClick(Sender: TObject);
begin
  xmlUtil.presentString (SelectedBind.FullCaption, SelectedBind.Value);
end;

procedure TShowXmlCoverageForm .DocumentationMemoClick (Sender : TObject );
begin
  OpenUrl(MemoIsLink(DocumentationMemo));
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

