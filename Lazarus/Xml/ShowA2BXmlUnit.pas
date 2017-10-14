unit ShowA2BXmlUnit;
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
  SysUtils
   , Classes, Graphics, Forms, Controls,
  ComCtrls, ExtCtrls, VirtualTrees
//   , IpmGunMainForm
   , A2BXmlz, Xmlz, Xsdz, a2bStringListUnit, Menus, Dialogs, ActnList, FormIniFilez
   ;

type

  { TShowA2BXmlForm }

  TShowA2BXmlForm = class(TForm)
    MenuItem1: TMenuItem;
    AddToSortColumnsMenuItem: TMenuItem;
    A2BGridMenuItem : TMenuItem ;
    checkRegExpFullCapMenuItem: TMenuItem;
    Panel1: TPanel;
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
    WriteMenuItem: TMenuItem;
    CopyDataToClipboardMenuItem: TMenuItem;
    FullExpandMenuItem: TMenuItem;
    FullCollapseMenuItem: TMenuItem;
    NodeCopyAction: TAction;
    NodeFullExpandAction: TAction;
    NodeFullCollapseAction: TAction;
    NodeWriteXmlAction: TAction;
    ActionImageList: TImageList;
    TreeViewImageList: TImageList;
    ignoreDiffrenvesOnMenuItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    NextDiffAction: TAction;
    PrevDiffAction: TAction;
    CloseAction: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ignoreFullCaptionMenuitem: TMenuItem;
    N3: TMenuItem;
    IgnoreOrderFullCaptionInclPrefixMenuItem: TMenuItem;
    ShowInWordMenuItem: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    IgnoreAddingMenuItem: TMenuItem;
    IgnoreAddingFullCaptionMenuItem: TMenuItem;
    N6: TMenuItem;
    IgnoreRemovingTagMenuItem: TMenuItem;
    IgnoreRemovingFullCaptionMenuItem: TMenuItem;
    procedure A2BGridMenuItemClick (Sender : TObject );
    procedure AddToSortColumnsMenuItemClick(Sender: TObject);
    procedure ignoreFullCaptionMenuitemClick(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure checkRegExpFullCapMenuItemClick(Sender: TObject);
    procedure NextDiffActionUpdate(Sender: TObject);
    procedure PrevDiffActionUpdate(Sender: TObject);
    procedure PrevDiffActionExecute(Sender: TObject);
    procedure NextDiffActionExecute(Sender: TObject);
    procedure ignoreDiffrenvesOnMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewEdited (Sender : TBaseVirtualTree ; Node : PVirtualNode ;
      Column : TColumnIndex );
    procedure TreeViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure TreeViewKeyDown (Sender : TObject ; var Key : Word ;
      Shift : TShiftState );
    procedure TreeViewNewText (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Column : TColumnIndex ; const NewText : String );
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
    procedure XmlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure IgnoreOrderOfTagMenuItemClick(Sender: TObject);
    procedure IgnoreOrderFullCaptionInclPrefixMenuItemClick(Sender: TObject);
    procedure ShowInWordMenuItemClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure IgnoreAddingMenuItemClick(Sender: TObject);
    procedure IgnoreAddingFullCaptionMenuItemClick(Sender: TObject);
    procedure IgnoreRemovingFullCaptionMenuItemClick(Sender: TObject);
    procedure IgnoreRemovingTagMenuItemClick(Sender: TObject);
  private
    fXml: TA2BXml;
    IniFile: TFormIniFile;
    FileName: String;
    SearchString: String;
    SearchScope: Integer;
    SearchIn: Integer;
    SearchUseRegExp: Boolean;
    FileContents: TStringList;
    procedure HaveString (aString: String);
    procedure setColumnHeaderA (AValue : String );
    procedure setColumnHeaderB (AValue : String );
    function ToolButtonUsed (Sender: TObject): Boolean;
    procedure SetXml (aXml: TA2BXml);
    procedure NodeToXml (aNode: PVirtualNode; var Xml: TA2BXml);
    procedure SelectedXml(var aXml: TA2BXml);
    procedure SearchDiff (aDown: Boolean);
  public
    RefreshNeeded: Boolean;
    ignoreDifferencesOn, checkValueAgainst, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TStringList;
    property Xml: TA2BXml read fXml write SetXml;
    property ColumnHeaderA: String write setColumnHeaderA;
    property ColumnHeaderB: String write setColumnHeaderB;
  end;


implementation

uses FindRegExpDialog
   , igGlobals
   , StrUtils
   , xmlUtilz
   , dualListUnit
   , wrdFunctionz
   , base64
   , Clipbrd
   , A2BXmlGridUnit
   , EditRegExpUnit
   ;
const
  iiGreenBullet = 132;
  iiRedBullet = 133;
  iiOrangeBullet = 135;
  iiRedCross = 134;
  iiOrangeCross = 141;
  iiRedPlus = 68;
  iiOrangePlus = 140;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
type
  PXmlTreeRec = ^TXmlTreeRec;
  TXmlTreeRec = record
    Xml: TA2BXml;
  end;

type TColumnEnum =
( tagColumn
, buttonColumn
, AColumn
, BColumn
);

function rmPrefix (aName: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aName) do
  begin
    if aName [x] = ':' then
      result := ''
    else
      result := result + aName [x];
  end;
end;

procedure TShowA2BXmlForm.FormCreate(Sender: TObject);
var
  wBttn: Integer;
begin
  wBttn := TreeView.Header.Columns [Ord(buttonColumn)].Width;
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  TreeView.Header.Columns [Ord(buttonColumn)].Width := wBttn;
  TreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  TreeView.RootNodeCount := 0;
  FileContents := TStringList.Create;
  CloseAction.ShortCut := VK_ESCAPE;
end;

procedure TShowA2BXmlForm .TreeViewEdited (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Column : TColumnIndex );
begin

end;

procedure TShowA2BXmlForm.TreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (aColumn <> buttonColumn);
end;

procedure TShowA2BXmlForm.SetXml(aXml: TA2BXml);
  procedure _ShowXml (Xml: TA2BXml; aNode: PVirtualNode);
  var
    ChildNode: PVirtualNode;
    Data: PXmlTreeRec;
    x: Integer;
  begin
    ChildNode := TreeView.AddChild(aNode);
    Data := TreeView.GetNodeData(ChildNode);
    Data.Xml := Xml;
    for x := 0 to (Xml.Items as TXmlList).Count - 1 do
    begin
      _ShowXml ((Xml.Items as TXmlList).XmlItems [x] as TA2BXml, ChildNode);
    end;
  end;
var
  xNode: PVirtualNode;
  xData: PXmlTreeRec;
  swapCursor: TCursor;
begin
  fXml := aXml;
  TreeView.Clear;
  if aXml = nil then
    exit;
  XmlUtil.PushCursor(crHourGlass);
  try
    _ShowXml (aXml, nil);
    xNode := TreeView.GetFirst;
    TreeView.FullCollapse (xNode);
    xData := TreeView.GetNodeData(xNode);
    while Assigned (xNode)
    and (   (xData.Xml.ChangeKind = ckCopy)
         or (xData.Xml.Ignored)
        ) do
    begin
      xNode := TreeView.GetNext(xNode);
      if Assigned (xNode) then
        xData := TreeView.GetNodeData(xNode);
    end;
    if not Assigned (xNode) then
      xNode := TreeView.GetFirst;
    if xNode = Treeview.GetFirst then
    begin
      TreeView.Selected [xNode] := True;
      TreeView.FocusedNode := xNode;
      TreeView.FullExpand(xNode);
    end
    else
    begin
      TreeView.Selected [xNode] := True;
      TreeView.FocusedNode := xNode;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TShowA2BXmlForm.ShowInWordMenuItemClick(Sender: TObject);
var
  aFileName, bFileName, aValue, bValue: String;
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    if (xXml.Items.Count > 0)
    or not (xXml.ChangeKind in [ckModify, ckCopy]) then
      Exit;
    aValue := xXml.Value;
    bValue := xXml.bValue;

    if AnsiStartsStr(base64RtfStartStr, aValue) then
      aFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileA.rtf'
    else
      aFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileA.docx';
    if AnsiStartsStr(base64DocxStartStr, aValue)
    or AnsiStartsStr(base64RtfStartStr, aValue) then
      SaveStringToFile ( aFileName , DecodeStringBase64 (aValue))
    else
      wrdStringToFile(aValue, aFileName);

    if AnsiStartsStr(base64RtfStartStr, bValue) then
      bFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileB.rtf'
    else
      bFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileB.docx';
    if AnsiStartsStr(base64DocxStartStr, bValue)
    or AnsiStartsStr(base64RtfStartStr, bValue) then
      SaveStringToFile ( bFileName , DecodeStringBase64 (bValue))
    else
      wrdStringToFile(bValue, bFileName);

    wrdFileDiffencesShow(bFileName, aFileName);
  end;
end;

procedure TShowA2BXmlForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
  FileContents.Free;
end;

procedure TShowA2BXmlForm.NodeToXml (aNode: PVirtualNode; var Xml: TA2BXml);
var
  Data: PXmlTreeRec;
begin
  Xml := nil;
  if Assigned (aNode) then
  begin
    Data := TreeView.GetNodeData(aNode);
    if Assigned (Data) then
      Xml := Data.Xml;
  end;
end;

procedure TShowA2BXmlForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  theNode: PVirtualNode;
  Selected: Boolean;
begin
  theNode := TreeView.FocusedNode;
  Selected := (theNode <> nil) and (TreeView.Selected [theNode] = True);

  WriteXmlAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeWriteXmlAction.Enabled := Selected;

  CopyAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeCopyAction.Enabled := Selected;

  FindAction.Enabled := (TreeView.RootNodeCount > 0);
  FindNextAction.Enabled := (Selected)
                        and (SearchString <> '')
                          ;
  FullExpandAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullExpandAction.Enabled := Selected and (theNode.ChildCount > 0);

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and (theNode.ChildCount > 0);

  Handled := True;
end;

procedure TShowA2BXmlForm .TreeViewKeyDown (Sender : TObject ; var Key : Word ;
  Shift : TShiftState );
begin
  if (Key = VK_RETURN) then with (Sender as TVirtualStringTree) do
    EditNode (FocusedNode, FocusedColumn);
end;

procedure TShowA2BXmlForm .TreeViewNewText (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Column : TColumnIndex ; const NewText : String );
var
  oldText: String;
begin
  TreeViewGetText(Sender, Node, Column, ttNormal, oldText);
  if NewText <> oldText then
    ShowMessage('Change not accepted because form is readonly');
end;

procedure TShowA2BXmlForm.WriteXmlActionExecute(Sender: TObject);
var
  xItem: TA2BXml;
begin
  xItem := nil;
  if ToolButtonUsed(Sender) then
    NodeToXml (TreeView.GetFirst, xItem)
  else
    SelectedXml(xItem);
  if xItem <> nil then
  begin
    SaveFileDialog.InitialDir := FileName;
    SaveFileDialog.DefaultExt := 'XML';
    SaveFileDialog.Filter := 'XML File (*.XML)|*.XML';
    SaveFileDialog.Title := 'Save '
                          + xItem.TagName
                          + ' information as...'
                          ;
    if SaveFileDialog.Execute = True then
    begin
      FileName := SaveFileDialog.FileName;
      FileContents.Text := xItem.StreamXml (False, True, 0, False, False);
      FileContents.SaveToFile (FileName);
      FileContents.Clear;
    end;
  end;
end;

procedure TShowA2BXmlForm.SelectedXml(var aXml: TA2BXml);
begin
  NodeToXml(TreeView.FocusedNode, aXml);
end;

procedure TShowA2BXmlForm.CopyActionExecute(Sender: TObject);
var
  theXml: TA2BXml;
begin
  theXml := nil;
  if ToolButtonUsed(Sender) then
    NodeToXml (TreeView.GetFirst, theXml)
  else
    NodeToXml (TreeView.FocusedNode, theXml);
  if theXml = nil then
    exit;
  Clipboard.AsText := theXml.AsTabSeparatedValues;
end;

procedure TShowA2BXmlForm.FullExpandActionExecute(Sender: TObject);
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

procedure TShowA2BXmlForm.FullCollapseActionExecute(Sender: TObject);
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

procedure TShowA2BXmlForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Xml: TA2BXml;
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    FindDlg.Caption := 'Find Tag';
    FindDlg.SearchEdit.Text := SearchString;
    FindDlg.ScopeRadioGroup.ItemIndex := SearchScope;
    FindDlg.SearchInRadioGroup.ItemIndex := SearchIn;
    FindDlg.RegularExpressionCheckBox.Checked := SearchUseRegExp;
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      SearchString := FindDlg.SearchEdit.Text;
      SearchScope := FindDlg.ScopeRadioGroup.ItemIndex;
      SearchIn := FindDlg.SearchInRadioGroup.ItemIndex;
      SearchUseRegExp := FindDlg.RegularExpressionCheckBox.Checked;
      Found := False;
      if SearchScope = 0 then // Search from next object
        CurItem := TreeView.GetNext (TreeView.FocusedNode);
      if (CurItem = nil) // if next object is nil
      or (SearchScope = 1) then // or search entire scope
        CurItem := TreeView.GetFirst; // search from begin
      Xml := nil;
      while not (CurItem = nil)
      and not Found do
      begin
        NodeToXml (CurItem, Xml);
        if SearchIn = 0 then // search tag
          Found := StringMatchesMask (Xml.TagName, SearchString, False, SearchUseRegExp)
        else // search description
          Found := StringMatchesMask (Xml.Value, SearchString, False, SearchUseRegExp);
        if not Found then
          CurItem := TreeView.GetNext (CurItem);
      end;
      if not Found then
        ShowMessage (SearchString + ' not found')
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

procedure TShowA2BXmlForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  Xml: TA2BXml;
begin
  if True then
  begin
    Xml := nil;
    Found := False;
    CurNode := TreeView.GetNext (TreeView.FocusedNode);
    while not (CurNode = nil)
    and not Found do
    begin
      NodeToXml (CurNode, Xml);
      if SearchIn = 0 then // search tag
        Found := StringMatchesMask (Xml.TagName, SearchString, False, SearchUseRegExp)
      else // search description
        Found := StringMatchesMask (Xml.Value, SearchString, False, SearchUseRegExp);
      if not Found then
        CurNode := TreeView.GetNext (CurNode);
    end;
    if not Found then
      ShowMessage (SearchString + ' not found')
    else
    begin
      TreeView.FocusedNode := CurNode;
      TreeView.Selected [CurNode] := True;
    end;
  end;
end;

procedure TShowA2BXmlForm.TreeViewPopupMenuPopup(Sender: TObject);
  function _hasRepeaters (aXml: TXml): Boolean;
  var
    x, y: Integer;
  begin
    result := False;
    x := 0;
    while (x < aXml.Items.Count) and (not Result) do
    begin
      y := x + 1;
      while (y < aXml.Items.Count) and (not Result) do
      begin
        result := (aXml.Items.XmlItems[x] <> aXml.Items.XmlItems[y])
              and (aXml.Items.XmlItems[x].Name = aXml.Items.XmlItems[y].Name);
        Inc(y);
      end;
      Inc(x);
    end;
  end;
var
  xXml: TA2BXml;
  f: Integer;
begin
  xXml := nil;
  if TreeView.FocusedNode = nil then exit;
  SelectedXml(xXml);
  ShowInWordMenuItem.Enabled := Assigned (xXml)
                            and (   (xXml.ChangeKind = ckModify)
                                 or (xXml.ChangeKind = ckCopy)
                                )
                              ;
  ignoreDiffrenvesOnMenuItem.Enabled := (Assigned (ignoreDifferencesOn))
                                    and (   (xXml.ChangeKind = ckModify)
                                        );
  ignoreFullCaptionMenuitem.Enabled := ignoreDiffrenvesOnMenuItem.Enabled;
  checkRegExpFullCapMenuItem.Enabled := (Assigned(checkValueAgainst))
                                    and (   (xXml.ChangeKind = ckModify)
                                         or (checkValueAgainst.Values[xXml.FullUQCaption] <> '')
                                        );
  IgnoreAddingMenuItem.Enabled := (Assigned (ignoreAddingOn))
                              and (   (xXml.ChangeKind = ckDelete)
                                  );
  IgnoreAddingFullCaptionMenuItem.Enabled := IgnoreAddingMenuItem.Enabled;
  IgnoreRemovingTagMenuItem.Enabled := (Assigned (ignoreAddingOn))
                                   and (   (xXml.ChangeKind = ckAdd)
                                       );
  IgnoreRemovingFullCaptionMenuItem.Enabled := IgnoreRemovingTagMenuItem.Enabled;
  IgnoreOrderFullCaptionInclPrefixMenuItem.Enabled := (Assigned (ignoreOrderOn))
                                        and (xXml.Differs)
                                        and (not xXml.ThisOneDiffers)
                                        and _hasRepeaters (xXml)
                                            ;
  AddToSortColumnsMenuItem.Enabled := (Assigned(regressionSortColumns))
                                   and (xXml.Items.Count = 0)
                                   and (not regressionSortColumns.Find(xXml.FullUQCaption, f))
                                     ;
  ignoreDiffrenvesOnMenuItem.Caption := 'Ignore differences on: *.' + rmPrefix(xXml.TagName);
  ignoreFullCaptionMenuitem.Caption := 'Ignore differences on: ' + xXml.FullUQCaption;
  checkRegExpFullCapMenuItem.Caption := 'Check regular expression on: ' + xXml.FullUQCaption + '...';
  ignoreFullCaptionMenuitem.Caption := 'Ignore differences on: ' + xXml.FullUQCaption;
  IgnoreAddingMenuItem.Caption := 'Ignore adding of: *.' + rmPrefix(xXml.TagName);
  IgnoreAddingFullCaptionMenuItem.Caption := 'Ignore adding of: ' + xXml.FullUQCaption;
  IgnoreRemovingTagMenuItem.Caption := 'Ignore removing of: *.' + rmPrefix(xXml.TagName);
  IgnoreRemovingFullCaptionMenuItem.Caption := 'Ignore removing of: ' + xXml.FullUQCaption;
  IgnoreOrderFullCaptionInclPrefixMenuItem.Caption := 'Ignore order for repeating subelements for : ' + xXml.Prefix + '.' + xXml.FullUQCaption;
  AddToSortColumnsMenuItem.Caption := 'Add ' + xXml.FullUQCaption + ' to list of additional sort elements';
  CopyDataToClipboardMenuItem.Caption := 'Copy tab separated data from '
                                       + xXml.TagName
                                       + ' to clipboard'
                                       ;
  FullExpandMenuItem.Caption := 'Full expand  '
                              + xXml.TagName
                              ;
  FullCollapseMenuItem.Caption := 'Full collapse  '
                                + xXml.TagName
                                ;
  A2BGridMenuItem.Enabled := xXml.Items.Count > 0;
end;

function TShowA2BXmlForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolbutton);
end;

procedure TShowA2BXmlForm.HaveString(aString: String);
begin
  FileContents.Add (aString);
end;

procedure TShowA2BXmlForm .setColumnHeaderA (AValue : String );
begin
  TreeView.Header.Columns[Ord(AColumn)].Text := AValue;
end;

procedure TShowA2BXmlForm .setColumnHeaderB (AValue : String );
begin
  TreeView.Header.Columns[Ord(BColumn)].Text := AValue;
end;

procedure TShowA2BXmlForm.IgnoreAddingFullCaptionMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreAddingOn.Add(xXml.FullUQCaption);
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.IgnoreAddingMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreAddingOn.Add(xXml.TagName);
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TShowA2BXmlForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Xml: TA2BXml;
begin
  Xml := nil;
  if Column > 0 then
  begin
    NodeToXml(Node, Xml);
    if Assigned (Xml) then
    begin
      if Xml.Group then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect( CellRect );
        end;
      end;
    end;
  end;
end;

procedure TShowA2BXmlForm.TreeViewClick(Sender: TObject);
begin
  case TColumnEnum((Sender as TVirtualStringTree).FocusedColumn) of
    buttonColumn: ShowInWordMenuItemClick(nil);
  end;
end;

procedure TShowA2BXmlForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowA2BXmlForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TreeView.Selected [TreeView.FocusedNode] := True;
end;

procedure TShowA2BXmlForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Xml: TA2BXml;
begin
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString.
  Xml := nil;
  NodeToXml(Node, Xml);
    case TColumnEnum(Column) of
    tagColumn:
       begin
         if Assigned (Xml) then
           CellText := Xml.Name
         else
           CellText := '';
       end;
    AColumn:
       begin
         if Assigned (Xml) then
           CellText := Xml.Value
         else
           CellText := '';
       end;
    BColumn:
       begin
         if Assigned (Xml) then
           CellText := Xml.bValue
         else
           CellText := '';
       end;
    end;
end;

procedure TShowA2BXmlForm.XmlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
  function _if(aCond: Boolean; trueIndex, falseIndex: Integer): Integer;
  begin
    if aCond then
      result := trueIndex
    else
      result := falseIndex;
  end;
var
  xXml: TA2BXml;
begin
  xXml := nil;
  NodeToXml(Node, xXml);
  if not Assigned (xXml) then Exit;
  case TColumnEnum(Column) of
    tagColumn:
      begin
        case Kind of
          ikNormal, ikSelected:
          begin
            if xXml.ThisOneDiffers
            or xXml.Differs then
              case xXml.ChangeKind of
                ckAdd:    ImageIndex := _if (xXml.Ignored, iiOrangePlus, iiRedPlus);
                ckDelete: ImageIndex := _if (xXml.Ignored, iiOrangeCross, iiRedCross);
              else
                ImageIndex := _if (xXml.Ignored, iiOrangeBullet, iiRedBullet);
              end
            else
              ImageIndex := iiGreenBullet;
          end;
        end;
      end;
    buttonColumn:
    begin
      if Assigned (xXml) then
      begin
        if xXml.Items.Count = 0 then
        begin
          case xXml.ChangeKind of
            ckModify: ImageIndex := 136;
            ckCopy: ImageIndex := 137;
          end;
        end;
      end;
    end;
  end;
end;

procedure TShowA2BXmlForm.ignoreDiffrenvesOnMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreDifferencesOn.Add(rmPrefix(xXml.TagName));
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.SearchDiff(aDown: Boolean);
var
  xNode, sNode: PVirtualNode;
  xData: PXmlTreeRec;
begin
  if not Assigned (TreeView.FocusedNode) then
    Raise Exception.Create ('Only possible when a node has focus');
  sNode := Treeview.FocusedNode;
  if aDown then
    xNode := TreeView.GetNext(TreeView.FocusedNode)
  else
    xNode := TreeView.GetPrevious(TreeView.FocusedNode);
  xData := TreeView.GetNodeData(xNode);
  while Assigned (xNode)
  and (   (not xData.Xml.ThisOneDiffers)
       or (xData.Xml.Ignored)
      ) do
  begin
    if aDown then
      xNode := TreeView.GetNext(xNode)
    else
      xNode := TreeView.GetPrevious(xNode);
    if Assigned (xNode) then
      xData := TreeView.GetNodeData(xNode);
  end;
  if not Assigned (xNode) then
  begin
    ShowMessage('not found');
    xNode := sNode;
  end;
  TreeView.Selected [xNode] := True;
  TreeView.FocusedNode := xNode;
end;

procedure TShowA2BXmlForm.NextDiffActionExecute(Sender: TObject);
begin
  SearchDiff(True);
end;

procedure TShowA2BXmlForm.PrevDiffActionExecute(Sender: TObject);
begin
  SearchDiff(False);
end;

procedure TShowA2BXmlForm.PrevDiffActionUpdate(Sender: TObject);
begin
  PrevDiffAction.Enabled := (Assigned (TreeView.FocusedNode));
end;

procedure TShowA2BXmlForm.NextDiffActionUpdate(Sender: TObject);
begin
  NextDiffAction.Enabled := (Assigned (TreeView.FocusedNode));
end;

procedure TShowA2BXmlForm.FormShow(Sender: TObject);
begin
  RefreshNeeded := False;
end;

procedure TShowA2BXmlForm.checkRegExpFullCapMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
  xForm: TEditRegExpForm;
  xRegExp: String;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    Application.CreateForm(TEditRegExpForm, xForm);
    try
      xForm.SampleValueEdit.Text := xXml.Value;
      xRegExp := checkValueAgainst.Values[xXml.FullUQCaption];
      if AnsiStartsStr(checkAgainstRegExpPrefix, xRegExp) then
        xform.RegularExpressionEdit.Text := Copy (xRegExp, Length(checkAgainstRegExpPrefix) + 1, MaxInt)
      else
        xForm.RegularExpressionEdit.Text := '';
      xForm.ShowModal;
      if xForm.ModalResult = mrOK then
      begin
        checkValueAgainst.Values[xXml.fullUQCaption] := checkAgainstRegExpPrefix + xform.RegularExpressionEdit.Text;
        RefreshNeeded := True;
      end;
    finally
      FreeAndNil(xForm);
    end;
  end;
end;

procedure TShowA2BXmlForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TShowA2BXmlForm.ignoreFullCaptionMenuitemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreDifferencesOn.Add(xXml.FullUQCaption);
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.AddToSortColumnsMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    regressionSortColumns.Add(xXml.FullUQCaption);
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm .A2BGridMenuItemClick (Sender : TObject );
var
  theXml: TA2BXml;
  xForm: TA2BXmlGridForm;
begin
  theXml := nil;
  if ToolButtonUsed(Sender) then
    NodeToXml (TreeView.GetFirst, theXml)
  else
    NodeToXml (TreeView.FocusedNode, theXml);
  if theXml = nil then
    exit;
  Application.CreateForm(TA2BXmlGridForm, xForm);
  try
    xForm.ignoreDifferencesOn := ignoreDifferencesOn;
    xForm.checkValueAgainst := checkValueAgainst;
    xForm.ignoreAddingOn := ignoreAddingon;
    xForm.ignoreRemovingOn := ignoreRemovingOn;
    xForm.ignoreOrderOn := ignoreOrderOn;
    xForm.regressionSortColumns := regressionSortColumns;
    xForm.Xml := theXml;
    xForm.ShowModal;
  finally
    xForm.Free;
  end;
end;

procedure TShowA2BXmlForm.IgnoreOrderFullCaptionInclPrefixMenuItemClick(Sender: TObject);
  procedure _fill (aXml: TXml; aSl, aSr: TStringList; aTag: String);
  var
    x, f: Integer;
  begin
    if (aXml.Items.Count = 0)
    and (not aSl.Find (aTag, f))
    and (not aSr.Find (aTag, f)) then
      aSl.Add (aTag);
    for x := 0 to aXml.Items.Count - 1 do
      _fill (aXml.Items.XmlItems[x], aSl, aSr, aTag + '.' + aXml.Items.XmlItems[x].Name);
  end;
var
  xXml: TA2BXml;
  s: String;
  xExisting: Boolean;
  x, f: Integer;
  sl, sr, snew: TStringList;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    sl := TStringList.Create;
    sr := TStringList.Create;
    s := xXml.Prefix + '.' + xXml.FullUQCaption;
    try
      xExisting := ignoreOrderOn.Find (s, f);
      if xExisting then
        sr.Text := (ignoreOrderOn.Objects[f] as TStringList).Text;
      for x := 0 to xXml.Items.Count - 1 do
        _fill (xXml.Items.XmlItems[x], sl, sr, xXml.Items.XmlItems[x].Name);
      Application.CreateForm(TdualListForm, dualListForm);
      try
        dualListForm.Caption := IgnoreOrderFullCaptionInclPrefixMenuItem.Caption;
        dualListForm.DstList.Items.Text := sr.Text;
        dualListForm.SrcList.Items.Text := sl.Text;
        dualListForm.DstCaption := 'Elements to sort on';
        dualListForm.SrcCaption := 'Candidate sort elements';
        duallistForm.EmptySelectionAllowed := True;
        dualListForm.ShowModal;
        if dualListForm.ModalResult = mrOk then
        begin
          sr.Text := dualListForm.DstList.Items.Text;
          if xExisting then
          begin
            if sr.Count = 0 then
              IgnoreOrderOn.Delete(f)
            else
              (ignoreOrderOn.Objects[f] as TStringList).Text := sr.Text
          end
          else
          begin
            if sr.Count > 0 then
            begin
              snew := TStringList.Create;
              snew.Text := sr.Text;
              ignoreOrderOn.AddObject(s, snew);
            end;
          end;
          RefreshNeeded := True;
        end;
      finally
        FreeAndNil (dualListForm);
      end;
    finally
      FreeAndNil (sl);
      FreeAndNil (sr);
    end;
  end;
end;

procedure TShowA2BXmlForm.IgnoreOrderOfTagMenuItemClick(Sender: TObject);
  procedure _createLists (aXml: TXml; aThread: String; aDsts, aSrcs: TStringList);
  var
    x, f: Integer;
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      if aXml.Items.XmlItems[x].Items.Count = 0 then
      begin
        if not aDsts.Find(aThread, f) then
          aSrcs.Add (aThread);
      end
      else
        _createLists(aXml.Items.XmlItems[x], aThread +  '.' + aXml.Items.XmlItems[x].Name, aDsts, aSrcs);
    end;
  end;

var
  xXml: TA2BXml;
  Srcs, Dsts, sl: TStringList;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    if (xXml.Items.Count = 0) then
    begin
      sl := TStringList.Create;
      sl.Add(xXml.TagName);
      ignoreOrderOn.AddObject('*.' + xXml.TagName, sl);
    end
    else
    begin
      Srcs := TStringList.Create;
      Dsts := TStringList.Create;
      _createLists (xXml, xXml.Name, Dsts, Srcs);
      try
        Application.CreateForm(TdualListForm, dualListForm);
        try
          dualListForm.Caption := 'List of elements to order on';
          dualListForm.DstList.Items.Text := Dsts.Text;
          dualListForm.SrcList.Items.Text := Srcs.Text;
          dualListForm.DstCaption := 'Order elements';
          dualListForm.SrcCaption := '';
          duallistForm.EmptySelectionAllowed := False;
          dualListForm.ShowModal;
          if dualListForm.ModalResult = mrOk then
          begin
            sl := TStringList.Create;
            sl.Text := dualListForm.DstList.Items.Text;
            ignoreOrderOn.AddObject('*.' + xXml.TagName, sl);
          end;
        finally
          FreeAndNil (dualListForm);
        end;
      finally
        Srcs.Clear;
        Srcs.Free;
        Dsts.Clear;
        Dsts.Free;
      end;
    end;
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.IgnoreRemovingTagMenuItemClick(Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreRemovingOn.Add(rmPrefix(xXml.TagName));
    RefreshNeeded := True;
  end;
end;

procedure TShowA2BXmlForm.IgnoreRemovingFullCaptionMenuItemClick(
  Sender: TObject);
var
  xXml: TA2BXml;
begin
  xXml := nil;
  if Assigned (TreeView.FocusedNode) then
  begin
    SelectedXml(xXml);
    ignoreRemovingOn.Add(xXml.FullUQCaption);
    RefreshNeeded := True;
  end;
end;

end.

