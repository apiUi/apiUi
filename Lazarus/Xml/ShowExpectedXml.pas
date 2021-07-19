{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit ShowExpectedXml;

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
  Buttons, ComCtrls, ExtCtrls, VirtualTrees
//   , IpmGunMainForm
   , Bind, Xmlz, Ipmz, Menus, Dialogs, ActnList
   , FormIniFilez
   ;

type
  TShowExpectedXmlForm = class(TForm)
    Panel1: TPanel;
    TreeView: TVirtualStringTree;
    ActionList1: TActionList;
    FindAction: TAction;
    FindNextAction: TAction;
    FullExpandAction: TAction;
    FullCollapseAction: TAction;
    ToolBar1: TToolBar;
    ToolButton3: TToolButton;
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
    TreeViewImageList: TImageList;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    NextDiffAction: TAction;
    PrevDiffAction: TAction;
    CloseAction: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NextDiffActionUpdate(Sender: TObject);
    procedure PrevDiffActionUpdate(Sender: TObject);
    procedure PrevDiffActionExecute(Sender: TObject);
    procedure NextDiffActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
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
    procedure XmlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    fBind: TCustomBindable;
    SearchString: String;
    SearchScope: Integer;
    SearchIn: Integer;
    SearchUseRegExp: Boolean;
    function ToolButtonUsed (Sender: TObject): Boolean;
    procedure SetBind (aBind: TCustomBindable);
    procedure NodeToBind (aNode: PVirtualNode; var Bind: TCustomBindable; var Attribute: TXmlAttribute);
    function SelectedBind: TCustomBindable;
    procedure SearchDiff (aDown: Boolean);
  public
    RefreshNeeded: Boolean;
    property Bind: TCustomBindable read fBind write SetBind;
  end;

var
  ShowExpectedXmlForm: TShowExpectedXmlForm;

implementation

uses FindRegExpDialog
   , igGlobals
   , xmlUtilz
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
type
  PXmlTreeRec = ^TXmlTreeRec;
  TXmlTreeRec = record
    Bind: TCustomBindable;
    Attribute: TXmlAttribute;
  end;

function _rmPrefix (aName: String): String;
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

procedure TShowExpectedXmlForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
  TreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  TreeView.RootNodeCount := 0;
  CloseAction.ShortCut := VK_ESCAPE;
end;

procedure TShowExpectedXmlForm.TreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := False;
end;

procedure TShowExpectedXmlForm.SetBind(aBind: TCustomBindable);
  procedure _ShowBind (aBind: TCustomBindable; aNode: PVirtualNode);
  var
    ChildNode: PVirtualNode;
    AttributeNode: PVirtualNode;
    Data: PXmlTreeRec;
    x: Integer;
  begin
    ChildNode := TreeView.AddChild(aNode);
    Data := TreeView.GetNodeData(ChildNode);
    Data.Bind := aBind;
    Data.Attribute := nil;
    if aBind is TXml then with aBind as TXml do
    begin
      for x := 0 to Attributes.Count - 1 do
      begin
        AttributeNode := TreeView.AddChild(ChildNode);
        Data := TreeView.GetNodeData(AttributeNode);
        Data.Bind := aBind as TXml;
        Data.Attribute := Attributes.XmlAttributes [x];
      end;
    end;
    for x := 0 to aBind.Children.Count - 1 do
    begin
      _ShowBind (aBind.Children.Bindables[x], ChildNode);
    end;
  end;
var
  swapCursor: TCursor;
begin
  fBind := aBind;
  TreeView.Clear;
  if aBind = nil then
    exit;
  XmlUtil.PushCursor(crHourGlass);
  try
    Screen.Cursor := crHourGlass;
    _ShowBind (aBind, nil);
    TreeView.FocusedNode := TreeView.GetFirst;
    TreeView.FullCollapse (Treeview.FocusedNode);
    try
      SearchDiff(True);
    except
      TreeView.FocusedNode := TreeView.GetFirst;
    end;
    TreeView.Selected[TreeView.FocusedNode]:= True;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TShowExpectedXmlForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TShowExpectedXmlForm.NodeToBind (aNode: PVirtualNode; var Bind: TCustomBindable; var Attribute: TXmlAttribute);
var
  Data: PXmlTreeRec;
begin
  Bind := nil;
  Attribute := nil;
  if Assigned (aNode) then
  begin
    Data := TreeView.GetNodeData(aNode);
    if Assigned (Data) then
    begin
      Bind := Data.Bind;
      Attribute := Data.Attribute;
    end;
  end;
end;

procedure TShowExpectedXmlForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  theNode: PVirtualNode;
  Selected: Boolean;
begin
  theNode := TreeView.FocusedNode;
  Selected := (theNode <> nil) and (TreeView.Selected [theNode] = True);

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

function TShowExpectedXmlForm.SelectedBind: TCustomBindable;
var
  xAttr: TXmlAttribute;
begin
  result := nil;
  xAttr := nil; // to avoid compiler warning
  NodeToBind(TreeView.FocusedNode, result, xAttr);
end;

procedure TShowExpectedXmlForm.FullExpandActionExecute(Sender: TObject);
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

procedure TShowExpectedXmlForm.FullCollapseActionExecute(Sender: TObject);
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

procedure TShowExpectedXmlForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Bind: TcustomBindable;
  xAttr: TXmlAttribute;
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
      while not (CurItem = nil)
      and not Found do
      begin
        Bind := nil; // to avoid compiler warning
        xAttr := nil; // to avoid compiler warning
        NodeToBind (CurItem, Bind, xAttr);
        if SearchIn = 0 then // search tag
          Found := StringMatchesMask (Bind.Name, SearchString, False, SearchUseRegExp)
        else // search description
          Found := StringMatchesMask (Bind.Value, SearchString, False, SearchUseRegExp);
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

procedure TShowExpectedXmlForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  Bind: TCustomBindable;
  xAttr: TXmlAttribute;
begin
  if True then
  begin
    Found := False;
    CurNode := TreeView.GetNext (TreeView.FocusedNode);
    while not (CurNode = nil)
    and not Found do
    begin
      Bind := nil; // to avoid compiler warning
      xAttr := nil; // to avoid compiler warning
      NodeToBind (CurNode, Bind, xAttr);
      if SearchIn = 0 then // search tag
        Found := StringMatchesMask (Bind.Name, SearchString, False, SearchUseRegExp)
      else // search description
        Found := StringMatchesMask (Bind.Value, SearchString, False, SearchUseRegExp);
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

procedure TShowExpectedXmlForm.TreeViewPopupMenuPopup(Sender: TObject);
var
  Bind: TCustomBindable;
begin
  if not Assigned (TreeView.FocusedNode) then exit;
  Bind := SelectedBind;
  FullExpandMenuItem.Caption := 'Full expand  '
                              + Bind.Name
                              ;
  FullCollapseMenuItem.Caption := 'Full collapse  '
                                + Bind.Name
                                ;
end;

function TShowExpectedXmlForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolbutton);
end;

procedure TShowExpectedXmlForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TShowExpectedXmlForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Bind: TCustomBindable;
  Attribute: TXmlAttribute;
begin
  if Column > 0 then
  begin
    Bind := nil; // to avoid compiler warning
    Attribute := nil; // to avoid compiler warning
    NodeToBind(Node, Bind, Attribute);
    if Assigned (Attribute) then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := clAqua;
        FillRect( CellRect );
      end;
    end
    else
    begin
      if Assigned (Bind) then
      begin
        if Bind.Group then
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
end;

procedure TShowExpectedXmlForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowExpectedXmlForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TreeView.Selected [TreeView.FocusedNode] := True;
end;

procedure TShowExpectedXmlForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Bind: TCustomBindable;
  Attribute: TXmlAttribute;
begin
  // A handler for the OnGetText event is always needed as it provides
  // the tree with the string data to display.
  // Note that we are always using WideString.
    Bind := nil; // to avoid compiler warning
    Attribute := nil; // to avoid compiler warning
    NodeToBind(Node, Bind, Attribute);
    case Column of
    0: begin
         if Assigned (Attribute) then
           CellText := Attribute.Name
         else
         begin
           if Assigned (Bind) then
             CellText := Bind.Name
           else
             CellText := '';
         end;
       end;
    1: begin
         if Assigned (Attribute) then
           CellText := Attribute.Value
         else
         begin
           if Assigned (Bind) then
           begin
             if (Bind is TIpmItem)
             or Bind.Checked then
               CellText := Bind.Value
             else
               CellText := bindNilstr;
           end
           else
             CellText := '';
         end;
       end;
    2: begin
         if Assigned (Attribute) then
           CellText := Attribute.Value
         else
         begin
           if Assigned (Bind) then
             CellText := Bind.ExpectedValue
           else
             CellText := '';
         end;
       end;
    end;
end;

procedure TShowExpectedXmlForm.XmlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Bind: TCustomBindable;
  xAtt: TXmlAttribute;
begin
  Bind := nil; // to avoid compiler warning
  xAtt := nil; // to avoid compiler warning
  NodeToBind (Node, Bind, xAtt);
  if Column = 0 then
  begin
    case Kind of
      ikNormal, ikSelected:
      begin
        if Assigned (xAtt) then
          ImageIndex := 6
        else
        begin
          if Assigned (Bind) then
          begin
            if not Bind.DoExpectValue then
              ImageIndex := 131
            else
              if Bind.HasUnExpectedValue then
                ImageIndex := 133
              else
                ImageIndex := 132;
          end;
        end;
      end;
    end;
  end;
end;

procedure TShowExpectedXmlForm.SearchDiff(aDown: Boolean);
var
  xNode: PVirtualNode;
  xData: PXmlTreeRec;
begin
  if not Assigned (TreeView.FocusedNode) then
    Raise Exception.Create ('Only possible when a node has focus');
  if aDown then
    xNode := TreeView.GetNext(TreeView.FocusedNode)
  else
    xNode := TreeView.GetPrevious(TreeView.FocusedNode);
  xData := TreeView.GetNodeData(xNode);
  while Assigned (xNode)
  and (   (not xData.Bind.HasUnExpectedValue)
       or (xData.Bind.Group)
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
    raise Exception.Create('not found');
  TreeView.Selected [xNode] := True;
  TreeView.FocusedNode := xNode;
end;

procedure TShowExpectedXmlForm.NextDiffActionExecute(Sender: TObject);
begin
  SearchDiff(True);
end;

procedure TShowExpectedXmlForm.PrevDiffActionExecute(Sender: TObject);
begin
  SearchDiff(False);
end;

procedure TShowExpectedXmlForm.PrevDiffActionUpdate(Sender: TObject);
begin
  PrevDiffAction.Enabled := (Assigned (TreeView.FocusedNode));
end;

procedure TShowExpectedXmlForm.NextDiffActionUpdate(Sender: TObject);
begin
  NextDiffAction.Enabled := (Assigned (TreeView.FocusedNode));
end;

procedure TShowExpectedXmlForm.FormShow(Sender: TObject);
begin
  RefreshNeeded := False;
  Screen.Cursor := crDefault;
end;

procedure TShowExpectedXmlForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

end.

