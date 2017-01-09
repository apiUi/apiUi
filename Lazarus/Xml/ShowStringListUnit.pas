unit ShowStringListUnit;

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
   , Dialogs
   , FormIniFilez, ToolWin, ActnList, Menus, ImgList
   , FilterDialog
   ;

Type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Index: Integer;
  end;


type
  TShowStringListForm = class;
  TSSLOnGetText = procedure ( aList: TStringList
                            ; aIndex, aColumn: Integer
                            ; var aText: WideString
                            ) of Object;
  TSSLOnFilter = procedure ( aList: TStringList
                           ; aIndex: Integer
                           ; var aPasses: Boolean
                           ) of Object;
  TOnHaveString = procedure (aString: String
                            ) of Object;
  TOnUpdateStatus = procedure (aNumber, aTotal: Integer
                              ) of Object;

  TSearchThread = class(TThread)
  private
    fFilenames: TStringList;
    fFilterDialog: TFilterDlg;
    fOnHaveString: TOnHaveString;
    fOnUpdateStatus: TOnUpdateStatus;
    fString: String;
    fFilter1, fFilter2, fFilter3: String;
    fNumber, fTotal: Integer;
    fForm: TShowStringListForm;
    fEnabled: Boolean;
    function StringPassesFilter (aString: String): Boolean;
    procedure fSynchronisedHaveData;
    procedure fSynchronisedStatusUpdate;
    procedure fSynchronisedEnableAbortButton;
  protected
    procedure UpdateStatus (aNumber, aTotal: Integer);
    procedure Execute; override;
  public
    abortPressed: Boolean;
    constructor Create ( aForm: TShowStringListForm
                       ; aFileNames: TStrings
                       ; aFilter1, aFilter2, aFilter3: String
                       );
  end;

  TShowStringListForm = class(TForm)
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
    ToolButton6: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    NodeCopyAction: TAction;
    NodeFullExpandAction: TAction;
    NodeFullCollapseAction: TAction;
    NodeWriteXmlAction: TAction;
    ActionImageList: TImageList;
    CloseAction: TAction;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    Panel2: TPanel;
    Memo: TMemo;
    DeleteAction: TAction;
    FilterAction: TAction;
    TvPopupMenu: TPopupMenu;
    ZoomMenuAction: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    Splitter1: TSplitter;
    OpenFileAction: TAction;
    ToolButton11: TToolButton;
    ProgressBar: TProgressBar;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    AbortAction: TAction;
    ToolButton14: TToolButton;
    AllXmlAction: TAction;
    procedure AllXmlActionHint(var HintStr: string; var CanShow: Boolean);
    procedure AllXmlActionUpdate(Sender: TObject);
    procedure AllXmlActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure AbortActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure WriteXmlActionExecute(Sender: TObject);
    procedure TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeViewHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ZoomMenuActionClick(Sender: TObject);
    procedure FilterActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewExit(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure XmlTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormDestroy(Sender: TObject);
    procedure XmlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure FindActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure TreeViewPopupMenuPopup(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure TreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
  private
    fReadOnly: Boolean;
    IniFile: TFormIniFile;
    FileName: String;
    ColumnWidths: TStringList;
    SaveLog4JFileName: String;
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    function ToolButtonUsed (Sender: TObject): Boolean;
  public
    isChanged: Boolean;
    Columns: TStringList;
    Data: TStringList;
    OnGetText: TSSLOnGetText;
    OnFilter: TSSLOnFilter;
    SearchThread: TSearchThread;
    numberVisible: Integer;
    property isReadOnly: Boolean read getReadOnly write setReadOnly;
  end;

var
  ShowStringListForm: TShowStringListForm;

implementation

uses FindRegExpDialog
   , igGlobals
   , ClipBrd
   , xmlUtilz
   , strUtils
   , IdSync
   ;

const treeTagColumn = 0;
const treeValueColumn = 2;
const treeButtonColumn = 1;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
function ReadStringFromFile (aFileName: String): String;
  function StreamToString(const Stm: Classes.TStream): string;
  var
    SS: Classes.TStringStream;  // used to copy stream to string
  begin
    SS := Classes.TStringStream.Create('');
    try
      // Copy given stream to string stream and return value
      SS.CopyFrom(Stm, 0);
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  end;
var
  FS: Classes.TFileStream;  // stream used to read file
begin
  // Open stream to file and copy stream to string
  FS := Classes.TFileStream.Create(
    aFileName, SysUtils.fmOpenRead or SysUtils.fmShareDenyNone
  );
  try
    Result := StreamToString(FS);
  finally
    FS.Free;
  end;
end;

procedure TShowStringListForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  TreeView.NodeDataSize := SizeOf (TTreeRec);
  TreeView.RootNodeCount := 0;
//CloseAction.ShortCut := VK_ESCAPE;
  isChanged := False;
  ColumnWidths := TStringList.Create;
  ColumnWidths.Text := IniFile.StringByName['StringListColumnWidths'];
  SaveLog4JFileName := IniFile.StringByNamedef ['SaveLog4JFileName', ''];
  FilterDlg.Caption := 'Configure filter';
end;

procedure TShowStringListForm.XmlTreeViewGetText ( Sender: TBaseVirtualTree
                                      ; Node: PVirtualNode
                                      ; Column: TColumnIndex
                                      ; TextType: TVSTTextType
                                      ; var CellText: WideString
                                      );
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData (Node);
  if Assigned (OnGetText) then
    OnGetText (Data, xData.Index, Column, CellText)
  else
    CellText := Data.Strings [xData.Index];
end;

procedure TShowStringListForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  TreeView.Clear;
  for x := TreeView.Header.Columns.Count - 1  downto 0 do
    ColumnWidths.Values [TreeView.Header.Columns.Items[x].Text]
    := IntToStr (TreeView.Header.Columns.Items[x].Width);
  IniFile.StringByName ['SaveLog4JFileName'] := SaveLog4JFileName;
  IniFile.StringByName['StringListColumnWidths']:=ColumnWidths.Text;
  IniFile.Save;
  IniFile.Free;
  ColumnWidths.Free;
end;

procedure TShowStringListForm.XmlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
{
  with TargetCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := NodeToBind(Node).bgColor (isReadOnly, Column);
    FillRect( CellRect );
  end;
}
end;

procedure TShowStringListForm.ActionList1Update(Action: TBasicAction;
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
                        and (xmlUtil.SearchString <> '')
                          ;
  FullExpandAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullExpandAction.Enabled := Selected and (theNode.ChildCount > 0);

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and (theNode.ChildCount > 0);

  Handled := True;
end;

procedure TShowStringListForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    FindDlg.Caption := 'Find Tag';
    FindDlg.SearchInRadioGroup.Enabled := False;
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      xCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
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
          xData := TreeView.GetNodeData (CurItem);
          Found := StringMatchesMask ( Data.Strings [xData.Index]
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
          TreeView.ClearSelection;
          TreeView.FocusedNode := CurItem;
        end;
      finally
        Screen.Cursor := xCursor;
      end;
    end;
  finally
    FreeAndNil (FindDlg);
  end;
end;

procedure TShowStringListForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if True then
    begin
      Found := False;
      CurNode := TreeView.GetNext (TreeView.FocusedNode);
      while not (CurNode = nil)
      and not Found do
      begin
        xData := TreeView.GetNodeData (CurNode);
        Found := StringMatchesMask ( Data.Strings [xData.Index]
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
        TreeView.ClearSelection;
        TreeView.FocusedNode := CurNode;
        TreeView.Selected [CurNode] := True;
      end;
    end;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.TreeViewPopupMenuPopup(Sender: TObject);
{
var
  Bind: TCustomBindable;
}
begin
{
  if TreeView.FocusedNode = nil then
    Raise Exception.Create ('no item selected');
  Bind := SelectedBind;
  AddMenuItem.Enabled := (Bind is TXml)
                     and Assigned((Bind as TXml).Xsd)
                     and ((Bind as TXml).Xsd.maxOccurs <> '1')
                     ;
  DeleteMenuItem.Enabled := (Bind is TXml)
                        and Assigned((Bind as TXml).Xsd)
                        and ((Bind as TXml).Xsd.maxOccurs <> '1')
                        and ((Bind as TXml).IndexOfRepeatableItem >= xsdElementsWhenRepeatable)
                        and False // As long as it access violates and I do not see why??
                        ;
  PasteMenuItem.Enabled := not isReadOnly;
  PopulateMenuItem.Enabled := not isReadOnly;
  FullExpandMenuItem.Enabled := (Bind.Children.Count > 0);
//  FullExpandMenuItem.Caption := 'Full expand  '+ (Bind as TXml).TagName;
  FullCollapseMenuItem.Enabled := (Bind.Children.Count > 0);
//  FullCollapseMenuItem.Caption := 'Full collapse  ' + (Bind as TXml).TagName;
//  ViewAsGridMenuItem.Caption := 'View ' + (Bind as TXml).TagName + ' in a grid';
  ViewAsGridMenuItem.Enabled := (Bind is TXml)
                            and (Bind.Children.Count > 0);
//  ZoomAsMenuItem.Caption := 'Zoom ' + (Bind as TXml).TagName + ' as';
  ZoomMenuItem.Enabled := (Bind.Children.Count = 0);
  ZoomAsMenuItem.Enabled := (Bind.Children.Count = 0);
}
end;

function TShowStringListForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolbutton);
end;

procedure TShowStringListForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TShowStringListForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowStringListForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  s: WideString;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Sender.Selected [Sender.FocusedNode] := True;
    s := '';
    if Assigned (Node)
    and Assigned (OnGetText) then
    begin
      xData := TreeView.GetNodeData (Node);
      OnGetText (Data, xData.Index, -1, s);
    end;
    Memo.Text := s;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.CloseActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TShowStringListForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

function TShowStringListForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure TShowStringListForm.setReadOnly(const Value: Boolean);
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

procedure TShowStringListForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure TShowStringListForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
{
var
  Bind: TCustomBindable;
  editAllowed: Boolean;
}
begin
{
  Bind := NodeToBind(Node);
  case Kind of
    ikNormal, ikSelected:
    begin
      case Column of
        treeTagColumn:
        begin
        end;
        treeButtonColumn:
        begin
          if Bind is TXmlAttribute then
            ImageIndex := -1;
          if (Bind is TXml)
          or (Bind is TIpmItem) then
          begin
            TreeViewEditing(Sender, Node, treeValueColumn, editAllowed);
            if editAllowed
            and XmlUtil.isEditSupported (Bind) then
              ImageIndex := 97
            else
            begin
              if XmlUtil.isGridAdviced (Bind) then
                ImageIndex := 36
              else
              begin
                if XmlUtil.isTreeAdviced (Bind) then
                  ImageIndex := 98
                else
                  ImageIndex := -1;
              end;
            end;
          end;
        end;
      end; {case column}                 {
    end; {Kind in ikNormal, ikSelected} {
  end; {case Kind} {
}
end;

procedure TShowStringListForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    (Sender as TVirtualStringTree).EditNode
      ( (Sender as TVirtualStringTree).FocusedNode
      , (Sender as TVirtualStringTree).FocusedColumn
      );
  end;
end;

procedure TShowStringListForm.TreeViewClick(Sender: TObject);
var
  xChanged: Boolean;
begin
  begin
    if (Assigned((Sender as TVirtualStringTree).FocusedNode))
    and ((Sender as TVirtualStringTree).FocusedColumn = treeValueColumn) then
      (Sender as TVirtualStringTree).EditNode
        ( (Sender as TVirtualStringTree).FocusedNode
        , (Sender as TVirtualStringTree).FocusedColumn
        );
  end;
end;

procedure TShowStringListForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TreeView.EndEditNode;
end;

procedure TShowStringListForm.ZoomMenuItemClick(Sender: TObject);
var
  xData: PTreeRec;
begin
  if Assigned (TreeView.FocusedNode) then
  begin
    xData := TreeView.GetNodeData (TreeView.FocusedNode);
    xmlUtil.presentString ('', Data.Strings[xData.Index]);
  end;
end;

procedure TShowStringListForm.FormShow(Sender: TObject);
var
  x: Integer;
begin
  if not Assigned (Columns) then
    raise Exception.Create ('arg Columns not assigned');
  if Columns.Count = 0 then
    raise Exception.Create ('arg Columns no headers delivered');
  if not Assigned (Data) then
    raise Exception.Create ('arg Data not assigned');
  TreeView.Header.Columns.Clear;
  for x := 0 to Columns.Count - 1 do
    with TreeView.Header.Columns.Add do
    begin
      Text := Columns.Strings[x];
      Width :=
        StrToIntDef ( ColumnWidths.Values [Text]
                    , Width
                    );
    end;
  TreeView.RootNodeCount := Data.Count;
  TreeView.Header.SortColumn := 0;
end;

procedure TShowStringListForm.FilterActionExecute(Sender: TObject);
var
  xNode: PVirtualNode;
  xPasses: Boolean;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  if not Assigned (OnFilter) then
    raise Exception.Create ('?no onfilter event assigned');
  FilterDlg.ShowModal;
  if FilterDlg.ModalResult = mrOk then
  begin
    xCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      numberVisible := 0;
      xNode := TreeView.GetFirst;
      while Assigned (xNode) do
      begin
        xData := TreeView.GetNodeData (xNode);
        OnFilter (Data, xData.Index, xPasses);
        TreeView.IsVisible [xNode] := xPasses;
        if xPasses then
          Inc (numberVisible);
        xNode := TreeView.GetNext (xNode);
      end;
    finally
      Screen.Cursor := xCursor;
    end;
  end;
end;

procedure TShowStringListForm.ZoomMenuActionClick(Sender: TObject);
var
  xData: PTreeRec;
  xNode: PVirtualNode;
  s: String;
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if not Assigned (TreeView.FocusedNode) then
      Exit;
    if Treeview.SelectedCount = 1 then
    begin
      xData := TreeView.GetNodeData (TreeView.FocusedNode);
      xmlUtil.PresentString ( 'String ' + IntToStr (xData.Index)
                            , Data.Strings [xData.Index]
                            );
    end
    else
    begin
      if TreeView.SelectedCount > 500 then
        raise Exception.Create ( 'More lines ('
                               + IntToStr (TreeView.SelectedCount)
                               + ') selected then allowed (500)'
                               );
      xNode := TreeView.GetFirstSelected;
      s := '<xmlContainer>';
      while Assigned (xNode) do
      begin
        xData := TreeView.GetNodeData (xNode);
        s := s + Data.Strings [xData.Index];
        xNode := TreeView.GetNextSelected (xNode);
      end;
      s := s + '</xmlContainer>';
      xmlUtil.PresentString ( 'Multiple strings'
                            , s
                            );
      s := '';
    end;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.TreeViewCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  s1, s2: WideString;
  d1, d2: PTreeRec;
begin
  Result := 0;
  if not Assigned (OnGetText) then
    Exit;
  d1 := TreeView.GetNodeData (Node1);
  d2 := TreeView.GetNodeData (Node2);
  if Column = 0 then
  begin
    Result := d1.Index - d2.Index;
    Exit;
  end;
  if Column = 1 then
  begin
    Result := Length (Data.Strings[d1.Index]) - Length (Data.Strings[d2.Index]);
    Exit;
  end;
  OnGetText (Data, d1.Index, Column, s1);
  OnGetText (Data, d2.Index, Column, s2);
  if s1 < s2 then
    result := -1;
  if s1 > s2 then
    result := 1;
end;

procedure TShowStringListForm.TreeViewHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if TreeView.Header.SortColumn = Column then
    begin
      if TreeView.Header.SortDirection = sdAscending then
        TreeView.Header.SortDirection := sdDescending
      else
        TreeView.Header.SortDirection := sdAscending;
    end
    else
    begin
      Treeview.Header.SortColumn := Column;
      TreeView.Header.SortDirection := sdAscending;
    end;
    TreeView.SortTree(Column, TreeView.Header.SortDirection, True);
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.TreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData(Node);
  xData.Index := Node.Index;
end;

procedure TShowStringListForm.WriteXmlActionExecute(Sender: TObject);
var
  s: String;
  xNode: PVirtualNode;
  xData: PTreeRec;
  xCursor: TCursor;
begin
  with TSaveDialog.Create (nil) do
  try
    FileName := SaveLog4JFileName;
    Options := Options + [ofOverwritePrompt];
    if Execute then
    begin
      xCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        SaveLog4JFileName := FileName;
        s := '<xmlContainer>';
        xNode := TreeView.GetFirstVisible;
        while Assigned (xNode) do
        begin
          xData := TreeView.GetNodeData (xNode);
          s := s + Data.Strings [xData.Index];
          xNode := TreeView.GetNextVisible (xNode);
        end;
        s := s + '</xmlContainer>';
        SaveStringToFile (FileName, s);
      finally
        s := '';
        Screen.Cursor := xCursor;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TShowStringListForm.OpenFileActionExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Options := Options + [ofAllowMultiSelect];
    if not Execute then
      Exit;
    FilterDlg.ShowModal;
    if FilterDlg.ModalResult <> mrOk then
      Exit;
    Data.Clear;
    TreeView.Clear;
    Treeview.Header.SortColumn := 0;
    TreeView.Header.SortDirection := sdAscending;
    numberVisible := 0;
    SearchThread := TSearchThread.Create ( Self
                                         , Files
                                         , FilterDlg.FindEdit0.Text
                                         , FilterDlg.FindEdit1.Text
                                         , FilterDlg.FindEdit2.Text
                                         );
  finally
    Free;
  end;
end;

{ TSearchThread }

constructor TSearchThread.Create ( aForm: TShowStringListForm
                                 ; aFileNames: TStrings
                                 ; aFilter1, aFilter2, aFilter3: String
                                 );
begin
  inherited Create (False);
  fForm := aForm;
  fFileNames := TStringList.Create;
  fFileNames.Text := aFileNames.Text;
  fFilter1 := aFilter1;
  fFilter2 := aFilter2;
  fFilter3 := aFilter3;
  abortPressed := False;
  FreeOnTerminate := True;
end;

procedure TSearchThread.Execute;
var
  s: String;
  x: Integer;
  sp, ep, xp: PChar;
  xOptions: TStringSearchOptions;
  sSearchString, eSearchString, xSearchString: String;
begin
  fEnabled := True;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
  sSearchString := '<log4j_event';
  eSearchString := '</log4j_event>';
  xOptions := [soDown, soMatchCase];
  if fFilter1 = '' then
    xSearchString := eSearchString
  else
    xSearchString := fFilter1;
  x := 0;
  while (not abortPressed)
  and (x < fFileNames.Count) do
  begin
    UpdateStatus (x + 1, fFileNames.Count + 1);
    s := ReadStringFromFile (fFileNames.Strings[x]);
    ep := @s[1];
    xp := strUtils.SearchBuf(ep, Length(s), 0, 0, xSearchString, xOptions);
    while (not abortPressed)
    and Assigned (xp) do
    begin
      Exclude (xOptions, soDown);
      sp := strUtils.SearchBuf(@s[1], xp - @s[1], xp - @s[1], 0, sSearchString, xOptions);
      if Assigned (sp) then
      begin
        Include (xOptions, soDown);
        ep := strUtils.SearchBuf(xp, Length(s) - (xp - @s[1]), 0, 0, eSearchString, xOptions);
        xp := nil;
        if Assigned (ep) then
        begin
          ep := ep + Length (eSearchString);
          fString := Copy (s, sp - @s[1] + 1, ep - sp);
          if StringPassesFilter (fString) then
          begin
            fForm.Data.Add (fString);
            Inc (fForm.numberVisible);
          end;
{
          begin
            with TIdSync.Create do
              try
                SynchronizeMethod (fSynchronisedHaveData);
              finally
                Free;
              end;
          end;
}
          xp := strUtils.SearchBuf(ep, Length (s) - (ep - @s[1]), 0, 0, xSearchString, xOptions);
        end;
      end;
    end;
    s := '';
    with TIdSync.Create do
      try
        SynchronizeMethod (fSynchronisedHaveData);
      finally
        Free;
      end;
    Inc (x);
  end;
  UpdateStatus (0, 0);
  fEnabled := False;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
end;

procedure TSearchThread.fSynchronisedEnableAbortButton;
begin
  fForm.AbortAction.Enabled := fEnabled;
end;

procedure TSearchThread.fSynchronisedHaveData;
begin
//fForm.Data.Add (fString);
  fForm.TreeView.RootNodeCount := fForm.Data.Count;
end;

procedure TSearchThread.fSynchronisedStatusUpdate;
begin
  fForm.ProgressBar.Max := fTotal;
  fForm.ProgressBar.Position := fNumber;
end;

function TSearchThread.StringPassesFilter(aString: String): Boolean;
begin
  result := (   (fFilter1 = '')
             or (Pos (fFilter1, aString) > 0)
            )
        and (   (fFilter2 = '')
             or (Pos (fFilter2, aString) > 0)
            )
        and (   (fFilter3 = '')
             or (Pos (fFilter3, aString) > 0)
            )
          ;
end;

procedure TSearchThread.UpdateStatus(aNumber, aTotal: Integer);
begin
  fNumber := aNumber;
  fTotal := aTotal;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedStatusUpdate);
    finally
      Free;
    end;
end;

procedure TShowStringListForm.AbortActionExecute(Sender: TObject);
begin
  if Assigned (SearchThread) then
  begin
    SearchThread.abortPressed := True;
    AbortAction.Enabled := False;
  end;
end;

procedure TShowStringListForm.CopyActionExecute(Sender: TObject);
{
  function _Columns (aNode: PVirtualNode): String;
  var
    col: Integer;
    xText: WideString;
    xSep: String;
  begin
    result := '';
    xSep := '';
    for col := 0 to TreeView.Header.Columns.Count - 1 do
    begin
      if ColumnVisible [col] then
      begin
        GridGetText(Grid, aNode, col, ttNormal,xText);
        result := result + xSep + xText;
        xSep := #9;
      end;
    end;
  end;
}
var
  xNode: PVirtualNode;
  xData: PTreeRec;
  s, xSep, xText: WideString;
  c: Integer;
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    xNode := TreeView.GetFirst;
    s := '';
    xSep := '';
    for c := 0 to Columns.Count - 1 do
//    if ColumnVisible [c] then
      begin
        s := s + xSep + '"' + Columns.Strings[c] + '"';
        xSep := #9;
      end;
    xNode := TreeView.GetFirst;
    while Assigned (xNode) do
    begin
      if TreeView.IsVisible [xNode] then
      begin
        xData := TreeView.GetNodeData (xNode);
        s := s + #$D#$A;
        xSep := '';
        for c := 0 to Columns.Count - 1 do
        begin
          if Assigned (OnGetText) then
            OnGetText (Data, xData.Index, c, xText);
          s := s + xSep  + '"' + xText + '"';
          xSep := #9;
        end;
      end;
      xNode := TreeView.GetNext(xNode);
    end;
    Clipboard.AsText := s;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.AllXmlActionExecute(Sender: TObject);
var
  s: String;
  xNode: PVirtualNode;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  if numberVisible >= 500 then
    raise Exception.Create ('Disabled because number of events exceeds supported number (max 500)');
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    s := '<xmlContainer>';
    xNode := TreeView.GetFirstVisible;
    while Assigned (xNode) do
    begin
      xData := TreeView.GetNodeData (xNode);
      s := s + Data.Strings[xData.Index];
      xNode := TreeView.GetNextVisible (xNode);
    end;
    s := s + '</xmlContainer>';
    xmlUtil.PresentString ( 'All (filtered)'
                          , s
                          );
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TShowStringListForm.AllXmlActionUpdate(Sender: TObject);
begin
  AllXmlAction.Enabled := (numberVisible < 500);
end;

procedure TShowStringListForm.AllXmlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if not AllXmlAction.Enabled then
    HintStr := HintStr + ' (disabled because of number of events)';
end;

end.

