{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit RecDefToXsdMain;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls
  , Forms, Dialogs, ActnList, ComCtrls, ToolWin, StdCtrls
  , ExtCtrls
  , Registry
  , IpmAnalyser
  , Menus
  , Globals
  , Ipmz
  , Xmlz
  , Grids, VirtualTrees, ImgList
  ;

type
  IDataObject = class (TObject); {could not find original}

type
  TWsToXsdForm = class(TForm)
    Label9: TLabel;
    MainPanel: TPanel;
    ListBoxPanel: TPanel;
    StatusBar: TStatusBar;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Label1: TLabel;
    FileNameEdit: TEdit;
    IpmItemPopupMenu: TPopupMenu;
    IpmItemReadMenuItem: TMenuItem;
    IpmItemWriteMenuItem: TMenuItem;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    CopyDataToClipboardMenuItem: TMenuItem;
    PasteDataFromClipboardMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    FindAction: TAction;
    ToolButton1: TToolButton;
    FindNextAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    FullExpandAction: TAction;
    FullCollapseAction: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ReadDataAction: TAction;
    WriteDataAction: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    FullExpandMenuItem: TMenuItem;
    FullCollapseMenuItem: TMenuItem;
    NodeFullExpandAction: TAction;
    NodeFullCollapseAction: TAction;
    ActionImages: TImageList;
    CloseAction: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TreeViewPanel: TPanel;
    TreeView: TVirtualStringTree;
    Splitter1: TSplitter;
    XsdMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure IpmItemPopupMenuPopup(Sender: TObject);
    procedure TreeViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure TreeViewDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure FindActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure FullExpandActionExecute(Sender: TObject);
    procedure FullCollapseActionExecute(Sender: TObject);
    procedure ReadDataActionExecute(Sender: TObject);
    procedure WriteDataActionExecute(Sender: TObject);
    procedure ReadDataActionHint(var HintStr: String;
      var CanShow: Boolean);
    procedure WriteDataActionHint(var HintStr: String;
      var CanShow: Boolean);
    procedure CopyActionHint(var HintStr: String; var CanShow: Boolean);
    procedure PasteActionHint(var HintStr: String; var CanShow: Boolean);
    procedure FullExpandActionHint(var HintStr: String;
      var CanShow: Boolean);
    procedure FullCollapseActionHint(var HintStr: String;
      var CanShow: Boolean);
    procedure CloseActionExecute(Sender: TObject);
  private
    StringList: TStringList;
    IniFile: TRegIniFile;
    ControlList: TStringList;
    IpmAnalyser: TIpmAnalyser;
    FileContents: TStringList;
    LineNumber: Integer;
    FFileName: String;
    SearchString: String;
    SearchScope: Integer;
    SearchIn: Integer;
    procedure GenerateXsd;
    function ToolButtonUsed (Sender: TObject): Boolean;
    procedure WriteXmlData (aItem: TIpmItem);
    function SelectedIpmItem: TIpmItem;
    function NodeToIpm (aNode: PVirtualNode): TIpmItem;
    procedure FoundError (ErrorString: String; aObject: TObject);
    function GetRequest: String;
    function FullNodeName (Node: TTreeNode): String;
    procedure SetFileName (aFileName: String);
    procedure HaveString (aString: String);
    procedure HaveScanned (aObject: TObject; aScanned: TScanned);
    procedure AnalyserNeedData( Sender: TObject
                              ; var MoreData: Boolean
                              ; var Data: String
                              );
    procedure AnalyserError
                ( Sender: TObject
                ; LineNumber, ColumnNumber, Offset: Integer
                ; TokenString, Data: String
                );
    procedure InitValues (aIpmItem: TIpmItem; KeepMissingValues: Boolean);
    procedure LoadValues (aIpmItem: TIpmItem; aXml: TXml);
    procedure ShowInputIpm (Initial: Boolean);
  public
    FullExpandTreeView: Boolean;
    Viewer: String;
    InputIpmItem: TIpmItem;
    XmlFileName: String;
    property Request: String read GetRequest;
    property FileName: String read FFileName write SetFileName;
    procedure Scan;
    procedure ReadXml (aXml: TXml; KeepMissingValues: Boolean);
    procedure WriteInputXml (aFileName: String);
    procedure ReflectChanges;
  end;

var
  WsToXsdForm: TWsToXsdForm;

implementation

{$R *.DFM}

uses ErrorFound, EditValueUnit
   , FindDialog;

{Consts copied from IpmScanner.pas}
const InitState = 2;
const PicState = 4;

type
  PIpmTreeRec = ^TIpmTreeRec;
  TIpmTreeRec = record
    Ipm: TIpmItem;
  end;

function TWsToXsdForm.FullNodeName (Node: TTreeNode): String;
var
  Separator: String;
  Text: String;
begin
  if Node = nil then
    result := ''
  else
  begin
    result := Node.Text;
    if Node.Parent <> nil then
    begin
      Separator := '.';
      result := FullNodeName (Node.Parent) + Separator + result;
    end;
  end;
end;

procedure TWsToXsdForm.FoundError (ErrorString: String; aObject: TObject);
var
  xNode: PVirtualNode;
  xItem: TIpmItem;
begin
  xItem := aObject as TIpmItem;
  xNode := TreeView.GetFirst;
  while xNode <> nil do
  begin
    if NodeToIpm(xNode) = xItem then
    begin
      TreeView.FocusedNode := xNode;
      TreeView.Selected [xNode] := True;
    end;
    xNode := TreeView.GetNext(xNode);
  end;
  Show;
  raise Exception.Create (ErrorString);
end;

function TWsToXsdForm.GetRequest: String;
begin
  result := InputIpmItem.ValuesToBuffer (FoundError);
end;

procedure TWsToXsdForm.SetFileName (aFileName: String);
begin
  FFileName := aFileName;
  FileNameEdit.Text := FFileName;
  TreeView.Clear;
  InputIpmItem.Free;
  InputIpmItem := nil;
  FileContents.Clear;
  FileContents.LoadFromFile (FileName);
  LineNumber := 0;
  IpmAnalyser.Prepare;
  LineNumber := 0;
  IpmAnalyser.Execute;
  InputIpmItem := BaseIpmItem;
  ShowInputIpm (True);
  GenerateXsd;
end;

procedure TWsToXsdForm.HaveScanned (aObject: TObject; aScanned: TScanned);
begin
end;

procedure TWsToXsdForm.AnalyserNeedData( Sender: TObject
                                 ; var MoreData: Boolean
                                 ; var Data: String
                                 );
begin
  if LineNumber = FileContents.Count then
    MoreData := False
  else
  begin
    Data := FileContents.Strings [LineNumber];
    Inc (LineNumber);
  end;
end;

procedure TWsToXsdForm.AnalyserError
            ( Sender: TObject
            ; LineNumber,  ColumnNumber, Offset: Integer
            ; TokenString, Data: String);
begin
  ErrorFoundDlg.FileNameEdit.Text := FileName;
  ErrorFoundDlg.LineNumberEdit.Text := IntToStr (LineNumber);
  ErrorFoundDlg.ColumnNumberEdit.Text := IntToStr (ColumnNumber);
  ErrorFoundDlg.TokenStringEdit.Text := TokenString;
  ErrorFoundDlg.Viewer := Viewer;
  ErrorFoundDlg.ShowModal;
end;

procedure TWsToXsdForm.Scan;
begin
end;

procedure TWsToXsdForm.InitValues (aIpmItem: TIpmItem; KeepMissingValues: Boolean);
var
  x: Integer;
begin
  aIpmItem.Loaded := False;
  if not KeepmissingValues then
    aIpmItem.Value := '';
  for x := 0 to (aIpmItem.Items as TIpmItemList).Count - 1 do
  begin
    InitValues ((aIpmItem.Items as TIpmItemList).IpmItems [x], KeepMissingValues);
  end;
end;

procedure TWsToXsdForm.LoadValues (aIpmItem: TIpmItem; aXml: TXml);
var
  x: Integer;
  y: Integer;
  xXml: TXml;
  xIpmItem: TIpmItem;
  function _PrepStr (aString: String): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to system.Length (aString) do
    begin
      case aString [x] of
        '_': result := result + '-';
      else
        result := result + aString [x];
      end;
    end;
    result := LowerCase(result);
  end;
begin
  if aXml = nil then
    raise Exception.Create ( 'Not valid XML data '
                           );
  if (_PrepStr (aXml.TagName) <> _PrepStr (aIpmItem.Name)) then
    raise Exception.Create ( 'Load mismatch: '
                           + 'IpmItem= '
                           + aIpmItem.Name
                           + ' XmlTag= '
                           + aXml.TagName
                           );
  if (aXml.Group <> aIpmItem.Group) then
    raise Exception.Create ( 'Group mismatch: '
                           + 'IpmItem= '
                           + aIpmItem.Name
                           + ' XmlTag= '
                           + aXml.TagName
                           );
  if aXml.Group = False then
  begin
    aIpmItem.Value := aXml.Value
  end
  else
  begin
    for x := 0 to (aXml.Items as TXmlList).Count - 1 do
    begin
      xXml := (aXml.Items as TXmlList).XmlItems [x];
      y := 0;
      xIpmItem := nil;
      while (y < (aIpmItem.Items as TIpmItemList).Count)
      and (xIpmItem = nil)
      do begin
        if (_PrepStr ((aIpmItem.Items as TIpmItemList).IpmItems [y].Name) = _PrepStr (xXml.TagName))
        and ((aIpmItem.Items as TIpmItemList).IpmItems [y].Loaded = False)
        then
          xIpmItem := (aIpmItem.Items as TIpmItemList).IpmItems [y];
        Inc (y);
      end;
      if xIpmItem <> nil then
        LoadValues (xIpmItem, xXml);
    end;
  end;
  aIpmItem.Loaded := True;
end;

procedure TWsToXsdForm.ReadXml (aXml: TXml; KeepMissingValues: Boolean);
begin
  InitValues (InputIpmItem, KeepMissingValues);
  LoadValues (InputIpmItem, aXml);
  ShowInputIpm (False);
end;

procedure TWsToXsdForm.FormCreate(Sender: TObject);
begin
  IniFile := TRegIniFile.Create ('RecDefToXsd.ini');
  Top := IniFile.ReadInteger ('MainScreen', 'Top', Top);
  Left := IniFile.ReadInteger ('MainScreen', 'Left', Left);
  Height := IniFile.ReadInteger ('MainScreen', 'Height', Height);
  Width := IniFile.ReadInteger ('MainScreen', 'Width', Width);
  TreeViewPanel.Width := IniFile.ReadInteger ('MainScreen', 'TreeViewWidth', TreeViewPanel.Width);
  TreeView.Header.Columns [0].Width := IniFile.ReadInteger ('MainScreen', 'TreeViewWidth', TreeView.Header.Columns [0].Width);
  TreeView.Header.Columns [1].Width := IniFile.ReadInteger ('MainScreen', 'Width02', TreeView.Header.Columns [1].Width);
  TreeView.Header.Columns [2].Width := IniFile.ReadInteger ('MainScreen', 'Width03', TreeView.Header.Columns [2].Width);

  IpmAnalyser := TIpmAnalyser.Create (self);
  IpmAnalyser.StartState := InitState;
  IpmAnalyser.OnNeedData := AnalyserNeedData;
  IpmAnalyser.OnError := AnalyserError;
  IpmAnalyser.OnHaveScanned := HaveScanned;
  FileContents := TStringList.Create;

  TreeView.NodeDataSize := SizeOf(TIpmTreeRec);
  TreeView.RootNodeCount := 0;
end;

procedure TWsToXsdForm.FormDestroy(Sender: TObject);
begin
  IniFile.WriteInteger ('MainScreen', 'Top', Top);
  IniFile.WriteInteger ('MainScreen', 'Left', Left);
  IniFile.WriteInteger ('MainScreen', 'Height', Height);
  IniFile.WriteInteger ('MainScreen', 'Width', Width);
  IniFile.WriteInteger ('MainScreen', 'TreeViewWidth', TreeView.Header.Columns [0].Width);
  IniFile.WriteInteger ('MainScreen', 'Width02', TreeView.Header.Columns [1].Width);
  IniFile.WriteInteger ('MainScreen', 'Width03', TreeView.Header.Columns [2].Width);
  IniFile.Free;
  FileContents.Free;
  InputIpmItem.Free;
  IpmAnalyser.Free;
end;

procedure TWsToXsdForm.Panel2Resize(Sender: TObject);
var
  Panel: TPanel;
begin
  Panel := Sender as TPanel;
  FileNameEdit.Width := Panel.Width
                      - FileNameEdit.Left
                      - 5
                      ;
end;

procedure TWsToXsdForm.ReflectChanges;
begin
  TreeView.Invalidate;
end;

procedure TWsToXsdForm.HaveString (aString: String);
begin
  XsdMemo.Lines.Add (aString);
end;

procedure TWsToXsdForm.WriteInputXml (aFileName: String);
begin
  FileContents.Clear;
  GenerateXmlHeader (HaveString);
  InputIpmItem.BuildXML (HaveString, 0);
  FileContents.SaveToFile (aFileName);
  FileContents.Clear;
end;

procedure TWsToXsdForm.IpmItemPopupMenuPopup(Sender: TObject);
var
  xItem: TIpmItem;
begin
  if TreeView.FocusedNode = nil then
  begin
    exit;
  end;
  xItem := SelectedIpmItem;
  IpmItemReadMenuItem.Caption := 'Read data for '
                               + xItem.Caption
                               + ' from file...'
                               ;
  IpmItemWriteMenuItem.Caption := 'Save data from '
                                + xItem.Caption
                                + ' to file...'
                                ;
  CopyDataToClipboardMenuItem.Caption := 'Copy data from '
                                       + xItem.Caption
                                       + ' to clipboard'
                                       ;
  PasteDataFromClipboardMenuItem.Caption := 'Paste data for '
                                          + xItem.Caption
                                          + ' from clipboard'
                                          ;
  FullExpandMenuItem.Caption := 'Full expand  '
                              + xItem.Caption
                              ;
  FullCollapseMenuItem.Caption := 'Full collapse  '
                                + xItem.Caption
                                ;
end;

procedure TWsToXsdForm.TreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Ipm: TIpmItem;
begin
  Allowed := False;
  if Column = 2 then
  begin
    Ipm := NodeToIpm (Node);
    if Assigned (Ipm) then
      if not Ipm.Group then
        Allowed := True;
  end;
  Allowed := False; {Calling my own edittor}
end;

procedure TWsToXsdForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Ipm: TIpmItem;
begin
  Ipm := NodeToIpm (Node);
  if Assigned (Ipm) then
  begin
    case Column of
      0: CellText := Ipm.Caption;
      1: begin
           if not Ipm.Group then
             CellText := Ipm.PictureCaption
           else
             CellText := '';
         end;
      2: begin
           if Ipm.minOccurs <> 1 then
             CellText := IntToStr (Ipm.minOccurs)
           else
             CellText := '';
         end;
      3: begin
           if Ipm.Occurs <> 1 then
             CellText := IntToStr (Ipm.Occurs)
           else
             CellText := '';
         end;
    end;
  end;
end;

function TWsToXsdForm.NodeToIpm(aNode: PVirtualNode): TIpmItem;
var
  Data: PIpmTreeRec;
begin
  result := nil;
  if Assigned (aNode) then
  begin
    Data := TreeView.GetNodeData(aNode);
    if Assigned (Data) then
      result := Data.Ipm;
  end;
end;

function TWsToXsdForm.SelectedIpmItem: TIpmItem;
var
  Data: PIpmTreeRec;
  SelectedNode: PVirtualNode;
begin
  SelectedNode := TreeView.FocusedNode;
  result := nil;
  if SelectedNode <> nil then
  begin
    Data := TreeView.GetNodeData(SelectedNode);
    if assigned (Data) then
      result := Data.Ipm;
  end;
end;

procedure TWsToXsdForm.ShowInputIpm(Initial: Boolean);
  procedure _ShowIpm (Ipm: TIpmItem; aNode: PVirtualNode);
  var
    ChildNode: PVirtualNode;
    Data: PIpmTreeRec;
    x: Integer;
  begin
    ChildNode := TreeView.AddChild(aNode);
    Data := TreeView.GetNodeData(ChildNode);
    Data.Ipm := Ipm;
    for x := 0 to (Ipm.Items as TIpmItemList).Count - 1 do
    begin
      _ShowIpm ((Ipm.Items as TIpmItemList).IpmItems [x], ChildNode);
    end;
  end;
var
  theNOde: PVirtualNode;
begin
  if InputIpmItem = nil then
    exit;
  if Initial then
  begin
    TreeView.Clear;
    _ShowIpm (InputIpmItem, nil);
    theNode := TreeView.GetFirst;
    TreeView.FullExpand (theNode);
    TreeView.Selected [theNode] := True;
    TreeView.FocusedNode := theNode;
  end
  else
  begin
    ReflectChanges;
  end;
end;

procedure TWsToXsdForm.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  StatusBar.SimpleText := SelectedIpmItem.FullCaption;
end;

procedure TWsToXsdForm.TreeViewDblClick(Sender: TObject);
begin
  if TreeView.FocusedNode <> nil then
  begin
    EditValueForm.Ipm := SelectedIpmItem;
    if not EditValueForm.Ipm.Group then
    begin
      EditValueForm.ReadOnly := False;
      EditValueForm.ShowModal;
      if EditValueForm.ModalResult = mrOk then
      begin
        TreeView.InvalidateNode (TreeView.FocusedNode);
      end;
    end;
  end;
end;

procedure TWsToXsdForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Column: TColumnIndex;
begin
  if (Key = VK_RETURN)
  or (Key = VK_F2)
  then
    TreeView.OnDblClick (Sender);
  if Key = VK_TAB then
  begin
    Column := TreeView.FocusedColumn;
    if Column = 2 then
      TreeView.FocusedColumn := 0
    else
      TreeView.FocusedColumn := Column + 1;
  end;
  if (Key = ord ('C'))
  and (Shift = [ssCtrl])
  and (TreeView.FocusedNode <> nil)
  then
    CopyDataToClipboardMenuItem.OnClick (Self);
end;

procedure TWsToXsdForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Item: TIpmItem;
begin
  if (Column <> 0)
  and (Column <> 2)
  then
  begin
    with TargetCanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
      FillRect( CellRect );
    end;
  end;
end;

procedure TWsToXsdForm.TreeViewDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  SourceIpm: TIpmItem;
  DestNode: PVirtualNode;
  DestIpm: TIpmItem;
begin
  if not (Source is TVirtualStringTree) then
    exit;
  SourceIpm := NodeToIpm ((Source as TVirtualStringTree).FocusedNode);
  if not Assigned (SourceIpm) then
    exit;
  DestNode := TreeView.GetNodeAt(Pt.X, Pt.Y);
  if not Assigned (DestNode) then
    exit;
  DestIpm := NodeToIpm (DestNode);
  if not Assigned (DestIpm) then
    exit;
  Accept := True;
end;

procedure TWsToXsdForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Ipm: TIpmItem;
begin
  FindDlg.Caption := 'Find Tag';
  FindDlg.SearchEdit.Text := SearchString;
  FindDlg.ScopeRadioGroup.ItemIndex := SearchScope;
  FindDlg.SearchInRadioGroup.ItemIndex := SearchIn;
  FindDlg.ShowModal;
  if FindDlg.ModalResult = mrOk then
  begin
    SearchString := FindDlg.SearchEdit.Text;
    SearchScope := FindDlg.ScopeRadioGroup.ItemIndex;
    SearchIn := FindDlg.SearchInRadioGroup.ItemIndex;
    Found := False;
    if SearchScope = 0 then // Search from next object
      CurItem := TreeView.GetNext (TreeView.FocusedNode);
    if (CurItem = nil) // if next object is nil
    or (SearchScope = 1) then // or search entire scope
      CurItem := TreeView.GetFirst; // search from begin
    while not (CurItem = nil)
    and not Found do
    begin
      Ipm := NodeToIpm (CurItem);
      if SearchIn = 0 then // search tag
        Found := StringMatchesMask (Ipm.Name, SearchString, False)
      else // search description
        Found := StringMatchesMask (Ipm.Value, SearchString, False);
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
{
  EnableActions;
}
end;

procedure TWsToXsdForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  Ipm: TIpmItem;
begin
  if True then
  begin
    Found := False;
    CurItem := TreeView.GetNext (TreeView.FocusedNode);
    while not (CurItem = nil)
    and not Found do
    begin
      Ipm := NodeToIpm (CurItem);
      if SearchIn = 0 then // search tag
        Found := StringMatchesMask (Ipm.Name, SearchString, False)
      else // search description
        Found := StringMatchesMask (Ipm.Value, SearchString, False);
      if not Found then
        CurItem := TreeView.GetNext (CurItem);
    end;
    if not Found then
      ShowMessage (SearchString + ' not found')
    else
    begin
      TreeView.FocusedNode := CurItem;
      TreeView.Selected [CurItem] := True;
    end;
  end;
{
  EnableActions;
}
end;

procedure TWsToXsdForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  theNode: PVirtualNode;
  Selected: Boolean;
begin
  theNode := TreeView.FocusedNode;
  Selected := (theNode <> nil) and (TreeView.Selected [theNode] = True);

  ReadDataAction.Enabled := True;

  WriteDataAction.Enabled := (TreeView.RootNodeCount > 0);

  FindAction.Enabled := (TreeView.RootNodeCount > 0);
  FindNextAction.Enabled := (Selected)
                        and (SearchString <> '')
                          ;
  FullExpandAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullExpandAction.Enabled := Selected and SelectedIpmItem.Group;

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and SelectedIpmItem.Group;

  Handled := True;
end;

procedure TWsToXsdForm.FullExpandActionExecute(Sender: TObject);
var
  theNode: PVirtualNode;
begin
  if ToolButtonUsed(Sender) then
    theNode := TreeView.GetFirst
  else
    theNode := TreeView.FocusedNode;
  if theNode = nil then
    exit;
  TreeView.FullExpand(theNode);
end;

procedure TWsToXsdForm.FullCollapseActionExecute(Sender: TObject);
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

procedure TWsToXsdForm.ReadDataActionExecute(Sender: TObject);
begin
  OpenFileDialog.Title := 'Open Cobol Working Storage definition';
  OpenFileDialog.InitialDir := FileName;
  if OpenFileDialog.Execute = True then
  begin
    FileName := OpenFileDialog.FileName;
  end;
end;

procedure TWsToXsdForm.WriteXmlData (aItem: TIpmItem);
begin
end;

procedure TWsToXsdForm.WriteDataActionExecute(Sender: TObject);
begin
  SaveFileDialog.Title := 'Save XSD to file';
  SaveFileDialog.InitialDir := FileName;
  SaveFileDialog.DefaultExt := 'XSD';
  SaveFileDialog.Filter := 'XML Schema (*.XSD)|*.XSD';
  if SaveFileDialog.Execute = True then
  begin
    XsdMemo.Lines.SaveToFile (SaveFileDialog.FileName);
  end;
end;

procedure TWsToXsdForm.ReadDataActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Read data for ' + theItem.Caption;
end;

procedure TWsToXsdForm.WriteDataActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Write data from ' + theItem.Caption;
end;

procedure TWsToXsdForm.CopyActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Copy data from ' + theItem.Caption + ' to clipboard';
end;

procedure TWsToXsdForm.PasteActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Paste data for ' + theItem.Caption + ' from clipboard';
end;

procedure TWsToXsdForm.FullExpandActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Full expand node ' + theItem.Caption;
end;

procedure TWsToXsdForm.FullCollapseActionHint(var HintStr: String;
  var CanShow: Boolean);
var
  theItem: TIpmItem;
begin
  theItem := NodeToIpm (Treeview.GetFirst);
  if theItem = nil then
    HintStr := 'No items'
  else
    HintStr := 'Full collapse node ' + theItem.Caption;
end;

function TWsToXsdForm.ToolButtonUsed(Sender: TObject): Boolean;
begin
  result := (Sender is TToolButton);
  if (Sender is TAction) then
    result := ((Sender as TAction).ActionComponent is TToolbutton);
end;

procedure TWsToXsdForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TWsToXsdForm.GenerateXsd;
begin
  XsdMemo.Clear;
  XmlTagsCase := xmlTCLowerCase;
  XmlTagsHyphen := xmlTHUnderscore;
  InputIpmItem.BuildXSD (HaveString, 0);
end;

end.
