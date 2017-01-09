unit ShowFolderDifferencesUnit;

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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, FileUtil, ToolWin, FormIniFilez,
  ComCtrls, ImgList, ActnList, a2bStringListUnit, Xmlz, A2BXmlz;

type
  TFileSpec = class (TObject)
    public
      Name: String;
      Size: Integer;
      Modified: TDateTime
  end;
  PVSTreeRec = ^TVSTreeRec;
  TVSTreeRec = record
    aFile: TFileSpec;
    bFile: TFileSpec;
    Match: Boolean;
    A2B: TA2BXml;
  end;

type
  TShowFolderDifferencesForm = class(TForm)
    Panel1: TPanel;
    ToolBar1: TToolBar;
    leftPanel: TPanel;
    mainVST: TVirtualStringTree;
    ActionImageList: TImageList;
    ActionList1: TActionList;
    ToolButton2: TToolButton;
    CheckAllAction: TAction;
    UncheckAllAction: TAction;
    HelpAction: TAction;
    OverviewStatusBar: TStatusBar;
    ToolButton1: TToolButton;
    MaintainIgnoreListAction: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    NextDiffAction: TAction;
    PrevDiffAction: TAction;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    CopyToClipboardAction: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    CloseAction: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    HtmlReportAction: TAction;
    procedure HtmlReportActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure PrevDiffActionExecute(Sender: TObject);
    procedure NextDiffActionExecute(Sender: TObject);
    procedure PrevDiffActionUpdate(Sender: TObject);
    procedure NextDiffActionUpdate(Sender: TObject);
    procedure MaintainIgnoreListActionUpdate(Sender: TObject);
    procedure MaintainIgnoreListActionExecute(Sender: TObject);
    procedure mainVSTClick(Sender: TObject);
    procedure mainVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ToolButton1Click(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure mainVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToggleExpandedButtonClick(Sender: TObject);
    procedure ShowNonVerbsButtonClick(Sender: TObject);
    procedure ShowDeltaButtonClick(Sender: TObject);
    procedure mainVSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure mainVSTResize(Sender: TObject);
    procedure mainVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure mainVSTColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    IniFile: TFormIniFile;
    fPreviewMode: Boolean;
    Diffs: TA2BStringList;
    procedure GetFolderFiles(FolderName: String; aFiles: TStringList);
    function getShowExpanded: Boolean;
    procedure setShowExpanded(const Value: Boolean);
    function getShowNonVerbs: Boolean;
    procedure setShowNonVerbs(const Value: Boolean);
    function getShowDelta: Boolean;
    procedure setShowDelta(const Value: Boolean);
    procedure setPreviewMode(const Value: Boolean);
    procedure PopulateMain;
    procedure CreateA (xData: PVSTreeRec);
    procedure CreateB (xData: PVSTreeRec);
    procedure CompareAB (xData: PVSTreeRec);
    procedure SearchDiff (aDown: Boolean);
    procedure CopyGridOnGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    function XmlFromFile (aFileName: String): TXml;
  public
    FolderName1, FolderName2: String;
    ignoreDifferencesOn, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TStringList;
    orderGroupsOn: TStringList;
    aFiles, bFiles: TStringList;
    ReferenceFileName: String;
//    PrintPreview: TNotifyEvent;
    FinishedEvent: TNotifyEvent;
    property ShowExpanded: Boolean read getShowExpanded write setShowExpanded;
    property ShowDelta: Boolean read getShowDelta write setShowDelta;
    property ShowNonVerbs: Boolean read getShowNonVerbs write setShowNonVerbs;
    property PreviewMode: Boolean read fPreviewMode write setPreviewMode;
  end;

var
  ShowFolderDifferencesForm: TShowFolderDifferencesForm;

implementation

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
{$ENDIF}
  Bind, Registry, ShowA2BXmlUnit, dualListUnit, igGlobals, ClipBrd
   , xmlUtilz
   , vstUtils
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function AppendSlash(const path: string): string;
var
  len: integer;
begin
  len := length(path);
  if (len = 0) or (path[len] = '\') then
    result := path else
    result := path+'\';
end;

procedure TShowFolderDifferencesForm.FormCreate(Sender: TObject);
var
  w3: Integer;
begin
  w3 := mainVST.Header.Columns.Items[3].Width;
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  mainVST.Header.Columns.Items[3].Width := w3;
  mainVST.NodeDataSize := SizeOf (TVSTreeRec);
{
  ShowExpanded := IniFile.ReadBool('rmPreview', 'ShowExpanded', True);
  ShowDelta := IniFile.ReadBool('rmPreview', 'ShowDelta', True);
  ShowNonVerbs := IniFile.ReadBool('rmPreview', 'ShowNonVerbs', True);
}
  Diffs := TA2BStringList.Create;
  aFiles := TSTringList.Create;
  bFiles := TSTringList.Create;
  CloseAction.ShortCut := VK_ESCAPE;
end;

procedure TShowFolderDifferencesForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
{
  IniFile.WriteBool('rmPreview', 'ShowExpanded', ShowExpanded);
  IniFile.WriteBool('rmPreview', 'ShowDelta', ShowDelta);
  IniFile.WriteBool('rmPreview', 'ShowNonVerbs', ShowNonVerbs);
}
  mainVst.Header.Columns.Items[0].Text := 'Name 1';
  mainVst.Header.Columns.Items[4].Text := 'Name 2';
  IniFile.Save;
  IniFile.Free;
  mainVST.Clear;
  Diffs.Free;
  for x := 0 to aFiles.Count - 1 do
    aFiles.Objects[x].Free;
  aFiles.Clear;
  aFiles.Free;
  for x := 0 to aFiles.Count - 1 do
    bFiles.Objects[x].Free;
  bFiles.Clear;
  bFiles.Free;
end;

procedure TShowFolderDifferencesForm.FormShow(Sender: TObject);
begin
  xmlUtil.PushCursor(crHourGlass);
  try
    mainVst.Header.Columns.Items[0].Text := FolderName1;
    mainVst.Header.Columns.Items[4].Text := FolderName2;
    PopulateMain;
    if Assigned (FinishedEvent) then
      FinishedEvent(Self);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TShowFolderDifferencesForm.GetFolderFiles(FolderName: String;
  aFiles: TStringList);
{
  function FileTime2DateTime(FileTime: TFileTime): TDateTime;
  var
    LocalFileTime: TFileTime;
    SystemTime: TSystemTime;
  begin
    FileTimeToLocalFileTime(FileTime, LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  end;
}
var
  x: Integer;
  res: integer;
  sr: TSearchRec;
  xFile: TFileSpec;
begin
  if not DirectoryExistsUTF8(FolderName) { *Converted from DirectoryExists* } then
    raise Exception.Create('Unknown folder: ' + FolderName);
  for x := 0 to aFiles.Count - 1 do
    aFiles.Objects[x].Free;
  aFiles.Clear;

  res := sysUtils.FindFirst(appendSlash(FolderName)+'*.*',faAnyFile,sr); { *Converted from FindFirst* }
  while res = 0 do
  begin
    if not (sr.Attr and faDirectory = faDirectory) then
    begin
      xFile := TFileSpec.Create;
      xFile.Name := sr.Name;
      xFile.Size := sr.Size;
      xFile.Modified := FileAgeUtf8(sr.Name);
      aFiles.AddObject(xFile.Name, xFile);
    end;
    res := sysUtils.FindNext(sr); { *Converted from FindNext* }
  end;
  aFiles.Sort;
end;

procedure TShowFolderDifferencesForm.PopulateMain;
var
  x, a, b, c, i: Integer;
  xNode, cNode: PVirtualNode;
  xData, cData: PVSTreeRec;
  s: String;
begin
  mainVST.BeginUpdate;
  try
    mainVST.Clear;
    GetFolderFiles(FolderName1, aFiles);
    GetFolderFiles(FolderName2, bFiles);
    Diffs.Execute(aFiles, bFiles);
    a := 0; b := 0;
    for c := 0 to Diffs.ChangeCount - 1 do
    begin
      while a < Diffs.Changes[c].x do
      begin
        xNode := mainVST.AddChild(nil,nil);
        xData := mainVST.GetNodeData(xNode);
        xData.aFile := aFiles.Objects[a] as TFileSpec;
        xData.bFile := bFiles.Objects[b] as TFileSpec;
        xData.Match := True;
        CompareAB(xData);
        inc(a); inc(b);
      end;
      if Diffs.Changes[c].Kind = ckAdd then
      begin
        for i := b to b + Diffs.Changes[c].Range - 1 do
        begin
          xNode := mainVST.AddChild(nil,nil);
          xData := mainVST.GetNodeData(xNode);
          xData.bFile := bFiles.Objects[b] as TFileSpec;
          CreateB(xData);
          inc(b);
        end;
      end
      else
      begin
        if Diffs.Changes[c].Kind = ckDelete then
        begin
          for i := a to a + Diffs.Changes[c].Range - 1 do
          begin
            xNode := mainVST.AddChild(nil,nil);
            xData := mainVST.GetNodeData(xNode);
            xData.aFile := aFiles.Objects[a] as TFileSpec;
            CreateA(xData);
            inc(a);
          end;
        end
        else
        begin
          for i := a to a + Diffs.Changes[c].Range - 1 do
          begin
            xNode := mainVST.AddChild(nil,nil);
            xData := mainVST.GetNodeData(xNode);
            xData.aFile := aFiles.Objects[a] as TFileSpec;
            CreateA(xData);
            inc(a);
          end;
          for i := b to b + Diffs.Changes[c].Range - 1 do
          begin
            xNode := mainVST.AddChild(nil,nil);
            xData := mainVST.GetNodeData(xNode);
            xData.bFile := bFiles.Objects[b] as TFileSpec;
            CreateB(xData);
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aFiles.Count) and (b < bFiles.Count) do
    begin
      xNode := mainVST.AddChild(nil,nil);
      xData := mainVST.GetNodeData(xNode);
      xData.aFile := aFiles.Objects[a] as TFileSpec;
      xData.bFile := bFiles.Objects[b] as TFileSpec;
      xData.Match := True;
      CompareAB(xData);
      inc(a); inc(b);
    end;
    while (a < aFiles.Count) do
    begin
      xNode := mainVST.AddChild(nil,nil);
      xData := mainVST.GetNodeData(xNode);
      xData.aFile := aFiles.Objects[a] as TFileSpec;
      CreateA(xData);
      inc(a);
    end;
    while (b < bFiles.Count) do
    begin
      xNode := mainVST.AddChild(nil,nil);
      xData := mainVST.GetNodeData(xNode);
      xData.bFile := bFiles.Objects[a] as TFileSpec;
      CreateB(xData);
      inc(b);
    end;
    mainVST.FocusedNode := mainVST.GetFirst;
    mainVST.SetFocus;
  finally
    mainVST.EndUpdate;
  end;
end;

procedure TShowFolderDifferencesForm.mainVSTColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowFolderDifferencesForm.mainVSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xData: PVSTreeRec;
begin
    xData := mainVST.GetNodeData(Node);
    case Column of
    3:
      begin
        if Assigned (xData.aFile)
        and Assigned (xData.bFile) then
          if xData.A2B.Differs then
            ImageIndex := 133
          else
            ImageIndex := 132
        else
        begin
          if Assigned (xData.aFile) then
            ImageIndex := 139
          else
            ImageIndex := 138;
        end;
      end;
    end;
end;

procedure TShowFolderDifferencesForm.mainVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xData: PVSTreeRec;
begin
  try
    CellText := '';
    xData := Sender.GetNodeData(Node);
    case Column of
    0: if Assigned (xData.aFile) then
         CellText := xData.aFile.Name;
    1: if Assigned (xData.aFile) then
         CellText := IntToStr (xData.aFile.Size);
    2: if Assigned (xData.aFile) then
         CellText := DateTimeToStr (xData.aFile.Modified);
    4: if Assigned (xData.bFile) then
         CellText := xData.bFile.Name;
    5: if Assigned (xData.bFile) then
         CellText := IntToStr (xData.bFile.Size);
    6: if Assigned (xData.bFile) then
         CellText := DateTimeToStr(xData.bFile.Modified);
    end;
  except
    on e: Exception do
    begin
      CellText := 'raised error: '  + e.Message;
    end;
  end;

end;

procedure TShowFolderDifferencesForm.mainVSTResize(Sender: TObject);
var
  x, y, w: Integer;
  xVst: TVirtualStringTree;
begin
{
  with Sender as TVirtualStringTree do
  begin
    w := Width - 20; // scrollbar width??;
    for x := 0 to Header.Columns.Count - 1 do
      if x <> Header.MainColumn then
        w := w - Header.Columns[x].Width;
    Header.Columns [Header.MainColumn].Width := w;
  end;
}
end;

procedure TShowFolderDifferencesForm.mainVSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  xData: PVSTreeRec;
begin
end;

procedure TShowFolderDifferencesForm.setPreviewMode(const Value: Boolean);
begin
end;

procedure TShowFolderDifferencesForm.ShowDeltaButtonClick(Sender: TObject);
begin
{
  mainVSTFocusChanged(mainVST, mainVST.FocusedNode, 0);
  mainVST.Invalidate;
}
end;

function TShowFolderDifferencesForm.getShowDelta: Boolean;
begin
end;

procedure TShowFolderDifferencesForm.setShowDelta(const Value: Boolean);
begin
end;

procedure TShowFolderDifferencesForm.ShowNonVerbsButtonClick(Sender: TObject);
begin
  PopulateMain;
end;

function TShowFolderDifferencesForm.getShowNonVerbs: Boolean;
begin
end;

procedure TShowFolderDifferencesForm.setShowNonVerbs(const Value: Boolean);
begin
end;

function TShowFolderDifferencesForm.getShowExpanded: Boolean;
begin
end;

procedure TShowFolderDifferencesForm.setShowExpanded(const Value: Boolean);
begin
end;

procedure TShowFolderDifferencesForm.ToggleExpandedButtonClick(Sender: TObject);
begin
  if ShowExpanded then
    mainVST.FullExpand(nil)
  else
    mainVST.FullCollapse(nil);
end;

procedure TShowFolderDifferencesForm.mainVSTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TVirtualStringTree).FocusedNode
    := (Sender as TVirtualStringTree).GetNodeAt(X, Y);
end;

procedure TShowFolderDifferencesForm.HelpActionExecute(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath (ParamStr(0)) + '\Documentation\wsdlStubRMPreview.htm';
  if not FileExistsUTF8(xFileName) { *Converted from FileExists* } then
    raise Exception.Create ('Could not find helpfile: ' + xFileName);
  if not OpenDocument(PChar (xFileName)
               ) { *Converted from ShellExecute* <= 32} then
    raise Exception.Create ('Could not open ' + xFileName);
end;

procedure TShowFolderDifferencesForm.ToolButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TShowFolderDifferencesForm.mainVSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xData: PVSTreeRec;
begin
  xData := Sender.GetNodeData(Node);
  FreeAndNil (xData.A2B);
end;

procedure TShowFolderDifferencesForm.CompareAB(xData: PVSTreeRec);
var
  aXml, bXml: TXml;
  x, y: Integer;
  aRecursPath: String;
begin
  aXml := XmlFromFile(AppendSlash (FolderName1) + xData.aFile.Name);
  bXml := XmlFromFile(AppendSlash (FolderName2) + xData.bFile.Name);
  for x := 0 to orderGroupsOn.Count - 1 do
    with orderGroupsOn.Objects[x] as TStringList do
      for y := Count - 1 downto 0 do
      begin
        aXml.Sort(orderGroupsOn.Strings[x], Strings[y]);
        bXml.Sort(orderGroupsOn.Strings[x], Strings[y]);
      end;
  xData.A2B := TA2BXml.CreateA2B('', aXml, bXml, ignoreDifferencesOn);
  FreeAndNil (aXml);
  FreeAndNil (bXml);
end;

procedure TShowFolderDifferencesForm.mainVSTClick(Sender: TObject);
var
  xData: PVSTreeRec;
  xForm: TShowA2BXmlForm;
begin
  xData := mainVST.GetNodeData(mainVST.FocusedNode);
  case mainVST.FocusedColumn of
  3:
    begin
      if Assigned(xData.A2B)
      {and xData.reqA2B.Differs} then
      begin
        Application.CreateForm(TShowA2BXmlForm, xForm);
        try
          xForm.Caption := 'Differences in requests';
          xForm.ignoreDifferencesOn := ignoreDifferencesOn;
          xForm.ignoreRemovingOn := ignoreRemovingOn;
          xForm.ignoreOrderOn := ignoreOrderOn;
          xForm.regressionSortColumns := regressionSortColumns;
//        xForm.orderGroupsOn := orderGroupsOn;
          xForm.Xml := xData.A2B;
          xForm.ShowModal;
          if xForm.RefreshNeeded then
            FormShow(nil);
        finally
          FreeAndNil (xForm);
        end;
      end;
    end;
  end;
end;

function TShowFolderDifferencesForm.XmlFromFile (aFileName: String): TXml;
begin
  result := TXml.Create;
  try
    result.LoadFromFile(aFileName, nil);
  except
    on e: Exception do
    begin
      result.Free;
      result := TXml.CreateAsString('ErrorParsingFile', '');
      result.AddXml(TXml.CreateAsString('FileName', aFileName));
      result.AddXml(TXml.CreateAsString('Exception', e.Message));
    end;
  end;
end;

procedure TShowFolderDifferencesForm.CreateA(xData: PVSTreeRec);
var
  aXml: TXml;
begin
  aXml := XmlFromFile(AppendSlash (FolderName1) + xData.aFile.Name);
  xData.A2B := TA2BXml.CreateA ('', aXml, False);
  FreeAndNil (aXml);
end;

procedure TShowFolderDifferencesForm.CreateB(xData: PVSTreeRec);
var
  bXml: TXml;
begin
  bXml := XmlFromFile(AppendSlash (FolderName2) + xData.bFile.Name);
  xData.A2B := TA2BXml.CreateB ('', bXml, False);
  FreeAndNil (bXml);
end;

procedure TShowFolderDifferencesForm.MaintainIgnoreListActionExecute(
  Sender: TObject);
var
  x, a, f: Integer;
  Srcs, Dsts: TStringList;
begin
  Srcs := TStringList.Create;
  Srcs.Sorted := True;
  Srcs.Duplicates := dupIgnore;
  Dsts := TStringList.Create;
  Dsts.Sorted := True;
  Dsts.Duplicates := dupIgnore;
  try
    Dsts.Text := ignoreDifferencesOn.Text;
    Application.CreateForm(TdualListForm, dualListForm);
    try
      dualListForm.Caption := 'List of elements to be ignored';
      dualListForm.DstList.Items.Text := Dsts.Text;
      dualListForm.SrcList.Items.Text := '';
      dualListForm.DstCaption := 'Ignored elements';
      dualListForm.SrcCaption := '';
      duallistForm.EmptySelectionAllowed := True;
      dualListForm.ShowModal;
      if dualListForm.ModalResult = mrOk then
      begin
        ignoreDifferencesOn.Text := dualListForm.DstList.Items.Text;
        FormShow(nil);
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

procedure TShowFolderDifferencesForm.MaintainIgnoreListActionUpdate(
  Sender: TObject);
begin
  MaintainIgnoreListAction.Enabled := Assigned (ignoreDifferencesOn)
                                  and (ignoreDifferencesOn.Count > 0);
end;

procedure TShowFolderDifferencesForm.NextDiffActionUpdate(Sender: TObject);
begin
  NextDiffAction.Enabled := Assigned (mainVST.FocusedNode);
end;

procedure TShowFolderDifferencesForm.PrevDiffActionUpdate(Sender: TObject);
begin
  PrevDiffAction.Enabled := Assigned (mainVST.FocusedNode);
end;

procedure TShowFolderDifferencesForm.SearchDiff(aDown: Boolean);
var
  xNode: PVirtualNode;
  xData: PVSTreeRec;
begin
  if not Assigned (mainVST.FocusedNode) then
    Raise Exception.Create ('Only possible when a node has focus');
  if aDown then
    xNode := mainVST.GetNext(mainVST.FocusedNode)
  else
    xNode := mainVST.GetPrevious(mainVST.FocusedNode);
  xData := mainVST.GetNodeData(xNode);
  while Assigned (xNode)
  and (not xData.A2B.Differs)
  and (Assigned (xData.aFile))
  and (Assigned (xData.bFile))
  do
  begin
    if aDown then
      xNode := mainVST.GetNext(xNode)
    else
      xNode := mainVST.GetPrevious(xNode);
    if Assigned (xNode) then
      xData := mainVST.GetNodeData(xNode);
  end;
  if not Assigned (xNode) then
    raise Exception.Create('not found');
  mainVST.Selected [xNode] := True;
  mainVST.FocusedNode := xNode;
end;

procedure TShowFolderDifferencesForm.NextDiffActionExecute(Sender: TObject);
begin
  SearchDiff(True);
end;

procedure TShowFolderDifferencesForm.PrevDiffActionExecute(Sender: TObject);
begin
  SearchDiff(False);
end;

procedure TShowFolderDifferencesForm.CopyToClipboardActionExecute(Sender: TObject);
begin
  XmlUtil.PushCursor(crHourGlass);
  try
    Clipboard.AsText := vstToGrid (mainVST, CopyGridOnGetText);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TShowFolderDifferencesForm.CopyGridOnGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xData: PVSTreeRec;
begin
  xData := mainVST.GetNodeData(Node);
  try
    CellText := '';
    case Column of
    0: if Assigned (xData.aFile) then
         CellText := xData.aFile.Name;
    1: if Assigned (xData.aFile) then
         CellText := IntToStr (xData.aFile.Size);
    2: if Assigned (xData.aFile) then
         CellText := DateTimeToStr (xData.aFile.Modified);
    3: if Assigned (xData.aFile)
       and Assigned (xData.bFile) then
         if xData.A2B.Differs then
           CellText := 'X'
         else
           CellText := ''
       else
         CellText := 'X';
    4: if Assigned (xData.bFile) then
         CellText := xData.bFile.Name;
    5: if Assigned (xData.bFile) then
         CellText := IntToStr (xData.bFile.Size);
    6: if Assigned (xData.bFile) then
         CellText := DateTimeToStr(xData.bFile.Modified);
    end;
  except
    on e: Exception do
    begin
      CellText := 'raised error: '  + e.Message;
    end;
  end;
end;

procedure TShowFolderDifferencesForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TShowFolderDifferencesForm.HtmlReportActionExecute(Sender: TObject);
{}{
var
  xXml, tableXml: TXml;
  swapCursor: TCursor;
  xNode: PVirtualNode;
  xData: PVSTreeRec;
  xRow: Integer;
  xRowSpan: Integer;
  xFirst: Boolean;
  function vTop (aXml: TXml): TXml;
  begin
    result := aXml;
    aXml.AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
  end;
  procedure htmlDiffs (aPrefix: String; aA2B: TA2BXml);
  var
    x: Integer;
  begin
    if aA2B.ThisOneDiffers then
    begin
      with tableXml do
      begin
        with AddXml (TXml.CreateAsString('tr', '')) do
        begin
          if xFirst then
          begin
            with AddXml (TXml.CreateAsString('td', IntToStr (xRow))) do
            begin
              AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
              AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
            end;
            try
              with AddXml (TXml.CreateAsString('td', DateTimeToStr (xData.aFile.InboundTimeStamp))) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            except
              with AddXml (TXml.CreateAsString('td', '_')) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            end;
            if Assigned (xData.aFile.Operation) then
            begin
              with AddXml (TXml.CreateAsString('td', xData.aFile.Operation.WsdlService.Name)) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
              with AddXml (TXml.CreateAsString('td', xData.aFile.Operation.Name)) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            end
            else
            begin
              with AddXml (TXml.CreateAsString('td', '_')) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
              with AddXml (TXml.CreateAsString('td', '_')) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            end;
            if Assigned (xData.aFile.Mssg) then
            begin
              with AddXml (TXml.CreateAsString('td', xData.aFile.Mssg.Name)) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            end
            else
            begin
              with AddXml (TXml.CreateAsString('td', '_')) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
            end;
            with AddXml (TXml.CreateAsString('td', xData.aFile.CorrelationId)) do
            begin
              AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
              AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
            end;
            xFirst := False;
          end;
          AddXml (vTop (TXml.CreateAsString('td', aPrefix + aA2B.FullUQCaption + ' ')));
          case aA2B.ChangeKind of
          ckModify:
            begin
              AddXml (vTop (TXml.CreateAsString('td', aA2B.Value + '_')));
              AddXml (vTop (TXml.CreateAsString('td', aA2B.bValue + '_')));
            end;
          ckAdd:
            begin
              AddXml (vTop (TXml.CreateAsString('td', 'Missing element')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
            end;
          ckDelete:
            begin
              AddXml (vTop (TXml.CreateAsString('td', 'New element')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
            end;
          end;
        end;
      end;
    end
    else
    begin
      for x := 0 to aA2B.Items.Count - 1 do
        htmlDiffs (aPrefix, aA2B.Items.XmlItems [x] as TA2BXml);
    end;
  end;
{}
begin
{}{
  xXml := TXml.CreateAsString('html', '');
  try
    swapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      tableXml := xXml.AddXml (TXml.CreateAsString('table', ''));
      with tableXml do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('border', '1'));
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', 'wsdlStub - Differences report')) do
            AddAttribute(TXmlAttribute.CreateAsString('colspan', '5'));
          with AddXml (TXml.CreateAsString('td', DateToStr(now))) do
            AddAttribute(TXmlAttribute.CreateAsString('colspan', '4'));
        end;
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', '_')) do
             AddAttribute(TXmlAttribute.CreateAsString('colspan', '5'));
          with AddXml (TXml.CreateAsString('td', '_')) do
             AddAttribute(TXmlAttribute.CreateAsString('colspan', '4'));
        end;

        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', 'Messages with diffrences against reference')) do
             AddAttribute(TXmlAttribute.CreateAsString('colspan', '5'));
          with AddXml (TXml.CreateAsString('td', ReferenceFilename)) do
             AddAttribute(TXmlAttribute.CreateAsString('colspan', '4'));
        end;
        with AddXml (Txml.CreateAsString ('tr', '')) do
        begin
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Row '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Sent/Received '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Service '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Operation '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Message '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Correlation '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Tag '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Current Value '));
          with AddXml (TXml.CreateAsString('td', '')) do
            AddXml (TXml.CreateAsString('b', 'Reference Value '));
        end;
        xNode := mainVst.GetFirst;
        xRow := 0;
        while Assigned (xNode) do
        begin
          xData := mainVST.GetNodeData(xNode);
          if (Assigned (xData.aFile)) then
            Inc (xRow);
          if (not Assigned (xData.aFile)) then
          begin
            with AddXml (Txml.CreateAsString ('tr', '')) do
            begin
              AddXml (vTop (TXml.CreateAsInteger('td', xRow)));
              try
                AddXml (vTop (TXml.CreateAsString('td', DateTimeToStr (xData.bFile.InboundTimeStamp))));
              except
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              end;
              if Assigned (xData.bFile.Operation) then
              begin
                AddXml (vTop (TXml.CreateAsString('td', xData.bFile.Operation.WsdlService.Name)));
                AddXml (vTop (TXml.CreateAsString('td', xData.bFile.Operation.Name)));
              end
              else
              begin
                AddXml (vTop (TXml.CreateAsString('td', '_')));
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              end;
              if Assigned (xData.bFile.Mssg) then
                AddXml (vTop (TXml.CreateAsString('td', xData.bFile.Mssg.Name)))
              else
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              AddXml (vTop (TXml.CreateAsString('td', xData.bFile.CorrelationId)));
              AddXml (vTop (TXml.CreateAsString('td', 'Missing row')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
            end;
          end
          else
          begin
            if (not Assigned (xData.bFile)) then
            begin
              with AddXml (Txml.CreateAsString ('tr', '')) do
              begin
                AddXml (vTop (TXml.CreateAsInteger('td', xRow)));
                try
                  AddXml (vTop (TXml.CreateAsString('td', DateTimeToStr (xData.aFile.InboundTimeStamp))));
                except
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                end;
                if Assigned (xData.aFile.Operation) then
                begin
                  AddXml (vTop (TXml.CreateAsString('td', xData.aFile.Operation.WsdlService.Name)));
                  AddXml (vTop (TXml.CreateAsString('td', xData.aFile.Operation.Name)));
                end
                else
                begin
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                end;
                if Assigned (xData.aFile.Mssg) then
                  AddXml (vTop (TXml.CreateAsString('td', xData.aFile.Mssg.Name)))
                else
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                AddXml (vTop (TXml.CreateAsString('td', xData.aFile.CorrelationId)));
                AddXml (vTop (TXml.CreateAsString('td', 'New row')));
                AddXml (vTop (TXml.CreateAsString('td', '_')));
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              end;
            end
            else
            begin
              if (xData.reqA2B.Differs)
              or (xData.rpyA2B.Differs)
              then begin
                xRowSpan := xData.reqA2B.numberOfDiffs + xData.rpyA2B.numberOfDiffs;
                if xRowSpan < 1 then
                  xRowSpan := 1;
                xFirst := True;
                htmlDiffs ('req.', xData.reqA2B);
                htmlDiffs ('rpy.', xData.rpyA2B);
              end;
            end;
          end;
          xNode := mainVST.GetNext(xNode);
        end;
      end;
    finally
      Screen.Cursor := swapCursor;
    end;
    Application.CreateForm(TShowHtmlForm, ShowHtmlForm);
    try
      ShowHtmlForm.Caption := 'wsdlStub - Differences report';
      ShowHtmlForm.Html := xXml.asHtmlString;
      ShowHtmlForm.ShowModal;
    finally
      FreeAndNil (ShowHtmlForm);
    end;
  finally
    FreeAndNil (xXml);
  end;
{}
end;

end.
