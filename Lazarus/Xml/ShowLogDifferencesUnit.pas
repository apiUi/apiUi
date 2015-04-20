unit ShowLogDifferencesUnit;

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
  SysUtils , Classes , Graphics , Controls , Forms ,
  Dialogs , ExtCtrls , VirtualTrees , FileUtil ,
  FormIniFilez , ComCtrls , ActnList , Logz , a2bStringListUnit ,
  Xmlz , A2BXmlz;

type
  PVSTreeRec = ^TVSTreeRec;
  TVSTreeRec = record
    aLog, bLog: TLog;
    Match: Boolean;
    reqA2B, rpyA2B: TA2BXml;
  end;

type
  TShowLogDifferencesForm = class(TForm)
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
    MaintainIgnoreDiffsAction: TAction;
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
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    MaintainIgnoreAdditionsAction: TAction;
    MaintainIgnoredRemovalsAction: TAction;
    procedure HtmlReportActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure PrevDiffActionExecute(Sender: TObject);
    procedure NextDiffActionExecute(Sender: TObject);
    procedure PrevDiffActionUpdate(Sender: TObject);
    procedure NextDiffActionUpdate(Sender: TObject);
    procedure MaintainIgnoreDiffsActionUpdate(Sender: TObject);
    procedure MaintainIgnoreDiffsActionExecute(Sender: TObject);
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
    procedure MaintainIgnoreAdditionsActionUpdate(Sender: TObject);
    procedure MaintainIgnoredRemovalsActionUpdate(Sender: TObject);
    procedure MaintainIgnoreAdditionsActionExecute(Sender: TObject);
    procedure MaintainIgnoredRemovalsActionExecute(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fPreviewMode: Boolean;
    Diffs: TA2BStringList;
    function getShowExpanded: Boolean;
    procedure setShowExpanded(const Value: Boolean);
    function getShowNonVerbs: Boolean;
    procedure setShowNonVerbs(const Value: Boolean);
    function getShowDelta: Boolean;
    procedure setShowDelta(const Value: Boolean);
    procedure setPreviewMode(const Value: Boolean);
    procedure PopulateMain;
    procedure MaintainList (aCaptian: String; aList: TStringList);
    procedure CreateA (xData: PVSTreeRec);
    procedure CreateB (xData: PVSTreeRec);
    procedure CompareAB (xData: PVSTreeRec);
    procedure SearchDiff (aDown: Boolean);
    procedure CopyGridOnGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  public
    ignoreDifferencesOn, ignoreAddingon, ignoreRemovingOn: TStringList;
    aLogs: TLogList;
    bLogs: TLogList;
    ReferenceFileName: String;
//    PrintPreview: TNotifyEvent;
    property ShowExpanded: Boolean read getShowExpanded write setShowExpanded;
    property ShowDelta: Boolean read getShowDelta write setShowDelta;
    property ShowNonVerbs: Boolean read getShowNonVerbs write setShowNonVerbs;
    property PreviewMode: Boolean read fPreviewMode write setPreviewMode;
  end;

var
  ShowLogDifferencesForm: TShowLogDifferencesForm;

implementation

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
{$ENDIF}
  Bind, ShowA2BXmlUnit, dualListUnit, igGlobals, ClipBrd, vstUtils, XmlXsdParser;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TShowLogDifferencesForm.FormCreate(Sender: TObject);
var
  w5: Integer;
begin
  w5 := mainVST.Header.Columns.Items[5].Width;
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  mainVST.Header.Columns.Items[5].Width := w5;
  mainVST.Header.Columns.Items[6].Width := w5;
  mainVST.NodeDataSize := SizeOf (TVSTreeRec);
{
  ShowExpanded := IniFile.ReadBool('rmPreview', 'ShowExpanded', True);
  ShowDelta := IniFile.ReadBool('rmPreview', 'ShowDelta', True);
  ShowNonVerbs := IniFile.ReadBool('rmPreview', 'ShowNonVerbs', True);
}
  Diffs := TA2BStringList.Create;
  CloseAction.ShortCut := VK_ESCAPE;
end;

procedure TShowLogDifferencesForm.FormDestroy(Sender: TObject);
begin
{
  IniFile.WriteBool('rmPreview', 'ShowExpanded', ShowExpanded);
  IniFile.WriteBool('rmPreview', 'ShowDelta', ShowDelta);
  IniFile.WriteBool('rmPreview', 'ShowNonVerbs', ShowNonVerbs);
}
  IniFile.Save;
  IniFile.Free;
  mainVST.Clear;
  Diffs.Free;
end;

procedure TShowLogDifferencesForm.FormShow(Sender: TObject);
begin
  PopulateMain;
  Screen.Cursor:=crDefault;
end;

procedure TShowLogDifferencesForm.PopulateMain;
var
  x, a, b, c, i: Integer;
  xNode: PVirtualNode;
  xData: PVSTreeRec;
  LA, LB: TStringList;
  s: String;
begin
  a2bInitialize;
  try
    mainVST.BeginUpdate;
    try
      mainVST.Clear;
      LA := TStringList.Create;
      LB := TStringList.Create;
      try
        for x := 0 to aLogs.Count - 1 do
        begin
          s := ';;';
          with aLogs.LogItems[x] do
          begin
            if Assigned (Operation) then
            begin
              s := Operation.WsdlService.Name + ';' + Operation.Name + ';';
            end;
            s := s + ';' + CorrelationId;
          end;
          LA.Add(s);
        end;
        for x := 0 to bLogs.Count - 1 do
        begin
          s := ';;';
          with bLogs.LogItems[x] do
          begin
            if Assigned (Operation) then
            begin
              s := Operation.WsdlService.Name + ';' + Operation.Name + ';';
            end;
            s := s + ';' + CorrelationId;
          end;
          LB.Add(s);
        end;
        Diffs.Execute(LA, LB);
        a := 0; b := 0;
        for c := 0 to Diffs.ChangeCount - 1 do
        begin
          while a < Diffs.Changes[c].x do
          begin
            xNode := mainVST.AddChild(nil,nil);
            xData := mainVST.GetNodeData(xNode);
            xData.aLog := aLogs.LogItems[a];
            xData.bLog := bLogs.LogItems[b];
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
              xData.bLog := bLogs.LogItems[b];
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
                xData.aLog := aLogs.LogItems[a];
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
                xData.aLog := aLogs.LogItems[a];
                CreateA(xData);
                inc(a);
              end;
              for i := b to b + Diffs.Changes[c].Range - 1 do
              begin
                xNode := mainVST.AddChild(nil,nil);
                xData := mainVST.GetNodeData(xNode);
                xData.bLog := bLogs.LogItems[b];
                CreateB(xData);
                inc(b);
              end;
            end;
          end;
        end;
        while (a < aLogs.Count) and (b < bLogs.Count) do
        begin
          xNode := mainVST.AddChild(nil,nil);
          xData := mainVST.GetNodeData(xNode);
          xData.aLog := aLogs.LogItems[a];
          xData.bLog := bLogs.LogItems[b];
          xData.Match := True;
          CompareAB(xData);
          inc(a); inc(b);
        end;
        while (a < aLogs.Count) do
        begin
          xNode := mainVST.AddChild(nil,nil);
          xData := mainVST.GetNodeData(xNode);
          xData.aLog := aLogs.LogItems[a];
          CreateA(xData);
          inc(a);
        end;
        while (b < bLogs.Count) do
        begin
          xNode := mainVST.AddChild(nil,nil);
          xData := mainVST.GetNodeData(xNode);
          xData.bLog := bLogs.LogItems[a];
          CreateB(xData);
          inc(b);
        end;
        mainVST.FocusedNode := mainVST.GetFirst;
        mainVST.SetFocus;
      finally
        FreeAndNil (LA);
        FreeAndNil (LB);
      end;
    finally
      mainVST.EndUpdate;
    end;
  finally
    a2bUninitialize;
  end;
end;

procedure TShowLogDifferencesForm.mainVSTColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TShowLogDifferencesForm.mainVSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xData: PVSTreeRec;
begin
    xData := mainVST.GetNodeData(Node);
    case Column of
    5:
      begin
        if Assigned (xData.aLog)
        and Assigned (xData.bLog) then
          if Assigned (xData.reqA2B)
          and xData.reqA2B.Differs then
          begin
            ImageIndex := 133;
            if xData.reqA2B.Ignored then
              ImageIndex := 140;
          end
          else
            ImageIndex := 132
        else
        begin
          if Assigned (xData.Alog) then
            ImageIndex := 139
          else
            ImageIndex := 138;
        end;
      end;
    6:
      begin
        if Assigned (xData.aLog)
        and Assigned (xData.bLog) then
          if Assigned (xData.rpyA2B)
          and xData.rpyA2B.Differs then
          begin
            ImageIndex := 133;
            if xData.rpyA2B.Ignored then
              ImageIndex := 140;
          end
          else
            ImageIndex := 132
        else
        begin
          if Assigned (xData.Alog) then
            ImageIndex := 139
          else
            ImageIndex := 138;
        end;
      end;
    end;
end;

procedure TShowLogDifferencesForm.mainVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xData: PVSTreeRec;
begin
  try
    CellText := '';
    xData := Sender.GetNodeData(Node);
    case Column of
    0: if Assigned (xData.aLog) then
         CellText := xsdFormatDateTime (xData.aLog.InboundTimeStamp, @TIMEZONE_UTC);
    1: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.WsdlService.Name;
    2: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.Name;
    3: if Assigned (xData.aLog) and Assigned (xData.aLog.Mssg) then
         CellText := xData.aLog.Mssg.Name;
    4: if Assigned (xData.aLog) then
         CellText := xData.aLog.CorrelationId;

    7: if Assigned (xData.bLog) then
         CellText := xsdFormatDateTime (xData.bLog.InboundTimeStamp, @TIMEZONE_UTC);
    8: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.WsdlService.Name;
    9: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.Name;
   10: if Assigned (xData.bLog) and Assigned (xData.bLog.Mssg) then
         CellText := xData.bLog.Mssg.Name;
   11: if Assigned (xData.bLog) then
         CellText := xData.bLog.CorrelationId;
    end;
  except
    on e: Exception do
    begin
      CellText := 'raised error: '  + e.Message;
    end;
  end;

end;

procedure TShowLogDifferencesForm.mainVSTResize(Sender: TObject);
begin
end;

procedure TShowLogDifferencesForm.mainVSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
end;

procedure TShowLogDifferencesForm.setPreviewMode(const Value: Boolean);
begin
end;

procedure TShowLogDifferencesForm.ShowDeltaButtonClick(Sender: TObject);
begin
{
  mainVSTFocusChanged(mainVST, mainVST.FocusedNode, 0);
  mainVST.Invalidate;
}
end;

function TShowLogDifferencesForm.getShowDelta: Boolean;
begin
end;

procedure TShowLogDifferencesForm.setShowDelta(const Value: Boolean);
begin
end;

procedure TShowLogDifferencesForm.ShowNonVerbsButtonClick(Sender: TObject);
begin
  PopulateMain;
end;

function TShowLogDifferencesForm.getShowNonVerbs: Boolean;
begin
end;

procedure TShowLogDifferencesForm.setShowNonVerbs(const Value: Boolean);
begin
end;

function TShowLogDifferencesForm.getShowExpanded: Boolean;
begin
end;

procedure TShowLogDifferencesForm.setShowExpanded(const Value: Boolean);
begin
end;

procedure TShowLogDifferencesForm.ToggleExpandedButtonClick(Sender: TObject);
begin
  if ShowExpanded then
    mainVST.FullExpand(nil)
  else
    mainVST.FullCollapse(nil);
end;

procedure TShowLogDifferencesForm.mainVSTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TVirtualStringTree).FocusedNode
    := (Sender as TVirtualStringTree).GetNodeAt(X, Y);
end;

procedure TShowLogDifferencesForm.HelpActionExecute(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath (ParamStr(0)) + '\Documentation\wsdlStubRMPreview.htm';
  if not FileExistsUTF8(xFileName) { *Converted from FileExists* } then
    raise Exception.Create ('Could not find helpfile: ' + xFileName);
  if not OpenDocument(xFileName) then
    raise Exception.Create ('Could not open ' + xFileName);
end;

procedure TShowLogDifferencesForm.ToolButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TShowLogDifferencesForm.mainVSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xData: PVSTreeRec;
begin
  xData := Sender.GetNodeData(Node);
  FreeAndNil (xData.reqA2B);
  FreeAndNil (xData.rpyA2B);
end;

procedure TShowLogDifferencesForm.CompareAB(xData: PVSTreeRec);
var
  aXml, bXml: TXml;
  xPrefix: String;
begin
  aXml := xData.aLog.reqBodyAsXml;
  bXml := xData.bLog.reqBodyAsXml;
  xData.reqA2B := TA2BXml.CreateA2B(xData.aLog.OperationName, aXml, bXml, False);
  xData.reqA2B.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
  FreeAndNil (aXml);
  FreeAndNil (bXml);
  aXml := xData.aLog.rpyBodyAsXml;
  bXml := xData.bLog.rpyBodyAsXml;
  xData.rpyA2B := TA2BXml.CreateA2B(xData.aLog.OperationName, aXml, bXml, False);
  xData.rpyA2B.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
  FreeAndNil (aXml);
  FreeAndNil (bXml);
end;

procedure TShowLogDifferencesForm.mainVSTClick(Sender: TObject);
var
  xData: PVSTreeRec;
begin
  case mainVST.FocusedColumn of
  5:
    begin
      xData := mainVST.GetNodeData(mainVST.FocusedNode);
      if Assigned(xData.reqA2B)
      {and xData.reqA2B.Differs} then
      begin
        Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
        try
          ShowA2BXmlForm.Caption := 'Differences in requests';
          ShowA2BXmlForm.ignoreDifferencesOn := ignoreDifferencesOn;
          ShowA2BXmlForm.ignoreAddingOn := ignoreAddingon;
          ShowA2BXmlForm.ignoreRemovingOn := ignoreRemovingOn;
          ShowA2BXmlForm.Xml := xData.reqA2B;
          ShowA2BXmlForm.ShowModal;
          if ShowA2BXmlForm.RefreshNeeded then
            FormShow(nil);
        finally
          FreeAndNil (ShowA2BXmlForm);
        end;
      end;
    end;
  6:
    begin
      xData := mainVST.GetNodeData(mainVST.FocusedNode);
      if Assigned (xData.rpyA2B)
      {and xData.rpyA2B.Differs} then
      begin
        Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
        try
          ShowA2BXmlForm.Caption := 'Differences in replies';
          ShowA2BXmlForm.ignoreDifferencesOn := ignoreDifferencesOn;
          ShowA2BXmlForm.ignoreAddingOn := ignoreAddingon;
          ShowA2BXmlForm.ignoreRemovingOn := ignoreRemovingOn;
          ShowA2BXmlForm.Xml := xData.rpyA2B;
          ShowA2BXmlForm.ShowModal;
          if ShowA2BXmlForm.RefreshNeeded then
            FormShow(nil);
        finally
          FreeAndNil (ShowA2BXmlForm);
        end;
      end;
    end;
  end;
end;

procedure TShowLogDifferencesForm.CreateA(xData: PVSTreeRec);
var
  aXml: TXml;
begin
  aXml := xData.aLog.reqBodyAsXml;
  xData.reqA2B := TA2BXml.CreateA (xData.aLog.OperationName, aXml, True);
  FreeAndNil (aXml);
  aXml := xData.aLog.rpyBodyAsXml;
  xData.rpyA2B := TA2BXml.CreateA(xData.aLog.OperationName, aXml, True);
  FreeAndNil (aXml);
end;

procedure TShowLogDifferencesForm.CreateB(xData: PVSTreeRec);
var
  bXml: TXml;
begin
  bXml := xData.bLog.reqBodyAsXml;
  xData.reqA2B := TA2BXml.CreateB (xData.bLog.OperationName, bXml, True);
  FreeAndNil (bXml);
  bXml := xData.bLog.rpyBodyAsXml;
  xData.rpyA2B := TA2BXml.CreateB (xData.bLog.OperationName, bXml, True);
  FreeAndNil (bXml);
end;

procedure TShowLogDifferencesForm.MaintainIgnoreAdditionsActionExecute(
  Sender: TObject);
begin
  MaintainList(MaintainIgnoreAdditionsAction.Caption, ignoreAddingon);
end;

procedure TShowLogDifferencesForm.MaintainIgnoreAdditionsActionUpdate(
  Sender: TObject);
begin
  MaintainIgnoreAdditionsAction.Enabled := Assigned (ignoreAddingon)
                                  and (ignoreAddingon.Count > 0);
end;

procedure TShowLogDifferencesForm.MaintainIgnoreDiffsActionExecute(
  Sender: TObject);
begin
  MaintainList(MaintainIgnoreDiffsAction.Caption, ignoreDifferencesOn);
end;

procedure TShowLogDifferencesForm.MaintainIgnoreDiffsActionUpdate(
  Sender: TObject);
begin
  MaintainIgnoreDiffsAction.Enabled := Assigned (ignoreDifferencesOn)
                                  and (ignoreDifferencesOn.Count > 0);
end;

procedure TShowLogDifferencesForm.MaintainIgnoredRemovalsActionExecute(
  Sender: TObject);
begin
  MaintainList(MaintainIgnoredRemovalsAction.Caption, ignoreRemovingOn);
end;

procedure TShowLogDifferencesForm.MaintainIgnoredRemovalsActionUpdate(
  Sender: TObject);
begin
  MaintainIgnoredRemovalsAction.Enabled := Assigned (ignoreRemovingOn)
                                  and (ignoreRemovingOn.Count > 0);
end;

procedure TShowLogDifferencesForm.MaintainList(aCaptian: String; aList: TStringList);
var
  Srcs, Dsts: TStringList;
begin
  Srcs := TStringList.Create;
  Srcs.Sorted := True;
  Srcs.Duplicates := dupIgnore;
  Dsts := TStringList.Create;
  Dsts.Sorted := True;
  Dsts.Duplicates := dupIgnore;
  try
    Dsts.Text := aList.Text;
    Application.CreateForm(TdualListForm, dualListForm);
    try
      dualListForm.Caption := aCaptian;
      dualListForm.DstList.Items.Text := Dsts.Text;
      dualListForm.SrcList.Items.Text := '';
      dualListForm.DstCaption := 'Ignored elements';
      dualListForm.SrcCaption := '';
      duallistForm.EmptySelectionAllowed := True;
      dualListForm.ShowModal;
      if dualListForm.ModalResult = mrOk then
      begin
        aList.Text := dualListForm.DstList.Items.Text;
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

procedure TShowLogDifferencesForm.NextDiffActionUpdate(Sender: TObject);
begin
  NextDiffAction.Enabled := Assigned (mainVST.FocusedNode);
end;

procedure TShowLogDifferencesForm.PrevDiffActionUpdate(Sender: TObject);
begin
  PrevDiffAction.Enabled := Assigned (mainVST.FocusedNode);
end;

procedure TShowLogDifferencesForm.SearchDiff(aDown: Boolean);
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
  and (not xData.reqA2B.Differs)
  and (not xData.rpyA2B.Differs)
  and (Assigned (xData.aLog))
  and (Assigned (xData.bLog))
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

procedure TShowLogDifferencesForm.NextDiffActionExecute(Sender: TObject);
begin
  SearchDiff(True);
end;

procedure TShowLogDifferencesForm.PrevDiffActionExecute(Sender: TObject);
begin
  SearchDiff(False);
end;

procedure TShowLogDifferencesForm.CopyToClipboardActionExecute(Sender: TObject);
var
  swapCursor: TCursor;
begin
  swapCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Clipboard.AsText := vstToGrid (mainVST, CopyGridOnGetText);
  finally
    Screen.Cursor := swapCursor;
  end;
end;

procedure TShowLogDifferencesForm.CopyGridOnGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xData: PVSTreeRec;
begin
  xData := mainVST.GetNodeData(Node);
  try
    CellText := '';
    case Column of
    0: if Assigned (xData.aLog) then
         CellText := DateTimeToStr (xData.aLog.InboundTimeStamp);
    1: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.WsdlService.Name;
    2: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.Name;
    3: if Assigned (xData.aLog) and Assigned (xData.aLog.Mssg) then
         CellText := xData.aLog.Mssg.Name;
    4: if Assigned (xData.aLog) then
         CellText := xData.aLog.CorrelationId;
    5: if Assigned (xData.aLog)
       and Assigned (xData.bLog) then
         if xData.reqA2B.Differs then
           CellText := 'X'
         else
           CellText := ''
       else
         CellText := 'X';
    6: if Assigned (xData.aLog)
       and Assigned (xData.bLog) then
         if xData.rpyA2B.Differs then
           CellText := 'X'
         else
           CellText := ''
       else
         CellText := 'X';
    7: if Assigned (xData.bLog) then
         CellText := DateTimeToStr (xData.bLog.InboundTimeStamp);
    8: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.WsdlService.Name;
    9: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.Name;
   10: if Assigned (xData.bLog) and Assigned (xData.bLog.Mssg) then
         CellText := xData.bLog.Mssg.Name;
   11: if Assigned (xData.bLog) then
         CellText := xData.bLog.CorrelationId;
    end;
  except
    on e: Exception do
    begin
      CellText := 'raised error: '  + e.Message;
    end;
  end;
end;

procedure TShowLogDifferencesForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TShowLogDifferencesForm.HtmlReportActionExecute(Sender: TObject);
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
              with AddXml (TXml.CreateAsString('td', DateTimeToStr (xData.aLog.InboundTimeStamp))) do
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
            if Assigned (xData.aLog.Operation) then
            begin
              with AddXml (TXml.CreateAsString('td', xData.aLog.Operation.WsdlService.Name)) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (xRowSpan)));
                AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
              end;
              with AddXml (TXml.CreateAsString('td', xData.aLog.Operation.Name)) do
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
            if Assigned (xData.aLog.Mssg) then
            begin
              with AddXml (TXml.CreateAsString('td', xData.aLog.Mssg.Name)) do
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
            with AddXml (TXml.CreateAsString('td', xData.aLog.CorrelationId)) do
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
begin
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
{}
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
          if (Assigned (xData.aLog)) then
            Inc (xRow);
          if (not Assigned (xData.aLog)) then
          begin
            with AddXml (Txml.CreateAsString ('tr', '')) do
            begin
              AddXml (vTop (TXml.CreateAsInteger('td', xRow)));
              try
                AddXml (vTop (TXml.CreateAsString('td', DateTimeToStr (xData.bLog.InboundTimeStamp))));
              except
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              end;
              if Assigned (xData.bLog.Operation) then
              begin
                AddXml (vTop (TXml.CreateAsString('td', xData.bLog.Operation.WsdlService.Name)));
                AddXml (vTop (TXml.CreateAsString('td', xData.bLog.Operation.Name)));
              end
              else
              begin
                AddXml (vTop (TXml.CreateAsString('td', '_')));
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              end;
              if Assigned (xData.bLog.Mssg) then
                AddXml (vTop (TXml.CreateAsString('td', xData.bLog.Mssg.Name)))
              else
                AddXml (vTop (TXml.CreateAsString('td', '_')));
              AddXml (vTop (TXml.CreateAsString('td', xData.bLog.CorrelationId)));
              AddXml (vTop (TXml.CreateAsString('td', 'Missing row')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
              AddXml (vTop (TXml.CreateAsString('td', '_')));
            end;
          end
          else
          begin
            if (not Assigned (xData.bLog)) then
            begin
              with AddXml (Txml.CreateAsString ('tr', '')) do
              begin
                AddXml (vTop (TXml.CreateAsInteger('td', xRow)));
                try
                  AddXml (vTop (TXml.CreateAsString('td', DateTimeToStr (xData.aLog.InboundTimeStamp))));
                except
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                end;
                if Assigned (xData.aLog.Operation) then
                begin
                  AddXml (vTop (TXml.CreateAsString('td', xData.aLog.Operation.WsdlService.Name)));
                  AddXml (vTop (TXml.CreateAsString('td', xData.aLog.Operation.Name)));
                end
                else
                begin
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                end;
                if Assigned (xData.aLog.Mssg) then
                  AddXml (vTop (TXml.CreateAsString('td', xData.aLog.Mssg.Name)))
                else
                  AddXml (vTop (TXml.CreateAsString('td', '_')));
                AddXml (vTop (TXml.CreateAsString('td', xData.aLog.CorrelationId)));
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
{ TODO : ShowHTML }
{
    Application.CreateForm(TShowHtmlForm, ShowHtmlForm);
    try
      ShowHtmlForm.Caption := 'wsdlStub - Differences report';
      ShowHtmlForm.Html := xXml.asHtmlString;
      ShowHtmlForm.ShowModal;
    finally
      FreeAndNil (ShowHtmlForm);
    end;
    }
  finally
    FreeAndNil (xXml);
  end;
end;

end.
