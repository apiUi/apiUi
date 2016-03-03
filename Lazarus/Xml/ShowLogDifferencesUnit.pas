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
  SysUtils , Classes , Graphics , Controls , Forms , Dialogs,
  ExtCtrls , VirtualTrees , FileUtil ,
  FormIniFilez , ComCtrls , ActnList , StdCtrls , Logz , a2bStringListUnit ,
  Xmlz , A2BXmlz;

type
  PVSTreeRec = ^TVSTreeRec;
  TVSTreeRec = record
    aLog, bLog: TLog;
    Match: Boolean;
    reqA2B, rpyA2B: TA2BXml;
  end;

type

  { TShowLogDifferencesForm }

  TShowLogDifferencesForm = class(TForm)
    CompareLogOrderByComboBox : TComboBox ;
    MaintainLogOrderColumnsAction : TAction ;
    MaintainIgnoredOrderAction : TAction ;
    Panel1: TPanel;
    Panel2 : TPanel ;
    ToolBar1: TToolBar;
    leftPanel: TPanel;
    mainVST: TVirtualStringTree;
    ActionImageList: TImageList;
    ActionList1: TActionList;
    ToolButton14 : TToolButton ;
    ToolButton15 : TToolButton ;
    ToolButton16 : TToolButton ;
    TotalResultButton : TToolButton ;
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
    procedure CompareLogOrderByComboBoxChange (Sender : TObject );
    procedure HtmlReportActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure MaintainIgnoredOrderActionExecute (Sender : TObject );
    procedure MaintainIgnoredOrderActionUpdate (Sender : TObject );
    procedure MaintainLogOrderColumnsActionExecute (Sender : TObject );
    procedure MaintainLogOrderColumnsActionUpdate (Sender : TObject );
    procedure mainVSTChange (Sender : TBaseVirtualTree ; Node : PVirtualNode );
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
    Diffs: TA2BStringList;
    fCompareLogOrderBy : TCompareLogOrderBy ;
    fConfigChanged : Boolean ;
    fDiffsFound : Boolean ;
    procedure PopulateMain (aChanged: Boolean);
    procedure MaintainList (aCaptian: String; aList: TStringList; aDoOrder: Boolean);
    procedure CreateA (xData: PVSTreeRec);
    procedure CreateB (xData: PVSTreeRec);
    procedure CompareAB (xData: PVSTreeRec);
    procedure SearchDiff (aDown: Boolean);
    procedure CopyGridOnGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure setCompareLogOrderBy (AValue : TCompareLogOrderBy );
    procedure onSlChanged (aObject: TObject);
  public
    ignoreDifferencesOn, ignoreAddingon, ignoreRemovingOn, ignoreOrderOn, regressionSortColumns: TStringList;
    aLogs: TLogList;
    bLogs: TLogList;
    ReferenceFileName: String;
    property compareLogOrderBy: TCompareLogOrderBy read fCompareLogOrderBy write setCompareLogOrderBy;
    property differencesFound: Boolean read fDiffsFound;
    property configChanged: Boolean read fConfigChanged;
  end;

var
  ShowLogDifferencesForm: TShowLogDifferencesForm;

implementation

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
{$ENDIF}
  ShowA2BXmlUnit, dualListUnit, igGlobals, ClipBrd, vstUtils, XmlXsdParser, xmlUtilz;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type ceColumnEnum =
( ceTimeColumn
, ceServiceColumn
, ceOperationColumn
, ceMessageColumn
, ceCorrelationColumn
, ceReqColumn
, ceRpyColumn
, ceRefTimeColumn
, ceRefServiceColumn
, ceRefOperationColumn
, ceRefMessageColumn
, ceRefCorrelationColumn
);

procedure TShowLogDifferencesForm.FormCreate(Sender: TObject);
  function _stringList (aSorted: Boolean): TStringList;
  begin
    result := TStringList.Create;
    Result.Sorted := aSorted;
    Result.Duplicates := dupIgnore;
    Result.OnChange := onSlChanged;
  end;

var
  w5: Integer;
begin
  ignoreDifferencesOn := _stringList(true);
  ignoreRemovingOn := _stringList(true);
  ignoreOrderOn := _stringList(true);
  ignoreAddingon := _stringList(true);
  regressionSortColumns := _stringList(false);
  w5 := mainVST.Header.Columns.Items[Ord(ceReqColumn)].Width;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
  mainVST.Header.Columns.Items[Ord(ceReqColumn)].Width := w5;
  mainVST.Header.Columns.Items[Ord(ceRpyColumn)].Width := w5;
  mainVST.NodeDataSize := SizeOf (TVSTreeRec);
  Diffs := TA2BStringList.Create;
  CloseAction.ShortCut := VK_ESCAPE;
end;

procedure TShowLogDifferencesForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
  mainVST.Clear;
  Diffs.Free;
  FreeAndNil (ignoreDifferencesOn);
  FreeAndNil (ignoreRemovingOn);
  FreeAndNil (ignoreOrderOn);
  FreeAndNil (ignoreAddingon);
  FreeAndNil (regressionSortColumns);
end;

procedure TShowLogDifferencesForm.FormShow(Sender: TObject);
begin
  PopulateMain (False);
  Screen.Cursor := crDefault;
end;

procedure TShowLogDifferencesForm.PopulateMain (aChanged: Boolean);
var
  a, b, c, i, x: Integer;
  xNode: PVirtualNode;
  xData: PVSTreeRec;
begin
  fConfigChanged := aChanged;
  Screen.Cursor := crHourGlass;
  TotalResultButton.ImageIndex := 131;
  fDiffsFound := False;
  Application.ProcessMessages;
  try
    aLogs.Sorted := False;
    aLogs.Duplicates := dupAccept;
    for x := 0 to aLogs.Count - 1 do
      aLogs.Strings[x] := aLogs.LogItems[x].CompareKey(compareLogOrderBy, regressionSortColumns);
    aLogs.CustomSort(logz.doOrder);
    bLogs.Sorted := False;
    bLogs.Duplicates := dupAccept;
    for x := 0 to bLogs.Count - 1 do
      bLogs.Strings[x] := bLogs.LogItems[x].CompareKey(compareLogOrderBy, regressionSortColumns);
    bLogs.CustomSort(logz.doOrder);
    a2bInitialize;
    try
      mainVST.BeginUpdate;
      try
        mainVST.Clear;
        Diffs.Execute(aLogs, bLogs);
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
        mainVST.EndUpdate;
      end;
    finally
      a2bUninitialize;
    end;
  finally
    if fDiffsFound then
      TotalResultButton.ImageIndex := 133
    else
      TotalResultButton.ImageIndex := 132;
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
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
    case ceColumnEnum(Column) of
    ceReqColumn:
      begin
        if Assigned (xData.aLog)
        and Assigned (xData.bLog) then
          if Assigned (xData.reqA2B)
          and xData.reqA2B.Differs
          and (not xData.reqA2B.Ignored) then
          begin
            ImageIndex := 133;
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
    ceRpyColumn:
      begin
        if Assigned (xData.aLog)
        and Assigned (xData.bLog) then
          if Assigned (xData.rpyA2B)
          and xData.rpyA2B.Differs
          and (not xData.rpyA2B.Ignored) then
          begin
            ImageIndex := 133;
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
    case ceColumnEnum(Column) of
    ceTimeColumn:
      if Assigned (xData.aLog) then
        CellText := xsdFormatDateTime (xData.aLog.InboundTimeStamp, @TIMEZONE_UTC);
    ceServiceColumn:
      if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
        CellText := xData.aLog.Operation.WsdlService.Name;
    ceOperationColumn:
      if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
        CellText := xData.aLog.Operation.Name;
    ceMessageColumn:
      if Assigned (xData.aLog) and Assigned (xData.aLog.Mssg) then
        CellText := xData.aLog.Mssg.Name;
    ceCorrelationColumn:
      if Assigned (xData.aLog) then
        CellText := xData.aLog.CorrelationId;
    ceRefTimeColumn:
      if Assigned (xData.bLog) then
        CellText := xsdFormatDateTime (xData.bLog.InboundTimeStamp, @TIMEZONE_UTC);
    ceRefServiceColumn:
      if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
        CellText := xData.bLog.Operation.WsdlService.Name;
    ceRefOperationColumn:
      if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
        CellText := xData.bLog.Operation.Name;
   ceRefMessageColumn:
     if Assigned (xData.bLog) and Assigned (xData.bLog.Mssg) then
       CellText := xData.bLog.Mssg.Name;
   ceRefCorrelationColumn:
     if Assigned (xData.bLog) then
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
begin
  aXml := xData.aLog.reqBodyAsXml;
  aXml.SeparateNsPrefixes;
  aXml.ResolveNameSpaces;
  bXml := xData.bLog.reqBodyAsXml;
  bXml.SeparateNsPrefixes;
  bXml.ResolveNameSpaces;
  xData.reqA2B := TA2BXml.CreateA2B(xData.aLog.OperationName, aXml, bXml, ignoreOrderOn);
  xData.reqA2B.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
  FreeAndNil (aXml);
  FreeAndNil (bXml);
  aXml := xData.aLog.rpyBodyAsXml;
  aXml.SeparateNsPrefixes;
  aXml.ResolveNameSpaces;
  bXml := xData.bLog.rpyBodyAsXml;
  bXml.SeparateNsPrefixes;
  bXml.ResolveNameSpaces;
  xData.rpyA2B := TA2BXml.CreateA2B(xData.aLog.OperationName, aXml, bXml, ignoreOrderOn);
  xData.rpyA2B.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
  FreeAndNil (aXml);
  FreeAndNil (bXml);
  fDiffsFound := fDiffsFound
              or (xData.reqA2B.Differs and (not xData.reqA2B.Ignored))
              or (xData.rpyA2B.Differs and (not xData.rpyA2B.Ignored))
               ;
end;

procedure TShowLogDifferencesForm.mainVSTClick(Sender: TObject);
var
  xData: PVSTreeRec;
begin
  case ceColumnEnum(mainVST.FocusedColumn) of
  ceReqColumn:
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
          ShowA2BXmlForm.ignoreOrderOn := ignoreOrderOn;
          ShowA2BXmlForm.regressionSortColumns := regressionSortColumns;
          ShowA2BXmlForm.Xml := xData.reqA2B;
          ShowA2BXmlForm.ShowModal;
          if ShowA2BXmlForm.RefreshNeeded then
            PopulateMain (True);
        finally
          FreeAndNil (ShowA2BXmlForm);
        end;
      end;
    end;
  ceRpyColumn:
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
          ShowA2BXmlForm.ignoreOrderOn := ignoreOrderOn;
          ShowA2BXmlForm.regressionSortColumns := regressionSortColumns;
          ShowA2BXmlForm.Xml := xData.rpyA2B;
          ShowA2BXmlForm.ShowModal;
          if ShowA2BXmlForm.RefreshNeeded then
            PopulateMain(True);
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
  fDiffsFound := True;
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
  fDiffsFound := True;
end;

procedure TShowLogDifferencesForm.MaintainIgnoreAdditionsActionExecute(
  Sender: TObject);
begin
  MaintainList(MaintainIgnoreAdditionsAction.Caption, ignoreAddingon, False);
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
  MaintainList(MaintainIgnoreDiffsAction.Caption, ignoreDifferencesOn, False);
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
  MaintainList(MaintainIgnoredRemovalsAction.Caption, ignoreRemovingOn, False);
end;

procedure TShowLogDifferencesForm.MaintainIgnoredRemovalsActionUpdate(
  Sender: TObject);
begin
  MaintainIgnoredRemovalsAction.Enabled := Assigned (ignoreRemovingOn)
                                  and (ignoreRemovingOn.Count > 0);
end;

procedure TShowLogDifferencesForm.MaintainList(aCaptian: String; aList: TStringList; aDoOrder: Boolean);
var
  Srcs, Dsts: TStringList;
begin
  Srcs := TStringList.Create;
  Srcs.Sorted := not aDoOrder;
  Srcs.Duplicates := dupIgnore;
  Dsts := TStringList.Create;
  Dsts.Sorted := not aDoOrder;
  Dsts.Duplicates := dupIgnore;
  try
    Dsts.Text := aList.Text;
    Application.CreateForm(TdualListForm, dualListForm);
    try
      dualListForm.Caption := aCaptian;
      dualListForm.DstList.Items.Text := Dsts.Text;
      dualListForm.SrcList.Items.Text := '';
      dualListForm.DstCaption := 'Selected elements';
      dualListForm.SrcCaption := '';
      duallistForm.EmptySelectionAllowed := True;
      dualListForm.doMaintainOrder := aDoOrder;
      dualListForm.ShowModal;
      if dualListForm.ModalResult = mrOk then
      begin
        aList.Text := dualListForm.DstList.Items.Text;
        PopulateMain(True);
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

procedure TShowLogDifferencesForm .MaintainIgnoredOrderActionExecute (
  Sender : TObject );
begin
  MaintainList(MaintainIgnoredOrderAction.Caption, ignoreOrderOn, False);
end;

procedure TShowLogDifferencesForm .MaintainIgnoredOrderActionUpdate (Sender : TObject
  );
begin
  MaintainIgnoredOrderAction.Enabled := Assigned (ignoreOrderOn)
                                  and (ignoreOrderOn.Count > 0);
end;

procedure TShowLogDifferencesForm .MaintainLogOrderColumnsActionExecute (
  Sender : TObject );
begin
  MaintainList(MaintainLogOrderColumnsAction.Caption, regressionSortColumns, True);
end;

procedure TShowLogDifferencesForm .MaintainLogOrderColumnsActionUpdate (
  Sender : TObject );
begin
  MaintainLogOrderColumnsAction.Enabled := Assigned (regressionSortColumns)
                                       and (regressionSortColumns.Count > 0);
end;

procedure TShowLogDifferencesForm .mainVSTChange (Sender : TBaseVirtualTree ;
  Node : PVirtualNode );
begin

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
    case ceColumnEnum(Column) of
    ceTimeColumn: if Assigned (xData.aLog) then
         CellText := DateTimeToStr (xData.aLog.InboundTimeStamp);
    ceServiceColumn: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.WsdlService.Name;
    ceOperationColumn: if Assigned (xData.aLog) and Assigned (xData.aLog.Operation) then
         CellText := xData.aLog.Operation.Name;
    ceMessageColumn: if Assigned (xData.aLog) and Assigned (xData.aLog.Mssg) then
         CellText := xData.aLog.Mssg.Name;
    ceCorrelationColumn: if Assigned (xData.aLog) then
         CellText := xData.aLog.CorrelationId;
    ceReqColumn: if Assigned (xData.aLog)
       and Assigned (xData.bLog) then
         if xData.reqA2B.Differs then
           CellText := 'X'
         else
           CellText := ''
       else
         CellText := 'X';
    ceRpyColumn: if Assigned (xData.aLog)
       and Assigned (xData.bLog) then
         if xData.rpyA2B.Differs then
           CellText := 'X'
         else
           CellText := ''
       else
         CellText := 'X';
    ceRefTimeColumn: if Assigned (xData.bLog) then
         CellText := DateTimeToStr (xData.bLog.InboundTimeStamp);
    ceRefServiceColumn: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.WsdlService.Name;
    ceRefOperationColumn: if Assigned (xData.bLog) and Assigned (xData.bLog.Operation) then
         CellText := xData.bLog.Operation.Name;
    ceRefMessageColumn: if Assigned (xData.bLog) and Assigned (xData.bLog.Mssg) then
         CellText := xData.bLog.Mssg.Name;
    ceRefCorrelationColumn: if Assigned (xData.bLog) then
         CellText := xData.bLog.CorrelationId;
    end;
  except
    on e: Exception do
    begin
      CellText := 'raised error: '  + e.Message;
    end;
  end;
end;

procedure TShowLogDifferencesForm .setCompareLogOrderBy (AValue : TCompareLogOrderBy );
begin
  if AValue = fCompareLogOrderBy then Exit;
  fCompareLogOrderBy := AValue ;
  CompareLogOrderByComboBox.ItemIndex := Ord (AValue);
end;

procedure TShowLogDifferencesForm.onSlChanged (aObject : TObject );
begin
  fConfigChanged := True;
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
    if aA2B.ThisOneDiffers and not aA2B.Ignored then
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
              if (xData.reqA2B.Differs and not xData.reqA2B.Ignored)
              or (xData.rpyA2B.Differs and not xData.rpyA2B.Ignored)
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
    XmlUtil.presentAsHTML('wsdlStub - Differences report', xXml.asHtmlString{.Text});
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TShowLogDifferencesForm .CompareLogOrderByComboBoxChange (
  Sender : TObject );
begin
  fCompareLogOrderBy := TCompareLogOrderBy(CompareLogOrderByComboBox.ItemIndex);
  PopulateMain (True);
end;

end.
