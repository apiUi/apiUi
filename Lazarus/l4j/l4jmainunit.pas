unit L4JMainUnit;

{$mode objfpc}{$H+}

interface

uses
  l4jTypes, sqldb, db, odbcconn, oracleconnection, LCLIntf, LCLType, LMessages,
  SysUtils
   , Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, VirtualTrees
   , Dialogs
   , FormIniFilez, ToolWin, ActnList, Menus, ImgList , FileUtil
   , FilterDialog
   , Xmlz
    ,A2BXmlz
   , Xsdz
   , ParserClasses
   , AbUnzper
   , Express
   , Bind
  {$ifdef WINDOWS}
   , ActiveX
  {$endif}
   ;
Type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Index: Integer;
  end;

Type
  TColumnEnum = (tceNumber, tceSize, tceTimestamp);

type

  TL4JMainForm = class;

  { TCustomThread }

  TCustomThread = class(TThread)
   private
     fNumber, fTotal: Integer;
     fStatusText, fMessage: AnsiString;
     fForm: TL4JMainForm;
     fString: AnsiString;
     fEnabled: Boolean;
     fAbortPressed: Boolean;
     fFilter1, fFilter2, fFilter3, fFilter4: AnsiString;
               fHasNot2, fHasNot3, fHasNot4: Boolean;
     function StringPassesFilter (aString: AnsiString): Boolean;
     procedure fSynchronisedShowMessage;
     procedure fSynchronisedOrderData;
     procedure fSynchronisedHaveData;
     procedure fSynchronisedStatusUpdate;
     procedure fSynchronisedEnableAbortButton;
     procedure fSynchronisedAdjustDisplayedColumns;
     procedure setAbortPressed(const Value: Boolean);
     procedure Extract (var s: AnsiString);
     procedure ExtractDisplayedColumns (var s: AnsiString);
   protected
     procedure UpdateStatus (aNumber, aTotal: Integer; aText: AnsiString);
   public
     property abortPressed: Boolean read fAbortPressed write setAbortPressed;
   end;

  { TStringThread }

  TStringThread = class(TCustomThread)
  private
    sString: AnsiString;
    function DbVis(s: AnsiString): AnsiString;
  protected
    procedure Execute; override;
  public
    constructor Create ( aForm: TL4JMainForm
                       ; aString: AnsiString
                       );
  end;

  TSearchThread = class(TCustomThread)
  private
    fFilenames: TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create ( aForm: TL4JMainForm
                       ; aFileNames: TStrings
                       ; aFilter1, aFilter2, aFilter3, aFilter4: AnsiString
                       ; aHasnot2, aHasNot3, aHasNot4: Boolean
                       );
  end;

  TQueryThread = class(TCustomThread)
  private
    fConnString, fQuery, fParam1, fParam2, fParam3, fParam4: String;
  protected
    procedure Execute; override;
  public
    constructor Create ( aForm: TL4JMainForm
                       ; aConnString, aQuery: String
                       ; aParam1, aParam2, aParam3, aParam4: String
                       );
  end;



  { TL4JMainForm }

  TL4JMainForm = class(TForm)
    AbortAction: TAction;
    About1: TMenuItem;
    MenuItem1: TMenuItem;
    CompareNodesMenuItem: TMenuItem;
    PasteFromClipboardAction : TAction ;
    ActionImageList: TImageList;
    ActionList1: TActionList;
    AllXmlAction: TAction;
    CloseAction: TAction;
    CloseAction1: TMenuItem;
    CopyAction: TAction;
    CopyAction1: TMenuItem;
    DeleteAction: TAction;
    Edit1: TMenuItem;
    Editconfig1: TMenuItem;
    Editdisplayedcolumns1: TMenuItem;
    EditDisplayedColumnsAction: TAction;
    ElementAttribute1: TMenuItem;
    Extra1: TMenuItem;
    File1: TMenuItem;
    FilterAction: TAction;
    Find1: TMenuItem;
    FindAction: TAction;
    FindNext1: TMenuItem;
    FindNextAction: TAction;
    FullCollapseAction: TAction;
    FullExpandAction: TAction;
    Help1: TMenuItem;
    License1: TMenuItem;
    MainMenu1: TMainMenu;
    Memo: TMemo;
    N1: TMenuItem;
    N2: TMenuItem;
    N4: TMenuItem;
    NodeCopyAction: TAction;
    NodeFullCollapseAction: TAction;
    NodeFullExpandAction: TAction;
    NodeWriteXmlAction: TAction;
    Openconfig1: TMenuItem;
    OpenFileAction: TAction;
    OpenLog4Jevents1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Paste1: TMenuItem;
    ProgressBar: TProgressBar;
    QueryDbAction: TAction;
    QueryDbAction1: TMenuItem;
    Save1: TMenuItem;
    Saveconfig1: TMenuItem;
    Saveconfigas1: TMenuItem;
    Savelogevents1: TMenuItem;
    Search1: TMenuItem;
    Splitter1: TSplitter;
    SQLConnector1: TSQLConnector;
    SqlQuery: TSQLQuery;
    ToolButton18 : TToolButton ;
    xSqlQuery: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TreeView: TVirtualStringTree;
    TvPopupMenu: TPopupMenu;
    View1: TMenuItem;
    WraptekstMenuItem: TMenuItem;
    WriteXmlAction: TAction;
    ZoomMenuAction: TMenuItem;
    procedure SaveAccordingSelected(aSelectedOnly: Boolean);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure CopyActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure CompareNodesMenuItemClick(Sender: TObject);
    procedure PasteFromClipboardActionExecute (Sender : TObject );
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TvPopupMenuPopup(Sender: TObject);
    procedure WriteXmlActionExecute(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure WraptekstMenuItemClick(Sender: TObject);
    procedure ElementAttribute1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Editconfig1Click(Sender: TObject);
    procedure Saveconfigas1Click(Sender: TObject);
    procedure Saveconfig1Click(Sender: TObject);
    procedure Openconfig1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure License1Click(Sender: TObject);
    procedure AllXmlActionHint(var HintStr: string; var CanShow: Boolean);
    procedure AllXmlActionUpdate(Sender: TObject);
    procedure AllXmlActionExecute(Sender: TObject);
    procedure AbortActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
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
      var CellText: String);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewPopupMenuPopup(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure TreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewDblClick(Sender: TObject);
    procedure EditDisplayedColumnsActionExecute(Sender: TObject);
    procedure QueryDbActionExecute(Sender: TObject);
  private
    fReadOnly: Boolean;
    SaveLog4JFileName, ReadLog4JFileName: String;
    ColumnWidths: TStringList;
    l4jDbName, l4jIniFileName, l4jXsdFileName, configFileName: String;
    iniXml: TXml;
    l4jXsdDescr: TXsdDescr;
    DisplayedColumns: TStringList;
    DisplayedColumnsXsd, configXsd: TXsd;
    DisplayedColumnsXml: TXml;
    DisplayedColumnsChanged: Boolean;
    configRead, configChanged: Boolean;
    function getDoWrapText: Boolean;
    function getReadOnly: Boolean;
    procedure setDoWrapText(AValue: Boolean);
    procedure setReadOnly(AValue: Boolean);
    procedure AdjustDisplayedColumns (aXml: TXml);
    function GetEndurance (aString: String): String;
    function GetAtt (aKey, aString: String): String;
    function GetElm (aKey, aString: String): String;
    function GetEscXmlElm (aKey, aString: String): String;
    function GetHasString (aDc: TDisplayedColumn; aString: String): String;
    function GetHasXmlValue (aDc: TDisplayedColumn; aString: String): String;
    function GetCellText (aData: TStringList; aIndex, aColumn: Integer): String;
    function Filter (Data: TStringList; aIndex: Integer): Boolean;
    procedure l4jInit (aIniFile: TFormIniFile);
    procedure ParserError(Sender: TObject; LineNumber, ColumnNumber,
      Offset: Integer; TokenString, Data: String);
    procedure saveConfig (aFileName: String);
    procedure readConfig (aFileName: String);
    function configAsXml: TXml;
    procedure configFromXml (aXml: TXml);
    procedure OnlyWhenLicensed;
    function OkToOpenCase: Boolean;
  public
    isChanged: Boolean;
    Data: TStringList;
    Thread: TCustomThread;
    numberVisible: Integer;
    ShowDetailed: String;
    logTypes: TLogTypes;
    readDisplayedColumnsXml: TXml;
    procedure UseReadDisplayedColumns;
    function findLogType (aString: AnsiString): TLogType;
    function prepareDataToZoom (s: String): String;
    property doWrapText: Boolean read getDoWrapText write setDoWrapText;
    property isReadOnly: Boolean read getReadOnly write setReadOnly;
  end;

var
  L4JMainForm: TL4JMainForm;
  dbs: TSQLConnector;
  qry, xqry: TSQLQuery;

implementation

uses FindRegExpDialog
   , ShowA2BXmlUnit
   , igGlobals
   , ClipBrd
   , xmlUtilz
   , strUtils
   , IdSync
   , ErrorFound
   , cbAbout
   , ShowXmlUnit
   , XmlGridUnit
   , PromptUnit
   , CommandDialog
   , DbFilterDialog
   , xmlxsdparser
   , RegExpr
   , xmlio
   ;

const treeIndexColumn = 0;
const treeSizeColumn = 1;
const treeTimeStampColumn = 2;
const statusInitial = 100;
const statusDensity = 1000;
const DbVisTag = '<DbVisualizer-Export>';


{$R *.lfm}

procedure sqlLoop;
var
  Msg: l4jTypes.TMsg;
  f, x, y: Integer;
  xEventData: String;
  s, sx, nm: String;
  field: TField;
  EventDataLength: Integer;
  RowId: String;
begin
  if (not xpFetched)
{ TODO : abortfie
 }
// and (not xp.abortPressed)
  then
    L4JMainForm.Thread.UpdateStatus(5, 10, 'Fetched first data...');
  xpFetched := True;

  TimeStamp:='';
  EventType:='';
  EventDataLength:=-1;
  EventData:='';
  sx := '';

  for f := 0 to Qry.Fields.Count - 1 do
  begin
    field := Qry.Fields.Fields[f];
    nm := UpperCase(field.DisplayName);
    if (nm = 'TIMESTAMP') then
      try TimeStamp := field.DisplayText; except end
    else
    begin
      if (nm = 'CORRELATIONID') then
        CorrelationId:=field.AsString
      else
      begin
        if (nm = 'TUPLEID') then
          RowId:=field.AsString
        else
        begin
          if AnsiStartsStr('EVENTTYPE', nm) then
            EventType:=field.AsString
          else
          begin
            if AnsiStartsStr('EVENTDATA', nm) then
              EventData:=EventData+field.AsString
            else
            begin
              if AnsiStartsStr('LENGTHEVENTDATA', nm) then
                EventDataLength:=field.AsInteger
              else
              begin
                sx := sx
                    + '<' + field.DisplayName + '>'
                    + field.AsString
                    + '</' + field.DisplayName + '>'
                    ;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  if Msgs.Find(CorrelationId, f) then
    Msg := Msgs.Msg[f]
  else
  begin
    Msg := l4jTypes.TMsg.Create;
    Msgs.AddObject(CorrelationId, Msg);
  end;
  if (TimeStamp < Msg.FirstTimeStamp) then
    Msg.FirstTimeStamp := TimeStamp;
  if (TimeStamp > Msg.LastTimeStamp) then
    Msg.LastTimeStamp := TimeStamp;
  Inc (Msg.Count);
  s := '<EventHeader>'
     + '<EVENT_TYPE>' + EventType + '</EVENT_TYPE>'
     + '<TimeStamp>' + TimeStamp + '</TimeStamp>'
     + '<CorrelationId>' + CorrelationId + '</CorrelationId>'
     + sx
     + '</EventHeader>'
     ;
  if EventDataLength > Length (EventData) then
  begin
    x := 1;
    while Length (EventData) < EventDataLength do
    begin
      xqry.SQL.Clear;
      xqry.SQL.Add ( Format ( 'select dbms_lob.substr (event_data, %d, %d) as EventData'
                            , [                                     SizeOfDataPart
                              ,                                         1 + x * SizeOfDataPart
                              ]
                            )
                   );
      xqry.SQL.Add ( 'from app_log_data');
      xqry.SQL.Add ( 'where RowId = ''' + RowId + '''');
      try
        xqry.Open;
        try
          xqry.First;
          if xqry.EOF then
            raise Exception.Create('EOF at selecting on RowId: ' + RowId);

          xEventData:=xqry.Fields[0].AsString;
        finally
          xqry.Close;
        end;
      except
        on e: Exception do
        begin
          xEventData :=  LineEnding + 'Exception in query EventData substrings: ' + e.Message;
          EventDataLength:=-1; // force out of loop
        end;
      end;
      if xEventData = '' then
        raise Exception.Create('Read empty string on RowId: ' + RowId);
      EventData:=EventData+xEventData;
      Inc (x);
    end;
    RowId := RowId;
  end;
  Msg.headers:= Msg.headers + s;
  Msg.events := Msg.events
              + '<' + EventType + '>' + EventData + '</' + EventType + '>'
              + LineEnding
              ;
end;

function xmlLoop (aXml: TXml): String;
var
  Msg: l4jTypes.TMsg;
  f, r, y: Integer;
  s, sx, nm: String;
  field: TField;
  rXml, fXml: TXml;
  xCorrelationId, xMessageId, xUserTaskId, xPGID: String;
  fProcessed: Boolean;
begin
  if aXml.Name <> 'ROWSET' then
    raise Exception.Create('procedure xmlLoop (aXml: TXml); illegal argument ' + aXml.Name);
  if (not xpFetched)
{ TODO : abortfie
 }
// and (not xp.abortPressed)
  then
    L4JMainForm.Thread.UpdateStatus(5, 10, 'Converting data...');
  xpFetched := True;
  for r := 0 to aXml.Items.Count - 1 do
  begin
    TimeStamp:='';
    CorrelationId:='';
    EventType:='';
    EventData:='';
    sx := '';
    xCorrelationId := '';
    xPGID:='';
    xUserTaskId:='';
    rXml := aXml.Items.XmlItems[r];
    for f := 0 to rXml.Items.Count - 1 do
    begin
      fXml := rXml.Items.XmlItems[f];
      nm := UpperCase(fXml.Name);
      fProcessed:=False;
      if (nm = 'LOG_TIME') then
      begin
        TimeStamp := fXml.Value;
        fProcessed := True;
      end;
      if (nm = 'PROCESS_GROUP_INSTANCE_ID') then
      begin
        xPGID:=xmlDecodeXml(fXml.Value);
        fProcessed := False;   // remains False intentionally
      end;
      if (nm = 'USER_TASK_ID') then
      begin
        xUserTaskId:=xmlDecodeXml(fXml.Value);
        fProcessed := False;
      end;
      if (nm = 'SERVICE_MESSAGE_ID') then
      begin
        xMessageId:=xmlDecodeXml(fXml.Value);
        fProcessed := False;
      end;
      if (nm = 'EVENT_TYPE') then
      begin
        EventType:=fXml.Value;
        fProcessed := False;
      end;
      if AnsiStartsStr('EVENT_DATA', nm) then
      begin
        EventData:=fXml.Value;
        fProcessed := True;
      end;
      if not fProcessed then
      begin
        sx := sx
            + '<' + fXml.Name + '>'
            + xmlDecodeXml(fXml.Value)
            + '</' + fXml.Name + '>'
            ;
        fProcessed := True;
      end;
    end;
    xCorrelationId := xPGID + ';' + xMessageId + ';' + xUserTaskId;
    if Msgs.Find(xCorrelationId, f) then
      Msg := Msgs.Msg[f]
    else
    begin
      Msg := l4jTypes.TMsg.Create;
      Msgs.AddObject(xCorrelationId, Msg);
    end;
    if (TimeStamp < Msg.FirstTimeStamp) then
      Msg.FirstTimeStamp := TimeStamp;
    if (TimeStamp > Msg.LastTimeStamp) then
      Msg.LastTimeStamp := TimeStamp;
    Inc (Msg.Count);
  {  &amp;lt;clinit&amp;gt; }
    EventData := strUtils.ReplaceText(EventData, '&lt;', '<');
    EventData := strUtils.ReplaceText(EventData, '&gt;', '>');
    EventData := strUtils.ReplaceText(EventData, '&quot;', '"');
    EventData := strUtils.ReplaceText(EventData, '&#47;', '/');
    EventData := strUtils.ReplaceText(EventData, '&#39;', '''');
    EventData := strUtils.ReplaceText(EventData, '&amp;', '&');
    EventData := strUtils.ReplaceText(EventData, '&lt;', '&amp;lt;');
    EventData := strUtils.ReplaceText(EventData, '&gt;', '&amp;gt;');
  {}
    s := '<EventHeader>'
       + '<EventType>' + EventType + '</EventType>'
       + '<TimeStamp>' + TimeStamp + '</TimeStamp>'
       + '<CorrelationId>' + CorrelationId + '</CorrelationId>'
       + sx
       + '</EventHeader>'
       ;
    Msg.headers:= Msg.headers + s;
    Msg.events := Msg.events
                + '<' + EventType + '>' + EventData + '</' + EventType + '>'
                + LineEnding
                ;
  end;

end;

{ TStringThread }

function TStringThread.DbVis(s: AnsiString): AnsiString;
var
  xp, sp: PAnsiChar;
  xXml: TXml;
  x: Integer;
begin
  result := s;
  xp := strUtils.SearchBuf(@s[1], 1000, 0, 0, DbVisTag, [soDown, soMatchCase]);
  if Assigned (xp) then
  begin
    xXml := TXml.Create;
    Msgs := TMsgList.Create;
    try
      Msgs.Sorted := True;
      xXml.LoadFromString(s, nil);
      if xXml.Items.Count > 0 then
        result := xmlLoop(xXml.Items.XmlItems[0]);
      if abortPressed then Exit;
      UpdateStatus (7, 10, 'Formatting output...');
      for x := 0 to Msgs.Count - 1 do
      begin
        fForm.Data.Add (Msgs.Msg[x].AsText);
        Inc (fForm.numberVisible);
        if abortPressed then Exit;
      end;
      with TIdSync.Create do
        try
          SynchronizeMethod (@fSynchronisedHaveData);
        finally
          Free;
        end;
      UpdateStatus (8, 10, 'Sorting output...');
      with TIdSync.Create do
        try
          SynchronizeMethod (@fSynchronisedOrderData);
        finally
          Free;
        end;
      UpdateStatus (9, 10, 'Cleaning up...');
    finally
      xXml.Free;
      Msgs.Clear;
      Msgs.Free;
    end;
  end;
end;

procedure TStringThread .Execute ;
var
  s: AnsiString;
begin
  fEnabled := True;
  with TIdSync.Create do
    try
      SynchronizeMethod (@fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
  try
    UpdateStatus (1, 3, 'from clipboard');
    s := DbVis(sString);
    Extract(s);
  finally
    UpdateStatus (0, 0, '');
    fEnabled := False;
    with TIdSync.Create do
      try
        SynchronizeMethod (@fSynchronisedEnableAbortButton);
      finally
        Free;
      end;
  end;
end;

constructor TStringThread .Create (aForm : TL4JMainForm ; aString : AnsiString
  );
begin
  inherited Create (False);
  fForm := aForm;
  sString := aString;
  abortPressed := False;
  FreeOnTerminate := True;
end;

{ TCustomThread }

procedure TCustomThread.fSynchronisedAdjustDisplayedColumns;
begin
  fForm.UseReadDisplayedColumns;
end;

procedure TCustomThread.fSynchronisedEnableAbortButton;
begin
  fForm.AbortAction.Enabled := fEnabled;
  fForm.OpenFileAction.Enabled := not fEnabled;
  fForm.QueryDbAction.Enabled := not fEnabled;
{
  fForm.ProgressBar.Visible := fEnabled;
  fForm.StatusBar.Visible := fEnabled;
}
end;

procedure TCustomThread.fSynchronisedHaveData;
begin
//fForm.Data.Add (fString);
  with fForm do
  begin
    TreeView.RootNodeCount := Data.Count;
  end;
end;

function TCustomThread .StringPassesFilter (aString : AnsiString ): Boolean ;
begin
  result := (   (fFilter1 = '')
             or (Pos (fFilter1, aString) > 0)
            )
        and (   (fFilter2 = '')
             or (    (not fHasNot2)
                 and (Pos (fFilter2, aString) > 0)
                )
             or (    (fHasNot2)
                 and (not (Pos (fFilter2, aString) > 0))
                )
            )
        and (   (fFilter3 = '')
             or (    (not fHasNot3)
                 and (Pos (fFilter3, aString) > 0)
                )
             or (    (fHasNot3)
                 and (not (Pos (fFilter3, aString) > 0))
                )
            )
        and (   (fFilter4 = '')
             or (    (not fHasNot4)
                 and (Pos (fFilter4, aString) > 0)
                )
             or (    (fHasNot4)
                 and (not (Pos (fFilter4, aString) > 0))
                )
            )
          ;
end;

procedure TCustomThread.fSynchronisedShowMessage;
begin
  ShowMessage (fMessage);
end;

procedure TCustomThread.fSynchronisedOrderData;
var
  xColumn: TColumnIndex;
begin
  with fForm do
  begin
    xColumn := Ord (tceTimestamp);
    TreeView.Header.SortColumn := xColumn;
    TreeView.Header.SortDirection := sdAscending;
    TreeView.SortTree(xColumn, TreeView.Header.SortDirection, True);
  end;
end;

procedure TCustomThread.fSynchronisedStatusUpdate;
begin
  fForm.ProgressBar.Max := fTotal;
  fForm.ProgressBar.Position := fNumber;
  fForm.StatusBar.SimpleText := fStatusText;
  fForm.StatusBar.Update;
end;

procedure TCustomThread.setAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
  { TODO : abortfie }
//    xp.abortPressed := True
    ;
end;

procedure TCustomThread .Extract (var s : AnsiString );
var
  x: Integer;
  sp, ep, xp: PAnsiChar;
  xOptions: TStringSearchOptions;
  sSearchString, eSearchString, xSearchString: AnsiString;
  xLogType: TLogType;
begin
  if s = '' then
    exit;
  try
    try
      xLogType := fForm.findLogType(s);
    except
      on e: Exception do
      begin
        raise Exception.Create('findLogType: ' + e.Message);
      end;
    end;
    if Assigned (xLogType) then
    begin
      sSearchString := xLogType.sTag;
      eSearchString := xLogType.eTag;
      xOptions := [soDown, soMatchCase];
      if fFilter1 = '' then
        xSearchString := eSearchString
      else
        xSearchString := fFilter1;
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
    end;
  finally
    with TIdSync.Create do
      try
        SynchronizeMethod (@fSynchronisedHaveData);
      finally
        Free;
      end;
  end;
end;

procedure TCustomThread .ExtractDisplayedColumns (var s : AnsiString );
var
  sp, ep: PAnsiChar;
  len: Integer;
  xSearchString, xXmlAnsiString: AnsiString;
  xXmlString: String;
begin
  fForm.readDisplayedColumnsXml.Items.Clear;
  xSearchString := '<DisplayedColumns>';
  len := 1000;
  if Length (s) < 1000 then
    len := Length (s);
  sp := strUtils.SearchBuf(@s[1], len, 0, 0, xSearchString, [soDown, soMatchCase]);
  if Assigned (sp) then
  begin
    xSearchString := '</DisplayedColumns>';
    ep := strUtils.SearchBuf(@s[1], Length(s), 0, 0, xSearchString, [soDown, soMatchCase]);
    if Assigned (ep) then
    begin
      xXmlAnsiString := Copy (s, sp - @s[1] + 1, ep + Length (xSearchString) - sp);
      xXmlString := xXmlAnsiString;
      fForm.readDisplayedColumnsXml.LoadFromString(xXmlString, nil);
      with TIdSync.Create do
        try
          SynchronizeMethod (@fSynchronisedAdjustDisplayedColumns);
        finally
          Free;
        end;
    end;
  end;
end;

{ TSearchThread }

constructor TSearchThread.Create ( aForm: TL4JMainForm
                                 ; aFileNames: TStrings
                                 ; aFilter1, aFilter2, aFilter3, aFilter4: AnsiString
                                 ; aHasNot2,aHasNot3,aHasNot4: Boolean
                                 );
begin
  inherited Create (False);
  fForm := aForm;
  fFileNames := TStringList.Create;
  fFileNames.Text := aFileNames.Text;
  fFilter1 := aFilter1;
  fFilter2 := aFilter2;
  fFilter3 := aFilter3;
  fFilter4 := aFilter4;
  fHasNot2 := aHasNot2;
  fHasNot3 := aHasNot3;
  fHasNot4 := aHasNot4;
  abortPressed := False;
  FreeOnTerminate := True;
end;

procedure TSearchThread.Execute;
var
  x, y, yC: Integer;
  s: AnsiString;
  ss: TMemoryStream;
  aUnzipper: TAbUnZipper;
  xExt, fn: String;
begin
  fEnabled := True;
  with TIdSync.Create do
    try
      SynchronizeMethod (@fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
  try
    x := 0;
    while (not abortPressed)
    and (x < fFileNames.Count) do
    begin
      try
        UpdateStatus (x + 1, fFileNames.Count + 1, fFileNames.Strings[x]);
        xExt := UpperCase(ExtractFileExt(fFileNames.Strings[x]));
        if (xExt = '.ZIP')
        or (xExt = '.GZ')
        then begin
 {
          aUnzipper := TAbUnZipper.Create(nil);
 }
          try
            aUnzipper := TAbUnZipper.Create(nil);
            aUnzipper.FileName := fFileNames.Strings[x];
            yC := aUnzipper.Count;
            FreeAndNil (aUnzipper);
            y := 0;
            while (y < yC)
            and (not abortPressed) do
            begin
              aUnzipper := TAbUnZipper.Create(nil);
              aUnzipper.FileName := fFileNames.Strings[x];
              ss := TMemoryStream.Create;
              try
                try
                  fn := aUnzipper.Items[y].FileName;
                  UpdateStatus ( statusInitial + statusDensity * x + ((y * statusDensity) div yc)
                               , statusInitial + statusDensity * fFileNames.Count
                               , fFileNames.Strings[x]
                               + ' ['
                               + fn
                               + ']'
                               );
                  aUnzipper.ExtractToStream(aUnzipper.Items[y].FileName, ss);
                  if ss.Size > 0 then
                  begin
                    ss.Position := 0;
                    SetLength(s, ss.Size);
                    ss.Read(Pointer(s)^, ss.Size);
                    if (x = 0) and (y = 0) then
                      ExtractDisplayedColumns (s);
                    Extract (s);
                  end;
                  s := '';
                except
                  on e: Exception do
                  begin
                    s := '';  // Deallocates memory
                    fMessage := e.Message;
                    with TIdSync.Create do
                      try
                        SynchronizeMethod (@fSynchronisedShowMessage);
                      finally
                        Free;
                      end;
                    raise;
                  end;
                end;
              finally
                FreeAndNil (ss);
                FreeAndNil (aUnzipper);
              end;
              Inc (y);
            end;
          finally
            aUnzipper.Free;
          end;
        end
        else
        begin
          try
            s := ReadStringFromFile (fFileNames.Strings[x]);
            if x = 0 then
              ExtractDisplayedColumns(s);
            Extract (s);
            s := '';
          except
            on e: Exception do
            begin
              s := '';  // Deallocates memory
              fMessage := e.Message;
              with TIdSync.Create do
                try
                  SynchronizeMethod (@fSynchronisedShowMessage);
                finally
                  Free;
                end;
              raise;
            end;
          end;
        end;
      finally
        s := '';
        Inc (x);
      end;
    end;
  finally
    UpdateStatus (0, 0, '');
    fEnabled := False;
    with TIdSync.Create do
      try
        SynchronizeMethod (@fSynchronisedEnableAbortButton);
      finally
        Free;
      end;
  end;
end;

{ TQueryThread }

constructor TQueryThread.Create(aForm: TL4JMainForm; aConnString,
  aQuery: String; aParam1, aParam2, aParam3, aParam4: String);
begin
  inherited Create (False);
  fConnString := aConnString;
  fQuery := aQuery;
  fForm := aForm;
  fParam1 := aParam1;
  fParam2 := aParam2;
  fParam3 := aParam3;
  fParam4 := aParam4;
  abortPressed := False;
  FreeOnTerminate := True;
end;

procedure TQueryThread.Execute;
var
  cs, qryText: String;
  x: Integer;
begin
  fEnabled := True;
  EventData:='';
  with TIdSync.Create do
    try
      SynchronizeMethod (@fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
  try
    try
      UpdateStatus (1, 10, 'Preparing...');
      qryText := ReplaceStrings( fQuery
                               , '$EventData'
                               , getEventDataQuery(True)
                               , false
                               , false
                              );
      xpFetched := False;
      if abortPressed then Exit;
      with TXml.Create do
      try
        LoadFromFile(fConnString, nil);
        if TagName <> 'DataSource' then
          raise Exception.CreateFmt('%s does not contain valid ConnectionString data', [fConnString]);
        try dbs.Connected:=False; Except end;
        dbs.ConnectorType:=Items.XmlValueByTagDef['ConnectorType', 'Oracle'];
        dbs.DatabaseName:=Items.XmlValueByTagDef['DatabaseName', 'XE'];
        dbs.HostName:=Items.XmlValueByTag['HostName'];
        dbs.Params.Text:=ReplaceStrings( Items.XmlValueByTag['Params']
                                      , ';'
                                      , LineEnding
                                      , false
                                      , false
                                      );
        dbs.Password:=XmlUtil.SimpleEncrypt(Items.XmlValueByTag['Password']);
        dbs.Params.Text:=ReplaceStrings( dbs.Params.Text
                                      , '%pwd%'
                                      , dbs.Password
                                      , false
                                      , false
                                      );
        dbs.UserName:=Items.XmlValueByTag['UserName'];
      finally
        Free;
      end;
      qry.SQL.Text:=qryText;
      for x := 0 to qry.Params.Count - 1 do
      begin
        if qry.Params.Items[x].Name = 'Param1' then qry.Params.Items[x].AsString:=fParam1;
        if qry.Params.Items[x].Name = 'Param2' then qry.Params.Items[x].AsString:=fParam2;
        if qry.Params.Items[x].Name = 'Param3' then qry.Params.Items[x].AsString:=fParam3;
        if qry.Params.Items[x].Name = 'Param4' then qry.Params.Items[x].AsString:=fParam4;
      end;
      if abortPressed then Exit;
      Msgs := TMsgList.Create;
      try
        Msgs.Sorted := True;
        UpdateStatus (2, 10, 'Querying datasource...');
        qry.Open;
        qry.First;
        try
          while (not qry.EOF)
          and (not abortPressed) do
          begin
            sqlLoop;
            qry.Next;
          end;
        finally
          qry.Close;
        end;
        if abortPressed then Exit;
        UpdateStatus (7, 10, 'Formatting output...');
        for x := 0 to Msgs.Count - 1 do
        begin
          fForm.Data.Add (Msgs.Msg[x].AsText);
          Inc (fForm.numberVisible);
          if abortPressed then Exit;
        end;
        with TIdSync.Create do
          try
            SynchronizeMethod (@fSynchronisedHaveData);
          finally
            Free;
          end;
        UpdateStatus (8, 10, 'Sorting output...');
        with TIdSync.Create do
          try
            SynchronizeMethod (@fSynchronisedOrderData);
          finally
            Free;
          end;
        UpdateStatus (9, 10, 'Cleaning up...');
      finally
        Msgs.Clear;
        Msgs.Free;
      end;
    except
      on e: Exception do
      begin
        fMessage := e.Message;
        with TIdSync.Create do
          try
            SynchronizeMethod (@fSynchronisedShowMessage);
          finally
            Free;
          end;
        raise;
      end;
    end;
  finally
    UpdateStatus (0, 0, '');
    fEnabled := False;
    with TIdSync.Create do
      try
        SynchronizeMethod (@fSynchronisedEnableAbortButton);
      finally
        Free;
      end;
  end;
end;

procedure TCustomThread.UpdateStatus(aNumber, aTotal: Integer; aText: AnsiString);
begin
  fNumber := aNumber;
  fTotal := aTotal;
  fStatusText := aText;
  with TIdSync.Create do
    try
      SynchronizeMethod (@fSynchronisedStatusUpdate);
    finally
      Free;
    end;
end;

procedure TL4JMainForm.AdjustDisplayedColumns(aXml: TXml);
var
  x: Integer;
  xDc: TDisplayedColumn;
  ct: String;
begin
  for x := TreeView.Header.Columns.Count - 1  downto 0 do
    ColumnWidths.Values [TreeView.Header.Columns.Items[x].Text]
    := IntToStr (TreeView.Header.Columns.Items[x].Width);
  TreeView.Header.Columns.Clear;
  for x := 0 to DisplayedColumns.Count - 1 do
    DisplayedColumns.Objects[x].Free;
  DisplayedColumns.Clear;
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'number';
  xDc.ColumnType := ctIndex;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'size';
  xDc.ColumnType := ctSize;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'timestamp';
  xDc.Key := 'timestamp';
  xDc.ColumnType := ctAttribute;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'endurance';
  xDc.ColumnType := ctElapsed;
  DisplayedColumns.AddObject('', xDc);
{
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'level';
  xDc.Key := 'level';
  xDc.ColumnType := ctAttribute;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'ServiceId';
  xDc.Key := 'ServiceId';
  xDc.ColumnType := ctElement;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'SrqstrId';
  xDc.Key := 'SrqstrId';
  xDc.ColumnType := ctElement;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'MessageId';
  xDc.Key := 'MessageId';
  xDc.ColumnType := ctElement;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'FaultCode';
  xDc.Key := 'FaultCode';
  xDc.ColumnType := ctElement;
  DisplayedColumns.AddObject('', xDc);
  xDc := TDisplayedColumn.Create;
  xDc.Header := 'FaultDescription';
  xDc.Key := 'FaultDescription';
  xDc.ColumnType := ctElement;
  DisplayedColumns.AddObject('', xDc);
}
  for x := 0 to aXml.Items.Count - 1 do
  begin
    if aXml.Items.XmlItems[x].Checked then
    begin
      with aXml.Items.XmlItems[x].Items do
      begin
        if XmlCheckedBooleanByTagDef ['Enabled', true] then
        begin
          xDc := TDisplayedColumn.Create;
          xDc.Key := XmlCheckedValueByTagDef['Key', ''];
          xDc.Header := XmlCheckedValueByTagDef['ColumnHeader', xDc.Key];
          ct := XmlCheckedValueByTagDef['Type', 'Element'];
          if ct = 'Attribute' then xDc.ColumnType := ctAttribute;
          if ct = 'Element' then xDc.ColumnType := ctElement;
          if ct = 'EscXmlElement' then xDc.ColumnType := ctEscXmlElement;
          if ct = 'HasString' then xDc.ColumnType := ctHasString;
          if ct = 'HasXmlValue' then xDc.ColumnType := ctHasXmlValue;
          DisplayedColumns.AddObject('', xDc);
        end;
      end;
    end;
  end;
  for x := 0 to DisplayedColumns.Count - 1 do
  begin
    with TreeView.Header.Columns.Add do
    begin
      Text := (DisplayedColumns.Objects [x] as TDisplayedColumn).Header;
      Width :=
        StrToIntDef ( ColumnWidths.Values [Text]
                    , Width
                    );
    end;
  end;
end;

function TL4JMainForm.GetEndurance(aString: String): String;
begin
  result := '';
  try
    result := FloatToStrF((  xsdParseDateTime(GetAtt ('lasttimestamp', aString))
                          - xsdParseDateTime(GetAtt ('timestamp', aString))
                          ) * 24 * 60 * 60
                         , ffNumber
                         , 18
                         , 6
                         );
  except
  end;
end;


function TL4JMainForm.GetEscXmlElm(aKey, aString: String): String;
var
  x: Integer;
begin
// &lt;ns2:Woonplaats&gt;Veghel&lt;
  result := '';
  if aKey = '' then
  begin
    result := aString;
    exit;
  end;
  x := Pos ('&lt;' + aKey + '&gt;', aString);
  if x < 1 then
  begin
    x := Pos (':' + aKey + '&gt;', aString); // maybe a nsprefix, maybe followed by an xml attribute
    if x < 1 then
      x := Length (aString) + 1
    else
      Inc (x);
  end
  else
  begin
    while (x < Length (aString))
    and (aString[x] <> ';') do
      Inc (x);
    Inc (x);
  end;
  while (x < Length (aString))
  and (aString[x] <> '&') do
    Inc (x);
  while (x < Length (aString))
  and (aString[x] <> ';') do
    Inc (x);
  Inc (x);
  while (x < Length (aString))
  and (aString[x] <> '&') do
  begin
    result := result + aString [x];
    Inc (x);
  end;
end;

function TL4JMainForm.GetHasString(aDc: TDisplayedColumn; aString: String
  ): String;
begin
  if Pos (aDc.Key, aString) > 0 then
    result := aDc.Header
  else
    result := '';
end;

function TL4JMainForm.GetHasXmlValue(aDc: TDisplayedColumn; aString: String
  ): String;
begin
  if Pos ('>' + aDc.Key + '<', aString) > 0 then
    result := aDc.Header
  else
    result := '';
end;

function TL4JMainForm.GetElm(aKey, aString: String): String;
var
  x: Integer;
begin
  result := '';
  if aKey = '' then
  begin
    result := aString;
    exit;
  end;
  x := Pos ('<' + aKey + '>', aString);
  if x < 1 then
      x := Pos (':' + aKey + '>', aString); // maybe a nsprefix, maybe followed by an xml attribute
  if x < 1 then
    x := Length (aString) + 1;
  while (x < Length (aString))
  and (aString[x] <> '>') do
    Inc (x);
  Inc (x);
  while (x < Length (aString))
  and (aString[x] <> '<') do
  begin
    result := result + aString [x];
    Inc (x);
  end;
end;

function TL4JMainForm.GetAtt(aKey, aString: String): String;
var
  x: Integer;
  s: String;
begin
  result := '';
  if aKey = '' then
  begin
    result := aString;
    exit;
  end;
  if aKey = '$endurance' then
  begin
    result := GetEndurance (aString);
    exit;
  end;
  with TRegExpr.Create do
  try
    Expression := aKey + ' *\= *\"[^\"]*\"';
    if Exec(aString) then
    begin
      s := Match[0];
      Expression:='"[^\"]*\"';
      if Exec (s) then
        Result := Copy (Match[0], 2, Length (Match[0]) - 2);
    end;
  finally
    Free;
  end;
end;

function TL4JMainForm.GetCellText(aData: TStringList; aIndex, aColumn: Integer
  ): String;
var
  xDc: TDisplayedColumn;
begin
  case aColumn of
   -1: result := Data.Strings[aIndex];
   else
   begin
     xDc := (DisplayedColumns.Objects[aColumn] as TDisplayedColumn);
     case xDc.ColumnType of
       ctAttribute: result := GetAtt (xDc.Key, Data.Strings[aIndex]);
       ctElement:  result := GetElm (xDc.Key, Data.Strings[aIndex]);
       ctEscXmlElement:  result := GetEscXmlElm (xDc.Key, Data.Strings[aIndex]);
       ctIndex: result := IntToStr (aIndex);
       ctElapsed: result := GetAtt ('$endurance', Data.Strings[aIndex]);
       ctSize: result := IntToStr (Length (Data.Strings[aIndex]));
       ctHasString: result := GetHasString (xDc, Data.Strings[aIndex]);
       ctHasXmlValue: result := GetHasXmlValue (xDc, Data.Strings[aIndex]);
     end;
   end;
  end;
end;

function TL4JMainForm.Filter(Data: TStringList; aIndex: Integer): Boolean;
var
  xString: String;
begin
  xString := Data.Strings [aIndex];
  result := (   (FilterDlg.FindEdit0.Text = '')
             or (Pos (FilterDlg.FindEdit0.Text, xString) > 0)
            )
        and (   (FilterDlg.FindEdit1.Text = '')
             or (    (not FilterDlg.HasNotCheckBox1.Checked)
                 and (Pos (FilterDlg.FindEdit1.Text, xString) > 0)
                )
             or (    (FilterDlg.HasNotCheckBox1.Checked)
                 and (not (Pos (FilterDlg.FindEdit1.Text, xString) > 0))
                )
            )
        and (   (FilterDlg.FindEdit2.Text = '')
             or (    (not FilterDlg.HasNotCheckBox2.Checked)
                 and (Pos (FilterDlg.FindEdit2.Text, xString) > 0)
                )
             or (    (FilterDlg.HasNotCheckBox2.Checked)
                 and (not (Pos (FilterDlg.FindEdit2.Text, xString) > 0))
                )
            )
        and (   (FilterDlg.FindEdit3.Text = '')
             or (    (not FilterDlg.HasNotCheckBox3.Checked)
                 and (Pos (FilterDlg.FindEdit3.Text, xString) > 0)
                )
             or (    (FilterDlg.HasNotCheckBox3.Checked)
                 and (not (Pos (FilterDlg.FindEdit3.Text, xString) > 0))
                )
            )
          ;
end;

procedure TL4JMainForm.l4jInit (aIniFile: TFormIniFile);
var
  x: Integer;
  xXml: TXml;
  xLogType: TLogType;
begin
  xmlUtil.doExpandFull := True;
  _xmlLicensed := True;
  try
    l4jDbName := '';
    l4jIniFileName := Copy ( ParamStr(0)
                           , 1
                           , Length (ParamStr(0)){$ifdef WINDOWS} - 4 {$endif}
                           )
                     + 'Ini.xml'
                     ;
    if not FileExistsUTF8(l4jIniFileName) { *Converted from FileExists* } then
      raise Exception.Create('could not find inifile: ' + l4jIniFileName);
    with iniXml do
    begin
      LoadFromFile(l4jIniFileName, nil);
      if not (TagName = 'l4jIni') then
        raise Exception.Create(l4jIniFileName + ': Not a valid INI Xmlfile');
      l4jDbName := ExpandRelativeFileName ( ParamStr(0)
                                                  , Items.XmlValueByTag ['licenseDatabase']
                                                  );
      l4jXsdFileName := ExpandRelativeFileName ( ParamStr(0)
                                                       , Items.XmlValueByTag ['xmlExplorerXsd']
                                                       );
      xXml := Items.XmlItemByTag['logTypes'];
      if not Assigned (xXml) then
        raise Exception.Create('Could not find logTypes in iniFile');
      for x := 0 to xXml.Items.Count - 1 do
      begin
        with xXml.Items.XmlItems[x] do
        begin
          xLogType := TLogType.Create;
          xLogType.Name := Attributes.ValueByTag['name'];
          xLogType.sTag := Items.XmlValueByTag['sTag'];
          xLogType.eTag := Items.XmlValueByTag['eTag'];
          xLogType.eyeCatchers := Items.XmlItemByTag['eyeCatchers'];
          LogTypes.AddObject(xLogType.Name, xLogType);
        end;
      end;
    end;

    if FileExistsUTF8(l4jXsdFileName) { *Converted from FileExists* } then
    begin
      l4jXsdDescr := TXsdDescr.Create;
      try
        l4jXsdDescr.LoadXsdFromFile (l4jXsdFileName, nil);
        if l4jXsdDescr.TypeDef.ElementDefs.Count = 1 then
        begin
          with l4jXsdDescr.TypeDef.ElementDefs.Xsds[0].sType.ElementDefs do
          begin
            DisplayedColumnsXsd := XsdByName ['DisplayedColumns'];
            configXsd := XsdByName['xmlExplorerConfig'];
          end;
        end;
      except
        ShowMessage ('Could not parse ' + l4jXsdFileName);
      end;
    end;

    if not Assigned (DisplayedColumnsXsd) then
      raise Exception.Create('Description for DisplayedColumns not found');
    DisplayedColumnsXml := TXml.Create(0, DisplayedColumnsXsd);
    DisplayedColumnsXml.LoadFromString(aIniFile.StringByName['DisplayedColumns'], nil);
    readDisplayedColumnsXml := TXml.Create;

    if FileExistsUTF8(l4jDbName) { *Converted from FileExists* } then
    begin
      _xmlLicensed := True;
      { TODO : logusage }
//      LogUsage(l4jDbName);
    end;
    configFileName := aIniFile.StringByNameDef  ['configFileName', ''];
  //DragAcceptFiles(Self.Handle, True);
    _OnParseErrorEvent := @ParserError;
    if configFileName <> '' then
      readConfig (configFileName);
    AdjustDisplayedColumns(DisplayedColumnsXml);
  finally
    if not _xmlLicensed then
      ShowMessage ( 'Since you are not a licensed user,'
                  + #$D#$A
                  + 'l4j will only show the first part of values.'
                  );
  end;
end;

procedure TL4JMainForm.ParserError(Sender: TObject; LineNumber, ColumnNumber,
  Offset: Integer; TokenString, Data: String);
begin
  Application.CreateForm(TErrorFoundDlg, ErrorFoundDlg);
  try
    ErrorFoundDlg.FileNameEdit.Text := _ParseFileName;
    ErrorFoundDlg.LineNumberEdit.Text := IntToStr (LineNumber);
    ErrorFoundDlg.ColumnNumberEdit.Text := IntToStr (ColumnNumber);
    ErrorFoundDlg.TokenStringEdit.Text := TokenString;
    ErrorFoundDlg.Viewer := 'Notepad';
    ErrorFoundDlg.ShowModal;
  finally
    FreeAndnil (ErrorFoundDlg);
  end;
end;

procedure TL4JMainForm.saveConfig(aFileName: String);
begin

end;

procedure TL4JMainForm.readConfig(aFileName: String);
begin

end;

function TL4JMainForm.configAsXml: TXml;
begin

end;

procedure TL4JMainForm.configFromXml(aXml: TXml);
begin

end;

procedure TL4JMainForm.OnlyWhenLicensed;
begin

end;

function TL4JMainForm.OkToOpenCase: Boolean;
begin

end;

procedure TL4JMainForm.FormCreate(Sender: TObject);
var
  xXml: TXml;
  xIniFile: TFormIniFile;
begin
  xIniFile := TFormIniFile.Create (Self, True);
  try
    xIniFile.Restore;
    TreeView.NodeDataSize := SizeOf (TTreeRec);
    TreeView.RootNodeCount := 0;
  //CloseAction.ShortCut := VK_ESCAPE;
    isChanged := False;
    DisplayedColumns := TStringList.Create;
    ColumnWidths := TStringList.Create;
    ColumnWidths.Text := xIniFile.StringByName ['ColumnWidths'];
    SaveLog4JFileName := xIniFile.StringByNamedef ['SaveLog4JFileName', ''];
    ReadLog4JFileName := xIniFile.StringByNamedef ['ReadLog4JFileName', ''];
    ShowDetailed := xIniFile.StringByNamedef ['ShowDetailed', ''];
    doWrapText := xIniFile.BooleanByNamedef ['doWrapText', False];
    WraptekstMenuItemClick (nil);
    { TODO : filterdialog }
  //FilterDlg.Caption := 'Configure filter';
    Data := TStringList.Create;
  {
    StatusBar.Visible := False;
    ProgressBar.Visible := False;
  }
    logTypes := TLogTypes.Create;
    iniXml := TXml.Create;
    readDisplayedColumnsXml := TXml.Create;
    l4jInit(xIniFile);
    dbs := SQLConnector1;
    qry := SqlQuery;
    xqry := xSqlQuery;
    systemStarting:=False;
  finally
    xIniFile.Free;
  end;
end;

procedure TL4JMainForm.XmlTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData (Node);
  CellText := GetCellText (Data, xData^.Index, Column);
end;

procedure TL4JMainForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  with TFormIniFile.Create (Self, False) do
  try
    TreeView.Clear;
    for x := 0 to DisplayedColumns.Count - 1 do
      DisplayedColumns.Objects [x].Free;
    DisplayedColumns.Free;
    for x := TreeView.Header.Columns.Count - 1  downto 0 do
      ColumnWidths.Values [TreeView.Header.Columns.Items[x].Text]
      := IntToStr (TreeView.Header.Columns.Items[x].Width);
    StringByName ['SaveLog4JFileName'] := SaveLog4JFileName;
    StringByName ['ReadLog4JFileName'] := ReadLog4JFileName;
    StringByName  ['configFileName'] := configFileName;
    StringByName ['ShowDetailed'] := ShowDetailed;
    BooleanByName ['doWrapText'] := doWrapText;
    StringByName ['ColumnWidths'] := ColumnWidths.Text;
    StringByName ['DisplayedColumns'] := DisplayedColumnsXml.Text;
    Save;
    ColumnWidths.Free;
    Data.Clear;
    Data.Free;
    for x := 0 to logTypes.Count - 1 do
      logTypes.Types[x].Free;
    logTypes.Free;
    iniXml.Free;
    DisplayedColumnsXml.Free;
    readDisplayedColumnsXml.Free;
    l4jXsdDescr.Free;
  finally
    Free;
  end;
end;

procedure TL4JMainForm.CopyActionExecute(Sender: TObject);
{
  function _Columns (aNode: PVirtualNode): String;
  var
    col: Integer;
    xText: String;
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
  s, xSep: String;
  c: Integer;
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    s := '';
    xSep := '';
    for c := 0 to DisplayedColumns.Count - 1 do
//    if ColumnVisible [c] then
      begin
        s := s + xSep + '"' + (DisplayedColumns.Objects[c] as TDisplayedColumn).Header + '"';
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
        for c := 0 to DisplayedColumns.Count - 1 do
        begin
          s := s + xSep  + '"' + GetCellText (Data, xData^.Index, c) + '"';
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

procedure TL4JMainForm.ActionList1Update(AAction: TBasicAction;
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
  NodeFullExpandAction.Enabled := Selected and (theNode^.ChildCount > 0);

  FullCollapseAction.Enabled := (TreeView.RootNodeCount > 0);
  NodeFullCollapseAction.Enabled := Selected and (theNode^.ChildCount > 0);

  Handled := True;
end;

procedure TL4JMainForm.FindActionExecute(Sender: TObject);
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
        if not xmlUtil.SearchUseRegExp then
        begin
          if xmlUtil.SearchString [1] <> '*' then
            xmlUtil.SearchString := '*' + xmlUtil.SearchString;
          if (system.Length(xmlUtil.SearchString) > 1)
          and (xmlUtil.SearchString [system.Length(xmlUtil.SearchString)] <> '*') then
            xmlUtil.SearchString := xmlUtil.SearchString + '*';
        end;
        Found := False;
        if xmlUtil.SearchScope = 0 then // Search from next object
          CurItem := TreeView.GetNext (TreeView.FocusedNode)
        else
          CurItem := nil;
        if (CurItem = nil) then // either because users choice or there is no next
          CurItem := TreeView.GetFirstVisible; // search from begin
        while not (CurItem = nil)
        and not Found do
        begin
          xData := TreeView.GetNodeData (CurItem);
          Found := StringMatchesMask ( Data.Strings [xData^.Index]
                                     , xmlUtil.SearchString
                                     , False
                                     , xmlUtil.SearchUseRegExp
                                     );
          if not Found then
            CurItem := TreeView.GetNextVisible (CurItem);
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

procedure TL4JMainForm.FindNextActionExecute(Sender: TObject);
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
      CurNode := TreeView.GetNextVisible (TreeView.FocusedNode);
      while not (CurNode = nil)
      and not Found do
      begin
        xData := TreeView.GetNodeData (CurNode);
        Found := StringMatchesMask ( Data.Strings [xData^.Index]
                                   , xmlUtil.SearchString
                                   , False
                                   , xmlUtil.SearchUseRegExp
                                   );
        if not Found then
          CurNode := TreeView.GetNextVisible (CurNode);
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

procedure TL4JMainForm.CompareNodesMenuItemClick(Sender: TObject);
var
  fNode, nNode: PVirtualNode;
  fData, nData: PTreeRec;
  fString, nString: String;
  fXml, nXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  nNode := nil;
  fNode := TreeView.GetFirstSelected;
  nNode := TreeView.GetNextSelected(fNode);
  if Assigned (fNode)
  and Assigned (nNode) then
  begin
    fData := TreeView.GetNodeData (fNode);
    fString := prepareDataToZoom(Data.Strings [fData^.Index]);
    fXml := TXml.CreateAsString('l4j', '');
    with fXml.AddXml (TXml.Create) do
      LoadFromString(fString, nil);
    nData := TreeView.GetNodeData (nNode);
    nString := prepareDataToZoom(Data.Strings [nData^.Index]);
    nXml := TXml.CreateAsString('l4j', '');
    with nXml.AddXml (TXml.Create) do
      LoadFromString(nString, nil);
    a2bInitialize;
    try
      xA2B := TA2BXml.CreateA2B('', '', fXml, nXml, Nil, Nil);
    finally
      a2bUninitialize;
    end;
    try
      Application.CreateForm(TShowA2BXmlForm, xForm);
      with xForm do
      try
        Caption := 'Diffrences in messages';
        ColumnHeaderA := 'Value first selected';
        ColumnHeaderB := 'Value next selected';
        Xml := xA2B;
        ShowModal;
      finally
        FreeAndNil(xForm);
      end;
    finally
      FreeAndNil(xA2B);
      FreeAndNil(fXml);
      FreeAndNil(nXml);
    end;
  end;
end;

procedure TL4JMainForm .PasteFromClipboardActionExecute (Sender : TObject );
var
  xString: AnsiString;
begin
  xString := Clipboard.AsText;
  if xString = '' then
  begin
    ShowMessage('Clipboard does not contain text');
    Exit;
  end;
  Memo.Clear;
  Data.Clear;
  TreeView.Clear;
  Treeview.Header.SortColumn := 0;
  TreeView.Header.SortDirection := sdAscending;
  numberVisible := 0;
  Thread := TStringThread.Create ( Self
                                 , xString
                                 );
end;

procedure TL4JMainForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
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
    Brush.Color := _decColor(clWhite);
    FillRect(CellRect);
  end;
end;

procedure TL4JMainForm.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

end;

procedure TL4JMainForm.TvPopupMenuPopup(Sender: TObject);
var
  n: Integer;
begin
  n := TreeView.SelectedCount;
  CompareNodesMenuItem.Enabled := (n = 2);
end;

procedure TL4JMainForm.TreeViewPopupMenuPopup(Sender: TObject);
begin
end;

procedure TL4JMainForm.UseReadDisplayedColumns;
begin
  DisplayedColumnsXml.Free;
  DisplayedColumnsXml := TXml.Create (0, DisplayedColumnsXsd);
  DisplayedColumnsXml.LoadValues(readDisplayedColumnsXml, False, True);
  DisplayedColumnsChanged := True;
  AdjustDisplayedColumns(DisplayedColumnsXml);
end;

procedure TL4JMainForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure TL4JMainForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TL4JMainForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  s: String;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Sender.Selected [Sender.FocusedNode] := True;
    s := '';
    if Assigned (Node) then
    begin
      xData := TreeView.GetNodeData (Node);
      s := GetAtt (ShowDetailed, Data.Strings[xData^.Index]);
    end;
    Memo.Text := s;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TL4JMainForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TL4JMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

function TL4JMainForm.findLogType(aString: AnsiString): TLogType;
var
  x: Integer;
  str: AnsiString;
  len: Integer;
begin
  len := 10000; // I assume logType can be determined on first 10000 chars
  result := nil;
  str := Copy (aString, 1, len);
  for x := 0 to logTypes.Count - 1 do
  begin
    if Assigned ( strUtils.SearchBuf( @str[1]
                                    , Length (Str)
                                    , 0
                                    , 0
                                    , logTypes.Types[x].sTag
                                    , [soDown, soMatchCase]
                                    )
                ) then
    begin
      result := logTypes.Types[x];
      exit;
    end;
  end;
end;

function TL4JMainForm.prepareDataToZoom(s: String): String;
var
  x: Integer;
  xLogType: TLogType;
  sp, ep, xp: PChar;
  sEyeCatcher, eEyeCatcher: TXml;
begin
  result := s;
  xLogType := findLogType(s);
  if Assigned (xLogType)
  and Assigned(xLogType.eyeCatchers)
  and (xLogType.eyeCatchers.Items.Count > 0) then
  begin
    result := '';
    sp := @s[1];
    xp := strUtils.SearchBuf(sp, Length(s), 0, 0, xLogType.eTag, [soDown, soMatchCase]);
    x := 0;
    sEyeCatcher := xLogType.eyeCatchers.Items.XmlItems[x];
    ep := strUtils.SearchBuf(sp, xp - sp, 0, 0, sEyeCatcher.Value, [soDown, soMatchCase]);
    if not Assigned (ep) then
      ep := xp;
    result := Copy (s, 1, ep - @s[1]);
    Inc (x);
    sp := ep;
    while (sp < xp)
    and (x < xLogType.eyeCatchers.Items.Count) do
    begin
      eEyeCatcher := xLogType.eyeCatchers.Items.XmlItems[x];
      ep := strUtils.SearchBuf(sp, xp - sp, 0, 0, eEyeCatcher.Value, [soDown, soMatchCase]);
      if not Assigned (ep) then
        ep := xp;
      result := result
              + '<'
              + sEyeCatcher.Attributes.ValueByTag['label']
              + '>'
              + Copy ( s
                     , sp - @s[1] + 1 + Length (sEyeCatcher.Value)
                     , ep - sp -  Length (sEyeCatcher.Value)
                     )
              + '</'
              + sEyeCatcher.Attributes.ValueByTag['label']
              + '>'
              ;
      sEyeCatcher := eEyeCatcher;
      sp := ep;
      Inc (x);
    end;
    result := result
            + '<'
            + sEyeCatcher.Attributes.ValueByTag['label']
            + '>'
            + Copy ( s
                   , sp - @s[1] + 1 + Length (sEyeCatcher.Value)
                   , xp - sp - Length (sEyeCatcher.Value)
                   )
            + '</'
            + sEyeCatcher.Attributes.ValueByTag['label']
            + '>'
            + xLogType.eTag;
            ;
  end;
  for x := 0 to DisplayedColumns.Count - 1 do
    with DisplayedColumns.Objects [x] as TDisplayedColumn do
      if ColumnType = ctHasXmlValue then
        result := ReplaceStrings ( result
                                 , '>' + Key + '<'
                                 , '>' + Header + ':' + Key + '<'
                                 , True
                                 , False
                                 );
end;

function TL4JMainForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

function TL4JMainForm.getDoWrapText: Boolean;
begin
  result := WraptekstMenuItem.Checked;
end;

procedure TL4JMainForm.setDoWrapText(AValue: Boolean);
begin
  WraptekstMenuItem.Checked := AValue;
end;

procedure TL4JMainForm.setReadOnly(AValue: Boolean);
begin
  fReadOnly := AValue;
  TreeView.ParentColor := AValue;
  if not AValue then
  begin
    TreeView.Color := clWindow;
    TreeView.Colors.GridLineColor := clBtnFace;
  end
  else
    TreeView.Colors.GridLineColor := clBtnHighlight;
end;

procedure TL4JMainForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure TL4JMainForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
end;

procedure TL4JMainForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData (Node);
  CellText := GetCellText (Data, xData^.Index, Column);
end;

procedure TL4JMainForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
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

procedure TL4JMainForm.TreeViewClick(Sender: TObject);
begin
  begin
{
    if (Assigned((Sender as TVirtualStringTree).FocusedNode))
    and ((Sender as TVirtualStringTree).FocusedColumn = treeValueColumn) then
      (Sender as TVirtualStringTree).EditNode
        ( (Sender as TVirtualStringTree).FocusedNode
        , (Sender as TVirtualStringTree).FocusedColumn
        );
}
  end;
end;

procedure TL4JMainForm.ZoomMenuItemClick(Sender: TObject);
var
  xData: PTreeRec;
begin
  if Assigned (TreeView.FocusedNode) then
  begin
    xData := TreeView.GetNodeData (TreeView.FocusedNode);
    xmlUtil.presentString ('', Data.Strings[xData^.Index]);
  end;
end;

procedure TL4JMainForm.FormShow(Sender: TObject);
var
  x: Integer;
begin
  if not Assigned (Data) then
    raise Exception.Create ('arg Data not assigned');
  AdjustDisplayedColumns(DisplayedColumnsXml);
  TreeView.RootNodeCount := Data.Count;
  TreeView.Header.SortColumn := 0;
end;

procedure TL4JMainForm.FilterActionExecute(Sender: TObject);
var
  xNode: PVirtualNode;
  xPasses: Boolean;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  Application.CreateForm(TFilterDlg, FilterDlg);
  try
    FilterDlg.ShowModal;
    if FilterDlg.ModalResult = mrOk then
    begin
      xCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        numberVisible := 0;
        Memo.Clear;
        xNode := TreeView.GetFirst;
        while Assigned (xNode) do
        begin
          xData := TreeView.GetNodeData (xNode);
          xPasses := Filter (Data, xData^.Index);
          TreeView.IsVisible [xNode] := xPasses;
          if xPasses then
            Inc (numberVisible);
          xNode := TreeView.GetNext (xNode);
        end;
      finally
        Screen.Cursor := xCursor;
      end;
    end;
  finally
    FreeAndNil(FilterDlg);
  end;
end;

procedure TL4JMainForm.ZoomMenuActionClick(Sender: TObject);
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
      xmlUtil.PresentString ( 'String ' + IntToStr (xData^.Index)
                            , prepareDataToZoom(Data.Strings [xData^.Index])
                            );
    end
    else
    begin
      if TreeView.SelectedCount > 5000 then
        raise Exception.Create ( 'More lines ('
                               + IntToStr (TreeView.SelectedCount)
                               + ') selected then allowed (5000)'
                               );
      xNode := TreeView.GetFirstSelected;
      s := '<xmlContainer>';
      while Assigned (xNode) do
      begin
        xData := TreeView.GetNodeData (xNode);
        s := s + prepareDataToZoom(Data.Strings [xData^.Index]);
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

procedure TL4JMainForm.TreeViewCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  s1, s2: String;
  d1, d2: PTreeRec;
begin
  Result := 0;
  d1 := TreeView.GetNodeData (Node1);
  d2 := TreeView.GetNodeData (Node2);
  if Column = treeIndexColumn then
  begin
    Result := d1^.Index - d2^.Index;
    Exit;
  end;
  if Column = treeSizeColumn then
  begin
    Result := Length (Data.Strings[d1^.Index]) - Length (Data.Strings[d2^.Index]);
    Exit;
  end;
  s1 := GetCellText (Data, d1^.Index, Column);
  s2 := GetCellText (Data, d2^.Index, Column);
  if  s1 < s2 then
    result := -1;
  if s1 > s2 then
    result := 1;
end;

procedure TL4JMainForm.TreeViewDblClick(Sender: TObject);
begin
  ZoomMenuActionClick(nil);
end;

procedure TL4JMainForm.EditDisplayedColumnsActionExecute(Sender: TObject);
var
  nXml: TXml;
  xForm: TXmlGridForm;
begin
  if Assigned (DisplayedColumnsXsd) then
  begin
    nXml := TXml.Create (0, DisplayedColumnsXsd);
    try
      nXml.LoadValues(DisplayedColumnsXml, False, True);
      Application.CreateForm(TXmlGridForm, xForm);
      try
        xForm.Caption := 'l4j DisplayedColumns ';
        xForm.Xml := nXml;
        xForm.isReadOnly := False;
        xForm.ShowModal;
        if (xForm.modalResult = mrOk)
        then
        begin
          DisplayedColumnsXml.Free;
          DisplayedColumnsXml := TXml.Create (0, DisplayedColumnsXsd);
          DisplayedColumnsXml.LoadValues(nXml, False, True);
          DisplayedColumnsChanged := True;
          AdjustDisplayedColumns(DisplayedColumnsXml);
        end;
      finally
        FreeAndNil (xForm);
      end;
    finally
      nXml.Free;
    end;
  end;
end;

procedure TL4JMainForm.QueryDbActionExecute(Sender: TObject);
begin
  Application.CreateForm(TDbFilterDlg, DbFilterDlg);
  try
    DbFilterDlg.ShowModal;
    if DbFilterDlg.ModalResult = mrOk then
    begin
      TreeView.Clear;
      Treeview.Header.SortColumn := 0;
      TreeView.Header.SortDirection := sdAscending;
      Memo.Clear;
      Data.Clear;
      numberVisible := 0;
      Thread := TQueryThread.Create ( Self
                                    , DbFilterDlg.ConnStringEdit.Text
                                    , DbFilterDlg.QueryEdit.Text
                                    , DbFilterDlg.ParamEdit1.Text
                                    , DbFilterDlg.ParamEdit2.Text
                                    , DbFilterDlg.ParamEdit3.Text
                                    , DbFilterDlg.ParamEdit4.Text
                                    );
    end;
  finally
    FreeAndNil(DbFilterDlg);
  end;
end;

procedure TL4JMainForm.TreeViewHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
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

procedure TL4JMainForm.TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData(Node);
  xData^.Index := Node^.Index;
end;

procedure TL4JMainForm.SaveAccordingSelected(aSelectedOnly: Boolean);
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
        s := '<xmlContainer>' + DisplayedColumnsXml.AsText (False, -1, True, False) + LineEnding;
        xNode := TreeView.GetFirstVisible;
        while Assigned (xNode) do
        begin
          if (not aSelectedOnly)
          or (TreeView.Selected[xNode]) then
          begin
            xData := TreeView.GetNodeData (xNode);
            s := s + Data.Strings [xData^.Index] + LineEnding;
          end;
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


procedure TL4JMainForm.WriteXmlActionExecute(Sender: TObject);
begin
  SaveAccordingSelected(False);
end;

procedure TL4JMainForm.Save1Click(Sender: TObject);
begin
  SaveAccordingSelected(True);
end;

procedure TL4JMainForm.WraptekstMenuItemClick(Sender: TObject);
begin
  if DoWrapText then
    Memo.ScrollBars := ssVertical
  else
    Memo.ScrollBars := ssBoth;
end;

procedure TL4JMainForm.ElementAttribute1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

end;

procedure TL4JMainForm.Editconfig1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.Saveconfigas1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.Saveconfig1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.Openconfig1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.About1Click(Sender: TObject);
var
  xForm: TAboutBox;
begin
  Application.CreateForm(TAboutBox, xForm);
  try
    xForm.NameLabel.Caption := 'l4j - LogEvents explorer';
    xForm.ShowModal;
  finally
    FreeAndNil (xForm);
  end;
end;


procedure TL4JMainForm.License1Click(Sender: TObject);
begin

end;

procedure TL4JMainForm.AllXmlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin

end;

procedure TL4JMainForm.AllXmlActionUpdate(Sender: TObject);
begin

end;

procedure TL4JMainForm.AllXmlActionExecute(Sender: TObject);
var
  s: String;
  xNode: PVirtualNode;
  xCursor: TCursor;
  xData: PTreeRec;
begin
  if numberVisible >= 5000 then
    raise Exception.Create ('Disabled because number of events exceeds supported number (max 5000)');
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    s := '<xmlContainer>';
    xNode := TreeView.GetFirstVisible;
    while Assigned (xNode) do
    begin
      xData := TreeView.GetNodeData (xNode);
      s := s + Data.Strings[xData^.Index];
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

procedure TL4JMainForm.AbortActionExecute(Sender: TObject);
begin
  if Assigned (Thread) then
  begin
    Thread.abortPressed := True;
    AbortAction.Enabled := False;
  end;
end;

procedure TL4JMainForm.OpenFileActionExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := ReadLog4JFileName;
    Options := Options + [ofAllowMultiSelect];
    if not Execute then
      Exit;
    if Files.Count > 0 then
      ReadLog4JFileName := Files.Strings[0];
    Application.CreateForm(TFilterDlg, FilterDlg);
    try
      FilterDlg.ShowModal;
      if FilterDlg.ModalResult <> mrOk then
        Exit;
      Memo.Clear;
      Data.Clear;
      TreeView.Clear;
      Treeview.Header.SortColumn := 0;
      TreeView.Header.SortDirection := sdAscending;
      numberVisible := 0;
      Thread := TSearchThread.Create ( Self
                                     , Files
                                     , FilterDlg.FindEdit0.Text
                                     , FilterDlg.FindEdit1.Text
                                     , FilterDlg.FindEdit2.Text
                                     , FilterDlg.FindEdit3.Text
                                     , FilterDlg.HasNotCheckBox1.Checked
                                     , FilterDlg.HasNotCheckBox2.Checked
                                     , FilterDlg.HasNotCheckBox3.Checked
                                     );
      if Files.Count > 0 then
        ReadLog4JFileName := Files.Strings[0];
    finally
      FreeAndNil (FilterDlg);
    end;
  finally
    Free;
  end;
end;

end.

