//  Search for ZoomElement, it is available
unit WsdlStubMainUnit;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// 20111017 Under Subversion
{ TODO : Zorgen dat het ook werkt bij andere shortdate formats (windows) }
interface

uses
{$IFnDEF FPC}
  AdoDb, Windows,
{$ELSE}
  sqldb , odbcconn, LCLIntf, LCLType,
{$ENDIF}
  Messages
   , SysUtils
   , Classes
   , Graphics
   , Controls
   , Forms
   , Dialogs
   , ActnList
   , Logz
   , snapshotz
   , ExceptionLogz
   , Ipmz
   , IpmTypes
   , WsdlProjectz
   , wsdlcontrolz
   , Wsdlz
   , Xmlz
   , Xsdz
   , StdCtrls
   , IdSync
   , IdUri
   , ComCtrls
   , ExtCtrls
   , FormIniFilez
   , Menus , PairSplitter
   , VirtualTrees
   , LazFileUtils
   , FileUtil
   , IpHtml
   , Bind
   , mqInterface
   , MQAPI
   , SwiftUnit
   , ParserClasses
   , types
   , ClaimListz
   , tacoInterface
   ;

type
  THackControl = class(TWinControl)
  public
    FHandle: HWnd;
    FParentWindow: HWnd;
  end;

  TShowPanel = (spDocumentation, spNotifications, spSnapshots, spMessages);
  TShowLogData = (slRequestHeaders, slRequestBody, slReplyHeaders, slReplyBody, slException, slValidation);
  TLogPanelIndex = (lpiFocus, lpiThreads);
  TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
  TProcedure = procedure of Object;
  TProcedureString = procedure(arg: String) of Object;
  TProcedureOperation = procedure(arg: TWsdlOperation) of Object;

  { TMainForm }

  TMainForm = class(TForm)
    AbortMenuItem : TMenuItem ;
    AbortAction : TAction ;
    Action2 : TAction ;
    MenuItem36 : TMenuItem ;
    XmlSampleOperationsMenuItem : TMenuItem ;
    XmlSampleOperations : TAction ;
    EditMessageAfterScriptAction : TAction ;
    EditMessageScriptAction : TAction ;
    DocumentationViewer: TIpHtmlPanel;
    OperationDocumentationViewer : TIpHtmlPanel ;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35 : TMenuItem ;
    EditMessageScriptMenuItem : TMenuItem ;
    MenuItem38 : TMenuItem ;
    SnapshotsFromFolderAction : TAction ;
    ShowResolvedProperties : TAction ;
    BrowseMqButton: TToolButton;
    DocumentationMemo: TMemo;
    ExceptionMemo: TMemo;
    ExceptionStatusBar: TStatusBar;
    ExceptionsVTS: TVirtualStringTree;
    logChartToolButton: TToolButton;
    LogMemo: TMemo;
    LogTabControl : TTabControl ;
    MenuItem27 : TMenuItem ;
    LogPanel: TPanel;
    MessagesStatusBar: TStatusBar;
    MessagesTabControl: TTabControl;
    MessagesVTS: TVirtualStringTree;
    Panel1: TPanel;
    MessagesPanel: TPanel;
    Panel2: TPanel;
    SnapshotsPanel: TPanel;
    DocumentationPanel: TPanel;
    NotificationsPanel: TPanel;
    Panel8: TPanel;
    Properties : TAction ;
    SnapshotsVTS: TVirtualStringTree;
    Splitter7: TSplitter;
    Splitter8: TSplitter;
    SummaryReportAction : TAction ;
    MenuItem26 : TMenuItem ;
    ShowSnapshotDifferencesAction : TAction ;
    LoadRefSavepointMessagesMenuItem : TMenuItem ;
    LoadSavepointMessagesMenuItem : TMenuItem ;
    MenuItem30 : TMenuItem ;
    MenuItem31 : TMenuItem ;
    ShowGridDifferencesAction : TAction ;
    MenuItem28 : TMenuItem ;
    MenuItem29 : TMenuItem ;
    ShowLogDifferencesAction : TAction ;
    MenuItem23 : TMenuItem ;
    MenuItem24: TMenuItem;
    ShowSavepointDetailsMenuitem : TMenuItem ;
    SnapshotCompareMenuitem: TMenuItem;
    PromoteToReferenceMenuItem : TMenuItem ;
    MenuItem25 : TMenuItem ;
    SnapshotsPopupMenu : TPopupMenu ;
    PingPongTimer : TTimer ;
    ToolBar1: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar6: TToolBar;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton42: TToolButton;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    ToolButton53: TToolButton;
    ToolButton59: TToolButton;
    ToolButton64: TToolButton;
    ToolButton65 : TToolButton ;
    ToolButton66 : TToolButton ;
    ToolButton67: TToolButton;
    ToolButton68: TToolButton;
    ToolButton69: TToolButton;
    SaveSnapshotsAction : TAction ;
    ReadSnapshotInformationAction : TAction ;
    ReportOnSnapshotsAction : TAction ;
    ClearSnapshotsAction : TAction ;
    ImportProjectScriptsAction : TAction ;
    MenuItem20 : TMenuItem ;
    MenuItem21 : TMenuItem ;
    MenuItem22 : TMenuItem ;
    MenuItem4 : TMenuItem ;
    ExportProjectScriptsAction : TAction ;
    MenuItem1 : TMenuItem ;
    MessagesStatusBar1 : TStatusBar ;
    ThreadsPanel : TPanel ;
    ShowShortCutActionsAction : TAction ;
    EditScriptMenuItem : TMenuItem ;
    SchemasToZip : TAction ;
    ExecuteAllRequestsToolButton : TToolButton ;
    ExecuteLoadTextToolbutton : TToolButton ;
    ExecuteRequestToolButton : TToolButton ;
    FreeFormatMemo : TMemo ;
    GridToolBar : TToolBar ;
    GridView : TVirtualStringTree ;
    InWsdlTreeView : TVirtualStringTree ;
    LastToolButton : TToolButton ;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    DesignPanelSplitVerticalMenuItem : TMenuItem ;
    MenuItem13 : TMenuItem ;
    MenuItem14 : TMenuItem ;
    MenuItem15 : TMenuItem ;
    MenuItem16 : TMenuItem ;
    MenuItem17 : TMenuItem ;
    MenuItem18 : TMenuItem ;
    MenuItem19 : TMenuItem ;
    UnhideOperationMenuItem : TMenuItem ;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OperationAliasAction : TAction ;
    logChartAction : TAction ;
    LoadTestAction : TAction ;
    CopyLogGridToClipBoardAction : TAction ;
    MasterClearLogAction : TAction ;
    MasterReactivateActon : TAction ;
    MasterReloadDesignAction : TAction ;
    MasterReloadDesignMenuItem : TMenuItem ;
    MasterRestartAction : TAction ;
    MasterRestartAction1 : TMenuItem ;
    OperationLabel : TLabel ;
    OperationsPopupMenu: TPopupMenu;
    PairSplitter1 : TPairSplitter ;
    PairSplitterSide1 : TPairSplitterSide ;
    PairSplitterSide2 : TPairSplitterSide ;
    MiddlePanel : TPanel ;
    GridDataPanel : TPanel ;
    DataPanel : TPanel ;
    TreeDataPanel : TPanel ;
    ProgressBar : TProgressBar ;
    Reactivatemaster1 : TMenuItem ;
    RunMenuItem : TMenuItem ;
    MenuItem2 : TMenuItem ;
    MenuItem3 : TMenuItem ;
    ServiceLabel : TLabel ;
    DataPanelSplitter : TSplitter ;
    startStopMenuItem : TMenuItem ;
    ProjectDesignToClipboardAction: TAction;
    PresentLogMemoTextAction : TAction ;
    DesignPanel: TPanel;
    alGeneral: TActionList;
    ScriptPanel: TPanel;
    ScriptSplitter: TSplitter;
    Splitter1 : TSplitter ;
    SQLConnector : TSQLConnector ;
    SQLTransaction : TSQLTransaction ;
    StatusPanel : TPanel ;
    ToolBar9 : TToolBar ;
    ToolButton10 : TToolButton ;
    ToolButton11 : TToolButton ;
    ToolButton12 : TToolButton ;
    ToolButton13 : TToolButton ;
    ToolButton14 : TToolButton ;
    ToolButton16 : TToolButton ;
    ToolButton18 : TToolButton ;
    ToolButton20 : TToolButton ;
    ToolButton25 : TToolButton ;
    ToolButton26 : TToolButton ;
    ToolButton27 : TToolButton ;
    ToolButton28 : TToolButton ;
    ToolButton29 : TToolButton ;
    ToolButton34 : TToolButton ;
    ToolButton38 : TToolButton ;
    ToolButton41 : TToolButton ;
    ToolButton44 : TToolButton ;
    ToolButton45 : TToolButton ;
    ToolButton46 : TToolButton ;
    ToolButton47 : TToolButton ;
    ToolButton54 : TToolButton ;
    ToolButton55 : TToolButton ;
    ToolButton56 : TToolButton ;
    ToolButton63 : TToolButton ;
    ToolButton7 : TToolButton ;
    ToolButton8 : TToolButton ;
    ToolButton9 : TToolButton ;
    View : TLabel ;
    ViewStyleComboBox : TComboBox ;
    WsdlComboBox : TComboBox ;
    WsdlInfoPanel : TPanel ;
    WsdlLabel : TLabel ;
    WsdlOperationsComboBox : TComboBox ;
    WsdlServicesComboBox : TComboBox ;
    XsdPanel: TPanel;
    MainToolBar: TToolBar;
    mainImageList: TImageList;
    OpenWsdlAction: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    OpenFileDialog: TOpenDialog;
    MainMenu1: TMainMenu;
    xsdSplitter: TSplitter;
    File1: TMenuItem;
    Exit1: TMenuItem;
    OpenWSDLfile1: TMenuItem;
    N1: TMenuItem;
    TreeviewImageList: TImageList;
    InWsdlPropertiesListView: TListView;
    Splitter4: TSplitter;
    WsdlPopupMenu: TPopupMenu;
    WsdlItemAddMenuItem: TMenuItem;
    N2: TMenuItem;
    Copytoclipboard1: TMenuItem;
    WsdlPasteFromClipboardMenuItem: TMenuItem;
    N3: TMenuItem;
    FullExpand1: TMenuItem;
    FullCollapse1: TMenuItem;
    N4: TMenuItem;
    XmlZoomValueAsTextMenuItem: TMenuItem;
    XmlZoomValueAsXMLMenuItem: TMenuItem;
    Expand2: TMenuItem;
    SaveStubCaseAsAction: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    SaveFileDialog: TSaveDialog;
    OpenStubCaseAction: TAction;
    ToolButton5: TToolButton;
    OpenStubCase1: TMenuItem;
    SaveStubCaseas1: TMenuItem;
    N5: TMenuItem;
    ReopenStubCaseAction: TAction;
    ToolButton6: TToolButton;
    ReopenStubcase1: TMenuItem;
    Help1: TMenuItem;
    wsdStubhelp1: TMenuItem;
    LicenseMenuItem: TMenuItem;
    About1: TMenuItem;
    HelpAction: TAction;
    Qry: TSQLQuery;
    runScriptAction: TAction;
    Extra1: TMenuItem;
    Options1: TMenuItem;
    OptionsAction: TAction;
    SelectCorrelationElementAction: TAction;
    AddMessageAction: TAction;
    DeleteMessageAction: TAction;
    MoveUpMessageAction: TAction;
    MoveDownMessageAction: TAction;
    ApplyToAction: TAction;
    SaveStubCaseAction: TAction;
    SaveStubCase1: TMenuItem;
    ActionToolBar: TToolBar;
    ActionComboBox: TComboBox;
    Button1: TButton;
    ToolButton17: TToolButton;
    RedirectAddressButton: TToolButton;
    ToolButton19: TToolButton;
    Splitter6: TSplitter;
    ToolButton21: TToolButton;
    ClearLogItemsAction: TAction;
    NewStubCaseAction: TAction;
    NewStubCase1: TMenuItem;
    N6: TMenuItem;
    ToolButton15: TToolButton;
    ToolButton23: TToolButton;
    ClearExceptionsAction: TAction;
    OperationReqsTreeView: TVirtualStringTree;
    SelectMessageColumnsAction: TAction;
    CopyGridAction: TAction;
    PasteGridAction: TAction;
    CopyLogMemoTextToClipBrdAction: TAction;
    ShowHttpReplyAsXMLAction: TAction;
    ShowHttpRequestAsXMLAction: TAction;
    View1: TMenuItem;
    SchemapropertiesMenuItem: TMenuItem;
    ListofOperationsMenuItem: TMenuItem;
    SaveMessagesAction: TAction;
    WsdlInformationMenuItem: TMenuItem;
    WsdlPopulateMenuItem: TMenuItem;
    ReadMessagesAction: TAction;
    MessagesRegressionAction: TAction;
    CheckGridFieldsAction: TAction;
    DesignPanelAtTopMenuItem: TMenuItem;
    CopyExceptionToClipboardAction: TAction;
    ShowExceptionAsHtmlAction: TAction;
    EditEnvironmentAction: TAction;
    EnvironmentMenuItem: TMenuItem;
    Editvariables1: TMenuItem;
    AddEnvironmentAction: TAction;
    RemoveEnvironmentAction: TAction;
    Addenvironment1: TMenuItem;
    RemoveEnvironmentAction1: TMenuItem;
    WsdlItemDelMenuItem: TMenuItem;
    ExecuteRequestAction: TAction;
    ExecuteAllRequestsAction: TAction;
    LogMenuItem: TMenuItem;
    ReadMessagesAction1: TMenuItem;
    SaveMessagesAction1: TMenuItem;
    Comparewithfile1: TMenuItem;
    N7: TMenuItem;
    ClearExceptionsAction1: TMenuItem;
    MessagesMenuItem: TMenuItem;
    Selectcorrelationelements1: TMenuItem;
    Selectcolumnelements1: TMenuItem;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    Copygrid1: TMenuItem;
    Pastegrid1: TMenuItem;
    Validategriddataagainstschema1: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    FilterLogAction: TAction;
    FilterLogAction1: TMenuItem;
    ToolButton43: TToolButton;
    ValidateRequestsButton: TToolButton;
    ValidateRepliesButton: TToolButton;
    ValidateRequestsAction: TAction;
    ValidateRepliesAction: TAction;
    LogPopupMenu: TPopupMenu;
    RequestMiMAction: TAction;
    RequestMiMAction1: TMenuItem;
    ReplyMiMAction: TAction;
    ChangesduetoAfterscript1: TMenuItem;
    FindAction: TAction;
    FindNextAction: TAction;
    N14: TMenuItem;
    Validate1: TMenuItem;
    CheckTreeAction: TAction;
    Project1: TMenuItem;
    Options2: TMenuItem;
    ProjectOptionsAction: TAction;
    ToolButton48: TToolButton;
    AddChildElementDefMenuItem: TMenuItem;
    All1: TMenuItem;
    Required1: TMenuItem;
    ShowReplyAsXmlGridAction: TAction;
    ShowRequestAsXmlGridAction: TAction;
    ToggleCheckExpectedValuesAction: TAction;
    ToggleBetaModeAction: TAction;
    ShowExpectedXmlAction: TAction;
    Action1: TAction;
    BrowseMqAction: TAction;
    BrowseMqMenuItem: TMenuItem;
    ShowRequestHeaderAsXmlAction: TAction;
    GridPopupMenu: TPopupMenu;
    MessagesToDiskMenuItem: TMenuItem;
    MessagesToDiskAction: TAction;
    Log2DesignAction: TAction;
    Addtodesign1: TMenuItem;
    ViewMssgAsTextAction: TAction;
    CopyCobolDataToClipboardMenuItem: TMenuItem;
    PasteCobolDataFromClipboardMenuItem: TMenuItem;
    ElementvalueMenuItem: TMenuItem;
    AssignExpressionMenuItem: TMenuItem;
    SelectExpectedElementsAction: TAction;
    ToolButton57: TToolButton;
    ReportUnexpectedValuesAction: TAction;
    WsdlItemChangeDataTypeMenuItem: TMenuItem;
    DataTypeDependingMenu: TMenuItem;
    ServiceOptionsAction: TAction;
    ServiceMenu: TMenuItem;
    Options3: TMenuItem;
    LogUsageTimer: TTimer;
    ConfigListenersAction: TAction;
    ToolButton58: TToolButton;
    readLog4jEventsAction: TAction;
    readLog4jEventsAction1: TMenuItem;
    Revalidatemessages1: TMenuItem;
    N15: TMenuItem;
    Reset1: TMenuItem;
    N16: TMenuItem;
    EnableMessageAction: TAction;
    Revalidatemessages2: TMenuItem;
    DisableMessageAction: TAction;
    Disablemessages1: TMenuItem;
    N17: TMenuItem;
    RemoveAllMessagesAction: TAction;
    Removeallmessagesfromalloperations1: TMenuItem;
    ShowLogDetailsAction: TAction;
    ShowLogdetails1: TMenuItem;
    N19: TMenuItem;
    ToolButton60: TToolButton;
    ToolButton61: TToolButton;
    ConfigLogAction: TAction;
    ToolButton62: TToolButton;
    DelayTimeButton: TToolButton;
    ToggleFileLogAction: TAction;
    N20: TMenuItem;
    RedirectAddressAction: TAction;
    OperationDelayResponseTimeAction: TAction;
    OperationApplySettingsAction: TAction;
    OperationWsaAction: TAction;
    ThrowExceptionAction: TAction;
    Configurelisteners1: TMenuItem;
    MessagesFromDiskAction: TAction;
    Readmessagesfromdiskfiles1: TMenuItem;
    LogDisplayedColumnsAction: TAction;
    Displayedcolumns1: TMenuItem;
    startStopButton: TToolButton;
    startAction: TAction;
    stopAction: TAction;
    ScriptsMenuItem: TMenuItem;
    N23: TMenuItem;
    ScriptGoMenuItem: TMenuItem;
    ExtendRecursivityMenuItem: TMenuItem;
    AssignEvaluationMenuItem: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    TestBeforeScriptAction: TAction;
    DebugBeforeScriptAction: TAction;
    GenerateMenuHelpAction: TAction;
    EditBetweenScriptMenuItem: TMenuItem;
    EditBeforeScriptMenuItem: TMenuItem;
    EditAfterScriptMenuItem: TMenuItem;
    CopySwiftdatatoclipboardMenuItem: TMenuItem;
    PasteSwiftdatafromclipboardMenuItem: TMenuItem;
    ToggleNotifyAction: TAction;
    SaveLogRequestsToFileAction: TAction;
    N27: TMenuItem;
    Saverequeststofile1: TMenuItem;
    SaveLogRepliesToFileAction: TAction;
    Saverequeststofile2: TMenuItem;
    ShowRequestAsHtmlAction: TAction;
    ShowReplyAsHtmlAction: TAction;
    ShowExceptAsHtmlAction: TAction;
    OperationOptionsAction: TAction;
    HideAllOperationsAction: TAction;
    N28: TMenuItem;
    UnhideAllOperationsAction: TAction;
    N29: TMenuItem;
    N30: TMenuItem;
    ShowSelectedRequestsAsXmlGridAction: TAction;
    ShowrequestinaGrid2: TMenuItem;
    ShowSelectedResponsesAsXmlGridAction: TAction;
    Showselectedresponsesinagrid1: TMenuItem;
    OperationZoomOnAction: TAction;
    ShowLogZoomElementAction: TAction;
    LogCoverageReportAction: TAction;
    LogCoverageReportAction1: TMenuItem;
    N26: TMenuItem;
    DisplayedcolumnMenuItem: TMenuItem;
    FreeFormatsAction: TAction;
    ViewMssgAsTextAction1: TMenuItem;
    CobolOperationsAction: TAction;
    CobolOperationsAction1: TMenuItem;
    SwiftMtOperationsAction: TAction;
    MaintainlistofSwiftMToperations1: TMenuItem;
    WsdlsPopupMenu: TPopupMenu;
    Maintainlistoffreeformatoperations1: TMenuItem;
    Maintainlistofcoboloperations1: TMenuItem;
    MaintainlistofSwiftMToperations2: TMenuItem;
    MaintainlistofWSDLfiles1: TMenuItem;
    N33: TMenuItem;
    XsdOperationsAction: TAction;
    XsdOperationsAction1: TMenuItem;
    Xsdoperations1: TMenuItem;
    ShowReplyHeaderAsXmlAction: TAction;
    CleanMenuItem: TMenuItem;
    N34: TMenuItem;
    ProjectCleanAction: TAction;
    Clean1: TMenuItem;
    HelpMainMenuAction: TAction;
    HelpMainMenuAction1: TMenuItem;
    N35: TMenuItem;
    GenerateScriptAssignmentAction: TAction;
    ToolButton70: TToolButton;
    AbortToolButton: TToolButton;
    RefreshLogTimer: TTimer;
    ZoomasScriptAssignments1: TMenuItem;
    Generate1: TMenuItem;
    XSDreportinClipBoardSpreadSheet1: TMenuItem;
    SeparatorToolButton: TToolButton;
    procedure EditMessageAfterScriptActionExecute (Sender : TObject );
    procedure EditMessageAfterScriptActionUpdate (Sender : TObject );
    procedure EditMessageScriptActionExecute (Sender : TObject );
    procedure DocumentationViewerHotClick(Sender: TObject);
    procedure ImportProjectScriptsActionHint (var HintStr : string ;
      var CanShow : Boolean );
    procedure LogTabControlChange(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure NeedTacoHostData (Sender: TTacoInterface);
    procedure OnTacoAuthorize (Sender: TObject);
    procedure AbortActionUpdate (Sender : TObject );
    procedure BrowseMqActionHint (var HintStr : string ; var CanShow : Boolean
      );
    procedure ClearSnapshotsActionExecute (Sender : TObject );
    procedure CopyLogGridToClipBoardActionExecute (Sender : TObject );
    procedure DesignPanelSplitVerticalMenuItemClick (Sender : TObject );
    procedure GridPopupMenuPopup (Sender : TObject );
    procedure ImportProjectScriptsActionExecute (Sender : TObject );
    procedure InWsdlTreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
      TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
      const CellRect : TRect );
    procedure LoadTestActionExecute (Sender : TObject );
    procedure LoadTestActionUpdate (Sender : TObject );
    procedure logChartActionExecute (Sender : TObject );
    procedure DesignSplitHorizontalMenuItemClick (Sender : TObject );
    procedure DesignSplitVerticalMenuItemClick (Sender : TObject );
    procedure MenuItem14Click (Sender : TObject );
    procedure MenuItem17Click (Sender : TObject );
    procedure AddChildElementRefMenuItemClick (Sender : TObject );
    procedure OperationReqsTreeViewClick(Sender: TObject);
    procedure OperationReqsTreeViewGetImageIndex (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Kind : TVTImageKind ; Column : TColumnIndex ;
      var Ghosted : Boolean ; var ImageIndex : Integer );
    procedure PingPongTimerTimer (Sender : TObject );
    procedure EditProjectPropertiesExecute (Sender : TObject );
    procedure ShowResolvedPropertiesExecute (Sender : TObject );
    procedure ShowSnapshotDifferencesActionExecute (Sender : TObject );
    procedure SnapshotCompareMenuitemClick(Sender: TObject);
    procedure SnapshotPromoteToReferenceMenuItemClick (Sender : TObject );
    procedure SnapshotsFromFolderActionExecute (Sender : TObject );
    procedure SnapshotsFromFolderActionUpdate (Sender : TObject );
    procedure SnapshotShowDetailsMenuitemClick (Sender : TObject );
    procedure SnapshotLoadRefMessagesMenuItemClick (Sender : TObject );
    procedure SnapshotLoadMessagesMenuItemClick (Sender : TObject );
    procedure MessagesTabControlChange (Sender : TObject );
    procedure MessagesTabControlGetImageIndex (Sender : TObject ;
      TabIndex : Integer ; var ImageIndex : Integer );
    procedure MessagesVTSCompareNodes (Sender : TBaseVirtualTree ; Node1 ,
      Node2 : PVirtualNode ; Column : TColumnIndex ; var Result : Integer );
    procedure VTSHeaderClick (Sender : TVTHeader ;
      Column : TColumnIndex ; Button : TMouseButton ; Shift : TShiftState ; X ,
      Y : Integer );
    procedure OperationAliasActionExecute (Sender : TObject );
    procedure OperationDelayResponseTimeActionExecute(Sender: TObject);
    procedure OperationReqsTreeViewPaintText (Sender : TBaseVirtualTree ;
      const TargetCanvas : TCanvas ; Node : PVirtualNode ;
      Column : TColumnIndex ; TextType : TVSTTextType );
    procedure PresentLogMemoTextActionExecute (Sender : TObject );
    procedure PresentLogMemoTextActionUpdate (Sender : TObject );
    procedure ProjectDesignToClipboardActionExecute(Sender: TObject);
    procedure ExportProjectScriptsActionExecute (Sender : TObject );
    procedure ReadSnapshotInformationActionExecute (Sender : TObject );
    procedure ReportOnSnapshots (aList: TClaimableObjectList);
    procedure ReportOnSnapshotsActionExecute (Sender : TObject );
    procedure SnapshotsPopupMenuPopup (Sender : TObject );
    procedure SnapshotsVTSClick (Sender : TObject );
    procedure SnapshotsVTSCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure SnapshotsVTSGetImageIndex (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Kind : TVTImageKind ; Column : TColumnIndex ;
      var Ghosted : Boolean ; var ImageIndex : Integer );
    procedure SnapshotsVTSGetText (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Column : TColumnIndex ; TextType : TVSTTextType ;
      var CellText : String );
    procedure SummaryReport (aList: TClaimableObjectList);
    procedure SummaryReportActionExecute (Sender : TObject );
    procedure SaveSnapshotsActionExecute (Sender : TObject );
    procedure SchemasToZipExecute (Sender : TObject );
    procedure ShowGridDifferencesActionExecute (Sender : TObject );
    procedure ShowLogDetailsActionExecute(Sender: TObject);
    procedure RemoveAllMessagesActionUpdate(Sender: TObject);
    procedure RemoveAllMessagesActionExecute(Sender: TObject);
    procedure DisableMessageActionExecute(Sender: TObject);
    procedure EnableMessageActionExecute(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure Revalidatemessages1Click(Sender: TObject);
    procedure readLog4jEventsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure readLog4jEventsActionUpdate(Sender: TObject);
    procedure readLog4jEventsActionExecute(Sender: TObject);
    procedure ConfigListenersActionUpdate(Sender: TObject);
    procedure ConfigListenersActionExecute(Sender: TObject);
    procedure LogUsageTimerTimer(Sender: TObject);
    procedure ServiceOptionsActionExecute(Sender: TObject);
    procedure ServiceOptionsActionUpdate(Sender: TObject);
    procedure ChangeXmlDataType(aXml: TXml; aDataType: TXsdDataType);
    procedure ChangeDataTypeMenuItemClick(Sender: TObject);
    procedure ReportUnexpectedValuesActionExecute(Sender: TObject);
    procedure ReportUnexpectedValuesActionUpdate(Sender: TObject);
    procedure SelectExpectedElementsActionUpdate(Sender: TObject);
    procedure SelectExpectedElementsActionExecute(Sender: TObject);
    procedure AssignExpressionMenuItemClick(Sender: TObject);
    procedure ElementvalueMenuItemClick(Sender: TObject);
    procedure PasteCobolDataFromClipboardMenuItemClick(Sender: TObject);
    procedure CopyCobolDataToClipboardMenuItemClick(Sender: TObject);
    procedure ShowLogDifferencesActionExecute (Sender : TObject );
    procedure ShowShortCutActionsActionExecute (Sender : TObject );
    procedure UnhideOperationMenuItemClick (Sender : TObject );
    procedure ViewMssgAsTextActionExecute(Sender: TObject);
    procedure ViewMssgAsTextActionUpdate(Sender: TObject);
    procedure Log2DesignActionExecute(Sender: TObject);
    procedure MessagesToDiskActionExecute(Sender: TObject);
    procedure MessagesToDiskActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BrowseMqActionExecute(Sender: TObject);
    procedure BrowseMqActionUpdate(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure ShowExpectedXmlActionExecute(Sender: TObject);
    procedure ShowRemarksActionExecute(Sender: TObject);
    procedure ToggleBetaModeActionExecute(Sender: TObject);
    procedure ToggleCheckExpectedValuesActionExecute(Sender: TObject);
    procedure ShowRequestAsXmlGridActionExecute(Sender: TObject);
    procedure ShowRequestAsXmlGridActionUpdate(Sender: TObject);
    procedure ShowReplyAsXmlGridActionExecute(Sender: TObject);
    procedure Required1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure AbortActionExecute(Sender: TObject);
    procedure GridToolBarResize(Sender: TObject);
    procedure InWsdlTreeViewFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure ViewStyleComboBoxChange(Sender: TObject);
    function getXmlViewType: TxvViewType;
    procedure AddChildElementTypeDefMenuItemClick(Sender: TObject);
    procedure ProjectOptionsActionExecute(Sender: TObject);
    procedure CheckTreeActionExecute(Sender: TObject);
    procedure CheckTreeActionUpdate(Sender: TObject);
    procedure Validate1Click(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FindNextActionUpdate(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindActionUpdate(Sender: TObject);
    procedure ReplyMiMActionExecute(Sender: TObject);
    procedure RequestMiMActionExecute(Sender: TObject);
    procedure LogPopupMenuPopup(Sender: TObject);
    procedure FilterLogActionExecute(Sender: TObject);
    procedure AfterRequestScriptButtonClick(Sender: TObject);
    procedure ExecuteLoadTest;
    procedure ExecuteAllRequests;
    procedure ExecuteAllRequestsActionUpdate(Sender: TObject);
    procedure ExecuteRequestActionUpdate(Sender: TObject);
    procedure ExecuteAllRequestsActionExecute(Sender: TObject);
    procedure WsdlItemChangeDataTypeMenuItemClick(Sender: TObject);
    procedure WsdlItemDelMenuItemClick(Sender: TObject);
    procedure WsdlPopupMenuPopup(Sender: TObject);
    procedure MessagesVTSClick(Sender: TObject);
    procedure MessagesVTSGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure doExecuteRequest;
    procedure doDisableMessages;
    procedure doRevalidateMessages;
    procedure doSaveLogRequestsToDisk;
    procedure doSaveLogRepliesToDisk;
    procedure doSaveMessagesToDisk;
    procedure doReadMessagesFromDisk;
    procedure ExecuteRequestActionExecute(Sender: TObject);
    procedure RemoveEnvironmentAction1Click(Sender: TObject);
    procedure AddEnvironmentActionExecute(Sender: TObject);
    procedure RemoveEnvironmentActionUpdate(Sender: TObject);
    procedure EditEnvironmentActionUpdate(Sender: TObject);
    procedure AddEnvironmentActionUpdate(Sender: TObject);
    procedure EditEnvironmentActionExecute(Sender: TObject);
    procedure InWsdlTreeViewChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure DesignPanelAtTopMenuItemClick(Sender: TObject);
    procedure CheckGridFieldsActionExecute(Sender: TObject);
    procedure CheckGridFieldsActionUpdate(Sender: TObject);
    procedure MessagesRegressionActionUpdate(Sender: TObject);
    procedure MessagesRegressionActionExecute(Sender: TObject);
    procedure ReadMessagesActionHint(var HintStr: string; var CanShow: Boolean);
    procedure ReadMessagesActionExecute(Sender: TObject);
    procedure ReadMessagesActionUpdate(Sender: TObject);
    procedure WsdlInformationMenuItemClick(Sender: TObject);
    procedure SaveMessagesActionExecute(Sender: TObject);
    procedure SaveMessagesActionUpdate(Sender: TObject);
    procedure ListofOperationsMenuItemClick(Sender: TObject);
    procedure SchemapropertiesMenuItemClick(Sender: TObject);
    procedure MessagesVTSPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure ShowHttpRequestAsXMLActionUpdate(Sender: TObject);
    procedure ShowHttpRequestAsXMLActionExecute(Sender: TObject);
    procedure CopyLogMemoTextToClipBrdActionUpdate(Sender: TObject);
    procedure CopyLogMemoTextToClipBrdActionExecute(Sender: TObject);
    procedure PasteGridActionExecute(Sender: TObject);
    procedure PasteGridActionUpdate(Sender: TObject);
    procedure CopyGridActionUpdate(Sender: TObject);
    procedure CopyGridActionExecute(Sender: TObject);
    procedure GridViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelectMessageColumnsActionExecute(Sender: TObject);
    procedure SelectMessageColumnsActionUpdate(Sender: TObject);
    procedure OperationReqsTreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ExceptionsVTSGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure ExceptionsVTSFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ClearExceptionsActionExecute(Sender: TObject);
    procedure ClearExceptionsActionUpdate(Sender: TObject);
    procedure NewStubCaseActionExecute(Sender: TObject);
    procedure MessagesVTSGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure MessagesVTSFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VTSEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DocumentationMemoChange(Sender: TObject);
    procedure ClearLogItemsActionExecute(Sender: TObject);
    procedure ClearLogItemsActionUpdate(Sender: TObject);
    procedure WsdlInfoPanelResize(Sender: TObject);
    procedure WsdlServicesComboBoxDropDown(Sender: TObject);
    procedure WsdlComboBoxDropDown(Sender: TObject);
    function CheckHttpAddress (aBind: TObject; aNewValue: String): Boolean;
    procedure RedirectAddressActionExecute(Sender: TObject);
    procedure TreeViewResize(Sender: TObject);
    procedure GridViewExit(Sender: TObject);
    procedure InWsdlTreeViewExit(Sender: TObject);
    procedure SaveStubCaseActionExecute(Sender: TObject);
    procedure GridViewEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure OperationApplySettingsActionExecute(Sender: TObject);
    procedure ApplyToActionUpdate(Sender: TObject);
    procedure MoveDownMessageActionExecute(Sender: TObject);
    procedure MoveDownMessageActionUpdate(Sender: TObject);
    procedure MoveUpMessageActionUpdate(Sender: TObject);
    procedure MoveUpMessageActionExecute(Sender: TObject);
    procedure DeleteMessageActionExecute(Sender: TObject);
    procedure DeleteMessageActionUpdate(Sender: TObject);
    procedure GridViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure AddMessageActionExecute(Sender: TObject);
    procedure AddMessageActionUpdate(Sender: TObject);
    procedure SelectCorrelationElementActionExecute(Sender: TObject);
    procedure SelectCorrelationElementActionUpdate(Sender: TObject);
    procedure GridViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure GridViewNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure GridViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure ActionComboBoxChange(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
    procedure OptionsActionUpdate(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure ReopenStubCaseActionUpdate(Sender: TObject);
    procedure WsdlComboBoxChange(Sender: TObject);
    procedure runScriptActionExecute(Sender: TObject);
    procedure LicenseMenuItemClick(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ReopenStubCaseActionExecute(Sender: TObject);
    procedure OpenStubCaseActionExecute(Sender: TObject);
    procedure SaveStubCaseAsActionExecute(Sender: TObject);
    procedure EditScriptButtonClick(Sender: TObject);
    procedure Expand2Click(Sender: TObject);
    procedure XmlSampleOperationsExecute (Sender : TObject );
    procedure XmlSampleOperationsHint (var HintStr : string ;
      var CanShow : Boolean );
    procedure XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
    procedure XmlZoomValueAsTextMenuItemClick(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure FullExpand1Click(Sender: TObject);
    procedure WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure XmlAddMenuItemClick(Sender: TObject);
    procedure InWsdlTreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure WsdlServicesComboBoxChange(Sender: TObject);
    procedure InWsdlTreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure InWsdlTreeViewNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InWsdlTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure InWsdlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure InWsdlTreeViewEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure InWsdlTreeViewEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure TreeViewClick(Sender: TObject);
    procedure InWsdlTreeViewChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure setWsdlMessage(const Value: TWsdlMessage);
    function getWsdlMessage: TWsdlMessage;
    function getWsdlOperation: TWsdlOperation;
    procedure setWsdlOperation(const Value: TWsdlOperation);
    procedure WsdlOperationsComboBoxChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure OpenWsdlActionExecute(Sender: TObject);
    procedure OpenWsdlActionHint(var HintStr: string; var CanShow: Boolean);
    procedure GridViewFocusedNode(aNode: PVirtualNode);
    procedure OperationReqsTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure GridViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure InWsdlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure OperationWsaActionExecute(Sender: TObject);
    procedure OperationWsaActionUpdate(Sender: TObject);
    procedure ThrowExceptionActionExecute(Sender: TObject);
    procedure MessagesFromDiskActionUpdate(Sender: TObject);
    procedure MessagesFromDiskActionExecute(Sender: TObject);
    procedure GridViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure GridViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolButton9DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FreeFormatMemoChange(Sender: TObject);
    procedure LogDisplayedColumnsActionExecute(Sender: TObject);
    procedure LogUpdateColumns;
    procedure ValidateRequestsActionExecute(Sender: TObject);
    procedure ValidateRepliesActionExecute(Sender: TObject);
    procedure startActionUpdate(Sender: TObject);
    procedure startActionExecute(Sender: TObject);
    procedure stopActionExecute(Sender: TObject);
    procedure stopActionUpdate(Sender: TObject);
    procedure ScriptSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure xsdSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure EditScriptMenuItemClick(Sender: TObject);
    procedure ScriptGoMenuItemClick(Sender: TObject);
    procedure ExtendRecursivityMenuItemClick(Sender: TObject);
    procedure AssignEvaluationMenuItemClick(Sender: TObject);
    procedure TestBeforeScriptActionExecute(Sender: TObject);
    procedure DebugBeforeScriptActionExecute(Sender: TObject);
    procedure EditBetweenScriptMenuItemClick(Sender: TObject);
    procedure EditBeforeScriptMenuItemClick(Sender: TObject);
    procedure EditAfterScriptMenuItemClick(Sender: TObject);
    procedure CopySwiftdatatoclipboardMenuItemClick(Sender: TObject);
    procedure PasteSwiftdatafromclipboardMenuItemClick(Sender: TObject);
    procedure ToggleNotifyActionExecute(Sender: TObject);
    procedure SaveLogRequestsToFileActionUpdate(Sender: TObject);
    procedure SaveLogRequestsToFileActionExecute(Sender: TObject);
    procedure SaveLogRepliesToFileActionUpdate(Sender: TObject);
    procedure SaveLogRepliesToFileActionExecute(Sender: TObject);
    procedure ShowRequestAsHtmlActionUpdate(Sender: TObject);
    procedure ShowRequestAsHtmlActionExecute(Sender: TObject);
    procedure ShowReplyAsHtmlActionExecute(Sender: TObject);
    procedure ShowExceptAsHtmlActionExecute(Sender: TObject);
    procedure OperationOptionsActionUpdate(Sender: TObject);
    procedure OperationOptionsActionExecute(Sender: TObject);
    procedure DownPageControlChange(Sender: TObject);
    procedure HideAllOperationsActionExecute(Sender: TObject);
    procedure UnhideAllOperationsActionExecute(Sender: TObject);
    procedure ToolButton67Click(Sender: TObject);
    procedure alGeneralExecute(Action: TBasicAction; var Handled: Boolean);
    procedure ShowSelectedRequestsAsXmlGridActionUpdate(Sender: TObject);
    procedure ShowSelectedRequestsAsXmlGridActionExecute(Sender: TObject);
    procedure ShowSelectedResponsesAsXmlGridActionUpdate(Sender: TObject);
    procedure ShowSelectedResponsesAsXmlGridActionExecute(Sender: TObject);
    procedure MessagesVTSGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure OperationZoomOnActionExecute(Sender: TObject);
    procedure OperationZoomOnActionUpdate(Sender: TObject);
    procedure ShowLogZoomElementActionExecute(Sender: TObject);
    procedure LogCoverageReportActionUpdate(Sender: TObject);
    procedure CoverageReport(aList: TLogList);
    procedure LogCoverageReportActionExecute(Sender: TObject);
    procedure LogDisplayedColumnsAddClick(Sender: TObject);
    procedure DisplayedcolumnMenuItemClick(Sender: TObject);
    procedure FreeFormatsActionHint(var HintStr: string; var CanShow: Boolean);
    procedure FreeFormatsActionExecute(Sender: TObject);
    procedure LogDisplayedColumnsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure CobolOperationsActionExecute(Sender: TObject);
    procedure CobolOperationsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure SwiftMtOperationsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure SwiftMtOperationsActionExecute(Sender: TObject);
    procedure XsdOperationsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure XsdOperationsActionExecute(Sender: TObject);
    procedure CleanMenuItemClick(Sender: TObject);
    procedure ProjectCleanActionExecute(Sender: TObject);
    procedure GenerateMenuHelpActionExecute(Sender: TObject);
    procedure HelpMainMenuActionExecute(Sender: TObject);
    procedure GenerateScriptAssignmentActionExecute(Sender: TObject);
    procedure ConfigLogActionExecute(Sender: TObject);
    procedure RefreshLog;
    procedure RefreshLogTimerTimer(Sender: TObject);
    procedure ZoomasScriptAssignments1Click(Sender: TObject);
    procedure XSDreportinClipBoardSpreadSheet1Click(Sender: TObject);
    property WsdlOperation: TWsdlOperation read getWsdlOperation write
      setWsdlOperation;
    property WsdlMessage: TWsdlMessage read getWsdlMessage write setWsdlMessage;
    property xmlViewType: TxvViewType read getXmlViewType;
  private
    enableTacoPingPong: Boolean;
    intervalTacoPingPong: Integer;
    editingNode: PVirtualNode;
    notifyTabCaption, MessagesTabCaption: String;
    notifyTabImageIndex: Integer;
    logValidationTabImageIndex: Integer;
    startStopShortCut: TShortCut;
    fLastCaption: String;
    QueueNameList: TStringList;
    captionFileName: String;
    isOptionsChanged: Boolean;
    doScrollMessagesIntoView: Boolean;
    doScrollExceptionsIntoView: Boolean;
    DisclaimerAccepted: Boolean;
    ActualXml: TCustomBindable;
    ActualXmlAttr: TXmlAttribute;
    ErrorReadingLicenseInfo: Boolean;
    CompanyName: String;
    xLicenseExpirationDate: String;
    xLicenseString: String;
    fElementsWhenRepeatable: Integer;
    fDoShowDesignAtTop: Boolean;
    grid_x, grid_y: Integer;
    fAbortPressed: Boolean;
    doNotify: Boolean;
    GetAuthError: String;
    tacoHost: String;
    tacoPort: Integer;
    procedure ShowChosenLogTab;
    function GetAuthorization: Boolean;
    function GetAuthorizationBaseString: String;
    procedure SetOperationZoomPath(aOperation: TWsdlOperation);
    function hintStringFromXsd(aPrefix, aSep, aPostFix: String;
      aXsd: TXsd): String;
    function inImageArea: Boolean;
    function getDoCheckExpectedValues: Boolean;
    procedure setDoCheckExpectedValues(const Value: Boolean);
    procedure ParserError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
    procedure ShowHtml(aCaption, aInfoString: String);
    procedure PopulateXml(aViewType: TxvViewType);
    function getDoValidateReplies: Boolean;
    function getDoValidateRequests: Boolean;
    procedure setDoValidateReplies(const Value: Boolean);
    procedure setDoValidateRequests(const Value: Boolean);
    function getIsRequestAction: Boolean;
    procedure setDoShowDesignAtTop(const Value: Boolean);
    procedure setElementsWhenRepeatable(const Value: Integer);
    procedure setWsdl(const Value: TWsdl);
    procedure setStubChanged(const Value: Boolean);
    function getWsdl: TWsdl;
    procedure DisplayServerMessage(const Msg: String);
    procedure LogServerNotification(const Msg: String);
    procedure LogServerException(const Msg: String; aException: Boolean; E: Exception);
    function HttpActiveHint: String;
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
    procedure WsdlPopulateServices(aWsdl: TWsdl);
    procedure WsdlPopulateOperations(aService: TWsdlService);
    procedure FillInWsdlEdits;
    function FillBindTreeView(aTreeView: TVirtualStringTree;
      aBind: TCustomBindable; aParentNode: PVirtualNode): PVirtualNode;
    procedure FillOperationReqsTreeView(aTreeView: TVirtualStringTree;
      aOperations: TWsdlOperations);
    procedure FillGridTreeView(aTreeView: TVirtualStringTree;
      aMessages: TWsdlMessages);
    function xmlVisibility(aXml: TXml): Boolean;
    procedure SetXmlNodeVisibility(aTreeView: TVirtualStringTree; aXml: TXml;
      aNode: PVirtualNode);
    procedure SetXmlNodeCheckBox(aTreeView: TVirtualStringTree; aXml: TXml;
      aNode: PVirtualNode; ForceChoice: Boolean);
    function FillNodeWithBind(aTreeView: TVirtualStringTree;
      aRootBind, aBind: TCustomBindable; aNode: PVirtualNode): PVirtualNode;
    function FillNodeWithIpm(aTreeView: TVirtualStringTree;
      aRootIpm, aIpm: TIpmItem; aNode: PVirtualNode): PVirtualNode;
    function NodeToMsgLog(aDoClaimLog: Boolean; aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TLog;
    function NodeToOperation(aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TWsdlOperation;
    function NodeToExceptionLog(aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TExceptionLog;
    function NodeToSnapshot(aDoClaimReport: Boolean; aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TSnapshot;
    procedure NodeToMessage(aTreeView: TBaseVirtualTree; aNode: PVirtualNode;
      var aMessage: TWsdlMessage);
    function NodeToBind(aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TCustomBindable;
    // procedure NodeToXml (aTreeView: TBaseVirtualTree; aNode: PVirtualNode; var Xml: TXml; var Attribute: TXmlAttribute);
    procedure RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
    function InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
    function BooleanPromptDialog(aPrompt: String): Boolean;
    procedure FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
    procedure EndEdit;
    procedure XmlAnalyserError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer;
      TokenString, Data: String);
    procedure UpdateXmlTreeViewNode(aTreeView: TVirtualStringTree;
      aNode: PVirtualNode);
    procedure ShowInfoForm(aCaption: String; aInfoString: String);
    procedure UpdateInWsdlCheckBoxes;
    procedure SaveWsdlStubCase(aFileName: String);
    procedure ShowReport (aReport: TSnapshot);
    procedure OpenStubCase(aFileName: String);
    procedure OpenLog4jEvents(aString: String; aIsFileName: Boolean;
      aLogList: TLogList);
    procedure ToAllLogList(aLogList: TLogList);
    procedure UpdateReopenList(aList: TStringList; aFileName: String);
    function LogUsage(aUserName: String): Boolean;
    procedure SetLogUsageTimer;
    function OpenLogUsageDatabase: Boolean;
    procedure ValidateLicense;
    function ValidateLicenseExpirationDate(eDt: String): Boolean;
    procedure PrepareOperation;
    procedure ClearConsole;
    procedure UpdateConsole(aIndex: Integer);
    procedure DoColorBindButtons;
    procedure UpdateCaption;
    function OkToOpenStubCase: Boolean;
    procedure FocusOperationsReqVTS;
    procedure ExchangeMessages(fReply, pReply: TWsdlMessage);
    procedure UpdateMessagesGrid;
    procedure UpdateLogCorrelationIds (aWsdlOperation: TWsdlOperation);
    procedure UpdateLogTabs (aLog: TLog);
    procedure RemoveMessageColumns;
    procedure FocusOnBind(aBind: TCustomBindable);
    function AddMessage(aCopyNode: PVirtualNode): PVirtualNode;
    procedure PasteGridFromPasteBoard;
    procedure PasteGridOnNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure ShowXmlInGrid(aXml: TXml; aReadOnly: Boolean);
    procedure ShowXml(aCaption: String; aXml: TXml);
    procedure ShowIpm(aCaption: String; aIpm: TIpmItem);
    procedure ShowTextAsGrid(aCaption, aText: String);
    procedure ShowTextAsXml(aCaption, aText: String);
    function ShowLogDifferences(aLogs, bLogs: TLogList; aAName, aBName: String): TSnapshotStatus;
    procedure CreateEnvironmentSubMenuItems;
    procedure CreateScriptsSubMenuItems;
    procedure SetEnvironmentClick(Sender: TObject);
    procedure RefreshLogList;
    function XmlBindToNode(aTreeView: TVirtualStringTree;
      aBind: TCustomBindable): PVirtualNode;
    procedure FocusOnFullCaption(aFullCaption: String);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure SetBetaMode;
    procedure LogMqMessage(Sender: TObject; aHeader, aBody: String;
      aRfhHeader: AnsiString; MsgType: MQLONG; MsgDesc: MQMD;
      MqReturnCode: String);
    procedure NeedMqInterfaceCaption(aSender, aObject: TObject;
      var aCaption: String);
    procedure DebugOperationViewAsXml;
    procedure DebugOperation;
    procedure SynchronizedOnMessageChanged;
    procedure OnChange;
    procedure OnMessageChanged(aMessage: TWsdlMessage);
    procedure CheckRpyOrFlt(aBind: TCustomBindable);
    procedure OptionsFromXml(aXml: TXml);
    function LicenseProvider(aRequest: String): String;
    function ReactivateCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ActivateCommand(aActivate: Boolean);
    procedure OpenProjectCommand(aProject: String);
    procedure Notify(const aString: String);
    procedure wsdlStubInitialise;
    function getStubChanged: Boolean;
    procedure ExpressError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
    function EditScript(aXml: TObject): Boolean;
    procedure SetAbortPressed(const Value: Boolean);
    procedure UpdateVisibiltyOfOperations;
    procedure UpdateVisibiltyTreeView (aFreeFormat: Boolean);
    procedure StartBlockingThreadEvent;
    procedure StartNonBlockingThreadEvent;
    procedure TerminateBlockingThreadEvent;
    procedure TerminateNonBlockingThreadEvent;
    procedure SetUiProgress;
    procedure OnRegressionReport (aReport: TRegressionSnapshot);
  private
    fdoShowDesignSplitVertical : Boolean ;
    function getHintStrDisabledWhileActive: String;
    procedure setdoShowDesignSplitVertical (AValue : Boolean );
    procedure ShowHttpReplyAsXMLActionExecute(Sender: TObject);
    procedure ReloadProject;
    function createListOfListsForTypeDefs (aTypeDefs: TXsdDataTypeList): TStringList;
    function createListOfListsForElements (aTypeDef: TXsdDataType): TStringList;
  published
  public
    se: TWsdlProject;
    sc: TWsdlControl;
    claimedLog: TLog;
    claimedReport: TSnapshot;
    mqServerEnv: String;
    CollapseHeaders: Boolean;
    wsdlStubMessagesFileName, wsdlStubSnapshotsFileName: String;
    log4jEventsFileName: String;
    nStubs: Integer;
    freeStubs: Integer;
    ColumnWidths: TStringList;
    lastWsdlOperation: TWsdlOperation;
    WsdlPaths: TStringList;
    ReopenCaseList: TStringList;
    WindowsUserName: String;
    saveToDiskFileName: String;
    saveToDiskDirectory: String;
    saveToDiskSeparator: String;
    saveToDiskExtention: String;
    FileNameList: TStringList;
    scriptPreparedWell: Boolean;
    MainToolBarDesignedButtonCount: Integer;
    StressTestDelayMsMin, StressTestDelayMsMax, StressTestConcurrentThreads, StressTestLoopsPerThread: Integer;
    NumberOfBlockingThreads, NumberOfNonBlockingThreads: Integer;
    function ActiveAfterPrompt: Boolean;
    function InactiveAfterPrompt: Boolean;
    property HintStrDisabledWhileActive
      : String read getHintStrDisabledWhileActive;
    property abortPressed: Boolean read fAbortPressed write SetAbortPressed;
    property doCheckExpectedValues: Boolean read getDoCheckExpectedValues write
      setDoCheckExpectedValues;
    property doValidateRequests: Boolean read getDoValidateRequests write
      setDoValidateRequests;
    property doValidateReplies: Boolean read getDoValidateReplies write
      setDoValidateReplies;
    property isRequestAction: Boolean read getIsRequestAction;
    property doShowDesignAtTop: Boolean read fDoShowDesignAtTop write
      setDoShowDesignAtTop;
    property doShowDesignSplitVertical: Boolean read fdoShowDesignSplitVertical write
      setdoShowDesignSplitVertical;
    property stubChanged: Boolean read getStubChanged write setStubChanged;
    property Wsdl: TWsdl read getWsdl write setWsdl;
    procedure BeginUpdate;
    Procedure EndUpdate;
    procedure PromptForOperationAlias (aOperation: TWsdlOperation);
    function OptionsAsXml: TXml;
    function doDecryptString(aString: AnsiString): AnsiString;
    function doEncryptString(aString: AnsiString): AnsiString;
    procedure ProjectDesignFromString(aString, aMainFileName: String);
    procedure OnlyWhenLicensed;
    function LogMaxEntriesEqualsUnbounded (aCaption: String): Boolean;
    procedure AcquireLock;
    procedure ReleaseLock;
  end;

type
  PXmlTreeRec = ^TXmlTreeRec;

  TXmlTreeRec = record
    Bind: TCustomBindable;
  end;

  POperationTreeRec = ^TOperationTreeRec;

  TOperationTreeRec = record
    Operation: TWsdlOperation;
  end;

  PMessageTreeRec = ^TMessageTreeRec;

  TMessageTreeRec = record
    Message: TWsdlMessage;
  end;

  PLogTreeRec = ^TLogTreeRec;

  TLogTreeRec = record
    Log: TLog;
  end;

  PReportTreeRec = ^TSnapshotTreeRec;
  TSnapshotTreeRec = record
    Report: TSnapshot;
  end;

  PExceptionTreeRec = ^TExceptionTreeRec;

  TExceptionTreeRec = record
    xLog: TExceptionLog;
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  LCLProc, wsdlListUnit, ErrorFound, ClipBrd, ShowXmlUnit,
  ShowXmlCoverageUnit,logChartzUnit, EditOperationScriptUnit, igGlobals,
  ChooseStringUnit, Choose2StringsUnit, AboutUnit, StrUtils, IpmGunLicense,
  IpmGunLicenseUnit, DisclaimerUnit,
  PromptUnit, SelectXmlElement, ApplyToUnit, wsaConfigUnit,
  SelectElementsUnit, A2BXmlz,
  ShowLogDifferencesUnit, EditListValuesUnit, AddFavouritesUnit,
  LogFilterUnit,
  ShowA2BXmlUnit, FindRegExpDialog,
  XmlGridUnit, IpmGridUnit,
  xmlUtilz, ShowExpectedXml, mqBrowseUnit, messagesToDiskUnit, messagesFromDiskUnit{$ifdef windows}, ActiveX{$endif}, EditStamperUnit,
  EditCheckerUnit, Math, vstUtils, DelayTimeUnit, StressTestUnit, base64, xmlxsdparser,
  HashUtilz, xmlio, xmlzConsts, AbZipper
  , htmlXmlUtilz, exceptionUtils, htmlreportz
  , PromptTacoUnit
  ;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TLogColumnEnum =
    ( logExpectedColumn
    , logRemarksColumn
    , logExceptionColumn
    , logRequestTreeColumn
    , logReplyTreeColumn
    , logRequestGridColumn
    , logReplyGridColumn
    , logTimeColumn
    , logDurationColumn
    , logActionColumn
    , logVerbColumn
    , logStatusColumn
    , logServiceColumn
    , logOperationColumn
    , logCorrelationIdColumn
    , logStdColumnCount
    );
  TSnapshotColumnEnum =
    ( snapshotStatusColumn
    , snapshotDateTimeColumn
    , snapshotNameColumn
    , snapshotMessageColumn
    );
  TOperationsColumnEnum =
    ( operationsColumnBeforeScript
    , operationsColumnAfterScript
    , operationsColumnAlias
    );
  const NScripts = 2;

procedure _SaveLogs(aContext: TObject; aFileName: String);
var
  xProject: TWsdlProject;
begin
  xProject := nil; //candidate context
  if aContext is TWsdlProject then
    xProject := aContext as TWsdlProject
  else
    if aContext is TWsdlOperation then with aContext as TWsdlOperation do
      xProject := Owner as TWsdlProject;
  if not Assigned (xProject) then
    raise Exception.Create(Format ('SaveLogs(''%s''); unable to determine context', [aFileName]));
  MainForm.RefreshLog;
  xProject.SaveLogs(ExpandRelativeFileName(xProject.projectFileName, aFileName));
end;

function AllChecked(Sender: TBaseVirtualTree; aNode: PVirtualNode): Boolean;
begin
  if MainForm.NodeToBind(Sender, aNode) is TIpmItem then
  begin
    result := True;
    exit;
  end;
  if aNode = aNode.NextSibling then
    result := True
  else
  begin
    result := (aNode.CheckState = csCheckedNormal);
    if result then
      result := AllChecked(Sender, aNode.Parent);
  end;
end;

procedure TMainForm.LogServerNotification(const Msg: String);
begin
  LogServerException(Msg, False, nil);
end;

procedure TMainForm.LogServerException(const Msg: String; aException: Boolean; E: Exception);
var
  xLog: TExceptionLog;
begin
  if aException then
    xLog := TExceptionLog.Create ( Msg
                                 + LineEnding
                                 + ExceptionStackListString(E)
                                 )
  else
    xLog := TExceptionLog.Create (Msg
                                 );
  se.AcquireLogLock;
  try
    se.toDisplayExceptions.AddEvent (xLog);
  finally
    se.ReleaseLogLock;
  end;
end;

procedure TMainForm.DisplayServerMessage(const Msg: String);
begin
  {
    if EnableLog then
    LogServerException(Msg);
  }
end;

procedure TMainForm.FoundErrorInBuffer(ErrorString: String; aObject: TObject);
begin
  (aObject as TIpmItem).Value := '?' + _progName + ' Error found: ' + ErrorString;
end;

procedure TMainForm.FreeFormatMemoChange(Sender: TObject);
begin
  if Assigned(WsdlMessage) then
  begin
    if WsdlOperation.StubAction = saRequest then
      WsdlMessage.FreeFormatReq := FreeFormatMemo.Text
    else
      WsdlMessage.FreeFormatRpy := FreeFormatMemo.Text;
    stubChanged := True;
  end;
end;

procedure TMainForm.FreeFormatsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.freeFormatOperationsXml;
  if EditXmlXsdBased ( 'Freeformat Operations'
                     , 'OperationDefs.FreeFormatOperations'
                     , 'FreeFormatOperations.Operation.Name'
                     , 'FreeFormatOperations.Operation.Name'
                     , se.IsActive
                     , xXml.Items.Count > 1
                     , esUsed
                     , OperationDefsXsd
                     , xXml
                     ) then
  begin
    AcquireLock;
    try
      stubChanged := True;
      se.freeFormatOperationsUpdate(xXml);
      PrepareOperation;
    finally
      ReleaseLock;
    end;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.FreeFormatsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of freeformat operations ' + HttpActiveHint;
end;

procedure TMainForm.OpenWsdlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Open WSDL web service description or API (OpenAPI f.k.a. Swagger)' + HttpActiveHint;
end;

function TMainForm.HttpActiveHint: String;
begin
  result := '';
end;

procedure TMainForm.OpenWsdlActionExecute(Sender: TObject);
var
  f, w: Integer;
begin
  if not InactiveAfterPrompt then Exit;
  Application.CreateForm(TWsdlListForm, WsdlListForm);
  try
    wsdlListForm.EnvVars := se.EnvVars;
    WsdlListForm.xsdElementsWhenRepeatable := xsdElementsWhenRepeatable;
    WsdlListForm.ShowOperationsWithEndpointOnly :=
      se.OperationsWithEndpointOnly;
    WsdlListForm.SaveRelativeFilenames := se.SaveRelativeFilenames;
    ClearConsole;
    if se.Wsdls.Find(se.FreeFormatWsdl.Name, f) then // not to be seen in list
      se.Wsdls.Delete(f); // will be restored again by PrepareOperation
    if se.Wsdls.Find(se.CobolWsdl.Name, f) then // not to be seen in list
      se.Wsdls.Delete(f); // will be restored again by PrepareOperation
    if se.Wsdls.Find(se.XsdWsdl.Name, f) then // not to be seen in list
      se.Wsdls.Delete(f); // will be restored again by PrepareOperation
    if se.Wsdls.Find(se.XmlSampleWsdl.Name, f) then // not to be seen in list
      se.Wsdls.Delete(f); // will be restored again by PrepareOperation
    if se.Wsdls.Find(se.SwiftMtWsdl.Name, f) then // not to be seen in list
      se.Wsdls.Delete(f); // will be restored again by PrepareOperation
    WsdlListForm.Wsdls := se.Wsdls;
    WsdlListForm.ShowModal;
    w := WsdlListForm.ListView.ItemIndex;
    PrepareOperation;
    { }
    if se.Wsdls.Find(se.FreeFormatWsdl.Name, f) then // not to be seen in list
      Inc(w);
    if se.Wsdls.Find(se.CobolWsdl.Name, f) then // not to be seen in list
      Inc(w);
    if se.Wsdls.Find(se.XsdWsdl.Name, f) then // not to be seen in list
      Inc(w);
    if se.Wsdls.Find(se.XmlSampleWsdl.Name, f) then // not to be seen in list
      Inc(w);
    if se.Wsdls.Find(se.SwiftMtWsdl.Name, f) then // not to be seen in list
      Inc(w);
    { }
    UpdateConsole(w);
    stubChanged := (stubChanged or WsdlListForm.stubChanged);
    if wsdlListForm.ReloadRequired then
      ReloadProject;
  finally
    FreeAndNil(WsdlListForm);
  end;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.WsdlPopulateServices(aWsdl: TWsdl);
var
  X: Integer;
begin
  WsdlServicesComboBox.Clear;
  WsdlOperationsComboBox.Clear;
  if not Assigned(Wsdl) then
    exit;
  for X := 0 to aWsdl.Services.Count - 1 do
  begin
    WsdlServicesComboBox.Items.Add(aWsdl.Services.Services[X].Name);
  end;
  { }{
    if (aWsdl.Services.Count > 0)
    and (aWsdl.Services.Services[0].Operations.Count > 0) then
    WsdlOperation := aWsdl.Services.Services[0].Operations.Operations[0];
    { }{ }
  if aWsdl.Services.Count > 0 then
  begin
    WsdlServicesComboBox.ItemIndex := 0;
    if Assigned(WsdlServicesComboBox.OnChange) then
      WsdlServicesComboBox.OnChange(Self);
  end;
  { }
end;

procedure TMainForm.WsdlPopulateOperations(aService: TWsdlService);
var
  X: Integer;
begin
  WsdlOperationsComboBox.Clear;
  for X := 0 to aService.Operations.Count - 1 do
  begin
    WsdlOperationsComboBox.Items.Add(aService.Operations.Operations[X].Name);
  end;
  if aService.Operations.Count > 0 then
  begin
    WsdlOperationsComboBox.ItemIndex := 0;
    if Assigned(WsdlOperationsComboBox.OnChange) then
      WsdlOperationsComboBox.OnChange(Self);
  end;
end;

procedure TMainForm.WsdlOperationsComboBoxChange(Sender: TObject);
var
  xs: Integer;
  xo: Integer;
  saveStubChanged: Boolean;
  saveOnChanged: TVTFocusChangeEvent;
begin
  xs := WsdlServicesComboBox.ItemIndex;
  xo := WsdlOperationsComboBox.ItemIndex;
  if (xs < 0) or (xo < 0) then
    exit;
  if Wsdl.Services.Services[xs].DescriptionType = ipmDTFreeFormat then
  begin
    InWsdlTreeView.Align := alLeft;
    InWsdlTreeView.Visible := False;
    FreeFormatMemo.Align := alClient;
    FreeFormatMemo.Visible := True;
  end
  else
  begin
    FreeFormatMemo.Align := alRight;
    FreeFormatMemo.Visible := False;
    InWsdlTreeView.Align := alClient;
    InWsdlTreeView.Visible := True;
  end;
  saveStubChanged := stubChanged;
  saveOnChanged := OperationReqsTreeView.OnFocusChanged;
  try
    OperationReqsTreeView.OnFocusChanged := nil;
    FillInWsdlEdits;
    GridView.Clear;
    UpdateMessagesGrid;
    FillGridTreeView(GridView, WsdlOperation.Messages);
    DoColorBindButtons;
    if Assigned(WsdlOperation) and Assigned(ActionComboBox.OnChange) then
      ActionComboBox.OnChange(nil);
    if Assigned(saveOnChanged) then
      FocusOperationsReqVTS;
  finally
    stubChanged := saveStubChanged;
    OperationReqsTreeView.OnFocusChanged := saveOnChanged;
  end;
end;

procedure TMainForm.FillInWsdlEdits;
begin
  if (WsdlOperation <> nil) then
  begin
    ActionComboBox.ItemIndex := Ord(WsdlOperation.StubAction);
    // SoapActionEdit.Text := WsdlOperation.SoapAction;
    try
      OperationDocumentationViewer.SetHtmlFromStr(textToHtml(WsdlOperation.Documentation.Text));
    except
    end;
  end;
end;

procedure TMainForm.FillOperationReqsTreeView(aTreeView: TVirtualStringTree;
  aOperations: TWsdlOperations);
var
  xNode: PVirtualNode;
  xData: POperationTreeRec;
  X: Integer;
begin
  aTreeView.EndEditNode;
  aTreeView.Clear;
  aTreeView.RootNodeCount := 0;
  aTreeView.NodeDataSize := SizeOf(TOperationTreeRec);
  if aOperations = nil then
    exit;
  for X := 0 to aOperations.Count - 1 do
  begin
    xNode := aTreeView.AddChild(nil);
    xData := aTreeView.GetNodeData(xNode);
    xData.Operation := aOperations.Operations[X];
  end;
  xNode := aTreeView.GetFirst;
  aTreeView.Selected[xNode] := True;
  aTreeView.FocusedNode := xNode;
  UpdateVisibiltyOfOperations;
end;

procedure TMainForm.FillGridTreeView(aTreeView: TVirtualStringTree;
  aMessages: TWsdlMessages);
var
  xNode: PVirtualNode;
  xData: PMessageTreeRec;
  X: Integer;
begin
  aTreeView.EndEditNode;
  aTreeView.Clear;
  aTreeView.RootNodeCount := 0;
  aTreeView.NodeDataSize := SizeOf(TMessageTreeRec);
  if aMessages = nil then
    exit;
  for X := 0 to aMessages.Count - 1 do
  begin
    xData := aTreeView.GetNodeData(aTreeView.AddChild(nil));
    xData.Message := aMessages.Messages[X];
  end;
  if Assigned(WsdlOperation.LastMessage) then
    WsdlMessage := WsdlOperation.LastMessage;
  if Assigned(WsdlMessage) then
    FocusOnFullCaption(WsdlOperation.LastFullCaption)
  else
  begin
    xNode := aTreeView.GetFirst;
    aTreeView.Selected[xNode] := True;
    aTreeView.FocusedNode := xNode;
  end;
end;

function TMainForm.FillBindTreeView(aTreeView: TVirtualStringTree;
  aBind: TCustomBindable; aParentNode: PVirtualNode): PVirtualNode;
var
  xHeaders: TWsdlHeaders;
  X: Integer;
  xXml: TXml;
  xNode: PVirtualNode;
begin
  result := nil;
  if aBind = nil then
    exit;
  result := FillNodeWithBind(aTreeView, aBind, aBind, aParentNode);
  aTreeView.FullExpand(aTreeView.GetFirst);
  if CollapseHeaders then
  begin
    if aBind is TXml then
    begin
      if WsdlOperation.StubAction = saRequest then
      begin
        xHeaders := WsdlOperation.InputHeaders;
        xXml := WsdlOperation.reqBind as TXml;
      end
      else
      begin
        xHeaders := WsdlOperation.OutputHeaders;
        xXml := WsdlOperation.rpyBind as TXml;
      end;
      for X := 0 to xHeaders.Count - 1 do
      begin
        xNode := XmlBindToNode(aTreeView, xXml.Items.XmlItems[X]);
        if Assigned(xNode) then
          aTreeView.FullCollapse(xNode);
      end;
    end;
  end;
end;

procedure TMainForm.setWsdlOperation(const Value: TWsdlOperation);
begin
  try
    if Assigned(Value) and (Value is TWsdlOperation) then
    begin
      Wsdl := Value.Wsdl;
      WsdlPopulateServices(Wsdl);
      WsdlServicesComboBox.ItemIndex := Wsdl.Services.IndexOfObject
        (Value.WsdlService);
      WsdlPopulateOperations
        (Wsdl.Services.Services[WsdlServicesComboBox.ItemIndex]);
      WsdlOperationsComboBox.ItemIndex :=
        Value.WsdlService.Operations.IndexOfObject(Value);
      WsdlOperationsComboBoxChange(nil);
      ActionComboBox.Enabled := (Value.WsdlService.DescriptionType <> ipmDTEmail);
      EditBetweenScriptMenuItem.Visible := (Value.StubAction = saStub);
      EditBeforeScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
      EditAfterScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
      UpdateVisibiltyTreeView(Value.WsdlService.DescriptionType = ipmDTFreeFormat);
    end;
  except
  end;
end;

function TMainForm.getWsdlOperation: TWsdlOperation;
begin
  result := nil;
  if (WsdlServicesComboBox.ItemIndex > -1) and
    (WsdlOperationsComboBox.ItemIndex > -1) then
    try
      result := Wsdl.Services.Services[WsdlServicesComboBox.ItemIndex]
        .Operations.Operations[WsdlOperationsComboBox.ItemIndex];
    except
    end;
end;

function TMainForm.FillNodeWithIpm(aTreeView: TVirtualStringTree;
  aRootIpm, aIpm: TIpmItem; aNode: PVirtualNode): PVirtualNode;
var
  xChildNode: PVirtualNode;
  xData: PXmlTreeRec;
  X: Integer;
begin
  if not Assigned(aIpm) then
    exit;
  xChildNode := aTreeView.AddChild(aNode);
  xData := aTreeView.GetNodeData(xChildNode);
  xData.Bind := aIpm;
  for X := 0 to aIpm.Items.Count - 1 do
  begin
    FillNodeWithIpm(aTreeView, aRootIpm, aIpm.Items.IpmItems[X], xChildNode);
  end;
  result := xChildNode;
end;

function TMainForm.FillNodeWithBind(aTreeView: TVirtualStringTree;
  aRootBind, aBind: TCustomBindable; aNode: PVirtualNode): PVirtualNode;
var
  xChildNode: PVirtualNode;
  xAttributeNode: PVirtualNode;
  xData: PXmlTreeRec;
  X: Integer;
begin
  if not Assigned(aBind) then
    exit;
  xChildNode := aTreeView.AddChild(aNode);
  xData := aTreeView.GetNodeData(xChildNode);
  xData.Bind := aBind;
  if aBind is TXml then
  begin
    SetXmlNodeCheckBox(aTreeView, aBind as TXml, xChildNode, aRootBind = aBind);
    SetXmlNodeVisibility(aTreeView, aBind as TXml, xChildNode);
    for X := 0 to (aBind as TXml).Attributes.Count - 1 do
    begin
      xAttributeNode := aTreeView.AddChild(xChildNode);
      xData := aTreeView.GetNodeData(xAttributeNode);
      xData.Bind := (aBind as TXml).Attributes.XmlAttributes[X];
      xAttributeNode.CheckType := ctCheckBox;
      if (aBind as TXml).Attributes.XmlAttributes[X].Checked then
        xAttributeNode.CheckState := csCheckedNormal
      else
        xAttributeNode.CheckState := csUnCheckedNormal;
    end;
  end;
  for X := 0 to aBind.Children.Count - 1 do
  begin
    FillNodeWithBind(aTreeView, aRootBind, aBind.Children.Bindables[X],
      xChildNode);
  end;
  result := xChildNode;
end;

function TMainForm.xmlVisibility(aXml: TXml): Boolean;
begin
  if not Assigned(aXml.Parent) then
  begin
    result := True;
    exit;
  end;
  case xmlViewType of
    xvAll:
      begin
        result := True;
        exit;
      end;
    xvRequired:
      begin
        result := Assigned(aXml.Xsd) and (aXml.Xsd.minOccurs <> '0');
        exit;
      end;
    xvUsed:
      begin
        result := aXml.Checked;
        exit;
      end;
    xvReqUsed:
      begin
        result := (aXml.Checked) or
          (Assigned(aXml.Xsd) and (aXml.Xsd.minOccurs <> '0'));
        exit;
      end;
  end;
end;

procedure TMainForm.SetXmlNodeVisibility(aTreeView: TVirtualStringTree;
  aXml: TXml; aNode: PVirtualNode);
begin
  aTreeView.IsVisible[aNode] := xmlVisibility(aXml);
end;

procedure TMainForm.SetXmlNodeCheckBox(aTreeView: TVirtualStringTree;
  aXml: TXml; aNode: PVirtualNode; ForceChoice: Boolean);
begin
  if (Assigned(aXml.Parent)) and (Assigned((aXml.Parent as TXml).Xsd)) then
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
    aNode.CheckType := ctCheckBox;
    aNode.CheckState := csCheckedNormal;
  end;
end;

function TMainForm.NodeToBind(aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode): TCustomBindable;
var
  Data: PXmlTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned(Data) then
      result := Data.Bind;
  end;
end;

procedure TMainForm.NodeToMessage(aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode; var aMessage: TWsdlMessage);
var
  Data: PMessageTreeRec;
begin
  aMessage := nil;
  if Assigned(aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned(Data) then
    begin
      aMessage := Data.Message;
    end;
  end;
end;

procedure TMainForm.InWsdlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  xBind: TCustomBindable;
  expXml: TXml;
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  if Column = treeValueColumn then
  begin
    xBind := NodeToBind(Sender, Node);
    if not AllChecked(Sender, Node) then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := bgNilValueColor;
        FillRect(CellRect);
      end;
      exit;
    end;
    if xBind is TXml then
      with xBind as TXml do
      begin
        if Group or ((Assigned(Xsd)) and ((TypeDef.ContentModel = 'Empty') or
              (TypeDef.ElementDefs.Count > 0))) then
        begin
          with TargetCanvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := clBtnFace;
            FillRect(CellRect);
          end;
        end;

      end;

    if xBind is TIpmItem then
    begin
      if ((xBind as TIpmItem).Group) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect(CellRect);
        end;
      end;

    end;

  end;
  if Column = treeTagColumn then
  begin
    xBind := NodeToBind(Sender, Node);
    NodeToMessage(GridView, GridView.FocusedNode, xMessage);
    if (xBind is TXml) or (xBind is TXmlAttribute) then
    begin
      expXml := nil;
      if Assigned(WsdlOperation) and Assigned(xMessage) then
        if (WsdlOperation.StubAction = saRequest) then
          expXml := xMessage.rpyBind as TXml
        else
          expXml := xMessage.reqBind as TXml;
      if Assigned(xMessage) and (expXml.IsAncestorOf(xBind) or (expXml = xBind)
        ) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          // Brush.Color := bgCorrelationItemColor;
          Brush.Color := bgExpectedValueColor;
          FillRect(CellRect);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.InWsdlTreeViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xBind: TCustomBindable;
begin
  WsdlOperation.AcquireLock;
  try
    xBind := NodeToBind(Sender, Node);
    stubChanged := True;
    xBind.Checked := (Node.CheckState = csCheckedNormal);
    if (not xBind.Checked) then
    begin
      if xmlUtil.doCollapseOnUncheck then
        InWsdlTreeView.FullCollapse(Node)
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
      CheckRpyOrFlt(xBind);
      if xmlUtil.doExpandOnCheck then
        InWsdlTreeView.FullExpand(Node);
    end;
    RevalidateXmlTreeView(Sender as TVirtualStringTree);
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  xNode := aTreeView.GetFirst; // search from begin
  while not(xNode = nil) do
  begin
    xBind := NodeToBind(aTreeView, xNode);
    if xBind is TXmlAttribute then
    begin
      if xBind.Checked then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    if (xBind is TXml) then
    begin
      aTreeView.IsVisible[xNode] := xmlVisibility(xBind as TXml);
      if (xBind.Checked) then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    xNode := aTreeView.GetNext(xNode);
  end;
  with aTreeView do
  begin
    // IsVisible [GetNextSibling (GetFirst)] := doCheckExpectedValues;
    Invalidate;
  end;
  GridView.InvalidateNode(GridView.FocusedNode);
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  xChanged: Boolean;
  xBind: TCustomBindable;
begin
  if (Sender = GridView) then
  begin
    if not Assigned (GridView.FocusedNode) then Exit;
    case GridView.FocusedColumn of
      Ord (operationsColumnBeforeScript): EditMessageScriptActionExecute(nil);
      Ord (operationsColumnAfterScript): EditMessageAfterScriptActionExecute(nil);
    end;
  end;
  if (    (Sender = GridView)
      and (inImageArea)
     )
  or (    (Sender = InWsdlTreeView)
      and (InWsdlTreeView.FocusedColumn = treeButtonColumn)
     )
  then
  begin
    xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    if xmlUtil.isExtendAdviced(xBind) then
      ExtendRecursivityMenuItemClick(nil)
    else
    begin
      xChanged := xmlUtil.editXml(xBind, Sender = InWsdlTreeView, False);
      if xChanged then
      begin
        UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
        RevalidateXmlTreeView(InWsdlTreeView);
      end;
      stubChanged := stubChanged or xChanged;
      InWsdlTreeView.FocusedColumn := treeValueColumn;
    end;
  end
  else
  begin
    if (Assigned((Sender as TVirtualStringTree).FocusedNode)) and
      (((Sender as TVirtualStringTree).FocusedColumn = treeValueColumn) or
        (Sender = GridView)) then (Sender as TVirtualStringTree)
      .EditNode((Sender as TVirtualStringTree).FocusedNode,
        (Sender as TVirtualStringTree).FocusedColumn);
  end;
end;

procedure TMainForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TMainForm.InWsdlTreeViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = treeValueColumn then
  begin
    try xmlUtil.CheckValidity(NodeToBind(Sender, Node)); Except End;
  end;
end;

procedure TMainForm.InWsdlTreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Sender, Node);
  case Column of
    treeTagColumn, treeButtonColumn:
      begin
        Allowed := False;
      end;
    treeValueColumn:
      begin
        Allowed := (xBind is TXmlAttribute) or
          ((xBind is TXml) and (not(((xBind as TXml).Group) or
                ((Assigned((xBind as TXml).Xsd)) and
                  (((xBind as TXml).TypeDef.ContentModel = 'Empty') or
                    ((xBind as TXml).TypeDef.ElementDefs.Count > 0))))))
          or ((xBind is TIpmItem) and (not(xBind as TIpmItem).Group));
      end;
  end;
end;

procedure TMainForm.InWsdlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xBind: TCustomBindable;
  Xml: TXml;
  XmlAttr: TXmlAttribute;
  Ipm: TIpmItem;
begin
  try
    xBind := NodeToBind(Sender, Node);
    if xBind is TXmlAttribute then
    begin
      XmlAttr := xBind as TXmlAttribute;
      Xml := XmlAttr.Parent as TXml;
      Ipm := nil;
    end;
    if xBind is TXml then
    begin
      XmlAttr := nil;
      Xml := xBind as TXml;
      Ipm := nil;
    end;
    if xBind is TIpmItem then
    begin
      XmlAttr := nil;
      Xml := nil;
      Ipm := xBind as TIpmItem;
    end;
    case Kind of
      ikNormal, ikSelected:
        begin
          case Column of
            treeTagColumn:
              begin
              end;
            treeButtonColumn:
              begin
                if Assigned(XmlAttr) then
                begin
                  ImageIndex := -1;
                  exit;
                end;
                if Assigned(Xml) then
                begin
                  if xmlUtil.isExtendAdviced(xBind) then
                  begin
                    ImageIndex := 28;
                    exit;
                  end;
                  if xmlUtil.isBoolean(Xml) then
                  begin
                    if (Xml.Value = 'true') or (Xml.Value = '1') then
                      ImageIndex := 22
                    else
                      ImageIndex := 21;
                    exit;
                  end;
                  if xmlUtil.isDateTime(xBind) then
                  begin
                    ImageIndex := 23;
                    exit;
                  end;
                  if xmlUtil.isDate(xBind) then
                  begin
                    ImageIndex := 24;
                    exit;
                  end;
                  if xmlUtil.isTime(xBind) then
                  begin
                    ImageIndex := 25;
                    exit;
                  end;
                  if xmlUtil.isEditSupported(xBind) then
                  begin
                    ImageIndex := 18;
                    exit;
                  end;
                  if xmlUtil.isGridAdviced(xBind) then
                  begin
                    ImageIndex := 19;
                    exit;
                  end;
                  if xmlUtil.isTreeAdviced(xBind) then
                  begin
                    ImageIndex := 20;
                    exit;
                  end;
                  ImageIndex := -1;
                  exit;
                end;
                if Assigned(Ipm) then
                begin
                  // tbd
                end;
              end;
            treeValueColumn:
              begin
                { }{
                  if Assigned (Xml)
                  and Xml.Checked
                  and Xml.isEvaluation then
                  ImageIndex := 27;
                  { }
              end;
          end; { case column }
        end; { Kind in ikNormal, ikSelected }
    end; { case Kind }
  except
  end;
end;

procedure TMainForm.InWsdlTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xBind: TCustomBindable;
begin
  CellText := '';
  try
    xBind := NodeToBind(Sender, Node);
    if Assigned(xBind) then
    begin
      case Column of
        treeTagColumn:
          CellText := xmlUtil.BindCaption(xBind);
        treeValueColumn:
          if xBind.isEvaluation then
            CellText := '=(' + xBind.Checker + ')'
          else
            CellText := xBind.Value;
      end;
    end;
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  xBind: TCustomBindable;
  xMessage: String;
begin
  xMessage := ''; //avoid warning
  try
    if (Key = VK_F8) then
    begin
      xBind := NodeToBind(Sender as TVirtualStringTree,
        (Sender as TVirtualStringTree).FocusedNode);
      if (xBind is TXml) then
        ShowInfoForm('Schema for ' + (xBind as TXml).TypeDef.Name,
          (xBind as TXml).TypeDef.SchemaAsText((xBind as TXml).TypeDef.Name));
    end;
    if (Key = VK_INSERT) then
    begin
      xBind := NodeToBind(Sender as TVirtualStringTree,
        (Sender as TVirtualStringTree).FocusedNode);
      if not xBind.IsValueValid(xMessage) then
        ShowMessage(xMessage);
    end;
    if (Key = VK_RETURN) then
    begin (Sender as TVirtualStringTree)
      .EditNode((Sender as TVirtualStringTree).FocusedNode,
        (Sender as TVirtualStringTree).FocusedColumn);
    end;
  except
    // in case typing in grid view while detailpage not showing a treeview (e.g. freeformat)
  end;
end;

procedure TMainForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin (Sender as TVirtualStringTree)
  .FocusedNode := (Sender as TVirtualStringTree).GetNodeAt(X
    { + (Sender as TVirtualStringTree).OffsetX }
    , Y { + (Sender as TVirtualStringTree).OffsetY }
  );
end;

procedure TMainForm.InWsdlTreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  xBind: TCustomBindable;
begin
  WsdlOperation.AcquireLock;
  try
    xBind := NodeToBind(Sender, Node);
    if Assigned(xBind) then
    begin
      case Column of
        treeTagColumn:
          begin
            if xBind.Name <> NewText then
            begin
              xBind.Name := NewText;
              stubChanged := True;
            end;
          end;
        treeValueColumn:
          begin
            if (NewText = '&nil') and not(xBind is TIpmItem) then
            begin
              if xBind.Checked then
              begin
                xBind.Checked := False;
                stubChanged := True;
              end;
            end
            else
            begin
              if (NewText <> xBind.Value) or (not AllChecked(Sender, Node)) then
              begin
                xBind.Value := NewText;
                xBind.Checked := True;
                CheckRpyOrFlt(xBind);
                stubChanged := True;
              end;
            end;
          end;
      end;
      RevalidateXmlTreeView(Sender as TVirtualStringTree);
    end;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.InWsdlTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TCustomBindable;
begin
  if (Node = (Sender as TVirtualStringTree).GetFirst) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;

  xBind := NodeToBind(Sender as TVirtualStringTree, Node);
  if xBind is TXmlAttribute then
  begin
    if ((Assigned((xBind as TXmlAttribute).XsdAttr)) and
        ((xBind as TXmlAttribute).XsdAttr.Use = 'required')) then
    begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if AllChecked(Sender, Node.Parent) then
        if not(xBind as TXmlAttribute).Checked then
          TargetCanvas.Font.Color := clRed { clLtGray } ;
    end
    else
    begin
      if (not Assigned((xBind as TXmlAttribute).XsdAttr)) then
        TargetCanvas.Font.Color := clRed { clLtGray } ;
    end;
    exit;
  end;
  if xBind is TXml then
  begin
    if (not Assigned((xBind as TXml).Xsd)) then
    begin
      TargetCanvas.Font.Color := clRed { clLtGray } ;
      exit;
    end;
    try
      if Assigned((xBind as TXml).Xsd) and
        (StrToIntDef((xBind as TXml).Xsd.minOccurs, 0) > 0) and Assigned
        (xBind.Parent) and Assigned(TXml(xBind.Parent).Xsd) and
        (TXml(xBind.Parent).TypeDef.ContentModel <> 'Choice') then
      begin
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        if AllChecked(Sender, Node.Parent) then
          if not xBind.Checked then
            TargetCanvas.Font.Color := clRed { clLtGray } ;
      end;
    except
      ShowMessage((xBind as TXml).Xsd.minOccurs);
    end;
  end;
  if xBind is TIpmItem then
  begin
    //
  end;
end;

procedure TMainForm.WsdlServicesComboBoxChange(Sender: TObject);
begin
  WsdlPopulateOperations(Wsdl.Services.Services[WsdlServicesComboBox.ItemIndex]
    );
end;

procedure TMainForm.InWsdlTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  f: Integer;
  Xml: TXml;
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
  swapEvent: TVTFocusChangeEvent;
begin
  xMessage := nil; //avoid warning
  Sender.Selected[Sender.FocusedNode] := True;
  xBind := NodeToBind(Sender, Sender.FocusedNode);
  if not Assigned(xBind) then
    exit;
  if xBind is TXmlAttribute then
  begin
    Xml := xBind.Parent as TXml;
  end;
  if xBind is TXml then
  begin
    Xml := xBind as TXml;
  end;
  if xBind is TIpmItem then
  begin
    Xml := nil;
  end;
  xmlUtil.ListXsdProperties(InWsdlPropertiesListView, xBind);
  // InWsdlEnumerationsListView.Clear;
  ActualXml := nil;
  ActualXmlAttr := nil;
  if xBind is TIpmItem then
    StatusPanel.Caption := '[' + IntToStr((xBind as TIpmItem).Offset + 1)
      + ':' + IntToStr((xBind as TIpmItem).Bytes) + '] ' + xBind.FullCaption
  else
    StatusPanel.Caption := xBind.FullCaption;
  try
    xmlUtil.ListXsdDocumentation(DocumentationViewer, xBind, False, False);
  except
  end;
  if not(tsUpdating in InWsdlTreeView.TreeStates) then
    WsdlOperation.LastFullCaption := StatusPanel.Caption;
  swapEvent := GridView.OnFocusChanged;
  if Assigned(swapEvent) then
  begin
    NodeToMessage(GridView, GridView.FocusedNode, xMessage);
    f := xMessage.ColumnXmls.IndexOfObject(Xml);
    if f > -1 then
    begin
      try
        GridView.OnFocusChanged := nil;
        GridView.FocusedColumn := f + 1 + NScripts + WsdlOperation.CorrelationBindables.Count;
      finally
        GridView.OnFocusChanged := swapEvent;
      end;
    end;
  end;
  { }{
    if (Column = treeValueColumn)
    and (xmlutil.isEditAllowed(xBind)) then
    InWsdlTreeView.EditNode(Sender.FocusedNode, Sender.FocusedColumn);
    { }
end;

procedure TMainForm.WsdlItemDelMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    if (LowerCase((xBind as TXml).Xsd.maxOccurs) = 'unbounded') or
      (StrToInt((xBind as TXml).Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isDeleteAllowed(xBind, True) then
        exit;
      InWsdlTreeView.BeginUpdate;
      try
        xmlUtil.Delete((xBind as TXml));
        InWsdlTreeView.DeleteNode(InWsdlTreeView.FocusedNode, True);
        WsdlOperation.AcquireLock;
        try
          se.UpdateMessageRow(WsdlOperation, WsdlMessage);
        finally
          WsdlOperation.ReleaseLock;
        end;
        InWsdlTreeView.Invalidate;
        GridView.InvalidateNode(GridView.FocusedNode);
        stubChanged := True;
      finally
        InWsdlTreeView.EndUpdate;
      end;
    end; { if maxOccurs greater than 1 }
  end; { if xml clicked; just to be sure }
end;

procedure TMainForm.ExtendRecursivityMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml) then
    with xBind as TXml do
    begin
      ExtendRecursivity;
      UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
      InWsdlTreeView.FocusedColumn := 0;
      InWsdlTreeView.FullExpand(InWsdlTreeView.FocusedNode);
      se.UpdateMessageRow(WsdlOperation, WsdlMessage);
      InWsdlTreeView.Invalidate;
      GridView.InvalidateNode(GridView.FocusedNode);
      InWsdlTreeViewFocusChanged(InWsdlTreeView, InWsdlTreeView.FocusedNode,
        InWsdlTreeView.FocusedColumn);
    end;
end;

procedure TMainForm.XmlAddMenuItemClick(Sender: TObject);
var
  Xml: TXml; { current }
  xXml: TXml; { new created }
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    Xml := xBind as TXml;
    if (LowerCase(Xml.Xsd.maxOccurs) = 'unbounded') or
      (StrToInt(Xml.Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isAddAllowed(NodeToBind(InWsdlTreeView,
          InWsdlTreeView.FocusedNode), True) then
        exit;
      xXml := xmlUtil.Add(Xml);
      InWsdlTreeView.FocusedNode := InsertXmlNode(InWsdlTreeView.FocusedNode,
        xXml);
      InWsdlTreeView.FocusedColumn := 0;
      InWsdlTreeView.Expanded[InWsdlTreeView.FocusedNode] := True;
      WsdlOperation.AcquireLock;
      try
        se.UpdateMessageRow(WsdlOperation, WsdlMessage);
      finally
        WsdlOperation.ReleaseLock;
      end;
      InWsdlTreeView.Invalidate;
      GridView.InvalidateNode(GridView.FocusedNode);
      stubChanged := True;
    end; { if maxOccurs greater than 1 }
  end; { if xml clicked; just to be sure }
end;

function TMainForm.InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
begin
  result := InWsdlTreeView.InsertNode(aNode, amInsertAfter);
  FinishXmlNode(result, aXml);
end;

procedure TMainForm.EndUpdate;
begin
  GridView.EndUpdate;
  InWsdlTreeView.EndUpdate;
end;

procedure TMainForm.BeginUpdate;
begin
  GridView.BeginUpdate;
  InWsdlTreeView.BeginUpdate;
end;

function TMainForm.BooleanPromptDialog(aPrompt: String): Boolean;
begin
  result := (MessageDlg(aPrompt, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TMainForm.FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
var
  attrNode: PVirtualNode;
  Data: PXmlTreeRec;
  I: Integer;
begin
  Data := InWsdlTreeView.GetNodeData(aNode);
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
    attrNode := InWsdlTreeView.AddChild(aNode);
    attrNode.CheckType := ctCheckBox;
    if aXml.Attributes.XmlAttributes[I].Checked then
      attrNode.CheckState := csCheckedNormal
    else
      attrNode.CheckState := csUnCheckedNormal;
    Data := InWsdlTreeView.GetNodeData(attrNode);
    Data.Bind := aXml.Attributes.XmlAttributes[I];
  end;
  for I := 0 to aXml.Items.Count - 1 do
    FinishXmlNode(InWsdlTreeView.AddChild(aNode), aXml.Items.XmlItems[I]);
end;

procedure TMainForm.Copytoclipboard1Click(Sender: TObject);
begin
  xmlUtil.CopyToClipboard(NodeToBind(InWsdlTreeView,
      InWsdlTreeView.FocusedNode));
end;

procedure TMainForm.EndEdit;
begin
  GridView.EndEditNode;
  InWsdlTreeView.EndEditNode;
end;

procedure TMainForm.PopulateXml(aViewType: TxvViewType);
begin
  xmlUtil.Populate(NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode),
    aViewType);
  stubChanged := True;
  RevalidateXmlTreeView(InWsdlTreeView);
end;

procedure TMainForm.WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
begin
  try
    xmlUtil.PasteFromClipboard(NodeToBind(InWsdlTreeView,
        InWsdlTreeView.FocusedNode));
    UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    stubChanged := True;
  finally
    RevalidateXmlTreeView(InWsdlTreeView);
  end;
end;

procedure TMainForm.XmlAnalyserError(Sender: TObject;
  LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
begin
  Application.CreateForm(TErrorFoundDlg, ErrorFoundDlg);
  try
    ErrorFoundDlg.FileNameEdit.Text := 'ClipBoard';
    ErrorFoundDlg.LineNumberEdit.Text := IntToStr(LineNumber);
    ErrorFoundDlg.ColumnNumberEdit.Text := IntToStr(ColumnNumber);
    ErrorFoundDlg.TokenStringEdit.Text := TokenString;
    ErrorFoundDlg.Viewer := '';
    ErrorFoundDlg.ShowModal;
  finally
    FreeAndNil(ErrorFoundDlg);
  end;
end;

procedure TMainForm.UpdateXmlTreeViewNode(aTreeView: TVirtualStringTree;
  aNode: PVirtualNode);
var
  X: Integer;
  xBind: TCustomBindable;
  xXml: TXml;
  xIpm: TIpmItem;
  xAttributeNode: PVirtualNode;
  xData: PXmlTreeRec;
  xExpanded: Boolean;
begin
  xExpanded := aTreeView.Expanded[aNode];
  xBind := NodeToBind(aTreeView, aNode);
  if xBind is TXmlAttribute then
  begin
    aNode := aNode.Parent;
    xBind := xBind.Parent;
  end;
  aTreeView.DeleteChildren(aNode);
  if xBind is TXml then
  begin
    xXml := xBind as TXml;
    SetXmlNodeCheckBox(aTreeView, xXml, aNode, False);
    for X := 0 to xXml.Attributes.Count - 1 do
    begin
      xAttributeNode := aTreeView.AddChild(aNode);
      xData := aTreeView.GetNodeData(xAttributeNode);
      xData.Bind := xXml.Attributes.XmlAttributes[X];
      xAttributeNode.CheckType := ctCheckBox;
      if xXml.Attributes.XmlAttributes[X].Checked then
        xAttributeNode.CheckState := csCheckedNormal
      else
        xAttributeNode.CheckState := csUnCheckedNormal;
    end;
    for X := 0 to xXml.Items.Count - 1 do
    begin
      FillNodeWithBind(aTreeView, nil, xXml.Items.XmlItems[X], aNode);
    end;
  end;
  if xBind is TIpmItem then
  begin
    xIpm := xBind as TIpmItem;
    for X := 0 to xIpm.Items.Count - 1 do
    begin
      FillNodeWithIpm(aTreeView, nil, xIpm.Items.IpmItems[X], aNode);
    end;
  end;
  if xExpanded then
    aTreeView.FullExpand(aNode);
end;

procedure TMainForm .ShowInfoForm (aCaption : String ; aInfoString : String );
begin
  ShowText(aCaption, aInfoString);
end;

procedure TMainForm.FullExpand1Click(Sender: TObject);
begin
  if Assigned(InWsdlTreeView.FocusedNode) then
    InWsdlTreeView.FullExpand(InWsdlTreeView.FocusedNode);
end;

procedure TMainForm.FullCollapse1Click(Sender: TObject);
begin
  if Assigned(InWsdlTreeView.FocusedNode) then
    InWsdlTreeView.FullCollapse(InWsdlTreeView.FocusedNode);
end;

procedure TMainForm.XmlZoomValueAsTextMenuItemClick(Sender: TObject);
var
  editAllowed: Boolean;
begin
  editAllowed := False;
  InWsdlTreeViewEditing(InWsdlTreeView, InWsdlTreeView.FocusedNode,
    treeValueColumn, editAllowed);
  xmlUtil.ZoomAsText(NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode),
    not editAllowed);
  if editAllowed then
  begin
    InWsdlTreeViewNewText(InWsdlTreeView, InWsdlTreeView.FocusedNode,
      treeValueColumn, xmlUtil.NewValue);
  end;
end;

procedure TMainForm.XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
begin
  xmlUtil.ZoomAsXml(NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode),
    True);
end;

procedure TMainForm.XsdOperationsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.xsdOperationsXml('');
  try
    if EditXmlXsdBased ( 'Xsd Operations'
                       , 'OperationDefs.XsdOperations'
                       , 'XsdOperations.Operation.Name'
                       , 'XsdOperations.Operation.Name'
                       , se.IsActive
                       , xXml.Items.Count > 1
                       , esUsed
                       , OperationDefsXsd
                       , xXml
                       ) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        se.xsdOperationsUpdate(xXml, se.projectFileName);
        PrepareOperation;
      finally
        ReleaseLock;
      end;
      CheckBoxClick(nil);
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.XsdOperationsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of XSD operations ' + HttpActiveHint;
end;

procedure TMainForm.XSDreportinClipBoardSpreadSheet1Click(Sender: TObject);
var
  xXml: TXml;
  xStrings: TStringList;
begin
  if not (NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) is TXml) then
    raise Exception.Create('Only implemented for Xml');
  xXml := (NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) as TXml);
  if not Assigned (xXml.Xsd)  then
    raise Exception.Create('Only possible for XSD based Xml''s');

  xStrings := TStringList.Create;
  try
    xXml.Xsd.GenerateReport(xStrings);
    Clipboard.AsText := xStrings.Text;
  finally
    xStrings.Free;
  end;
end;

procedure TMainForm.ShowChosenLogTab;
begin
  DocumentationPanel.Visible := False;
  NotificationsPanel.Visible := False;
  SnapshotsPanel.Visible := False;
  MessagesPanel.Visible := False;
  case LogTabControl.TabIndex of
    Ord (spDocumentation): DocumentationPanel.Visible := True;
    Ord (spNotifications):
    begin
      LogTabControl.Tabs [Ord (spNotifications)] := notifyTabCaption;
      NotificationsPanel.Visible := True;
    end;
    Ord (spSnapshots): SnapshotsPanel.Visible := True;
    Ord (spMessages): MessagesPanel.Visible := True;
  end;
end;

function TMainForm .GetAuthorization : Boolean ;
var
  Y, m, d: Word;
  ymd: Integer;
  xLicenseDate: TDateTime;
  xRpy, xReq: String;
  xTimestamp, xKey, xLicensed, xExpireDate: String;
begin
{$ifdef TrialVersion}
  xLicenseExpirationDate := {$I %date%}; // yyyy/mm/dd
                                         // 1234567890
  y := StrToInt(Copy (xLicenseExpirationDate, 1, 4));
  m := StrToInt(Copy (xLicenseExpirationDate, 6, 2));
  d := StrToInt(Copy (xLicenseExpirationDate, 9, 2));
  xLicenseDate := EncodeDate(2099, 12, 31);
  xLicenseExpirationDate := FormatDateTime('yyyy-mm-dd', xLicenseDate);
  result := ValidateLicenseExpirationDate(xLicenseExpirationDate);
  LicenseMenuItem.Enabled := False;
  Exit;
{$endif}
  result := False;
  xTimestamp := xsdNowAsDateTime;
  with TXml.CreateAsString ('getAuthorization', '') do
  try
    AddXml (TXml.CreateAsString('UserName', WindowsUserName));
    AddXml (TXml.CreateAsString('TimeStamp', xTimestamp));
    AddXml (TXml.CreateAsString('Program', _ProgName));
    AddXml (TXml.CreateAsString('Version', _xmlProgVersion));
    AddXml (TXml.CreateAsString('key', Sha1 (xTimestamp + '_JanBo')));
    LoadFromString (HttpPostDialog(Text, authorizationServerEndpoint), nil);
    xLicensed := Items.XmlValueByTag['authorized'];
    xExpireDate := Items.XmlValueByTag['expireDate'];
    CompanyName := Items.XmlValueByTag['licensee'];
    xKey := Items.XmlValueByTag['key'];
    if (xKey <> Sha1 ( WindowsUserName
                    + xTimestamp
                    + '^abra^'
                    + xLicensed
                    )) then
      raise Exception.Create ('Received inalid reply from Authorization server');
    if (xLicensed = 'true') then
      result := ValidateLicenseExpirationDate(xExpireDate) // warning...
    else
      raise Exception.CreateFmt('Not authorized%sLicense expiredate: %s', [LineEnding, xExpireDate]);
  finally
    Free;
  end;
end;

function TMainForm .GetAuthorizationBaseString : String ;
var
  xTimestamp: String;
begin
  result := '';
  xTimestamp := xsdNowAsDateTime;
  with TXml.CreateAsString ('getAuthorizationBaseString', '') do
  try
    AddXml (TXml.CreateAsString('key', Sha1 (xTimestamp + '_JanBo')));
    LoadFromString (HttpPostDialog(Text, authorizationServerEndpoint), nil);
    result := Items.XmlValueByTag['BaseString'];
  finally
    Free;
  end;
end;

procedure TMainForm.xsdSplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
  function SizeOfToolBar: Integer;
  var
    X: Integer;
  begin
    result := 6;
    with GridToolBar do
      for X := 0 to ControlCount - 1 do
        result := result + Controls[X].Width;
  end;

begin
  Accept := (DesignPanel.Width - ScriptPanel.Width - NewSize > SizeOfToolBar);
  // Accept := (NewSize > SizeOfToolBar);
end;

procedure TMainForm.ZoomasScriptAssignments1Click(Sender: TObject);
begin
  if not (NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) is TXml) then
    raise Exception.Create('Only implemented for Xml');
  ShowInfoForm( 'ScriptAssignments'
              , (NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) as TXml).asAssignments
              );
end;

procedure TMainForm.SetOperationZoomPath(aOperation: TWsdlOperation);
begin
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  with SelectXmlElementForm do
    try
      Caption := 'Select zoom element';
      LastCaption := aOperation.ZoomElementCaption;
      doShowReq := Assigned(aOperation.reqBind);
      doShowRpy := Assigned(aOperation.rpyBind);
      WsdlOperation := aOperation;
      IncludeRecurring := False;
      ShowModal;
      if ModalResult = mrOk then
      begin
        aOperation.ZoomElementCaption := SelectedCaption;
        stubChanged := True;
        MessagesVTS.Invalidate;
      end;
    finally
      FreeAndNil(SelectXmlElementForm);
    end;
end;

procedure TMainForm.OperationZoomOnActionExecute(Sender: TObject);
begin
  SetOperationZoomPath(WsdlOperation);
end;

procedure TMainForm.OperationZoomOnActionUpdate(Sender: TObject);
begin
  OperationZoomOnAction.Enabled := (Assigned(WsdlOperation)) and
    (Assigned(WsdlOperation.reqBind) or Assigned(WsdlOperation.rpyBind));
end;

procedure TMainForm.Expand2Click(Sender: TObject);
begin
  if Assigned(InWsdlTreeView.FocusedNode) then
  begin
    InWsdlTreeView.FullCollapse(nil);
    InWsdlTreeView.Expanded[InWsdlTreeView.FocusedNode] := True;
  end;
end;

procedure TMainForm.XmlSampleOperationsExecute (Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.xmlSampleOperationsXml('');
  try
    if EditXmlXsdBased ( 'XmlSample Operations'
                       , 'OperationDefs.XmlSampleOperations'
                       , 'XmlSampleOperations.Operation.Name'
                       , 'XmlSampleOperations.Operation.Name'
                       , se.IsActive
                       , xXml.Items.Count > 1
                       , esUsed
                       , OperationDefsXsd
                       , xXml
                       ) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        se.xmlSampleOperationsUpdate(xXml, se.projectFileName);
        PrepareOperation;
      finally
        ReleaseLock;
      end;
      CheckBoxClick(nil);
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm .XmlSampleOperationsHint (var HintStr : string ;
  var CanShow : Boolean );
begin
  HintStr := 'Maintain list of XmlSample operations ' + HttpActiveHint;
end;

procedure TMainForm.AfterRequestScriptButtonClick(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  xOperation := TWsdlOperation.Create(WsdlOperation);
  if xOperation.StubAction = saStub then Exit;
  try
    Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
    try
      EditOperationScriptForm.ScriptName := xOperation.Alias + ' / After Script';
      EditOperationScriptForm.After := True;
      EditOperationScriptForm.WsdlOperation := xOperation;
      EditOperationScriptForm.ScriptEdit.Lines.Text := xOperation.AfterScriptLines.Text;
      EditOperationScriptForm.ShowModal;
      if EditOperationScriptForm.ModalResult = mrOk then
      begin
        stubChanged := True;
        WsdlOperation.AfterScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
        try WsdlOperation.PrepareBefore; Except end;
        try WsdlOperation.PrepareAfter; Except end;
      end;
      FillInWsdlEdits;
    finally
      FreeAndNil(EditOperationScriptForm);
    end;
  finally
    xOperation.Free;
  end;
end;

procedure TMainForm.alGeneralExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  EndEdit;
end;

procedure TMainForm.EditScriptButtonClick(Sender: TObject);
var
  xOperation: TWsdlOperation;
  xScriptName: String;
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
    if xOperation.StubAction = saStub then
      xScriptName := ' / Main Script'
    else
      xScriptName := ' / Before Script';
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xOperation.Alias + xScriptName;
        EditOperationScriptForm.After := False;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ScriptEdit.Lines.Text := xOperation.BeforeScriptLines.Text;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          WsdlOperation.BeforeScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try WsdlOperation.PrepareBefore; Except end;
          try WsdlOperation.PrepareAfter; Except end;
        end;
        FillInWsdlEdits;
      finally
        FreeAndNil(EditOperationScriptForm);
      end;
    finally
      xOperation.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.UpdateInWsdlCheckBoxes;
var
  xNode: PVirtualNode;
  xData: PXmlTreeRec;
begin
  xNode := InWsdlTreeView.GetFirst;
  while Assigned(xNode) do
  begin
    xData := InWsdlTreeView.GetNodeData(xNode);
    if Assigned(xData.Bind) then
    begin
      if xData.Bind.Checked then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    xNode := InWsdlTreeView.GetNext(xNode);
  end;
end;

procedure TMainForm.SaveStubCaseAsActionExecute(Sender: TObject);
begin
  OnlyWhenLicensed;
  try
    EndEdit;
    SaveFileDialog.DefaultExt := 'wsdlStub';
    SaveFileDialog.FileName := se.projectFileName;
    SaveFileDialog.Filter := 'wsdlStub Case (*.wsdlStub)|*.wsdlStub';
    SaveFileDialog.Title := 'Save wsdlStub case';
    if SaveFileDialog.Execute then
    begin
      se.projectFileName := SaveFileDialog.FileName;
      SaveWsdlStubCase(se.projectFileName);
    end;
  finally
  end;
end;

procedure TMainForm.SaveWsdlStubCase(aFileName: String);
begin
  captionFileName := ExtractFileName(aFileName);
  XmlUtil.PushCursor (crHourGlass);
  try
    if Assigned (WsdlOperation) then
    begin
      se.FocusOperationName := WsdlOperation.reqTagName;
      se.FocusOperationNameSpace := WsdlOperation.reqTagNameSpace;
      se.FocusMessageIndex := WsdlOperation.Messages.IndexOfObject(WsdlMessage);
    end
    else
    begin
      se.FocusOperationName := '';
      se.FocusOperationNameSpace := '';
      se.FocusMessageIndex := 0;
    end;
    SaveStringToFile(aFileName, se.ProjectDesignAsString(aFileName));
    stubChanged := False;
    se.stubRead := True; // well,... but logically ...
    UpdateReopenList(ReopenCaseList, aFileName);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowReport (aReport : TSnapshot);
  procedure _ShowCoverageReport;
  var
    xList: TLogList;
    x: Integer;
  begin
    xList := TLogList.Create;
    try
      for x := 0 to se.displayedSnapshots.Count - 1 do
      begin
        if se.displayedSnapshots.SnapshotItems[x] is TRegressionSnapshot then
          se.OpenMessagesLog(se.displayedSnapshots.SnapshotItems[x].FileName, True, False, xList);
      end;
      CoverageReport(xList);
    finally
      xList.Clear;
      xList.Free;
    end;
  end;
  procedure _ShowRegressionReport;
  var
    xLogList, xRefLogList: TLoglist;
  begin
    xLogList := TLogList.Create;
    try
      se.OpenMessagesLog(aReport.FileName, True, False, xLogList);
      xRefLogList := TLogList.Create;
      try
        se.OpenMessagesLog(aReport.RefFileName, True, False, xRefLogList);
        aReport.Status := ShowLogDifferences(xLogList, xRefLogList, 'Current', aReport.Name);
        xRefLogList.Clear;
        xLogList.Clear;
      finally
        FreeAndNil (xRefLogList);
      end;
    finally
      FreeAndNil (xLogList);
    end;
  end;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    if aReport is TRegressionSnapshot then _ShowRegressionReport;
    if aReport is TCoverageReport then _ShowCoverageReport;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.OpenStubCaseActionExecute(Sender: TObject);
begin
  if not InactiveAfterPrompt then Exit;
  if OkToOpenStubCase then
  begin
    OpenFileDialog.DefaultExt := 'wsdlStub';
    OpenFileDialog.FileName := se.projectFileName;
    OpenFileDialog.Filter := 'wsdlStub case (*.wsdlStub)|*.wsdlStub';
    OpenFileDialog.Title := 'Open Stub Case';
    if OpenFileDialog.Execute then
    begin
      se.projectFileName := OpenFileDialog.FileName;
      OpenStubCase(se.projectFileName);
      // TProcedureThread.Create (OpenStubCase, OpenFileDialog.FileName);
    end;
  end;
end;

procedure TMainForm.OpenStubCase(aFileName: String);
var
  f: Integer;
begin
  captionFileName := ExtractFileName(aFileName);
  ProjectDesignFromString(ReadStringFromFile(aFileName), aFileName);
  UpdateReopenList(ReopenCaseList, aFileName);
  if allOperations.Find (se.FocusOperationName + ';' + se.FocusOperationNameSpace, f) then
  begin
    WsdlOperation := allOperations.Operations[f];
    if (se.FocusMessageIndex < WsdlOperation.Messages.Count)
    and (se.FocusMessageIndex > -1) then
      WsdlMessage := WsdlOperation.Messages.Messages[se.FocusMessageIndex];
  end;
end;

function TMainForm.ReactivateCommand: String;
begin
  result := 'Instance of ' + _progName + ' reactivated ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Reactivate refused because instance of ' + _progName + ' is inactive');
      try
        se.Activate(False);
        // IsActive := False;
        Sleep(1000); // allow mq threads some time
        se.Activate(True);
        // IsActive := xActive;
        if not se.IsActive then
          raise Exception.Create(
            'Reactivate failed, see instance exceptionlog for details');
      finally
      end;
    finally
      // ReleaseLock;
    end;
  except
    on E: Exception do
      result := E.Message + LineEnding + LineEnding + ExceptionStackListString(e);
  end;
end;

function TMainForm.QuitCommand(aDoRaiseExceptions: Boolean): String;
begin
  {$ifdef windows}
  result := 'Instance of ' + _progName + ' is shutting down ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Shurtdown refused because instance of ' + _progName +
            ' is inactive');
      if stubChanged then
        raise Exception.Create(
          'Shurtdown refused because of unsaved changes in design');
      try
        se.Activate(False);
        // IsActive := False;
        Sleep(2000); // allow mq threads some time
        PostMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      finally
      end;
    finally
      // ReleaseLock;
    end;
  except
    on E: Exception do
    begin
      result := E.Message + LineEnding + LineEnding + ExceptionStackListString(e);
      if aDoRaiseExceptions then
        raise Exception.Create(result);
    end;
  end;
  {$else}
  result := 'QuitCommand not implemented';
  {$endif}
end;

function TMainForm.RestartCommand: String;
  procedure _Exec(const CommandLine: string);
  begin
  end;

begin
{$ifdef windows}
  result := 'instance of ' + _progName +
    ' will restart, try after some time... ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Restart refused because instance of ' + _progName +
            ' is inactive');
      try
        se.Activate(False);
        // IsActive := False;
        Sleep(1000); // allow mq threads some time
        _Exec('"' + ParamStr(0) + '" "' + se.projectFileName + '"');
        PostMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
      finally
      end;
    finally
      // ReleaseLock;
    end;
  except
    on E: Exception do
      result := E.Message + LineEnding + LineEnding + ExceptionStackListString(e);
  end;
{$else}
  result := 'RestartCommand not implemented';
{$endif}
end;

procedure TMainForm.ReleaseLock;
begin
//  if False then Wsdlz.ReleaseLock;
end;

function TMainForm.ReloadDesignCommand: String;
begin
  result := 'Instance of ' + _progName + ' reloaded ' +
    se.projectFileName + ' successfully';
  try
    AcquireLock;
    try
      if stubChanged then
        raise Exception.Create
          ('Reload refused because instance of ' + _progName +
            ' has unsaved changes in design');
      if not Assigned(WsdlOperation) then
        raise Exception.Create(
          'Reload refused because instance has no project loaded');
      if not se.IsActive then
        raise Exception.Create
          ('Reload refused because instance of ' + _progName +
            ' is inactive');
      se.Activate(False);
      OpenStubCase(se.projectFileName);
      se.Activate(True);
    finally
      ReleaseLock;
    end;
  except
    on E: Exception do
      result := E.Message;
  end;
end;

procedure TMainForm.ProjectCleanActionExecute(Sender: TObject);
begin
  try
    se.Clean;
  finally
    stubChanged := True;
    OperationReqsTreeViewFocusChanged(OperationReqsTreeView,
      OperationReqsTreeView.FocusedNode, OperationReqsTreeView.FocusedColumn);
  end;
end;

procedure TMainForm.ProjectDesignFromString(aString, aMainFileName: String);
begin
  { }
  InWsdlTreeView.BeginUpdate;
  GridView.BeginUpdate;
  OperationReqsTreeView.BeginUpdate;
  { }
  try
    GridView.Clear;
    ClearConsole;
    doValidateRequests := False;
    doValidateReplies := False;
    doCheckExpectedValues := False;
    _WsdlDisableOnCorrelate := False;
    XmlUtil.PushCursor (crHourGlass);
    try
      se.ProjectDesignFromString(aString, aMainFileName);
      AcquireLock;
      try
        PrepareOperation;
        CreateEnvironmentSubMenuItems;
        CreateScriptsSubMenuItems;
        LogUpdateColumns;
        if se.Wsdls.Count > -1 then
          UpdateConsole(0)
        else
          UpdateConsole(-1);
      finally
        ReleaseLock;
      end;
    finally
      { }
      XmlUtil.PopCursor;
      stubChanged := se.stubChanged;
      se.stubRead := True;
      { }
    end;
  finally
    OperationReqsTreeView.EndUpdate;
    GridView.EndUpdate;
    InWsdlTreeView.EndUpdate;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.ReopenStubCaseActionExecute(Sender: TObject);
var
  X: Integer;
  ChoosenString: String;
  FileName: String;
begin
  if not InactiveAfterPrompt then Exit;
  if OkToOpenStubCase then
  begin
    Application.CreateForm(TChooseStringForm, ChooseStringForm);
    try
      ChooseStringForm.ListBox.Clear;
      for X := 0 to ReopenCaseList.Count - 1 do
      begin
        ChooseStringForm.ListBox.Items.Add
          (IntToStr(X) + ' ' + ReopenCaseList.Strings[X]);
      end;
      ChooseStringForm.Caption := 'Open recent Stub case';
      ChooseStringForm.ShowModal;
      if ChooseStringForm.ModalResult = mrOk then
      begin
        ChoosenString := ChooseStringForm.ChoosenString;
        FileName := Copy(ChoosenString, 3, Length(ChoosenString) - 2);
        se.projectFileName := FileName;
        OpenStubCase(FileName);
      end;
    finally
      FreeAndNil(ChooseStringForm);
    end;
  end;
end;

procedure TMainForm.UpdateReopenList(aList: TStringList; aFileName: String);
var
  X: Integer;
begin
  for X := 0 to aList.Count - 1 do
  begin
    if aList.Strings[X] = aFileName then
    begin
      aList.Delete(X);
      Break;
    end;
  end;
  aList.Insert(0, aFileName);
  if aList.Count = 11 then
    aList.Delete(10);
end;

procedure TMainForm.UpdateVisibiltyOfOperations;
var
  Node: PVirtualNode;
  xOperation: TWsdlOperation;
  xMenuItem: TMenuItem;
begin
  UnhideOperationMenuItem.OnClick := nil;
  UnhideOperationMenuItem.Clear;
  Node := OperationReqsTreeView.GetFirst;
  while Assigned(Node) do
  begin
    xOperation := NodeToOperation(OperationReqsTreeView, Node);
    OperationReqsTreeView.IsVisible[Node] := not xOperation.HiddenFromUI;
    if xOperation.HiddenFromUI then
    begin
      xMenuItem := TMenuItem.Create(nil);
      xMenuItem.Tag := PtrInt(xOperation);
      xMenuItem.Caption := xOperation.Alias;
      xMenuItem.OnClick := UnhideOperationMenuItemClick;
      UnhideOperationMenuItem.Add(xMenuItem);
    end;
    Node := OperationReqsTreeView.GetNext(Node);
  end;
  UnhideOperationMenuItem.Enabled := (UnhideOperationMenuItem.Count > 0);
  UnhideAllOperationsAction.Enabled := UnhideOperationMenuItem.Enabled;
end;

procedure TMainForm.UpdateVisibiltyTreeView(aFreeFormat: Boolean);
begin
  if aFreeFormat then
  begin
    InWsdlTreeView.Align := alLeft;
    InWsdlTreeView.Visible := False;
    FreeFormatMemo.Align := alClient;
    FreeFormatMemo.Visible := True;
  end
  else
  begin
    FreeFormatMemo.Align := alRight;
    FreeFormatMemo.Visible := False;
    InWsdlTreeView.Align := alClient;
    InWsdlTreeView.Visible := True;
  end;
end;

procedure TMainForm.StartBlockingThreadEvent ;
begin
  if NumberOfBlockingThreads = 0 then
  begin
    ExecuteRequestToolButton.Down := True;
    ExecuteAllRequestsToolButton.Down := True;
    se.ProgressPos := 0;
    XmlUtil.PushCursor (crHourGlass);
    abortPressed := False;
  end;
  Inc (NumberOfBlockingThreads);
end;

procedure TMainForm.TerminateBlockingThreadEvent ;
begin
  Dec (NumberOfBlockingThreads);
  if (NumberOfBlockingThreads <= 0) then
  begin
    NumberOfBlockingThreads := 0;
    ExecuteRequestToolButton.Down := False;
    ExecuteAllRequestsToolButton.Down := False;
    XmlUtil.PopCursor;
    se.ProgressPos := 0;
    if NumberOfNonBlockingThreads <= 0 then
    begin
      abortPressed := False;
    end;
    UpdateInWsdlCheckBoxes;
    GridView.Invalidate;
    InWsdlTreeView.Invalidate;
  end;
end;

procedure TMainForm.StartNonBlockingThreadEvent ;
begin
  if NumberOfNonBlockingThreads = 0 then
  begin
    LogTabControl.TabIndex := Ord (spMessages);
    abortPressed := False;
  end;
  Inc (NumberOfNonBlockingThreads);
end;

procedure TMainForm.TerminateNonBlockingThreadEvent ;
begin
  Dec (NumberOfNonBlockingThreads);
  if (NumberOfNonBlockingThreads <= 0) then
  begin
    LogTabControl.TabIndex := Ord (spMessages);
    if NumberOfBlockingThreads <= 0 then
      abortPressed := False;
  end;
end;

procedure TMainForm.SetUiProgress;
begin
  if (ProgressBar.Max <> se.ProgressMax)
  or (ProgressBar.Position <> se.ProgressPos) then
  begin
    ProgressBar.Max := se.ProgressMax;
    if se.ProgressMax > 0 then
      ProgressBar.Position := se.ProgressPos
    else
      ProgressBar.Position := 0;
  end;
  ThreadsPanel.Caption
    := IfThen ( NumberOfBlockingThreads + NumberOfNonBlockingThreads = 0
              , ''
              , 'Threads: '
              + IntToStr(NumberOfBlockingThreads + NumberOfNonBlockingThreads)
              );
end;

procedure TMainForm .OnRegressionReport (aReport : TRegressionSnapshot );
begin
  aReport.Status := rsOk;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  Application.CreateForm(TAboutBox, AboutBox);
  try
    AboutBox.ProgName := _progName;
    AboutBox.LicensedTo := CompanyName;
    AboutBox.VersionInfo:= _xmlProgVersion;
    AboutBox.ShowModal;
  finally
    FreeAndNil(AboutBox);
  end;
end;

procedure TMainForm.HelpActionExecute(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := SetDirSeparators ( ExtractFilePath(ParamStr(0))
                                + 'Documentation'
                                + DirectorySeparator
                                + _progName
                                + '.htm'
                                );
  if not LazFileUtils.FileExistsUTF8(xFileName) { *Converted from FileExists* } then
    raise Exception.Create('Could not find helpfile: ' + xFileName);
  if not OpenDocument(xFileName) then
    raise Exception.Create('Could not open ' + xFileName);
end;

procedure TMainForm.HelpMainMenuActionExecute(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := SetDirSeparators ( ExtractFilePath(ParamStr(0))
                                + 'Documentation'
                                + DirectorySeparator
                                + _progName
                                + '_Menu_hlp.htm'
                                );
  if not LazFileUtils.FileExistsUTF8(xFileName) { *Converted from FileExists* } then
    raise Exception.Create('Could not find helpfile: ' + xFileName);
  if not OpenDocument(xFileName) then
    raise Exception.Create('Could not open ' + xFileName);
end;

procedure TMainForm.HideAllOperationsActionExecute(Sender: TObject);
var
  X: Integer;
begin
  for X := 0 to allOperations.Count - 1 do
  begin
    if not allOperations.Operations[X].HiddenFromUI then
    begin
      stubChanged := True;
      allOperations.Operations[X].HiddenFromUI := True;
    end;
  end;
  UpdateVisibiltyOfOperations;
end;

function TMainForm.hintStringFromXsd(aPrefix, aSep, aPostFix: String;
  aXsd: TXsd): String;
var
  X: Integer;
  xSep: String;
begin
  result := '';
  xSep := aPrefix;
  for X := 0 to aXsd.sType.ElementDefs.Count - 1 do
  begin
    result := result + xSep + aXsd.sType.ElementDefs.Xsds[X].ElementName;
    xSep := aSep;
  end;
  result := result + aPostFix;
end;

procedure TMainForm.OnlyWhenLicensed;
begin
  if not se.Licensed then
    raise Exception.Create(
      'Function is disabled because you are not a licensed ' + _progName +
        ' user');
end;

function TMainForm .LogMaxEntriesEqualsUnbounded(aCaption: String): Boolean ;
begin
  if se.displayedLogsmaxEntries > - 1 then
    if BooleanPromptDialog
        (Format ('Max Log entries now "%d",%s"%s" requires it to be "unbounded".%sSet Log maxEntries to "unbounded"'
                , [se.displayedLogsmaxEntries, LineEnding, aCaption, LineEnding]
                )
        ) then
    begin
      se.displayedLogsmaxEntries := - 1;
      stubChanged := True;
    end;
  result := (se.displayedLogsmaxEntries = - 1);
end;

procedure TMainForm.wsdlStubInitialise;
begin
  se.Licensed := False;
  WindowsUserName := getUserName;
  if authorizationServerEndpoint <> '' then
  begin
    try
      se.Licensed := GetAuthorization;
    except
      on e: Exception do
        GetAuthError := e.Message;
    end;
  end
  else
  begin
    OpenLogUsageDatabase;
    LogUsage(WindowsUserName);
    ValidateLicense;
    SqlConnector.Connected := False;
  end;
  SetLogUsageTimer;
  ConfigListenersAction.Hint := hintStringFromXsd('Configure listeners (',
    ', ', ')', listenersConfigXsd);
end;

procedure TMainForm.LogUpdateColumns;
var
  X: Integer;
begin
  while MessagesVTS.Header.Columns.Count > Ord(logStdColumnCount) do
    MessagesVTS.Header.Columns.Delete(MessagesVTS.Header.Columns.Count - 1);
  for X := 0 to se.DisplayedLogColumns.Count - 1 do
    with MessagesVTS.Header.Columns.Add do
      Text := se.DisplayedLogColumns[X];
end;

function TMainForm.LogUsage(aUserName: String): Boolean;
var
  xUpdated: TDateTime;
  xUsageDate: TDateTime;
begin
  result := True;
{$ifdef linux}
  exit;
{$endif}
  xUpdated := Now;
  xUsageDate := sysutils.Date;
  if (aUserName <> 'JanBo')
  and (aUserName <> 'Bouwman')
  and (SqlConnector.Connected) then
  begin
    SqlConnector.Transaction.StartTransaction;
    try
      try
        Qry.SQL.Clear;
        Qry.SQL.Add('Insert into UsageNames');
        Qry.SQL.Add('( UserName');
        Qry.SQL.Add(', nUsage');
        Qry.SQL.Add(', Updated');
        Qry.SQL.Add(') values');
        Qry.SQL.Add('( :UserName');
        Qry.SQL.Add(', 1');
        Qry.SQL.Add(', :Updated');
        Qry.SQL.Add(')');
        Qry.Params.ParamValues['UserName'] := aUserName;
        Qry.Params.ParamValues['Updated'] := xUpdated;
        Qry.ExecSql;
      except
        on E: Exception do
        begin
          try
            Qry.SQL.Clear;
            Qry.SQL.Add('Update UsageNames');
            Qry.SQL.Add('set nUsage = nUsage + 1');
            Qry.SQL.Add('  , Updated = :Updated');
            Qry.SQL.Add('where UserName = :UserName');
            Qry.Params.ParamValues['Updated'] := xUpdated;
            Qry.Params.ParamValues['UserName'] := aUserName;
            Qry.ExecSql;
          except
          end;
        end; { try to update UsageNames when insert failed }
      end; { try to insert UsageNames }

      try
        Qry.SQL.Clear;
        Qry.SQL.Add('Insert into UsageDates');
        Qry.SQL.Add('( UsageDate');
        Qry.SQL.Add(', nUsage');
        Qry.SQL.Add(', Updated');
        Qry.SQL.Add(') values');
        Qry.SQL.Add('( :UsageDate');
        Qry.SQL.Add(', 1');
        Qry.SQL.Add(', :Updated');
        Qry.SQL.Add(')');
        Qry.Params.ParamValues['UsageDate'] := xUsageDate;
        Qry.Params.ParamValues['Updated'] := xUpdated;
        Qry.ExecSql;
      except
        on E: Exception do
        begin
          try
            Qry.SQL.Clear;
            Qry.SQL.Add('Update UsageDates');
            Qry.SQL.Add('set nUsage = nUsage + 1');
            Qry.SQL.Add('  , Updated = :Updated');
            Qry.SQL.Add('where UsageDate = :UsageDate');
            Qry.Params.ParamValues['Updated'] := xUpdated;
            Qry.Params.ParamValues['UsageDate'] := xUsageDate;
            Qry.ExecSql;
          except
          end;
        end; { try to update UsageDates when insert failed }
      end; { try to insert UsageDates }
    finally
      SqlConnector.Transaction.Commit;
    end; // try
  end; { if user <> JAN BOUWMAN }
end;

function TMainForm.OpenLogUsageDatabase: Boolean;
begin
  result := False;
{$ifdef linux}
  exit;
{$endif}
  try
    try
      SqlConnector.Connected := False;
    except
    end;
    if LazFileUtils.FileExistsUTF8(licenseDatabaseName) then
    begin
      try
        SQLConnector.ConnectorType := 'odbc';
        SqlConnector.Params.Text := 'DBQ=' + licenseDatabaseName;
        SqlConnector.Connected := True;
      except
        ShowMessage ( 'Can not open database: '
                    + licenseDatabaseName
                    + LineEnding
                    + LineEnding
                    + 'Please contact your '
                    + _ProgName
                    + ' provider for assistence'
                    );
//        Close;
      end;
    end
    else
    begin
      ShowMessage ( 'Can not find database: '
                  + licenseDatabaseName
                  + LineEnding
                  + LineEnding
                  + 'Please contact your '
                  + _ProgName
                  + ' provider for assistence'
                  );
//      Close;
    end;
    result := SqlConnector.Connected;
  except
  end;
end;


procedure TMainForm.ActivateCommand(aActivate: Boolean);
begin
  if aActivate <> se.IsActive then
  begin
    try
      se.Activate(aActivate);
    finally
      CheckBoxClick(nil);
    end;
  end;
end;

procedure TMainForm.OpenProjectCommand(aProject: String);
begin
  AcquireLock;
  try
    if stubChanged then
      raise Exception.Create
        ('Reload refused because instance of ' + _progName +
          ' has unsaved changes in design');
    se.Activate(False);
    OpenStubCase(aProject);
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.ValidateLicense;
var
  Y, m, d: Word;
  ymd: Integer;
  xLicenseDate: TDateTime;
const
  xDisableFunctions = 'Therefore the Save Project as... and some reporting functions are disabled.';
begin
{$ifdef TrialVersion}
  xLicenseExpirationDate := {$I %date%}; // yyyy/mm/dd
                                         // 1234567890
  y := StrToInt(Copy (xLicenseExpirationDate, 1, 4));
  m := StrToInt(Copy (xLicenseExpirationDate, 6, 2));
  d := StrToInt(Copy (xLicenseExpirationDate, 9, 2));
  xLicenseDate := EncodeDate(y, m, d) + 180;
  xLicenseExpirationDate := FormatDateTime('yyyy-mm-dd', xLicenseDate);
  se.Licensed := ValidateLicenseExpirationDate(xLicenseExpirationDate);
  LicenseMenuItem.Enabled := False;
  Exit;
{$endif}
{$ifdef linux}
  se.Licensed := True;
  exit;
{$endif}
  se.Licensed := False;
  ErrorReadingLicenseInfo := True;
  if SqlConnector.Connected then
  try
    ErrorReadingLicenseInfo := False;
    Qry.SQL.Clear;
    Qry.SQL.Add('Select CompanyName, LicenseExpireDate, LicenseString');
    Qry.SQL.Add('from LicenseInformation');
    Qry.ParseSQL := False;
    Qry.Open;
    while not Qry.EOF do
    begin
      CompanyName := Qry.FieldByName('CompanyName').AsString;
      xLicenseExpirationDate := Qry.FieldByName('LicenseExpireDate').AsString;
      xLicenseString := Qry.FieldByName('LicenseString').AsString;
      Qry.Next;
    end;
    Qry.Close;
    se.Licensed := (validateIpmLicense( CompanyName
                                      + xLicenseExpirationDate
                                      + generateIpmLicense(licenseDatabaseName)
                                      , xLicenseString
                                      )
                   )
               and (   (AnsiStartsStr('\\', licenseDatabaseName))
                    or (WindowsUserName = 'Jan')
                    or (WindowsUserName = 'Bouwman')
                    or (WindowsUserName = 'BouwmanJW')
                   );
  except
    on E: Exception do
    begin
      { }
      ShowMessage(E.Message);
      ErrorReadingLicenseInfo := True;
      { } {
        ErrorReadingLicenseInfo := False;
        se.Licensed := True;
        xLicenseExpirationDate := '2013-10-01';
        { }
    end;
  end;

  if se.Licensed and (not ErrorReadingLicenseInfo) then
  begin
    se.Licensed := ValidateLicenseExpirationDate(xLicenseExpirationDate);
    if se.Licensed then // set a license for four days for stand alone pc usage
    begin
      DecodeDate(Now + 14, Y, m, d);
      with TFormIniFile.Create(self, False) do
      try
        IntegerByName['LicenseExpirationDate'] := 10000 * Y + 100 * m + d;
        StringByName['LicenseKey'] := generateIpmLicense(IntToStr(10000 * Y + 100 * m + d));
      finally
        Free;
      end;
    end;
  end
  else
  begin
    if ErrorReadingLicenseInfo then
    begin
      with TFormIniFile.Create(self, False) do
      try
        // first try if we have a stand alone license
        ymd := IntegerByName['LicenseExpirationDate'];
        d := ymd mod 100;
        ymd := ymd div 100;
        m := ymd mod 100;
        Y := ymd div 100;
        xLicenseDate := EncodeDate(Y, m, d);
        if (Now <= xLicenseDate) and ((Now + 16) > xLicenseDate) then
          se.Licensed := validateIpmLicense(IntToStr(10000 * Y + 100 * m + d),
            StringByName['LicenseKey']);
        if se.Licensed then
          ShowMessage('' + _progName +
              ' could not read license information from the server.' + LineEnding +
              LineEnding + 'Your offline license is valid until ' + DateToStr
              (xLicenseDate))
        else
          ShowMessage('' + _progName +
              ' could not read the license information.' + LineEnding +
              LineEnding + xDisableFunctions
              + LineEnding + LineEnding
              + 'Please contact your ' + _progName + ' provider for assistance.');
      finally
        Free;
      end;
    end
    else
    begin
      { }
      ShowMessage('' + _progName + ' did not find a valid licensestring.' +
          LineEnding + LineEnding + xDisableFunctions
            + LineEnding + LineEnding +
          'Please contact your ' + _progName + ' provider for' + LineEnding +
          'a valid licensestring or technical assistance.');
      { }
    end;
  end;
end;

function TMainForm.ValidateLicenseExpirationDate(eDt: String): Boolean;
var
  xDt: TDateTime;
  xYear, xMonth, xDay: Word;
begin
  // 2007-01-01
  // 1234567890
  xYear := StrToInt(Copy(eDt, 1, 4));
  xMonth := StrToInt(Copy(eDt, 6, 2));
  xDay := StrToInt(Copy(eDt, 9, 2));
  xDt := EncodeDate(xYear, xMonth, xDay);
  result := (Now < xDt);
  if not result then
  begin
    ShowMessage('Your ' + _progName + ' license has expired on ' + DateToStr
        (xDt) + '.' + LineEnding + LineEnding + 'Therefore ' + _progName +
        ' will have limited functionallity.' + LineEnding + LineEnding +
        'Please contact your ' + _progName + ' provider.');
  end
  else
  begin
    if ((xDt - Now) < 30) then
      ShowMessage('Your ' + _progName + ' license expires on ' + DateToStr(xDt)
          + LineEnding + 'Please contact your ' + _progName + ' provider');
  end;
end;

procedure TMainForm.LicenseMenuItemClick(Sender: TObject);
  function _getBaseString: String;
  begin
    result := '';
    with TXml.CreateAsString('getAuthorizationBaseString', '') do
    try
      AddXml (TXml.CreateAsString('key', Sha1 (xsdNowAsDateTime + '_JanBo')));
      LoadFromString (HttpPostDialog(Text, authorizationServerEndpoint), nil);
      if Name = 'wsAutorizationBaseString' then
        result := Items.XmlValueByTag['value'];
    finally
      Free;
    end;
  end;
var
  xResult: String;
begin
  if ErrorReadingLicenseInfo then
    raise Exception.Create('' + _progName +
        ' could not read the license information.' + LineEnding + LineEnding +
        'Please contact your ' + _progName + ' provider for assistance.');

  Application.CreateForm(TIpmGunLicenseForm, IpmGunLicenseForm);
  try
    IpmGunLicenseForm.Caption := '' + _progName + ' - License information';
    IpmGunLicenseForm.Company := CompanyName;
    IpmGunLicenseForm.LicenseExpirationDate := xLicenseExpirationDate;
    IpmGunLicenseForm.BaseString := _getBaseString;
    IpmGunLicenseForm.ShowModal;
    if IpmGunLicenseForm.ModalResult = mrOk then
    begin
      with TXml.CreateAsString ('setAuthorization', '') do
      try
        AddXml (TXml.CreateAsString('Company', IpmGunLicenseForm.Company));
        AddXml (TXml.CreateAsString('LicenseDate', IpmGunLicenseForm.LicenseExpirationDate));
        AddXml (TXml.CreateAsString('LicenseString', IpmGunLicenseForm.LicenseString));
        xResult :=  HttpPostDialog(Text, authorizationServerEndpoint);
        LoadFromString (xResult, nil);
        if Name <> 'wsAutorizationSet' then
          raise Exception.Create ('setAuthorization exception:' + LineEnding + xResult);
        se.Licensed := GetAuthorization;
      finally
        Free;
      end;
    end;
  finally
    FreeAndNil(IpmGunLicenseForm);
  end;
end;

function TMainForm.LicenseProvider(aRequest: String): String;
var
  xUser: String;
  xTimeStamp: String;
  xReceivedHash: String;
begin
  result := '';
  with TXml.Create do
    try
      try
        if not se.Licensed then
          raise Exception.Create
            ('' +
              _progName + ' license-server not licensed');
        LoadFromString(aRequest, nil);
        xUser := Items.XmlValueByTag['User'];
        if xUser = '' then
          raise Exception.Create('No UserId found');
        xTimeStamp := Items.XmlValueByTag['TimeStamp'];
        if xTimeStamp = '' then
          raise Exception.Create('No TimeStamp provided');
        xReceivedHash := Items.XmlValueByTag['Hash'];
        if Sha1(xUser + ';' + xTimeStamp + xTimeStamp + '..##..')
          <> xReceivedHash then
          raise Exception.Create('Suspect request, refused');
        Items.XmlValueByTag['Hash'] := EncodeStringBase64
          (xmlUtil.SimpleEncrypt(xReceivedHash));
        result := Text;
      except
        on E: Exception do
        begin
          result := E.Message;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.runScriptActionExecute(Sender: TObject);
begin
{
  if Sender is TMenuItem then
    xMenuItem := Sender as TMenuItem;
  if Sender is TAction then with Sender as TAction do
    if ActionComponent is TMenuItem then with ActionComponent as TMenuItem do
      xMenuItem := ActionComponent as TMenuItem;
  if Assigned (xMenuItem) then
    xMenuItem.OnClick (xMenuItem);
}
end;

procedure TMainForm.PrepareOperation;
begin
  se.PrepareAllOperations(LogServerException);
  WsdlOperationsComboBox.Clear;
  WsdlServicesComboBox.Clear;
  WsdlComboBox.Items.Text := se.Wsdls.Text;
  FillOperationReqsTreeView(OperationReqsTreeView, allAliasses);
  if se.scriptErrorCount > 0 then
    ShowMessage(IntToStr(se.scriptErrorCount) +
        ' Script(s) found with errors, see Exceptions log');
end;

procedure TMainForm.setWsdl(const Value: TWsdl);
begin
  if Assigned(Value) and (Value is TWsdl) then
    WsdlComboBox.ItemIndex := se.Wsdls.IndexOfObject(Value)
  else
    WsdlComboBox.ItemIndex := -1;
end;

function TMainForm.getWsdl: TWsdl;
begin
  if WsdlComboBox.ItemIndex < 0 then
    result := nil
  else
  begin
    try
      result := se.Wsdls.Objects[WsdlComboBox.ItemIndex]
        as TWsdl except result := nil;
    end;
  end;
end;

procedure TMainForm.WsdlComboBoxChange(Sender: TObject);
begin
  WsdlPopulateServices(Wsdl);
end;

procedure TMainForm.ClearConsole;
begin
  RemoveMessageColumns;
  DocumentationMemo.Clear;
  SnapshotsVTS.Clear;
  SnapshotsVTS.Header.SortColumn := -1;
  SnapshotsVTS.Header.SortDirection := sdAscending;
  MessagesVTS.Clear;
  MessagesVTS.Header.SortColumn := -1;
  MessagesVTS.Header.SortDirection := sdAscending;
  LogMemo.Clear;
  GridView.Clear;
  ExceptionMemo.Clear;
  ExceptionsVTS.Clear;
  OperationReqsTreeView.Clear;
  // InWSdlEnumerationsListView.Clear;
  InWsdlPropertiesListView.Clear;
  OperationDocumentationViewer.Canvas.Clear;
  InWsdlTreeView.Clear;
  WsdlServicesComboBox.Clear;
  WsdlOperationsComboBox.Clear;
  WsdlComboBox.Clear;
  while MessagesVTS.Header.Columns.Count > Ord(logStdColumnCount) do
    MessagesVTS.Header.Columns.Delete(MessagesVTS.Header.Columns.Count - 1);
end;

procedure TMainForm.UpdateConsole(aIndex: Integer);
begin
  WsdlComboBox.ItemIndex := aIndex;
  WsdlPopulateServices(Wsdl);
end;

procedure TMainForm.ReopenStubCaseActionUpdate(Sender: TObject);
begin
  ReopenStubCaseAction.Enabled := (ReopenCaseList.Count > 0)
                                ;
end;

procedure TMainForm.CheckBoxClick(Sender: TObject);
  procedure setTreeviewColors(aVST: TVirtualStringTree);
  begin
    if se.IsActive and False then
    begin
      aVST.ParentColor := True;
      aVST.Colors.UnfocusedSelectionColor := clWhite;
      aVST.Colors.GridLineColor := clWhite;
    end
    else
    begin
      aVST.Color := clWhite;
      aVST.Colors.UnfocusedSelectionColor := clBtnFace;
      aVST.Colors.GridLineColor := clBtnFace;
    end;
    aVST.Invalidate;
  end;

  procedure setMemoColors(aMemo: TMemo);
  begin
    if se.IsActive and False then
    begin
      aMemo.ParentColor := True;
    end
    else
    begin
      aMemo.Color := clWhite;
    end;
    aMemo.Invalidate;
  end;

begin
  try
    setMemoColors(FreeFormatMemo);
  except
  end;
  try
    setTreeviewColors(InWsdlTreeView);
  except
  end;
  try
    setTreeviewColors(GridView);
  except
  end;
  ToggleCheckExpectedValuesAction.Checked := doCheckExpectedValues;
  ValidateRepliesAction.Checked := doValidateReplies;
  ValidateRequestsAction.Checked := doValidateRequests;
  ActionComboBox.Enabled := Assigned(WsdlOperation) and
    (WsdlOperation.WsdlService.DescriptionType <> ipmDTEmail);
  WsdlItemAddMenuItem.Enabled := True;
  WsdlPasteFromClipboardMenuItem.Enabled := True;
  WsdlPopulateMenuItem.Enabled := True;
  FreeFormatMemo.ReadOnly := se.IsActive and False;
  DocumentationMemo.ReadOnly := se.IsActive and False;
  DocumentationMemo.ParentColor := DocumentationMemo.ReadOnly;
  if not DocumentationMemo.ParentColor then
    DocumentationMemo.Color := clWindow;
  if se.IsActive then
  begin
    LogTabControl.TabIndex := Ord (spMessages);
    MessagesTabControl.TabIndex := Ord(slRequestBody);
    if se.IsActive then
    begin
      Application.Title := '' + _progName + ' (Active)';
      startAction.ShortCut := 0;
      stopAction.ShortCut := startStopShortCut;
      startStopButton.Action := stopAction;
    end;
    if nStubs > freeStubs then
      freeStubs := nStubs + 10 + Random(10);
  end
  else
  begin
    Application.Title := '' + _progName + '';
    stopAction.ShortCut := 0;
    startAction.ShortCut := startStopShortCut;
    startStopButton.Action := startAction;
  end;
  startStopMenuItem.Action := startStopButton.Action;
end;

procedure TMainForm.OptionsActionUpdate(Sender: TObject);
begin
  OptionsAction.Enabled := (not se.IsActive);
end;

function TMainForm.OptionsAsXml: TXml;
begin
  result := TXml.CreateAsString('wsdlStubOptions', '');
  with result.AddXml(TXml.CreateAsString('General', '')) do
  begin
    AddXml(TXml.CreateAsBoolean('ConfirmRemovals', xmlUtil.doConfirmRemovals));
    AddXml(TXml.CreateAsBoolean('ScrollMessagesIntoView',
        doScrollMessagesIntoView));
    AddXml(TXml.CreateAsBoolean('ScrollExceptionsIntoView',
        doScrollExceptionsIntoView));
    AddXml(TXml.CreateAsBoolean('CheckScriptAssignments',
        xsdValidateAssignmentsAgainstSchema));
    AddXml(TXml.CreateAsBoolean('InitialCollapseHeaders', CollapseHeaders));
    AddXml(TXml.CreateAsBoolean('CollapseXmlNodeOnUncheck',
        xmlUtil.doCollapseOnUncheck));
    AddXml(TXml.CreateAsBoolean('ExpandXmlNodeOnCheck',
        xmlUtil.doExpandOnCheck));
  end;
  with result.AddXml(TXml.CreateAsString('Http', '')) do
  begin
    // AddXml (TXml.CreateAsBoolean('KeepAlive', se.HTTPServer.KeepAlive));
    AddXml(TXml.CreateAsInteger('ListenQueue', se.HTTPServer.ListenQueue));
    AddXml(TXml.CreateAsInteger('MaxConnections', se.HTTPServer.MaxConnections)
      );
    with AddXml(TXml.CreateAsString('ProxyServer', '')) do
    begin
      AddXml(TXml.CreateAsBoolean('Enabled', se.doViaProxyServer));
      AddXml(TXml.CreateAsString('Host', se.ViaProxyServer));
      AddXml(TXml.CreateAsInteger('Port', se.ViaProxyPort));
    end;
    with result.AddXml(TXml.CreateAsString('Mq', '')) do
    begin
      case se.mqUse of
        mquUndefined:
          AddXml(TXml.CreateAsString('Use', 'Undefined'));
        mquServer:
          AddXml(TXml.CreateAsString('Use', 'LocalServer'));
        mquClient:
          AddXml(TXml.CreateAsString('Use', 'LocalClient'));
      end;
      AddXml(TXml.CreateAsInteger('MaxWorkingThreads', se.mqMaxWorkingThreads));
    end;
    with result.AddXml(TXml.CreateAsString('Colors', '')) do
    begin
      with AddXml(TXml.CreateAsString('Xml', '')) do
      begin
        AddXml(TXml.CreateAsString('UnassignedValues',
            ColorToHtml(bgNilValueColor)));
        AddXml(TXml.CreateAsString('CorrelationValues',
            ColorToHtml(bgCorrelationItemColor)));
        AddXml(TXml.CreateAsString('ExpectedValues',
            ColorToHtml(bgExpectedValueColor)));
      end;
    end;
  end;
  with result.AddXml(TXml.CreateAsString('TaCo', '')) do
  begin
    with AddXml(TXml.CreateAsString('pingpong', '')) do
    begin
      AddXml(TXml.CreateAsBoolean('Enabled', enableTacoPingPong));
      AddXml(TXml.CreateAsInteger('interval', intervalTacoPingPong));
    end;
  end;
  with result.AddXml(TXml.CreateAsString('RemoteControl', '')) do
  begin
    AddXml(TXml.CreateAsBoolean('Enabled', sc.Enabled));
    AddXml(TXml.CreateAsInteger('Port', sc.portNumber));
  end;
end;

procedure TMainForm.OptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := OptionsAsXml;
  try
    if EditXmlXsdBased('Options', '', '', '', se.IsActive, False, esUsed, optionsXsd, xXml) then
    begin
      isOptionsChanged := True;
      OptionsFromXml(xXml);
      CheckBoxClick(nil);
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.ActionComboBoxChange(Sender: TObject);
begin
  if Assigned(WsdlOperation) then
  begin
    if (WsdlOperation.StubAction <> TStubAction(ActionComboBox.ItemIndex)) and
      ((WsdlOperation.StubAction = saRequest) or
        (TStubAction(ActionComboBox.ItemIndex) = saRequest)) then
    begin
      WsdlOperation.StubAction := TStubAction(ActionComboBox.ItemIndex);
      OperationReqsTreeView.OnFocusChanged(OperationReqsTreeView,
        OperationReqsTreeView.FocusedNode, 0);
    end;
    WsdlOperation.StubAction := TStubAction(ActionComboBox.ItemIndex);
    stubChanged := True;
    OperationReqsTreeView.Invalidate;
    OperationDelayResponseTimeAction.Visible :=
      (WsdlOperation.StubAction <> saRequest);
    if (WsdlOperation.DelayTimeMsMin = 0) and
      (WsdlOperation.DelayTimeMsMax = 0) then
      OperationDelayResponseTimeAction.ImageIndex := 60
    else
      OperationDelayResponseTimeAction.ImageIndex := 61;
    RedirectAddressAction.Visible := (WsdlOperation.StubAction = saRedirect) or
      (WsdlOperation.StubAction = saRequest);
    if WsdlOperation.StubAction = saStub then
      EditMessageScriptAction.Caption := 'Edit Message Script'
    else
      EditMessageScriptAction.Caption := 'Edit Message Before Script';
    EditBetweenScriptMenuItem.Visible := (WsdlOperation.StubAction = saStub);
    EditBeforeScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
    EditAfterScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
    {
      if (WsdlOperation.StubAction = saRequest) then
      begin
      MessagesMenuItem.Caption := 'Request';
      AddMessageAction.Caption := 'Add request';
      DeleteMessageAction.Caption := 'Delete request';
      end
      else
      begin
      MessagesMenuItem.Caption := 'Reply';
      AddMessageAction.Caption := 'Add reply';
      DeleteMessageAction.Caption := 'Delete reply';
      end;
    }
  end;
end;

procedure TMainForm.GridViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
begin
  // requires an imagelist attached to treeview
  xMessage := nil; //avoid warning
  if Column = NScripts then
    Exit;
  if not Assigned(wsdlOperation) then
    exit;
  NodeToMessage(Sender, Node, xMessage);
  if not Assigned(xMessage) then
    exit;
  try
    case Kind of
      ikNormal, ikSelected:
        begin
          if Column < NScripts then
          begin
            case Column of
              Ord (operationsColumnBeforeScript):
                begin
                   if (xMessage.BeforeScriptLines.Count > 0) then
                   begin
                     if (not xMessage.PreparedBefore) then
                       ImageIndex := 6
                     else
                       ImageIndex := 5;
                   end
                   else
                     ImageIndex := 4;
                end;
              Ord (operationsColumnAfterScript):
                begin
                  if wsdlOperation.StubAction <> saStub then
                  begin
                    if (xMessage.AfterScriptLines.Count > 0) then
                    begin
                      if (not xMessage.PreparedAfter) then
                        ImageIndex := 6
                      else
                        ImageIndex := 5;
                    end
                    else
                      ImageIndex := 4;
                  end;
                end;
              end;
            exit;
          end;
          if Column < (NScripts + xMessage.CorrelationBindables.Count + 1) then
            Exit;
          xBind := xMessage.ColumnXmls.Bindables
            [Column - NScripts - xMessage.CorrelationBindables.Count - 1];
          if Assigned(xBind) then
          begin
            if xBind is TXmlAttribute then
            begin
              ImageIndex := -1;
              exit;
            end;
            if xBind is TXml then
            begin
              if xmlUtil.isBoolean(xBind) then
              begin
                if (xBind.Value = 'true') or (xBind.Value = '1') then
                  ImageIndex := 22
                else
                  ImageIndex := 21;
                exit;
              end;
              if xmlUtil.isDateTime(xBind) then
              begin
                ImageIndex := 23;
                exit;
              end;
              if xmlUtil.isDate(xBind) then
              begin
                ImageIndex := 24;
                exit;
              end;
              if xmlUtil.isTime(xBind) then
              begin
                ImageIndex := 25;
                exit;
              end;
              if xmlUtil.isEditSupported(xBind) then
              begin
                ImageIndex := 18;
                exit;
              end;
              //if xmlUtil.isGridAdviced(xBind) then
              //begin
              //  ImageIndex := 19;
              //  exit;
              //end;
              if xmlUtil.isTreeAdviced(xBind) then
              begin
                ImageIndex := 20;
                exit;
              end;
              ImageIndex := -1;
              exit;
            end;
            if xBind is TIpmItem then
            begin
              // tbd
            end;
          end
        end;
    end;
  except
  end;
end;

procedure TMainForm.GridViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
begin
{
  first column Message Name
  n columns correlation values
  m columns displayed columns
}
  xMessage := nil; //avoid warning
  CellText := '';
  NodeToMessage(Sender, Node, xMessage);
  if not Assigned(xMessage) then Exit;
  if Column < NScripts then Exit;
  try
    if Column = NScripts then
      CellText := xMessage.Name
    else
    begin
      if (Column - NScripts) <= xMessage.CorrelationBindables.Count then
        try
          if Assigned (xMessage.CorrelationBindables.Bindables[Column - NScripts - 1]) then
            CellText := xMessage.CorrelationBindables.Bindables[Column - NScripts - 1].CorrelationValue
          else
            CellText := '?';
        except
        end
      else
      begin
        xBind := xMessage.ColumnXmls.Bindables
          [Column - NScripts - xMessage.CorrelationBindables.Count - 1];
        if Assigned(xBind) then
        begin
          if (   (xBind is TIpmItem)
              or (not Assigned(xBind.Parent))
              or (xBind.Parent.CheckedAllUp)
             ) then
            CellText := xBind.GetStringData
          else
            CellText := '&nil';
        end
        else
          CellText := '?';
      end;
    end;
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.GridViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  grid_x := X;
  grid_y := Y;
end;

procedure TMainForm.PasteGridOnNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
  procedure _RaiseError(aMessage: String);
  begin
    Sender.FocusedNode := Node;
    Sender.FocusedColumn := Column;
    raise Exception.Create(aMessage);
  end;

var
  xMessage, xOrgMessage: TWsdlMessage;
  xBind: TCustomBindable;
  xData: PMessageTreeRec;
begin
  if Node = Nil then
  begin
    if Column = 0 then
    begin
      xOrgMessage := WsdlOperation.Messages.Messages[0];
      if (WsdlOperation.StubAction = saRequest) then
        xMessage := TWsdlMessage.CreateRequest(WsdlOperation,
          'Request' + IntToStr(WsdlOperation.Messages.Count),
          'Pattern' + IntToStr(WsdlOperation.Messages.Count),
          xOrgMessage.Documentation)
      else
        xMessage := TWsdlMessage.CreateReply(WsdlOperation,
          'Reply' + IntToStr(WsdlOperation.Messages.Count),
          'Pattern' + IntToStr(WsdlOperation.Messages.Count),
          xOrgMessage.Documentation);
      if WsdlOperation.reqBind is TIpmItem then
      begin (xMessage.reqBind as TIpmItem)
        .LoadValues(xOrgMessage.reqBind as TIpmItem);
(xMessage.rpyBind as TIpmItem)
        .LoadValues(xOrgMessage.rpyBind as TIpmItem);
(xMessage.fltBind as TIpmItem)
        .LoadValues(xOrgMessage.fltBind as TIpmItem);
      end
      else
      begin (xMessage.reqBind as TXml)
        .LoadValues(xOrgMessage.reqBind as TXml, False, True);
(xMessage.rpyBind as TXml)
        .LoadValues(xOrgMessage.rpyBind as TXml, False, True);
        se.UpdateMessageRow(WsdlOperation, xMessage);
        if Assigned(WsdlOperation.FaultMessages) then
        begin
          xMessage.fltBind.Name := xOrgMessage.fltBind.Name;
(xMessage.fltBind as TXml)
          .Xsd := WsdlOperation.FaultXsd; (xMessage.fltBind as TXml)
          .LoadValues(xOrgMessage.fltBind as TXml, True);
        end;
      end;
      Node := GridView.AddChild(nil);
      xData := GridView.GetNodeData(Node);
      xData.Message := xMessage;
      stubChanged := True;
    end
    else
    begin
      Node := GridView.GetLast;
      NodeToMessage(Sender, Node, xMessage);
    end;
  end
  else
  begin
    NodeToMessage(Sender, Node, xMessage);
  end;

  if Column = 0 then
  begin
    if NewText <> xMessage.Name then
    begin
      if Node = Sender.GetFirst then
        _RaiseError('Not allowed to change this reply name into ' + NewText)
      else
      begin
        xMessage.Name := NewText;
        stubChanged := True;
      end;
    end;
  end
  else
  begin
    if Column <= xMessage.CorrelationBindables.Count then
    begin
      if NewText <> xMessage.CorrelationBindables.Bindables[Column - 1]
        .CorrelationValue then
      begin
        if Node = Sender.GetFirst then
          _RaiseError('Not allowed to change this pattern into ' + NewText)
        else
        begin
          xMessage.CorrelationBindables.Bindables[Column - 1].CorrelationValue := NewText;
          stubChanged := True;
        end;
      end;
    end
    else
    begin
      if (NewText <> '?') and
        (Assigned(xMessage.ColumnXmls.Bindables
            [Column - xMessage.CorrelationBindables.Count - 1])) then
      begin
        xBind := xMessage.ColumnXmls.Bindables
          [Column - xMessage.CorrelationBindables.Count - 1];
        if NewText <> '&nil' then
        begin
          if (NewText <> xBind.Value) or (not xBind.CheckedAllUp) then
          begin
            xBind.Value := NewText;
            xBind.Checked := True;
            stubChanged := True;
          end;
        end
        else
        begin
          if xBind.Checked then
          begin
            xBind.Checked := False;
            stubChanged := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.PasteSwiftdatafromclipboardMenuItemClick(Sender: TObject);
var
  dXml, fXml, sXml, bXml: TXml;
  X: Integer;
begin
  try
    dXml := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) as TXml;
    fXml := dXml;
    while fXml.Name <> 'FinMessage' do
      fXml := fXml.Parent as TXml;
  except
    on E: Exception do
      raise Exception.Create
        ('PasteSwiftdatafromclipboardMenuItemClick: ' + E.Message);
  end;
  try
    try
      with TSwiftMt.Create(ClipBoard.AsText, fXml.Xsd) do
        try
          sXml := AsXml;
          try
            WsdlOperation.AcquireLock;
            try
              if sXml.Name = dXml.Name then
              begin
                for X := 0 to sXml.Items.Count - 1 do
                begin
                  if sXml.Items.XmlItems[X].Checked then
                    with dXml.Items.XmlItemByTag[sXml.Items.XmlItems[X].Name] do
                    begin
                      Reset;
                      Checked := True;
                      LoadValues(sXml.Items.XmlItems[X], False, True);
                    end;
                end;
              end
              else
              begin
                bXml := sXml.Items.XmlItemByTag[dXml.Name];
                dXml.Reset;
                dXml.Checked := True;
                dXml.LoadValues(bXml, False, True);
              end;
            finally
              WsdlOperation.ReleaseLock;
              stubChanged := True;
            end;
          finally
            sXml.Free;
          end;
        finally
          Free;
        end;
    except
      on E: Exception do
        ShowInfoForm('Error parsing ClipBoard as SwiftMt message', E.Message);
    end;
  finally
    RevalidateXmlTreeView(InWsdlTreeView);
  end;
end;

procedure TMainForm.GridViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
  procedure _RaiseError(aMessage: String);
  begin
    Sender.FocusedNode := Node;
    Sender.FocusedColumn := Column;
    raise Exception.Create(aMessage);
  end;

var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  WsdlOperation.AcquireLock;
  try
    NodeToMessage(Sender, Node, xMessage);
    if Column = NScripts then
    begin
      if NewText <> xMessage.Name then
      begin
        if Node = Sender.GetFirst then
          _RaiseError('Not allowed to change this reply name into ' + NewText)
        else
        begin
          xMessage.Name := NewText;
          stubChanged := True;
        end;
      end;
    end
    else
    begin
      if (Column - NScripts) <= xMessage.CorrelationBindables.Count then
      begin
        xNode := GridView.GetFirstSelected;
        while Assigned(xNode) do
        begin
          NodeToMessage(Sender, xNode, xMessage);
          if NewText <> xMessage.CorrelationBindables.Bindables[Column - NScripts - 1]
            .CorrelationValue then
          begin
            if xNode = Sender.GetFirst then
            begin
              Node := xNode;
              _RaiseError('Not allowed to change this pattern into ' + NewText);
            end;
            xMessage.CorrelationBindables.Bindables[Column - NScripts - 1].CorrelationValue := NewText;
            stubChanged := True;
          end;
          xNode := GridView.GetNextSelected(xNode);
        end;
      end
      else
      begin
        { }{
          if (Assigned (xMessage.ColumnXmls.Bindables[Column - xMessage.CorrelationBindables.Count - 1])) then
          InWsdlTreeView.OnNewText ( InWsdlTreeView
          , editingNode
          , treeValueColumn
          , NewText
          );
          { }
        xNode := GridView.GetFirstSelected;
        while Assigned(xNode) do
        begin
          NodeToMessage(Sender, xNode, xMessage);
          xBind := xMessage.ColumnXmls.Bindables
            [Column - NScripts - xMessage.CorrelationBindables.Count - 1];
          if (xBind is TXml) or (xBind is TXmlAttribute) then
          begin
            if NewText = '&nil' then
            begin
              if xBind.Checked then
              begin
                xBind.Checked := False;
                stubChanged := True;
              end;
            end
            else
            begin
              if (NewText <> xBind.Value) or (not AllChecked(Sender, Node)) then
              begin
                xBind.Value := NewText;
                xBind.Checked := True;
                stubChanged := True;
              end;
            end;
          end;
          if xBind is TIpmItem then
          begin
            if NewText <> xBind.Value then
            begin
              xBind.Value := NewText;
              xBind.Checked := True;
              stubChanged := True;
            end;
          end;
          xNode := GridView.GetNextSelected(xNode);
        end;
        RevalidateXmlTreeView(InWsdlTreeView);
      end;
    end;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.GridViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  xMessage: TWsdlMessage;
  swapEvent: TVTFocusChangeEvent;
  xDataIndex: Integer;
begin
  xMessage := nil; //avoid warning
  NodeToMessage(Sender, Node, xMessage);
  editingNode := InWsdlTreeView.FocusedNode;
  if Column < NScripts then
  begin
    Allowed := False;
    Exit;
  end;
  if Column = NScripts then
  begin
    Allowed := (Node <> Sender.GetFirst) and (GridView.SelectedCount = 1);
    exit;
  end;
  if (Column - NScripts) <= xMessage.CorrelationBindables.Count then
  begin
    Allowed := (Node <> Sender.GetFirst)
           and (Assigned (xMessage.CorrelationBindables.Bindables[Column- NScripts - 1]))
             ;
    exit;
  end;
  xDataIndex := Column - NScripts - xMessage.CorrelationBindables.Count - 1;
  Allowed := Assigned(xMessage.ColumnXmls.Bindables [xDataIndex])
         and (xMessage.ColumnXmls.Bindables [xDataIndex].Children.Count = 0)
           ;
  if Allowed then
  begin
    swapEvent := InWsdlTreeView.OnFocusChanged;
    try
      InWsdlTreeView.OnFocusChanged := nil;
      FocusOnBind(xMessage.ColumnXmls.Bindables [xDataIndex]);
    finally
      InWsdlTreeView.OnFocusChanged := swapEvent;
    end;
  end;
end;

procedure TMainForm.SelectCorrelationElementActionUpdate(Sender: TObject);
begin
  SelectCorrelationElementAction.Enabled := Assigned(WsdlOperation);
  // and (WsdlOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
  // and (WsdlOperation.StubAction <> saRequest)
                                          ;
end;

procedure TMainForm.SelectCorrelationElementActionExecute(Sender: TObject);
var
  swapBindable: TCustomBindable;
begin
  if not InactiveAfterPrompt then Exit;
  with WsdlOperation do
  begin
    if WsdlService.DescriptionType in [ipmDTFreeFormat] then
      FreeFormatReq := Messages.Messages[0].FreeFormatReq; // always and only from first
  end;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  SelectElementsForm.Caption := 'Maintain list of correlation elements';
  try
    GridView.BeginUpdate;
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.GroupAllowed := False;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    SelectElementsForm.ControlBinds := WsdlOperation.CorrelationBindables;
    SelectElementsForm.ShowModal;
    begin
      se.UpdateReplyColumns(WsdlOperation);
      UpdateMessagesGrid;
      UpdateLogCorrelationIds (WsdlOperation);
      stubChanged := stubChanged or SelectElementsForm.stubChanged;
    end;
  finally
    GridView.EndUpdate;
    FreeAndNil(SelectElementsForm);
    DoColorBindButtons;
  end;
end;

procedure TMainForm.AddMessageActionUpdate(Sender: TObject);
begin
  AddMessageAction.Enabled := Assigned(WsdlOperation) and
    ((WsdlOperation.CorrelationBindables.Count > 0) or
      (WsdlOperation.StubAction = saRequest)) and Assigned
    (GridView.FocusedNode);
end;

procedure TMainForm.AddMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  cNode, nNode, sNode: PVirtualNode;
  n: Integer;
begin
  xMessage := nil; //avoid warning
  WsdlOperation.AcquireLock;
  try
    nNode := nil;
    sNode := nil;
    cNode := GridView.GetFirst;
    n := WsdlOperation.Messages.Count;
    while Assigned(cNode) and (n > 0) do
    begin
      if GridView.Selected[cNode] then
      begin
        GridView.Selected[cNode] := False;
        NodeToMessage(GridView, cNode, xMessage);
        if Assigned(xMessage) then
        begin
          nNode := AddMessage(cNode);
          if not Assigned(sNode) then
            sNode := nNode;
          GridView.Selected[nNode] := True;
          stubChanged := True;
        end;
      end;
      Dec(n);
      cNode := GridView.GetNext(cNode);
    end;
    GridView.Invalidate;
    GridView.SetFocus;
    DoColorBindButtons;
    if Assigned(sNode) then
      GridView.FocusedNode := sNode;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

function TMainForm.AddMessage(aCopyNode: PVirtualNode): PVirtualNode;
var
  xOrgMessage, xNewMessage: TWsdlMessage;
  xData: PMessageTreeRec;
begin
  xOrgMessage := nil; //avoid warning
  result := nil;
  NodeToMessage(GridView, aCopyNode, xOrgMessage);
  if (WsdlOperation.StubAction = saRequest) then
    xNewMessage := TWsdlMessage.CreateRequest(WsdlOperation,
      'Request' + IntToStr(WsdlOperation.Messages.Count),
      'Pattern' + IntToStr(WsdlOperation.Messages.Count),
      xOrgMessage.Documentation)
  else
    xNewMessage := TWsdlMessage.CreateReply(WsdlOperation,
      'Reply' + IntToStr(WsdlOperation.Messages.Count),
      'Pattern' + IntToStr(WsdlOperation.Messages.Count),
      xOrgMessage.Documentation);
  if WsdlOperation.DescriptionType = ipmDTFreeFormat then
  begin
    xNewMessage.FreeFormatReq := xOrgMessage.FreeFormatReq;
    xNewMessage.FreeFormatRpy := xOrgMessage.FreeFormatRpy;
  end;
  if WsdlOperation.reqBind is TIpmItem then
  begin
    (xNewMessage.reqBind as TIpmItem).LoadValues(xOrgMessage.reqBind as TIpmItem);
    (xNewMessage.rpyBind as TIpmItem).LoadValues(xOrgMessage.rpyBind as TIpmItem);
    (xNewMessage.fltBind as TIpmItem).LoadValues(xOrgMessage.fltBind as TIpmItem);
    se.UpdateMessageRow(WsdlOperation, xNewMessage);
  end
  else
  begin
    (xNewMessage.reqBind as TXml).LoadValues(xOrgMessage.reqBind as TXml, False, True);
    (xNewMessage.rpyBind as TXml).LoadValues(xOrgMessage.rpyBind as TXml, False, True);
    se.UpdateMessageRow(WsdlOperation, xNewMessage);
    if Assigned(WsdlOperation.FaultMessages) then
    begin
      xNewMessage.fltBind.Name := xOrgMessage.fltBind.Name;
      (xNewMessage.fltBind as TXml).Xsd := WsdlOperation.FaultXsd;
      (xNewMessage.fltBind as TXml).LoadValues(xOrgMessage.fltBind as TXml, True);
    end;
  end;
  result := GridView.AddChild(nil);
  xData := GridView.GetNodeData(result);
  xData.Message := xNewMessage;
end;

procedure TMainForm.GridViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xMessage: TWsdlMessage;
  swapEvent: TVTFocusChangeEvent;
  swapNotifyEvent, swapMemoEvent: TNotifyEvent;
begin
  xMessage := nil; //avoid warning
  InWsdlTreeView.BeginUpdate;
  try
    swapNotifyEvent := DocumentationMemo.OnChange;
    swapMemoEvent := FreeFormatMemo.OnChange;
    DocumentationMemo.OnChange := nil;
    FreeFormatMemo.OnChange := nil;
    try
      Sender.Selected[Sender.FocusedNode] := True;
      NodeToMessage(GridView, GridView.FocusedNode, xMessage);
      InWsdlTreeView.Clear;
      DocumentationMemo.Clear;
      if Assigned(xMessage) then
      begin
        WsdlOperation.LastMessage := xMessage;
        InWsdlTreeView.EndEditNode;
        InWsdlTreeView.Clear;
        InWsdlTreeView.RootNodeCount := 0;
        InWsdlTreeView.NodeDataSize := SizeOf(TXmlTreeRec);
        if WsdlOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
        begin
          if WsdlOperation.StubAction = saRequest then
            FreeFormatMemo.Text := WsdlMessage.FreeFormatReq
          else
            FreeFormatMemo.Text := WsdlMessage.FreeFormatRpy;
        end
        else
        begin
          if WsdlOperation.StubAction = saRequest then
          begin
            FillBindTreeView(InWsdlTreeView, xMessage.reqBind, nil);
            FillBindTreeView(InWsdlTreeView, xMessage.rpyBind, nil);
          end
          else
          begin
            FillBindTreeView(InWsdlTreeView, xMessage.rpyBind, nil);
            FillBindTreeView(InWsdlTreeView, xMessage.reqBind, nil);
          end;
        end;
        DocumentationMemo.Text := xMessage.Documentation;
        if (Column - NScripts) > xMessage.CorrelationBindables.Count then
        begin
          swapEvent := GridView.OnFocusChanged;
          try
            GridView.OnFocusChanged := nil;
            FocusOnBind
              (xMessage.ColumnXmls.Bindables
                [Column - NScripts - xMessage.CorrelationBindables.Count - 1]);
          finally
            GridView.OnFocusChanged := swapEvent;
          end;
        end
        else
          with InWsdlTreeView do
          begin
            Selected[GetFirst] := True;
            FocusedNode := GetFirst;
          end;
        try
          with InWsdlTreeView do
          begin
            // IsVisible [GetNextSibling (GetFirst)] := doCheckExpectedValues;
          end;
        except
        end;
      end;
    finally
      DocumentationMemo.OnChange := swapNotifyEvent;
      FreeFormatMemo.OnChange := swapMemoEvent;
    end;
  finally
    InWsdlTreeView.EndUpdate;
    if Assigned(InWsdlTreeView.FocusedNode) then
      InWsdlTreeView.ScrollIntoView(InWsdlTreeView.FocusedNode, False, False);
  end;
end;

procedure TMainForm.DeleteMessageActionUpdate(Sender: TObject);
begin
  with GridView do
    DeleteMessageAction.Enabled := Assigned(FocusedNode)
                               and not (FocusedNode = GetFirst)
                                 ;
end;

procedure TMainForm.DeleteMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  cNode, nNode: PVirtualNode;
begin
  xMessage := nil; //avoid warning
  WsdlOperation.AcquireLock;
  try
    cNode := GridView.GetFirst;
    if Assigned(cNode) then
      cNode := GridView.GetNext(cNode); // never delete default
    while Assigned(cNode) do
    begin
      nNode := GridView.GetNext(cNode);  // because it still can
      if GridView.Selected[cNode] then
      begin
        NodeToMessage(GridView, cNode, xMessage);
        GridView.DeleteNode(cNode, True);
        if Assigned(xMessage) then
        begin
          WsdlOperation.Messages.DeleteMessage(xMessage);
          stubChanged := True;
        end;
      end;
      cNode := nNode;
    end;
    GridView.Invalidate;
    GridView.SetFocus;
    GridViewFocusedNode(GridView.FocusedNode);
    DoColorBindButtons;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.MoveUpMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  fNode: PVirtualNode;
  pNode: PVirtualNode;
  fData: PMessageTreeRec;
  pData: PMessageTreeRec;
begin
  EndEdit;
  WsdlOperation.AcquireLock;
  try
    with GridView do
    begin
      fNode := GetFirstSelected;
      while Assigned(fNode) do
      begin
        pNode := GetPrevious(fNode);
        if not Assigned(pNode) then
          exit;
        fData := GetNodeData(fNode);
        pData := GetNodeData(pNode);
        ExchangeMessages(fData.Message, pData.Message);
        xMessage := pData.Message;
        pData.Message := fData.Message;
        fData.Message := xMessage;
        InvalidateNode(pNode);
        InvalidateNode(fNode);
        Selected[pNode] := True;
        Selected[fNode] := False;
        fNode := GetNextSelected(fNode)
      end;
      GridView.FocusedNode := GetFirstSelected;
    end;
    stubChanged := True;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.MoveUpMessageActionUpdate(Sender: TObject);
begin
  with GridView do
    MoveUpMessageAction.Enabled := (SelectedCount > 0)
                               and not Selected[GetFirst]
                               and not Selected[GetNext(GetFirst)]
                                 ;
end;

procedure TMainForm.MoveDownMessageActionUpdate(Sender: TObject);
begin
  with GridView do
    MoveDownMessageAction.Enabled := (SelectedCount > 0)
                                 and not Selected[GetFirst]
                                 and not Selected[GetLast]
                                   ;
end;

procedure TMainForm.MoveDownMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  fNode: PVirtualNode;
  nNode: PVirtualNode;
  fData: PMessageTreeRec;
  nData: PMessageTreeRec;
begin
  EndEdit;
  WsdlOperation.AcquireLock;
  try
    with GridView do
    begin
      fNode := GetLast;
      while Assigned(fNode) and (fNode <> GetFirst) do
      begin
        if Selected[fNode] then
        begin
          nNode := GetNext(fNode);
          if not Assigned(nNode) then
            exit;
          fData := GridView.GetNodeData(fNode);
          nData := GridView.GetNodeData(nNode);
          ExchangeMessages(fData.Message, nData.Message);
          xMessage := nData.Message;
          nData.Message := fData.Message;
          fData.Message := xMessage;
          InvalidateNode(nNode);
          InvalidateNode(fNode);
          Selected[nNode] := True;
          Selected[fNode] := False;
        end;
        fNode := GetPrevious(fNode);
      end;
      GridView.FocusedNode := GetFirstSelected;
    end;
    stubChanged := True;
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.DoColorBindButtons;
begin
  if ((WsdlOperation.CorrelationBindables.Count > 0) or
      (WsdlOperation.StubAction = saRequest)) then
    SelectCorrelationElementAction.ImageIndex := 8
  else
    SelectCorrelationElementAction.ImageIndex := 7;
  if WsdlOperation.ExpectationBindables.Count > 0 then
    SelectExpectedElementsAction.ImageIndex := 62
  else
    SelectExpectedElementsAction.ImageIndex := 53;
end;

procedure TMainForm.ApplyToActionUpdate(Sender: TObject);
begin
  ApplyToAction.Enabled := Assigned(WsdlOperation);
end;

procedure TMainForm.OperationApplySettingsActionExecute(Sender: TObject);
  procedure _Apply(d, s: TWsdlOperation);
  begin
    if (d = s) then
      exit; // you would loose the mqheader or wsa data entered
    d.StubAction := s.StubAction;
    d.OnRequestViolatingSchema := s.OnRequestViolatingSchema;
    d.OnRequestViolatingAddressPath := s.OnRequestViolatingAddressPath;
    d.DelayTimeMsMin := s.DelayTimeMsMin;
    d.DelayTimeMsMax := s.DelayTimeMsMax;
    d.StubTransport := s.StubTransport;
    d.StubHttpAddress := s.StubHttpAddress;
    if not d.isOpenApiService then
    begin
      d.httpVerb := s.httpVerb;
    end;
    d.ContentEncoding := s.ContentEncoding;
    d.AcceptGzipEncoding := s.AcceptGzipEncoding;
    d.AcceptDeflateEncoding := s.AcceptDeflateEncoding;
    d.StubMqPutManager := s.StubMqPutManager;
    d.StubMqPutQueue := s.StubMqPutQueue;
    d.StubMqGetManager := s.StubMqGetManager;
    d.StubMqGetQueue := s.StubMqGetQueue;
    d.StubMqTimeOut := s.StubMqTimeOut;
    d.StubMqHeaderXml.CheckDownLine(False);
    d.StubMqHeaderXml.LoadValues(s.StubMqHeaderXml, False, True);
    d.StubStompPutHost := s.StubStompPutHost;
    d.StubStompPutPort := s.StubStompPutPort;
    d.StubStompPutUseCredentials := s.StubStompPutUseCredentials;
    d.StubStompPutUserName := s.StubStompPutUserName;
    d.StubStompPutPassword := s.StubStompPutPassword;
    d.StubStompPutClientId := s.StubStompPutClientId;
    d.StubStompTimeOut := s.StubStompTimeOut;
    d.StubStompHeaderXml.CheckDownLine(False);
    d.StubStompHeaderXml.LoadValues(s.StubStompHeaderXml, False, True);
    d.wsaEnabled := s.wsaEnabled;
    d.wsaSpecificMustUnderstand := s.wsaSpecificMustUnderstand;
    d.wsaMustUnderstand := s.wsaMustUnderstand;
    d.reqWsaXml.CheckDownLine(False);
    d.reqWsaXml.LoadValues(s.reqWsaXml, False, True);
    d.reqWsaXml.CheckDownLine(False);
    d.reqWsaXml.LoadValues(s.reqWsaXml, False, True);
  end;

var
  w, s, o: Integer;
  xWsdl: TWsdl;
begin
  Application.CreateForm(TApplyToForm, ApplyToForm);
  try
    ApplyToForm.ShowModal;
    if ApplyToForm.ModalResult = mrOk then
    begin
      AcquireLock;
      try
        case ApplyToForm.RadioGroup.ItemIndex of
          0:
            begin
              s := WsdlServicesComboBox.ItemIndex;
              for o := 0 to Wsdl.Services.Services[s].Operations.Count - 1 do
                _Apply(Wsdl.Services.Services[s].Operations.Operations[o],
                  WsdlOperation);
            end;
          1:
            begin
              for s := 0 to Wsdl.Services.Count - 1 do
              begin
                for o := 0 to Wsdl.Services.Services[s].Operations.Count - 1 do
                  _Apply(Wsdl.Services.Services[s].Operations.Operations[o],
                    WsdlOperation);
              end;
            end;
          2:
            begin
              for w := 0 to se.Wsdls.Count - 1 do
              begin
                xWsdl := TWsdl(se.Wsdls.Objects[w]);
                for s := 0 to xWsdl.Services.Count - 1 do
                begin
                  for o := 0 to xWsdl.Services.Services[s].Operations.Count - 1
                    do
                    _Apply(xWsdl.Services.Services[s].Operations.Operations[o],
                      WsdlOperation);
                end;
              end;
            end;
        end;
        OperationReqsTreeView.Invalidate;
        stubChanged := True;
      finally
        ReleaseLock;
      end;
    end;
  finally
    FreeAndNil(ApplyToForm);
  end;
end;

procedure TMainForm.GridViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
  function _decColor (aColor: TColor): TColor;
  begin
    result := aColor;
    if GridView.Selected[Node] then Result := DecColor(Result, 6);
    if GridView.FocusedNode = Node then Result := DecColor(Result, 9);
  end;
var
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
  expXml: TXml;
begin
  xMessage := nil; //avoid warning
  try
    NodeToMessage(Sender, Node, xMessage);
    if not Assigned(xMessage) then
      exit;
    if Column < NScripts then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := _decColor(self.Color);
        FillRect(CellRect);
      end;
      exit;
    end;
    if Column <= NScripts + xMessage.CorrelationBindables.Count then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := _decColor(bgCorrelationItemColor);
        FillRect(CellRect);
      end;
      exit;
    end;
    try
      xBind := xMessage.ColumnXmls.Bindables
        [Column - NScripts - xMessage.CorrelationBindables.Count - 1];
    except
      exit;
    end;
    if ((xBind is TXml) or (xBind is TXmlAttribute))
    and Assigned (WsdlOperation) then
    begin
      if WsdlOperation.StubAction = saRequest then
        expXml := xMessage.rpyBind as TXml
      else
        expXml := xMessage.reqBind as TXml;
      if expXml.IsAncestorOf(xBind) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := _decColor(bgExpectedValueColor);
          FillRect(CellRect);
        end;
        exit;
      end;
      if (not Assigned(xBind)) or not(xBind.CheckedAllUp) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := _decColor(bgNilValueColor);
          FillRect(CellRect);
        end;
        exit;
      end;
    end;
    with TargetCanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := _decColor(GridView.Color);
      FillRect(CellRect);
    end;
  except
    exit;
  end;
end;

procedure TMainForm.GridViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if (Column - NScripts) > WsdlOperation.CorrelationBindables.Count then
  begin
    InWsdlTreeView.OnEdited(InWsdlTreeView, editingNode, treeValueColumn);
  end;
  editingNode := nil;
end;

function TMainForm.OkToOpenStubCase: Boolean;
var
  ret: Word;
begin
  result := True;
  if stubChanged then
  begin
    ret := MessageDlg('Save changes to stub?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if (ret = mrYes) then
    begin
      OnlyWhenLicensed;
      if se.stubRead then
      begin
        SaveWsdlStubCase(se.projectFileName);
      end
      else
      begin
        SaveFileDialog.DefaultExt := 'wsdlStub';
        SaveFileDialog.FileName := se.projectFileName;
        SaveFileDialog.Filter := 'wsdlStub Case (*.wsdlStub)|*.wsdlStub';
        SaveFileDialog.Title := 'Save wsdlStub case';
        if SaveFileDialog.Execute then
        begin
          se.projectFileName := SaveFileDialog.FileName;
          SaveWsdlStubCase(SaveFileDialog.FileName);
        end
        else
          result := False;
      end;
    end;
    if (ret = mrCancel) then
      result := False;
  end;
end;

procedure TMainForm.SaveStubCaseActionExecute(Sender: TObject);
begin
  EndEdit;
  if not se.stubRead then
    SaveStubCaseAsActionExecute(Sender)
  else
    SaveWsdlStubCase(se.projectFileName);
end;

procedure TMainForm.InWsdlTreeViewExit(Sender: TObject);
begin
  InWsdlTreeView.EndEditNode;
end;

procedure TMainForm.GridViewExit(Sender: TObject);
begin
  GridView.EndEditNode;
end;

procedure TMainForm.TreeViewResize(Sender: TObject);
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

function TMainForm.CheckHttpAddress (aBind: TObject; aNewValue: String): Boolean;
begin
  result := True;
  if WsdlOperation.isOpenApiService then
  begin
    with TIdUri.Create(aNewValue) do
    try
      if (Path + Document <> '/') then
      begin
        ShowMessage (Format ('no path (%s) allowed on OpenApi service', [Path + Document]));
        result := False;
      end;
    finally
      free;
    end;
  end;
end;

procedure TMainForm.RedirectAddressActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned(WsdlOperation) then
    raise Exception.Create('No operation selected');
  with WsdlOperation do
  begin
    xXml := endpointConfigAsXml;
    try
      endpointConfigXsd.FindXsd('endpointConfig.Http.Verb').isReadOnly := (WsdlOperation.isOpenApiService);
      endpointConfigXsd.FindXsd('endpointConfig.Https.Verb').isReadOnly := (WsdlOperation.isOpenApiService);
      endpointConfigXsd.FindXsd('endpointConfig.Http.Address').CheckNewValue := CheckHttpAddress;
      endpointConfigXsd.FindXsd('endpointConfig.Https.Address').CheckNewValue := CheckHttpAddress;
      if EditXmlXsdBased('Configure Endpoint', '', '', '', False, False, esUsed,
        endpointConfigXsd, xXml) then
      begin
        AcquireLock;
        try
          stubChanged := True;
          endpointConfigFromXml(xXml);
        finally
          ReleaseLock;
        end;
      end;
    finally
      xXml.Free;
    end;
  end;
end;

procedure TMainForm.WsdlComboBoxDropDown(Sender: TObject);
begin
  // bug van delphi??
  WsdlServicesComboBox.ItemIndex := 0;
  WsdlOperationsComboBox.ItemIndex := 0;
end;

procedure TMainForm.WsdlServicesComboBoxDropDown(Sender: TObject);
begin
  WsdlOperationsComboBox.ItemIndex := 0;
end;

function TMainForm.getStubChanged: Boolean;
begin
  result := Assigned(se) and se.stubChanged;
end;

procedure TMainForm.setStubChanged(const Value: Boolean);
begin
  if Value then
  begin
    if Assigned(se) then
      se.stubChanged := Value;
  end
  else
    se.stubChanged := Value;
  UpdateCaption;
end;

procedure TMainForm.UnhideAllOperationsActionExecute(Sender: TObject);
var
  X: Integer;
begin
  for X := 0 to allOperations.Count - 1 do
  begin
    if allOperations.Operations[X].HiddenFromUI then
    begin
      allOperations.Operations[X].HiddenFromUI := False;
      stubChanged := True;
    end;
  end;
  UpdateVisibiltyOfOperations;
end;

procedure TMainForm.UnhideOperationMenuItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    TWsdlOperation(tag).HiddenFromUI := False;
  UpdateVisibiltyOfOperations;
  stubChanged := True;
end;

procedure TMainForm.UpdateCaption;
var
  c, s: String;
begin
  c := _progName;
  if stubChanged then
    s := ' *'
  else
    s := '';
  if stubChanged or (captionFileName <> '') then
    c := c + ' - [' + captionFileName + s + ']';
  Caption := c;
end;

procedure TMainForm.WsdlInfoPanelResize(Sender: TObject);
  function _adjust (aLeft, aWidth, aSpace: Integer; aLabel, aOther: TControl): Integer;
  begin
    result := aLeft + aWidth;
    aLabel.Left := aLeft;
    aOther.Left := aLeft + aLabel.Width;
    aOther.Width := result - aOther.Left - aSpace;
  end;
var
  lft, wdth, spc: Integer;
begin
  spc := 3;
  lft := spc;
  wdth := (Sender as TPanel).Width;
  lft := _adjust(lft, wdth div 2, spc, WsdlLabel, WsdlComboBox);
  lft := _adjust(lft, wdth div 4, spc, ServiceLabel, WsdlServicesComboBox);
  lft := _adjust(lft, wdth - lft, spc, OperationLabel, WsdlOperationsComboBox);
end;

procedure TMainForm.ClearLogItemsActionUpdate(Sender: TObject);
begin
  ClearLogItemsAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.DisplayedcolumnMenuItemClick(Sender: TObject);
var
  X: Integer;
  xLog: TLog;
begin
  AcquireLock;
  try
    xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  finally
    ReleaseLock;
  end;
  if not Assigned(xLog) then
    exit;
  X := MessagesVTS.FocusedColumn - Ord(logStdColumnCount);
  while xLog.Operation.LogColumns.Count < se.DisplayedLogColumns.Count do
    xLog.Operation.LogColumns.Add('');
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  with SelectXmlElementForm do
    try
      Caption := 'Select element to display';
      LastCaption := xLog.Operation.LogColumns.Strings[X];
      doShowReq := Assigned(xLog.Operation.reqBind);
      doShowRpy := Assigned(xLog.Operation.rpyBind);
      WsdlOperation := xLog.Operation;
      IncludeRecurring := False;
      ShowModal;
      if ModalResult = mrOk then
      begin
        xLog.Operation.LogColumns.Strings[X] := SelectedCaption;
        xLog.Operation.LogColumns.Bindables[X] := SelectedBind;
        stubChanged := True;
        AcquireLock;
        try
          se.displayedLogs.InvalidateDisplayedColumns(xLog.Operation);
        finally
          ReleaseLock;
        end;
        MessagesVTS.Invalidate;
      end;
    finally
      FreeAndNil(SelectXmlElementForm);
    end;
end;

procedure TMainForm.DocumentationMemoChange(Sender: TObject);
var
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  NodeToMessage(GridView, GridView.FocusedNode, xMessage);
  if Assigned(xMessage) then
  begin
    xMessage.Documentation := DocumentationMemo.Text;
    stubChanged := True;
  end;
end;

procedure TMainForm.VTSEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := False;
end;

function TMainForm.NodeToMsgLog(aDoClaimLog: Boolean; aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode): TLog;
var
  Data: PLogTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    if aDoClaimLog then
      se.AcquireLogLock;
    try
      Data := aTreeView.GetNodeData(aNode);
      if Assigned(Data) then
      begin
        result := Data.Log;
        if aDoClaimLog then
          result.Claim;
      end;
    finally
      if aDoClaimLog then
        se.ReleaseLogLock;
    end;
  end;
end;

procedure TMainForm.UpdateLogTabs (aLog: TLog);
var
  s: String;
begin
  if not Assigned (aLog) then
  begin
    LogMemo.Text := '';
    Exit;
  end;
  case MessagesTabControl.TabIndex of
    Ord (slRequestHeaders): LogMemo.Text := aLog.RequestHeaders;
    Ord (slRequestBody): LogMemo.Text := aLog.RequestBody;
    Ord (slReplyHeaders): LogMemo.Text := aLog.ReplyHeaders;
    Ord (slReplyBody): LogMemo.Text := aLog.ReplyBody;
    Ord (slException): LogMemo.Text := aLog.Exception;
    Ord (slValidation):
    begin
      s := '';
      if aLog.RequestValidateResult <> '' then
        s := s + 'Request:' + LineEnding + aLog.RequestValidateResult + LineEnding;
      if aLog.ReplyValidateResult <> '' then
        s := s  + 'Reply:' + LineEnding + aLog.ReplyValidateResult + LineEnding;
      LogMemo.Text := s;
    end;
  end;
  MessagesTabControl.Invalidate;
end;

procedure TMainForm.MessagesVTSFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xLog: TLog;
  xNode: PVirtualNode;
  xData: PMessageTreeRec;
begin
  try
    Sender.Selected[Sender.FocusedNode] := True;
    xLog := NodeToMsgLog(False,Sender, Node);
    if Assigned(xLog) and (xLog is TLog) then
    begin
      case xLog.TransportType of
        ttHttp:
          begin
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'HTTP Request Headers';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'HTTP Request Body';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'HTTP Reply Headers';
            MessagesTabControl.Tabs[Ord(slReplyBody)] := 'HTTP Reply Body';
          end;
        ttMQ:
          begin
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'MQ Request Descriptor';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'MQ Request Body';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'MQ Reply Descriptor';
            MessagesTabControl.Tabs[Ord(slReplyBody)] := 'MQ Reply Body';
          end;
        ttStomp:
          begin
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'Stomp Request Headers';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'Stomp Request Body';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'Stomp Reply Headers';
            MessagesTabControl.Tabs[Ord(slReplyBody)] := 'Stomp Reply Body';
          end;
        ttSmtp:
          begin
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'SMTP Request Headers';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'Request as XML';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'SMTP Reply Headers';
            MessagesTabControl.Tabs[Ord(slReplyBody)] := 'SMTP Reply Body';
          end;
      else
        begin
          MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'Request Headers';
          MessagesTabControl.Tabs[Ord(slRequestBody)] := 'Request Body';
          MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'Reply Headers';
          MessagesTabControl.Tabs[Ord(slReplyBody)] := 'Reply Body';
        end;
      end;
      if (not xLog.RequestValidated)
      and (not xLog.ReplyValidated) then
        logValidationTabImageIndex := 40
      else
      begin
        if (xLog.RequestValidateResult = '') and
          (xLog.ReplyValidateResult = '') then
          logValidationTabImageIndex := 39
        else
          logValidationTabImageIndex := 25;
      end;
      UpdateLogTabs (xLog);
      MessagesStatusBar.Panels.Items[Ord(lpiFocus)].Text := '[' + IntToStr(xLog.Nr)
        + ' : ' + IntToStr(se.displayedLogs.Number) + ']';
      if Assigned(xLog.Operation) and (xLog.Operation is TWsdlOperation) then
      begin
        GridView.BeginUpdate;
        try
          WsdlOperation := xLog.Operation;
          if Assigned(xLog.Mssg) and (xLog.Mssg is TWsdlMessage) then
          begin
            xNode := GridView.GetFirst;
            while Assigned(xNode) do
            begin
              xData := GridView.GetNodeData(xNode);
              if xData.Message = xLog.Mssg then
              begin
                GridView.Selected[xNode] := True;
                GridViewFocusedNode(xNode);
              end;
              xNode := GridView.GetNext(xNode);
            end;
          end;
        finally
          GridView.EndUpdate;
          GridView.ScrollIntoView(GridView.FocusedNode, False, False);
          OperationReqsTreeView.ScrollIntoView(OperationReqsTreeView.FocusedNode,
            False, False);
        end;
      end;
    end
    else
    begin
      LogMemo.Text := '';
      MessagesStatusBar.Panels.Items[Ord(lpiFocus)].Text := '';
    end;
  except
  end;
end;

procedure TMainForm.MessagesVTSGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xLog: TLog;
begin
  xLog := NodeToMsgLog(False,Sender, Node);
  CellText := '';
  try
    if Assigned(xLog) and (xLog is TLog) then
    begin
      case TLogColumnEnum(Column) of
        logTimeColumn:
          if xLog.StubAction = saRequest then
            CellText := xsdFormatDateTime(xLog.OutboundTimeStamp, @TIMEZONE_UTC)
          else
            CellText := xsdFormatDateTime(xLog.InboundTimeStamp, @TIMEZONE_UTC);
        logDurationColumn:
          try
            CellText := xLog.DurationAsString;
          except
          end;
        logActionColumn:
          case xLog.StubAction of
            saStub: CellText := 'Stub';
            saForward: CellText := 'Forward';
            saRedirect: CellText := 'Redirect';
            saRequest: CellText := 'Request';
          end;
        logVerbColumn: CellText := xLog.httpCommand;
        logStatusColumn: CellText := IntToStr(xLog.httpResponseCode);
        logServiceColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.WsdlService.Name
          else
            CellText := xLog.ServiceName;
        logOperationColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.Name
          else
            CellText := xLog.OperationName;
        logCorrelationIdColumn:
          CellText := xLog.CorrelationId;
      end;
      if (Column >= Ord(logStdColumnCount)) and Assigned(xLog.Operation) then
      begin
        if not xLog.DisplayedColumnsValid then
        begin
          xLog.toBindables (xLog.Operation);
          xLog.InitDisplayedColumns(xLog.Operation, se.DisplayedLogColumns);
        end;
        CellText := xLog.DisplayedColumns.Strings[Column - Ord(logStdColumnCount)];
      end;
    end;
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.ClearLogItemsActionExecute(Sender: TObject);
begin
  if se.displayedLogs.Count > 0 then
  begin
    if (not xmlUtil.doConfirmRemovals)
    or BooleanPromptDialog ('Remove all messages') then
    begin
      se.doClearLogs := True;
    end;
  end;
end;

function TMainForm.EditScript(aXml: TObject): Boolean;
var
  xOperation: TWsdlOperation;
  xScript: TXml;
begin
  result := False;
  XmlUtil.PushCursor (crHourGlass);
  try
    xScript := (aXml as TXml).Parent as TXml;
  finally
    XmlUtil.PopCursor;
  end;
  xOperation := se.CreateScriptOperation(xScript);
  try
    if xOperation.PrepareErrors <> '' then
      if not BooleanPromptDialog (xOperation.PrepareErrors + LineEnding + 'Continue') then
        Exit;
    Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
    try
      EditOperationScriptForm.ScriptName := xOperation.Name;
      EditOperationScriptForm.After := False;
      EditOperationScriptForm.WsdlOperation := xOperation;
      EditOperationScriptForm.ScriptEdit.Lines.Text := xOperation.BeforeScriptLines.Text;
      EditOperationScriptForm.ShowModal;
      if EditOperationScriptForm.ModalResult = mrOk then
      begin
        (aXml as TXml).Value := EditOperationScriptForm.ScriptEdit.Lines.Text;
        (aXml as TXml).Checked := True;
        result := True;
      end;
    finally
      FreeAndNil(EditOperationScriptForm);
    end;
  finally
    xOperation.Wsdl.Free;
    xOperation.Free;
  end;
end;

procedure TMainForm.EditBetweenScriptMenuItemClick(Sender: TObject);
begin
  EditScriptButtonClick(nil);
end;

procedure TMainForm.EditScriptMenuItemClick(Sender: TObject);
var
  xXml: TXml;
  xXsd: TXsd;
  xEnum: TXsdEnumeration;
  o: Integer;
begin
  xXsd := ScriptsXsd.XsdByCaption ['Scripts.Script.Code'];
  xXsd.EditProcedure := EditScript;
  xXsd := ScriptsXsd.XsdByCaption ['Scripts.Script.Invoke.operations.name'];
  while xXsd.sType.Enumerations.Count > 0 do
  begin
    xXsd.sType.Enumerations.Objects[0].Free;
    xXsd.sType.Enumerations.Delete(0);
  end;
  for o := 0 to allAliasses.Count - 1 do
  begin
    xEnum := TXsdEnumeration.Create;
    xEnum.Value := allAliasses.Operations[o].Alias;
    xXsd.sType.Enumerations.AddObject(xEnum.Value, xEnum);
  end;
  xXml := TXml.Create;
  try
    xXml.CopyDownLine(se.Scripts, True);
    if EditXmlXsdBased ( 'Scripts'
                       , ''
                       , ''
                       , ''
                       , False
                       , xXml.Items.Count > 1
                       , esUsed
                       , ScriptsXsd
                       , xXml
                       ) then
    begin
      stubChanged := True;
      se.Scripts.CopyDownLine(xXml, True);
      CreateScriptsSubMenuItems;
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.NewStubCaseActionExecute(Sender: TObject);
begin
  if not InactiveAfterPrompt then Exit;
  if OkToOpenStubCase then
  begin
    ClearConsole;
    stubChanged := False;
    captionFileName := '';
    se.Clear;
    CreateScriptsSubMenuItems;
    Wsdl := nil;
    UpdateCaption;
    UpdateVisibiltyOfOperations;
    se.Clear;
  end;
end;

procedure TMainForm.ClearExceptionsActionUpdate(Sender: TObject);
begin
  ClearExceptionsAction.Enabled := (se.displayedExceptions.Count > 0);
end;

procedure TMainForm.ClearExceptionsActionExecute(Sender: TObject);
begin
  ExceptionsVTS.Clear;
  se.AcquireLogLock;
  try
    se.displayedExceptions.Clear;
  finally
    se.ReleaseLogLock;
  end;
  ExceptionMemo.Text := '';
end;

procedure TMainForm.ExceptionsVTSFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xLog: TExceptionLog;
begin
  Sender.Selected[Sender.FocusedNode] := True;
  xLog := NodeToExceptionLog(Sender, Node);
  if Assigned(xLog) and (xLog is TExceptionLog) then
  begin
    ExceptionMemo.Text := xLog.Text;
    ExceptionStatusBar.Panels.Items[0].Text := '[' + IntToStr(xLog.Nr)
      + ' : ' + IntToStr(se.displayedExceptions.Count) + ']';
  end
  else
  begin
    ExceptionMemo.Text := '';
    ExceptionStatusBar.Panels.Items[0].Text := '';
  end;
end;

function TMainForm.NodeToOperation(aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode): TWsdlOperation;
var
  Data: POperationTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned(Data) then
    begin
      result := Data.Operation;
    end;
  end;
end;

procedure TMainForm.Notify(const aString: String);
begin
  if doNotify then
    LogServerNotification(aString);
end;

function TMainForm.NodeToExceptionLog(aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode): TExceptionLog;
var
  Data: PExceptionTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned(Data) then
    begin
      result := Data.xLog;
    end;
  end;
end;

function TMainForm .NodeToSnapshot (aDoClaimReport: Boolean; aTreeView : TBaseVirtualTree ;
  aNode : PVirtualNode ): TSnapshot ;
var
  Data: PReportTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    if aDoClaimReport then
      se.AcquireLogLock;
    try
      Data := aTreeView.GetNodeData(aNode);
      if Assigned(Data) then
      begin
        result := Data.Report;
        if aDoClaimReport then
          result.Claim;
      end;
    finally
      if aDoClaimReport then
        se.ReleaseLogLock;
    end;
  end;
end;

procedure TMainForm.ExceptionsVTSGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xLog: TExceptionLog;
begin
  try
    xLog := NodeToExceptionLog(Sender, Node);
    if Assigned(xLog) and (xLog is TExceptionLog) then
    begin
      case Column of
        0:
          CellText := DateTimeToStr(xLog.TimeStamp);
        1:
          CellText := xLog.Text;
      end;
    end
    else
      CellText := '';
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.OperationReqsTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xOperation: TWsdlOperation;
  swap: TVTFocusChangeEvent;
begin
  Sender.Selected[Sender.FocusedNode] := True;
  xOperation := NodeToOperation(Sender, Node);
  if Assigned(xOperation) and (xOperation is TWsdlOperation) then
  begin
    swap := OperationReqsTreeView.OnFocusChanged;
    try
      OperationReqsTreeView.OnFocusChanged := nil;
      WsdlOperation := xOperation;
    finally
      OperationReqsTreeView.OnFocusChanged := swap;
    end;
  end;
end;

procedure TMainForm.OperationReqsTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xOperation: TWsdlOperation;
begin
  try
    xOperation := NodeToOperation(Sender, Node);
    if Assigned(xOperation) then
    begin
      case Column of
        Ord (operationsColumnAlias):
          CellText := xOperation.Alias;
      end;
    end
    else
      CellText := '';
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.OperationWsaActionExecute(Sender: TObject);
var
  reqWsaXml: TXml;
  rpyWsaXml: TXml;
begin
  if not Assigned(_WsdlWsaXsd) then
    raise Exception.Create
      ('Function should be disabled since wsdXsd not known');
  if Assigned(WsdlOperation) then
  begin
    reqWsaXml := TXml.Create(-10000, _WsdlWsaXsd);
    rpyWsaXml := TXml.Create(-10000, _WsdlWsaXsd);
    try
      reqWsaXml.CheckDownLine(False);
      reqWsaXml.LoadValues(WsdlOperation.reqWsaXml, False, True);
      rpyWsaXml.CheckDownLine(False);
      rpyWsaXml.LoadValues(WsdlOperation.rpyWsaXml, False, True);
      Application.CreateForm(TwsaConfigForm, wsaConfigForm);
      try
        wsaConfigForm.doReadOnly := se.IsActive;
        wsaConfigForm.Caption := 'Configure WS-Addressing';
        wsaConfigForm.wsaEnabled := WsdlOperation.wsaEnabled;
        wsaConfigForm.wsaSpecificMustUnderstand :=
          WsdlOperation.wsaSpecificMustUnderstand;
        wsaConfigForm.wsaMustUnderstand := WsdlOperation.wsaMustUnderstand;
        { }
        if WsdlOperation.StubAction = saRequest then
          wsaConfigForm.wsaXml := reqWsaXml;
        { }
        wsaConfigForm.ShowModal;
        if wsaConfigForm.ModalResult = mrOk then
        begin
          WsdlOperation.wsaEnabled := wsaConfigForm.wsaEnabled;
          WsdlOperation.wsaSpecificMustUnderstand :=
            wsaConfigForm.wsaSpecificMustUnderstand;
          WsdlOperation.wsaMustUnderstand := wsaConfigForm.wsaMustUnderstand;
          WsdlOperation.reqWsaXml.CheckDownLine(False);
          WsdlOperation.reqWsaXml.LoadValues(reqWsaXml, False, True);
          WsdlOperation.rpyWsaXml.CheckDownLine(False);
          WsdlOperation.rpyWsaXml.LoadValues(rpyWsaXml, False, True);
          stubChanged := True;
        end;
      finally
        FreeAndNil(wsaConfigForm);
      end;
    finally
      reqWsaXml.Free;
      rpyWsaXml.Free;
    end;
  end
  else
    raise Exception.Create('no operation');
end;

procedure TMainForm.OperationWsaActionUpdate(Sender: TObject);
begin
  OperationWsaAction.Enabled := (not se.IsActive)
                            and Assigned(_WsdlWsaXsd)
                            and Assigned(WsdlOperation)
                              ;
end;

procedure TMainForm.FocusOperationsReqVTS;
var
  xNode: PVirtualNode;
begin
  xNode := OperationReqsTreeView.GetFirst;
  while not(xNode = nil) do
  begin
    if NodeToOperation(OperationReqsTreeView, xNode) = WsdlOperation then
    begin
      OperationReqsTreeView.FocusedNode := xNode;
      OperationReqsTreeView.Selected[xNode] := True;
      exit;
    end;
    xNode := OperationReqsTreeView.GetNext(xNode);
  end;
end;

procedure TMainForm.ExchangeMessages(fReply, pReply: TWsdlMessage);
var
  f, p: Integer;
begin
  f := WsdlOperation.Messages.IndexOfObject(fReply);
  p := WsdlOperation.Messages.IndexOfObject(pReply);
  if (f > -1) and (p > -1) then
  begin
    WsdlOperation.Messages.Objects[f] := pReply;
    WsdlOperation.Messages.Objects[p] := fReply;
  end;
end;

procedure TMainForm.SelectMessageColumnsActionUpdate(Sender: TObject);
begin
  SelectMessageColumnsAction.Enabled := Assigned(WsdlOperation);
end;

procedure TMainForm.SelectMessageColumnsActionExecute(Sender: TObject);
begin
  if not InactiveAfterPrompt then Exit;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  try
    GridView.BeginUpdate;
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.GroupAllowed := True;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    SelectElementsForm.ControlBinds := WsdlOperation.Messages.Messages[0].ColumnXmls;
    SelectElementsForm.ShowModal;
    if SelectElementsForm.stubChanged then
    begin
      se.UpdateReplyColumns(WsdlOperation);
      UpdateMessagesGrid;
      stubChanged := stubChanged or SelectElementsForm.stubChanged;
    end;
  finally
    GridView.EndUpdate;
    FreeAndNil(SelectElementsForm);
  end;
end;

procedure TMainForm.RemoveMessageColumns;
var
  c: Integer;
begin
  GridView.FocusedColumn := NScripts;
  for c := GridView.Header.Columns.Count - 1 downto NScripts do
    ColumnWidths.Values[GridView.Header.Columns[c].Text] :=
      IntToStr (GridView.Header.Columns[c].Width);
  if Assigned(WsdlOperation) then
  begin
    try
      while GridView.Header.Columns.Count >
        (NScripts + 1 + WsdlOperation.CorrelationBindables.Count +
          WsdlOperation.Messages.Messages[0].ColumnXmls.Count) do
        GridView.Header.Columns.Delete(GridView.Header.Columns.Count - 1);
      while GridView.Header.Columns.Count <
        (NScripts + 1 + WsdlOperation.CorrelationBindables.Count +
          WsdlOperation.Messages.Messages[0].ColumnXmls.Count) do
        GridView.Header.Columns.Add;
    except
    end;
  end;
end;

procedure TMainForm.UpdateMessagesGrid;
var
  X, c: Integer;
  vc: TVirtualTreeColumn;
begin
  RemoveMessageColumns;
  c := Ord (operationsColumnAfterScript) + 1;
  vc := GridView.Header.Columns.Items[c];
  if WsdlOperation.StubAction = saRequest then
    vc.Text := 'Request'
  else
    vc.Text := 'Reply';
  vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
  Inc(c);
  if WsdlOperation.Messages.Count > 0 then
  begin
    with WsdlOperation.Messages.Messages[0] do
    begin
      for X := 0 to CorrelationBindables.Count - 1 do
      begin
        vc := GridView.Header.Columns.Items[c];
        if Assigned(CorrelationBindables.Bindables[X]) then
          vc.Text := CorrelationBindables.Bindables[X].Name
        else
          vc.Text := '?';
        vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
        Inc(c);
      end;
      for X := 0 to ColumnXmls.Count - 1 do
      begin
        vc := GridView.Header.Columns.Items[c];
        vc.Text := LastCaption(ColumnXmls.Strings[X]);
        vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
        Inc(c);
      end;
    end;
  end;
  lastWsdlOperation := WsdlOperation;
end;

procedure TMainForm .UpdateLogCorrelationIds (aWsdlOperation : TWsdlOperation );
var
  x: Integer;
begin
  se.AcquireLogLock;
  try
  for x := 0 to se.displayedLogs.Count - 1 do with se.displayedLogs.LogItems[x] do
  begin
    if Operation = aWsdlOperation then
    begin
      Operation.RequestStringToBindables(RequestBody);
      CorrelationId := Operation.CorrelationIdAsText('; ');
    end;
  end;
  finally
    MessagesVTS.Invalidate;
    se.ReleaseLogLock;
  end;
end;

procedure TMainForm.FocusOnBind(aBind: TCustomBindable);
  procedure _ForceVisibility(aNode: PVirtualNode);
  begin
    if aNode = aNode.NextSibling then
      exit;
    _ForceVisibility(aNode.Parent);
    InWsdlTreeView.IsVisible[aNode] := True;
  end;

var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  xNode := InWsdlTreeView.GetFirst; // search from begin
  while not(xNode = nil) do
  begin
    xBind := NodeToBind(InWsdlTreeView, xNode);
    if xBind = aBind then
    begin
      _ForceVisibility(xNode);
      InWsdlTreeView.InvalidateNode(xNode);
      InWsdlTreeView.FocusedNode := xNode;
      InWsdlTreeView.FocusedColumn := treeValueColumn;
      exit;
    end;
    xNode := InWsdlTreeView.GetNext(xNode);
  end;
end;

procedure TMainForm.setWsdlMessage(const Value: TWsdlMessage);
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  xNode := GridView.GetFirst;
  while Assigned(xNode) do
  begin
    NodeToMessage(GridView, xNode, xMessage);
    if xMessage = Value then
    begin
      GridViewFocusedNode(xNode);
      exit;
    end;
    xNode := GridView.GetNext(xNode);
  end;
end;

function TMainForm.getWsdlMessage: TWsdlMessage;
begin
  Result := nil; //avod warning
  NodeToMessage(GridView, GridView.FocusedNode, result);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  X, wBttn: Integer;
  xIniFile: TFormIniFile;
  xXml: TXml;
begin
  (MessagesTabControl as TWinControl).Color := Self.Color;
  MessagesTabCaption := LogTabControl.Tabs [Ord (spMessages)];
  notifyTabCaption := LogTabControl.Tabs [Ord (spNotifications)];
  notifyTabImageIndex := 66;
//  ExceptionTabSheet.ImageIndex := -1;
  se := TWsdlProject.Create;
  sc := TWsdlControl.Create;
  sc.se := se;
  RefreshLogTimer.Enabled := True;
  NumberOfBlockingThreads := 0;
  se.OnBooleanDialog := BooleanPromptDialog;
  se.OnStartBlockingThread := StartBlockingThreadEvent;
  se.OnTerminateBlockingThread := TerminateBlockingThreadEvent;
  se.OnStartNonBlockingThread := StartNonBlockingThreadEvent;
  se.OnTerminateNonBlockingThread := TerminateNonBlockingThreadEvent;
  sc.OnActivateEvent := ActivateCommand;
  sc.OnOpenProjectEvent := OpenProjectCommand;
  se.Notify := Notify;
  se.LogServerMessage := LogServerException;
  se.OnDebugOperationEvent := DebugOperation;
  se.FoundErrorInBuffer := FoundErrorInBuffer;
  se.OnReactivateEvent := ReactivateCommand;
  sc.OnQuitEvent := QuitCommand;
  se.OnRestartEvent := RestartCommand;
  se.OnReloadDesignEvent := ReloadDesignCommand;
  se.OnNeedTacoHostData := NeedTacoHostData;
  se.OnTacoAutorize := OnTacoAuthorize;
  DecryptString := doDecryptString;
  EncryptString := doEncryptString;
  xmlUtil.doExpandFull := True;
  ColumnWidths := TStringList.Create;
  QueueNameList := TStringList.Create;
  QueueNameList.Sorted := True;
  QueueNameList.Duplicates := dupIgnore;
  Application.OnException := HandleException;
  { }
  Randomize;
  startStopShortCut := startAction.ShortCut;
  wBttn := MessagesVTS.Header.Columns[Ord(logExpectedColumn)].Width;
  xIniFile := TFormIniFile.Create(Self, True);
  doShowDesignSplitVertical := xIniFile.BooleanByNameDef['doShowDesignSplitVertical', False];
  xIniFile.Restore;
  if doShowDesignSplitVertical then
    GridDataPanel.Width := xIniFile.IntegerByNameDef['GridDataPanelWidth', GridDataPanel.Width]
  else
    GridDataPanel.Height := xIniFile.IntegerByNameDef['GridDataPanelHeight', GridDataPanel.Height];
  ViewStyleComboBox.ItemIndex := Ord(xvAll);
  for X := 0 to Ord(logTimeColumn) - 1 do
    MessagesVTS.Header.Columns[X].Width := wBttn;
  for X := 0 to Ord(snapshotDateTimeColumn) - 1 do
    SnapshotsVTS.Header.Columns[X].Width := wBttn;
  OperationReqsTreeView.Header.Columns[0].Width := wBttn;
  OperationReqsTreeView.Header.Columns[1].Width := wBttn;
  GridView.Header.Columns[0].Width := wBttn;
  GridView.Header.Columns[1].Width := wBttn;
  se.projectFileName := xIniFile.StringByName['WsdlStubFileName'];
  wsdlStubMessagesFileName := xIniFile.StringByName['WsdlStubMessagesFileName'];
  wsdlStubSnapshotsFileName := xIniFile.StringByName['wsdlStubSnapshotsFileName'];
  DisclaimerAccepted := xIniFile.BooleanByName['DisclaimerAccepted'];
  BetaMode := xIniFile.BooleanByNameDef['BetaMode', False];
  ListofOperationsMenuItem.Checked := xIniFile.BooleanByNameDef
    ['ListofOperationsVisible', True];
  ScriptPanel.Visible := ListofOperationsMenuItem.Checked;
  ScriptSplitter.Visible := ListofOperationsMenuItem.Checked;
  SchemapropertiesMenuItem.Checked := xIniFile.BooleanByNameDef
    ['SchemaPropertiesVisible', True];
  XsdPanel.Visible := SchemapropertiesMenuItem.Checked;
  xsdSplitter.Visible := SchemapropertiesMenuItem.Checked;
  WsdlInformationMenuItem.Checked := xIniFile.BooleanByNameDef
    ['WsdlInformationVisible', True];
  WsdlInfoPanel.Visible := WsdlInformationMenuItem.Checked;
  se.LogFilter.FilterStyle := TLogFilterStyle
    (xIniFile.IntegerByNameDef['LogFilter.FilterStyle', 0]);
  se.LogFilter.MatchAny := xIniFile.BooleanByNameDef['LogFilter.MatchAny',
    False];
  se.LogFilter.StubActionEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.StubActionEnabled', False];
  se.LogFilter.StubActionEquals := xIniFile.BooleanByNameDef
    ['LogFilter.StubActionEquals', True];
  se.LogFilter.StubAction := TStubAction
    (xIniFile.IntegerByNameDef['LogFilter.StubAction', 0]);
  se.LogFilter.MiMEnabled := xIniFile.BooleanByNameDef['LogFilter.MiMEnabled',
    False];
  se.LogFilter.RequestMiMEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.RequestMiMEnabled', True];
  se.LogFilter.ReplyMiMEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.ReplyMiMEnabled', True];
  se.LogFilter.MessageValidationEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.MessageValidationEnabled', False];
  se.LogFilter.RequestValidationEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.RequestValidationEnabled', True];
  se.LogFilter.ReplyValidationEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.ReplyValidationEnabled', True];
  se.LogFilter.ExceptionEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.ExceptionEnabled', False];
  se.LogFilter.ExceptionEquals := xIniFile.BooleanByNameDef
    ['LogFilter.ExceptionEquals', False];
  se.LogFilter.Exception := xIniFile.StringByNameDef['LogFilter.Exception', ''];
  se.LogFilter.ExceptionRegExp := xIniFile.BooleanByNameDef
    ['LogFilter.ExceptionRegExp', False];
  se.LogFilter.ServiceEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.ServiceEnabled', False];
  se.LogFilter.ServiceEquals := xIniFile.BooleanByNameDef
    ['LogFilter.ServiceEquals', True];
  se.LogFilter.Service := xIniFile.StringByNameDef['LogFilter.Service', ''];
  se.LogFilter.ServiceRegExp := xIniFile.BooleanByNameDef
    ['LogFilter.ServiceRegExp', False];
  se.LogFilter.OperationEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.OperationEnabled', False];
  se.LogFilter.OperationEquals := xIniFile.BooleanByNameDef
    ['LogFilter.OperationEquals', True];
  se.LogFilter.Operation := xIniFile.StringByNameDef['LogFilter.Operation', ''];
  se.LogFilter.OperationRegExp := xIniFile.BooleanByNameDef
    ['LogFilter.OperationRegExp', False];
  se.LogFilter.CorrelationEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.CorrelationEnabled', False];
  se.LogFilter.CorrelationEquals := xIniFile.BooleanByNameDef
    ['LogFilter.CorrelationEquals', True];
  se.LogFilter.Correlation := xIniFile.StringByNameDef['LogFilter.Correlation',
    ''];
  se.LogFilter.CorrelationRegExp := xIniFile.BooleanByNameDef
    ['LogFilter.CorrelationRegExp', False];
  se.LogFilter.RequestEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.RequestEnabled', False];
  se.LogFilter.RequestEquals := xIniFile.BooleanByNameDef
    ['LogFilter.RequestEquals', True];
  se.LogFilter.Request := xIniFile.StringByNameDef['LogFilter.Request', ''];
  se.LogFilter.ReplyEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.ReplyEnabled', False];
  se.LogFilter.ReplyEquals := xIniFile.BooleanByNameDef['LogFilter.ReplyEquals',
    True];
  se.LogFilter.Reply := xIniFile.StringByNameDef['LogFilter.Reply', ''];
  se.LogFilter.UnexpectedValuesEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.UnexpectedValuesEnabled', False];
  se.LogFilter.RemarksEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.RemarksEnabled', False];
  tacoHost := xIniFile.StringByNameDef['tacoHost', 'localhost'];
  tacoPort := xIniFile.IntegerByNameDef['tacoPort', 1025];
  CollapseHeaders := xIniFile.BooleanByNameDef['CollapseHeaders', True];
  InWsdlTreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  InWsdlTreeView.RootNodeCount := 0;
  OperationReqsTreeView.NodeDataSize := SizeOf(TOperationTreeRec);
  OperationReqsTreeView.RootNodeCount := 0;
  GridView.NodeDataSize := SizeOf(TMessageTreeRec);
  GridView.RootNodeCount := 0;
  SnapshotsVTS.NodeDataSize := SizeOf(TSnapshotTreeRec);
  SnapshotsVTS.RootNodeCount := 0;
  MessagesVTS.NodeDataSize := SizeOf(TLogTreeRec);
  MessagesVTS.RootNodeCount := 0;
  ExceptionsVTS.NodeDataSize := SizeOf(TExceptionTreeRec);
  ExceptionsVTS.RootNodeCount := 0;
  FileNameList := TStringList.Create;
  ReopenCaseList := TStringList.Create;
  ReopenCaseList.Text := xIniFile.StringByName['RecentFiles'];
  WsdlPaths := TStringList.Create;
  WsdlPaths.Sorted := True;

  se.notStubbedExceptionMessage := xIniFile.StringByNameDef
    ['notStubbedExceptionMessage', 'No operation recognized'];
  se.doViaProxyServer := xIniFile.BooleanByName['doViaProxyServer'];
  doScrollMessagesIntoView := xIniFile.BooleanByNameDef
    ['doScrollMessagesIntoView', True];
  doScrollExceptionsIntoView := xIniFile.BooleanByNameDef
    ['doScrollExceptionsIntoView', True];
  xmlUtil.doConfirmRemovals := xIniFile.BooleanByNameDef['doConfirmRemovals',
    True];
  xsdValidateAssignmentsAgainstSchema := xIniFile.BooleanByNameDef
    ['doValidateScriptAssignmentAgainstSchema', False];
  // se.HTTPServer.KeepAlive := xIniFile.BooleanByNameDef ['HTTPServer.KeepAlive', se.HTTPServer.KeepAlive];
  se.HTTPServer.ListenQueue := xIniFile.IntegerByNameDef
    ['HTTPServer.ListenQueue',
    se.HTTPServer.ListenQueue];
  se.HTTPServer.MaxConnections := xIniFile.IntegerByNameDef
    ['HTTPServer.MaxConnections', se.HTTPServer.MaxConnections];
  se.ViaProxyServer := xIniFile.StringByNameDef['ViaProxyServer', 'localhost'];
  se.ViaProxyPort := StrToIntDef(xIniFile.StringByName['ViaProxyPort'], 8081);
  if (se.mmqqMqInterface.MQServerOK and se.mmqqMqInterface.MQClientOK) then
    se.mqUse := TMqUse(StrToIntDef(xIniFile.StringByName['mqUse'],
        Ord(mquServer)));
  if (se.mmqqMqInterface.MQServerOK and (not se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquServer;
  if (not se.mmqqMqInterface.MQServerOK and (se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquClient;
  if (not se.mmqqMqInterface.MQServerOK and (not se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquUndefined;
  se.mqMaxWorkingThreads := xIniFile.IntegerByNameDef['MaxWorkingThreads', 15];
  se.CompareLogOrderBy := TCompareLogOrderBy
    (xIniFile.IntegerByNameDef['CompareLogOrderBy', Ord(clTimeStamp)]);
  se.ShowLogCobolStyle := TShowLogCobolStyle
    (xIniFile.IntegerByNameDef['ShowLogCobolStyle', Ord(slCobol)]);
  mqServerEnv := GetEnvironmentVariable('MQSERVER');
  ColumnWidths.Text := xIniFile.StringByNameDef['ColumnWidths', ''];
  xsdElementsWhenRepeatable := StrToIntDef
    (xIniFile.StringByName['ElementsWhenRepeatable'], 1);
  doShowDesignAtTop := xIniFile.BooleanByNameDef['doShowDesignAtTop', True];
  bgCorrelationItemColor := xIniFile.IntegerByNameDef['bgCorrelationItemColor',
    bgCorrelationItemColor];
  bgExpectedValueColor := xIniFile.IntegerByNameDef['bgExpectedValueColor',
    bgExpectedValueColor];
  bgNilValueColor := xIniFile.IntegerByNameDef['bgNilValueColor',
    bgNilValueColor];
  DesignPanelAtTopMenuItem.Checked := doShowDesignAtTop;
  xXml := TXml.Create;
  try
    xXml.LoadFromString(xIniFile.StringByName['Options'], nil);
    if xXml.Name = 'wsdlStubOptions' then
      OptionsFromXml(xXml);
  finally
    xXml.Free;
  end;
  xIniFile.Free;
  wsdlStubInitialise;
  se.stubRead := False;
  stubChanged := False;
  nStubs := 0;
  freeStubs := -1;
  logChartToolButton.Visible := (WindowsUserName = 'Jan')
                             or (WindowsUserName = 'BouwmanJW')
                              ;
  BrowseMqMenuItem.Visible :=
    (se.mmqqMqInterface.MQServerOK or se.mmqqMqInterface.MQClientOK);
  BrowseMqButton.Visible :=
    (se.mmqqMqInterface.MQServerOK or se.mmqqMqInterface.MQClientOK);
  HelpAction.Caption := 'Help on ' + _progName;
  UpdateVisibiltyOfOperations;
  SetBetaMode;
  _OnParseErrorEvent := ParserError;
  _WsdlOnMessageChange := OnMessageChanged;
  try
    UpdateVisibiltyTreeView(False);
  except
  end;
  xmlUtil.AcquireLock := AcquireLock;
  xmlUtil.ReleaseLock := ReleaseLock;
  _OnEndUpdate := EndUpdate;
  _OnBeginUpdate := BeginUpdate;
  Xmlz.OnNotify := LogServerNotification;
  // due to a bug in TPageControl, not al tabs are visible....
  // statements below make all tabs visible again...??
//  DownPageControl.TabPosition := tpBottom;
//  DownPageControl.TabPosition := tpTop;
  if ParamStr(1) <> '' then
  begin
    Update;
    se.projectFileName := ParamStr(1);
    OpenStubCase(se.projectFileName);
    se.Activate(True);
    CheckBoxClick(nil);
    if ParamStr(2) <> '' then
    begin
      ExecuteScript(se, ParamStr(2));
    end;
  end;
  MainToolBarDesignedButtonCount := MainToolBar.ButtonCount;
  CreateScriptsSubMenuItems;
  try
    sc.Active := True;
  except
  end;
  systemStarting := False;
end;

function TMainForm.inImageArea: Boolean;
var
  xRect: TRect;
  xImageIndex: Integer;
  xGosthed: Boolean;
begin
  xGosthed := False; //avoid warning
  result := False;
  xImageIndex := -1;
  if Assigned(GridView.FocusedNode) then
  begin
    xRect := GridView.GetDisplayRect(GridView.FocusedNode,
      GridView.FocusedColumn, False, False, False);
    if ((grid_x - xRect.Left) < 20) then
      GridViewGetImageIndex(GridView, GridView.FocusedNode, ikNormal,
        GridView.FocusedColumn, xGosthed, xImageIndex);
  end;
  result := (xImageIndex > -1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  xIniFile: TFormIniFile;
begin
  if Assigned(se) then
    se.Activate(False);
  ClearConsole;
  xIniFile := TFormIniFile.Create(self, False);
  xIniFile.BooleanByName['DisclaimerAccepted'] := DisclaimerAccepted;
  xIniFile.BooleanByName['BetaMode'] := BetaMode;
  xIniFile.BooleanByName['doShowDesignAtTop'] := doShowDesignAtTop;
  xIniFile.StringByName['RecentFiles'] := ReopenCaseList.Text;
  xIniFile.StringByName['ColumnWidths'] := ColumnWidths.Text;
  xIniFile.BooleanByName['ListofOperationsVisible'] :=
    ListofOperationsMenuItem.Checked;
  xIniFile.BooleanByName['SchemaPropertiesVisible'] :=
    SchemapropertiesMenuItem.Checked;
  xIniFile.BooleanByName['WsdlInformationVisible'] :=
    WsdlInformationMenuItem.Checked;
  xIniFile.BooleanByName['doShowDesignSplitVertical'] := doShowDesignSplitVertical;
  xIniFile.IntegerByName['GridDataPanelWidth'] := GridDataPanel.Width;
  xIniFile.IntegerByName['GridDataPanelHeight'] := GridDataPanel.Height;
  xIniFile.StringByName['WsdlStubFileName'] := se.projectFileName;
  xIniFile.StringByName['WsdlStubMessagesFileName'] := wsdlStubMessagesFileName;
  xIniFile.StringByName['wsdlStubSnapshotsFileName'] := wsdlStubSnapshotsFileName;
  xIniFile.StringByName['tacoHost'] := tacoHost;
  xIniFile.IntegerByName['tacoPort'] := tacoPort;
  xIniFile.IntegerByName['LogFilter.FilterStyle'] := Ord
    (se.LogFilter.FilterStyle);
  xIniFile.BooleanByName['LogFilter.MatchAny'] := se.LogFilter.MatchAny;
  xIniFile.BooleanByName['LogFilter.StubActionEnabled'] :=
    se.LogFilter.StubActionEnabled;
  xIniFile.BooleanByName['LogFilter.StubActionEquals'] :=
    se.LogFilter.StubActionEquals;
  xIniFile.IntegerByName['LogFilter.StubAction'] := Ord(se.LogFilter.StubAction);
  xIniFile.BooleanByName['LogFilter.MiMEnabled'] := se.LogFilter.MiMEnabled;
  xIniFile.BooleanByName['LogFilter.RequestMiMEnabled'] :=
    se.LogFilter.RequestMiMEnabled;
  xIniFile.BooleanByName['LogFilter.ReplyMiMEnabled'] :=
    se.LogFilter.ReplyMiMEnabled;
  xIniFile.BooleanByName['LogFilter.MessageValidationEnabled'] :=
    se.LogFilter.MessageValidationEnabled;
  xIniFile.BooleanByName['LogFilter.RequestValidationEnabled'] :=
    se.LogFilter.RequestValidationEnabled;
  xIniFile.BooleanByName['LogFilter.ReplyValidationEnabled'] :=
    se.LogFilter.ReplyValidationEnabled;
  xIniFile.BooleanByName['LogFilter.ExceptionEnabled'] :=
    se.LogFilter.ExceptionEnabled;
  xIniFile.BooleanByName['LogFilter.ExceptionEquals'] :=
    se.LogFilter.ExceptionEquals;
  xIniFile.StringByName['LogFilter.Exception'] := se.LogFilter.Exception;
  xIniFile.BooleanByName['LogFilter.ExceptionRegExp'] :=
    se.LogFilter.ExceptionRegExp;
  xIniFile.BooleanByName['LogFilter.ServiceEnabled'] :=
    se.LogFilter.ServiceEnabled;
  xIniFile.BooleanByName['LogFilter.ServiceEquals'] :=
    se.LogFilter.ServiceEquals;
  xIniFile.StringByName['LogFilter.Service'] := se.LogFilter.Service;
  xIniFile.BooleanByName['LogFilter.ServiceRegExp'] :=
    se.LogFilter.ServiceRegExp;
  xIniFile.BooleanByName['LogFilter.OperationEnabled'] :=
    se.LogFilter.OperationEnabled;
  xIniFile.BooleanByName['LogFilter.OperationEquals'] :=
    se.LogFilter.OperationEquals;
  xIniFile.StringByName['LogFilter.Operation'] := se.LogFilter.Operation;
  xIniFile.BooleanByName['LogFilter.OperationRegExp'] :=
    se.LogFilter.OperationRegExp;
  xIniFile.BooleanByName['LogFilter.CorrelationEnabled'] :=
    se.LogFilter.CorrelationEnabled;
  xIniFile.BooleanByName['LogFilter.CorrelationEquals'] :=
    se.LogFilter.CorrelationEquals;
  xIniFile.StringByName['LogFilter.Correlation'] := se.LogFilter.Correlation;
  xIniFile.BooleanByName['LogFilter.CorrelationRegExp'] :=
    se.LogFilter.CorrelationRegExp;
  xIniFile.BooleanByName['LogFilter.RequestEnabled'] :=
    se.LogFilter.RequestEnabled;
  xIniFile.BooleanByName['LogFilter.RequestEquals'] :=
    se.LogFilter.RequestEquals;
  xIniFile.StringByName['LogFilter.Request'] := se.LogFilter.Request;
  xIniFile.BooleanByName['LogFilter.ReplyEnabled'] := se.LogFilter.ReplyEnabled;
  xIniFile.BooleanByName['LogFilter.ReplyEquals'] := se.LogFilter.ReplyEquals;
  xIniFile.StringByName['LogFilter.Reply'] := se.LogFilter.Reply;
  xIniFile.BooleanByName['LogFilter.UnexpectedValuesEnabled'] :=
    se.LogFilter.UnexpectedValuesEnabled;
  xIniFile.BooleanByName['LogFilter.RemarksEnabled'] :=
    se.LogFilter.RemarksEnabled;
  xIniFile.BooleanByName['CollapseHeaders'] := CollapseHeaders;
  xIniFile.StringByName['QueueNames'] := QueueNameList.Text;
  WsdlInformationMenuItem.Checked := xIniFile.BooleanByNameDef['', True];
  xIniFile.Save;
  xIniFile.Free;
  QueueNameList.Free;
  ReopenCaseList.Free;
  FileNameList.Free;
  WsdlPaths.Free;
  FreeAndNil(se);
  FreeAndNil(sc);
  ColumnWidths.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  X: Integer;
begin
  if not DisclaimerAccepted then
  begin
    Application.CreateForm(TDisclaimerForm, DisclaimerForm);
    try
      DisclaimerForm.Caption := _progName + DisclaimerForm.Caption;
      DisclaimerForm.ShowModal;
      if (DisclaimerForm.ModalResult = mrOk) and
        (DisclaimerForm.DisclaimerAcceptedCheckBox.Checked) then
        DisclaimerAccepted := True;
    finally
      FreeAndNil(DisclaimerForm);
      if not DisclaimerAccepted then
      begin
        ShowMessage('Because you did not accept the disclaimer, ' + _progName +
            ' will now close');
        Close;
      end;
    end;
  end;
  ActionComboBoxChange(nil);
  // next loop somehow prevents that an excception is thrown when another tab is choosen (???)
//  for X := 0 to DownPageControl.PageCount - 1 do
//    DownPageControl.ActivePageIndex := X;
  LogTabControl.TabIndex := Ord (spNotifications);
  MessagesTabControl.TabIndex := Ord (slRequestBody);
  ShowChosenLogTab;
  DocumentationPanel.Align := alClient;
  NotificationsPanel.Align := alClient;
  MessagesPanel.Align := alClient;
  SnapshotsPanel.Align := alClient;
{$ifdef UNIX}
  LogTabControl.Height := LogTabControl.Height + 3;
{$endif}
  CheckBoxClick(nil);
  stubChanged := False;
  if (not se.Licensed)
  and (GetAuthError <> '') then
    ShowMessage ('Not licenced' + LineEnding + LineEnding + GetAuthError);
end;

procedure TMainForm.GridViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TCustomBindable;
  Xml: TXml;
  XmlAttr: TXmlAttribute;
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  NodeToMessage(Sender, Node, xMessage);
  if Column < NScripts then exit;
  if (Column = NScripts) then
  begin
    if xMessage.Disabled then
      if (Node <> GridView.GetFirst) then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsStrikeOut] +
          [fsBold]
      else
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;
  if (Column - NScripts) <= xMessage.CorrelationBindables.Count then
    exit;
  xBind := nil;
  try
    xBind := xMessage.ColumnXmls.Bindables
      [Column - NScripts - xMessage.CorrelationBindables.Count - 1];
  except
  end;
  if not Assigned(xBind) then
    exit;
  Xml := nil;
  XmlAttr := nil;
  if xBind is TXmlAttribute then
    XmlAttr := xBind as TXmlAttribute;
  if xBind is TXml then
    Xml := xBind as TXml;

  {
    if (Node = (Sender as TVirtualStringTree).GetFirst) then
    begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
    end;
  }

  if xBind is TXmlAttribute then
  begin
    if ((Assigned(XmlAttr.XsdAttr)) and (XmlAttr.XsdAttr.Use = 'required')) then
    begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if (Sender.FocusedNode <> Node) or (Sender.FocusedColumn <> Column) then
      begin
        if (not XmlAttr.Checked) and (XmlAttr.Parent as TXml).CheckedAllUp then
          TargetCanvas.Font.Color := clRed {
            else
            if not (XmlAttr.Parent as TXml).CheckedAllUp then
            TargetCanvas.Font.Color := clBlue } ;
      end;
      {
        end
        else
        begin
        if (Sender.FocusedNode <> Node)
        or (Sender.FocusedColumn <> Column) then
        if not (XmlAttr.Parent as TXml).CheckedAllUp then
        TargetCanvas.Font.Color := clBlue;
      }
    end;
    exit;
  end;
  if xBind is TXml then
  begin
    if (not Assigned(Xml.Xsd)) then
    begin
      {
        if (Sender.FocusedNode <> Node)
        or (Sender.FocusedColumn <> Column) then
        begin
        if not (Xml.Parent as TXml).CheckedAllUp then
        TargetCanvas.Font.Color := clBlue	;
        end;
        }
      exit;
    end;
    try
      if Assigned(Xml.Xsd) and (StrToIntDef(Xml.Xsd.minOccurs, 0) > 0)
        and Assigned(Xml.Parent) and Assigned(TXml(Xml.Parent).Xsd) and
        (TXml(Xml.Parent).TypeDef.ContentModel <> 'Choice') then
      begin
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        if (Sender.FocusedNode <> Node) or (Sender.FocusedColumn <> Column) then
        begin
          if (not Xml.Checked) and (Xml.Parent as TXml).CheckedAllUp then
            TargetCanvas.Font.Color := clRed {
              else
              if not (Xml.Parent as TXml).CheckedAllUp then
              TargetCanvas.Font.Color := clBlue } ;
        end;
      end
      else
      begin
        if (Sender.FocusedNode <> Node) or (Sender.FocusedColumn <> Column) then
        begin
          if not(Xml.Parent as TXml).CheckedAllUp then
            TargetCanvas.Font.Color := clBlue;
        end;
      end;
    except
      ShowMessage('Error in logic for paintext [' + Xml.TagName + ';' + IntToStr
          (Column) + ']');
    end;
  end;
end;

procedure TMainForm.CopyGridActionExecute(Sender: TObject);
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    ClipBoard.AsText := vstToGrid(GridView, GridViewGetText);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.CopyGridActionUpdate(Sender: TObject);
begin
  try
    CopyGridAction.Enabled := Assigned(WsdlOperation)
                            ;
  except
  end;
end;

procedure TMainForm.PasteGridActionUpdate(Sender: TObject);
begin
  PasteGridAction.Enabled := Assigned(WsdlOperation)
                           ;
end;

procedure TMainForm.PasteGridActionExecute(Sender: TObject);
var
  swapNode: PVirtualNode;
  swapColumn: Integer;
begin
  if not ClipBoard.HasFormat(CF_TEXT) then
    raise Exception.Create('Clipboard does not contain text');
  if not InactiveAfterPrompt then Exit;
  try
    GridView.BeginUpdate;
    InWsdlTreeView.BeginUpdate;
    swapNode := GridView.FocusedNode;
    swapColumn := GridView.FocusedColumn;
    XmlUtil.PushCursor (crHourGlass);
    // vstFromGrid(GridView, ClipBoard.AsText, PasteGridOnNewText);
    PasteGridFromPasteBoard;
    DoColorBindButtons;
  finally
    GridViewFocusedNode(swapNode);
    GridView.FocusedColumn := swapColumn;
    GridView.Invalidate;
    GridView.EndUpdate;
    InWsdlTreeView.Invalidate;
    InWsdlTreeView.EndUpdate;
    GridView.SetFocus;
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowXml(aCaption: String; aXml: TXml);
begin
  Application.CreateForm(TShowXmlForm, ShowXmlForm);
  try
    ShowXmlForm.Caption := aCaption;
    ShowXmlForm.Bind := aXml;
    ShowXmlForm.isReadOnly := True;
    ShowXmlForm.ShowModal;
  finally
    FreeAndNil(ShowXmlForm);
  end;
end;

procedure TMainForm.ShowXmlInGrid(aXml: TXml; aReadOnly: Boolean);
begin
  Application.CreateForm(TXmlGridForm, XmlGridForm);
  try
    XmlGridForm.isReadOnly := aReadOnly;
    XmlGridForm.doConfirmRemovals := xmlUtil.doConfirmRemovals;
    XmlGridForm.Xml := aXml;
    XmlGridForm.ShowModal;
  finally
    FreeAndNil(XmlGridForm);
  end;
end;

procedure TMainForm.ShowIpm(aCaption: String; aIpm: TIpmItem);
begin
  Application.CreateForm(TShowXmlForm, ShowXmlForm);
  try
    ShowXmlForm.Caption := aCaption;
    ShowXmlForm.Bind := aIpm;
    ShowXmlForm.isReadOnly := True;
    ShowXmlForm.ShowModal;
  finally
    FreeAndNil(ShowXmlForm);
  end;
end;

procedure TMainForm.ShowTextAsGrid(aCaption, aText: String);
var
  xXml: TXml;
  xXsdDescr: TXsdDescr;
  swapMaxDepthXmlGen: Integer;
begin
  Application.CreateForm(TXmlGridForm, XmlGridForm);
  swapMaxDepthXmlGen := xsdMaxDepthXmlGen;
  xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
  try
    xXsdDescr := TXsdDescr.Create(1);
    try
      xXml := TXml.Create;
      try
        XmlUtil.PushCursor (crHourGlass);
        try
          xXml.LoadFromString(aText, nil);
          xmlUtil.CreateXsdFromXml(xXsdDescr, xXml, True);
        finally
          XmlUtil.PopCursor;
        end;
        try
          XmlGridForm.isReadOnly := True;
          XmlGridForm.doConfirmRemovals := xmlUtil.doConfirmRemovals;
          XmlGridForm.Xml := xXml;
          XmlGridForm.ShowModal;
        finally
        end;
      finally
        xXml.Free;
      end;
    finally
      xXsdDescr.Free;
    end;
  finally
    xsdMaxDepthXmlGen := swapMaxDepthXmlGen;
    FreeAndNil(XmlGridForm);
  end;
end;

procedure TMainForm.ShowTextAsXml(aCaption, aText: String);
var
  xXml: TXml;
  xXsdDescr: TXsdDescr;
begin
  XmlUtil.PushCursor (crHourGlass);
  xXsdDescr := TXsdDescr.Create(1);
  try
    xXml := TXml.Create;
    try
      try
        xXml.LoadFromString(aText, nil);
        if xXml.Name = '' then
          try
            xXml.LoadJsonFromString(aText, nil);
          except
            ShowInfoForm(aCaption + ' -- failed parsing', aText);
            exit;
          end;
        xmlUtil.CreateXsdFromXml(xXsdDescr, xXml, True);
      finally
        XmlUtil.PopCursor;
      end;
      Application.CreateForm(TShowXmlForm, ShowXmlForm);
      try
        ShowXmlForm.Caption := aCaption;
        ShowXmlForm.Bind := xXml;
        ShowXmlForm.isReadOnly := True;
        ShowXmlForm.ShowModal;
      finally
        FreeAndNil(ShowXmlForm);
      end;
    finally
      xXml.Free;
    end;
  finally
    xXsdDescr.Free;
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.CopyLogMemoTextToClipBrdActionExecute(Sender: TObject);
begin
  ClipBoard.AsText := LogMemo.Text;
end;

procedure TMainForm.CopyLogMemoTextToClipBrdActionUpdate(Sender: TObject);
begin
  CopyLogMemoTextToClipBrdAction.Enabled := (LogMemo.Text <> '');
end;

procedure TMainForm.ShowHttpReplyAsXMLActionExecute(Sender: TObject);
var
  xXml: TXml;
  xString: String;
begin
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.ReplyBody, '');
  if (xString <> '') then
  begin
    if Assigned (claimedLog.Operation)
    and (claimedLog.Operation.isOpenApiService) then
    begin
      with claimedLog.rpyBodyAsXml do
      try
        ShowTextAsXml('Reply as XML', AsText(False, 0, False, False));
      finally
        Free;
      end;
      exit;
    end;
    if Assigned(claimedLog.Mssg) then
    begin
      case claimedLog.Operation.WsdlService.DescriptionType of
        ipmDTFreeFormat:
          ShowInfoForm('Reply freeformat', xString);
        ipmDTCobol, ipmDTBmtp:
          begin
            if se.ShowLogCobolStyle = slCobol then
            begin (claimedLog.Operation.rpyBind as TIpmItem)
              .BufferToValues(FoundErrorInBuffer, xString);
              ShowIpm('Cobol view on Reply',
                claimedLog.Operation.rpyBind as TIpmItem);
            end
            else
            begin
              with claimedLog.rpyBodyAsXml do
                try
                  ShowTextAsXml('Reply as XML', AsText(False, 0, False, False));
                finally
                  Free;
                end;
            end;
          end;
        ipmDTXml:
          ShowTextAsXml('Reply as XML', xString);
        ipmDTXsd:
          ShowTextAsXml('Reply as XML', xString);
        ipmDTWsdl:
          ShowTextAsXml('Reply as XML', xString);
        ipmDTEmail:
          ShowInfoForm('Reply freeformat', xString);
        ipmDTSwiftMT:
          begin
            xXml := claimedLog.rpyBodyAsXml;
            try
              Application.CreateForm(TShowXmlForm, ShowXmlForm);
              try
                ShowXmlForm.Caption := 'Reply as XML';
                ShowXmlForm.isCheckedOnly := True;
                ShowXmlForm.isReadOnly := True;
                ShowXmlForm.Bind := xXml;
                ShowXmlForm.ShowModal;
              finally
                FreeAndNil(ShowXmlForm);
              end;
            finally
              xXml.Free;
            end;
          end;
        ipmDTJson:
          ShowTextAsXml('Reply as XML', xString);
      end;
    end
    else
      ShowTextAsXml('Reply as XML', xString);
  end;
end;

procedure TMainForm.ReloadProject;
var
  xChanged, xRead: Boolean;
  f: Integer;
begin
  XmlUtil.PushCursor (crHourGlass);
  Application.ProcessMessages;
  try
    xChanged := stubChanged;
    xRead := se.stubRead;
    se.FocusOperationName := ifthen(Assigned (WsdlOperation), WsdlOperation.reqTagName);
    se.FocusMessageIndex := ifthen(Assigned (WsdlOperation), WsdlOperation.Messages.IndexOfObject(WsdlMessage));
    ProjectDesignFromString(se.ProjectDesignAsString(se.projectFileName), se.projectFileName);
    if allOperations.Find (se.FocusOperationName + ';' + se.FocusOperationNameSpace, f) then
    begin
      WsdlOperation := allOperations.Operations[f];
      if (se.FocusMessageIndex < WsdlOperation.Messages.Count) then
        WsdlMessage := WsdlOperation.Messages.Messages[se.FocusMessageIndex];
    end;
    stubChanged := xChanged;
    se.StubRead := xRead;
  finally
    XmlUtil.PopCursor;
    Application.ProcessMessages;
  end;
end;

function TMainForm.createListOfListsForTypeDefs (aTypeDefs: TXsdDataTypeList): TStringList ;
var
  x, f: Integer;
begin
  result := TStringList.Create;
  result.Sorted := True;
  for x := 0 to aTypeDefs.Count - 1 do with aTypeDefs.XsdDataTypes[x] do
  begin
    if NameSpace <> '' then
    begin
      if not result.Find(NameSpace, f) then
        f := result.AddObject (NameSpace, TStringList.Create);
      (result.Objects[f] as TStringlist).Add (Name);
    end;
  end;
end;

function TMainForm.createListOfListsForElements (aTypeDef: TXsdDataType): TStringList;
var
  x, f: Integer;
begin
  result := TStringList.Create;
  result.Sorted := True;
  for x := 0 to aTypeDef.ElementDefs.Count - 1 do with aTypeDef.ElementDefs.Xsds[x] do
  begin
    if ElementNameSpace <> '' then
    begin
      if not result.Find(ElementNameSpace, f) then
        f := result.AddObject (ElementNameSpace, TStringList.Create);
      (result.Objects[f] as TStringlist).Add (ElementName);
    end;
  end;
end;

function TMainForm.ActiveAfterPrompt : Boolean ;
begin
  result := False;
  if Assigned (se) then
  begin
    if not se.IsActive then
    begin
      if BooleanPromptDialog('wsdlStub not active' + LineEnding + 'Activate now') then
        startActionExecute (self);
    end;
    result := se.IsActive;
  end;
end;

function TMainForm.InactiveAfterPrompt : Boolean ;
begin
  result := True;
  if Assigned (se) then
  begin
    if se.IsActive then
    begin
      if BooleanPromptDialog('wsdlStub active' + LineEnding + 'Deactivate now') then
        stopActionExecute (self);
    end;
    result := not se.IsActive;
  end;
end;

procedure TMainForm.ShowHttpRequestAsXMLActionExecute(Sender: TObject);
var
  xXml: TXml;
  xString: String;
begin
  if Assigned (claimedLog)
  and Assigned (claimedLog.Operation)
  and claimedLog.Operation.isOpenApiService then
  begin
    Application.CreateForm(TShowXmlForm, ShowXmlForm);
    try
      ShowXmlForm.Caption := 'Request as XML';
      ShowXmlForm.isCheckedOnly := True;
      ShowXmlForm.isReadOnly := True;
      ShowXmlForm.Bind := claimedLog.reqBodyAsXml;
      try
        ShowXmlForm.ShowModal;
      finally
        ShowXmlForm.Bind.Free;
      end;
    finally
      FreeAndNil(ShowXmlForm);
    end;
    Exit;
  end;

  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.RequestBody, '');
  if (xString <> '') then
  begin
    if Assigned(claimedLog.Mssg) then
    begin
      case claimedLog.Operation.WsdlService.DescriptionType of
        ipmDTFreeFormat:
          ShowInfoForm('Request freeformat', xString);
        ipmDTCobol, ipmDTBmtp:
          begin
            if se.ShowLogCobolStyle = slCobol then
            begin (claimedLog.Operation.reqBind as TIpmItem)
              .BufferToValues(FoundErrorInBuffer, xString);
              ShowIpm('Cobol view on Request',
                claimedLog.Operation.reqBind as TIpmItem);
            end
            else
            begin
              with claimedLog.reqBodyAsXml do
                try
                  ShowTextAsXml('Request as XML',
                    AsText(False, 0, False, False));
                finally
                  Free;
                end;
            end;
          end;
        ipmDTXml:
          ShowTextAsXml('Request as XML', xString);
        ipmDTXsd:
          ShowTextAsXml('Request as XML', xString);
        ipmDTWsdl:
          ShowTextAsXml('Request as XML', xString);
        ipmDTEmail:
          ShowInfoForm('Request freeformat', xString);
        ipmDTSwiftMT:
          begin
            xXml := claimedLog.reqBodyAsXml;
            try
              Application.CreateForm(TShowXmlForm, ShowXmlForm);
              try
                ShowXmlForm.Caption := 'Request as XML';
                ShowXmlForm.isCheckedOnly := True;
                ShowXmlForm.isReadOnly := True;
                ShowXmlForm.Bind := xXml;
                ShowXmlForm.ShowModal;
              finally
                FreeAndNil(ShowXmlForm);
              end;
            finally
              xXml.Free;
            end;
          end;
        ipmDTJson:
          ShowTextAsXml('Request as XML', xString);
      end;
    end
    else
      ShowTextAsXml('Request as XML', xString);
  end;
end;

procedure TMainForm.ShowHttpRequestAsXMLActionUpdate(Sender: TObject);
begin
  ShowHttpRequestAsXMLAction.Enabled := (LogMemo.Text <> '');
end;

procedure TMainForm.MessagesVTSPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xLog: TLog;
begin
  xLog := NodeToMsgLog(False,Sender, Node);
  if Assigned(xLog) and (xLog is TLog) then
  begin
    if xLog.ShowHighLighted then
    begin
      if not(Node = Sender.FocusedNode) then
        TargetCanvas.Font.Color := clBlue;
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    end;
  end;
end;

procedure TMainForm.SchemapropertiesMenuItemClick(Sender: TObject);
begin
  SchemapropertiesMenuItem.Checked := not SchemapropertiesMenuItem.Checked;
  XsdPanel.Visible := SchemapropertiesMenuItem.Checked;
  xsdSplitter.Visible := SchemapropertiesMenuItem.Checked;
end;

procedure TMainForm.ListofOperationsMenuItemClick(Sender: TObject);
begin
  ListofOperationsMenuItem.Checked := not ListofOperationsMenuItem.Checked;
  ScriptSplitter.Visible := ListofOperationsMenuItem.Checked;
  ScriptPanel.Visible := ListofOperationsMenuItem.Checked;
end;

procedure TMainForm.SaveMessagesActionUpdate(Sender: TObject);
begin
  SaveMessagesAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.WsdlInformationMenuItemClick(Sender: TObject);
begin
  WsdlInformationMenuItem.Checked := not WsdlInformationMenuItem.Checked;
  WsdlInfoPanel.Visible := WsdlInformationMenuItem.Checked;
end;

procedure TMainForm.ReadMessagesActionUpdate(Sender: TObject);
begin
  ReadMessagesAction.Enabled := (se.Wsdls.Count > 0);
end;

procedure TMainForm.ReadMessagesActionExecute(Sender: TObject);
var
  xLogList: TLogList;
  X: Integer;
  xOpenOptions: TOpenOptions;
begin
  if not LogMaxEntriesEqualsUnbounded (ReadMessagesAction.Caption) then Exit;
  with OpenFileDialog do
  begin
    xOpenOptions := Options;
    try
      DefaultExt := 'xml';
      FileName := wsdlStubMessagesFileName;
      Filter := 'XML file (*.xml)|*.xml';
      Title := 'Read ' + _progName + ' messages from files';
      Options := Options + [ofAllowMultiSelect];
      if Execute then
      begin
        wsdlStubMessagesFileName := FileName;
        xLogList := TLogList.Create;
        try
          for X := 0 to Files.Count - 1 do
            try
              se.OpenMessagesLog(Files.Strings[X], True, True, xLogList);
            except
              on E: Exception do
                raise Exception.CreateFmt('Error opening file %s%s%s',
                  [Files.Strings[X], LineEnding, E.Message]);
            end;
          if xLogList.designSuspect then
          begin
            if not BooleanPromptDialog(
              'Maybe due to differences in current design or Wsdls,' + LineEnding +
                'some Operations or Messages could not be relocated;'
                + LineEnding + LineEnding + 'Continue') then
            begin
              xLogList.Clear;
              raise Exception.Create('Operation aborted');
            end;
          end;
          for X := 0 to xLogList.Count - 1 do
            se.CheckExpectedValues(xLogList.LogItems[X], xLogList.LogItems[X].Operation, doCheckExpectedValues);
          ToAllLogList(xLogList);
        finally
          xLogList.Clear;
          FreeAndNil(xLogList);
        end;
      end;
    finally
      Options := xOpenOptions;
    end;
  end;
end;

procedure TMainForm.SaveLogRepliesToFileActionExecute(Sender: TObject);
begin
  SaveFileDialog.DefaultExt := 'txt';
  SaveFileDialog.FileName := saveToDiskFileName;
  SaveFileDialog.Filter := 'Text file (*.txt)|*.txt';
  SaveFileDialog.Title := 'Save ' + _progName + ' replies from log';
  if SaveFileDialog.Execute then
  begin
    saveToDiskFileName := SaveFileDialog.FileName;
    doSaveLogRepliesToDisk;
  end;
end;

procedure TMainForm.SaveLogRepliesToFileActionUpdate(Sender: TObject);
begin
  SaveLogRepliesToFileAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.SaveLogRequestsToFileActionExecute(Sender: TObject);
begin
  SaveFileDialog.DefaultExt := 'txt';
  SaveFileDialog.FileName := saveToDiskFileName;
  SaveFileDialog.Filter := 'Text file (*.txt)|*.txt';
  SaveFileDialog.Title := 'Save ' + _progName + ' requests from log';
  if SaveFileDialog.Execute then
  begin
    saveToDiskFileName := SaveFileDialog.FileName;
    doSaveLogRequestsToDisk;
  end;
end;

procedure TMainForm.SaveLogRequestsToFileActionUpdate(Sender: TObject);
begin
  SaveLogRequestsToFileAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.SaveMessagesActionExecute(Sender: TObject);
begin
  EndEdit;
  SaveFileDialog.DefaultExt := 'xml';
  SaveFileDialog.FileName := wsdlStubMessagesFileName;
  SaveFileDialog.Filter := 'XML file (*.xml)|*.xml';
  SaveFileDialog.Title := 'Save ' + _progName + ' messages';
  if SaveFileDialog.Execute then
  begin
    wsdlStubMessagesFileName := SaveFileDialog.FileName;
    se.SaveLogs(SaveFileDialog.FileName);
  end;
end;

procedure TMainForm.ReadMessagesActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (se.Wsdls.Count = 0) then
    HintStr := HintStr + ' (no WSDLS read)';
end;

procedure TMainForm.TestBeforeScriptActionExecute(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(se) then
    exit;
  if not ActiveAfterPrompt then
    exit;
  if Assigned(WsdlOperation) then
  begin
    xOperation := TWsdlOperation.Create(WsdlOperation);
    try
      xOperation.CorrelatedMessage := WsdlMessage;
      with xOperation.reqBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlMessage.reqBind as TXml), False, True, True, True);
      end;
      with xOperation.rpyBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlMessage.rpyBind as TXml), False, True, True, True);
      end;
      xOperation.ExecuteBefore;
      if xOperation.StubAction = saRequest then
        wsdlz.PromptRequest(xOperation)
      else
        wsdlz.PromptReply(xOperation);
    finally
      xOperation.Free;
    end;
  end;
end;

procedure TMainForm.ThrowExceptionActionExecute(Sender: TObject);
begin
  raise Exception.Create('janBo was here');
end;

procedure TMainForm.ToAllLogList(aLogList: TLogList);
var
  X: Integer;
begin
  try
    XmlUtil.PushCursor (crHourGlass);
{
    ClearLogItemsActionExecute(nil);
    if not se.displayedLogs.Count = 0 then
      raise Exception.Create('Operation aborted');
}
    MessagesVTS.BeginUpdate;
    MessagesVTS.Header.SortColumn := -1;
    MessagesVTS.Header.SortDirection := sdAscending;
    for X := 0 to aLogList.Count - 1 do
    begin
      se.DisplayLog('', aLogList.LogItems[X]);
    end;
  finally
    MessagesVTS.EndUpdate;
    XmlUtil.PopCursor;
    LogTabControl.TabIndex := Ord (spMessages);
  end;
end;

procedure TMainForm.MessagesRegressionActionUpdate(Sender: TObject);
begin
  MessagesRegressionAction.Enabled := { }{ not se.IsActive
    and { } (se.Wsdls.Count > 0);
end;

procedure TMainForm.MessagesFromDiskActionExecute(Sender: TObject);
  function _filenames(aSl: TStrings): String;
  var
    X: Integer;
  begin
    result := '';
    for X := 0 to aSl.Count - 1 do
      result := result + '"' + aSl.Strings[X] + '" ';
  end;

begin
  if WsdlOperation.CorrelationBindables.Count = 0 then
  begin
    ShowMessage ( 'Function requires correlation data'
                + LineEnding
                + 'Please provide correlation data first'
                );
    Exit;
  end;
  if not InactiveAfterPrompt then Exit;
  with TOpenDialog.Create(nil) do
    try
      DefaultExt := 'xml';
      FileName := '';
      Filter := 'XML file (*.xml)|*.xml';
      Title := 'Read messages from disk';
      Options := Options + [ofAllowMultiSelect, ofFileMustExist];
      if Execute then
      begin
        Application.CreateForm(TmessagesFromDiskForm, messagesFromDiskForm);
        try
          messagesFromDiskForm.FileNamesEdit.Text := _filenames(Files);
          messagesFromDiskForm.ShowModal;
          if messagesFromDiskForm.ModalResult = mrOk then
          begin
            saveToDiskSeparator := messagesFromDiskForm.SeparatorEdit.Text;
            FileNameList.Text := Files.Text;
            TProcedureThread.Create(False, True, se, doReadMessagesFromDisk);
          end;
        finally
          FreeAndNil(messagesFromDiskForm);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.MessagesFromDiskActionUpdate(Sender: TObject);
begin
  MessagesFromDiskAction.Enabled := Assigned (WsdlOperation);
end;

procedure TMainForm.MessagesRegressionActionExecute(Sender: TObject);
var
  xLogList: TLogList;
begin
  OnlyWhenLicensed;
  if not LogMaxEntriesEqualsUnbounded (MessagesRegressionAction.Caption) then Exit;
  OpenFileDialog.DefaultExt := 'xml';
  OpenFileDialog.FileName := wsdlStubMessagesFileName;
  OpenFileDialog.Filter := 'XML file (*.xml)|*.xml';
  OpenFileDialog.Title := 'Compare ' + _progName + ' log items from file';
  if OpenFileDialog.Execute then
  begin
    XmlUtil.PushCursor(crHourGlass);
    wsdlStubMessagesFileName := OpenFileDialog.FileName;
    xLogList := TLogList.Create;
    try
      try
        se.OpenMessagesLog(OpenFileDialog.FileName, True, True, xLogList);
      except
        on E: Exception do
        begin
          raise Exception.Create(
            'Can not compare with log file because next exception was raised' +
              LineEnding + E.Message);
        end;
      end;
{
      if xLogList.designSuspect then
      begin
        if not BooleanPromptDialog(
          'Maybe due to differences in design or Wsdls,' + LineEnding +
            'some Operations or Messages could not be relocated;' + LineEnding +
            LineEnding + 'Continue') then
        begin
          raise Exception.Create('Operation aborted');
        end;
      end;
}
      ShowLogDifferences(se.displayedLogs, xLogList, 'Current', wsdlStubMessagesFileName);
    finally
      xLogList.Clear;
      FreeAndNil(xLogList);
      XmlUtil.PopCursor;
    end;
  end;
end;

function TMainForm.ShowLogDifferences(aLogs, bLogs: TLogList; aAName, aBName: String): TSnapshotStatus;
var
  X: Integer;
  xForm: TShowLogDifferencesForm;
begin
  result := rsUndefined;
  Application.CreateForm(TShowLogDifferencesForm, xForm);
  try
    xForm.ProgName := _ProgName;
    xForm.StyleSheet := _wsdlStubStylesheet;
    xForm.aLogs := TLogList.Create;
    xForm.bLogs := TLogList.Create;
    xForm.ReferenceFileName := aBName;
    xForm.compareLogOrderBy := se.CompareLogOrderBy;
    try
      for X := 0 to aLogs.Count - 1 do
        if aLogs.LogItems[X].PassesFilter then
          xForm.aLogs.AddObject ( '', aLogs.LogItems[X]);
      for X := 0 to bLogs.Count - 1 do
        if bLogs.LogItems[X].PassesFilter then
          xForm.bLogs.AddObject ( '', bLogs.LogItems[X]);
      xForm.ignoreDifferencesOn.Text := se.ignoreDifferencesOn.Text;
      xForm.ignoreAddingon.Text := se.ignoreAddingOn.Text;
      xForm.ignoreRemovingOn.Text := se.ignoreRemovingOn.Text;
      for x := 0 to xForm.ignoreOrderOn.Count - 1 do
        xForm.ignoreOrderOn.Objects[x].Free;
      xForm.ignoreOrderOn.Text := se.ignoreOrderOn.Text;
      for x := 0 to xForm.ignoreOrderOn.Count - 1 do
      begin
        xForm.ignoreOrderOn.Objects[x] := TStringList.Create;
        (xForm.ignoreOrderOn.Objects[x] as TStringList).Text :=
          (se.ignoreOrderOn.Objects[x] as TStringList).Text;
      end;
      xForm.regressionSortColumns.Text := se.regressionSortColumns.Text;
      xForm.ShowModal;
      if xForm.configChanged then
      begin
        if BooleanPromptDialog('Accept changes to Regression report settings') then
        begin
          se.CompareLogOrderBy := xForm.compareLogOrderBy;
          se.ignoreDifferencesOn.Text := xForm.ignoreDifferencesOn.Text;
          se.ignoreAddingOn.Text := xForm.ignoreAddingon.Text;
          se.ignoreRemovingOn.Text := xForm.ignoreRemovingOn.Text;
          for x := 0 to se.ignoreOrderOn.Count - 1 do
            se.ignoreOrderOn.Objects[x].Free;
          se.ignoreOrderOn.Text := xForm.ignoreOrderOn.Text;
          for x := 0 to se.ignoreOrderOn.Count - 1 do
          begin
            se.ignoreOrderOn.Objects[x] := TStringList.Create;
            (se.ignoreOrderOn.Objects[x] as TStringList).Text
              := (xForm.ignoreOrderOn.Objects[x] as TStringList).Text;
          end;
          se.regressionSortColumns.Text := xForm.regressionSortColumns.Text;
          stubChanged := True;
        end;
      end;
      if xForm.differencesFound then
        result := rsNok
      else
        Result := rsOk;
    finally
      xForm.aLogs.Free;
      xForm.bLogs.Free;
    end;
  finally
    UpdateCaption;
    FreeAndNil(xForm);
  end;
end;

procedure TMainForm.setElementsWhenRepeatable(const Value: Integer);
begin
  fElementsWhenRepeatable := Value;
end;

procedure TMainForm.setDoShowDesignAtTop(const Value: Boolean);
begin
  fDoShowDesignAtTop := Value;
  if doShowDesignAtTop then
  begin
    LogPanel.Align := alBottom;
    Splitter1.Align := alBottom;
  end
  else
  begin
    LogPanel.Align := alTop;
    Splitter1.Align := alTop;
  end;
end;

procedure TMainForm.CheckGridFieldsActionUpdate(Sender: TObject);
begin
  CheckGridFieldsAction.Enabled := Assigned(WsdlOperation)
                               and (WsdlOperation.Messages.Count > 0)
                               and (WsdlOperation.Messages.Messages[0].ColumnXmls.Count > 0)
                                 ;
end;

procedure TMainForm.CheckGridFieldsActionExecute(Sender: TObject);
var
  X, Y, r: Integer;
  xString: String;
  xNode: PVirtualNode;
begin
  xString := ''; //avoid warning
  try
    XmlUtil.PushCursor (crHourGlass);
    with WsdlOperation.Messages do
    begin
      for X := 0 to Count - 1 do
      begin
        for Y := 0 to Messages[X].ColumnXmls.Count - 1 do
        begin
          if Assigned(Messages[X].ColumnXmls.Bindables[Y])
            and Messages[X].ColumnXmls.Bindables[Y].Checked then
          begin
            if (((Messages[X].ColumnXmls.Bindables[Y]) is TXml) and
                (not((Messages[X].ColumnXmls.Bindables[Y]) as TXml)
                  .IsValueValidAgainstXsd(xString))) or
              (((Messages[X].ColumnXmls.Bindables[Y]) is TIpmItem) and
                (not((Messages[X].ColumnXmls.Bindables[Y]) as TIpmItem)
                  .IsValueValid(xString))) then
            begin
              xNode := GridView.GetFirst;
              r := X;
              while r > 0 do
              begin
                xNode := GridView.GetNext(xNode);
                Dec(r);
              end;
              GridView.Selected[xNode] := True;
              GridViewFocusedNode(xNode);
              GridView.FocusedColumn :=
                Y + WsdlOperation.CorrelationBindables.Count + 1;
              GridView.Update;
              GridView.SetFocus;
              raise Exception.Create(xString);
            end;
          end;
        end;
      end;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.DesignPanelAtTopMenuItemClick(Sender: TObject);
begin
  DesignPanelAtTopMenuItem.Checked := not DesignPanelAtTopMenuItem.Checked;
  doShowDesignAtTop := DesignPanelAtTopMenuItem.Checked;
end;

procedure TMainForm.InWsdlTreeViewChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  { }{
    Allowed := not se.IsActive;
    { }
end;

procedure TMainForm.EditAfterScriptMenuItemClick(Sender: TObject);
begin
  AfterRequestScriptButtonClick(nil);
end;

procedure TMainForm.EditBeforeScriptMenuItemClick(Sender: TObject);
begin
  EditScriptButtonClick(nil);
end;

procedure TMainForm.EditEnvironmentActionExecute(Sender: TObject);
begin
  Application.CreateForm(TEditListValuesForm, EditListValuesForm);
  try
    if se.IsActive and False then
      EditListValuesForm.Caption := 'View environment variables'
    else
      EditListValuesForm.Caption := 'Edit environment variables';
{
Environment Variables

These variables can be manipulated from script with:
- DecEnvNumber (aKey)
- IncEnvNumber (aKey)
- ResetEnvVar (aKey)
- ResetEnvVars (aRegularExpr)
- SetEnvNumber (aKey, aNumber)
- SetEnvVar (aKey, aValue)
and be read with:
- GetEnvNumber (aKey)
- GetEnvNumberDef (aKey, aDefault)
- GetEnvVar (aKey)
- GetEnvVarDef (aKey, aDefault)
- for each MatchingEnvVar (aRegExpr) as [stringvar] do ...
}
    EditListValuesForm.isReadOnly := se.IsActive and False;
    EnvVarLock.Acquire;
    try
      EditListValuesForm.ValueListEditor.Strings.Text := se.EnvVars.Text;
    finally
      EnvVarLock.Release;
    end;
    EditListValuesForm.ValueListEditor.Strings.Sort;
    EditListValuesForm.ShowModal;
    if (EditListValuesForm.ModalResult = mrOk)
    { }{ and (not se.IsActive){ } then
    begin
      EnvVarLock.Acquire;;
      try
        se.EnvVars.Text := EditListValuesForm.ValueListEditor.Strings.Text;
      finally
        EnvVarLock.Release;
      end;
    end;
  finally
    FreeAndNil(EditListValuesForm);
  end;
end;

procedure TMainForm.AddEnvironmentActionUpdate(Sender: TObject);
begin
  AddEnvironmentAction.Enabled := (se.EnvVars.Count > 0);
end;

procedure TMainForm.EditEnvironmentActionUpdate(Sender: TObject);
begin
  if se.IsActive and False then
    EditEnvironmentAction.Caption := 'Show ...'
  else
    EditEnvironmentAction.Caption := 'Edit ...';
end;

procedure TMainForm.RemoveEnvironmentActionUpdate(Sender: TObject);
begin
  RemoveEnvironmentAction.Enabled := True;
end;

procedure TMainForm.AddEnvironmentActionExecute(Sender: TObject);
var
  f, X: Integer;
  xXml: TXml;
begin
  Application.CreateForm(TAddFavouritesForm, AddFavouritesForm);
  try
    AddFavouritesForm.ShowModal;
    if AddFavouritesForm.ModalResult = mrOk then
    begin
      EnvVarLock.Acquire;
      try
        if se.EnvironmentList.Find(AddFavouritesForm.FavouriteName, f) then
        begin
          if not BooleanPromptDialog
            ('Environment (' + AddFavouritesForm.FavouriteName +
              ') already exist, Overwrite') then
            exit;
          se.EnvironmentList.Delete(f);
        end;
        xXml := TXml.CreateAsString('Vars', '');
        with xXml do
        begin
          for X := 0 to se.EnvVars.Count - 1 do
          begin
            with AddXml(TXml.CreateAsString('Var', '')) do
            begin
              AddXml(TXml.CreateAsString('Key', se.EnvVars.Names[X]));
              AddXml(TXml.CreateAsString('Value', se.EnvVars.ValueFromIndex[X]));
            end;
          end;
        end;
        se.EnvironmentList.AddObject(AddFavouritesForm.FavouriteName, xXml);
        stubChanged := True;
        {
          IniFile.WriteString ( 'Environment'
          , AddFavouritesForm.FavouriteName
          , xXml.Text
          );
          }
        CreateEnvironmentSubMenuItems;
      finally
        EnvVarLock.Release;
      end;
    end;
  finally
    FreeAndNil(AddFavouritesForm);
  end;
end;

procedure TMainForm.CreateEnvironmentSubMenuItems;
var
  xMenuItem: TMenuItem;
  X: Integer;
begin
  while EnvironmentMenuItem.Count > 7 do
    EnvironmentMenuItem.Delete(7);
  { in case you want to use this code: ADJUST to current menu design
    EnvironmentMenuItem.Clear;
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Action := EditEnvironmentAction;
    EnvironmentMenuItem.Add(xMenuItem);
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Action := AddEnvironmentAction;
    EnvironmentMenuItem.Add(xMenuItem);
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Action := RemoveEnvironmentAction;
    EnvironmentMenuItem.Add(xMenuItem);
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Caption := '-';
    EnvironmentMenuItem.Add(xMenuItem);
  }
  EnvVarLock.Acquire;
  try
    for X := 0 to se.EnvironmentList.Count - 1 do
    begin
      xMenuItem := TMenuItem.Create(Self);
      xMenuItem.Caption := se.EnvironmentList.Strings[X];
      xMenuItem.OnClick := SetEnvironmentClick;
      xMenuItem.Tag := X;
      EnvironmentMenuItem.Add(xMenuItem);
    end;
  finally
    EnvVarLock.Release;
  end;
  EnvironmentMenuItem.Enabled := True;
end;

procedure TMainForm.ScriptGoMenuItemClick(Sender: TObject);
var
  xScript: TXml;
  x: Integer;
begin
  if not Assigned(se) then
    exit;
  if not ActiveAfterPrompt then
    exit;
  se.ProgressMax := 5;
  se.ProgressPos := 0;
  xScript := se.Scripts.Items.XmlItems[(Sender as TMenuItem).Tag];
  TProcedureThread.Create(False, False, se, se.ScriptExecute, xScript as TObject);
end;

procedure TMainForm.CreateScriptsSubMenuItems;
var
  xMenuItem: TMenuItem;
  X: Integer;
begin
{}{
  while MainToolBar.ButtonCount > MainToolBarDesignedButtonCount do
    MainToolBar.Buttons[MainToolBar.ButtonCount].Free;
{}
  while ScriptsMenuItem.Count > 2 do
    ScriptsMenuItem.Delete(2);
  for X := 0 to se.Scripts.Items.Count - 1 do
  begin
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Action := runScriptAction; // only for enabling
    xMenuItem.Caption := se.Scripts.Items.XmlItems[X].Items.XmlValueByTag['Name'];
    xMenuItem.OnClick := ScriptGoMenuItemClick;
    xMenuItem.Tag := X;
    xMenuItem.Enabled := True;
    ScriptsMenuItem.Add(xMenuItem);
{}{
    xButton := TToolButton.Create(self);
    xButton.Parent := MainToolBar;
    xButton.OnClick := ScriptGoMenuItemClick;
    xButton.Tag := X;
    xButton.Action := runScriptAction; // only for enabling
    xButton.Caption := se.Scripts.Strings[X];
    xButton.Hint := se.Scripts.Strings[X];
    xButton.Left := MainToolBar.Width - xButton.Width;
    Inc (xLeft, xButton.Width);
{}
  end;
  ScriptsMenuItem.Enabled := True;
end;

procedure TMainForm.SetEnvironmentClick(Sender: TObject);
var
  X: Integer;
  xXml: TXml;
begin
  EnvVarLock.Acquire;
  try
    X := (Sender as TMenuItem).Tag;
    xXml := se.EnvironmentList.Objects[X] as TXml;
    se.EnvVars.Clear;
    for X := 0 to xXml.Items.Count - 1 do
      with xXml.Items.XmlItems[X].Items do
        se.EnvVars.Values[XmlValueByTag['Key']] := XmlValueByTag['Value'];
  finally
    EnvVarLock.Release;
  end;
end;

procedure TMainForm.RemoveEnvironmentAction1Click(Sender: TObject);
var
  f: Integer;
begin
  Application.CreateForm(TChooseStringForm, ChooseStringForm);
  try
    ChooseStringForm.ListBox.Clear;
    ChooseStringForm.ListBox.Items.Text := se.EnvironmentList.Text;
    ChooseStringForm.Caption := 'Remove environment';
    ChooseStringForm.ShowModal;
    if ChooseStringForm.ModalResult = mrOk then
    begin
      EnvVarLock.Acquire;
      try
        se.EnvironmentList.Find(ChooseStringForm.ChoosenString, f);
        (se.EnvironmentList.Objects[f] as TXml).Free;
        se.EnvironmentList.Delete(f);
        with TFormIniFile.Create(self, False) do
        try
          DeleteKey('Environment' + ChooseStringForm.ChoosenString);
        finally
          Free;
        end;
        CreateEnvironmentSubMenuItems;
        stubChanged := True;
      finally
        EnvVarLock.Release;
      end;
    end;
  finally
    FreeAndNil(ChooseStringForm);
  end;
end;

function TMainForm.getIsRequestAction: Boolean;
begin
  if Assigned(WsdlOperation) then
    result := (WsdlOperation.StubAction = saRequest)
  else
    result := False;
end;

function TMainForm.doDecryptString(aString: AnsiString): AnsiString;
begin
  result := FormIniFilez.DecryptPassword(aString);
end;

function TMainForm.doEncryptString(aString: AnsiString): AnsiString;
begin
  result := FormIniFilez.EncryptPassword(aString);
end;

procedure TMainForm.doExecuteRequest;
var
  xOperation: TWsdlOperation;
begin
  if not Assigned (WsdlOperation) then
    raise Exception.Create ('TMainForm.doExecuteRequest: wsdlOp ...');
  WsdlOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
  finally
    WsdlOperation.ReleaseLock;
  end;
  try
    xOperation.CorrelatedMessage := WsdlMessage;
    se.SendMessage(xOperation, WsdlMessage, '');
  finally
    xOperation.Free;
  end;
end;

procedure TMainForm.ExecuteRequestActionExecute(Sender: TObject);
begin
  if not ActiveAfterPrompt then exit;
  LogTabControl.TabIndex := Ord (spMessages);
  TProcedureThread.Create(False, True, se, doExecuteRequest);
end;

procedure TMainForm.ExecuteAllRequests;
var
  X: Integer;
  xOperation: TWsdlOperation;
begin
  se.AcquireLogLock;
  se.ProgressMax := WsdlOperation.Messages.Count;
  se.ReleaseLogLock;
  for X := 0 to WsdlOperation.Messages.Count - 1 do
  begin
    if abortPressed then
      Break;
    WsdlOperation.AcquireLock;
    try
      xOperation := TWsdlOperation.Create(WsdlOperation); // fresh copy
    finally
      WsdlOperation.ReleaseLock;
    end;
    try
      if not WsdlOperation.Messages.Messages[X].Disabled then
      begin
        se.AcquireLogLock;
        se.ProgressPos := X + 1;
        se.ReleaseLogLock;
        try
          se.SendMessage(xOperation, WsdlOperation.Messages.Messages[X], '');
        except
        end;
      end;
    finally
      FreeAndNil(xOperation);
    end;
  end;
end;

procedure TMainForm.ExecuteLoadTest;
var
  X, y: Integer;
  xOperation: TWsdlOperation;
  doSleep: Boolean;
begin
  doSleep := False;
  WsdlOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
  finally
    WsdlOperation.ReleaseLock;
  end;
  try
    for y := 0 to StressTestLoopsPerThread - 1 do
    begin
      for X := 0 to xOperation.Messages.Count - 1 do
      begin
        if abortPressed then Exit;
        if not xOperation.Messages.Messages[X].Disabled then
        begin
          if doSleep then
          begin
            if (StressTestDelayMsMin > 0)
            or (StressTestDelayMsMax > 0) then
            begin
              Sleep (StressTestDelayMsMin + Random (StressTestDelayMsMax - StressTestDelayMsMin));
              if abortPressed then Exit;
            end;
          end;
          doSleep := True;
          try
            se.SendMessage(xOperation, xOperation.Messages.Messages[X], '');
          except
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(xOperation);
  end;
end;

procedure TMainForm.ExecuteAllRequestsActionExecute(Sender: TObject);
begin
  if not ActiveAfterPrompt then exit;
  LogTabControl.TabIndex := Ord (spMessages);
  TProcedureThread.Create(False, True, se, ExecuteAllRequests);
end;

procedure TMainForm.WsdlItemChangeDataTypeMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
  xTypeDef: TXsdDataType;
  x, f: Integer;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if not Assigned (xBind)
  or not (xBind is TXml)
  or not Assigned ((xBind as TXml).Xsd) then
    Exit;
  Application.CreateForm(TChooseStringForm, ChooseStringForm);
  try
    ChooseStringForm.ListBox.Clear;
    ChooseStringForm.ListBox.Sorted := False;
    ChooseStringForm.ListBox.Items.Text := Wsdl.XsdDescr.TypeDefs.Text;
    for X := 0 to Wsdl.XsdDescr.TypeDefs.Count - 1 do
      if Wsdl.XsdDescr.TypeDefs.Strings[X]
        <> ChooseStringForm.ListBox.Items.Strings[X] then
        ShowMessage(IntToStr(X) + '' + Wsdl.XsdDescr.TypeDefs.Strings[X]
            + ' ' + ChooseStringForm.ListBox.Items.Strings[X]);

    ChooseStringForm.Caption := 'Choose from Types';
    ChooseStringForm.ShowModal;
    if ChooseStringForm.ModalResult = mrOk then with ((xBind as TXml).Xsd) do
    begin
      f := ChooseStringForm.ListBox.ItemIndex;
      xTypeDef := Wsdl.XsdDescr.TypeDefs.XsdDataTypes[f];
      if xTypeDef <> sType then
      begin
        stubChanged := True;
        with (Wsdl.XsdDescr.ChangedElementDefs as TXml) do
        begin
          with AddXml(TXml.CreateAsString('ChangedElementTypedef', '')) do
          begin
            AddXml (TXml.CreateAsString('NameSpace', ElementNameSpace));
            AddXml (TXml.CreateAsString('Name', ElementName));
            with AddXml (TXml.CreateAsString('TypeDef', '')) do
            begin
              AddXml (TXml.CreateAsString('NameSpace', xTypeDef.NameSpace));
              AddXml (TXml.CreateAsString('Name', xTypeDef.Name));
            end;
          end;
        end;
        ReloadProject;
      end;
    end;
  finally
    ChooseStringForm.Free;
  end;
end;

procedure TMainForm.MessagesVTSGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  if Column < Ord(logStdColumnCount) then
  begin
    case TLogColumnEnum(Column) of
      logExpectedColumn:
        HintText := 'Expected Values';
      logRemarksColumn:
        HintText := 'Remarks';
      logRequestTreeColumn:
        HintText := 'Request Tree';
      logReplyTreeColumn:
        HintText := 'Reply Tree';
      logRequestGridColumn:
        HintText := 'Browse request';
      logReplyGridColumn:
        HintText := 'Browse reply';
    end;
  end;
end;

procedure TMainForm.MessagesVTSGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xLog: TLog;
begin
  try
    case TLogColumnEnum(Column) of
      logExpectedColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if Assigned(xLog) and xLog.ExpectedValuesChecked then
            if xLog.HasUnexpectedValue then
              ImageIndex := 48
            else
              ImageIndex := 47;
        end;
      logRemarksColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if Assigned(xLog) and (xLog.Remarks <> '') then
            ImageIndex := 65
          else
            ImageIndex := -1;
        end;
      logExceptionColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if Assigned(xLog) and (xLog.Exception <> '') then
            ImageIndex := 84
          else
            ImageIndex := -1;
        end;
      logRequestTreeColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.RequestBody <> '') then
            if (not xLog.RequestValidated) then
              ImageIndex := 40
            else if xLog.RequestValidateResult = '' then
              ImageIndex := 39
            else
              ImageIndex := 25;
        end;
      logReplyTreeColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.ReplyBody <> '') then
            if (not xLog.ReplyValidated) then
              ImageIndex := 40
            else if xLog.ReplyValidateResult = '' then
              ImageIndex := 39
            else
              ImageIndex := 25;
        end;
      logRequestGridColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.RequestBody <> '') then
            ImageIndex := 76;
        end;
      logReplyGridColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.ReplyBody <> '') then
            ImageIndex := 76;
        end;
    end;
  except
  end;
end;

procedure TMainForm.MessagesVTSClick(Sender: TObject);
begin
  if not Assigned (MessagesVTS.FocusedNode) then Exit;
  claimedLog := NodeToMsgLog(True,MessagesVTS, MessagesVTS.FocusedNode);
  if Assigned (claimedLog) then
  try
    case TLogColumnEnum((Sender as TVirtualStringTree).FocusedColumn) of
      logExpectedColumn: ShowExpectedXmlActionExecute(nil);
      logRemarksColumn: ShowRemarksActionExecute(nil);
      logExceptionColumn: xmlUtil.presentString('Exception', claimedLog.Exception);
      logRequestTreeColumn: ShowHttpRequestAsXMLActionExecute(nil);
      logReplyTreeColumn: ShowHttpReplyAsXMLActionExecute(nil);
      logRequestGridColumn: ShowRequestAsXmlGridActionExecute(nil);
      logReplyGridColumn: ShowReplyAsXmlGridActionExecute(nil);
    end;
    if (Sender as TVirtualStringTree).FocusedColumn >= Ord(logStdColumnCount) then
      DisplayedcolumnMenuItemClick(Sender);
  finally
    claimedLog.Disclaim;
  end;
end;

procedure TMainForm.WsdlPopupMenuPopup(Sender: TObject);
  procedure _createTypeSubMenuItems(aRootMenuItem: TMenuItem;
    aCurrent, aBase: TXsdDataType);
  var
    xMenuItem: TMenuItem;
    X: Integer;
  begin
    if (aBase <> aCurrent) and (not aBase.isAbstract) then
    begin
      xMenuItem := TMenuItem.Create(Self);
      xMenuItem.Caption := aBase.Name + ' (' + aBase.NameSpace + ')';
      xMenuItem.OnClick := ChangeDataTypeMenuItemClick;
      xMenuItem.Tag := Integer(aBase);
      aRootMenuItem.Add(xMenuItem);
    end;
    for X := 0 to aBase.ExtendedByList.Count - 1 do
      _createTypeSubMenuItems(aRootMenuItem, aCurrent,
        aBase.ExtendedByList.XsdDataTypes[X]);
  end;

var
  xBind: TCustomBindable;
  xEnableAddMenuItems: Boolean;
  xEnableDelMenuItems: Boolean;
  xEnableCheck: Boolean;
  xEnableStamp: Boolean;
  xExtRecurVisible: Boolean;
  xAddChildVisible: Boolean;
  xRootBase: TXsdDataType;
begin
  EndEdit;
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  xEnableAddMenuItems := False;
  xEnableDelMenuItems := False;
  xAddChildVisible := False;
  xExtRecurVisible := False;
  xEnableCheck := False;
  xEnableStamp := False;
  if xBind is TXml then
    with xBind as TXml do
    begin
      xEnableAddMenuItems := Assigned(Xsd) and (Xsd.maxOccurs <> '1');
      xEnableDelMenuItems := Assigned(Xsd) and
        ((xBind as TXml).Xsd.maxOccurs <> '1') and
        ((xBind as TXml).IndexOfRepeatableItem >= xsdElementsWhenRepeatable);
      if Assigned(TypeDef) then
      begin
        if (TypeDef.IsComplex and (TypeDef.ElementDefs.Count = 0))
        or ((TypeDef.IsBuiltIn) and (TypeDef.BaseDataTypeName = 'anyType'))
        or (TypeDef.Manually) then
          xAddChildVisible := True;
        if (Items.Count = 0) and (TypeDef.ElementDefs.Count > 0) then
          xExtRecurVisible := True;
      end;
      xAddChildVisible := xAddChildVisible { }{ and BetaMode{ } ;
      xEnableCheck := Items.Count > 0;
      xEnableStamp := Items.Count = 0;
      CopySwiftdatatoclipboardMenuItem.Visible :=
        WsdlOperation.WsdlService.DescriptionType in [ipmDTSwiftMT];
      CopySwiftdatatoclipboardMenuItem.Enabled := Assigned(Xsd) and
        (Xsd.Obj is TSwiftMtProps or (Name = 'FinMessage') or
          (Assigned(Parent) and (Parent.Name = 'FinMessage')));
      PasteSwiftdatafromclipboardMenuItem.Visible :=
        WsdlOperation.WsdlService.DescriptionType in [ipmDTSwiftMT];
      PasteSwiftdatafromclipboardMenuItem.Enabled := Assigned(Xsd) and
        ((Name = 'FinMessage') or (Assigned(Parent) and
            (Parent.Name = 'FinMessage')));
    end;
  CopyCobolDataToClipboardMenuItem.Visible := (xBind is TIpmItem);
  PasteCobolDataFromClipboardMenuItem.Visible := (xBind is TIpmItem);
  WsdlItemAddMenuItem.Enabled := xEnableAddMenuItems;
  WsdlItemDelMenuItem.Enabled := xEnableDelMenuItems;
  AddChildElementDefMenuItem.Visible := xAddChildVisible;
  ExtendRecursivityMenuItem.Visible := xExtRecurVisible;
  WsdlItemChangeDataTypeMenuItem.Clear;
  WsdlItemChangeDataTypeMenuItem.Enabled := (xBind is TXml) and
    (Assigned((xBind as TXml).Xsd)) and
    ((xBind as TXml).Xsd.IsTypeDefEnabled);
  WsdlItemChangeDataTypeMenuItem.OnClick:=nil;
  if WsdlItemChangeDataTypeMenuItem.Enabled then
  begin
    if (    ((xBind as TXml).TypeDef.xsdType = dtComplexType)
        and ((xBind as TXml).TypeDef.ElementDefs.Count = 0)
        and ((xBind as TXml).TypeDef.AttributeDefs.Count = 0)
       ) then
    begin
      WsdlItemChangeDataTypeMenuItem.OnClick := WsdlItemChangeDataTypeMenuItemClick;
      WsdlItemChangeDataTypeMenuItem.Caption := 'Change datatype to...';
    end
    else
    begin // to stay within boudarues of extention
      xRootBase := (xBind as TXml).TypeDef;
      while Assigned(xRootBase.BaseDataType) do
        xRootBase := xRootBase.BaseDataType;
      _createTypeSubMenuItems(WsdlItemChangeDataTypeMenuItem, (xBind as TXml).TypeDef, xRootBase);
      WsdlItemChangeDataTypeMenuItem.Caption := 'Change datatype to';
    end;
  end;
  ElementvalueMenuItem.Enabled := xEnableStamp;
  AssignExpressionMenuItem.Enabled := xEnableStamp;
  AssignEvaluationMenuItem.Enabled := xEnableCheck;
end;

procedure TMainForm.ExecuteRequestActionUpdate(Sender: TObject);
begin
  ExecuteRequestAction.Enabled :=
        Assigned(WsdlOperation)
    and Assigned(WsdlMessage)
    and (NumberOfBlockingThreads < 1)
    ;
end;

procedure TMainForm.ExecuteAllRequestsActionUpdate(Sender: TObject);
begin
  ExecuteAllRequestsAction.Enabled := Assigned(WsdlOperation)
                                  and Assigned(WsdlMessage)
                                  and (WsdlOperation.StubAction = saRequest)
                                  and (NumberOfBlockingThreads < 1)
                                    ;
end;

procedure TMainForm.ScriptSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
  function SizeOfOperationToolBar: Integer;
  var
    X: Integer;
  begin
    result := 6;
    with ActionToolBar do
      for X := 0 to ControlCount - 1 do
        result := result + Controls[X].Width;
  end;

begin
  Accept := (NewSize > SizeOfOperationToolBar);
end;

procedure TMainForm.FilterLogActionExecute(Sender: TObject);
begin
  Application.CreateForm(TLogFilterForm, LogFilterForm);
  try
    LogFilterForm.Caption := 'Log messages filter';
    LogFilterForm.FilterEnabled := se.LogFilter.Enabled;
    LogFilterForm.FilterStyle := se.LogFilter.FilterStyle;
    LogFilterForm.MatchAny := se.LogFilter.MatchAny;
    LogFilterForm.ActionEnabled := se.LogFilter.StubActionEnabled;
    LogFilterForm.ActionEquals := se.LogFilter.StubActionEquals;
    LogFilterForm.ActionValue := se.LogFilter.StubAction;
    LogFilterForm.MiMEnabled := se.LogFilter.MiMEnabled;
    LogFilterForm.RequestMiMEnabled := se.LogFilter.RequestMiMEnabled;
    LogFilterForm.ReplyMiMEnabled := se.LogFilter.ReplyMiMEnabled;
    LogFilterForm.MessageValidationEnabled :=
      se.LogFilter.MessageValidationEnabled;
    LogFilterForm.RequestValidationEnabled :=
      se.LogFilter.RequestValidationEnabled;
    LogFilterForm.ReplyValidationEnabled := se.LogFilter.ReplyValidationEnabled;
    LogFilterForm.ExceptionEnabled := se.LogFilter.ExceptionEnabled;
    LogFilterForm.ExceptionEquals := se.LogFilter.ExceptionEquals;
    LogFilterForm.ExceptionValue := se.LogFilter.Exception;
    LogFilterForm.ExceptionIsRegExpr := se.LogFilter.ExceptionRegExp;
    LogFilterForm.ServiceEnabled := se.LogFilter.ServiceEnabled;
    LogFilterForm.ServiceEquals := se.LogFilter.ServiceEquals;
    LogFilterForm.ServiceValue := se.LogFilter.Service;
    LogFilterForm.ServiceIsRegExpr := se.LogFilter.ServiceRegExp;
    LogFilterForm.OperationEnabled := se.LogFilter.OperationEnabled;
    LogFilterForm.OperationEquals := se.LogFilter.OperationEquals;
    LogFilterForm.OperationValue := se.LogFilter.Operation;
    LogFilterForm.OperationIsRegExpr := se.LogFilter.OperationRegExp;
    LogFilterForm.CorrelationEnabled := se.LogFilter.CorrelationEnabled;
    LogFilterForm.CorrelationEquals := se.LogFilter.CorrelationEquals;
    LogFilterForm.CorrelationValue := se.LogFilter.Correlation;
    LogFilterForm.CorrelationIsRegExpr := se.LogFilter.CorrelationRegExp;
    LogFilterForm.RequestEnabled := se.LogFilter.RequestEnabled;
    LogFilterForm.RequestEquals := se.LogFilter.RequestEquals;
    LogFilterForm.RequestValue := se.LogFilter.Request;
    LogFilterForm.ReplyEnabled := se.LogFilter.ReplyEnabled;
    LogFilterForm.ReplyEquals := se.LogFilter.ReplyEquals;
    LogFilterForm.ReplyValue := se.LogFilter.Reply;
    LogFilterForm.UnexpectedValuesEnabled :=
      se.LogFilter.UnexpectedValuesEnabled;
    LogFilterForm.RemarksEnabled := se.LogFilter.RemarksEnabled;
    LogFilterForm.ShowModal;
    if LogFilterForm.ModalResult = mrOk then
    begin
      se.AcquireLogLock;
      try
        se.LogFilter.Enabled := LogFilterForm.FilterEnabled;
        se.LogFilter.FilterStyle := LogFilterForm.FilterStyle;
        se.LogFilter.MatchAny := LogFilterForm.MatchAny;
        se.LogFilter.StubActionEnabled := LogFilterForm.ActionEnabled;
        se.LogFilter.StubActionEquals := LogFilterForm.ActionEquals;
        se.LogFilter.StubAction := LogFilterForm.ActionValue;
        se.LogFilter.MiMEnabled := LogFilterForm.MiMEnabled;
        se.LogFilter.RequestMiMEnabled := LogFilterForm.RequestMiMEnabled;
        se.LogFilter.ReplyMiMEnabled := LogFilterForm.ReplyMiMEnabled;
        se.LogFilter.MessageValidationEnabled :=
          LogFilterForm.MessageValidationEnabled;
        se.LogFilter.RequestValidationEnabled :=
          LogFilterForm.RequestValidationEnabled;
        se.LogFilter.ReplyValidationEnabled :=
          LogFilterForm.ReplyValidationEnabled;
        se.LogFilter.ExceptionEnabled := LogFilterForm.ExceptionEnabled;
        se.LogFilter.ExceptionEquals := LogFilterForm.ExceptionEquals;
        se.LogFilter.Exception := LogFilterForm.ExceptionValue;
        se.LogFilter.ExceptionRegExp := LogFilterForm.ExceptionIsRegExpr;
        se.LogFilter.ServiceEnabled := LogFilterForm.ServiceEnabled;
        se.LogFilter.ServiceEquals := LogFilterForm.ServiceEquals;
        se.LogFilter.Service := LogFilterForm.ServiceValue;
        se.LogFilter.ServiceRegExp := LogFilterForm.ServiceIsRegExpr;
        se.LogFilter.OperationEnabled := LogFilterForm.OperationEnabled;
        se.LogFilter.OperationEquals := LogFilterForm.OperationEquals;
        se.LogFilter.Operation := LogFilterForm.OperationValue;
        se.LogFilter.OperationRegExp := LogFilterForm.OperationIsRegExpr;
        se.LogFilter.CorrelationEnabled := LogFilterForm.CorrelationEnabled;
        se.LogFilter.CorrelationEquals := LogFilterForm.CorrelationEquals;
        se.LogFilter.Correlation := LogFilterForm.CorrelationValue;
        se.LogFilter.CorrelationRegExp := LogFilterForm.CorrelationIsRegExpr;
        se.LogFilter.RequestEnabled := LogFilterForm.RequestEnabled;
        se.LogFilter.RequestEquals := LogFilterForm.RequestEquals;
        se.LogFilter.Request := LogFilterForm.RequestValue;
        se.LogFilter.ReplyEnabled := LogFilterForm.ReplyEnabled;
        se.LogFilter.ReplyEquals := LogFilterForm.ReplyEquals;
        se.LogFilter.Reply := LogFilterForm.ReplyValue;
        se.LogFilter.UnexpectedValuesEnabled :=
          LogFilterForm.UnexpectedValuesEnabled;
        se.LogFilter.RemarksEnabled := LogFilterForm.RemarksEnabled;
        if se.LogFilter.Enabled then
          if (se.LogFilter.FilterStyle = fsShowMatch) or
            (se.LogFilter.FilterStyle = fsShowMismatch) then
            FilterLogAction.ImageIndex := 34
          else
            FilterLogAction.ImageIndex := 35
          else
            FilterLogAction.ImageIndex := 33;
        RefreshLogList;
      finally
        se.ReleaseLogLock;
      end;
    end;
  finally
    FreeAndNil(LogFilterForm);
  end;
end;

procedure TMainForm.RefreshLogList;
var
  xLog: TLog;
  xNode: PVirtualNode;
begin
  MessagesVTS.BeginUpdate;
  try
    xNode := MessagesVTS.GetFirst;
    while Assigned(xNode) do
    begin
      xLog := NodeToMsgLog(False,MessagesVTS, xNode);
      se.LogFilter.Execute(xLog);
      MessagesVTS.IsVisible[xNode] := xLog.PassesFilter;
      xNode := MessagesVTS.GetNext(xNode);
    end;
  finally
    MessagesVTS.EndUpdate;
  end;
end;


procedure TMainForm.RefreshLog;
  function _refreshLogging: Boolean;
  var
    x: Integer;
    xLog: TLog;
    xData: PLogTreeRec;
    xNode: PVirtualNode;
  begin
    result := False;
    if se.toDisplayLogs.Count > 0 then with MessagesVTS.Header do
    begin
      SortColumn := -1;
      SortDirection := sdAscending;
    end;
    for x := 0 to se.toDisplayLogs.Count - 1 do
    begin
      xLog := se.toDisplayLogs.LogItems[x];
      se.displayedLogs.SaveLog('', xLog);
      xLog.Nr := se.displayedLogs.Number;
      result := True;
      se.LogFilter.Execute(xLog);
      xNode := MessagesVTS.AddChild(nil);
      xData := MessagesVTS.GetNodeData(xNode);
      xData.Log := xLog;
      xLog.displayRef := PDisplayRef(xNode);
      MessagesVTS.IsVisible[xNode] := xLog.PassesFilter;
      if se.displayedLogsmaxEntries > -1 then
      while se.displayedLogs.Count > se.displayedLogsmaxEntries do
      begin
        MessagesVTS.DeleteNode(MessagesVTS.GetFirst);
        se.displayedLogs.LogItems[0].displayRef := nil;
        se.displayedLogs.Delete(0);
      end;
    end;
    se.toDisplayLogs.Clear;
    for x := 0 to se.toUpdateDisplayLogs.Count - 1 do
    begin
      xLog := se.toUpdateDisplayLogs.LogItems[x];
      if Assigned (xlog.displayRef) then
      begin
        xNode := PVirtualNode (xLog.displayRef);
        MessagesVTS.InvalidateNode(xNode);
        if xNode = MessagesVTS.FocusedNode then
        begin
          UpdateLogTabs (xLog);
        end;
      end;
    end;
    se.toUpdateDisplayLogs.Clear;
  end;
  function _refreshExceptions: Boolean;
  var
    xLog: TExceptionLog;
    xNode: PVirtualNode;
    xData: PExceptionTreeRec;
    x: Integer;
  begin
    result := False;
    for x := 0 to se.toDisplayExceptions.Count - 1 do
    begin
      xLog := se.toDisplayExceptions.EventItems[x];
      se.displayedExceptions.AddObject('', xLog);
      result := True;
      xNode := ExceptionsVTS.AddChild(nil);
      xData := ExceptionsVTS.GetNodeData(xNode);
      xData.xLog := xLog;
    end;
    se.toDisplayExceptions.Clear;
  end;
  function _refreshSnapshots: Boolean;
  var
    xReport: TSnapshot;
    xNode: PVirtualNode;
    xData: PReportTreeRec;
    x: Integer;
  begin
    result := False;
    for x := 0 to se.toDisplaySnapshots.Count - 1 do
    begin
      xReport := se.toDisplaySnapshots.SnapshotItems[x];
      se.displayedSnapshots.AddObject('', xReport);
      result := True;
      xNode := SnapshotsVTS.AddChild(nil);
      xData := SnapshotsVTS.GetNodeData(xNode);
      xData.Report := xReport;
    end;
    se.toDisplaySnapshots.Clear;
    if LogTabControl.TabIndex = Ord (spSnapshots) then
      SnapshotsVTS.Invalidate;
  end;
var
  logAdded, exceptionAdded, uiInvalidated: Boolean;
begin
  if not Assigned (se) then Exit;
  se.AcquireLogLock;
  try
    uiInvalidated := se.uiInvalid;
    se.uiInvalid := False;
    if se.doClearLogs then
    begin
      MessagesVTS.Clear;
      MessagesVTS.Header.SortColumn := -1;
      MessagesVTS.Header.SortDirection := sdAscending;
      se.displayedLogs.Clear;
      LogMemo.Text := '';
      se.doClearLogs := False;
    end;
    if se.doClearSnapshots then
    begin
      SnapshotsVTS.Clear;
      SnapshotsVTS.Header.SortColumn := -1;
      SnapshotsVTS.Header.SortDirection := sdAscending;
      se.doClearSnapshots := False;
    end;
    logAdded := _refreshLogging;
    exceptionAdded := _refreshExceptions;
    _refreshSnapshots;
    SetUiProgress;
  finally
    se.ReleaseLogLock;
  end;
  if uiInvalidated then
    setWsdlOperation(WsdlOperation);
  if logAdded then
    if doScrollMessagesIntoView then
      MessagesVTS.ScrollIntoView(MessagesVTS.GetLast, True, False);
  if exceptionAdded then
  begin
    if LogTabControl.TabIndex <> Ord (spNotifications) then
    begin
      LogTabControl.Tabs [Ord (spNotifications)] := notifyTabCaption + ' [*]';
    end;
    if doScrollExceptionsIntoView then
    begin
      ExceptionsVTS.ScrollIntoView(ExceptionsVTS.GetLast, True, False);
      LogTabControl.TabIndex := Ord (spNotifications);
    end;
  end;
end;

procedure TMainForm.RefreshLogTimerTimer(Sender: TObject);
begin
  RefreshLog;
end;

function TMainForm.getDoValidateReplies: Boolean;
begin
  result := Assigned(se) and se.doValidateReplies;
end;

function TMainForm.getDoValidateRequests: Boolean;
begin
  result := Assigned(se) and se.doValidateRequests;
end;

function TMainForm.getHintStrDisabledWhileActive: String;
begin
  result := '';
  if Assigned(se) and se.IsActive then
    result := ' (disabled while Active)';
end;

procedure TMainForm .setdoShowDesignSplitVertical (AValue : Boolean );
begin
  if fdoShowDesignSplitVertical = AValue then Exit ;
  fdoShowDesignSplitVertical := AValue ;
  if fdoShowDesignSplitVertical then
  begin
    GridDataPanel.Align := alLeft;
    GridDataPanel.Width := MiddlePanel.Width Div 2;
    GridDataPanel.Left := 0;
    DataPanelSplitter.Align := alNone;
    DataPanelSplitter.Left := 1;
    DataPanelSplitter.Align := alLeft;
  end
  else
  begin
    GridDataPanel.Align := alTop;
    GridDataPanel.Height := MiddlePanel.Height Div 2;
    GridDataPanel.Top := 0;
    DataPanelSplitter.Align := alNone;
    DataPanelSplitter.Top := 1;
    DataPanelSplitter.Align := alTop;
  end;
  DesignPanelSplitVerticalMenuItem.Checked := AValue;
end;

procedure TMainForm.setDoValidateReplies(const Value: Boolean);
begin
  stubChanged := doValidateReplies <> Value;
  if Assigned(se) then
    se.doValidateReplies := Value;
  CheckBoxClick(nil);
end;

procedure TMainForm.setDoValidateRequests(const Value: Boolean);
begin
  stubChanged := doValidateRequests <> Value;
  if Assigned(se) then
    se.doValidateRequests := Value;
  CheckBoxClick(nil);
end;

procedure TMainForm.ValidateRequestsActionExecute(Sender: TObject);
begin
  doValidateRequests := not doValidateRequests;
end;

procedure TMainForm.ValidateRepliesActionExecute(Sender: TObject);
begin
  doValidateReplies := not doValidateReplies;
end;

procedure TMainForm.LogPopupMenuPopup(Sender: TObject);
var
  xLog: TLog;
  n: Integer;
begin
  n := MessagesVTS.SelectedCount;
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  ShowLogDifferencesAction.Enabled := (n = 2);
  RequestMiMAction.Enabled := Assigned(xLog)
                          and (xLog.RequestBodyMiM <> '')
                          and (xLog.RequestBodyMiM <> xLog.RequestBody)
                            ;
  ReplyMiMAction.Enabled := Assigned(xLog)
                        and (xLog.ReplyBodyMiM <> '')
                        and (xLog.ReplyBodyMiM <> xLog.ReplyBody)
                          ;
  Log2DesignAction.Enabled := (n > 0);
  DisplayedcolumnMenuItem.Enabled := Assigned (xLog)
                                 and Assigned (xLog.Operation)
                                 and (n = 1)
                                 and (MessagesVTS.FocusedColumn >= Ord (logStdColumnCount))
                                   ;
end;

procedure TMainForm.RequestMiMActionExecute(Sender: TObject);
var
  xLog: TLog;
  aXml, bXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  xLog := NodeToMsgLog(True,MessagesVTS, MessagesVTS.FocusedNode);
  try
    aXml := TXml.Create;
    aXml.LoadFromString(xLog.RequestBody, nil);
    bXml := TXml.Create;
    bXml.LoadFromString(xLog.RequestBodyMiM, nil);
  finally
    xLog.Disclaim;
  end;
  a2bInitialize;
  try
    xA2B := TA2BXml.CreateA2B('', aXml, bXml, Nil);
  finally
    a2bUninitialize;
  end;
  FreeAndNil(aXml);
  FreeAndNil(bXml);
  Application.CreateForm(TShowA2BXmlForm, xForm);
  try
    xForm.Caption := 'Changes in Request';
    xForm.ignoreDifferencesOn := se.ignoreDifferencesOn;
    xForm.ignoreAddingOn := se.ignoreAddingOn;
    xForm.ignoreRemovingOn := se.ignoreRemovingOn;
    xForm.Xml := xA2B;
    xForm.ShowModal;
    if xForm.RefreshNeeded then
      FormShow(nil);
  finally
    FreeAndNil(xForm);
  end;
end;

procedure TMainForm.ReplyMiMActionExecute(Sender: TObject);
var
  xLog: TLog;
  aXml, bXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  try
    aXml := TXml.Create;
    aXml.LoadFromString(xLog.ReplyBodyMiM, nil);
    bXml := TXml.Create;
    bXml.LoadFromString(xLog.ReplyBody, nil);
  finally
    xLog.Disclaim;
  end;
  a2bInitialize;
  try
    xA2B := TA2BXml.CreateA2B('', aXml, bXml, Nil);
  finally
    a2bUninitialize;
  end;
  FreeAndNil(aXml);
  FreeAndNil(bXml);
  Application.CreateForm(TShowA2BXmlForm, xForm);
  try
    xForm.Caption := 'Changes in Reply';
    xForm.ignoreDifferencesOn := se.ignoreDifferencesOn;
    xForm.ignoreAddingOn := se.ignoreAddingOn;
    xForm.ignoreRemovingOn := se.ignoreRemovingOn;
    xForm.Xml := xA2B;
    xForm.ShowModal;
    if xForm.RefreshNeeded then
      FormShow(nil);
  finally
    FreeAndNil(xForm);
  end;
end;

procedure TMainForm.FindActionUpdate(Sender: TObject);
begin
  FindAction.Enabled := (Assigned(WsdlMessage));
end;

procedure TMainForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  xNodeText: String;
begin
  xNodeText := '';
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
        CurItem := InWsdlTreeView.GetNext(InWsdlTreeView.FocusedNode);
      if (CurItem = nil) // if next object is nil
        or (xmlUtil.SearchScope = 1) then // or search entire scope
        CurItem := InWsdlTreeView.GetFirst; // search from begin
      while not(CurItem = nil) and not Found do
      begin
        if xmlUtil.SearchIn = treeButtonColumn then
          xmlUtil.SearchIn := treeValueColumn;
        InWsdlTreeViewGetText(InWsdlTreeView, CurItem, xmlUtil.SearchIn,
          ttNormal, xNodeText);
        Found := StringMatchesMask(xNodeText, xmlUtil.SearchString, False,
          xmlUtil.SearchUseRegExp);
        if not Found then
          CurItem := InWsdlTreeView.GetNext(CurItem);
      end;
      if not Found then
        ShowMessage(xmlUtil.SearchString + ' not found')
      else
      begin
        InWsdlTreeView.FocusedNode := CurItem;
      end;
    end;
  finally
    FreeAndNil(FindDlg);
  end;
  {
    EnableActions;
  }
end;

procedure TMainForm.FindNextActionUpdate(Sender: TObject);
begin
  FindNextAction.Enabled := (Assigned(WsdlMessage)) and
    (xmlUtil.SearchString <> '');
end;

procedure TMainForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  xNodeText: String;
begin
  xNodeText := ''; //avoid warning
  if True then
  begin
    Found := False;
    CurNode := InWsdlTreeView.GetNext(InWsdlTreeView.FocusedNode);
    while not(CurNode = nil) and not Found do
    begin
      InWsdlTreeViewGetText(InWsdlTreeView, CurNode, xmlUtil.SearchIn,
        ttNormal, xNodeText);
      Found := StringMatchesMask(xNodeText, xmlUtil.SearchString, False,
        xmlUtil.SearchUseRegExp);
      if not Found then
        CurNode := InWsdlTreeView.GetNext(CurNode);
    end;
    if not Found then
      ShowMessage(xmlUtil.SearchString + ' not found')
    else
    begin
      InWsdlTreeView.FocusedNode := CurNode;
    end;
  end;
  {
    EnableActions;
  }
end;

procedure TMainForm.Validate1Click(Sender: TObject);
begin
  xmlUtil.Validate(NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode));
end;

function TMainForm.XmlBindToNode(aTreeView: TVirtualStringTree;
  aBind: TCustomBindable): PVirtualNode;
var
  xBind: TCustomBindable;
begin
  result := aTreeView.GetFirst;
  while Assigned(result) do
  begin
    xBind := NodeToBind(aTreeView, result);
    if (xBind is TXml) and ((xBind as TXml).Xsd = (aBind as TXml).Xsd) then
      exit;
    result := aTreeView.GetNext(result);
  end;
end;

procedure TMainForm.FocusOnFullCaption(aFullCaption: String);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  if aFullCaption <> '' then
  begin
    xNode := InWsdlTreeView.GetFirst;
    while Assigned(xNode) do
    begin
      xBind := NodeToBind(InWsdlTreeView, xNode);
      if (xBind.FullCaption = aFullCaption) then
      begin
        InWsdlTreeView.FocusedNode := xNode;
        InWsdlTreeView.FocusedColumn := treeValueColumn;
        exit;
      end;
      xNode := InWsdlTreeView.GetNext(xNode);
    end;
  end;
end;

procedure TMainForm.CheckTreeActionUpdate(Sender: TObject);
begin
  CheckTreeAction.Enabled := (Assigned(WsdlOperation))
                         and (WsdlOperation.Messages.Count > 0)
                         and (WsdlOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
                           ;
end;

procedure TMainForm.CheckTreeActionExecute(Sender: TObject);
var
  xNode, lastNode: PVirtualNode;
  xBind: TCustomBindable;
  xMessage: String;
begin
  xMessage := ''; //avoid warning
  try
    XmlUtil.PushCursor (crHourGlass);
    xNode := InWsdlTreeView.GetFirst;
    xBind := NodeToBind(InWsdlTreeView, xNode);
    if (xBind is TIpmItem) then
      raise Exception.Create('Not implemented for Cobol');
    if WsdlOperation.WsdlService.DescriptionType = ipmDTFreeFormat then
      raise Exception.Create('Not implemented for FreeFormat');
    if (not(xBind as TXml).TypeDef.IsValidXml((xBind as TXml), xMessage)) then
    // at least one error; try to come close
    // might be an assignment
    begin
      while Assigned(xNode) and ((xNode.Parent <> InWsdlTreeView.RootNode) or
          (xNode = InWsdlTreeView.GetFirst)) do
      begin // first check elements
        lastNode := xNode; // remember for next testloop
        xBind := NodeToBind(InWsdlTreeView, xNode);
        if (xBind is TXml) and (xBind as TXml).CheckedAllUp and
          (not(xBind as TXml).Group) and (not xBind.IsExpression) and
          (not(xBind as TXml).IsValueValidAgainstXsd(xMessage)) then
        begin
          InWsdlTreeView.FocusedNode := xNode;
          InWsdlTreeView.FocusedColumn := treeValueColumn;
          InWsdlTreeView.SetFocus;
          Raise Exception.Create(xMessage);
        end;
        if (xBind is TXml) // check for missing mandatory element or group
          and (not(xBind as TXml).Checked) and
          ((xBind as TXml).Xsd.minOccurs <> '0') and
          ((xBind as TXml).Xsd.maxOccurs = '1') // better not check in case more ocurrences possible
          and ((not Assigned((xBind as TXml).Parent)) or
            ((xBind as TXml).Parent.CheckedAllUp and
              (LowerCase((xBind.Parent as TXml).Xsd.sType.ContentModel)
                <> 'choice'))) then
        begin
          InWsdlTreeView.FocusedNode := xNode;
          InWsdlTreeView.FocusedColumn := treeValueColumn;
          InWsdlTreeView.SetFocus;
          Raise Exception.Create((xBind as TXml).TagName + ' expected');
        end;
        xNode := InWsdlTreeView.GetNext(xNode);
      end;

      // xNode := InWsdlTreeView.GetLast; // check groups, from last to first to be most specific
      xNode := lastNode; // check groups, from last to first to be most specific
      while Assigned(xNode) do
      begin
        xBind := NodeToBind(InWsdlTreeView, xNode);
        if (xBind is TXml) and (xBind as TXml).CheckedAllUp and
          ((xBind as TXml).Group) and (not(xBind as TXml).TypeDef.IsValidXml
            ((xBind as TXml), xMessage)) then
        begin
          InWsdlTreeView.FocusedNode := xNode;
          InWsdlTreeView.FocusedColumn := InWsdlTreeView.Header.MainColumn;
          InWsdlTreeView.SetFocus;
          Raise Exception.Create(xMessage);
        end;
        xNode := InWsdlTreeView.GetPrevious(xNode);
      end;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ProjectOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.ProjectOptionsAsXml(False, '');
  try
    if EditXmlXsdBased ('Project Options'
                       , ''
                       , ''
                       , ''
                       , se.IsActive
                       , False
                       , esOne
                       , projectOptionsXsd
                       , xXml
                       ) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        se.ProjectOptionsFromXml(xXml);
        LogUpdateColumns;
      finally
        ReleaseLock;
      end;
      CheckBoxClick(nil);
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.HandleException(Sender: TObject; E: Exception);
var
  s: String;
begin
  if AnsiStartsText('System Error.  Code: 1400.', E.Message) then
    exit; // no idea, no impact
  { }
  s := ExceptionStackListString (E);
  { }
  if MessageDlg(E.Message + #10#13#10#13 + 'Show stack trace?', mtError,
    [mbNo, mbYes], 0) = mrYes then
    ShowText('Exception details', E.Message + #10#13#10#13 + s);
end;

procedure TMainForm.LogDisplayedColumnsAddClick(Sender: TObject);
var
  f: Integer;
begin
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Name for new column';
    PromptForm.PromptEdit.Text := '';
    PromptForm.Numeric := False;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
    begin
      if PromptForm.PromptEdit.Text = '' then
        raise Exception.Create('Column name must have a value');
      if se.DisplayedLogColumns.Find(PromptForm.PromptEdit.Text, f) then
        raise Exception.Create(Format('A column with name %s already exists',
            [PromptForm.PromptEdit.Text]));
      se.DisplayedLogColumns.Add(PromptForm.PromptEdit.Text);
      // CreateScriptsSubMenuItems;
      stubChanged := True;
    end;
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm.AddChildElementTypeDefMenuItemClick(Sender: TObject);
  procedure _updateTypedef(aBinder: TWsdlBinder; aPath: String; nType: TXsdDataType; aXsd: TXsd);
    procedure _update(aXml: TXml; aPath: String);
    var
      x: Integer;
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _update(aXml.Items.XmlItems[x], aPath);
      if aXml.FullCaption = aPath then
      begin
        bindRefId := 0;
        aXml.AddXml(TXml.Create(0, aXsd));
        aXml.TypeDef := nType;
      end;
    end;
  var
    xBind: TCustomBindable;
  begin
    xBind := aBinder.FindBind(aPath);
    if Assigned (xBind) then
    begin
      if aBinder.reqBind.IsAncestorOf(xBind) then
        _update(aBinder.reqXml, xBind.FullCaption)
      else
        _update(aBinder.rpyXml, xBind.FullCaption);
    end;
  end;
var
  X, m: Integer;
  xxsd: TXsd;
  xXml: TXml;
  xBind: TCustomBindable;
  nTypeDef, oTypeDef, cTypeDef: TXsdDataType;
  xPath: String;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if not Assigned(xBind) then
    raise Exception.Create('no element selected');
  if not(xBind is TXml) then
    raise Exception.Create('operation only valid on XML elements');
  xXml := xBind as TXml;
  if not Assigned(xXml.Xsd) then
    raise Exception.Create('opeation requires an XSD on the selected element');
  Application.CreateForm(TChoose2StringsForm, Choose2StringsForm);
  try
    with Choose2StringsForm do
    begin
      ListOfLists := createListOfListsForTypeDefs (Wsdl.XsdDescr.TypeDefs);
      Caption := 'Choose from Types';
      ShowModal;
      if ModalResult = mrOk then
      begin
        cTypeDef := Wsdl.XsdDescr.FindTypeDef(ChoosenLeftString, ChoosenRightString);
        if not Assigned (cTypeDef) then
          raise Exception.Create ('procedure TMainForm.AddChildElementDefMenuItemClick(Sender: TObject): cTypeDef not assigned');
        Application.CreateForm(TPromptForm, PromptForm);
        try
          PromptForm.Caption := 'Name for '
                              + cTypeDef.NameSpace
                              + ';'
                              + cTypeDef.Name
                              + ' at '
                              + xXml.FullCaption
                              ;
          PromptForm.PromptEdit.Text := cTypeDef.Name;
          PromptForm.Numeric := False;
          PromptForm.ShowModal;
          if PromptForm.ModalResult = mrOk then
          begin
            oTypeDef := xXml.Xsd.sType;
            xxsd := xXml.Xsd.AddElementDef ( Wsdl.XsdDescr
                                           , PromptForm.PromptEdit.Text
                                           , cTypeDef
                                           );
            nTypeDef := xXml.Xsd.sType;
            xPath := IfThen(WsdlMessage.reqBind.IsAncestorOf(xXml), 'Req.', 'Rpy.')
                   + xXml.FullCaption
                   ;
            if not oTypeDef.Manually then
              WsdlOperation.BindablesWithAddedElement.AddObject (xPath, WsdlOperation.FindBind(xPath));
            xXml.Checked := True;
            _updateTypedef ( WsdlOperation
                           , xPath
                           , nTypeDef
                           , xxsd
                           );
            for m := 0 to WsdlOperation.Messages.Count - 1 do
            begin
              _updateTypedef ( WsdlOperation.Messages.Messages[m]
                             , xPath
                             , nTypeDef
                             , xxsd
                             );
            end;
            GridView.OnFocusChanged(GridView, GridView.FocusedNode,
              GridView.FocusedColumn);
            stubChanged := True;
          end;
        finally
          FreeAndNil(PromptForm);
        end;
      end;
    end;
  finally
    if Assigned (Choose2StringsForm.ListOfLists) then
      Choose2StringsForm.ListOfLists.Free;
    FreeAndNil(Choose2StringsForm);
  end;
end;

function TMainForm.getXmlViewType: TxvViewType;
begin
  result := TxvViewType(ViewStyleComboBox.ItemIndex);
end;

procedure TMainForm.ViewStyleComboBoxChange(Sender: TObject);
begin
  if Assigned(Wsdl) then
  begin
    RevalidateXmlTreeView(InWsdlTreeView);
    InWsdlTreeView.ScrollIntoView(InWsdlTreeView.FocusedNode, True, False);
  end;
end;

procedure TMainForm.InWsdlTreeViewFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  xBind: TCustomBindable;
begin
  if Assigned(OldNode) then
  begin
    xBind := NodeToBind(Sender, OldNode);
    if xBind is TXml then
      Sender.IsVisible[OldNode] := xmlVisibility(xBind as TXml);
  end;
end;

procedure TMainForm.ExpressError(Sender: TObject;
  LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
begin
  scriptPreparedWell := False;
  ShowMessage(Format('%s at line %d, column %d: %s', [Data, LineNumber,
      ColumnNumber, TokenString]));
end;

procedure TMainForm.GridToolBarResize(Sender: TObject);
begin
  // ProgressBar.Width := ToolBar2.Width - ProgressBar.Left - 4;
end;

procedure TMainForm.ToolButton67Click(Sender: TObject);
begin
  EndEdit;
end;

procedure TMainForm.ToolButton9DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Sender);
end;

procedure TMainForm.AbortActionExecute(Sender: TObject);
begin
  abortPressed := True;
end;

procedure TMainForm.All1Click(Sender: TObject);
begin
  PopulateXml(xvAll);
end;

procedure TMainForm.Required1Click(Sender: TObject);
begin
  PopulateXml(xvRequired);
end;

procedure TMainForm.ShowReplyAsHtmlActionExecute(Sender: TObject);
var
  xLog: TLog;
begin
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  if Assigned(xLog) and (xLog is TLog) and (xLog.ReplyBody <> '') then
    ShowHtml('Reply as HTML', xLog.ReplyBody);
end;

procedure TMainForm.ShowReplyAsXmlGridActionExecute(Sender: TObject);
var
  cForm: TIpmGridForm;
  xString: String;
begin
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.ReplyBody, '');
  if (xString <> '') then
  begin
    if Assigned(claimedLog.Mssg) and (claimedLog.Operation.rpyBind is TIpmItem) then
    begin
      if se.ShowLogCobolStyle = slCobol then
      begin
        Application.CreateForm(TIpmGridForm, cForm);
        try
          cForm.isReadOnly := False;
          cForm.doConfirmRemovals := True;
          cForm.Ipm := claimedLog.Operation.rpyBind as TIpmItem;
          cForm.Ipm.BufferToValues(FoundErrorInBuffer, xString);
          cForm.Caption := 'Reply as Grid';
          cForm.ShowModal;
        finally
          FreeAndNil(cForm);
        end;
      end
      else
      begin
        with claimedLog.rpyBodyAsXml do
          try
            ShowTextAsGrid('Reply as Grid', AsText(False, 0, False, False));
          finally
            Free;
          end;
      end;
      exit;
    end;
    if Assigned(claimedLog.Operation) then
    begin
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTSwiftMT then
      begin
        with claimedLog.rpyBodyAsXml do
          try
            ShowTextAsGrid('Reply as Grid', AsText(False, 0, False, False));
          finally
            Free;
          end;
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTFreeFormat then
      begin
        xmlUtil.presentString('Reply freeformat', claimedLog.ReplyBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTEmail then
      begin
        xmlUtil.presentString('Reply freeformat', claimedLog.ReplyBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTWsdl then
      begin
        XmlUtil.PushCursor (crHourGlass);
        try
          xmlUtil.ShowSoapBodyInGrid(claimedLog.ReplyBody);
        finally
          XmlUtil.PopCursor;
        end;
        exit;
      end;
    end;
    { }{
      with claimedLog.rpyBodyAsXml do
      try
      ShowTextAsGrid ('Reply as Grid', asText(False, 0, False, False));
      finally
      Free;
      end;
      { }
    xmlUtil.presentString('Reply', claimedLog.ReplyBody);
  end;
end;

procedure TMainForm.ShowRequestAsXmlGridActionUpdate(Sender: TObject);
begin
  ShowRequestAsXmlGridAction.Enabled := (LogMemo.Text <> '');
end;

procedure TMainForm.ShowSelectedRequestsAsXmlGridActionExecute(Sender: TObject);
var
  xNode: PVirtualNode;
  xData: PLogTreeRec;
  aXml: TXml;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    aXml := TXml.CreateAsString('reqXmls', '');
    try
      xNode := MessagesVTS.GetFirstSelected;
      while Assigned(xNode) do
      begin
        xData := MessagesVTS.GetNodeData(xNode);
        if Assigned(xData.Log) and (xData.Log is TLog) then
        with xData.Log as TLog do
          aXml.AddXml(reqBodyAsXml);
        xNode := MessagesVTS.GetNextSelected(xNode);
      end;
      ShowTextAsGrid('Requests as Grid', aXml.AsText(False, 0, False, False));
    finally
      aXml.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowSelectedRequestsAsXmlGridActionUpdate(Sender: TObject);
begin
  ShowSelectedRequestsAsXmlGridAction.Enabled :=
    (MessagesVTS.SelectedCount > 0);
end;

procedure TMainForm.ShowSelectedResponsesAsXmlGridActionExecute
  (Sender: TObject);
var
  xNode: PVirtualNode;
  xData: PLogTreeRec;
  aXml: TXml;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    aXml := TXml.CreateAsString('rpyXmls', '');
    try
      xNode := MessagesVTS.GetFirstSelected;
      while Assigned(xNode) do
      begin
        xData := MessagesVTS.GetNodeData(xNode);
        if Assigned(xData.Log) and (xData.Log is TLog) then
        with xData.Log as TLog do
          aXml.AddXml(rpyBodyAsXml);
        xNode := MessagesVTS.GetNextSelected(xNode);
      end;
      ShowTextAsGrid('Responses as Grid', aXml.AsText(False, 0, False, False));
    finally
      aXml.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowSelectedResponsesAsXmlGridActionUpdate(Sender: TObject);
begin
  ShowSelectedResponsesAsXmlGridAction.Enabled :=
    (MessagesVTS.SelectedCount > 0);
end;

procedure TMainForm.ShowRequestAsHtmlActionExecute(Sender: TObject);
var
  xLog: TLog;
begin
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  if Assigned(xLog) and (xLog is TLog) and (xLog.RequestBody <> '') then
    ShowHtml('Request as HTML', xLog.RequestBody);
end;

procedure TMainForm.ShowRequestAsHtmlActionUpdate(Sender: TObject);
begin
  ShowRequestAsHtmlAction.Enabled := (LogMemo.Text <> '');
end;

procedure TMainForm.ShowLogZoomElementActionExecute(Sender: TObject);
var
  xLog: TLog;
  xXml, zXml: TXml;
  xCaption: String;
  p: Integer;
begin
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  if (Assigned(xLog)) and (xLog is TLog) and Assigned(xLog.Operation) then
  begin
    if (xLog.Operation.ZoomElementCaption <> '') then
    begin
      try
        if Copy(xLog.Operation.ZoomElementCaption, 1, 4) = 'Req.' then
          xXml := xLog.reqBodyAsXml
        else
          xXml := xLog.rpyBodyAsXml;
        xCaption := Copy(xLog.Operation.ZoomElementCaption, 5, 300000);
        if xLog.Operation.WsdlService.DescriptionType = ipmDTWsdl then
        begin
          p := Pos('.', xCaption);
          if p > 0 then
            xCaption := 'Envelope.Body' + Copy(xCaption, p, 300000);
        end;
        zXml := xXml.FindUQXml(xCaption);
        if Assigned(zXml) then
        begin
          if zXml.Items.Count > 0 then
            ShowXml(xLog.Operation.ZoomElementCaption, zXml)
          else
            xmlUtil.presentString(xLog.Operation.ZoomElementCaption,
              zXml.Value);
        end
        else
          ShowMessage('Could not find ' + xLog.Operation.ZoomElementCaption);
      finally
        FreeAndNil(xXml);
      end;
    end
    else
    begin
      SetOperationZoomPath(xLog.Operation);
    end;
  end;
end;

procedure TMainForm.ShowRequestAsXmlGridActionExecute(Sender: TObject);
var
  cForm: TIpmGridForm;
  xString: String;
begin
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.RequestBody, '');
  if (xString <> '') then
  begin
    if Assigned(claimedLog.Mssg) and Assigned(claimedLog.Operation) and
      (claimedLog.Operation.reqBind is TIpmItem) then
    begin
      if se.ShowLogCobolStyle = slCobol then
      begin
        Application.CreateForm(TIpmGridForm, cForm);
        try
          cForm.isReadOnly := False;
          cForm.doConfirmRemovals := True;
          cForm.Ipm := claimedLog.Operation.reqBind as TIpmItem;
          cForm.Ipm.BufferToValues(FoundErrorInBuffer, xString);
          cForm.Caption := 'Request as Grid';
          cForm.ShowModal;
        finally
          FreeAndNil(cForm);
        end;
      end
      else
      begin
        with claimedLog.reqBodyAsXml do
          try
            ShowTextAsGrid('Request as Grid', AsText(False, 0, False, False));
          finally
            Free;
          end;
      end;
      exit;
    end;
    if Assigned(claimedLog.Operation) then
    begin
      if (claimedLog.Operation.WsdlService.DescriptionType = ipmDTSwiftMT) then
      begin
        with claimedLog.reqBodyAsXml do
          try
            ShowTextAsGrid('Request as Grid', AsText(False, 0, False, False));
          finally
            Free;
          end;
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTFreeFormat then
      begin
        xmlUtil.presentString('Request', claimedLog.RequestBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTEmail then
      begin
        ShowInfoForm('Request freeformat', claimedLog.RequestBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTWsdl then
      begin
        XmlUtil.PushCursor (crHourGlass);
        try
          xmlUtil.ShowSoapBodyInGrid(claimedLog.RequestBody);
        finally
          XmlUtil.PopCursor;
        end;
        exit;
      end;
    end;
    xmlUtil.presentString('Request', claimedLog.RequestBody);
  end;
end;

function TMainForm.getDoCheckExpectedValues: Boolean;
begin
  result := Assigned(se) and se.doCheckExpectedValues;
end;

procedure TMainForm.setDoCheckExpectedValues(const Value: Boolean);
begin
  if Assigned(se) then
    se.doCheckExpectedValues := Value;
  stubChanged := True;
  CheckBoxClick(nil);
end;

procedure TMainForm.ToggleCheckExpectedValuesActionExecute(Sender: TObject);
begin
  AcquireLock;
  try
    stubChanged := True;
    doCheckExpectedValues := not doCheckExpectedValues;
    if Assigned(WsdlOperation) then
    begin
      try
        with InWsdlTreeView do
          IsVisible[GetNextSibling(GetFirst)] := doCheckExpectedValues;
      except
      end;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.ToggleBetaModeActionExecute(Sender: TObject);
begin
  BetaMode := not BetaMode;
  SetBetaMode;
end;

procedure TMainForm.SetAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
  se.abortPressed := Value;
  MessagesStatusBar.Invalidate;
end;

procedure TMainForm.SetBetaMode;
begin
  // ScriptsMenuItem.Visible := BetaMode;
end;

procedure TMainForm.ShowRemarksActionExecute(Sender: TObject);
begin
  if Assigned (claimedLog)
  and (claimedLog.Remarks <> '') then
    xmlUtil.presentString('Remarks', claimedLog.Remarks);
end;

procedure TMainForm.ShowExpectedXmlActionExecute(Sender: TObject);
var
  xHasUnexpectedValue: Boolean;
  xExpectedValuesChecked: Boolean;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    if Assigned(claimedLog) and Assigned(claimedLog.Operation) and Assigned(claimedLog.Mssg) then
    begin
//    AcquireLock;
      try
        xHasUnexpectedValue := claimedLog.HasUnexpectedValue;
        xExpectedValuesChecked := claimedLog.ExpectedValuesChecked;
        claimedLog.Operation.RequestStringToBindables(claimedLog.RequestBody);
        claimedLog.Operation.ReplyStringToBindables(claimedLog.ReplyBody);
        claimedLog.HasUnexpectedValue := claimedLog.Mssg.CheckValues(claimedLog.Operation);
        claimedLog.ExpectedValuesChecked := True;
        MessagesVTS.InvalidateNode(MessagesVTS.FocusedNode);
      finally
//      ReleaseLock;
      end;
      if (xHasUnexpectedValue = claimedLog.HasUnexpectedValue) and
        (xExpectedValuesChecked = claimedLog.ExpectedValuesChecked) then
      begin
        Application.CreateForm(TShowExpectedXmlForm, ShowExpectedXmlForm);
        try
          if claimedLog.StubAction <> saRequest then
            ShowExpectedXmlForm.Bind := claimedLog.Operation.reqBind
          else
            ShowExpectedXmlForm.Bind := claimedLog.Operation.rpyBind;
          ShowExpectedXmlForm.ShowModal;
        finally
          FreeAndNil(ShowExpectedXmlForm);
        end;
      end;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowHtml(aCaption, aInfoString: String);
begin
  xmlUtil.presentAsHTML(aCaption, aInfoString);
end;

procedure TMainForm.AcquireLock;
begin
//  if False then Wsdlz.AcquireLock;
end;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  XmlUtil.presentAsText('AllOps', allOperations.Text);
end;

procedure TMainForm.GenerateScriptAssignmentActionExecute(Sender: TObject);
  function _indent(aLvl: Integer): String;
  begin
    result := '';
    while aLvl > 0 do
    begin
      result := result + ' ';
      Dec(aLvl);
    end;
  end;
  function _xml(aXml: TXml; aLvl: Integer): String;
  var
    X: Integer;
  begin
    result := '';
    if not aXml.Checked then
      exit;
    if aXml.Xsd.sType.ElementDefs.Count = 0 then
    begin
      result := _indent(aLvl) + '.' + aXml.Name + ' := ''' + aXml.Value +
        ''';' + LineEnding;
      exit;
    end;
    result := _indent(aLvl) + 'with new .' + aXml.Name + ' do' + LineEnding + _indent
      (aLvl) + '{' + LineEnding;
    for X := 0 to aXml.Items.Count - 1 do
      result := result + _xml(aXml.Items.XmlItems[X], aLvl + 2);
    result := result + _indent(aLvl) + '}' + LineEnding;
  end;

var
  xXml: TXml;
  xStmnt: String;
begin
  xXml := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) as TXml;
  xStmnt := 'with ' + IfThen(xXml.Root = WsdlOperation.reqBind, 'Req.', 'Rpy.')
    + xXml.Parent.FullCaption + ' do' + LineEnding + '{' + LineEnding + _indent(2)
    + '.' + xXml.Name + ' := nil;' + LineEnding + _xml(xXml, 2) + '}' + LineEnding;
  ClipBoard.AsText := xStmnt;
end;

procedure TMainForm.GenerateMenuHelpActionExecute(Sender: TObject);
var
  xXml: TXml;
  xXsdDescr: TXsdDescr;
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0))
             + DirectorySeparator
             + 'Config'
             + DirectorySeparator
             + _xmlProgName
             + 'Menu.xsd'
             ;
  xXsdDescr := TXsdDescr.Create(1);
  try
    try
      xXsdDescr.LoadXsdFromFile(xFileName, nil);
      xXml := TXml.Create(0, xXsdDescr.TypeDef.ElementDefs.Xsds[0]);
      try
        EditXmlXsdBased('Press Ctrl_Alt_H to generate Help file', 'Menu',
          'Menu', '', True, False, esUsed, xXsdDescr.TypeDef.ElementDefs.Xsds[0], xXml);
      finally
        FreeAndNil(xXml);
      end;
    except
      raise Exception.Create('Could not parse ' + xFileName);
    end;
  finally
    FreeAndNil(xXsdDescr);
  end;
end;

procedure TMainForm.ShowExceptAsHtmlActionExecute(Sender: TObject);
var
  xLog: TLog;
begin
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  if Assigned(xLog) and (xLog is TLog) and (xLog.Exception <> '') then
    ShowHtml('Exception as HTML', xLog.Exception);
end;

procedure TMainForm.startActionExecute(Sender: TObject);
begin
  try
    se.Activate(True);
  finally
    if Assigned(Sender) then // to avoid running out of system resources on reactivate (??)
      CheckBoxClick(nil);
  end;
end;

procedure TMainForm.startActionUpdate(Sender: TObject);
begin
  startAction.Enabled := Assigned(se)
                     and (not se.IsActive)
                     and (NumberOfBlockingThreads < 1)
                     and (NumberOfNonBlockingThreads < 1)
                       ;
end;

procedure TMainForm.stopActionExecute(Sender: TObject);
begin
  try
    se.Activate(False);
  finally
    if Assigned(Sender) then // to avoid running out of system resources on reactivate (??)
      CheckBoxClick(nil);
  end;
end;

procedure TMainForm.stopActionUpdate(Sender: TObject);
begin
  stopAction.Enabled := Assigned(se) and (se.IsActive);
end;

procedure TMainForm.BrowseMqActionUpdate(Sender: TObject);
begin
  BrowseMqAction.Enabled := (se.mmqqMqInterface.MQServerOK or se.mmqqMqInterface.MQClientOK);
end;

procedure TMainForm.BrowseMqActionExecute(Sender: TObject);
var
  xMqInterface: TMqInterface;
begin
  Application.CreateForm(TMqBrowseForm, MqBrowseForm);
  try
    MqBrowseForm.GetQueueEdit.Items.Text := QueueNameList.Text;
    MqBrowseForm.ShowModal;
    if MqBrowseForm.ModalResult = mrOk then
    begin
      QueueNameList.Add(MqBrowseForm.GetQueueEdit.Text);
      LogTabControl.TabIndex := Ord (spMessages);
      XmlUtil.PushCursor (crHourGlass);
      try
        xMqInterface := TMqInterface.Create;
        try
          xMqInterface.Use := se.mqUse;
          xMqInterface.QManager := MqBrowseForm.GetManagerEdit.Text;
          xMqInterface.GetQueue := MqBrowseForm.GetQueueEdit.Text;
          xMqInterface.Browse(LogMqMessage, nil, False, False);
          QueueNameList.Add(MqBrowseForm.GetQueueEdit.Text);
        finally
          FreeAndNil(xMqInterface);
        end;
      finally
        XmlUtil.PopCursor;
      end;
    end;
  finally
    FreeAndNil(MqBrowseForm);
  end;
end;

procedure TMainForm.LogMqMessage(Sender: TObject; aHeader, aBody: String;
  aRfhHeader: AnsiString; MsgType: MQLONG; MsgDesc: MQMD; MqReturnCode: String);
  function _MessageTimeStamp: TDateTime;
  var
    sPutDate: String;
    sPutTime: String;
    yy, mm, dd, hh, mn, ss, pp: Word;
  begin
    sPutDate := MsgDesc.PutDate;
    yy := StrToIntDef(Copy(sPutDate, 1, 4), 0);
    mm := StrToIntDef(Copy(sPutDate, 5, 2), 0);
    dd := StrToIntDef(Copy(sPutDate, 7, 2), 0);
    sPutTime := MsgDesc.PutTime;
    hh := StrToIntDef(Copy(sPutTime, 1, 2), 0);
    mn := StrToIntDef(Copy(sPutTime, 3, 2), 0);
    ss := StrToIntDef(Copy(sPutTime, 5, 2), 0);
    pp := StrToIntDef(Copy(sPutTime, 7, 2), 0);
    result := EncodeDate(yy, mm, dd) + EncodeTime(hh, mn, ss, pp * 10);
  end;

var
  xLogItem: TLog;
  xIsRequest: Boolean;
begin
  xIsRequest := False; //avoid warning
  AcquireLock;
  Inc(se.mqCurWorkingThreads);
  try
    try
      try
        xLogItem := TLog.Create;
        try
          xLogItem.InboundTimeStamp := _MessageTimeStamp;
        except
        end;
        xLogItem.TransportType := ttMQ;
        xLogItem.RequestHeaders := aHeader;
        xLogItem.RequestBody := aBody;
        xLogItem.Exception := MqReturnCode;
        if se.Wsdls.Count > 0 then
        begin
          try
            se.FindRequestReply(xLogItem, '', aBody, xIsRequest);
            if not xIsRequest then
            begin
              xLogItem.RequestBody := '';
              xLogItem.ReplyBody := aBody;
            end;
          except
          end;
        end;
      except
        on E: Exception do
        begin
          xLogItem.Exception := E.Message;
          LogServerException(E.Message, True, e);
        end;
      end;
    finally
      try
        xLogItem.InitDisplayedColumns(xLogItem.Operation, se.DisplayedLogColumns);
        se.DisplayLog('', xLogItem);
      finally
      end;
    end;
  finally
    Dec(se.mqCurWorkingThreads);
    ReleaseLock;
  end;
end;

procedure TMainForm.GridViewFocusedNode(aNode: PVirtualNode);
begin
  if (toMultiSelect in GridView.TreeOptions.SelectionOptions) then
  begin
    GridView.TreeOptions.SelectionOptions :=
      GridView.TreeOptions.SelectionOptions - [toMultiSelect];
    GridView.Selected[aNode] := True;
    GridView.TreeOptions.SelectionOptions :=
      GridView.TreeOptions.SelectionOptions + [toMultiSelect];
  end;
  GridView.Selected[aNode] := True;
  GridView.FocusedNode := aNode;
end;

procedure TMainForm.CopySwiftdatatoclipboardMenuItemClick(Sender: TObject);
begin
  with TStwiftMtStreamer.Create(NodeToBind(InWsdlTreeView,
      InWsdlTreeView.FocusedNode) as TXml) do
    try
      ClipBoard.AsText := AsText;
    finally
      Free;
    end;
end;

procedure TMainForm.NeedMqInterfaceCaption(aSender, aObject: TObject;
  var aCaption: String);
begin
  if (aObject is TMqInterface) then
    aCaption := (aObject as TMqInterface).QManager + '/' +
      (aObject as TMqInterface).GetQueue;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  ret: Word;
  xXml: TXml;
begin
  EndEdit;
  se.Activate(False);
  CheckBoxClick(nil);
  CanClose := OkToOpenStubCase;
  if not CanClose then
    exit;
  if isOptionsChanged then
  begin
    ret := MessageDlg('Save changes to options?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if (ret = mrYes) then
    begin
      with TFormIniFile.Create(self, False) do
      try
        BooleanByName['doViaProxyServer'] := se.doViaProxyServer;
        BooleanByName['doScrollMessagesIntoView'] :=
          doScrollMessagesIntoView;
        BooleanByName['doScrollExceptionsIntoView'] :=
          doScrollExceptionsIntoView;
        BooleanByName['doConfirmRemovals'] := xmlUtil.doConfirmRemovals;
        // BooleanByName ['HTTPServer.KeepAlive'] := se.HTTPServer.KeepAlive;
        IntegerByName['HTTPServer.ListenQueue'] :=
          se.HTTPServer.ListenQueue;
        IntegerByName['HTTPServer.MaxConnections'] :=
          se.HTTPServer.MaxConnections;
        StringByName['ViaProxyServer'] := se.ViaProxyServer;
        StringByName['ViaProxyPort'] := IntToStr(se.ViaProxyPort);
        StringByName['mqUse'] := IntToStr(Ord(se.mqUse));
        IntegerByName['MaxWorkingThreads'] := se.mqMaxWorkingThreads;
        StringByName['mqServerEnv'] := mqServerEnv;
        IntegerByName['CompareLogOrderBy'] := Ord(se.CompareLogOrderBy);
        IntegerByName['ShowLogCobolStyle'] := Ord(se.ShowLogCobolStyle);
        StringByName['ElementsWhenRepeatable'] := IntToStr
          (xsdElementsWhenRepeatable);
        BooleanByName['doValidateScriptAssignmentAgainstSchema'] :=
          xsdValidateAssignmentsAgainstSchema;
        IntegerByName['bgCorrelationItemColor'] := bgCorrelationItemColor;
        IntegerByName['bgExpectedValueColor'] := bgExpectedValueColor;
        IntegerByName['bgNilValueColor'] := bgNilValueColor;
        xXml := OptionsAsXml;
        try
          StringByName['Options'] := xXml.Text;
        finally
          xXml.Free;
        end;
      finally
        Free;
      end;
    end;
    if (ret = mrCancel) then
      CanClose := False
    else
      NewStubCaseActionExecute(nil);
  end;
end;

procedure TMainForm.DebugOperationViewAsXml;
begin
  xmlUtil.ViewAsXml(se.DebugOperation.rpyBind, False);
end;

procedure TMainForm.DebugBeforeScriptActionExecute(Sender: TObject);
begin
  ShowInfoForm('debug Before', WsdlOperation.BeforeActivatorDebugString);
end;

procedure TMainForm.DebugOperation;
begin
  // ReleaseLock;
  // SetForegroundWindow(Application.Handle);
  // xmlUtil.ViewAsXml(fDebugOperation.rpyBind, False);
  // AcquireLock;
  with TIdSync.Create do
  begin
    try
      SynchronizeMethod(DebugOperationViewAsXml);
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.MessagesToDiskActionUpdate(Sender: TObject);
begin
  MessagesToDiskAction.Enabled := not se.IsActive;
end;

procedure TMainForm.MessagesToDiskActionExecute(Sender: TObject);
begin
  EndEdit;
  Application.CreateForm(TmessagesToDiskForm, messagesToDiskForm);
  try
    messagesToDiskForm.ShowModal;
    if messagesToDiskForm.ModalResult = mrOk then
    begin
      saveToDiskDirectory := messagesToDiskForm.DirectoryEdit.Text;
      saveToDiskExtention := messagesToDiskForm.ExtentionEdit.Text;
      saveToDiskSeparator := messagesToDiskForm.SeparatorEdit.Text;
      TProcedureThread.Create(False, True, se, doSaveMessagesToDisk);
    end;
  finally
    FreeAndNil(messagesToDiskForm);
  end;
end;

procedure TMainForm.doSaveLogRepliesToDisk;
var
  xNode: PVirtualNode;
  xLog: TLog;
  sl: TStringList;
begin
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := se.displayedLogs.Count + 20;
    ProgressBar.Position := 0;
    try
      sl := TStringList.Create;
      try
        xNode := MessagesVTS.GetFirst;
        while Assigned(xNode) do
        begin
          ProgressBar.Position := ProgressBar.Position + 1;
          if MessagesVTS.Selected[xNode] then
          begin
            xLog := NodeToMsgLog(False,MessagesVTS, xNode);
            if Assigned(xLog) then
              sl.Add(xLog.ReplyBody);
          end;
          xNode := MessagesVTS.GetNext(xNode);
        end;
        sl.SaveToFile(saveToDiskFileName);
      finally
        sl.Free;
      end;
    finally
      XmlUtil.PopCursor;
      ProgressBar.Position := 0;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.doSaveLogRequestsToDisk;
var
  xNode: PVirtualNode;
  xLog: TLog;
  sl: TStringList;
begin
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := se.displayedLogs.Count + 20;
    ProgressBar.Position := 0;
    try
      sl := TStringList.Create;
      try
        xNode := MessagesVTS.GetFirst;
        while Assigned(xNode) do
        begin
          ProgressBar.Position := ProgressBar.Position + 1;
          if MessagesVTS.Selected[xNode] then
          begin
            xLog := NodeToMsgLog(False,MessagesVTS, xNode);
            if Assigned(xLog) then
              sl.Add(xLog.RequestBody);
          end;
          xNode := MessagesVTS.GetNext(xNode);
        end;
        sl.SaveToFile(saveToDiskFileName);
      finally
        sl.Free;
      end;
    finally
      XmlUtil.PopCursor;
      ProgressBar.Position := 0;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.doSaveMessagesToDisk;
var
  X: Integer;
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
  xFileName, xSeparator, xMsgString: String;
begin
  xMessage := nil; //avoid warning
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := WsdlOperation.Messages.Count;
    ProgressBar.Position := 0;
    abortPressed := False;
    AbortAction.Enabled := True;
    CheckBoxClick(nil);
  finally
    ReleaseLock;
  end;
  try
    xNode := GridView.GetFirst;
    while Assigned(xNode) do
    begin
      if abortPressed then
        Break;
      AcquireLock;
      try
        ProgressBar.Position := ProgressBar.Position + 1;
        if GridView.Selected[xNode] then
        begin
          NodeToMessage(GridView, xNode, xMessage);
          if Assigned(xMessage) then
          begin
            try
              xFileName := saveToDiskDirectory + DirectorySeparator;
              xSeparator := '';
              for X := 0 to xMessage.CorrelationBindables.Count - 1 do
              begin
                xFileName := xFileName + xSeparator +
                  xMessage.CorrelationBindables.Bindables[X].CorrelationValue;
                xSeparator := saveToDiskSeparator;
              end;
              if saveToDiskExtention <> '' then
                xFileName := xFileName + '.' + saveToDiskExtention;
              if WsdlOperation.StubAction = saRequest then
              begin
                if WsdlOperation.reqBind is TIpmItem then
                begin
                  WsdlOperation.reqIpm.LoadValues(xMessage.reqIpm);
                  WsdlOperation.ExecuteBefore;
                  WsdlOperation.ExecuteReqStampers;
                  xMsgString := WsdlOperation.reqIpm.ValuesToBuffer(nil)
                end
                else
                begin (WsdlOperation.reqBind as TXml)
                  .ResetValues; (WsdlOperation.reqBind as TXml)
                  .LoadValues(xMessage.reqBind as TXml, True, True);
                  WsdlOperation.ExecuteBefore;
                  WsdlOperation.ExecuteReqStampers;
                  xMsgString := WsdlOperation.StreamRequest(_progName, True,
                    True, True);
                end;
              end
              else
              begin
                if WsdlOperation.rpyBind is TIpmItem then
                begin
                  WsdlOperation.rpyIpm.LoadValues(xMessage.rpyIpm);
                  WsdlOperation.ExecuteBefore;
                  WsdlOperation.ExecuteRpyStampers;
                  xMsgString := WsdlOperation.rpyIpm.ValuesToBuffer(nil)
                end
                else
                begin (WsdlOperation.rpyBind as TXml)
                  .ResetValues; (WsdlOperation.rpyBind as TXml)
                  .LoadValues(xMessage.rpyBind as TXml, True, True);
                  WsdlOperation.ExecuteBefore;
                  WsdlOperation.ExecuteRpyStampers;
                  xMsgString := WsdlOperation.StreamReply(_progName, True);
                end;
              end;
              SaveStringToFile(xFileName, xMsgString);
            except
              on E: Exception do
                LogServerException(Format(
                    'Exception %s in SaveMessagesToDisk. Exception is:"%s".',
                    [E.ClassName, E.Message]), True, E);
            end;
          end;
        end;
      finally
        ReleaseLock;
      end;
      xNode := GridView.GetNext(xNode);
    end;
  finally
    AcquireLock;
    try
      CheckBoxClick(nil);
      AbortAction.Enabled := False;
      abortPressed := False;
      ExecuteAllRequestsToolButton.Down := False;
      ProgressBar.Position := 0;
      XmlUtil.PopCursor;
    finally
      ReleaseLock;
    end;
  end;
end;

procedure TMainForm.DownPageControlChange(Sender: TObject);
begin
  if LogTabControl.TabIndex = Ord (spMessages) then
  begin
    LogTabControl.Tabs[Ord (spMessages)] := MessagesTabCaption; // remove asterix since exceptions can be viewed now
  end;
  if LogTabControl.TabIndex = Ord (spNotifications) then
  begin
    LogTabControl.Tabs[Ord (spNotifications)] := notifyTabCaption; // remove asterix since exceptions can be viewed now
  end;
end;

procedure TMainForm.doReadMessagesFromDisk;
var
  f: Integer;
  xMessage: TWsdlMessage;
  xFileName, xMsgString: String;
  xPatterns: TStringList;
  nErrors: Integer;
  xXml: TXml;
  mNode: PVirtualNode;
  mData: PMessageTreeRec;
begin
  nErrors := 0;
  XmlUtil.PushCursor (crHourGlass);
  ProgressBar.Min := 0;
  ProgressBar.Position := 0;
  ProgressBar.Max := FileNameList.Count;
  ProgressBar.Position := 0;
  abortPressed := False;
  AbortAction.Enabled := True;
  CheckBoxClick(nil);
  xPatterns := TStringList.Create;
  try
    try
      for f := 0 to FileNameList.Count - 1 do
      begin
        if abortPressed then
          Break;
        WsdlOperation.AcquireLock;
        try
          ProgressBar.Position := ProgressBar.Position + 1;
          try
            xFileName := ExtractFileName(FileNameList.Strings[f]);
            xFileName := Copy(xFileName, 1,
              Length(xFileName) - Length
                (ExtractFileExt(FileNameList.Strings[f])));
            if (ExplodeStr(xFileName, saveToDiskSeparator[1], xPatterns, True,
                False) <> WsdlOperation.CorrelationBindables.Count) then
              raise Exception.Create('Filename: ' + xFileName +
                  ' does not fit correlation');
            xMsgString := ReadStringFromFile(FileNameList.Strings[f]);
            if WsdlOperation.StubAction = saRequest then
            begin
              xMessage := TWsdlMessage.CreateRequest(WsdlOperation,
                'Request' + IntToStr(WsdlOperation.Messages.Count),
                xPatterns.Text,
                xsdNowAsDateTime + '  Read from ' + FileNameList.Strings[f]);
              if xMessage.reqBind is TIpmItem then
  (xMessage.reqBind as TIpmItem)
                .BufferToValues(FoundErrorInBuffer, xMsgString)
              else
              begin
                xXml := TXml.Create;
                try
                  xXml.LoadFromString(xMsgString, nil);
                  WsdlOperation.SoapXmlRequestToBindables(xXml, False);
(xMessage.reqBind as TXml)
                  .Reset; (xMessage.reqBind as TXml)
                  .LoadValues((WsdlOperation.reqBind as TXml), True, True);
                finally
                  xXml.Free;
                end;
              end;
            end
            else
            begin
              xMessage := TWsdlMessage.CreateReply(WsdlOperation,
                'Reply' + IntToStr(WsdlOperation.Messages.Count),
                xPatterns.Text,
                xsdNowAsDateTime + '  Read from ' + FileNameList.Strings[f]);
              if xMessage.rpyBind is TIpmItem then
  (xMessage.rpyBind as TIpmItem)
                .BufferToValues(FoundErrorInBuffer, xMsgString)
              else
              begin
                xXml := TXml.Create;
                try
                  xXml.LoadFromString(xMsgString, nil);
                  WsdlOperation.SoapXmlReplyToBindables(xXml, False);
                  (xMessage.rpyBind as TXml).Reset;
                  (xMessage.rpyBind as TXml).LoadValues((WsdlOperation.rpyBind as TXml), True, True);
                finally
                  xXml.Free;
                end;
              end;
            end;
            se.UpdateMessageRow(WsdlOperation, xMessage);
            mNode := GridView.AddChild(nil);
            mData := GridView.GetNodeData(mNode);
            mData.Message := xMessage;
            stubChanged := True;
          except
            on E: Exception do
            begin
              LogServerException(E.Message, False, nil);
              Inc(nErrors);
            end;
          end;
        finally
          WsdlOperation.ReleaseLock;
        end;
      end;
    finally
      GridView.Invalidate;
      CheckBoxClick(nil);
      AbortAction.Enabled := False;
      abortPressed := False;
      ExecuteAllRequestsToolButton.Down := False;
      ProgressBar.Position := 0;
      XmlUtil.PopCursor;
    end;
  finally
    xPatterns.Free;
  end;
end;

procedure TMainForm.doDisableMessages;
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
  WsdlOperation.AcquireLock;
  xMessage := nil; //avoid warning
  try
    xNode := GridView.GetFirstSelected;
    while Assigned(xNode) do
    begin
      NodeToMessage(GridView, xNode, xMessage);
      if Assigned(xMessage) and (xNode <> GridView.GetFirst) then
      // can not disable the default message
        xMessage.Disabled := True;
      xNode := GridView.GetNextSelected(xNode);
    end;
    GridView.InvalidateColumn(0);
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.doRevalidateMessages;
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
  xMessage := nil; //avoid warning
  WsdlOperation.AcquireLock;
  try
    xNode := GridView.GetFirstSelected;
    while Assigned(xNode) do
    begin
      NodeToMessage(GridView, xNode, xMessage);
      if Assigned(xMessage) then
        xMessage.Disabled := False;
      xNode := GridView.GetNextSelected(xNode);
    end;
    GridView.InvalidateColumn(0);
  finally
    WsdlOperation.ReleaseLock;
  end;
end;

procedure TMainForm.PasteGridFromPasteBoard;
  function TabSepLineToStringGrid(aLine: String): TStringList;
  var
    c: Integer;
    col: String;
  begin
    result := TStringList.Create;
    col := '';
    for c := 1 to Length(aLine) do
    begin
      if aLine[c] = #9 then
      begin
        result.Add(col);
        col := '';
      end
      else
        col := col + aLine[c];
    end;
    if Length(aLine) > 0 then
      result.Add(col);
  end;

var
  copyLines, copyColumns: TStringList;
  l, c: Integer;
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
begin
  copyLines := TStringList.Create;
  try
    copyLines.Text := ClipBoard.AsText;
    // first check if first line is columnheader line
    copyColumns := TabSepLineToStringGrid(copyLines.Strings[0]);
    c := 0;
    while (c < copyColumns.Count) and (c < GridView.Header.Columns.Count) do
    begin
      if (copyColumns.Strings[c] <> GridView.Header.Columns.Items[c].Text) then
        raise Exception.Create('Columnheaders do not match, Operation aborted');
      Inc(c);
    end;

    l := 1; // line zero contains columnheaders
    while (l < copyLines.Count) do
    begin
      if l <= WsdlOperation.Messages.Count then
        xMessage := WsdlOperation.Messages.Messages[l - 1]
      else
        NodeToMessage(GridView, AddMessage(GridView.GetFirst), xMessage);
      copyColumns := TabSepLineToStringGrid(copyLines.Strings[l]);
      try
        c := 0 + NScripts;
        while (c < copyColumns.Count) and (c < GridView.Header.Columns.Count) do
        begin
          // OnNewText (aVst, xNode, c, copyColumns.Strings[c]);
          if c = 0 + NScripts then
          begin
            if copyColumns.Strings[c] <> xMessage.Name then
            begin
              if l = 1 then
                Raise Exception.Create(
                  'Not allowed to change this reply name into ' +
                    copyColumns.Strings[c])
              else
              begin
                xMessage.Name := copyColumns.Strings[c];
                stubChanged := True;
              end;
            end;
          end
          else
          begin
            if c <= xMessage.CorrelationBindables.Count + NScripts then
            begin
              if c < copyColumns.Count then
              begin
                if copyColumns.Strings[c] <> xMessage.CorrelationBindables.Bindables[c - 1 - NScripts]
                  .CorrelationValue then
                begin
                  if l = 1 then
                    raise Exception.Create(
                      'Not allowed to change this pattern into ' +
                        copyColumns.Strings[c])
                  else
                  begin
                    xMessage.CorrelationBindables.Bindables[c - 1 - NScripts].CorrelationValue :=
                      copyColumns.Strings[c];
                    stubChanged := True;
                  end;
                end;
              end;
            end
            else
            begin
              if (copyColumns.Strings[c] <> '?')
              and (Assigned(xMessage.ColumnXmls.Bindables
                    [c - NScripts - xMessage.CorrelationBindables.Count - 1])) then
              begin
                xBind := xMessage.ColumnXmls.Bindables [c - NScripts - xMessage.CorrelationBindables.Count - 1];
                if copyColumns.Strings[c] <> '&nil' then
                begin
                  if (copyColumns.Strings[c] <> xBind.Value) or
                    (not xBind.CheckedAllUp) then
                  begin
                    xBind.Value := copyColumns.Strings[c];
                    xBind.Checked := True;
                    stubChanged := True;
                  end;
                end
                else
                begin
                  if xBind.Checked then
                  begin
                    xBind.Checked := False;
                    stubChanged := True;
                  end;
                end;
              end;
            end;
          end;
          //
          Inc(c);
        end;
      finally
        FreeAndNil(copyColumns);
      end;
      Inc(l);
    end;
  finally
    FreeAndNil(copyLines);
  end;
end;

procedure TMainForm.Log2DesignActionExecute(Sender: TObject);
var
  ok, nok: Integer;
  xNode, mNode: PVirtualNode;
  xLog: TLog;
  xMessage: TWsdlMessage;
  xXml: TXml;
  mData: PMessageTreeRec;
  xNotify: String;
begin
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ok := 0;
    nok := 0;
    xNode := MessagesVTS.GetFirst;
    while Assigned(xNode) do
    begin
      if MessagesVTS.Selected[xNode] then
      begin
        xLog := NodeToMsgLog(False,MessagesVTS, xNode);
        if Assigned(xLog) and Assigned(xLog.Operation) and
          (xLog.Exception = '') then
        begin
          xLog.toBindables(xLog.Operation);
          if xLog.Operation.StubAction = saRequest then
          begin
            xMessage := TWsdlMessage.CreateRequest(xLog.Operation,
              'Request' + IntToStr(xLog.Operation.Messages.Count),
              xLog.Operation.CorrelationBindables.ValueText,
              'Logged at ' + DateTimeToStr(xLog.InboundTimeStamp));
          end
          else
          begin
            xMessage := TWsdlMessage.CreateReply(xLog.Operation,
              'Reply' + IntToStr(xLog.Operation.Messages.Count),
              xLog.Operation.CorrelationBindables.ValueText,
              'Logged at ' + DateTimeToStr(xLog.InboundTimeStamp));
          end;
          if xLog.Operation.reqBind is TIpmItem then
          begin
            (xMessage.reqBind as TIpmItem).BufferToValues(FoundErrorInBuffer, xLog.RequestBody);
            (xMessage.rpyBind as TIpmItem).BufferToValues(FoundErrorInBuffer, xLog.ReplyBody);
          end
          else
          begin
            try
              xMessage.reqXml.Reset;
              xMessage.reqXml.LoadValues(xLog.Operation.reqXml, True, True);
            except
            end;
            try
              xMessage.rpyXml.Reset;
              xMessage.rpyXml.LoadValues(xLog.Operation.rpyXml, True, True);
            except
            end;
          end;
          se.UpdateMessageRow(xLog.Operation, xMessage);
          if xLog.Operation = WsdlOperation then
          begin
            mNode := GridView.AddChild(nil);
            mData := GridView.GetNodeData(mNode);
            mData.Message := xMessage;
          end;
          xLog.Mssg := xMessage;
          Inc(ok);
          stubChanged := True;
        end
        else
          Inc(nok);
      end;
      xNode := MessagesVTS.GetNext(xNode);
    end;
  finally
    ReleaseLock;
    XmlUtil.PopCursor;
  end;
  case ok of
    1: xNotify := 'Added 1 message';
  else
    xNotify := 'Added ' + IntToStr(ok) + ' messages';
  end;
  case nok of
    0: ;
    1: xNotify := xNotify + '; 1 message was not added';
  else
    xNotify := xNotify + '; ' + IntToStr(nok) + ' messages were not added';
  end;
  ShowMessage(xNotify);
end;

procedure TMainForm.CoverageReport(aList: TLogList);
  procedure _updateIgnoreCoverageOn(xXml: TXmlCvrg; sl: TStringList);
  var
    X: Integer;
  begin
    if xXml.Ignore then
      sl.Add(xXml.FullUQCaption)
    else
      for X := 0 to xXml.Items.Count - 1 do
        _updateIgnoreCoverageOn(xXml.Items.XmlItems[X] as TXmlCvrg, sl);
  end;

var
  xXml: TXmlCvrg;
  xForm: TShowXmlCoverageForm;
begin
  XmlUtil.PushCursor(crHourGlass);
  try
    xXml := aList.PrepareCoverageReportAsXml ( allAliasses
                                             , se.ignoreCoverageOn
                                             );
    try
      Application.CreateForm(TShowXmlCoverageForm, xForm);
      try
        xForm.Caption := '' + _progName + ' - Coverage report';
        xForm.Bind := xXml;
        xForm.initialExpandStyle := esOne;
        xForm.ShowModal;
        if xForm.Changed then
        begin
          if BooleanPromptDialog('Accept changes to Coverage report settings') then
          begin
            stubChanged := True;
            se.ignoreCoverageOn.Clear;
            _updateIgnoreCoverageOn(xXml, se.ignoreCoverageOn);
          end;
        end;
      finally
        xForm.Free;
      end;
    finally
      FreeAndNil(xXml);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;


procedure TMainForm.LogCoverageReportActionExecute(Sender: TObject);
var
  xLogList: TLogList;
  xForm: TShowXmlCoverageForm;
  x: Integer;
begin
  OnlyWhenLicensed;
  xLogList := TLogList.Create;
  XmlUtil.PushCursor (crHourGlass);
  try
    AcquireLock;
    try
      for x := 0 to se.displayedLogs.Count -1 do
        xLogList.AddObject('', se.displayedLogs.LogItems[x]);
    finally
      ReleaseLock;
    end;
    CoverageReport(xLogList);
  finally
    xLogList.Clear;
    xLogList.Free;
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.LogCoverageReportActionUpdate(Sender: TObject);
begin
  LogCoverageReportAction.Enabled := Assigned(WsdlOperation);
end;

procedure TMainForm.LogDisplayedColumnsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.ProjectOptionsLogDisplayedColumnsAsXml;
  if EditXmlXsdBased('Additional log columns'
                    ,'projectOptions.Log.DisplayedColumns'
                    ,'DisplayedColumns.DisplayedColumn.Header'
                    , ''
                    , False
                    , False
                    , esUsed
                    , projectOptionsXsd
                    , xXml
                    ) then
  begin
    AcquireLock;
    try
      stubChanged := True;
      se.ProjectOptionsLogDisplayedColumnsFromXml(xXml);
      LogUpdateColumns;
    finally
      ReleaseLock;
    end;
  end;
end;

procedure TMainForm.LogDisplayedColumnsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Displayed columns';
end;

procedure TMainForm.ViewMssgAsTextActionUpdate(Sender: TObject);
begin
  ViewMssgAsTextAction.Enabled := Assigned(WsdlMessage);
end;

procedure TMainForm.ViewMssgAsTextActionExecute(Sender: TObject);
var
  xMessage: String;
begin
  EndEdit;
  if not Assigned(WsdlMessage) then
    raise Exception.Create('No message selected');
  if WsdlOperation.StubAction = saRequest then
  begin
    WsdlOperation.AcquireLock;
    try
      if WsdlOperation.reqBind is TXml then with WsdlOperation.reqBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlMessage.reqBind as TXml), False, True);
        xMessage := WsdlOperation.StreamRequest(_progName, True, True, True);
      end
      else
      begin
        xMessage := (WsdlMessage.reqBind as TIpmItem).ValuesToBuffer(nil);
      end;
    finally
      WsdlOperation.ReleaseLock;
    end;
    ShowInfoForm('Request', xMessage);
  end
  else
  begin
    try
      WsdlOperation.AcquireLock;
      if WsdlOperation.rpyBind is TXml then with WsdlOperation.rpyBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlMessage.rpyBind as TXml), False, True);
        (WsdlOperation.fltBind as TXml).ResetValues;
        (WsdlOperation.fltBind as TXml).LoadValues((WsdlMessage.fltBind as TXml), False, True);
        if WsdlMessage.fltBind.Checked then
          xMessage := WsdlOperation.StreamFault(_progName, True)
        else
          xMessage := WsdlOperation.StreamReply(_progName, True);
      end
      else
      begin
        xMessage := (WsdlMessage.rpyBind as TIpmItem).ValuesToBuffer(nil);
      end;
    finally
      WsdlOperation.ReleaseLock;
    end;
    ShowInfoForm('Reply', xMessage);
  end;
end;

procedure TMainForm.CopyCobolDataToClipboardMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := (NodeToBind(InWsdlTreeView,
      InWsdlTreeView.FocusedNode) as TIpmItem).ValuesToBuffer(nil);
end;

procedure TMainForm .ShowLogDifferencesActionExecute (Sender : TObject );
var
  fNode, nNode: PVirtualNode;
  fLog, nLog: TLog;
  fXml, nXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  nNode := nil;
  fNode := MessagesVTS.GetFirstSelected;
  nNode := MessagesVTS.GetNextSelected(fNode);
  if Assigned (fNode)
  and Assigned (nNode) then
  begin
    fLog := NodeToMsgLog(True,MessagesVTS, fNode);
    nLog := NodeToMsgLog(True,MessagesVTS, nNode);
    try
      fXml := TXml.CreateAsString('firstSelected', '');
      with fXml do
      begin
        AddXml (TXml.CreateAsTimeStamp('inboundTimestamp', fLog.InboundTimeStamp));
        AddXml (TXml.CreateAsTimeStamp('outboundTimestamp', fLog.OutboundTimeStamp));
        with AddXml (TXml.CreateAsString('Req', '')) do
          AddXml (fLog.reqBodyAsXml);
        with AddXml (TXml.CreateAsString('Rpy', '')) do
          AddXml (fLog.rpyBodyAsXml);
        fXml.SeparateNsPrefixes;
      end;
      nXml := TXml.CreateAsString('nextSelected', '');
      with nXml do
      begin
        AddXml (TXml.CreateAsTimeStamp('inboundTimestamp', nLog.InboundTimeStamp));
        AddXml (TXml.CreateAsTimeStamp('outboundTimestamp', nLog.OutboundTimeStamp));
        with AddXml (TXml.CreateAsString('Req', '')) do
          AddXml (nLog.reqBodyAsXml);
        with AddXml (TXml.CreateAsString('Rpy', '')) do
          AddXml (nLog.rpyBodyAsXml);
        nXml.SeparateNsPrefixes;
      end;
    finally
      fLog.Disclaim;
      nLog.Disclaim;
    end;
    a2bInitialize;
    try
      xA2B := TA2BXml.CreateA2B('', fXml, nXml, Nil);
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

procedure TMainForm.ShowShortCutActionsActionExecute (Sender : TObject );
var
  sl: TStringList;
  x: Integer;
  xKey: Word;
  xShift : TShiftState;
  xKeyString: String;
begin
  sl := TStringList.Create;
  try
    for x := 0 to alGeneral.ActionCount - 1 do with alGeneral.Actions[x] as TCustomAction do
    begin
      ShortCutToKey(ShortCut, xKey, xShift);
      xKeyString := '';
      if (ssShift in xShift) then
        xKeyString := xKeyString + 'Shift+';
      if (ssCtrl in xShift) then
        xKeyString := xKeyString + 'Ctrl+';
      if (ssAlt in xShift) then
        xKeyString := xKeyString + 'Alt+';
      if xKey <> 0 then
      begin
        xKeyString := xKeyString + Copy (DbgsVKCode(xKey), 4, MaxInt);
        sl.Values[Caption] := xKeyString;
      end;
    end;
    ShowInfoForm('Actions with ShortCut', sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TMainForm.CleanMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if not(xBind is TXml) then
    raise Exception.Create('FocusedNode not an XML element');
  try
    WsdlOperation.AcquireLock;
    try
      (xBind as TXml).Clean(xsdElementsWhenRepeatable, xsdMaxDepthBillOfMaterials);
    finally
      WsdlOperation.ReleaseLock;
    end;
    UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    stubChanged := True;
  finally
    RevalidateXmlTreeView(InWsdlTreeView);
  end;
end;

procedure TMainForm.PasteCobolDataFromClipboardMenuItemClick(Sender: TObject);
begin (NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode) as TIpmItem)
  .BufferToValues(nil, ClipBoard.AsText);
  InWsdlTreeView.Invalidate;
end;

procedure TMainForm.ParserError(Sender: TObject;
  LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
begin
  Application.CreateForm(TErrorFoundDlg, ErrorFoundDlg);
  try
    ErrorFoundDlg.FileNameEdit.Text := _ParseFileName;
    ErrorFoundDlg.LineNumberEdit.Text := IntToStr(LineNumber);
    ErrorFoundDlg.ColumnNumberEdit.Text := IntToStr(ColumnNumber);
    ErrorFoundDlg.TokenStringEdit.Text := TokenString;
    ErrorFoundDlg.Viewer := 'Notepad';
    ErrorFoundDlg.ShowModal;
  finally
    FreeAndNil(ErrorFoundDlg);
  end;
end;

procedure TMainForm.ElementvalueMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
    try
      SelectXmlElementForm.doShowReq := True;
      SelectXmlElementForm.doShowRpy := True;
      SelectXmlElementForm.WsdlOperation := WsdlOperation;
      if xBind.IsExpression then
        SelectXmlElementForm.LastCaption := Trim(Copy(xBind.Value, 3, 30000))
      else
        SelectXmlElementForm.LastCaption := fLastCaption;
      SelectXmlElementForm.IncludeRecurring := True;
      SelectXmlElementForm.ShowModal;
      if SelectXmlElementForm.ModalResult = mrOk then
      begin
        fLastCaption := SelectXmlElementForm.SelectedCaption;
        xBind.Value := ':=' + fLastCaption;
        WsdlOperation.PrepareRpyStamper(xBind);
      end;
    finally
      FreeAndNil(SelectXmlElementForm);
    end;
  end;
end;

procedure TMainForm.AssignEvaluationMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    WsdlOperation.BindChecker(xBind);
    Application.CreateForm(TEditCheckerForm, EditCheckerForm);
    try
      EditCheckerForm.ScriptMemo.ReadOnly := False;
      EditCheckerForm.WsdlOperation := WsdlOperation;
      EditCheckerForm.Bindable := xBind;
      EditCheckerForm.ShowModal;
      if EditCheckerForm.ModalResult = mrOk then
      begin
        stubChanged := True;
      end;
    finally
      FreeAndNil(EditCheckerForm);
    end;
  end;
end;

procedure TMainForm.AssignExpressionMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    Application.CreateForm(TEditStamperForm, EditStamperForm);
    try
      EditStamperForm.ScriptMemo.ReadOnly := False;
      EditStamperForm.WsdlOperation := WsdlOperation;
      EditStamperForm.Bindable := xBind;
      EditStamperForm.ShowModal;
      if EditStamperForm.ModalResult = mrOk then
      begin
        stubChanged := True;
      end;
    finally
      FreeAndNil(EditStamperForm);
    end;
  end;
end;

procedure TMainForm.SelectExpectedElementsActionExecute(Sender: TObject);
begin
  if not InactiveAfterPrompt then Exit;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  SelectElementsForm.Caption :=
    'Maintain list of elements to check expected values';
  try
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    SelectElementsForm.ControlBinds := WsdlOperation.ExpectationBindables;
    SelectElementsForm.DuplicatesAllowed := False;
    SelectElementsForm.GroupAllowed := False;
    SelectElementsForm.ShowModal;
    // if SelectElementsForm.ModalResult = mrOk then
    begin
      se.UpdateReplyColumns(WsdlOperation);
      UpdateMessagesGrid;
      DoColorBindButtons;
      stubChanged := stubChanged or SelectElementsForm.stubChanged;
    end;
  finally
    FreeAndNil(SelectElementsForm);
  end;
end;

procedure TMainForm.SelectExpectedElementsActionUpdate(Sender: TObject);
begin
  SelectExpectedElementsAction.Enabled := Assigned(WsdlOperation);
end;

procedure TMainForm.ReportUnexpectedValuesActionUpdate(Sender: TObject);
begin
  ReportUnexpectedValuesAction.Enabled := { }{ not se.IsActive
    and{ } (se.Wsdls.Count > 0);
end;

procedure TMainForm.ReportUnexpectedValuesActionExecute(Sender: TObject);
var
  d, X, nDiffs: Integer;
  xXml: THtmlXml;
  tableXml: THtmlTableXml;
  xLog: TLog;
  xBind: TCustomBindable;
  showReqRep: Boolean;
begin
  OnlyWhenLicensed;
  xXml := htmlCreateXml(_ProgName, 'Unexpected values report');
  with htmlFindContentXml(xXml) do
  begin
    try
      XmlUtil.PushCursor (crHourGlass);
      try
        AddP;
        tableXml := AddTable.Border(1);
        with tableXml do
        begin
          with AddTr do
          begin
            AddTh.ColSpan(3).AddB('Messages with unexpected values');
            AddTh.ColSpan(3).AddB('_');
          end;
          with AddTr do
          begin
            AddTh.AddB('Row_');
            AddTh.AddB('Sent_');
            AddTh.AddB('Correlation_');
            AddTh.AddB('Tag_');
            AddTh.AddB('Value_');
            AddTh.AddB('Reference_');
          end;
          AcquireLock;
          try
            for X := 0 to se.displayedLogs.Count - 1 do
            begin
              xLog := se.displayedLogs.LogItems[X];
              xLog.toBindables(xLog.Operation);
              se.CheckExpectedValues(xLog, xLog.Operation, True);
              if xLog.HasUnexpectedValue then
              begin
                nDiffs := 0;
                for d := 0 to xLog.Operation.ExpectationBindables.Count - 1 do
                  if xLog.Operation.ExpectationBindables.Bindables[d].HasUnexpectedValue then
                    Inc(nDiffs);
                showReqRep := True;
                for d := 0 to xLog.Operation.ExpectationBindables.Count - 1 do
                begin
                  xBind := xLog.Operation.ExpectationBindables.Bindables[d];
                  if xBind.HasUnexpectedValue then
                  begin
                    with tableXml.AddTr.vtop do
                    begin
                      if showReqRep then
                      begin
                        AddTd.RowSpan(nDiffs).AddB(IntToStr(X + 1));
                        AddTd.RowSpan(nDiffs).AddB(FormatDateTime('hh:nn:ss', xLog.InboundTimeStamp));
                        AddTd.RowSpan(nDiffs).AddB(xLog.CorrelationId);
                        showReqRep := False;
                      end;
                      AddTd.AddB(xBind.FullIndexCaption);
                      AddTd.AddB(xBind.Value + '_');
                      AddTd.AddB(xBind.ExpectedValue + '_');
                    end; // with row xml
                  end; // if unexpected
                end; // for each expected value
              end; // with atleast a unexpected value
            end; // for each log
          finally
            ReleaseLock;
          end;
        end; // table
      finally
        XmlUtil.PopCursor;
      end;
      ShowHtml(_progName + ' - Unexpected values report', htmlXmlAsString (xXml, _wsdlStubStylesheet));
    finally
      xXml.Free;
    end;
  end;
end;

procedure TMainForm.ChangeXmlDataType(aXml: TXml; aDataType: TXsdDataType);
var
  nXml: TXml;
  aXmlAsText: String;
  x: Integer;
begin
  aXmlAsText := aXml.AsText(False, 0, True, False);
  if aXml.TypeDef <> aDataType then
  begin
    aXml.TypeDef := aDataType;
    aXml.XsdCreate(0, aXml.Xsd);
    nXml := TXml.Create;
    try
      nXml.LoadFromString(aXmlAsText, nil);
      for X := nXml.Attributes.Count - 1 downto 0 do
        if NameWithoutPrefix(nXml.Attributes.XmlAttributes[X].Name)
          = 'type' then
          nXml.Attributes.XmlAttributes[X].Value := aDataType.Name;
      aXml.LoadValues(nXml, False, False);
      stubChanged := True;
    finally
      nXml.Free;
    end;
  end;
end;

procedure TMainForm.ChangeDataTypeMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    ChangeXmlDataType(xBind as TXml, TXsdDataType((Sender as TMenuItem).Tag));
    UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    InWsdlTreeView.FocusedColumn := 0;
    InWsdlTreeView.Expanded[InWsdlTreeView.FocusedNode] := True;
    se.UpdateMessageRow(WsdlOperation, WsdlMessage);
    InWsdlTreeView.Invalidate;
    GridView.InvalidateNode(GridView.FocusedNode);
    InWsdlTreeViewFocusChanged(InWsdlTreeView, InWsdlTreeView.FocusedNode,
      InWsdlTreeView.FocusedColumn);
  end;
end;

procedure TMainForm.ServiceOptionsActionUpdate(Sender: TObject);
begin
  ServiceOptionsAction.Enabled := (WsdlServicesComboBox.ItemIndex > -1);
end;

procedure TMainForm.ServiceOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if WsdlServicesComboBox.ItemIndex < 0 then
    raise Exception.Create('No service selected');
  with Wsdl.Services.Services[WsdlServicesComboBox.ItemIndex] do
  begin
    xXml := OptionsAsXml;
    try
      if EditXmlXsdBased('Service Options', '', '', '', False, False, esUsed, serviceOptionsXsd, xXml) then
      begin
        AcquireLock;
        try
          stubChanged := True;
          OptionsFromXml(xXml);
        finally
          ReleaseLock;
        end;
      end;
    finally
      xXml.Free;
    end;
  end;
end;

procedure TMainForm.LogUsageTimerTimer(Sender: TObject);
begin
  try
    try
      if authorizationServerEndpoint <> '' then
        se.Licensed := GetAuthorization
      else
      begin
        try
          if OpenLogUsageDatabase then
            LogUsage(WindowsUserName);
        finally
          SqlConnector.Connected := False;
        end;
      end;
    except
    end;
  finally
    SetLogUsageTimer;
  end;
end;

procedure TMainForm.SetLogUsageTimer;
{ Lets the timer trigger just after midnight }
begin
  LogUsageTimer.Interval := Ceil(((SysUtils.Date + 1) - Now) * 24 * 60 * 60 * 1000);
  LogUsageTimer.Enabled := True;
end;

procedure TMainForm.CobolOperationsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.cobolOperationsXml;
  if EditXmlXsdBased ( 'Cobol Operations'
                     , 'OperationDefs.CobolOperations'
                     , 'CobolOperations.Operation.Name'
                     , 'CobolOperations.Operation.Name'
                     , se.IsActive
                     , xXml.Items.Count > 1
                     , esUsed
                     , OperationDefsXsd
                     , xXml
                     ) then
  begin
    AcquireLock;
    try
      stubChanged := True;
      se.cobolOperationsUpdate(xXml, se.projectFileName);
      PrepareOperation;
    finally
      ReleaseLock;
    end;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.CobolOperationsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of cobol operations ' + HttpActiveHint;
end;

procedure TMainForm.ConfigListenersActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := TXml.Create;
  try
    xXml.CopyDownLine(se.Listeners.SpecificationXml, True);
    if EditXmlXsdBased('Configure Listeners', '', '', '', se.IsActive, False, esUsed, listenersConfigXsd, xXml) then
    begin
      stubChanged := True;
      se.Listeners.SpecificationXml.CopyDownLine(xXml, True);
      se.Listeners.FromXml(se.HaveStompFrame);
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.ConfigListenersActionUpdate(Sender: TObject);
begin
  ConfigListenersAction.Enabled := (Assigned(listenersConfigXsd));

end;

procedure TMainForm.ConfigLogActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.ProjectLogOptionsAsXml;
  try
    if EditXmlXsdBased('Project log options', 'projectOptions.Log',
      'Log.maxEntries', '', False, False, esUsed, projectOptionsXsd, xXml) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        se.ProjectLogOptionsFromXml(xXml);
      finally
        ReleaseLock;
      end;
      LogUpdateColumns;
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.readLog4jEventsActionExecute(Sender: TObject);
var
  xLogList: TLogList;
begin
  OpenFileDialog.DefaultExt := 'xml';
  OpenFileDialog.FileName := log4jEventsFileName;
  OpenFileDialog.Filter := 'XML file (*.xml)|*.xml';
  OpenFileDialog.Title := 'Read Log4J_Events from file';
  if OpenFileDialog.Execute then
  begin
    log4jEventsFileName := OpenFileDialog.FileName;
    xLogList := TLogList.Create;
    try
      OpenLog4jEvents(log4jEventsFileName, True, xLogList);
      try
        ToAllLogList(xLogList);
        MessagesTabControl.TabIndex := Ord (slRequestBody);
      except
        xLogList.Clear;
        raise ;
      end;
    finally
      xLogList.Clear;
      FreeAndNil(xLogList);
    end;
  end;
end;

procedure TMainForm.readLog4jEventsActionUpdate(Sender: TObject);
begin
  readLog4jEventsAction.Enabled := (se.Wsdls.Count > 0);
end;

procedure TMainForm.readLog4jEventsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (se.Wsdls.Count = 0) then
    HintStr := HintStr + ' (no WSDLS read)';
end;

procedure TMainForm.OpenLog4jEvents(aString: String; aIsFileName: Boolean;
  aLogList: TLogList);
  procedure _DiscoverOperation(aXml: TXml; aLog: TLog; var aReqXml, aRpyXml: TXml);
    function _DiscoverOperationFromXml(aXml: TXml; aLog: TLog; var aReqXml, aRpyXml: TXml): Boolean;
    var
      f: Integer;
    begin
      result := False;
      if allOperations.Find(aXml.Name + ';' + aXml.NameSpace, f) then
      begin
        result := True;
        aLog.Operation := allOperations.Operations[f];
        aReqXml := aXml;
      end;
      if allOperationsRpy.Find(aXml.Name, f) then
      begin
        result := True;
        aLog.Operation := allOperationsRpy.Operations[f];
        aRpyXml := aXml;
      end;
    end;
    procedure _DiscoverOperationFromXmlValue(aXml: TXml; aLog: TLog; var aReqXml, aRpyXml: TXml);
    var
      xOperation: TWsdlOperation;
    begin
      try
        xOperation := se.FindOperationOnRequest(nil, '', aXml.Value, False);
      Except
        xOperation := nil;
      End;
      if Assigned(xOperation) then
      begin
        aLog.Operation := xOperation;
        aReqXml := aXml;
      end;
      try
        xOperation := se.FindOperationOnReply(aXml.Value);
      Except
        xOperation := nil;
      End;
      if Assigned(xOperation) then
      begin
        aLog.Operation := xOperation;
        aRpyXml := aXml;
      end;
    end;

  var
    X: Integer;
  begin
    aLog.Operation := nil;
    if aXml.Items.Count = 0 then
      _DiscoverOperationFromXmlValue (aXml, aLog, aReqXml, aRpyXml)
    else
    begin
      if not _DiscoverOperationFromXml (aXml, aLog, aReqXml, aRpyXml) then
      begin
        for X := 0 to aXml.Items.Count - 1 do
        begin
          _DiscoverOperation(aXml.Items.XmlItems[X], aLog, aReqXml, aRpyXml);
        end;
      end;
    end;
  end;
  function _MessageAsText (aOperation: TWsdlOperation; aXml: TXml): string;
  begin
    if aXml.Items.Count = 0 then
      result := aXml.Value
    else
    begin
      if aOperation.isSoapService
      and (not aXml.isSoapEnvelope) then
        result := '<se:Envelope xmlns:se="http://schemas.xmlsoap.org/soap/envelope/"><se:Body>'
                + LineEnding
                + aXml.AsText(True, 2, False, False)
                + LineEnding
                + '</se:Body></se:Envelope>'
      else
      begin
        result := aXml.AsText(True, 0, False, False);
      end;
    end;
  end;

var
  xXml, xReqXml, xRpyXml: TXml;
  X, Y: Integer;
  xLog: TLog;
begin
  try
    XmlUtil.PushCursor (crHourGlass);
    xXml := TXml.Create;
    aLogList.designSuspect := False;
    xLog := TLog.Create;
    try
      if aIsFileName then
        xXml.LoadFromFile(aString, nil)
      else
        xXml.LoadFromString(aString, nil);
      xXml.SeparateNsPrefixes;
      xXml.ResolveNameSpaces;
      for X := 0 to xXml.Items.Count - 1 do
      begin
        with xXml.Items.XmlItems[X] do
        begin
          xReqXml := nil;
          xRpyXml := nil;
          xLog.Operation := nil;
          y := 0;
          while (y < Items.Count) do
          begin
            _DiscoverOperation(Items.XmlItems[Y], xLog, xReqXml, xRpyXml);
            Inc (y);
          end;
          if Assigned(xLog.Operation) then
          begin
            if Assigned (xReqXml) then
            begin
              xLog.RequestBody := _MessageAsText(xLog.Operation, xReqXml);
              xLog.Operation.RequestStringToBindables(xLog.RequestBody);
              xLog.CorrelationId := xLog.Operation.CorrelationIdAsText('; ');
            end;
            if Assigned (xRpyXml) then
              xLog.ReplyBody := _MessageAsText(xLog.Operation, xRpyXml);
            aLogList.SaveLog('', xLog);
            xLog := TLog.Create;
          end;
        end;
      end; // for each xml
    finally
      FreeAndNil(xXml);
      FreeAndNil(xLog);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.Revalidatemessages1Click(Sender: TObject);
var
  o, m: Integer;
begin
  for o := 0 to allOperations.Count - 1 do
    for m := 0 to allOperations.Operations[o].Messages.Count - 1 do
      allOperations.Operations[o].Messages.Messages[m].Disabled := False;
  GridView.InvalidateColumn(0);
end;

procedure TMainForm.Reset1Click(Sender: TObject);
begin
  EnvVarLock.Acquire;
  try
    ResetEnvVars(allOperations.Operations[0], '.*');
  finally
    EnvVarLock.Release;
  end;
end;

procedure TMainForm.EnableMessageActionExecute(Sender: TObject);
begin
  doRevalidateMessages;
end;

procedure TMainForm.DisableMessageActionExecute(Sender: TObject);
begin
  doDisableMessages;
end;

procedure TMainForm.SwiftMtOperationsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.swiftMtOperationsXml;
  if EditXmlXsdBased('SwiftMT Operations', 'OperationDefs.SwiftMtOperations',
    'SwiftMtOperations.Operation.Name', 'SwiftMtOperations.Operation.Name',
    se.IsActive, xXml.Items.Count > 1, esUsed, OperationDefsXsd, xXml) then
  begin
    AcquireLock;
    try
      stubChanged := True;
      se.swiftMtOperationsUpdate(xXml, se.projectFileName);
      PrepareOperation;
    finally
      ReleaseLock;
    end;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.SwiftMtOperationsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of SwiftMt operations ' + HttpActiveHint;
end;

procedure TMainForm.SynchronizedOnMessageChanged;
begin
  GridView.InvalidateColumn(0);
end;

procedure TMainForm.OnChange;
begin
  with TIdSync.Create do
    try
      SynchronizeMethod(SynchronizedOnMessageChanged);
      // maybe someday more updates required //
    finally
      Free;
    end;
end;

procedure TMainForm.OnMessageChanged(aMessage: TWsdlMessage);
begin
  with TIdSync.Create do
    try
      SynchronizeMethod(SynchronizedOnMessageChanged);
    finally
      Free;
    end;
end;

procedure TMainForm.RemoveAllMessagesActionExecute(Sender: TObject);
var
  o: Integer;
begin
  if (xmlUtil.doConfirmRemovals) and (not BooleanPromptDialog(
      'Remove all except default messages from all operations')) then
    exit;
  GridView.RootNodeCount := 1;
  for o := 0 to allOperations.Count - 1 do
  begin
    while allOperations.Operations[o].Messages.Count > 1 do
      allOperations.Operations[o].Messages.DeleteMessage(1);
  end;
  GridView.Invalidate;
  GridView.SetFocus;
  GridViewFocusedNode(GridView.FocusedNode);
  DoColorBindButtons;
  stubChanged := True;
end;

procedure TMainForm.RemoveAllMessagesActionUpdate(Sender: TObject);
begin
  RemoveAllMessagesAction.Enabled := (not se.IsActive) and
    (allOperations.Count > 0);
end;

procedure TMainForm.ShowLogDetailsActionExecute(Sender: TObject);
var
  xNode: PVirtualNode;
  xData: PLogTreeRec;
  aXml: TXml;
begin
  aXml := TXml.CreateAsString('xmlContainer', '');
  try
    AcquireLock;
    try
      xNode := MessagesVTS.GetFirstSelected;
      while Assigned(xNode) do
      begin
        xData := MessagesVTS.GetNodeData(xNode);
        aXml.AddXml(xData.Log.AsXml);
        xNode := MessagesVTS.GetNextSelected(xNode);
      end;
    finally
      ReleaseLock;
    end;
    if aXml.Items.Count = 1 then
      ShowXml('LogDetails', aXml.Items.XmlItems[0])
    else
      ShowXml('LogDetails', aXml);
  finally
    aXml.Free;
  end;
end;

procedure TMainForm.CheckRpyOrFlt(aBind: TCustomBindable);
begin
  if (not(aBind is TIpmItem)) and (WsdlOperation.StubAction <> saRequest) then
  begin
    if aBind.Root = WsdlMessage.rpyBind then
      WsdlMessage.fltBind.Checked := False
    else if Assigned(WsdlMessage.rpyBodyBind) then
      WsdlMessage.rpyBodyBind.Checked := False;
  end;
end;

procedure TMainForm.ToggleNotifyActionExecute(Sender: TObject);
begin
  doNotify := not doNotify;
end;

procedure TMainForm.OperationDelayResponseTimeActionExecute(Sender: TObject);
  procedure _Apply(d, s: TWsdlOperation);
  begin
    if (d = s) then
      exit;
    d.DelayTimeMsMin := s.DelayTimeMsMin;
    d.DelayTimeMsMax := s.DelayTimeMsMax;
  end;

var
  w, s, o: Integer;
  xWsdl: TWsdl;
begin
  if not Assigned(WsdlOperation) then
    raise Exception.Create('no operation');
  Application.CreateForm(TDelayTimeForm, DelayTimeForm);
  try
    DelayTimeForm.Caption := 'Delay response for ' + WsdlOperation.Name +
      ' (ms)';
    DelayTimeForm.DelayMsMin := WsdlOperation.DelayTimeMsMin;
    DelayTimeForm.DelayMsMax := WsdlOperation.DelayTimeMsMax;
    DelayTimeForm.ShowModal;
    if DelayTimeForm.ModalResult = mrOk then
    begin
      WsdlOperation.DelayTimeMsMin := DelayTimeForm.DelayMsMin;
      WsdlOperation.DelayTimeMsMax := DelayTimeForm.DelayMsMax;
      if (WsdlOperation.DelayTimeMsMin = 0) and
        (WsdlOperation.DelayTimeMsMax = 0) then
        OperationDelayResponseTimeAction.ImageIndex := 60
      else
        OperationDelayResponseTimeAction.ImageIndex := 61;
      case DelayTimeForm.ApplyToRadioGroup.ItemIndex of
        1:
          begin
            s := WsdlServicesComboBox.ItemIndex;
            for o := 0 to Wsdl.Services.Services[s].Operations.Count - 1 do
              _Apply(Wsdl.Services.Services[s].Operations.Operations[o],
                WsdlOperation);
          end;
        2:
          begin
            for s := 0 to Wsdl.Services.Count - 1 do
            begin
              for o := 0 to Wsdl.Services.Services[s].Operations.Count - 1 do
                _Apply(Wsdl.Services.Services[s].Operations.Operations[o],
                  WsdlOperation);
            end;
          end;
        3:
          begin
            for w := 0 to se.Wsdls.Count - 1 do
            begin
              xWsdl := TWsdl(se.Wsdls.Objects[w]);
              for s := 0 to xWsdl.Services.Count - 1 do
              begin
                for o := 0 to xWsdl.Services.Services[s].Operations.Count - 1 do
                  _Apply(xWsdl.Services.Services[s].Operations.Operations[o],
                    WsdlOperation);
              end;
            end;
          end;
      end;
      stubChanged := True;
    end;
  finally
    FreeAndNil(DelayTimeForm);
  end;
end;

procedure TMainForm .OperationReqsTreeViewPaintText (
  Sender : TBaseVirtualTree ; const TargetCanvas : TCanvas ;
  Node : PVirtualNode ; Column : TColumnIndex ; TextType : TVSTTextType );
var
  xOperation: TWsdlOperation;
  o, n: Integer;
begin
  try
    n := 0;
    xOperation := NodeToOperation(Sender, Node);
    if Assigned(xOperation) then
      for o := 0 to allOperations.Count - 1 do
        if allOperations.Operations[o].Alias = xOperation.Alias then
          Inc (n);
    if n <> 1 then with TargetCanvas.Font do
    begin
      Color := clRed;
      Style := Style + [fsBold];
    end;
    if xOperation.isDepricated then with TargetCanvas.Font do
      Style := Style + [fsStrikeOut];
  except
    on e: Exception do
      TargetCanvas.Font.Color := clRed;
  end;
end;

procedure TMainForm .PresentLogMemoTextActionExecute (Sender : TObject );
begin
  xmlUtil.presentString(MessagesTabControl.Tabs[MessagesTabControl.TabIndex], LogMemo.Text);
end;

procedure TMainForm .PresentLogMemoTextActionUpdate (Sender : TObject );
begin
  PresentLogMemoTextAction.Enabled := (LogMemo.Lines.Count > 0);
end;

procedure TMainForm.ProjectDesignToClipboardActionExecute(Sender: TObject);
begin
  Clipboard.AsText := se.ProjectDesignAsString(se.projectFileName);
end;

procedure TMainForm .ExportProjectScriptsActionExecute (Sender : TObject );
begin
  OnlyWhenLicensed;
  try
    SaveFileDialog.DefaultExt := 'xml';
    SaveFileDialog.Filter := 'XML file (*.xml)|*.xml';
    SaveFileDialog.Title := 'Export all scripts';
    if SaveFileDialog.Execute then
    begin
      with se.ProjectScriptsAsXml do
      try
        SaveStringToFile (SaveFileDialog.FileName, Text);
      finally
        Free;
      end;
    end;
  finally
  end;
end;

procedure TMainForm.ReadSnapshotInformationActionExecute (Sender: TObject );
var
  X, Y: Integer;
  xOpenOptions: TOpenOptions;
  xXml, yXml: TXml;
begin
  with OpenFileDialog do
  begin
    xOpenOptions := Options;
    xXml := TXml.Create;
    try
      DefaultExt := 'xml';
      FileName := wsdlStubSnapshotsFileName;
      Filter := 'XML file (*.xml)|*.xml';
      Title := 'Read ' + _progName + ' snapshots information from file';
      Options := Options + [ofAllowMultiSelect];
      if Execute then
      begin
        wsdlStubSnapshotsFileName := FileName;
        for X := 0 to Files.Count - 1 do
        try
          xXml.LoadFromFile(Files.Strings[X], nil);
          if xXml.Name <> 'wsdlStubSnapshotList' then
            raise Exception.Create('not a SnapshotList');
          for Y := 0 to xXml.Items.Count - 1 do
          begin
            yXml := xXml.Items.XmlItems[Y];
            if yXml.Name = 'SnapshotDetails' then
              se.CreateSnapshot ( yXml.Items.XmlValueByTag ['name']
                                , yXml.Items.XmlValueByTag ['fileName']
                                , yXml.Items.XmlValueByTag ['refFileName']
                                , False
                                , False
                                );
          end;
        except
          on E: Exception do
            raise Exception.CreateFmt('Error opening file %s%s%s',
              [Files.Strings[X], LineEnding, E.Message]);
        end;
      end;
    finally
      Options := xOpenOptions;
      xXml.Free;
    end;
  end;
end;

procedure TMainForm.ReportOnSnapshots (aList: TClaimableObjectList);
var
  x: Integer;
begin
{$ifdef windows}
  CoInitialize(nil);
  try
{$endif}
    with aList as TSnapshotList do
      for x := 0 to Count - 1 do
        if not se.abortPressed then
          SnapshotItems[x].doReport;
{$ifdef windows}
  finally
    CoUninitialize;
  end;
{$endif}
end;


procedure TMainForm .ReportOnSnapshotsActionExecute (Sender : TObject );
var
  x: Integer;
  xList: TSnapshotList;
begin
  xList := TSnapshotList.Create;
  se.AcquireLogLock;
  try
    for x := 0 to se.displayedSnapshots.Count - 1 do
      xList.AddObject('', se.displayedSnapshots.SnapshotItems[x]);
  finally
    se.ReleaseLogLock;
  end;
  TProcedureThread.Create(False, True, se, ReportOnSnapshots, xList);
end;

procedure TMainForm .SnapshotsPopupMenuPopup (Sender : TObject );
  function _selectionHasRegressionReport: boolean;
  var
    xNode: PVirtualNode;
    xReport: TSnapshot;
  begin
    result := False;
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode)
    and (not result) do
    begin
      xReport := NodeToSnapshot(False, SnapshotsVTS, xNode);
      if Assigned (xReport)
      and (xReport is TRegressionSnapshot) then
         Result := True;
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
  end;
var
  n: Integer;
begin
  n := SnapshotsVTS.SelectedCount;
  PromoteToReferenceMenuItem.Enabled := _selectionHasRegressionReport;
  ShowSnapshotDifferencesAction.Enabled := (n = 2);
end;

procedure TMainForm .SnapshotsVTSClick (Sender : TObject );
begin
  if not Assigned (SnapshotsVTS.FocusedNode) then Exit;
  claimedReport := NodeToSnapshot(True, SnapshotsVTS, SnapshotsVTS.FocusedNode);
  try
    case TSnapshotColumnEnum((Sender as TVirtualStringTree).FocusedColumn) of
      snapshotStatusColumn: ShowReport (claimedReport);
    end;
    SnapshotsVTS.InvalidateNode(SnapshotsVTS.FocusedNode);
    Application.ProcessMessages;
  finally
    claimedReport.Disclaim;
  end;
end;

procedure TMainForm.SnapshotsVTSCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  s1, s2: String;
  Snp1, Snp2: TSnapshot;
begin
  Result := 0;
  s1 := '';
  s2 := '';
  case TSnapshotColumnEnum(Column) of
    snapshotStatusColumn:
    begin
      Snp1 := NodeToSnapshot(False,Sender as TVirtualStringTree, Node1);
      if Assigned (Snp1) then
        s1 := Snp1.statusAsText;
      Snp2 := NodeToSnapshot(False,Sender as TVirtualStringTree, Node2);
      if Assigned (Snp2) then
        s2 := Snp2.statusAsText;
    end;
    else
    begin
      SnapshotsVTSGetText(Sender, Node1, Column, ttNormal, s1);
      SnapshotsVTSGetText(Sender, Node2, Column, ttNormal, s2);
    end;
  end;
  if  s1 < s2 then
    result := -1;
  if s1 > s2 then
    result := 1;
end;

procedure TMainForm .SnapshotsVTSGetImageIndex (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Kind : TVTImageKind ; Column : TColumnIndex ;
  var Ghosted : Boolean ; var ImageIndex : Integer );
var
  xReport: TSnapshot;
begin
  try
    case TSnapshotColumnEnum(Column) of
      snapshotStatusColumn:
      begin
        xReport := NodeToSnapshot(False,Sender as TVirtualStringTree, Node);
        if Assigned(xReport) then
        begin
          case xReport.Status of
            rsUndefined: ImageIndex := 49;
            rsOk: ImageIndex := 47;
            rsNok: ImageIndex := 48;
            rsException: ImageIndex := 45;
          end;
        end;
      end;
    end;
  finally
  end;
end;

procedure TMainForm .SnapshotsVTSGetText (Sender : TBaseVirtualTree ;
  Node : PVirtualNode ; Column : TColumnIndex ; TextType : TVSTTextType ;
  var CellText : String );
var
  xReport: TSnapshot;
begin
  try
    CellText := '';
    xReport := NodeToSnapshot(False, Sender, Node);
    if Assigned(xReport) and (xReport is TSnapshot) then
    begin
      case TSnapshotColumnEnum(Column) of
        snapshotDateTimeColumn: if xReport.timeStamp <> 0 then CellText := xsdFormatDateTime(xReport.timeStamp, @TIMEZONE_UTC);
        snapshotNameColumn: CellText := xReport.Name;
        snapshotMessageColumn: CellText := xReport.Message;
      end;
    end
    else
      CellText := '';
  except
    on e: Exception do
      CellText := e.Message;
  end;
end;

procedure TMainForm.SummaryReport (aList: TClaimableObjectList);
begin
  XmlUtil.presentAsHTML('wsdlStub - Test summary report', htmlReportTestSummary(se, aList as TSnapshotList));
end;

procedure TMainForm .SummaryReportActionExecute (Sender : TObject );
var
  xList: TSnapshotList;
  x: Integer;
begin
  xList := TSnapshotList.Create;
  se.AcquireLogLock;
  try
    for x := 0 to se.displayedSnapshots.Count - 1 do
      xList.AddObject('', se.displayedSnapshots.SnapshotItems[x]);
  finally
    se.ReleaseLogLock;
  end;
  TProcedureThread.Create(False, True, se, SummaryReport, xList);
end;

procedure TMainForm.SaveSnapshotsActionExecute (Sender : TObject );
begin
  if Assigned (se) then
  begin
    SaveFileDialog.DefaultExt := 'xml';
    SaveFileDialog.FileName := wsdlStubSnapshotsFileName;
    SaveFileDialog.Filter := 'XML file (*.xml)|*.xml';
    SaveFileDialog.Title := 'Save ' + _progName + ' snapshots information';
    if SaveFileDialog.Execute then
    begin
      wsdlStubSnapshotsFileName := SaveFileDialog.FileName;
      with se.displayedSnapshots.AsXml do
      try
        SaveStringToFile(wsdlStubSnapshotsFileName, Text);
      finally
        Free;
      end;
    end;
  end;
end;

procedure TMainForm .SchemasToZipExecute (Sender : TObject );
  procedure _wsdlZipper (aZipFileName: String);
  var
    x, w, n, f: Integer;
    slFileNames, slNames: TStringList;
    xXml: TXml;
    newText: String;
    zipper: TAbZipper;
    MS: TStringStream;
    procedure _scanXml (aXml: TXml; aFileName: String);
    var
      x: Integer;
      xFileName: String;
    begin
      if (NameWithoutPrefix(aXml.Name) = 'import')
      or (NameWithoutPrefix(aXml.Name) = 'include')  then
        for x := 0 to aXml.Attributes.Count - 1 do with aXml.Attributes.XmlAttributes[x] do
          if (Name = tagSchemaLocation)
          or (Name = 'location') then
          begin
            xFileName := ExpandRelativeFileName(aFileName, Value);
            Value := slNames.Values[xFileName];
          end;
      for x := 0 to aXml.Items.Count - 1 do
        _scanXml(aXml.Items.XmlItems[x], aFileName);
    end;

  begin
    zipper := TAbZipper.Create(nil);
    try
      zipper.FileName:= aZipFileName;
      zipper.AutoSave:=True;
      while zipper.Count > 0 do
        zipper.DeleteAt(0);
      slFileNames := TStringList.Create;
      try
        slFileNames.Sorted := True;
        slFileNames.Duplicates := dupError;
        for w := 0 to se.Wsdls.Count - 1 do with se.Wsdls.Objects[w] as TWsdl do
        begin
          for n := 0 to XsdDescr.ReadFileNames.Count - 1 do
            if not slFileNames.Find(XsdDescr.ReadFileNames.Strings[n], f) then
              slFileNames.Add(XsdDescr.ReadFileNames.Strings[n]);
        end;
        slNames := TStringList.Create;
        try
          slNames.Sorted := False;
          for n := 0 to slFileNames.Count - 1 do
            slNames.Values[slFileNames.Strings[n]] := 'fn' + IntToStr(n + 1) + '.xsd'; // you might do better
          for w := 0 to se.Wsdls.Count - 1 do with se.Wsdls.Objects[w] as TWsdl do
            slNames.Values[FileName] := 'rootfn' + IntToStr(w + 1) + '.wsdl'; // you might do better
          for n := 0 to slFileNames.Count - 1 do
          begin
            xXml := TXml.Create;
            try
              xXml.LoadFromFile(slFileNames.Strings[n], nil);
              _scanXml (xXml, slFileNames.Strings[n]);
              MS := TStringStream.Create(xXml.Text);
              try
                MS.Position := 0;
                zipper.AddFromStream(slNames.Values[slFileNames.Strings[n]], MS);
              finally
                MS.Free;
              end;
            finally
              xXml.Free;
            end;
          end;
        finally
          FreeAndNil(slNames);
        end;
      finally
        FreeAndNil(slFileNames);
      end;
    finally
      zipper.Free;
    end;
  end;
begin
  with SaveFileDialog do
  begin
    DefaultExt := 'zip';
    Filter := 'zip file (*.zip)|*.zip';
    Title := 'wsdlZipper';
    if Execute then
      _wsdlZipper(FileName);
  end;
end;

procedure TMainForm .ShowGridDifferencesActionExecute (Sender : TObject );
var
  fNode, nNode: PVirtualNode;
  fData, nData: PMessageTreeRec;
  fMessage, nMessage: TWsdlMessage;
  fXml, nXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  if WsdlOperation.DescriptionType in [ipmDTFreeFormat, ipmDTEmail] then
  begin
    ShowMessage('not implemented for freeformat operations');
    Exit;
  end;
  nNode := nil;
  fNode := GridView.GetFirstSelected;
  nNode := GridView.GetNextSelected(fNode);
  if Assigned (fNode)
  and Assigned (nNode) then
  begin
    fData := GridView.GetNodeData(fNode);
    fMessage := fData.Message;
    nData := GridView.GetNodeData(nNode);
    nMessage := nData.Message;
    try
      fXml := TXml.CreateAsString('compare', '');
      with fXml do
      begin
        AddXml (TXml.CreateAsString('name', fMessage.Name));
        with AddXml (TXml.CreateAsString('Req', '')) do
        begin
          if fMessage.reqBind is TXml then
            with AddXml (TXml.CreateAsString(WsdlOperation.reqTagName, '')) do
              CopyDownLine(fMessage.reqBind as TXml, True);
          if (fMessage.reqBind is TIpmItem) then
            AddXml((fMessage.reqBind as TIpmItem).AsXml);
        end;
        with AddXml (TXml.CreateAsString('Rpy', '')) do
        begin
          if fMessage.rpyBind is TXml then
            with AddXml (TXml.CreateAsString(WsdlOperation.rpyTagName, '')) do
              CopyDownLine(fMessage.rpyBind as TXml, True);
          if (fMessage.rpyBind is TIpmItem) then
            AddXml((fMessage.rpyBind as TIpmItem).AsXml);
        end;
      end;
      nXml := TXml.CreateAsString('compare', '');
      with nXml do
      begin
        AddXml (TXml.CreateAsString('name', nMessage.Name));
        with AddXml (TXml.CreateAsString('Req', '')) do
        begin
          if nMessage.reqBind is TXml then
            with AddXml (TXml.CreateAsString(WsdlOperation.reqTagName, '')) do
              CopyDownLine(nMessage.reqBind as TXml, True);
          if (nMessage.reqBind is TIpmItem) then
            AddXml((nMessage.reqBind as TIpmItem).AsXml);
        end;
        with AddXml (TXml.CreateAsString('Rpy', '')) do
        begin
          if nMessage.rpyBind is TXml then
            with AddXml (TXml.CreateAsString(WsdlOperation.rpyTagName, '')) do
              CopyDownLine(nMessage.rpyBind as TXml, True);
          if (nMessage.rpyBind is TIpmItem) then
            AddXml((nMessage.rpyBind as TIpmItem).AsXml);
        end;
      end;
    finally
    end;
    a2bInitialize;
    try
      xA2B := TA2BXml.CreateA2B('', fXml, nXml, Nil);
    finally
      a2bUninitialize;
    end;
    try
      Application.CreateForm(TShowA2BXmlForm, xForm);
      with xForm do
      try
        Caption := 'Diffrences in design messages';
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

procedure TMainForm.OperationOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
  xXsd: TXsd;
  xEnum: TXsdEnumeration;
  o: Integer;
begin
  if not Assigned(WsdlOperation) then
    raise Exception.Create('No operation selected');
  xXsd := operationOptionsXsd.XsdByCaption ['operationOptions.scripts.invoke.operations.name'];
  while xXsd.sType.Enumerations.Count > 0 do
  begin
    xXsd.sType.Enumerations.Objects[0].Free;
    xXsd.sType.Enumerations.Delete(0);
  end;
  for o := 0 to allAliasses.Count - 1 do
  begin
    if allAliasses.Operations[o] <> WsdlOperation then
    begin
      xEnum := TXsdEnumeration.Create;
      xEnum.Value := allAliasses.Operations[o].Alias;
      xXsd.sType.Enumerations.AddObject(xEnum.Value, xEnum);
    end;
  end;
  with WsdlOperation do
  begin
    xXml := OptionsAsXml;
    try
      if EditXmlXsdBased ( 'Operation options for ' + WsdlOperation.Alias
                         , ''
                         , ''
                         , ''
                         , False
                         , False
                         , esUsed
                         , operationOptionsXsd
                         , xXml
                         ) then
      begin
        AcquireLock;
        try
          stubChanged := True;
          OptionsFromXml(xXml);
          PrepareOperation;
        finally
          ReleaseLock;
        end;
      end;
    finally
      xXml.Free;
    end;
  end;
end;

procedure TMainForm.OperationOptionsActionUpdate(Sender: TObject);
begin
  OperationOptionsAction.Enabled := (WsdlOperationsComboBox.ItemIndex > -1);
end;

procedure TMainForm.OptionsFromXml(aXml: TXml);
var
  xXml, yXml: TXml;
begin
  if not Assigned(aXml) then
    exit;

  enableTacoPingPong := True;
  intervalTacoPingPong := 5 * 60 * 1000;
  xmlUtil.doConfirmRemovals := True;
  xmlUtil.doCollapseOnUncheck := True;
  xmlUtil.doExpandOnCheck := True;
  doScrollMessagesIntoView := False;
  doScrollExceptionsIntoView := False;
  // se.HTTPServer.KeepAlive := True;
  se.HTTPServer.ListenQueue := 15;
  se.HTTPServer.MaxConnections := 15;
  se.doViaProxyServer := False;
  se.ViaProxyServer := '';
  se.ViaProxyPort := 8081;
  se.mqUse := mquUndefined;
  se.mqMaxWorkingThreads := 15;
  xsdValidateAssignmentsAgainstSchema := False;
  sc.Enabled := False;
  sc.portNumber:=3738;
  CollapseHeaders := False;
  xmlSetDefaultColors;

  if not aXml.Checked then
    exit;
  with aXml.Items do
  begin
    xXml := XmlCheckedItemByTag['General'];
    if Assigned(xXml) then
    begin
      xmlUtil.doConfirmRemovals := xXml.Items.XmlCheckedBooleanByTagDef
        ['ConfirmRemovals', xmlUtil.doConfirmRemovals];
      doScrollMessagesIntoView := xXml.Items.XmlCheckedBooleanByTagDef
        ['ScrollMessagesIntoView', doScrollMessagesIntoView];
      doScrollExceptionsIntoView := xXml.Items.XmlCheckedBooleanByTagDef
        ['ScrollExceptionsIntoView', doScrollExceptionsIntoView];
      xsdValidateAssignmentsAgainstSchema :=
        xXml.Items.XmlCheckedBooleanByTagDef['CheckScriptAssignments',
        doScrollMessagesIntoView];
      CollapseHeaders := xXml.Items.XmlCheckedBooleanByTagDef
        ['InitialCollapseHeaders', CollapseHeaders];
      xmlUtil.doCollapseOnUncheck := xXml.Items.XmlCheckedBooleanByTagDef
        ['CollapseXmlNodeOnUncheck', xmlUtil.doCollapseOnUncheck];
      xmlUtil.doExpandOnCheck := xXml.Items.XmlCheckedBooleanByTagDef
        ['ExpandXmlNodeOnCheck', xmlUtil.doExpandOnCheck];
    end;
    xXml := XmlCheckedItemByTag['Http'];
    if Assigned(xXml) then
    begin
      // se.HTTPServer.KeepAlive := xXml.Items.XmlCheckedBooleanByTagDef['KeepAlive', se.HTTPServer.KeepAlive];
      se.HTTPServer.ListenQueue := xXml.Items.XmlCheckedIntegerByTagDef
        ['ListenQueue', se.HTTPServer.ListenQueue];
      se.HTTPServer.MaxConnections := xXml.Items.XmlCheckedIntegerByTagDef
        ['MaxConnections', se.HTTPServer.MaxConnections];
      yXml := xXml.Items.XmlCheckedItemByTag['ProxyServer'];
      if Assigned(yXml) then
      begin
        se.doViaProxyServer := yXml.Items.XmlCheckedBooleanByTagDef['Enabled',
          se.doViaProxyServer];
        se.ViaProxyServer := yXml.Items.XmlCheckedValueByTagDef['Host',
          se.ViaProxyServer];
        se.ViaProxyPort := yXml.Items.XmlCheckedIntegerByTagDef['Port',
          se.ViaProxyPort];
      end;
    end;
    xXml := XmlCheckedItemByTag['TaCo'];
    if Assigned(xXml) then with xXml.Items do
    begin
      yXml := XmlCheckedItemByTag['pingpong'];
      if Assigned (yXml) then with yXml.Items do
      begin
        enableTacoPingPong := XmlCheckedBooleanByTagDef['Enabled', enableTacoPingPong];
        intervalTacoPingPong := XmlCheckedIntegerByTagDef['interval', intervalTacoPingPong];
      end;
    end;
    xXml := XmlCheckedItemByTag['RemoteControl'];
    if Assigned(xXml) then
    begin
      sc.Enabled:=xXml.Items.XmlCheckedBooleanByTagDef['Enabled', sc.Enabled];
      try
        sc.portNumber := xXml.Items.XmlCheckedIntegerByTagDef['Port', sc.portNumber];
      except
        on e: exception do
        begin
          LogServerException (e.Message + LineEnding + 'while setting portnumber for remote control' +LineEnding, True, e);
        end;
      end;
    end;
    xXml := XmlCheckedItemByTag['Mq'];
    if Assigned(xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag['Use'];
      if Assigned(yXml) then
      begin
        if yXml.Value = 'Undefined' then
          se.mqUse := mquUndefined;
        if yXml.Value = 'LocalServer' then
          se.mqUse := mquServer;
        if yXml.Value = 'LocalClient' then
          se.mqUse := mquClient;
      end;
      se.mqMaxWorkingThreads := xXml.Items.XmlCheckedIntegerByTagDef
        ['MaxWorkingThreads', se.mqMaxWorkingThreads];
    end;
    xXml := XmlCheckedItemByTag['Colors'];
    if Assigned(xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag['Xml'];
      if Assigned(yXml) then
        with yXml.Items do
        begin
          bgCorrelationItemColor := HtmlToColor
            (XmlCheckedValueByTagDef['CorrelationValues',
            ColorToHtml(bgCorrelationItemColor)]);
          bgExpectedValueColor := HtmlToColor
            (XmlCheckedValueByTagDef['ExpectedValues',
            ColorToHtml(bgExpectedValueColor)]);
          bgNilValueColor := HtmlToColor
            (XmlCheckedValueByTagDef['UnassignedValues',
            ColorToHtml(bgNilValueColor)]);
          bgElementValueColor := HtmlToColor
            (XmlCheckedValueByTagDef['ElementValues',
            ColorToHtml(bgElementValueColor)]);
        end;
    end;
  end;
end;

procedure TMainForm .DesignPanelSplitVerticalMenuItemClick (Sender : TObject );
begin
  doShowDesignSplitVertical := not doShowDesignSplitVertical;
end;

procedure TMainForm .GridPopupMenuPopup (Sender : TObject );
var
  n: Integer;
begin
  n := GridView.SelectedCount;
  ShowGridDifferencesAction.Enabled := (n = 2);
end;

procedure TMainForm.ImportProjectScriptsActionExecute (Sender : TObject );
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  OpenFileDialog.DefaultExt := 'xml';
  OpenFileDialog.Filter := 'XML file (*.xml)|*.xml';
  OpenFileDialog.Title := 'Import scripts';
  if OpenFileDialog.Execute then
  begin
    XmlUtil.PushCursor (crHourGlass);
    try
      xXml := TXml.Create;
      try
        xXml.LoadFromFile(OpenFileDialog.FileName, nil);
        if xXml.Name <> 'projectScripts' then
          raise Exception.CreateFmt('%s does not contain a valid Script export', [OpenFileDialog.FileName]);
        se.ProjectScriptsFromXml(xXml);
        CreateScriptsSubMenuItems;
        se.PrepareAllOperations(LogServerException);
        FillInWsdlEdits;
        stubChanged := True;
      finally
        xXml.Free;
      end;
    finally
      XmlUtil.PopCursor;
    end;
  end;
end;

procedure TMainForm .InWsdlTreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
  TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
  const CellRect : TRect );
var
  r: TRect;
  xBind: TCustomBindable;
begin
  if Column = treeTagColumn then
  begin
    xBind := NodeToBind(Sender, Node);
    if xBind is TXml then with xBind as TXml do
    begin
      if Assigned(Xsd)
      and (Xsd.maxOccurs <> '1') then
      begin
        r := Sender.GetDisplayRect(Node, Column, true);
        TreeviewImageList.Draw(TargetCanvas, r.Right - 16, CellRect.Top, 31);
      end;
    end;
  end;
end;

procedure TMainForm.LoadTestActionExecute (Sender : TObject );
var
  x: Integer;
begin
  if not ActiveAfterPrompt then exit;
  LogTabControl.TabIndex := Ord(spMessages);
  Application.CreateForm(TStressTestForm, StressTestForm);
  try
    StressTestForm.Caption := 'Loadtest operation: ' + WsdlOperation.Name;
    StressTestForm.ShowModal;
    if StressTestForm.ModalResult = mrOk then
    begin
      StressTestConcurrentThreads := StressTestForm.ConcurrentThreads;
      StressTestLoopsPerThread := StressTestForm.LoopsPerThread;
      StressTestDelayMsMin := StressTestForm.DelayMsMin;
      StressTestDelayMsMax := StressTestForm.DelayMsMax;
      for x := 0 to StressTestConcurrentThreads - 1 do
        TProcedureThread.Create(False, True, se, ExecuteLoadTest);
    end;
  finally
    FreeAndNil(StressTestForm);
  end;
end;

procedure TMainForm .LoadTestActionUpdate (Sender : TObject );
begin
  LoadTestAction.Enabled :=
        Assigned(WsdlOperation)
    and Assigned(WsdlMessage)
    and (WsdlOperation.StubAction = saRequest)
    and (WsdlOperation.StubTransport <> ttTaco) // server (and client are) is single threaded
    and (NumberOfBlockingThreads < 1)
    ;
end;

procedure TMainForm .logChartActionExecute (Sender : TObject );
var
  xForm: TlogChartForm;
begin
  OnlyWhenLicensed;
  XmlUtil.PushCursor (crHourGlass);
  try
    Application.CreateForm(TlogChartForm, xForm);
    try
      xForm.Caption := '' + _progName + ' - Performance report';
      xForm.Operations := allOperations;
      xForm.Logs := se.displayedLogs;
      XmlUtil.PopCursor;
      xForm.ShowModal;
      if xForm.Changed then
      begin
        if BooleanPromptDialog('Accept changes to performance report setting') then
        begin
          stubChanged := True;
        end;
      end;
    finally
      xForm.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .DesignSplitHorizontalMenuItemClick (Sender : TObject );
begin
  GridDataPanel.Align := alTop;
  GridDataPanel.Height := MiddlePanel.Height Div 2;
  GridDataPanel.Top := 0;
  DataPanelSplitter.Align := alNone;
  DataPanelSplitter.Top := 1;
  DataPanelSplitter.Align := alTop;
end;

procedure TMainForm .DesignSplitVerticalMenuItemClick (Sender : TObject );
begin
  GridDataPanel.Align := alLeft;
  GridDataPanel.Width := MiddlePanel.Width Div 2;
  GridDataPanel.Left := 0;
  DataPanelSplitter.Align := alNone;
  DataPanelSplitter.Left := 1;
  DataPanelSplitter.Align := alLeft;
end;

procedure TMainForm .MenuItem14Click (Sender : TObject );
var
  xOperation: TWsdlOperation;
begin
  xOperation := WsdlOperation;
  FillOperationReqsTreeView(OperationReqsTreeView, allAliasses);
  WsdlOperation := xOperation;
end;

procedure TMainForm .MenuItem17Click (Sender : TObject );
begin
  if Assigned (WsdlOperation) then
  begin
    WsdlOperation.HiddenFromUI := True;
    UpdateVisibiltyOfOperations;
    stubChanged := True;
  end;
end;

procedure TMainForm .AddChildElementRefMenuItemClick (Sender : TObject );
  procedure _updateTypedef(aBinder: TWsdlBinder; aPath: String; nType: TXsdDataType; aXsd: TXsd);
    procedure _update(aXml: TXml; aPath: String);
    var
      x: Integer;
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _update(aXml.Items.XmlItems[x], aPath);
      if aXml.FullCaption = aPath then
      begin
        bindRefId := 0;
        aXml.AddXml(TXml.Create(0, aXsd));
        aXml.TypeDef := nType;
      end;
    end;
  var
    xBind: TCustomBindable;
  begin
    xBind := aBinder.FindBind(aPath);
    if Assigned (xBind) then
    begin
      if aBinder.reqBind.IsAncestorOf(xBind) then
        _update(aBinder.reqXml, xBind.FullCaption)
      else
        _update(aBinder.rpyXml, xBind.FullCaption);
    end;
  end;
var
  X, m: Integer;
  xxsd: TXsd;
  xXml: TXml;
  xBind: TCustomBindable;
  cXsd: TXsd;
  nTypeDef, oTypeDef, cTypeDef: TXsdDataType;
  xPath: String;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if not Assigned(xBind) then
    raise Exception.Create('no element selected');
  if not(xBind is TXml) then
    raise Exception.Create('operation only valid on XML elements');
  xXml := xBind as TXml;
  if not Assigned(xXml.Xsd) then
    raise Exception.Create('opeation requires an XSD on the selected element');
  Application.CreateForm(TChoose2StringsForm, Choose2StringsForm);
  try
    with Choose2StringsForm do
    begin
      ListOfLists := createListOfListsForElements (Wsdl.XsdDescr.TypeDef);
      Caption := 'Choose from Elements';
      ShowModal;
      if ModalResult = mrOk then
      begin
        cXsd := Wsdl.XsdDescr.FindElement(ChoosenLeftString, ChoosenRightString);
        if not Assigned (cXsd) then
          raise Exception.Create ('procedure TMainForm.AddChildElementRefMenuItemClick(Sender: TObject): cXsd not assigned');
        cTypeDef := cXsd.sType;
        if not Assigned (cTypeDef) then
          raise Exception.Create ('procedure TMainForm.AddChildElementRefMenuItemClick(Sender: TObject): cTypeDef not assigned');
        oTypeDef := xXml.Xsd.sType;
        xxsd := xXml.Xsd.AddElementDef ( Wsdl.XsdDescr
                                       , ChoosenRightString
                                       , cTypeDef
                                       );
        xXsd._RefNameSpace := ChoosenLeftString;
        xXsd._RefElementName := ChoosenRightString;
        nTypeDef := xXml.Xsd.sType;
        xPath := IfThen(WsdlMessage.reqBind.IsAncestorOf(xXml), 'Req.', 'Rpy.')
               + xXml.FullCaption
               ;
        if not oTypeDef.Manually then
          WsdlOperation.BindablesWithAddedElement.AddObject (xPath, WsdlOperation.FindBind(xPath));
        xXml.Checked := True;
        _updateTypedef ( WsdlOperation
                       , xPath
                       , nTypeDef
                       , xxsd
                       );
        for m := 0 to WsdlOperation.Messages.Count - 1 do
        begin
          _updateTypedef ( WsdlOperation.Messages.Messages[m]
                         , xPath
                         , nTypeDef
                         , xxsd
                         );
        end;
        GridView.OnFocusChanged(GridView, GridView.FocusedNode, GridView.FocusedColumn);
        stubChanged := True;
      end;
    end;
  finally
    if Assigned (Choose2StringsForm.ListOfLists) then
      Choose2StringsForm.ListOfLists.Free;
    FreeAndNil(Choose2StringsForm);
  end;
end;

procedure TMainForm.OperationReqsTreeViewClick(Sender: TObject);
begin
  if not Assigned (OperationReqsTreeView.FocusedNode) then Exit;
  case OperationReqsTreeView.FocusedColumn of
    Ord (operationsColumnBeforeScript): EditScriptButtonClick(nil);
    Ord (operationsColumnAfterScript): AfterRequestScriptButtonClick(nil);
  end;
end;

procedure TMainForm .OperationReqsTreeViewGetImageIndex (
  Sender : TBaseVirtualTree ; Node : PVirtualNode ; Kind : TVTImageKind ;
  Column : TColumnIndex ; var Ghosted : Boolean ; var ImageIndex : Integer );
var
  xOperation: TWsdlOperation;
begin
  ImageIndex := -1;
  try
    xOperation := NodeToOperation(Sender, Node);
    if Assigned(xOperation) then
    begin
      case Column of
      Ord (operationsColumnBeforeScript):
        begin
           if (xOperation.BeforeScriptLines.Count > 0) then
           begin
             if (not xOperation.PreparedBefore) then
               ImageIndex := 96
             else
               ImageIndex := 95;
           end
           else
             ImageIndex := 94;
        end;
      Ord (operationsColumnAfterScript):
        begin
          if xOperation.StubAction <> saStub then
          begin
            if (xOperation.AfterScriptLines.Count > 0) then
            begin
              if (not xOperation.PreparedAfter) then
                ImageIndex := 96
              else
                ImageIndex := 95;
            end
            else
              ImageIndex := 94;
          end;
        end;
      end;
    end;
  except
  end;
end;

procedure TMainForm.PingPongTimerTimer (Sender : TObject );
begin
  se.TacoPingPong;
end;

procedure TMainForm .EditProjectPropertiesExecute (Sender : TObject );
begin
  Application.CreateForm(TEditListValuesForm, EditListValuesForm);
  try
    if se.IsActive and False then
      EditListValuesForm.Caption := 'View project properties'
    else
      EditListValuesForm.Caption := 'Edit project properties';
  {
  }
    EditListValuesForm.isReadOnly := se.IsActive and False;
    se.ppLock.Acquire;
    try
      EditListValuesForm.ValueListEditor.Strings.Text := se.projectProperties.Text;
    finally
      se.ppLock.Release;
    end;
    EditListValuesForm.ValueListEditor.Strings.Sort;
    EditListValuesForm.ShowModal;
    if (EditListValuesForm.ModalResult = mrOk)
    { }{ and (not se.IsActive){ } then
    begin
      se.ppLock.Acquire;
      try
        stubChanged := True;
        se.projectProperties.Text := EditListValuesForm.ValueListEditor.Strings.Text;
      finally
        se.ppLock.Release;
      end;
    end;
  finally
    FreeAndNil(EditListValuesForm);
  end;
end;

procedure TMainForm.ShowResolvedPropertiesExecute (Sender : TObject );
var
  x: Integer;
  xXml: TXml;
begin
  xXml := TXml.CreateAsString('resolvedProperties', '');
  try
    for x := 0 to se.projectProperties.Count - 1 do
    begin
      with xXml.AddXml(TXml.CreateAsString('property', '')) do
      begin
        AddXml (TXml.CreateAsString ( 'key', '${' + se.projectProperties.Names[x] + '}'));
        AddXml (TXml.CreateAsString ( 'value', se.projectProperties.Values[se.projectProperties.Names[x]]));
        AddXml (TXml.CreateAsString ( 'resolvesTo'
                                    , xmlio.resolveAliasses ( '${' + se.projectProperties.Names[x] + '}'
                                                            , se.projectProperties
                                                            )
                                    )
               );
      end;
    end;
    ShowXml('project Properties', xXml);
  finally
    xXml.Free;
  end;
end;

procedure TMainForm .ShowSnapshotDifferencesActionExecute (Sender : TObject );
var
  fNode, nNode: PVirtualNode;
  fSnapshot, nSnapshot: TSnapshot;
  fLogList, nLogList: TLogList;
begin
  nNode := nil;
  fNode := SnapshotsVTS.GetFirstSelected;
  nNode := SnapshotsVTS.GetNextSelected(fNode);
  if Assigned (fNode)
  and Assigned (nNode) then
  begin
    fSnapshot := NodeToSnapshot(True, SnapshotsVTS, fNode);
    nSnapshot := NodeToSnapshot(True, SnapshotsVTS, nNode);
    try
      if Assigned (fSnapshot)
      and Assigned (nSnapshot) then
      begin
        fLogList := TLogList.Create;
        nLogList := TLogList.Create;
        try
          se.OpenMessagesLog(fSnapshot.FileName, True, False, fLogList);
          se.OpenMessagesLog(nSnapshot.FileName, True, False, nLogList);
          ShowLogDifferences(fLogList, nLogList, fSnapshot.Name, nSnapshot.Name);
        finally
          fLogList.Clear;
          fLogList.Free;
          nLogList.Clear;
          nLogList.Free;
        end;
      end;
    finally
      fSnapshot.Disclaim;
      nSnapshot.Disclaim;
    end;
  end;
end;

procedure TMainForm.SnapshotCompareMenuitemClick(Sender: TObject);
var
  xReport: TSnapshot;
  xNode: PVirtualNode;
  xList: TSnapshotList;
begin
  xList := TSnapshotList.Create;
  se.AcquireLogLock;
  try
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode) do
    begin
      xReport := NodeToSnapshot(True, SnapshotsVTS, xNode);
      if Assigned (xReport) then
        xList.AddObject('', xReport);
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
  finally
    se.ReleaseLogLock;
  end;
  TProcedureThread.Create(False, True, se, ReportOnSnapshots, xList);
end;

procedure TMainForm .SnapshotPromoteToReferenceMenuItemClick (Sender : TObject );
var
  xReport: TSnapshot;
  xNode: PVirtualNode;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode) do
    begin
      xReport := NodeToSnapshot(True, SnapshotsVTS, xNode);
      try
        if Assigned (xReport)
        and (xReport is TRegressionSnapshot) then
        try
          xmlio.SaveStringToFile ( xReport.RefFileName
                                 , xmlio.ReadStringFromFile (xReport.FileName)
                                 );
          xReport.Status := rsOk;
          xReport.Message := '';
          SnapshotsVTS.InvalidateNode(xNode);
          Application.ProcessMessages;
        except
          on e: Exception do
          begin
            xReport.Message := 'Exception: ' + e.Message;
            xReport.Status := rsException;
            SnapshotsVTS.Invalidate;
          end;
        end;
      finally
        xReport.Disclaim;
      end;
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .SnapshotsFromFolderActionExecute (Sender : TObject );
var
  x: Integer;
  xName: String;
  sl: TSnapshotList;
  s: TSnapshot;
begin
  sl := TSnapshotList.Create;
  try
    sl.Sorted := True;
    sl.Duplicates := dupAccept;
    with FileUtil.FindAllFiles(se.CurrentFolder, '*.xml', False) do
    try
      for x := 0 to Count - 1 do
      begin
        xName := ExtractFileNameOnly(Strings[x]);
        s := TRegressionSnapshot.Create ( xName
                                        , se.CurrentFolder + DirectorySeparator + xName + '.xml'
                                        , se.ReferenceFolder + DirectorySeparator + xName + '.xml'
                                        );
        s.OnReport := se.doRegressionReport;
        s.timeStamp := xmlio.GetFileChangedTime(s.FileName);
        sl.SaveObject(xsdFormatDateTime(s.timeStamp, @TIMEZONE_UTC), s);
      end;
    finally
      Free;
    end;
    se.AcquireLogLock;
    try
      for x := 0 to sl.Count - 1 do
        se.toDisplaySnapshots.AddObject('', sl.SnapshotItems[x]);
    finally
      se.ReleaseLogLock;
    end;
    sl.Clear;
  finally
    sl.Free;
  end;
end;

procedure TMainForm .SnapshotsFromFolderActionUpdate (Sender : TObject );
begin
  SnapshotsFromFolderAction.Enabled := True
                                     ;
end;

procedure TMainForm .SnapshotShowDetailsMenuitemClick (Sender : TObject );
var
  xSnapshot: TSnapshot;
  xNode: PVirtualNode;
  xXml: TXml;
begin
  XmlUtil.PushCursor (crHourGlass);
  xXml := TXml.CreateAsString('selectedSnapshotsDetails','');
  with xXml do
  try
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode) do
    begin
      xSnapshot := NodeToSnapshot(True, SnapshotsVTS, xNode);
      try
        if Assigned (xSnapshot) then
        try
          AddXml(xSnapshot.AsXml);
        except
          on e: Exception do
          begin
            xSnapshot.Message := 'Exception: ' + e.Message;
            xSnapshot.Status := rsException;
            SnapshotsVTS.Invalidate;
          end;
        end;
      finally
        xSnapshot.Disclaim;
      end;
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
    ShowXml('Report details', xXml);
  finally
    xXml.Free;
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .SnapshotLoadRefMessagesMenuItemClick (Sender : TObject );
var
  xReport: TSnapshot;
  xLogList: TLogList;
  xNode: PVirtualNode;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    se.AcquireLogLock;
    try
      while se.displayedLogs.Count > 0 do
      begin
        MessagesVTS.DeleteNode(MessagesVTS.GetFirst);
        se.displayedLogs.LogItems[0].displayRef := nil;
        se.displayedLogs.Delete(0);
      end;
    finally
      se.ReleaseLogLock;
    end;
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode) do
    begin
      xReport := NodeToSnapshot(True, SnapshotsVTS, xNode);
      try
        if Assigned (xReport) then
        begin
          xLogList := TLogList.Create;
          try
            se.OpenMessagesLog(xReport.RefFileName, True, False, xLogList);
            ToAllLogList(xLogList);
          finally
            xLogList.Clear;
            FreeAndNil(xLogList);
          end;
        end;
      finally
        xReport.Disclaim;
      end;
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .SnapshotLoadMessagesMenuItemClick (Sender : TObject );
var
  xSnapshot: TSnapshot;
  xLogList: TLogList;
  xNode: PVirtualNode;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    se.AcquireLogLock;
    try
      while se.displayedLogs.Count > 0 do
      begin
        MessagesVTS.DeleteNode(MessagesVTS.GetFirst);
        se.displayedLogs.LogItems[0].displayRef := nil;
        se.displayedLogs.Delete(0);
      end;
    finally
      se.ReleaseLogLock;
    end;
    xNode := SnapshotsVTS.GetFirstSelected;
    while Assigned (xNode) do
    begin
      xSnapshot := NodeToSnapshot(True, SnapshotsVTS, xNode);
      try
        if Assigned (xSnapshot) then
        begin
          xLogList := TLogList.Create;
          try
            se.OpenMessagesLog(xSnapshot.FileName, True, False, xLogList);
            ToAllLogList(xLogList);
          finally
            xLogList.Clear;
            FreeAndNil(xLogList);
          end;
        end;
      finally
        xSnapshot.Disclaim;
      end;
      xNode := SnapshotsVTS.GetNextSelected(xNode);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .CopyLogGridToClipBoardActionExecute (Sender : TObject );
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    ClipBoard.AsText := vstToGrid(MessagesVTS, MessagesVTSGetText);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .BrowseMqActionHint (var HintStr : string ;
  var CanShow : Boolean );
begin
  if not (   se.mmqqMqInterface.MQServerOK
          or se.mmqqMqInterface.MQClientOK
         ) then
    HintStr := HintStr + ' (IBM WebSphere MQ not installed?)';
end;

procedure TMainForm .ClearSnapshotsActionExecute (Sender : TObject );
begin
  if se.displayedSnapshots.Count > 0 then
  begin
    if (not xmlUtil.doConfirmRemovals)
    or BooleanPromptDialog ('Remove all snapshots information') then
    begin
      se.doClearSnapshots := True;
    end;
  end;
end;

procedure TMainForm.NeedTacoHostData (Sender : TTacoInterface );
var
  xForm: TPromptTacoForm;
begin
  Application.CreateForm(TPromptTacoForm, xForm);
  try
    xForm.Address := tacoHost;
    xForm.Port := tacoPort;
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
    begin
      tacoHost := xForm.Address;
      Sender.Host := tacoHost;
      tacoPort := xForm.Port;
      Sender.Port := tacoPort;
      Sender.Authorisation := xForm.Authorisation;
      Sender.UserName := xmlio.GetUserName;
    end;
  finally
    FreeAndNil(xForm);
  end;
end;

procedure TMainForm.DocumentationViewerHotClick(Sender: TObject);
begin
  OpenUrl((Sender as TIpHtmlPanel).HotURL);
end;

procedure TMainForm .ImportProjectScriptsActionHint (var HintStr : string ;
  var CanShow : Boolean );
begin

end;

procedure TMainForm .EditMessageScriptActionExecute (Sender : TObject );
var
  xOperation: TWsdlOperation;
  xScriptName: String;
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  if not Assigned (WsdlMessage) then Exit;
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
    if xOperation.StubAction = saStub then
      xScriptName := Format('%s / %s  / Main Script', [WsdlOperation.Alias, WsdlMessage.Name])
    else
      xScriptName := Format('%s / %s  / Before Script', [WsdlOperation.Alias, WsdlMessage.Name]);
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xScriptName;
        EditOperationScriptForm.After := False;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ScriptEdit.Lines.Text := WsdlMessage.BeforeScriptLines.Text;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          WsdlMessage.BeforeScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try WsdlMessage.CheckBefore; Except end;
          try WsdlMessage.CheckAfter; Except end;
        end;
        FillInWsdlEdits;
      finally
        FreeAndNil(EditOperationScriptForm);
      end;
    finally
      xOperation.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .EditMessageAfterScriptActionExecute (Sender : TObject );
var
  xOperation: TWsdlOperation;
  xScriptName: String;
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  if not Assigned (WsdlMessage) then Exit;
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
    if xOperation.StubAction = saStub then
      xScriptName := Format('%s / %s  / Main Script', [WsdlOperation.Alias, WsdlMessage.Name])
    else
      xScriptName := Format('%s / %s  / After Script', [WsdlOperation.Alias, WsdlMessage.Name]);
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xScriptName;
        EditOperationScriptForm.After := True;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ScriptEdit.Lines.Text := WsdlMessage.AfterScriptLines.Text;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          WsdlMessage.AfterScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try WsdlMessage.CheckBefore; Except end;
          try WsdlMessage.CheckAfter; Except end;
        end;
        FillInWsdlEdits;
      finally
        FreeAndNil(EditOperationScriptForm);
      end;
    finally
      xOperation.Free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.EditMessageAfterScriptActionUpdate (Sender : TObject );
begin
  EditMessageAfterScriptAction.Enabled := (WsdlOperation.StubAction <> saStub);
end;

procedure TMainForm.LogTabControlChange(Sender: TObject);
begin
  ShowChosenLogTab;
end;

procedure TMainForm.MenuItem33Click(Sender: TObject);
begin
   Clipboard.AsText := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode).Name;
end;

procedure TMainForm.MenuItem34Click(Sender: TObject);
begin
  Clipboard.AsText := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode).FullCaption;
end;

procedure TMainForm .OnTacoAuthorize (Sender : TObject );
begin
  PingPongTimer.Interval := intervalTacoPingPong;
  with Sender as TTacoInterface do
    PingPongTimer.Enabled := (Authorized and enableTacoPingPong);
end;

procedure TMainForm .AbortActionUpdate (Sender : TObject );
begin
  AbortAction.Enabled := (NumberOfBlockingThreads > 0)
                      or (NumberOfNonBlockingThreads > 0)
                       ;
end;

procedure TMainForm .MessagesTabControlChange (Sender : TObject );
begin
  UpdateLogTabs(NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode));
end;

procedure TMainForm .MessagesTabControlGetImageIndex (Sender : TObject ;
  TabIndex : Integer ; var ImageIndex : Integer );
begin
  ImageIndex := logValidationTabImageIndex;
  ImageIndex := 46;
end;

procedure TMainForm .MessagesVTSCompareNodes (Sender : TBaseVirtualTree ;
  Node1 , Node2 : PVirtualNode ; Column : TColumnIndex ; var Result : Integer );
var
  s1, s2: String;
  log1, log2: TLog;
begin
  Result := 0;
  s1 := '';
  s2 := '';
  case TLogColumnEnum(Column) of
    logExpectedColumn: ;
    logRemarksColumn:
      begin
        log1 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node1);
        if Assigned (log1) then
          s1 := log1.Remarks;
        log2 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node2);
        if Assigned (log2) then
          s2 := log2.Remarks;
      end;
    logExceptionColumn:
      begin
        log1 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node1);
        if Assigned (log1) then
          s1 := log1.Exception;
        log2 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node2);
        if Assigned (log2) then
          s2 := log2.Exception;
      end;
    logRequestTreeColumn: ;
    logReplyTreeColumn: ;
    logRequestGridColumn: ;
    logReplyGridColumn: ;
    else
      begin
        MessagesVTSGetText(Sender, Node1, Column, ttNormal, s1);
        MessagesVTSGetText(Sender, Node2, Column, ttNormal, s2);
      end;
  end;
  if  s1 < s2 then
    result := -1;
  if s1 > s2 then
    result := 1;
end;

procedure TMainForm .VTSHeaderClick (Sender : TVTHeader ;
  Column : TColumnIndex ; Button : TMouseButton ; Shift : TShiftState ; X ,
  Y : Integer );
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    if Sender.SortColumn = Column then
    begin
      if Sender.SortDirection = sdAscending then
        Sender.SortDirection := sdDescending
      else
        Sender.SortDirection := sdAscending;
    end
    else
    begin
      Sender.SortColumn := Column;
      Sender.SortDirection := sdAscending;
    end;
    Sender.Treeview.SortTree(Column, Sender.SortDirection, True);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm .PromptForOperationAlias (aOperation : TWsdlOperation );
var
  oldAlias: String;
  m: Integer;
begin
  if not Assigned(aOperation) then
    raise Exception.Create('TMainForm .PromptForOperationAlias (aOperation : TWsdlOperation )  No argument assigned');
  oldAlias := aOperation.Alias;
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Alias for operation ' + aOperation.reqTagName + ' ; ' + aOperation.reqTagNameSpace;
    PromptForm.PromptEdit.Text := aOperation.Alias;
    PromptForm.Numeric := False;
    PromptForm.Pattern := '[A-Za-z]([0-9]|[A-Za-z]|\_|\-|\$)*'; // {id} regexp from express/scanner.l
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
    begin
      if PromptForm.PromptEdit.Text = '' then
      with aOperation do begin
        Alias := reqTagName;
        if Assigned (reqBind) then reqBind.Name := reqMessageName;
        if Assigned (rpyBind) then rpyBind.Name := rpyMessageName;
      end
      else
      with aOperation do begin
        Alias := PromptForm.PromptEdit.Text;
        if Assigned (reqBind) then reqBind.Name := Alias;
        if Assigned (rpyBind) then rpyBind.Name := Alias;
      end;
      if aOperation.Alias <> oldAlias then
      begin
        stubChanged := True;
        se.UpdateOperationAliasses;
        for m := 0 to aOperation.Messages.Count - 1 do
        begin
          aOperation.Messages.Messages[m].reqBind.Name := aOperation.reqBind.Name;
          aOperation.Messages.Messages[m].rpyBind.Name := aOperation.rpyBind.Name;
        end;
        aOperation.RefreshBindables;
        try aOperation.PrepareBefore; Except end;
        try aOperation.PrepareAfter; Except end;
        FillInWsdlEdits;
        OperationReqsTreeView.Invalidate;
        InWsdlTreeView.Invalidate;
      end;
    end;
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm .OperationAliasActionExecute (Sender : TObject );
begin
  if not Assigned(WsdlOperation) then
    raise Exception.Create('No operation selected');
  PromptForOperationAlias(WsdlOperation);
end;

{$ifdef windows}
initialization
  CoInitialize(nil);
  _WsdlSaveLogs := _SaveLogs;
finalization
  CoUninitialize;
{$endif}
end.
