// Search for ZoomElement, it is available
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
  sqldb, oracleconnection , odbcconn, LCLIntf, LCLType,
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
   , ExceptionLogz
   , Ipmz
   , IpmTypes
   , WsdlProjectz
   , Wsdlz
   , Xmlz
   , Xsdz
   , StdCtrls
   , IdHTTP
   , IdSync
   , Buttons
   , ComCtrls
   , ExtCtrls
   , FormIniFilez
   , Menus
   , VirtualTrees , FileUtil , RichBox
   , Bind
   , mqInterface
   , MQAPI
   , SwiftUnit
   , ParserClasses
   , types;

type
  THackControl = class(TWinControl)
  public
    FHandle: HWnd;
    FParentWindow: HWnd;
  end;

  TShowLogData = (slRequestHeaders, slRequestBody, slReplyHeaders, slReplyBody, slException, slValidation);
  TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
  TProcedure = procedure of Object;
  TProcedureString = procedure(arg: String) of Object;
  TProcedureOperation = procedure(arg: TWsdlOperation) of Object;

  { TMainForm }

  TMainForm = class(TForm)
    SqlConnector : TODBCConnection ;
    ProjectDesignToClipboardAction: TAction;
    PresentLogMemoTextAction : TAction ;
    DesignPanel: TPanel;
    alGeneral: TActionList;
    DataTypeDocumentationMemo : TlzRichEdit ;
    FreeFormatMemo: TMemo;
    InWsdlTreeView: TVirtualStringTree;
    LogMemo: TMemo;
    MessagesTabControl: TTabControl;
    Panel1: TPanel;
    ScriptPanel: TPanel;
    ScriptSplitter: TSplitter;
    Splitter1 : TSplitter ;
    SQLTransaction : TSQLTransaction ;
    ToolBar4 : TToolBar ;
    ToolButton30 : TToolButton ;
    ToolButton32 : TToolButton ;
    XsdPanel: TPanel;
    MainToolBar: TToolBar;
    mainImageList: TImageList;
    OpenWsdlAction: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    OpenFileDialog: TOpenDialog;
    MainMenu1: TMainMenu;
    xsdSplitter: TSplitter;
    Panel6: TPanel;
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
    WsdlInfoPanel: TPanel;
    WsdlOperationsComboBox: TComboBox;
    WsdlServicesComboBox: TComboBox;
    OperationLabel: TLabel;
    ServiceLabel: TLabel;
    WsdlLabel: TLabel;
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
    License1: TMenuItem;
    About1: TMenuItem;
    HelpAction: TAction;
    Qry: TSQLQuery;
    runScriptAction: TAction;
    WsdlComboBox: TComboBox;
    Extra1: TMenuItem;
    Options1: TMenuItem;
    OptionsAction: TAction;
    GridToolBar: TToolBar;
    GridView: TVirtualStringTree;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton10: TToolButton;
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
    OperationDocumentationEdit: TMemo;
    Splitter6: TSplitter;
    ToolButton21: TToolButton;
    DownPageControl: TPageControl;
    MessagesTabSheet: TTabSheet;
    ClearLogItemsAction: TAction;
    ToolBar6: TToolBar;
    ToolButton22: TToolButton;
    DocumentationTabSheet: TTabSheet;
    DocumentationMemo: TMemo;
    Splitter7: TSplitter;
    MessagesVTS: TVirtualStringTree;
    Panel2: TPanel;
    MessagesStatusBar: TStatusBar;
    NewStubCaseAction: TAction;
    NewStubCase1: TMenuItem;
    N6: TMenuItem;
    ToolButton15: TToolButton;
    ToolButton23: TToolButton;
    ExceptionTabSheet: TTabSheet;
    Panel8: TPanel;
    Splitter8: TSplitter;
    ExceptionStatusBar: TStatusBar;
    ExceptionsVTS: TVirtualStringTree;
    ToolBar3: TToolBar;
    ToolButton24: TToolButton;
    ClearExceptionsAction: TAction;
    ExceptionMemo: TMemo;
    OperationReqsTreeView: TVirtualStringTree;
    Splitter10: TSplitter;
    ToolButton25: TToolButton;
    SelectMessageColumnsAction: TAction;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    CopyGridAction: TAction;
    PasteGridAction: TAction;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    CopyLogMemoTextToClipBrdAction: TAction;
    ShowHttpReplyAsXMLAction: TAction;
    ShowHttpRequestAsXMLAction: TAction;
    ToolButton35: TToolButton;
    View1: TMenuItem;
    SchemapropertiesMenuItem: TMenuItem;
    ListofOperationsMenuItem: TMenuItem;
    SaveMessagesAction: TAction;
    WsdlInformationMenuItem: TMenuItem;
    WsdlPopulateMenuItem: TMenuItem;
    ReadMessagesAction: TAction;
    ToolButton31: TToolButton;
    MessagesRegressionAction: TAction;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton38: TToolButton;
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
    ToolButton41: TToolButton;
    ExecuteRequestToolButton: TToolButton;
    WsdlItemDelMenuItem: TMenuItem;
    ExecuteAllRequestsToolButton: TToolButton;
    ExecuteRequestAction: TAction;
    ExecuteAllRequestsAction: TAction;
    LogMenuItem: TMenuItem;
    ReadMessagesAction1: TMenuItem;
    SaveMessagesAction1: TMenuItem;
    Comparewithfile1: TMenuItem;
    N7: TMenuItem;
    ClearExceptionsAction1: TMenuItem;
    N8: TMenuItem;
    ShowrequestasXML1: TMenuItem;
    ShowreplyasXML1: TMenuItem;
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
    ExecuteRequestMenuItem: TMenuItem;
    ExecuteAllRequestsMenuItem: TMenuItem;
    ScriptButtonsPanel: TPanel;
    EditScriptButton: TPanel;
    AfterRequestScriptButton: TPanel;
    FilterLogAction: TAction;
    ToolButton42: TToolButton;
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
    ToolBar9: TToolBar;
    ToolButton44: TToolButton;
    FindAction: TAction;
    ToolButton45: TToolButton;
    FindNextAction: TAction;
    N14: TMenuItem;
    Validate1: TMenuItem;
    ToolButton46: TToolButton;
    CheckTreeAction: TAction;
    ToolButton47: TToolButton;
    Project1: TMenuItem;
    Options2: TMenuItem;
    ProjectOptionsAction: TAction;
    ToolButton48: TToolButton;
    AddChildElementDefMenuItem: TMenuItem;
    ToolButton18: TToolButton;
    View: TLabel;
    ViewStyleComboBox: TComboBox;
    LastToolButton: TToolButton;
    ProgressBar: TProgressBar;
    All1: TMenuItem;
    Required1: TMenuItem;
    ShowReplyAsXmlGridAction: TAction;
    ShowRequestAsXmlGridAction: TAction;
    ToggleCheckExpectedValuesAction: TAction;
    ToggleBetaModeAction: TAction;
    ShowExpectedXmlAction: TAction;
    ShowrequestinaGrid1: TMenuItem;
    ShowreplyasGrid1: TMenuItem;
    Action1: TAction;
    httpRequestMessagesAction: TAction;
    MasterMessagesToolButton: TToolButton;
    MasterMessagesMenuItem: TMenuItem;
    BrowseMqAction: TAction;
    BrowseMqMenuItem: TMenuItem;
    ShowRequestHeaderAsXmlAction: TAction;
    BrowseMqButton: TToolButton;
    httpRequestDesignAction: TAction;
    httpRequestDesignButton: TToolButton;
    MasterDesignMenuItem: TMenuItem;
    RefreshTimer: TTimer;
    GridPopupMenu: TPopupMenu;
    MessagesToDiskMenuItem: TMenuItem;
    MessagesToDiskAction: TAction;
    Log2DesignAction: TAction;
    Addtodesign1: TMenuItem;
    ToolButton16: TToolButton;
    ToolButton54: TToolButton;
    ViewMssgAsTextAction: TAction;
    CopyCobolDataToClipboardMenuItem: TMenuItem;
    PasteCobolDataFromClipboardMenuItem: TMenuItem;
    ElementvalueMenuItem: TMenuItem;
    AssignExpressionMenuItem: TMenuItem;
    ToolButton55: TToolButton;
    ToolButton56: TToolButton;
    SelectExpectedElementsAction: TAction;
    ToolButton57: TToolButton;
    ToolButton49: TToolButton;
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
    ToolButton59: TToolButton;
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
    N18: TMenuItem;
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
    MasterReloadDesignMenuItem: TMenuItem;
    MasterReloadDesignAction: TAction;
    MasterClearLogAction: TAction;
    N21: TMenuItem;
    Operation1: TMenuItem;
    RedirectAddressAction: TAction;
    OperationDelayResponseTimeAction: TAction;
    RedirectAddressAction1: TMenuItem;
    Delayresponse1: TMenuItem;
    OperationApplySettingsAction: TAction;
    Applysettingsto1: TMenuItem;
    OperationWsaAction: TAction;
    WsA1: TMenuItem;
    MasterReactivateActon: TAction;
    Reactivatemaster1: TMenuItem;
    N22: TMenuItem;
    QueryMasterMessages1: TMenuItem;
    ThrowExceptionAction: TAction;
    MasterRestartAction: TAction;
    MasterRestartAction1: TMenuItem;
    Configurelisteners1: TMenuItem;
    MessagesFromDiskAction: TAction;
    ToolButton20: TToolButton;
    ToolButton63: TToolButton;
    Readmessagesfromdiskfiles1: TMenuItem;
    LogDisplayedColumnsAction: TAction;
    ToolButton50: TToolButton;
    Displayedcolumns1: TMenuItem;
    startStopButton: TToolButton;
    startAction: TAction;
    stopAction: TAction;
    StatusPanel: TPanel;
    ScriptsMenuItem: TMenuItem;
    EditScriptMenuItem: TMenuItem;
    N23: TMenuItem;
    ScriptGoMenuItem: TMenuItem;
    New2: TMenuItem;
    RemoveScriptMenuItem: TMenuItem;
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
    Options4: TMenuItem;
    HideAllOperationsAction: TAction;
    c: TMenuItem;
    N28: TMenuItem;
    UnhideAllOperationsAction: TAction;
    Unhidealloperations1: TMenuItem;
    HideoperationMenuItem: TMenuItem;
    UnhideOperationMenuItem: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    ShowSelectedRequestsAsXmlGridAction: TAction;
    ShowrequestinaGrid2: TMenuItem;
    ShowSelectedResponsesAsXmlGridAction: TAction;
    Showselectedresponsesinagrid1: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    OperationZoomOnAction: TAction;
    ShowLogZoomElementAction: TAction;
    LogCoverageReportAction: TAction;
    ToolButton67: TToolButton;
    ToolButton68: TToolButton;
    ToolButton69: TToolButton;
    LogCoverageReportAction1: TMenuItem;
    N26: TMenuItem;
    DisplayedcolumnMenuItem: TMenuItem;
    FreeFormatsAction: TAction;
    ViewMssgAsTextAction1: TMenuItem;
    CobolOperationsAction: TAction;
    CobolOperationsAction1: TMenuItem;
    SwiftMtOperationsAction: TAction;
    MaintainlistofSwiftMToperations1: TMenuItem;
    OperationsPopupMenu: TPopupMenu;
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
    procedure DataTypeDocumentationMemoClick (Sender : TObject );
    procedure MessagesTabControlChange (Sender : TObject );
    procedure MessagesTabControlGetImageIndex (Sender : TObject ;
      TabIndex : Integer ; var ImageIndex : Integer );
    procedure MessagesVTSChange (Sender : TBaseVirtualTree ;
      Node : PVirtualNode );
    procedure OperationDelayResponseTimeActionExecute(Sender: TObject);
    procedure PresentLogMemoTextActionExecute (Sender : TObject );
    procedure PresentLogMemoTextActionUpdate (Sender : TObject );
    procedure ProjectDesignToClipboardActionExecute(Sender: TObject);
    procedure RequestBodyTabSheetContextPopup (Sender : TObject ;
      MousePos : TPoint ; var Handled : Boolean );
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
    procedure ToolBar8Click (Sender : TObject );
    procedure ViewMssgAsTextActionExecute(Sender: TObject);
    procedure ViewMssgAsTextActionUpdate(Sender: TObject);
    procedure Log2DesignActionExecute(Sender: TObject);
    procedure MessagesToDiskActionExecute(Sender: TObject);
    procedure MessagesToDiskActionUpdate(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure httpRequestDesignActionExecute(Sender: TObject);
    procedure httpRequestDesignActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure httpRequestDesignActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BrowseMqActionExecute(Sender: TObject);
    procedure BrowseMqActionUpdate(Sender: TObject);
    procedure httpRequestMessagesActionExecute(Sender: TObject);
    procedure httpRequestMessagesActionUpdate(Sender: TObject);
    procedure httpRequestMessagesActionHint(var HintStr: string;
      var CanShow: Boolean);
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
    procedure AbortToolButtonClick(Sender: TObject);
    procedure GridToolBarResize(Sender: TObject);
    procedure InWsdlTreeViewFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure ViewStyleComboBoxChange(Sender: TObject);
    function getXmlViewType: TxvViewType;
    procedure AddChildElementDefMenuItemClick(Sender: TObject);
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
    procedure ScriptButtonsPanelResize(Sender: TObject);
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
    procedure NewStubCaseActionUpdate(Sender: TObject);
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
    procedure RedirectAddressActionExecute(Sender: TObject);
    procedure TreeViewResize(Sender: TObject);
    procedure GridViewExit(Sender: TObject);
    procedure InWsdlTreeViewExit(Sender: TObject);
    procedure SaveStubCaseActionUpdate(Sender: TObject);
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
    procedure SaveStubCaseAsActionUpdate(Sender: TObject);
    procedure OpenStubCaseActionUpdate(Sender: TObject);
    procedure WsdlComboBoxChange(Sender: TObject);
    procedure runScriptActionExecute(Sender: TObject);
    procedure License1Click(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ReopenStubCaseActionExecute(Sender: TObject);
    procedure OpenStubCaseActionExecute(Sender: TObject);
    procedure SaveStubCaseAsActionExecute(Sender: TObject);
    procedure EditScriptButtonClick(Sender: TObject);
    procedure Expand2Click(Sender: TObject);
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
    procedure setWsdlReply(const Value: TWsdlMessage);
    function getWsdlReply: TWsdlMessage;
    function getWsdlOperation: TWsdlOperation;
    procedure setWsdlOperation(const Value: TWsdlOperation);
    procedure WsdlOperationsComboBoxChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure OpenWsdlActionExecute(Sender: TObject);
    procedure OpenWsdlActionHint(var HintStr: string; var CanShow: Boolean);
    procedure OpenWsdlActionUpdate(Sender: TObject);
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
    procedure MasterReloadDesignActionUpdate(Sender: TObject);
    procedure MasterReloadDesignActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure MasterReloadDesignActionExecute(Sender: TObject);
    procedure MasterClearLogActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure SendMasterCommand(aPrompt, aCommand: String);
    procedure MasterClearLogActionUpdate(Sender: TObject);
    procedure MasterClearLogActionExecute(Sender: TObject);
    procedure OperationWsaActionExecute(Sender: TObject);
    procedure OperationWsaActionUpdate(Sender: TObject);
    procedure MasterReactivateActonUpdate(Sender: TObject);
    procedure MasterReactivateActonHint(var HintStr: string;
      var CanShow: Boolean);
    procedure MasterReactivateActonExecute(Sender: TObject);
    procedure ThrowExceptionActionExecute(Sender: TObject);
    procedure MasterRestartActionUpdate(Sender: TObject);
    procedure MasterRestartActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure MasterRestartActionExecute(Sender: TObject);
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
    procedure New2Click(Sender: TObject);
    procedure RemoveScriptMenuItemClick(Sender: TObject);
    procedure runScriptActionUpdate(Sender: TObject);
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
    procedure HideoperationMenuItemClick(Sender: TObject);
    procedure UnhideOperationMenuItemClick(Sender: TObject);
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
    procedure LogCoverageReportActionExecute(Sender: TObject);
    procedure LogDisplayedColumnsAddClick(Sender: TObject);
    procedure DisplayedcolumnMenuItemClick(Sender: TObject);
    procedure FreeFormatsActionHint(var HintStr: string; var CanShow: Boolean);
    procedure FreeFormatsActionUpdate(Sender: TObject);
    procedure FreeFormatsActionExecute(Sender: TObject);
    procedure LogDisplayedColumnsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure CobolOperationsActionExecute(Sender: TObject);
    procedure CobolOperationsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure CobolOperationsActionUpdate(Sender: TObject);
    procedure SwiftMtOperationsActionUpdate(Sender: TObject);
    procedure SwiftMtOperationsActionHint(var HintStr: string;
      var CanShow: Boolean);
    procedure SwiftMtOperationsActionExecute(Sender: TObject);
    procedure XsdOperationsActionUpdate(Sender: TObject);
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
    property WsdlReply: TWsdlMessage read getWsdlReply write setWsdlReply;
    property xmlViewType: TxvViewType read getXmlViewType;
  private
    procedureThread: TProcedureThread;
    editingNode: PVirtualNode;
    notifyTabCaption, logTabCaption: String;
    notifyTabImageIndex: Integer;
    logValidationTabImageIndex: Integer;
    startStopShortCut: TShortCut;
    fLastCaption: String;
    QueueNameList: TStringList;
    captionFileName: String;
    isOptionsChanged: Boolean;
    MasterAddress: String;
    AutoRefreshSlave: Boolean;
    doScrollMessagesIntoView: Boolean;
    doScrollExceptionsIntoView: Boolean;
    DisclaimerAccepted: Boolean;
    IniFile: TFormIniFile;
    ActualXml: TCustomBindable;
    ActualXmlAttr: TXmlAttribute;
    ErrorReadingLicenseInfo: Boolean;
    xCompanyName: String;
    xLicenseExpirationDate: String;
    xLicenseString: String;
    fElementsWhenRepeatable: Integer;
    fDoShowDesignAtTop: Boolean;
    grid_x, grid_y: Integer;
    fAbortPressed: Boolean;
    fIsBusy: Boolean;
    doNotify: Boolean;
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
    procedure UpdateLogTabs (aLog: TLog);
    procedure RemoveMessageColumns;
    procedure FocusOnBind(aBind: TCustomBindable);
    function AddMessage(aCopyNode: PVirtualNode): PVirtualNode;
    procedure PasteGridFromPasteBoard;
    procedure PasteGridOnNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure CopyGridOnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure InitMasterServer;
    procedure ShowXmlInGrid(aXml: TXml; aReadOnly: Boolean);
    procedure ShowXml(aCaption: String; aXml: TXml);
    procedure ShowIpm(aCaption: String; aIpm: TIpmItem);
    procedure ShowTextAsGrid(aCaption, aText: String);
    procedure ShowTextAsXml(aCaption, aText: String);
    procedure ShowLogDifferences(aLogs, bLogs: TLogList;
      aReferenceFileName: String);
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
    function ClearLogCommand(aDoRaiseExceptions: Boolean): String;
    function ReactivateCommand: String;
    function QuitCommand(aDoRaiseExceptions: Boolean): String;
    function RestartCommand: String;
    function ReloadDesignCommand: String;
    procedure ActivateCommand(aActivate: Boolean);
    procedure OpenProjectCommand(aProject: String);
    procedure Notify(aString: String);
    procedure wsdlStubInitialise;
    function getStubChanged: Boolean;
    procedure ExpressError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
    procedure EditScript(X: Integer);
    procedure SetAbortPressed(const Value: Boolean);
    procedure SetIsBusy(const Value: Boolean);
    procedure UpdateVisibiltyOfOperations;
    procedure UpdateVisibiltyTreeView (aFreeFormat: Boolean);
    procedure SetUiBusy;
    procedure SetUiReady;
    procedure SetUiProgress;
  private
    function getHintStrDisabledWhileActive: String;
    procedure ShowHttpReplyAsXMLActionExecute(Sender: TObject);
    procedure ReloadProject;
  published
  public
    se: TWsdlProject;
    claimedLog: TLog;
    isSlaveMode: Boolean;
    mqServerEnv: String;
    CollapseHeaders: Boolean;
    wsdlStubMessagesFileName: String;
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
    property HintStrDisabledWhileActive
      : String read getHintStrDisabledWhileActive;
    property isBusy: Boolean read fIsBusy write SetIsBusy;
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
    property stubChanged: Boolean read getStubChanged write setStubChanged;
    property Wsdl: TWsdl read getWsdl write setWsdl;
    procedure BeginUpdate;
    Procedure EndUpdate;
    function OptionsAsXml: TXml;
    function doDecryptString(aString: AnsiString): AnsiString;
    function doEncryptString(aString: AnsiString): AnsiString;
    procedure ProjectDesignFromString(aString, aMainFileName: String);
    procedure OnlyWhenLicensed;
    function LogMaxEntriesEqualsUnbounded (aCaption: String): Boolean;
    procedure LicenseRequestFromServer(aProviderAddress: String);
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
    Reply: TWsdlMessage;
  end;

  PLogTreeRec = ^TLogTreeRec;

  TLogTreeRec = record
    Log: TLog;
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
  wsdlListUnit, ErrorFound, ClipBrd, ShowXmlUnit,
  ShowXmlCoverageUnit, EditOperationScriptUnit, igGlobals,
  ChooseStringUnit, AboutUnit, StrUtils, IpmGunLicense,
  IpmGunLicenseUnit, DisclaimerUnit,
  PromptUnit, SelectXmlElement, ApplyToUnit, wsaConfigUnit,
  SelectElementsUnit, A2BXmlz,
  ShowLogDifferencesUnit, EditListValuesUnit, AddFavouritesUnit,
  LogFilterUnit,
  ShowA2BXmlUnit, FindRegExpDialog,
  XmlGridUnit, IpmGridUnit,
  xmlUtilz, ShowExpectedXml, mqBrowseUnit, messagesToDiskUnit, messagesFromDiskUnit{$ifdef windows}, ActiveX{$endif}, EditStamperUnit,
  EditCheckerUnit, Math, vstUtils, DelayTimeUnit, base64, xmlxsdparser,
  HashUtilz, xmlio;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TLogColumnEnum = (logExpectedColumn, logRemarksColumn, logRequestTreeColumn,
    logReplyTreeColumn, logRequestGridColumn, logReplyGridColumn,
    logTimeColumn, logDurationColumn, logActionColumn, logServiceColumn,
    logOperationColumn, logCorrelationIdColumn, logStdColumnCount);

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
    xLog := TExceptionLog.Create (Msg
                                 + LineEnding
                                 + se.ExceptionStackListString(nil)
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
  if Assigned(WsdlReply) then
  begin
    if WsdlOperation.StubAction = saRequest then
      WsdlReply.FreeFormatReq := FreeFormatMemo.Text
    else
      WsdlReply.FreeFormatRpy := FreeFormatMemo.Text;
    stubChanged := True;
  end;
end;

procedure TMainForm.FreeFormatsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.freeFormatOperationsXml;
  if EditXmlXsdBased('Freeformat Operations',
    'OperationDefs.FreeFormatOperations',
    'FreeFormatOperations.Operation.Name',
    'FreeFormatOperations.Operation.Name',
    se.IsActive, OperationDefsXsd, xXml) then
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

procedure TMainForm.FreeFormatsActionUpdate(Sender: TObject);
begin
  FreeFormatsAction.Enabled := not se.IsActive;
end;

procedure TMainForm.OpenWsdlActionUpdate(Sender: TObject);
begin
  OpenWsdlAction.Enabled := not se.IsActive;
end;

procedure TMainForm.OpenWsdlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Open WSDL web service description' + HttpActiveHint;
end;

function TMainForm.HttpActiveHint: String;
begin
end;

procedure TMainForm.OpenWsdlActionExecute(Sender: TObject);
var
  f, w: Integer;
begin
  EndEdit;
  Application.CreateForm(TWsdlListForm, WsdlListForm);
  try
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
  EditScriptButton.Font.Style := EditScriptButton.Font.Style - [fsBold] -
    [fsStrikeOut];
  AfterRequestScriptButton.Font.Style := AfterRequestScriptButton.Font.Style -
    [fsBold] - [fsStrikeOut];
  if (WsdlOperation <> nil) then
  begin
    ActionComboBox.ItemIndex := Ord(WsdlOperation.StubAction);
    // SoapActionEdit.Text := WsdlOperation.SoapAction;
    OperationDocumentationEdit.Text := WsdlOperation.Documentation.Text;
    if Trim(WsdlOperation.BeforeScriptLines.Text) <> '' then
    begin
      EditScriptButton.Font.Style := EditScriptButton.Font.Style + [fsBold];
      if not WsdlOperation.PreparedBefore then
        EditScriptButton.Font.Style := EditScriptButton.Font.Style +
          [fsStrikeOut];
    end;
    if Trim(WsdlOperation.AfterScriptLines.Text) <> '' then
    begin
      AfterRequestScriptButton.Font.Style :=
        AfterRequestScriptButton.Font.Style + [fsBold];
      if not WsdlOperation.PreparedAfter then
        AfterRequestScriptButton.Font.Style :=
          AfterRequestScriptButton.Font.Style + [fsStrikeOut];
    end;
  end
  else
  begin
    // OperationDocumentationEdit.Text := '';
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
    xData.Reply := aMessages.Messages[X];
  end;
  if Assigned(WsdlOperation.LastMessage) then
    WsdlReply := WsdlOperation.LastMessage;
  if Assigned(WsdlReply) then
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
      aMessage := Data.Reply;
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
      xChanged := xmlUtil.editXml(xBind, False);
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
  EndEdit;
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
  DataTypeDocumentationMemo.Clear;
  ActualXml := nil;
  ActualXmlAttr := nil;
  if xBind is TIpmItem then
    StatusPanel.Caption := '[' + IntToStr((xBind as TIpmItem).Offset + 1)
      + ':' + IntToStr((xBind as TIpmItem).Bytes) + '] ' + xBind.FullCaption
  else
    StatusPanel.Caption := xBind.FullCaption;
  try
    xmlUtil.ListXsdDocumentation(DataTypeDocumentationMemo, xBind, False, False);
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
        GridView.FocusedColumn := f + 1 +
          WsdlOperation.CorrelationBindables.Count;
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
          se.UpdateMessageRow(WsdlOperation, WsdlReply);
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
      se.UpdateMessageRow(WsdlOperation, WsdlReply);
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
        se.UpdateMessageRow(WsdlOperation, WsdlReply);
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
  xXml := se.xsdOperationsXml('');
  if EditXmlXsdBased('Xsd Operations', 'OperationDefs.XsdOperations',
    'XsdOperations.Operation.Name', 'XsdOperations.Operation.Name',
    se.IsActive, OperationDefsXsd, xXml) then
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
end;

procedure TMainForm.XsdOperationsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of XSD operations ' + HttpActiveHint;
end;

procedure TMainForm.XsdOperationsActionUpdate(Sender: TObject);
begin
  XsdOperationsAction.Enabled := not se.IsActive;
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

procedure TMainForm.AfterRequestScriptButtonClick(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  xOperation := TWsdlOperation.Create(WsdlOperation);
  try
    Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
    try
      EditOperationScriptForm.ScriptName := xOperation.Name;
      EditOperationScriptForm.After := True;
      EditOperationScriptForm.WsdlOperation := xOperation;
      EditOperationScriptForm.ShowModal;
      if EditOperationScriptForm.ModalResult = mrOk then
      begin
        stubChanged := True;
        WsdlOperation.BeforeScriptLines := xOperation.BeforeScriptLines;
        WsdlOperation.AfterScriptLines := xOperation.AfterScriptLines;
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
begin
  if not Assigned(WsdlOperation) then
    Raise Exception.Create('First get a Wsdl');
  Screen.Cursor := crHourGlass;
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
    if xOperation.PrepareErrors <> '' then
      if not BooleanPromptDialog (xOperation.PrepareErrors + LineEnding + 'Continue') then
        Exit;
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xOperation.Name;
        EditOperationScriptForm.After := False;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          WsdlOperation.BeforeScriptLines := xOperation.BeforeScriptLines;
          try WsdlOperation.PrepareBefore; Except end;
          WsdlOperation.AfterScriptLines := xOperation.AfterScriptLines;
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
    Screen.Cursor := crDefault;
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
      SaveWsdlStubCase(SaveFileDialog.FileName);
    end;
  finally
  end;
end;

procedure TMainForm.SaveWsdlStubCase(aFileName: String);
var
  SwapCursor: TCursor;
begin
  captionFileName := ExtractFileName(aFileName);
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    se.FocusOperationName := ifthen(Assigned (WsdlOperation), WsdlOperation.reqTagName);
    se.FocusMessageIndex := ifthen(Assigned (WsdlOperation), WsdlOperation.Messages.IndexOfObject(WsdlReply));
    SaveStringToFile(aFileName, se.ProjectDesignAsString(aFileName));
    stubChanged := False;
    se.stubRead := True; // well,... but logically ...
    UpdateReopenList(ReopenCaseList, aFileName);
  finally
    Screen.Cursor := SwapCursor;
  end;
end;

procedure TMainForm.OpenStubCaseActionExecute(Sender: TObject);
begin
  EndEdit;
  if OkToOpenStubCase then
  begin
    OpenFileDialog.DefaultExt := 'wsdlStub';
    OpenFileDialog.FileName := se.projectFileName;
    OpenFileDialog.Filter := 'wsdlStub case (*.wsdlStub)|*.wsdlStub';
    OpenFileDialog.Title := 'Open Stub Case';
    if OpenFileDialog.Execute then
    begin
      se.projectFileName := OpenFileDialog.FileName;
      OpenStubCase(OpenFileDialog.FileName);
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
  if allOperations.Find (se.FocusOperationName, f) then
  begin
    WsdlOperation := allOperations.Operations[f];
    if (se.FocusMessageIndex < WsdlOperation.Messages.Count) then
      WsdlReply := WsdlOperation.Messages.Messages[se.FocusMessageIndex];
  end;
end;

function TMainForm.ClearLogCommand(aDoRaiseExceptions: Boolean): String;
begin
  result := 'Master log cleared ' + se.projectFileName + ' successfully';
  try
    AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Clear log refused because master instance of ' + _progName +
            ' is inactive');
      MessagesVTS.Clear;
      se.AsynchRpyLogs.Clear;
      se.displayedLogs.Clear;
      LogMemo.Text := '';
    finally
      ReleaseLock;
    end;
  except
    on E: Exception do
      if aDoRaiseExceptions then
        raise
      else
        result := E.Message;
  end;
end;

function TMainForm.ReactivateCommand: String;
begin
  result := 'Master instance of ' + _progName + ' reactivated ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Reactivate refused because master instance of ' + _progName + ' is inactive');
      try
        se.Activate(False);
        // IsActive := False;
        Sleep(1000); // allow mq threads some time
        se.Activate(True);
        // IsActive := xActive;
        if not se.IsActive then
          raise Exception.Create(
            'Reactivate failed, see master exceptionlog for details');
      finally
      end;
    finally
      // ReleaseLock;
    end;
  except
    on E: Exception do
      result := E.Message + LineEnding + LineEnding + se.ExceptionStackListString(e);
  end;
end;

function TMainForm.QuitCommand(aDoRaiseExceptions: Boolean): String;
begin
  {$ifdef windows}
  result := 'Master instance of ' + _progName + ' is shutting down ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Shurtdown refused because master instance of ' + _progName +
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
      result := E.Message + LineEnding + LineEnding + se.ExceptionStackListString(e);
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
  result := 'Master instance of ' + _progName +
    ' will restart, try after some time... ';
  try
    // AcquireLock;
    try
      if not se.IsActive then
        raise Exception.Create
          ('Restart refused because master instance of ' + _progName +
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
      result := E.Message + LineEnding + LineEnding + se.ExceptionStackListString(e);
  end;
{$else}
  result := 'RestartCommand not implemented';
{$endif}
end;

procedure TMainForm.ReleaseLock;
begin
  if False then Wsdlz.ReleaseLock;
end;

function TMainForm.ReloadDesignCommand: String;
begin
  result := 'Master instance of ' + _progName + ' reloaded ' +
    se.projectFileName + ' successfully';
  try
    AcquireLock;
    try
      if stubChanged then
        raise Exception.Create
          ('Reload refused because master instance of ' + _progName +
            ' has unsaved changes in design');
      if not Assigned(WsdlOperation) then
        raise Exception.Create(
          'Reload refused because master has not project loaded');
      if not se.IsActive then
        raise Exception.Create
          ('Reload refused because master instance of ' + _progName +
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
var
  SwapCursor: TCursor;
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
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
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
      InitMasterServer;
      _WsdlPortNumber := IntToStr(se.Listeners.httpPort);
    finally
      { }
      Screen.Cursor := SwapCursor;
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
  EndEdit;
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
  X: Integer;
  xMenuItem: TMenuItem;
begin
  HideoperationMenuItem.OnClick := nil;
  HideoperationMenuItem.Clear;
  UnhideOperationMenuItem.OnClick := nil;
  UnhideOperationMenuItem.Clear;
  Node := OperationReqsTreeView.GetFirst;
  X := 0;
  while Assigned(Node) do
  begin
    OperationReqsTreeView.IsVisible[Node] := not allOperations.Operations[X]
      .HiddenFromUI;
    if allOperations.Operations[X].HiddenFromUI then
    begin
      xMenuItem := TMenuItem.Create(nil);
      xMenuItem.Tag := X;
      xMenuItem.Caption := allOperations.Operations[X].reqTagName;
      xMenuItem.OnClick := UnhideOperationMenuItemClick;
      UnhideOperationMenuItem.Add(xMenuItem);
    end
    else
    begin
      xMenuItem := TMenuItem.Create(nil);
      xMenuItem.Tag := X;
      xMenuItem.Caption := allOperations.Operations[X].reqTagName;
      xMenuItem.OnClick := HideoperationMenuItemClick;
      HideoperationMenuItem.Add(xMenuItem);
    end;
    Node := OperationReqsTreeView.GetNext(Node);
    Inc(X);
  end;
  HideoperationMenuItem.Enabled := (HideoperationMenuItem.Count > 0);
  HideAllOperationsAction.Enabled := HideoperationMenuItem.Enabled;
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

procedure TMainForm.SetUiBusy ;
begin
  isBusy := True;
  DownPageControl.ActivePage := MessagesTabSheet;
  ExecuteRequestToolButton.Down := True;
  ExecuteAllRequestsToolButton.Down := True;
  se.ProgressPos := 0;
  Screen.Cursor := crHourGlass;
  abortPressed := False;
  AbortToolButton.Enabled := True;
end;

procedure TMainForm.SetUiReady ;
begin
  ExecuteRequestToolButton.Down := False;
  ExecuteAllRequestsToolButton.Down := False;
  Screen.Cursor := crDefault;
  DownPageControl.ActivePage := MessagesTabSheet;
  se.ProgressPos := 0;
  abortPressed := False;
  AbortToolButton.Enabled := False;
  isBusy := False;
  UpdateInWsdlCheckBoxes;
  GridView.Invalidate;
  InWsdlTreeView.Invalidate;
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
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  Application.CreateForm(TAboutBox, AboutBox);
  try
    AboutBox.ProgName := _progName;
    AboutBox.LicensedTo := xCompanyName;
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
  xFileName := ExtractFilePath(ParamStr(0)) + '\Documentation\' + _progName +
    '.htm';
  if not FileExistsUTF8(xFileName) { *Converted from FileExists* } then
    raise Exception.Create('Could not find helpfile: ' + xFileName);
  if not OpenDocument(xFileName) then
    raise Exception.Create('Could not open ' + xFileName);
end;

procedure TMainForm.HelpMainMenuActionExecute(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + '\Documentation\' + _progName +
    '_Menu_hlp.htm';
  if not FileExistsUTF8(xFileName) { *Converted from FileExists* } then
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

procedure TMainForm.HideoperationMenuItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    allOperations.Operations[Tag].HiddenFromUI := True;
  UpdateVisibiltyOfOperations;
  stubChanged := True;
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
  OpenLogUsageDatabase;
  LogUsage(WindowsUserName);
  ValidateLicense;
  SqlConnector.Connected := False;
  SetLogUsageTimer;
  ConfigListenersAction.Hint := hintStringFromXsd('Configure listeners (',
    ', ', ')', listenersConfigXsd);
  InitMasterServer;
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
  try
    try
      SqlConnector.Connected := False;
    except
    end;
    if FileExistsUTF8(licenseDatabaseName) then
    begin
      try
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
        Close;
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
      Close;
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
        ('Reload refused because master instance of ' + _progName +
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
      xCompanyName := Qry.FieldByName('CompanyName').AsString;
      xLicenseExpirationDate := Qry.FieldByName('LicenseExpireDate').AsString;
      xLicenseString := Qry.FieldByName('LicenseString').AsString;
      Qry.Next;
    end;
    Qry.Close;
    se.Licensed := (validateIpmLicense( xCompanyName
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
      IniFile.IntegerByName['LicenseExpirationDate'] := 10000 * Y + 100 * m + d;
      IniFile.StringByName['LicenseKey'] := generateIpmLicense(IntToStr(10000 * Y + 100 * m + d));
    end;
  end
  else
  begin
    if ErrorReadingLicenseInfo then
    begin
      // first try if we have a stand alone license
      ymd := IniFile.IntegerByName['LicenseExpirationDate'];
      d := ymd mod 100;
      ymd := ymd div 100;
      m := ymd mod 100;
      Y := ymd div 100;
      xLicenseDate := EncodeDate(Y, m, d);
      if (Now <= xLicenseDate) and ((Now + 16) > xLicenseDate) then
        se.Licensed := validateIpmLicense(IntToStr(10000 * Y + 100 * m + d),
          IniFile.StringByName['LicenseKey']);
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
        ' will have limited functionallity and will' + LineEnding +
        'stub only a limited number of requests.' + LineEnding + LineEnding +
        'Please contact your ' + _progName + ' provider.');
  end
  else
  begin
    if ((xDt - Now) < 30) then
      ShowMessage('Your ' + _progName + ' license expires on ' + DateToStr(xDt)
          + LineEnding + 'Please contact your ' + _progName + ' provider');
  end;
end;

procedure TMainForm.License1Click(Sender: TObject);
begin
  if ErrorReadingLicenseInfo then
    raise Exception.Create('' + _progName +
        ' could not read the license information.' + LineEnding + LineEnding +
        'Please contact your ' + _progName + ' provider for assistance.');

  Application.CreateForm(TIpmGunLicenseForm, IpmGunLicenseForm);
  try
    IpmGunLicenseForm.Caption := '' + _progName + ' - License information';
    IpmGunLicenseForm.Company := xCompanyName;
    IpmGunLicenseForm.LicenseExpirationDate := xLicenseExpirationDate;
    IpmGunLicenseForm.DbName := licenseDatabaseName;
    IpmGunLicenseForm.ShowModal;
    if IpmGunLicenseForm.ModalResult = mrOk then
    begin
      OpenLogUsageDatabase;
      try
        try
          Qry.SQL.Clear;
          Qry.SQL.Add('Update LicenseInformation');
          Qry.SQL.Add('set CompanyName = :CompanyName');
          Qry.SQL.Add('  , LicenseExpireDate = :LicenseExpireDate');
          Qry.SQL.Add('  , LicenseString = :LicenseString');
          Qry.Params.ParamValues['CompanyName'] :=
            IpmGunLicenseForm.Company;
          Qry.Params.ParamValues['LicenseExpireDate'] :=
            IpmGunLicenseForm.LicenseExpirationDate;
          Qry.Params.ParamValues['LicenseString'] :=
            IpmGunLicenseForm.LicenseString;
          Qry.Transaction.Active := True;
          Qry.ExecSql;
          Qry.Transaction.Active := False;
          ValidateLicense;
        except
          on E: Exception do
          begin
            ShowMessage(E.Message);
          end;
        end;
      finally
        SqlConnector.Connected := False;
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

procedure TMainForm.LicenseRequestFromServer(aProviderAddress: String);
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  s: String;
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    HttpClient := TIdHTTP.Create;
    try
      HttpRequest := TStringStream.Create('');
      try
        HttpClient.Request.ContentType := 'text/xml';
        HttpClient.Request.CharSet := '';
        try
          if se.doViaProxyServer then
          begin
            HttpClient.ProxyParams.ProxyServer := se.ViaProxyServer;
            HttpClient.ProxyParams.ProxyPort := se.ViaProxyPort;
          end
          else
          begin
            HttpClient.ProxyParams.ProxyServer := '';
            HttpClient.ProxyParams.ProxyPort := 0;
          end;
          try
            s := HttpClient.Post(aProviderAddress + '/ReadLicenseFromServer',
              HttpRequest);
          finally
          end;
          if HttpClient.ResponseCode = 500 then
            raise Exception.Create(s);
        finally
          if HttpClient.Connected then { in case server s-alive }
            HttpClient.Disconnect;
        end;
      finally
        FreeAndNil(HttpRequest);
      end;
    finally
      FreeAndNil(HttpClient);
    end;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TMainForm.runScriptActionExecute(Sender: TObject);
var
  xMenuItem: TMenuItem;
begin
  xMenuItem := nil;
  if Sender is TMenuItem then
    xMenuItem := Sender as TMenuItem;
  if Sender is TAction then with Sender as TAction do
    if ActionComponent is TMenuItem then with ActionComponent as TMenuItem do
      xMenuItem := ActionComponent as TMenuItem;
  if Assigned (xMenuItem) then
    xMenuItem.OnClick (xMenuItem);
end;

procedure TMainForm.runScriptActionUpdate(Sender: TObject);
begin
  runScriptAction.Enabled := se.IsActive and not se.isBusy;
end;

procedure TMainForm.PrepareOperation;
begin
  se.PrepareAllOperations(LogServerException);
  WsdlOperationsComboBox.Clear;
  WsdlServicesComboBox.Clear;
  WsdlComboBox.Items.Text := se.Wsdls.Text;
  FillOperationReqsTreeView(OperationReqsTreeView, allOperations);
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
  MessagesVTS.Clear;
  LogMemo.Clear;
  GridView.Clear;
  ExceptionMemo.Clear;
  ExceptionsVTS.Clear;
  OperationReqsTreeView.Clear;
  DataTypeDocumentationMemo.Clear;
  // InWSdlEnumerationsListView.Clear;
  InWsdlPropertiesListView.Clear;
  OperationDocumentationEdit.Clear;
  InWsdlTreeView.Clear;
  WsdlServicesComboBox.Clear;
  WsdlOperationsComboBox.Clear;
  WsdlComboBox.Clear;
  se.isMasterModeEnabled := False;
  while MessagesVTS.Header.Columns.Count > Ord(logStdColumnCount) do
    MessagesVTS.Header.Columns.Delete(MessagesVTS.Header.Columns.Count - 1);
end;

procedure TMainForm.UpdateConsole(aIndex: Integer);
begin
  WsdlComboBox.ItemIndex := aIndex;
  WsdlPopulateServices(Wsdl);
end;

procedure TMainForm.OpenStubCaseActionUpdate(Sender: TObject);
begin
  OpenStubCaseAction.Enabled := not se.IsActive;
end;

procedure TMainForm.SaveStubCaseAsActionUpdate(Sender: TObject);
begin
  // SaveStubCaseAsAction.Enabled := not se.IsActive;
end;

procedure TMainForm.ReopenStubCaseActionUpdate(Sender: TObject);
begin
  ReopenStubCaseAction.Enabled := not se.IsActive;
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
  OpenWsdlAction.Enabled := not se.IsActive;
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
    DownPageControl.ActivePage := MessagesTabSheet;
    MessagesTabControl.TabIndex := Ord(slRequestBody);
    if se.IsActive then
    begin
      Application.Title := '' + _progName + ' (Active)';
      startAction.ShortCut := 0;
      stopAction.ShortCut := startStopShortCut;
      startStopButton.Action := stopAction;
    end;
    if isSlaveMode then
      Application.Title := '' + _progName + ' (Slave)'
    else
      RefreshTimer.Enabled := False;
    if nStubs > freeStubs then
      freeStubs := nStubs + 10 + Random(10);
  end
  else
  begin
    Application.Title := '' + _progName + '';
    RefreshTimer.Enabled := False;
    stopAction.ShortCut := 0;
    startAction.ShortCut := startStopShortCut;
    startStopButton.Action := startAction;
  end;
  httpRequestDesignButton.Visible := isSlaveMode;
  MasterDesignMenuItem.Visible := isSlaveMode;
  MasterMessagesMenuItem.Visible := isSlaveMode;
  MasterMessagesToolButton.Visible := isSlaveMode;
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
    with result.AddXml(TXml.CreateAsString('MasterSlave', '')) do
    begin
      AddXml(TXml.CreateAsBoolean('SlaveEnabled', isSlaveMode));
      AddXml(TXml.CreateAsString('MasterAddress', MasterAddress));
      AddXml(TXml.CreateAsBoolean('AutoRefresh', AutoRefreshSlave));
      AddXml(TXml.CreateAsBoolean('LoadOnStartup', se.doLoadFromMasterOnStartUp)
        );
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
end;

procedure TMainForm.OptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := OptionsAsXml;
  if EditXmlXsdBased('Options', '', '', '', se.IsActive, optionsXsd, xXml)
    then
  begin
    isOptionsChanged := True;
    OptionsFromXml(xXml);
    RefreshTimer.Enabled :=
      RefreshTimer.Enabled and isSlaveMode and AutoRefreshSlave;
    CheckBoxClick(nil);
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
    OperationDelayResponseTimeAction.Visible :=
      (WsdlOperation.StubAction <> saRequest);
    if (WsdlOperation.DelayTimeMsMin = 0) and
      (WsdlOperation.DelayTimeMsMax = 0) then
      OperationDelayResponseTimeAction.ImageIndex := 60
    else
      OperationDelayResponseTimeAction.ImageIndex := 61;
    RedirectAddressAction.Visible := (WsdlOperation.StubAction = saRedirect) or
      (WsdlOperation.StubAction = saRequest);
    ExecuteRequestToolButton.Visible := (WsdlOperation.StubAction = saRequest);
    ExecuteAllRequestsToolButton.Visible :=
      (WsdlOperation.StubAction = saRequest);
    ExecuteRequestMenuItem.Visible := (WsdlOperation.StubAction = saRequest);
    ExecuteAllRequestsMenuItem.Visible :=
      (WsdlOperation.StubAction = saRequest);
    if (WsdlOperation.StubAction = saStub) then
    begin
      EditScriptButton.Caption := 'Script';
      AfterRequestScriptButton.Visible := False;
    end
    else
    begin
      EditScriptButton.Caption := 'Before';
      AfterRequestScriptButton.Visible := True;
    end;
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

procedure TMainForm.CopyGridOnGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
begin
  CellText := '';
  NodeToMessage(Sender, Node, xMessage);
  if Column = 0 then
  begin
    if Assigned(xMessage) then
      CellText := xMessage.Name;
  end
  else
  begin
    if Assigned(xMessage) then
    begin
      if Column <= xMessage.corBinds.Count then
        try
          CellText := xMessage.corBinds.Bindables[Column - 1].CorrelationValue;
        except
        end
      else
      begin
        try
          xBind := xMessage.ColumnXmls.Bindables
            [Column - xMessage.corBinds.Count - 1];
          if (not Assigned(xBind)) or
            ((not(xBind is TIpmItem)) and (not xBind.Parent.CheckedAllUp)) then
            CellText := '?'
          else
            CellText := xBind.GetStringData;
        except
          CellText := '?';
        end;
      end;
    end;
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
  try
    case Kind of
      ikNormal, ikSelected:
        begin
          NodeToMessage(Sender, Node, xMessage);
          if not Assigned(xMessage) then
            exit;
          if Column = 0 then
            exit
          else
          begin
            if Column <= xMessage.corBinds.Count then
              exit
            else
            begin
              try
                xBind := xMessage.ColumnXmls.Bindables
                  [Column - xMessage.corBinds.Count - 1];
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
                  if xBind is TIpmItem then
                  begin
                    // tbd
                  end;
                end
                else
                  exit;
              except
                on E: Exception do
                  exit;
              end;
            end;
          end;
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
  NodeToMessage(Sender, Node, xMessage);
  if not Assigned(xMessage) then
    exit;
  try
    if Column = 0 then
      CellText := xMessage.Name
    else
    begin
      if Column <= xMessage.corBinds.Count then
        try
          if Assigned (xMessage.corBinds.Bindables[Column - 1]) then
            CellText := xMessage.corBinds.Bindables[Column - 1].CorrelationValue
          else
            CellText := '?';
        except
        end
      else
      begin
        xBind := xMessage.ColumnXmls.Bindables
          [Column - xMessage.corBinds.Count - 1];
        if Assigned(xBind) then
        begin
          if ((xBind is TIpmItem) or (not Assigned(xBind.Parent)) or
              (xBind.Parent.CheckedAllUp)) then
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
      xData.Reply := xMessage;
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
    if Column <= xMessage.corBinds.Count then
    begin
      if NewText <> xMessage.corBinds.Bindables[Column - 1]
        .CorrelationValue then
      begin
        if Node = Sender.GetFirst then
          _RaiseError('Not allowed to change this pattern into ' + NewText)
        else
        begin
          xMessage.corBinds.Bindables[Column - 1].CorrelationValue := NewText;
          stubChanged := True;
        end;
      end;
    end
    else
    begin
      if (NewText <> '?') and
        (Assigned(xMessage.ColumnXmls.Bindables
            [Column - xMessage.corBinds.Count - 1])) then
      begin
        xBind := xMessage.ColumnXmls.Bindables
          [Column - xMessage.corBinds.Count - 1];
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
  WsdlOperation.AcquireLock;
  try
    NodeToMessage(Sender, Node, xMessage);
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
      if Column <= xMessage.corBinds.Count then
      begin
        xNode := GridView.GetFirstSelected;
        while Assigned(xNode) do
        begin
          NodeToMessage(Sender, xNode, xMessage);
          if NewText <> xMessage.corBinds.Bindables[Column - 1]
            .CorrelationValue then
          begin
            if xNode = Sender.GetFirst then
            begin
              Node := xNode;
              _RaiseError('Not allowed to change this pattern into ' + NewText);
            end;
            xMessage.corBinds.Bindables[Column - 1].CorrelationValue := NewText;
            stubChanged := True;
          end;
          xNode := GridView.GetNextSelected(xNode);
        end;
      end
      else
      begin
        { }{
          if (Assigned (xMessage.ColumnXmls.Bindables[Column - xMessage.corBinds.Count - 1])) then
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
            [Column - xMessage.corBinds.Count - 1];
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
begin
  { }{
    if se.IsActive then
    begin
    Allowed := False;
    exit;
    end;
    { }
  NodeToMessage(Sender, Node, xMessage);
  editingNode := InWsdlTreeView.FocusedNode;
  if Column = 0 then
  begin
    Allowed := (Node <> Sender.GetFirst) and (GridView.SelectedCount = 1);
    exit;
  end;
  if Column <= xMessage.corBinds.Count then
  begin
    Allowed := (Node <> Sender.GetFirst);
    exit;
  end;
  Allowed := Assigned(xMessage.ColumnXmls.Bindables
      [Column - xMessage.corBinds.Count - 1]);
  if Allowed then
  begin
    swapEvent := InWsdlTreeView.OnFocusChanged;
    try
      InWsdlTreeView.OnFocusChanged := nil;
      FocusOnBind(xMessage.ColumnXmls.Bindables
          [Column - xMessage.corBinds.Count - 1]);
    finally
      InWsdlTreeView.OnFocusChanged := swapEvent;
    end;
  end;
end;

procedure TMainForm.SelectCorrelationElementActionUpdate(Sender: TObject);
begin
  SelectCorrelationElementAction.Enabled := Assigned(WsdlOperation) and
    (not se.IsActive) and (WsdlOperation.WsdlService.DescriptionType <>
      ipmDTFreeFormat)
  // and (WsdlOperation.StubAction <> saRequest)
    ;
end;

procedure TMainForm.SetIsBusy(const Value: Boolean);
begin
  fIsBusy := Value;
  se.isBusy := Value;
end;

procedure TMainForm.SelectCorrelationElementActionExecute(Sender: TObject);
var
  swapBindable: TCustomBindable;
begin
  EndEdit;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  SelectElementsForm.Caption := 'Maintain list of correlation elements';
  try
    GridView.BeginUpdate;
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    swapBindable := WsdlOperation.rpyBind;
    try
      WsdlOperation.rpyBind := nil;
      SelectElementsForm.ControlBinds := WsdlOperation.CorrelationBindables;
      SelectElementsForm.ShowModal;
      begin
        se.UpdateReplyColumns(WsdlOperation);
        UpdateMessagesGrid;
        stubChanged := stubChanged or SelectElementsForm.stubChanged;
      end;
    finally
      WsdlOperation.rpyBind := swapBindable;
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
  EndEdit;
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
  if WsdlOperation.reqBind is TIpmItem then
  begin (xNewMessage.reqBind as TIpmItem)
    .LoadValues(xOrgMessage.reqBind as TIpmItem);
(xNewMessage.rpyBind as TIpmItem)
    .LoadValues(xOrgMessage.rpyBind as TIpmItem);
(xNewMessage.fltBind as TIpmItem)
    .LoadValues(xOrgMessage.fltBind as TIpmItem);
    se.UpdateMessageRow(WsdlOperation, xNewMessage);
  end
  else
  begin (xNewMessage.reqBind as TXml)
    .LoadValues(xOrgMessage.reqBind as TXml, False, True);
(xNewMessage.rpyBind as TXml)
    .LoadValues(xOrgMessage.rpyBind as TXml, False, True);
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
  xData.Reply := xNewMessage;
end;

procedure TMainForm.GridViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  xMessage: TWsdlMessage;
  swapEvent: TVTFocusChangeEvent;
  swapNotifyEvent, swapMemoEvent: TNotifyEvent;
  rpyNode: PVirtualNode;
begin
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
            FreeFormatMemo.Text := WsdlReply.FreeFormatReq
          else
            FreeFormatMemo.Text := WsdlReply.FreeFormatRpy;
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
        if Column > xMessage.corBinds.Count then
        begin
          swapEvent := GridView.OnFocusChanged;
          try
            GridView.OnFocusChanged := nil;
            FocusOnBind
              (xMessage.ColumnXmls.Bindables
                [Column - xMessage.corBinds.Count - 1]);
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
    DeleteMessageAction.Enabled := Assigned(FocusedNode) and not
      (FocusedNode = GetFirst);
end;

procedure TMainForm.DeleteMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  cNode, nNode: PVirtualNode;
begin
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
        ExchangeMessages(fData.Reply, pData.Reply);
        xMessage := pData.Reply;
        pData.Reply := fData.Reply;
        fData.Reply := xMessage;
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
      and not Selected[GetFirst] and not Selected[GetNext(GetFirst)];
end;

procedure TMainForm.MoveDownMessageActionUpdate(Sender: TObject);
begin
  with GridView do
    MoveDownMessageAction.Enabled := (SelectedCount > 0)
      and not Selected[GetFirst] and not Selected[GetLast];
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
          ExchangeMessages(fData.Reply, nData.Reply);
          xMessage := nData.Reply;
          nData.Reply := fData.Reply;
          fData.Reply := xMessage;
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
    d.DelayTimeMsMin := s.DelayTimeMsMin;
    d.DelayTimeMsMax := s.DelayTimeMsMax;
    d.StubTransport := s.StubTransport;
    d.StubHttpAddress := s.StubHttpAddress;
    d.httpVerb := s.httpVerb;
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
    d.AsynchronousDialog := s.AsynchronousDialog;
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
var
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
  expXml: TXml;
begin
  try
    NodeToMessage(Sender, Node, xMessage);
    if not Assigned(xMessage) then
      exit;
    {
      if Column = 0 then
      begin
      if xMessage.Disabled
      and (Node = Sender.GetFirst) then with TargetCanvas do
      begin
      Brush.Style := bsSolid;
      Brush.Color := clFuchsia;
      FillRect( CellRect );
      end;
      Exit;
      end;
      }
    NodeToMessage(Sender, Node, xMessage);
    if not Assigned(xMessage) then
      exit;
    if Column <= xMessage.corBinds.Count then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := bgCorrelationItemColor;
        FillRect(CellRect);
      end;
      exit;
    end;
    try
      xBind := xMessage.ColumnXmls.Bindables
        [Column - xMessage.corBinds.Count - 1];
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
          Brush.Color := bgExpectedValueColor;
          FillRect(CellRect);
        end;
        exit;
      end;
      if (not Assigned(xBind)) or not(xBind.CheckedAllUp) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := bgNilValueColor;
          FillRect(CellRect);
        end;
        exit;
      end;
    end;
  except
    exit;
  end;
end;

procedure TMainForm.GridViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column > WsdlOperation.CorrelationBindables.Count then
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
  if stubChanged and Assigned(Wsdl) then
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

procedure TMainForm.SaveStubCaseActionUpdate(Sender: TObject);
begin
  // SaveStubCaseAction.Enabled := not se.IsActive;
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

procedure TMainForm.RedirectAddressActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned(WsdlOperation) then
    raise Exception.Create('No operation selected');
  with WsdlOperation do
  begin
    xXml := endpointConfigAsXml;
    if EditXmlXsdBased('Configure Endpoint', '', '', '', False,
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
    if Assigned(se) and Assigned(Wsdl) then
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
    allOperations.Operations[Tag].HiddenFromUI := False;
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
begin
  WsdlComboBox.Width := (Sender as TPanel).Width - WsdlComboBox.Left - 5;
  WsdlServicesComboBox.Width := (Sender as TPanel).Width div 2 -
    WsdlServicesComboBox.Left - 5 - 17;
  OperationLabel.Left := WsdlServicesComboBox.Left +
    WsdlServicesComboBox.Width + 2;
  WsdlOperationsComboBox.Left := OperationLabel.Left + OperationLabel.Width + 2;
  WsdlOperationsComboBox.Width := (Sender as TPanel)
    .Width - WsdlOperationsComboBox.Left - 5;
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
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'MQ Message Descriptor';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'MQ Request Body';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'MQ Message Headers';
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
      MessagesStatusBar.Panels.Items[0].Text := '[' + IntToStr(xLog.Nr)
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
              if xData.Reply = xLog.Mssg then
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
      MessagesStatusBar.Panels.Items[0].Text := '';
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
        logServiceColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.WsdlService.Name;
        logOperationColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.Name;
        logCorrelationIdColumn:
          CellText := xLog.CorrelationId;
      end;
      if (Column >= Ord(logStdColumnCount)) and Assigned(xLog.Operation) then
      begin
        if not xLog.DisplayedColumnsValid then
        begin
          xLog.toBindables;
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
    if (not xmlUtil.doConfirmRemovals) or BooleanPromptDialog
      ('Remove all messages') then
    begin
      se.AcquireLogLock;
      try
        MessagesVTS.Clear;
        se.AsynchRpyLogs.Clear;
        se.displayedLogs.Clear;
        LogMemo.Text := '';
      finally
        se.ReleaseLogLock;
      end;
    end;
  end;
end;

procedure TMainForm.NewStubCaseActionUpdate(Sender: TObject);
begin
  NewStubCaseAction.Enabled := not se.IsActive;
end;

procedure TMainForm.EditScript(X: Integer);
var
  xOperation: TWsdlOperation;
begin
  xOperation := se.CreateScriptOperation((se.Scripts.Objects[X] as TStringList).Text);
  try
    if xOperation.PrepareErrors <> '' then
      if not BooleanPromptDialog (xOperation.PrepareErrors + LineEnding + 'Continue') then
        Exit;
    Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
    try
      EditOperationScriptForm.ScriptName := xOperation.Name;
      EditOperationScriptForm.After := False;
      EditOperationScriptForm.WsdlOperation := xOperation;
      EditOperationScriptForm.ShowModal;
      if EditOperationScriptForm.ModalResult = mrOk then
      begin
        stubChanged := True;
        (se.Scripts.Objects[X] as TStringList).Text := xOperation.BeforeScriptLines.Text;
      end;
      FillInWsdlEdits;
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
begin
  with Sender as TMenuItem do
    EditScript(Tag);
end;

procedure TMainForm.New2Click(Sender: TObject);
var
  f: Integer;
begin
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Name for new script';
    PromptForm.PromptEdit.Text := '';
    PromptForm.Numeric := False;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
    begin
      if PromptForm.PromptEdit.Text = '' then
        raise Exception.Create('Script name must have a value');
      if se.Scripts.Find(PromptForm.PromptEdit.Text, f) then
        raise Exception.Create(Format('A script with name %s already exists',
            [PromptForm.PromptEdit.Text]));
      se.Scripts.Objects[se.Scripts.Add(PromptForm.PromptEdit.Text)] :=
        TStringList.Create;
      CreateScriptsSubMenuItems;
      stubChanged := True;
      if se.Scripts.Find(PromptForm.PromptEdit.Text, f) then
        EditScript(f);
    end;
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm.NewStubCaseActionExecute(Sender: TObject);
begin
  EndEdit;
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

procedure TMainForm.Notify(aString: String);
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
        0:
          CellText := xOperation.reqTagName;
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
        wsaConfigForm.AsynchronousDialog := WsdlOperation.AsynchronousDialog;
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
          WsdlOperation.AsynchronousDialog := wsaConfigForm.AsynchronousDialog;
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
  OperationWsaAction.Enabled := (not se.IsActive) and Assigned(_WsdlWsaXsd)
    and Assigned(WsdlOperation);
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
  SelectMessageColumnsAction.Enabled := Assigned(WsdlOperation) and
    (not se.IsActive);
end;

procedure TMainForm.SelectMessageColumnsActionExecute(Sender: TObject);
begin
  EndEdit;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  try
    GridView.BeginUpdate;
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    SelectElementsForm.ControlBinds := WsdlOperation.Messages.Messages[0]
      .ColumnXmls;
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
  GridView.FocusedColumn := 0;
  for c := GridView.Header.Columns.Count - 1 downto 0 do
    ColumnWidths.Values[GridView.Header.Columns[c].Text] := IntToStr
      (GridView.Header.Columns[c].Width);
  if Assigned(WsdlOperation) then
  begin
    try
      while GridView.Header.Columns.Count >
        (1 + WsdlOperation.CorrelationBindables.Count +
          WsdlOperation.Messages.Messages[0].ColumnXmls.Count) do
        GridView.Header.Columns.Delete(GridView.Header.Columns.Count - 1);
      while GridView.Header.Columns.Count <
        (1 + WsdlOperation.CorrelationBindables.Count +
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
  c := 0;
  vc := GridView.Header.Columns.Items[c];
  if WsdlOperation.StubAction = saRequest then
    vc.Text := 'Request'
  else
    vc.Text := 'Reply';
  vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
  Inc(c);
  with WsdlOperation.CorrelationBindables do
  begin
    for X := 0 to Count - 1 do
    begin
      vc := GridView.Header.Columns.Items[c];
      if Assigned(Bindables[X]) then
        vc.Text := TXml(Bindables[X]).TagName
      else
        vc.Text := '?';
      vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
      Inc(c);
    end;
  end;
  if WsdlOperation.Messages.Count > 0 then
  begin
    with WsdlOperation.Messages.Messages[0] do
    begin
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

procedure TMainForm.setWsdlReply(const Value: TWsdlMessage);
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
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

function TMainForm.getWsdlReply: TWsdlMessage;
begin
  NodeToMessage(GridView, GridView.FocusedNode, result);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  X, wBttn: Integer;
begin
  DataTypeDocumentationMemo.Color := Self.Color;
  (MessagesTabControl as TWinControl).Color := Self.Color;
  logTabCaption := MessagesTabSheet.Caption;
  notifyTabCaption := ExceptionTabSheet.Caption;
  notifyTabImageIndex := 66;
  ExceptionTabSheet.ImageIndex := -1;
  se := TWsdlProject.Create;
  se.OnBusy := SetUiBusy;
  se.OnReady := SetUiReady;
  se.OnActivateEvent := ActivateCommand;
  se.OnOpenProjectEvent := OpenProjectCommand;
  se.Notify := Notify;
  se.LogServerMessage := LogServerException;
  se.OnDebugOperationEvent := DebugOperation;
  se.FoundErrorInBuffer := FoundErrorInBuffer;
  se.OnClearLogEvent := ClearLogCommand;
  se.OnReactivateEvent := ReactivateCommand;
  se.OnQuitEvent := QuitCommand;
  se.OnRestartEvent := RestartCommand;
  se.OnReloadDesignEvent := ReloadDesignCommand;
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
  IniFile := TFormIniFile.Create(Self);
  IniFile.Restore;
  ViewStyleComboBox.ItemIndex := Ord(xvAll);
  for X := 0 to Ord(logTimeColumn) - 1 do
    MessagesVTS.Header.Columns[X].Width := wBttn;
  se.projectFileName := IniFile.StringByName['WsdlStubFileName'];
  wsdlStubMessagesFileName := IniFile.StringByName['WsdlStubMessagesFileName'];
  DisclaimerAccepted := IniFile.BooleanByName['DisclaimerAccepted'];
  BetaMode := IniFile.BooleanByNameDef['BetaMode', False];
  ListofOperationsMenuItem.Checked := IniFile.BooleanByNameDef
    ['ListofOperationsVisible', True];
  ScriptPanel.Visible := ListofOperationsMenuItem.Checked;
  ScriptSplitter.Visible := ListofOperationsMenuItem.Checked;
  SchemapropertiesMenuItem.Checked := IniFile.BooleanByNameDef
    ['SchemaPropertiesVisible', True];
  XsdPanel.Visible := SchemapropertiesMenuItem.Checked;
  xsdSplitter.Visible := SchemapropertiesMenuItem.Checked;
  WsdlInformationMenuItem.Checked := IniFile.BooleanByNameDef
    ['WsdlInformationVisible', True];
  WsdlInfoPanel.Visible := WsdlInformationMenuItem.Checked;
  se.LogFilter.FilterStyle := TLogFilterStyle
    (IniFile.IntegerByNameDef['LogFilter.FilterStyle', 0]);
  se.LogFilter.MatchAny := IniFile.BooleanByNameDef['LogFilter.MatchAny',
    False];
  se.LogFilter.StubActionEnabled := IniFile.BooleanByNameDef
    ['LogFilter.StubActionEnabled', False];
  se.LogFilter.StubActionEquals := IniFile.BooleanByNameDef
    ['LogFilter.StubActionEquals', True];
  se.LogFilter.StubAction := TStubAction
    (IniFile.IntegerByNameDef['LogFilter.StubAction', 0]);
  se.LogFilter.MiMEnabled := IniFile.BooleanByNameDef['LogFilter.MiMEnabled',
    False];
  se.LogFilter.RequestMiMEnabled := IniFile.BooleanByNameDef
    ['LogFilter.RequestMiMEnabled', True];
  se.LogFilter.ReplyMiMEnabled := IniFile.BooleanByNameDef
    ['LogFilter.ReplyMiMEnabled', True];
  se.LogFilter.MessageValidationEnabled := IniFile.BooleanByNameDef
    ['LogFilter.MessageValidationEnabled', False];
  se.LogFilter.RequestValidationEnabled := IniFile.BooleanByNameDef
    ['LogFilter.RequestValidationEnabled', True];
  se.LogFilter.ReplyValidationEnabled := IniFile.BooleanByNameDef
    ['LogFilter.ReplyValidationEnabled', True];
  se.LogFilter.ExceptionEnabled := IniFile.BooleanByNameDef
    ['LogFilter.ExceptionEnabled', False];
  se.LogFilter.ExceptionEquals := IniFile.BooleanByNameDef
    ['LogFilter.ExceptionEquals', False];
  se.LogFilter.Exception := IniFile.StringByNameDef['LogFilter.Exception', ''];
  se.LogFilter.ExceptionRegExp := IniFile.BooleanByNameDef
    ['LogFilter.ExceptionRegExp', False];
  se.LogFilter.ServiceEnabled := IniFile.BooleanByNameDef
    ['LogFilter.ServiceEnabled', False];
  se.LogFilter.ServiceEquals := IniFile.BooleanByNameDef
    ['LogFilter.ServiceEquals', True];
  se.LogFilter.Service := IniFile.StringByNameDef['LogFilter.Service', ''];
  se.LogFilter.ServiceRegExp := IniFile.BooleanByNameDef
    ['LogFilter.ServiceRegExp', False];
  se.LogFilter.OperationEnabled := IniFile.BooleanByNameDef
    ['LogFilter.OperationEnabled', False];
  se.LogFilter.OperationEquals := IniFile.BooleanByNameDef
    ['LogFilter.OperationEquals', True];
  se.LogFilter.Operation := IniFile.StringByNameDef['LogFilter.Operation', ''];
  se.LogFilter.OperationRegExp := IniFile.BooleanByNameDef
    ['LogFilter.OperationRegExp', False];
  se.LogFilter.CorrelationEnabled := IniFile.BooleanByNameDef
    ['LogFilter.CorrelationEnabled', False];
  se.LogFilter.CorrelationEquals := IniFile.BooleanByNameDef
    ['LogFilter.CorrelationEquals', True];
  se.LogFilter.Correlation := IniFile.StringByNameDef['LogFilter.Correlation',
    ''];
  se.LogFilter.CorrelationRegExp := IniFile.BooleanByNameDef
    ['LogFilter.CorrelationRegExp', False];
  se.LogFilter.RequestEnabled := IniFile.BooleanByNameDef
    ['LogFilter.RequestEnabled', False];
  se.LogFilter.RequestEquals := IniFile.BooleanByNameDef
    ['LogFilter.RequestEquals', True];
  se.LogFilter.Request := IniFile.StringByNameDef['LogFilter.Request', ''];
  se.LogFilter.ReplyEnabled := IniFile.BooleanByNameDef
    ['LogFilter.ReplyEnabled', False];
  se.LogFilter.ReplyEquals := IniFile.BooleanByNameDef['LogFilter.ReplyEquals',
    True];
  se.LogFilter.Reply := IniFile.StringByNameDef['LogFilter.Reply', ''];
  se.LogFilter.UnexpectedValuesEnabled := IniFile.BooleanByNameDef
    ['LogFilter.UnexpectedValuesEnabled', False];
  se.LogFilter.RemarksEnabled := IniFile.BooleanByNameDef
    ['LogFilter.RemarksEnabled', False];
  CollapseHeaders := IniFile.BooleanByNameDef['CollapseHeaders', True];
  InWsdlTreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  InWsdlTreeView.RootNodeCount := 0;
  OperationReqsTreeView.NodeDataSize := SizeOf(TOperationTreeRec);
  OperationReqsTreeView.RootNodeCount := 0;
  GridView.NodeDataSize := SizeOf(TMessageTreeRec);
  GridView.RootNodeCount := 0;
  MessagesVTS.NodeDataSize := SizeOf(TLogTreeRec);
  MessagesVTS.RootNodeCount := 0;
  ExceptionsVTS.NodeDataSize := SizeOf(TExceptionTreeRec);
  ExceptionsVTS.RootNodeCount := 0;
  FileNameList := TStringList.Create;
  ReopenCaseList := TStringList.Create;
  ReopenCaseList.Text := IniFile.StringByName['RecentFiles'];
  WsdlPaths := TStringList.Create;
  WsdlPaths.Sorted := True;

  se.notStubbedExceptionMessage := IniFile.StringByNameDef
    ['notStubbedExceptionMessage', 'No operation recognized'];
  se.doViaProxyServer := IniFile.BooleanByName['doViaProxyServer'];
  doScrollMessagesIntoView := IniFile.BooleanByNameDef
    ['doScrollMessagesIntoView', True];
  doScrollExceptionsIntoView := IniFile.BooleanByNameDef
    ['doScrollExceptionsIntoView', True];
  xmlUtil.doConfirmRemovals := IniFile.BooleanByNameDef['doConfirmRemovals',
    True];
  xsdValidateAssignmentsAgainstSchema := IniFile.BooleanByNameDef
    ['doValidateScriptAssignmentAgainstSchema', False];
  // se.HTTPServer.KeepAlive := IniFile.BooleanByNameDef ['HTTPServer.KeepAlive', se.HTTPServer.KeepAlive];
  se.HTTPServer.ListenQueue := IniFile.IntegerByNameDef
    ['HTTPServer.ListenQueue',
    se.HTTPServer.ListenQueue];
  se.HTTPServer.MaxConnections := IniFile.IntegerByNameDef
    ['HTTPServer.MaxConnections', se.HTTPServer.MaxConnections];
  se.ViaProxyServer := IniFile.StringByNameDef['ViaProxyServer', 'localhost'];
  se.ViaProxyPort := StrToIntDef(IniFile.StringByName['ViaProxyPort'], 8081);
  if (se.mmqqMqInterface.MQServerOK and se.mmqqMqInterface.MQClientOK) then
    se.mqUse := TMqUse(StrToIntDef(IniFile.StringByName['mqUse'],
        Ord(mquServer)));
  if (se.mmqqMqInterface.MQServerOK and (not se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquServer;
  if (not se.mmqqMqInterface.MQServerOK and (se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquClient;
  if (not se.mmqqMqInterface.MQServerOK and (not se.mmqqMqInterface.MQClientOK)) then
    se.mqUse := mquUndefined;
  se.mqMaxWorkingThreads := IniFile.IntegerByNameDef['MaxWorkingThreads', 15];
  se.CompareLogOrderBy := TCompareLogOrderBy
    (IniFile.IntegerByNameDef['CompareLogOrderBy', Ord(clTimeStamp)]);
  se.ShowLogCobolStyle := TShowLogCobolStyle
    (IniFile.IntegerByNameDef['ShowLogCobolStyle', Ord(slCobol)]);
  mqServerEnv := GetEnvironmentVariable('MQSERVER');
  ColumnWidths.Text := IniFile.StringByNameDef['ColumnWidths', ''];
  isSlaveMode := IniFile.BooleanByNameDef['isSlaveMode', False];
  MasterAddress := IniFile.StringByName['MasterAddress'];
  AutoRefreshSlave := IniFile.BooleanByNameDef['AutoRefreshSlave', False];
  se.doLoadFromMasterOnStartUp := IniFile.BooleanByNameDef
    ['doLoadFromMasterOnStartUp', False];
  xsdElementsWhenRepeatable := StrToIntDef
    (IniFile.StringByName['ElementsWhenRepeatable'], 1);
  doShowDesignAtTop := IniFile.BooleanByNameDef['doShowDesignAtTop', True];
  bgCorrelationItemColor := IniFile.IntegerByNameDef['bgCorrelationItemColor',
    bgCorrelationItemColor];
  bgExpectedValueColor := IniFile.IntegerByNameDef['bgExpectedValueColor',
    bgExpectedValueColor];
  bgNilValueColor := IniFile.IntegerByNameDef['bgNilValueColor',
    bgNilValueColor];
  DesignPanelAtTopMenuItem.Checked := doShowDesignAtTop;
  wsdlStubInitialise;
  se.stubRead := False;
  stubChanged := False;
  nStubs := 0;
  freeStubs := -1;
  ExecuteRequestToolButton.Visible := False;
  ExecuteAllRequestsToolButton.Visible := False;
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
  Wsdlz.OnNotify := LogServerNotification;
  if ParamStr(1) <> '' then
  begin
    Update;
    se.projectFileName := ParamStr(1);
    OpenStubCase(se.projectFileName);
    se.Activate(True);
    CheckBoxClick(nil);
  end;
  MainToolBarDesignedButtonCount := MainToolBar.ButtonCount;
  CreateScriptsSubMenuItems;
  RefreshLogTimer.Enabled := True;
  systemStarting := False;
end;

function TMainForm.inImageArea: Boolean;
var
  xRect: TRect;
  xImageIndex: Integer;
  xGosthed: Boolean;
begin
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
begin
  if Assigned(se) then
    se.Activate(False);
  ClearConsole;
  IniFile.BooleanByName['DisclaimerAccepted'] := DisclaimerAccepted;
  IniFile.BooleanByName['BetaMode'] := BetaMode;
  IniFile.BooleanByName['doShowDesignAtTop'] := doShowDesignAtTop;
  IniFile.StringByName['RecentFiles'] := ReopenCaseList.Text;
  IniFile.StringByName['ColumnWidths'] := ColumnWidths.Text;
  IniFile.BooleanByName['ListofOperationsVisible'] :=
    ListofOperationsMenuItem.Checked;
  IniFile.BooleanByName['SchemaPropertiesVisible'] :=
    SchemapropertiesMenuItem.Checked;
  IniFile.BooleanByName['WsdlInformationVisible'] :=
    WsdlInformationMenuItem.Checked;
  IniFile.StringByName['WsdlStubFileName'] := se.projectFileName;
  IniFile.StringByName['WsdlStubMessagesFileName'] := wsdlStubMessagesFileName;
  // IniFile.BooleanByName ['LogFilter.Enabled'] := se.LogFilter.Enabled;
  IniFile.IntegerByName['LogFilter.FilterStyle'] := Ord
    (se.LogFilter.FilterStyle);
  IniFile.BooleanByName['LogFilter.MatchAny'] := se.LogFilter.MatchAny;
  IniFile.BooleanByName['LogFilter.StubActionEnabled'] :=
    se.LogFilter.StubActionEnabled;
  IniFile.BooleanByName['LogFilter.StubActionEquals'] :=
    se.LogFilter.StubActionEquals;
  IniFile.IntegerByName['LogFilter.StubAction'] := Ord(se.LogFilter.StubAction);
  IniFile.BooleanByName['LogFilter.MiMEnabled'] := se.LogFilter.MiMEnabled;
  IniFile.BooleanByName['LogFilter.RequestMiMEnabled'] :=
    se.LogFilter.RequestMiMEnabled;
  IniFile.BooleanByName['LogFilter.ReplyMiMEnabled'] :=
    se.LogFilter.ReplyMiMEnabled;
  IniFile.BooleanByName['LogFilter.MessageValidationEnabled'] :=
    se.LogFilter.MessageValidationEnabled;
  IniFile.BooleanByName['LogFilter.RequestValidationEnabled'] :=
    se.LogFilter.RequestValidationEnabled;
  IniFile.BooleanByName['LogFilter.ReplyValidationEnabled'] :=
    se.LogFilter.ReplyValidationEnabled;
  IniFile.BooleanByName['LogFilter.ExceptionEnabled'] :=
    se.LogFilter.ExceptionEnabled;
  IniFile.BooleanByName['LogFilter.ExceptionEquals'] :=
    se.LogFilter.ExceptionEquals;
  IniFile.StringByName['LogFilter.Exception'] := se.LogFilter.Exception;
  IniFile.BooleanByName['LogFilter.ExceptionRegExp'] :=
    se.LogFilter.ExceptionRegExp;
  IniFile.BooleanByName['LogFilter.ServiceEnabled'] :=
    se.LogFilter.ServiceEnabled;
  IniFile.BooleanByName['LogFilter.ServiceEquals'] :=
    se.LogFilter.ServiceEquals;
  IniFile.StringByName['LogFilter.Service'] := se.LogFilter.Service;
  IniFile.BooleanByName['LogFilter.ServiceRegExp'] :=
    se.LogFilter.ServiceRegExp;
  IniFile.BooleanByName['LogFilter.OperationEnabled'] :=
    se.LogFilter.OperationEnabled;
  IniFile.BooleanByName['LogFilter.OperationEquals'] :=
    se.LogFilter.OperationEquals;
  IniFile.StringByName['LogFilter.Operation'] := se.LogFilter.Operation;
  IniFile.BooleanByName['LogFilter.OperationRegExp'] :=
    se.LogFilter.OperationRegExp;
  IniFile.BooleanByName['LogFilter.CorrelationEnabled'] :=
    se.LogFilter.CorrelationEnabled;
  IniFile.BooleanByName['LogFilter.CorrelationEquals'] :=
    se.LogFilter.CorrelationEquals;
  IniFile.StringByName['LogFilter.Correlation'] := se.LogFilter.Correlation;
  IniFile.BooleanByName['LogFilter.CorrelationRegExp'] :=
    se.LogFilter.CorrelationRegExp;
  IniFile.BooleanByName['LogFilter.RequestEnabled'] :=
    se.LogFilter.RequestEnabled;
  IniFile.BooleanByName['LogFilter.RequestEquals'] :=
    se.LogFilter.RequestEquals;
  IniFile.StringByName['LogFilter.Request'] := se.LogFilter.Request;
  IniFile.BooleanByName['LogFilter.ReplyEnabled'] := se.LogFilter.ReplyEnabled;
  IniFile.BooleanByName['LogFilter.ReplyEquals'] := se.LogFilter.ReplyEquals;
  IniFile.StringByName['LogFilter.Reply'] := se.LogFilter.Reply;
  IniFile.BooleanByName['LogFilter.UnexpectedValuesEnabled'] :=
    se.LogFilter.UnexpectedValuesEnabled;
  IniFile.BooleanByName['LogFilter.RemarksEnabled'] :=
    se.LogFilter.RemarksEnabled;
  IniFile.BooleanByName['CollapseHeaders'] := CollapseHeaders;
  IniFile.StringByName['QueueNames'] := QueueNameList.Text;
  WsdlInformationMenuItem.Checked := IniFile.BooleanByNameDef['', True];
  IniFile.Save;
  IniFile.Free;
  QueueNameList.Free;
  ReopenCaseList.Free;
  FileNameList.Free;
  WsdlPaths.Free;
  FreeAndNil(se);
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
  for X := 0 to DownPageControl.PageCount - 1 do
    DownPageControl.ActivePageIndex := X;
  DownPageControl.ActivePage := DocumentationTabSheet;
  MessagesTabControl.TabIndex := Ord (slRequestBody);
  MasterMessagesMenuItem.Visible := isSlaveMode;
  MasterMessagesToolButton.Visible := isSlaveMode;
  CheckBoxClick(nil);
  stubChanged := False;
  if isSlaveMode and se.doLoadFromMasterOnStartUp then
  begin
    httpRequestDesignActionExecute(nil);
    httpRequestMessagesActionExecute(nil);
    RefreshTimer.Enabled := False;
    RefreshTimer.Enabled := AutoRefreshSlave;
    DownPageControl.ActivePage := MessagesTabSheet;
  end;
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
  NodeToMessage(Sender, Node, xMessage);
  if (Column = 0) then
  begin
    if xMessage.Disabled then
      if (Node <> GridView.GetFirst) then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsStrikeOut] +
          [fsBold]
      else
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;
  if Column <= xMessage.corBinds.Count then
    exit;
  xBind := nil;
  try
    xBind := xMessage.ColumnXmls.Bindables
      [Column - xMessage.corBinds.Count - 1];
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
var
  xCursor: TCursor;
begin
  EndEdit;
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    ClipBoard.AsText := vstToGrid(GridView, CopyGridOnGetText);
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TMainForm.CopyGridActionUpdate(Sender: TObject);
begin
  try
    CopyGridAction.Enabled := Assigned(WsdlOperation) and
      ((WsdlOperation.CorrelationBindables.Count > 0) or
        (WsdlOperation.StubAction = saRequest)) and Assigned
      (GridView.FocusedNode);
  except
  end;
end;

procedure TMainForm.PasteGridActionUpdate(Sender: TObject);
begin
  PasteGridAction.Enabled := Assigned(WsdlOperation) and
    ((WsdlOperation.CorrelationBindables.Count > 0) or
      (WsdlOperation.StubAction = saRequest)) and Assigned
    (GridView.FocusedNode);
end;

procedure TMainForm.PasteGridActionExecute(Sender: TObject);
var
  swapNode: PVirtualNode;
  swapColumn: Integer;
  SwapCursor: TCursor;
begin
  EndEdit;
  if not ClipBoard.HasFormat(CF_TEXT) then
    raise Exception.Create('Clipboard does not contain text');
  if se.IsActive then
    raise Exception.Create('Not allowed while active');
  try
    GridView.BeginUpdate;
    InWsdlTreeView.BeginUpdate;
    swapNode := GridView.FocusedNode;
    swapColumn := GridView.FocusedColumn;
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
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
    Screen.Cursor := SwapCursor;
  end;
end;

procedure TMainForm.InitMasterServer;
begin
  se.InitMasterServer;
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
  xCursor: TCursor;
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
        xCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          xXml.LoadFromString(aText, nil);
          xmlUtil.CreateXsdFromXml(xXsdDescr, xXml, True);
        finally
          Screen.Cursor := xCursor;
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
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
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
        Screen.Cursor := xCursor;
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
    Screen.Cursor := xCursor;
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
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    xChanged := stubChanged;
    xRead := se.stubRead;
    se.FocusOperationName := ifthen(Assigned (WsdlOperation), WsdlOperation.reqTagName);
    se.FocusMessageIndex := ifthen(Assigned (WsdlOperation), WsdlOperation.Messages.IndexOfObject(WsdlReply));
    ProjectDesignFromString(se.ProjectDesignAsString(se.projectFileName), se.projectFileName);
    if allOperations.Find (se.FocusOperationName, f) then
    begin
      WsdlOperation := allOperations.Operations[f];
      if (se.FocusMessageIndex < WsdlOperation.Messages.Count) then
        WsdlReply := WsdlOperation.Messages.Messages[se.FocusMessageIndex];
    end;
    stubChanged := xChanged;
    se.StubRead := xRead;
  finally
    screen.Cursor:=crDefault;
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.ShowHttpRequestAsXMLActionExecute(Sender: TObject);
var
  xXml: TXml;
  xString: String;
begin
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
    if (xLog.Exception <> '') then
    begin
      TargetCanvas.Font.Color := clRed;
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    end
    else
    begin
      if xLog.ShowHighLighted then
      begin
        if not(Node = Sender.FocusedNode) then
          TargetCanvas.Font.Color := clBlue;
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      end;
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
  ReadMessagesAction.Enabled := not se.IsActive and (se.Wsdls.Count > 0) and
    (se.displayedLogs.Count = 0);
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
              se.OpenMessagesLog(Files.Strings[X], True, xLogList);
            except
              on E: Exception do
                raise Exception.CreateFmt('Error opening file %s%s%s',
                  [Files.Strings[X], LineEnding, E.Message]);
            end;
          if xLogList.designSuspect then
          begin
            if not BooleanPromptDialog(
              'Maybe due to differences in current design or Wsdls,' + LineEnding +
                'some Operations, Messages or Correlation data could not be relocated;'
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
    se.SaveMessagesLog(SaveFileDialog.FileName);
  end;
end;

procedure TMainForm.ReadMessagesActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (se.Wsdls.Count = 0) then
    HintStr := HintStr + ' (no WSDLS read)';
  if (se.displayedLogs.Count > 0) then
    HintStr := HintStr + ' (List of messages must be empty)';
end;

procedure TMainForm.TestBeforeScriptActionExecute(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not se.IsActive then
    raise Exception.Create('Not active');
  if Assigned(WsdlOperation) then
  begin
    xOperation := TWsdlOperation.Create(WsdlOperation);
    try
      with xOperation.reqBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlReply.reqBind as TXml), False, True, True);
      end;
      with xOperation.rpyBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlReply.rpyBind as TXml), False, True, True);
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
  SwapCursor: TCursor;
  X: Integer;
begin
  try
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    ClearLogItemsActionExecute(nil);
    if not se.displayedLogs.Count = 0 then
      raise Exception.Create('Operation aborted');
    MessagesVTS.BeginUpdate;
    for X := 0 to aLogList.Count - 1 do
    begin
      se.SaveLog('', aLogList.LogItems[X]);
    end;
  finally
    MessagesVTS.EndUpdate;
    Screen.Cursor := SwapCursor;
  end;
end;

procedure TMainForm.MessagesRegressionActionUpdate(Sender: TObject);
begin
  MessagesRegressionAction.Enabled := { }{ not se.IsActive
    and { } (se.Wsdls.Count > 0);
end;

procedure TMainForm.SendMasterCommand(aPrompt, aCommand: String);
var
  ret: Word;
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  xCursor: TCursor;
  xRefresh: Boolean;
  s: String;
begin
  ret := MessageDlg(aPrompt, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  if (ret = mrYes) then
  begin
    xCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      xRefresh := RefreshTimer.Enabled;
      try
        HttpClient := TIdHTTP.Create;
        try
          HttpRequest := TStringStream.Create('');
          try
            HttpClient.Request.ContentType := 'text/xml';
            HttpClient.Request.CharSet := '';
            try
              if se.doViaProxyServer then
              begin
                HttpClient.ProxyParams.ProxyServer := se.ViaProxyServer;
                HttpClient.ProxyParams.ProxyPort := se.ViaProxyPort;
              end
              else
              begin
                HttpClient.ProxyParams.ProxyServer := '';
                HttpClient.ProxyParams.ProxyPort := 0;
              end;
              s := HttpClient.Post(MasterAddress + '/' + aCommand, HttpRequest);
              if HttpClient.ResponseCode = 500 then
                raise Exception.Create(s);
              ShowMessage(s);
            finally
              if HttpClient.Connected then { in case server s-alive }
                HttpClient.Disconnect;
            end;
          finally
            FreeAndNil(HttpRequest);
          end;
        finally
          FreeAndNil(HttpClient);
        end;
      finally
        RefreshTimer.Enabled := xRefresh;
      end;
    finally
      Screen.Cursor := xCursor;
    end;
  end;
end;

procedure TMainForm.MasterClearLogActionExecute(Sender: TObject);
begin
  SendMasterCommand('Send clear log command to master instance of ' +
      _progName + '?' + LineEnding + LineEnding +
      '(May impact other users of the ' + _progName + ' master instance)' +
      LineEnding + '(' + MasterAddress + ')', 'ClearLog');
end;

procedure TMainForm.MasterClearLogActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.MasterClearLogActionUpdate(Sender: TObject);
begin
  MasterClearLogAction.Enabled := isSlaveMode and (MasterAddress <> '');
end;

procedure TMainForm.MasterReactivateActonExecute(Sender: TObject);
begin
  SendMasterCommand('Reactivate master instance of ' + _progName + '?' +
      LineEnding + LineEnding + '(May impact other users of the ' + _progName +
      ' master instance)' + LineEnding + '(' + MasterAddress + ')', 'Reactivate');
end;

procedure TMainForm.MasterReactivateActonHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.MasterReactivateActonUpdate(Sender: TObject);
begin
  MasterReactivateActon.Enabled := isSlaveMode and (MasterAddress <> '');
end;

procedure TMainForm.MasterReloadDesignActionExecute(Sender: TObject);
begin
  SendMasterCommand('Send reload command to master instance of ' + _progName +
      '?' + LineEnding + LineEnding + '(May impact other users of the ' + _progName +
      ' master instance)' + LineEnding + '(' + MasterAddress + ')', 'ReloadDesign');
end;

procedure TMainForm.MasterReloadDesignActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.MasterReloadDesignActionUpdate(Sender: TObject);
begin
  MasterReloadDesignAction.Enabled := isSlaveMode and (MasterAddress <> '');
end;

procedure TMainForm.MasterRestartActionExecute(Sender: TObject);
begin
  SendMasterCommand('Shut down and restart master instance of ' + _progName +
      '?' + LineEnding + 'Expect an error message ... and wait a while...' +
      LineEnding + LineEnding + '(May impact other users of the ' + _progName +
      ' master instance)' + LineEnding + '(' + MasterAddress + ')', 'Restart');
end;

procedure TMainForm.MasterRestartActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.MasterRestartActionUpdate(Sender: TObject);
begin
  MasterRestartAction.Enabled := isSlaveMode and (MasterAddress <> '');
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
  EndEdit;
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
            TProcedureThread.Create(False, se, doReadMessagesFromDisk);
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
  MessagesFromDiskAction.Enabled := (not se.IsActive) and Assigned
    (WsdlOperation) and
    (WsdlOperation.CorrelationBindables.Count > 0);
end;

procedure TMainForm.MessagesRegressionActionExecute(Sender: TObject);
var
  xLogList: TLogList;
  xCursor: TCursor;
begin
  OnlyWhenLicensed;
  if not LogMaxEntriesEqualsUnbounded (MessagesRegressionAction.Caption) then Exit;
  OpenFileDialog.DefaultExt := 'xml';
  OpenFileDialog.FileName := wsdlStubMessagesFileName;
  OpenFileDialog.Filter := 'XML file (*.xml)|*.xml';
  OpenFileDialog.Title := 'Compare ' + _progName + ' log items from file';
  if OpenFileDialog.Execute then
  begin
    xCursor:=SCreen.Cursor;
    Screen.Cursor:=crHourGlass;
    wsdlStubMessagesFileName := OpenFileDialog.FileName;
    xLogList := TLogList.Create;
    try
      try
        se.OpenMessagesLog(OpenFileDialog.FileName, True, xLogList);
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
      ShowLogDifferences(se.displayedLogs, xLogList, wsdlStubMessagesFileName);
      UpdateCaption;
    finally
      xLogList.Clear;
      FreeAndNil(xLogList);
      Screen.Cursor:=xCursor;
    end;
  end;
end;

procedure TMainForm.ShowLogDifferences(aLogs, bLogs: TLogList;
  aReferenceFileName: String);
var
  X: Integer;
begin
  Application.CreateForm(TShowLogDifferencesForm, ShowLogDifferencesForm);
  try
    ShowLogDifferencesForm.aLogs := TLogList.Create;
    ShowLogDifferencesForm.aLogs.Sorted :=
      (se.CompareLogOrderBy <> clTimeStamp);
    ShowLogDifferencesForm.aLogs.Duplicates := dupAccept;
    ShowLogDifferencesForm.bLogs := TLogList.Create;
    ShowLogDifferencesForm.ReferenceFileName := aReferenceFileName;
    ShowLogDifferencesForm.bLogs.Sorted :=
      (se.CompareLogOrderBy <> clTimeStamp);
    ShowLogDifferencesForm.bLogs.Duplicates := dupAccept;
    try
      for X := 0 to aLogs.Count - 1 do
        if aLogs.LogItems[X].PassesFilter then
          ShowLogDifferencesForm.aLogs.AddObject(aLogs.LogItems[X].CompareKey(se.CompareLogOrderBy),
            aLogs.LogItems[X]);
      for X := 0 to bLogs.Count - 1 do
        if bLogs.LogItems[X].PassesFilter then
          ShowLogDifferencesForm.bLogs.AddObject(bLogs.LogItems[X].CompareKey(se.CompareLogOrderBy),
            bLogs.LogItems[X]);
      ShowLogDifferencesForm.ignoreDifferencesOn := se.ignoreDifferencesOn;
      ShowLogDifferencesForm.ignoreAddingon := se.ignoreAddingOn;
      ShowLogDifferencesForm.ignoreRemovingOn := se.ignoreRemovingOn;
      ShowLogDifferencesForm.ShowModal;
    finally
      ShowLogDifferencesForm.aLogs.Free;
      ShowLogDifferencesForm.bLogs.Free;
    end;
  finally
    FreeAndNil(ShowLogDifferencesForm);
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
//    LogPanel.Align := alBottom;
    Splitter1.Align := alBottom;
  end
  else
  begin
//    LogPanel.Align := alTop;
    Splitter1.Align := alTop;
  end;
end;

procedure TMainForm.CheckGridFieldsActionUpdate(Sender: TObject);
begin
  CheckGridFieldsAction.Enabled := (Assigned(WsdlOperation)) and
    (WsdlOperation.Messages.Count > 0) and
    (WsdlOperation.Messages.Messages[0].ColumnXmls.Count > 0);
end;

procedure TMainForm.CheckGridFieldsActionExecute(Sender: TObject);
var
  X, Y, r: Integer;
  xString: String;
  xNode: PVirtualNode;
  SwapCursor: TCursor;
begin
  EndEdit;
  SwapCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
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
    Screen.Cursor := SwapCursor;
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
    EditListValuesForm.isReadOnly := se.IsActive and False;
    EnvVarLock.Acquire;
    try
      EditListValuesForm.ValueListEditor.Strings.Text := _WsdlVars.Text;
    finally
      EnvVarLock.Release;
    end;
    EditListValuesForm.ShowModal;
    if (EditListValuesForm.ModalResult = mrOk)
    { }{ and (not se.IsActive){ } then
    begin
      EnvVarLock.Acquire;;
      try
        _WsdlVars.Text := EditListValuesForm.ValueListEditor.Strings.Text;
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
  AddEnvironmentAction.Enabled := (_WsdlVars.Count > 0);
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
          for X := 0 to _WsdlVars.Count - 1 do
          begin
            with AddXml(TXml.CreateAsString('Var', '')) do
            begin
              AddXml(TXml.CreateAsString('Key', _WsdlVars.Names[X]));
              AddXml(TXml.CreateAsString('Value', _WsdlVars.ValueFromIndex[X]));
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
  xScript: String;
begin
  if not Assigned(se) then
    exit;
  if not se.IsActive then
    raise Exception.Create(Format('%s not active', [_progName]));
  se.ProgressMax := 5;
  se.ProgressPos := 0;
  isBusy := True;
  with Sender as TMenuItem do
    with se.Scripts.Objects[Tag] as TStringList do
      xScript := Text;
  TProcedureThread.Create(False, se, se.ScriptExecute, xScript);
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
  while ScriptsMenuItem.Count > 4 do
    ScriptsMenuItem.Delete(4);
  EditScriptMenuItem.Clear;
  EditScriptMenuItem.OnClick := nil;
  RemoveScriptMenuItem.Clear;
  RemoveScriptMenuItem.OnClick := nil;
  for X := 0 to se.Scripts.Count - 1 do
  begin
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Action := runScriptAction; // only for enabling
    xMenuItem.Caption := se.Scripts.Strings[X];
    xMenuItem.OnClick := ScriptGoMenuItemClick;
    xMenuItem.Tag := X;
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
    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Caption := se.Scripts.Strings[X];
    xMenuItem.OnClick := EditScriptMenuItemClick;
    xMenuItem.Tag := X;
    EditScriptMenuItem.Add(xMenuItem);

    xMenuItem := TMenuItem.Create(Self);
    xMenuItem.Caption := se.Scripts.Strings[X];
    xMenuItem.OnClick := RemoveScriptMenuItemClick;
    xMenuItem.Tag := X;
    RemoveScriptMenuItem.Add(xMenuItem);
  end;
  ScriptsMenuItem.Enabled := True;
  EditScriptMenuItem.Enabled := (EditScriptMenuItem.Count > 0);
  RemoveScriptMenuItem.Enabled := (RemoveScriptMenuItem.Count > 0);

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
    _WsdlVars.Clear;
    for X := 0 to xXml.Items.Count - 1 do
      with xXml.Items.XmlItems[X].Items do
        _WsdlVars.Values[XmlValueByTag['Key']] := XmlValueByTag['Value'];
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
        IniFile.DeleteKey('Environment' + ChooseStringForm.ChoosenString);
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
  result := IniFile.DecryptPassword(aString);
end;

function TMainForm.doEncryptString(aString: AnsiString): AnsiString;
begin
  result := IniFile.EncryptPassword(aString);
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
    se.SendMessage(xOperation, WsdlReply, '');
  finally
    xOperation.Free;
  end;
end;

procedure TMainForm.ExecuteRequestActionExecute(Sender: TObject);
begin
  EndEdit;
  TProcedureThread.Create(False, se, doExecuteRequest);
end;

procedure TMainForm.ExecuteAllRequests;
var
  X: Integer;
  xOperation: TWsdlOperation;
begin
  se.AcquireLogLock;
  se.ProgressMax := WsdlOperation.Messages.Count;
  se.ReleaseLogLock;
  WsdlOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(WsdlOperation);
  finally
    WsdlOperation.ReleaseLock;
  end;
  try
    for X := 0 to xOperation.Messages.Count - 1 do
    begin
      if abortPressed then
        Break;
      if not xOperation.Messages.Messages[X].Disabled then
      begin
        se.AcquireLogLock;
        se.ProgressPos := X + 1;
        se.ReleaseLogLock;
        try
          se.SendMessage(xOperation, xOperation.Messages.Messages[X], '');
        except
        end;
      end;
    end;
  finally
    FreeAndNil(xOperation);
  end;
end;

procedure TMainForm.ExecuteAllRequestsActionExecute(Sender: TObject);
begin
  EndEdit;
  procedureThread := TProcedureThread.Create(False, se, ExecuteAllRequests);
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
  try
    case TLogColumnEnum((Sender as TVirtualStringTree).FocusedColumn) of
      logExpectedColumn: ShowExpectedXmlActionExecute(nil);
      logRemarksColumn: ShowRemarksActionExecute(nil);
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
        if (TypeDef.IsComplex and (TypeDef.ElementDefs.Count = 0)) or
          ((TypeDef.IsBuiltIn) and (TypeDef.BaseDataTypeName = 'anyType')) or
          (TypeDef.Manually) then
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
    and Assigned(WsdlReply)
    and (WsdlOperation.StubAction = saRequest)
    and (se.IsActive)
    and (not se.isBusy)
    and (not ExecuteRequestToolButton.Down)
    and (not ExecuteAllRequestsToolButton.Down)
    ;
end;

procedure TMainForm.ExecuteAllRequestsActionUpdate(Sender: TObject);
begin
  ExecuteAllRequestsAction.Enabled := Assigned(WsdlOperation)
                                  and Assigned(WsdlReply)
                                  and (WsdlOperation.StubAction = saRequest)
                                  and (se.IsActive)
                                  and (not se.isBusy)
                                  and (not ExecuteRequestToolButton.Down)
                                  and (not ExecuteAllRequestsToolButton.Down)
                                    ;
end;

procedure TMainForm.ScriptButtonsPanelResize(Sender: TObject);
begin
  AfterRequestScriptButton.Width := ScriptButtonsPanel.Width div 2;
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
var
  logAdded, exceptionAdded: Boolean;
begin
  se.AcquireLogLock;
  try
    logAdded := _refreshLogging;
    exceptionAdded := _refreshExceptions;
    SetUiProgress;
  finally
    se.ReleaseLogLock;
  end;
  if logAdded then
    if doScrollMessagesIntoView then
      MessagesVTS.ScrollIntoView(MessagesVTS.GetLast, True, False);
  if exceptionAdded then
  begin
    if DownPageControl.ActivePage <> ExceptionTabSheet then
    begin
      ExceptionTabSheet.Caption := notifyTabCaption + ' *';
      ExceptionTabSheet.ImageIndex := notifyTabImageIndex;
    end;
    if doScrollExceptionsIntoView then
    begin
      ExceptionsVTS.ScrollIntoView(ExceptionsVTS.GetLast, True, False);
      DownPageControl.ActivePage := ExceptionTabSheet;
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
  RequestMiMAction.Enabled := Assigned(xLog) and (xLog.RequestBodyMiM <> '')
    and (xLog.RequestBodyMiM <> xLog.RequestBody);
  ReplyMiMAction.Enabled := Assigned(xLog) and (xLog.ReplyBodyMiM <> '') and
    (xLog.ReplyBodyMiM <> xLog.ReplyBody);
  Log2DesignAction.Enabled := (n > 0);
  DisplayedcolumnMenuItem.Enabled := Assigned(xLog) and Assigned
    (xLog.Operation) and (n = 1) and (MessagesVTS.FocusedColumn >= Ord
      (logStdColumnCount));
end;

procedure TMainForm.RequestMiMActionExecute(Sender: TObject);
var
  xLog: TLog;
  aXml, bXml: TXml;
  xA2B: TA2BXml;
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
    xA2B := TA2BXml.CreateA2B('', aXml, bXml, False);
  finally
    a2bUninitialize;
  end;
  FreeAndNil(aXml);
  FreeAndNil(bXml);
  Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
  try
    ShowA2BXmlForm.Caption := 'Changes in Request';
    ShowA2BXmlForm.ignoreDifferencesOn := se.ignoreDifferencesOn;
    ShowA2BXmlForm.ignoreAddingOn := se.ignoreAddingOn;
    ShowA2BXmlForm.ignoreRemovingOn := se.ignoreRemovingOn;
    ShowA2BXmlForm.Xml := xA2B;
    ShowA2BXmlForm.ShowModal;
    if ShowA2BXmlForm.RefreshNeeded then
      FormShow(nil);
  finally
    FreeAndNil(ShowA2BXmlForm);
  end;
end;

procedure TMainForm.ReplyMiMActionExecute(Sender: TObject);
var
  xLog: TLog;
  aXml, bXml: TXml;
  xA2B: TA2BXml;
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
    xA2B := TA2BXml.CreateA2B('', aXml, bXml, False);
  finally
    a2bUninitialize;
  end;
  FreeAndNil(aXml);
  FreeAndNil(bXml);
  Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
  try
    ShowA2BXmlForm.Caption := 'Changes in Reply';
    ShowA2BXmlForm.ignoreDifferencesOn := se.ignoreDifferencesOn;
    ShowA2BXmlForm.ignoreAddingOn := se.ignoreAddingOn;
    ShowA2BXmlForm.ignoreRemovingOn := se.ignoreRemovingOn;
    ShowA2BXmlForm.Xml := xA2B;
    ShowA2BXmlForm.ShowModal;
    if ShowA2BXmlForm.RefreshNeeded then
      FormShow(nil);
  finally
    FreeAndNil(ShowA2BXmlForm);
  end;
end;

procedure TMainForm.FindActionUpdate(Sender: TObject);
begin
  FindAction.Enabled := (Assigned(WsdlReply));
end;

procedure TMainForm.FindActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurItem: PVirtualNode;
  xNodeText: String;
begin
  EndEdit;
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
  FindNextAction.Enabled := (Assigned(WsdlReply)) and
    (xmlUtil.SearchString <> '');
end;

procedure TMainForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  CurNode: PVirtualNode;
  xNodeText: String;
begin
  EndEdit;
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
  CheckTreeAction.Enabled := (Assigned(WsdlOperation)) and
    (WsdlOperation.Messages.Count > 0) and
    (WsdlOperation.WsdlService.DescriptionType <> ipmDTFreeFormat);
end;

procedure TMainForm.CheckTreeActionExecute(Sender: TObject);
var
  xNode, lastNode: PVirtualNode;
  xBind: TCustomBindable;
  xCursor: TCursor;
  xMessage: String;
begin
  EndEdit;
  xCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
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
    Screen.Cursor := xCursor;
  end;
end;

procedure TMainForm.ProjectOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.ProjectOptionsAsXml;
  if EditXmlXsdBased('Project Options', '', '', '', se.IsActive,
    projectOptionsXsd, xXml) then
  begin
    AcquireLock;
    try
      stubChanged := True;
      se.ProjectOptionsFromXml(xXml);
      LogUpdateColumns;
    finally
      ReleaseLock;
    end;
    InitMasterServer;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.HandleException(Sender: TObject; E: Exception);
var
  s: String;
begin
  if AnsiStartsText('System Error.  Code: 1400.', E.Message) then
    exit; // no idea, no impact
  { }
  s := se.ExceptionStackListString (E);
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

procedure TMainForm.AddChildElementDefMenuItemClick(Sender: TObject);
  procedure _updateTypedef(aXml: TXml; oXsd, aXsd: TXsd;
    nType, oType: TXsdDataType);
  var
    X: Integer;
  begin
    for X := 0 to aXml.Items.Count - 1 do
      _updateTypedef(aXml.Items.XmlItems[X], oXsd, aXsd, nType, oType);
    if (aXml.Xsd = oXsd) then
    begin
      bindRefId := 0;
      aXml.AddXml(TXml.Create(0, aXsd));
      aXml.TypeDef := nType;
    end;
  end;

var
  X, f, m: Integer;
  xxsd: TXsd;
  xXml: TXml;
  xBind: TCustomBindable;
  nTypeDef, oTypeDef, cTypeDef: TXsdDataType;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if not Assigned(xBind) then
    raise Exception.Create('no element selected');
  if not(xBind is TXml) then
    raise Exception.Create('operation only valid on XML elements');
  xXml := xBind as TXml;
  if not Assigned(xXml.Xsd) then
    raise Exception.Create('opeation requires an XSD on the selected element');
  try
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
      if ChooseStringForm.ModalResult = mrOk then
      begin
        f := ChooseStringForm.ListBox.ItemIndex;
        cTypeDef := Wsdl.XsdDescr.TypeDefs.XsdDataTypes[f];
        Application.CreateForm(TPromptForm, PromptForm);
        try
          PromptForm.Caption := 'Name for ' + Wsdl.XsdDescr.TypeDefs.Strings[f];
          PromptForm.PromptEdit.Text := '';
          PromptForm.Numeric := False;
          PromptForm.ShowModal;
          if PromptForm.ModalResult = mrOk then
          begin
            oTypeDef := xXml.Xsd.sType;
            xxsd := xXml.Xsd.AddElementDef(Wsdl.XsdDescr,
              PromptForm.PromptEdit.Text, cTypeDef);
            nTypeDef := xXml.Xsd.sType;
            xXml.Checked := True;
            _updateTypedef(WsdlOperation.reqBind as TXml, xXml.Xsd, xxsd,
              nTypeDef, oTypeDef);
            _updateTypedef(WsdlOperation.rpyBind as TXml, xXml.Xsd, xxsd,
              nTypeDef, oTypeDef);
            for m := 0 to WsdlOperation.Messages.Count - 1 do
            begin
              _updateTypedef
                (WsdlOperation.Messages.Messages[m].reqBind as TXml, xXml.Xsd,
                xxsd, nTypeDef, oTypeDef);
              _updateTypedef
                (WsdlOperation.Messages.Messages[m].rpyBind as TXml, xXml.Xsd,
                xxsd, nTypeDef, oTypeDef);
            end;
            GridView.OnFocusChanged(GridView, GridView.FocusedNode,
              GridView.FocusedColumn);
            stubChanged := True;
          end;
        finally
          FreeAndNil(PromptForm);
        end;
      end;
    finally
      FreeAndNil(ChooseStringForm);
    end;
  finally
  end;
  { }
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

procedure TMainForm.AbortToolButtonClick(Sender: TObject);
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
  xCursor: TCursor;
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
        xCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          xmlUtil.ShowSoapBodyInGrid(claimedLog.ReplyBody);
        finally
          Screen.Cursor := xCursor;
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
  xLog: TLog;
  xNode: PVirtualNode;
  xData: PLogTreeRec;
  aXml: TXml;
  SwapCursor: TCursor;
begin
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    aXml := TXml.CreateAsString('reqXmls', '');
    try
      xNode := MessagesVTS.GetFirstSelected;
      while Assigned(xNode) do
      begin
        xData := MessagesVTS.GetNodeData(xNode);
        if Assigned(xData.Log) and (xData.Log is TLog) then
        begin
          xLog := xData.Log;
          if Assigned(xLog.Operation) then
            aXml.AddXml(xLog.reqBodyAsXml);
        end;
        xNode := MessagesVTS.GetNextSelected(xNode);
      end;
      ShowTextAsGrid('Requests as Grid', aXml.AsText(False, 0, False, False));
    finally
      aXml.Free;
    end;
  finally
    Screen.Cursor := SwapCursor;
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
  xLog: TLog;
  xNode: PVirtualNode;
  xData: PLogTreeRec;
  aXml: TXml;
  SwapCursor: TCursor;
begin
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    aXml := TXml.CreateAsString('rpyXmls', '');
    try
      xNode := MessagesVTS.GetFirstSelected;
      while Assigned(xNode) do
      begin
        xData := MessagesVTS.GetNodeData(xNode);
        if Assigned(xData.Log) and (xData.Log is TLog) then
        begin
          xLog := xData.Log;
          if Assigned(xLog.Operation) then
            aXml.AddXml(xLog.rpyBodyAsXml);
        end;
        xNode := MessagesVTS.GetNextSelected(xNode);
      end;
      ShowTextAsGrid('Responses as Grid', aXml.AsText(False, 0, False, False));
    finally
      aXml.Free;
    end;
  finally
    Screen.Cursor := SwapCursor;
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
  xCursor: TCursor;
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
        ShowInfoForm('Request freeformat', claimedLog.RequestBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTEmail then
      begin
        ShowInfoForm('Request freeformat', claimedLog.RequestBody);
        exit;
      end;
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTWsdl then
      begin
        xCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          xmlUtil.ShowSoapBodyInGrid(claimedLog.RequestBody);
        finally
          Screen.Cursor := xCursor;
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
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
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
    Screen.Cursor := xCursor;
  end;
end;

procedure TMainForm.ShowHtml(aCaption, aInfoString: String);
begin
  xmlUtil.presentAsHTML(aCaption, aInfoString);
end;

procedure TMainForm.AcquireLock;
begin
  if False then Wsdlz.AcquireLock;
end;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  ShowHtml('', '<html>' + _WsdlHostName + '</html>');
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
  xFileName := ExtractFilePath(ParamStr(0)) + '\Config\' + _xmlProgName +
    'Menu.xsd';
  xXsdDescr := TXsdDescr.Create(1);
  try
    try
      xXsdDescr.LoadXsdFromFile(xFileName, nil);
      xXml := TXml.Create(0, xXsdDescr.TypeDef.ElementDefs.Xsds[0]);
      try
        EditXmlXsdBased('Press Ctrl_Alt_H to generate Help file', 'Menu',
          'Menu', '', True, xXsdDescr.TypeDef.ElementDefs.Xsds[0], xXml);
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
  startAction.Enabled := Assigned(se) and (not se.IsActive) and
    (not isSlaveMode);
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

procedure TMainForm.httpRequestMessagesActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.httpRequestMessagesActionUpdate(Sender: TObject);
begin
  httpRequestMessagesAction.Enabled := isSlaveMode and (MasterAddress <> '');
end;

procedure TMainForm.httpRequestMessagesActionExecute(Sender: TObject);
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  s: String;
  xCursor: TCursor;
  xLogList: TLogList;
begin
  DownPageControl.ActivePage := MessagesTabSheet;
  if se.displayedLogs.Count > 0 then
  begin
    if (not xmlUtil.doConfirmRemovals) or BooleanPromptDialog
      ('Remove all messages') then
    begin
      MessagesVTS.Clear;
      se.AsynchRpyLogs.Clear;
      se.displayedLogs.Clear;
      LogMemo.Text := '';
    end;
  end;
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if se.displayedLogs.Count = 0 then
    begin
      RefreshTimer.Enabled := False;
      HttpClient := TIdHTTP.Create;
      try
        HttpRequest := TStringStream.Create('');
        try
          HttpClient.Request.ContentType := 'text/xml';
          HttpClient.Request.CharSet := '';
          try
            if se.doViaProxyServer then
            begin
              HttpClient.ProxyParams.ProxyServer := se.ViaProxyServer;
              HttpClient.ProxyParams.ProxyPort := se.ViaProxyPort;
            end
            else
            begin
              HttpClient.ProxyParams.ProxyServer := '';
              HttpClient.ProxyParams.ProxyPort := 0;
            end;
            try
              s := HttpClient.Post(MasterAddress + '/' + 'Log', HttpRequest);
            finally
            end;
            if HttpClient.ResponseCode = 500 then
              raise Exception.Create(s);
          finally
            if HttpClient.Connected then { in case server s-alive }
              HttpClient.Disconnect;
          end;
        finally
          FreeAndNil(HttpRequest);
        end;
      finally
        FreeAndNil(HttpClient);
      end;
      xLogList := TLogList.Create;
      se.OpenMessagesLog(s, False, xLogList);
      ToAllLogList(xLogList);
      MessagesVTS.FocusedNode := MessagesVTS.GetLast;
      xLogList.Clear;
      FreeAndNil(xLogList);
      RefreshTimer.Enabled := AutoRefreshSlave;
    end;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure TMainForm.BrowseMqActionUpdate(Sender: TObject);
begin
  BrowseMqAction.Enabled := (not se.IsActive) and
    (se.mmqqMqInterface.MQServerOK or se.mmqqMqInterface.MQClientOK);
end;

procedure TMainForm.BrowseMqActionExecute(Sender: TObject);
var
  xMqInterface: TMqInterface;
  xCursor: TCursor;
begin
  Application.CreateForm(TMqBrowseForm, MqBrowseForm);
  try
    MqBrowseForm.GetQueueEdit.Items.Text := QueueNameList.Text;
    MqBrowseForm.ShowModal;
    if MqBrowseForm.ModalResult = mrOk then
    begin
      QueueNameList.Add(MqBrowseForm.GetQueueEdit.Text);
      DownPageControl.ActivePage := MessagesTabSheet;
      if se.displayedLogs.Count > 0 then
      begin
        if (not xmlUtil.doConfirmRemovals) or BooleanPromptDialog
          ('Remove all logrecords') then
        begin
          MessagesVTS.Clear;
          se.AsynchRpyLogs.Clear;
          se.displayedLogs.Clear;
          LogMemo.Text := '';
        end;
      end;
      if se.displayedLogs.Count = 0 then
      begin
        xCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
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
          Screen.Cursor := xCursor;
        end;
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
        se.SaveLog('', xLogItem);
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
      IniFile.BooleanByName['doViaProxyServer'] := se.doViaProxyServer;
      IniFile.BooleanByName['doScrollMessagesIntoView'] :=
        doScrollMessagesIntoView;
      IniFile.BooleanByName['doScrollExceptionsIntoView'] :=
        doScrollExceptionsIntoView;
      IniFile.BooleanByName['doConfirmRemovals'] := xmlUtil.doConfirmRemovals;
      // IniFile.BooleanByName ['HTTPServer.KeepAlive'] := se.HTTPServer.KeepAlive;
      IniFile.IntegerByName['HTTPServer.ListenQueue'] :=
        se.HTTPServer.ListenQueue;
      IniFile.IntegerByName['HTTPServer.MaxConnections'] :=
        se.HTTPServer.MaxConnections;
      IniFile.StringByName['ViaProxyServer'] := se.ViaProxyServer;
      IniFile.StringByName['ViaProxyPort'] := IntToStr(se.ViaProxyPort);
      IniFile.StringByName['mqUse'] := IntToStr(Ord(se.mqUse));
      IniFile.IntegerByName['MaxWorkingThreads'] := se.mqMaxWorkingThreads;
      IniFile.StringByName['mqServerEnv'] := mqServerEnv;
      IniFile.IntegerByName['CompareLogOrderBy'] := Ord(se.CompareLogOrderBy);
      IniFile.IntegerByName['ShowLogCobolStyle'] := Ord(se.ShowLogCobolStyle);
      IniFile.BooleanByName['isSlaveMode'] := isSlaveMode;
      IniFile.StringByName['MasterAddress'] := MasterAddress;
      IniFile.BooleanByName['AutoRefreshSlave'] := AutoRefreshSlave;
      IniFile.BooleanByName['doLoadFromMasterOnStartUp'] :=
        se.doLoadFromMasterOnStartUp;
      IniFile.StringByName['ElementsWhenRepeatable'] := IntToStr
        (xsdElementsWhenRepeatable);
      IniFile.BooleanByName['doValidateScriptAssignmentAgainstSchema'] :=
        xsdValidateAssignmentsAgainstSchema;
      IniFile.IntegerByName['bgCorrelationItemColor'] := bgCorrelationItemColor;
      IniFile.IntegerByName['bgExpectedValueColor'] := bgExpectedValueColor;
      IniFile.IntegerByName['bgNilValueColor'] := bgNilValueColor;
      xXml := OptionsAsXml;
      try
        IniFile.StringByName['Options'] := xXml.Text;
      finally
        xXml.Free;
      end;
    end;
    if (ret = mrCancel) then
      CanClose := False
    else
      NewStubCaseActionExecute(nil);
  end;
end;

procedure TMainForm.httpRequestDesignActionUpdate(Sender: TObject);
begin
  httpRequestDesignAction.Enabled := isSlaveMode and (MasterAddress <> '');
end;

procedure TMainForm.httpRequestDesignActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (MasterAddress = '') then
    HintStr := HintStr + ' (no Master address known)';
end;

procedure TMainForm.httpRequestDesignActionExecute(Sender: TObject);
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  s: String;
  xCursor: TCursor;
  xRefreshEnabled: Boolean;
begin
  DownPageControl.ActivePage := MessagesTabSheet;
  if se.displayedLogs.Count > 0 then
  begin
    if (not xmlUtil.doConfirmRemovals) or BooleanPromptDialog
      ('Remove all messages') then
    begin
      MessagesVTS.Clear;
      se.AsynchRpyLogs.Clear;
      se.displayedLogs.Clear;
      LogMemo.Text := '';
    end;
  end;
  if se.displayedLogs.Count > 0 then
    exit;
  if not OkToOpenStubCase then
    exit;
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    xRefreshEnabled := RefreshTimer.Enabled;
    RefreshTimer.Enabled := False;
    HttpClient := TIdHTTP.Create;
    try
      HttpRequest := TStringStream.Create('');
      try
        HttpClient.Request.ContentType := 'text/xml';
        HttpClient.Request.CharSet := '';
        try
          if se.doViaProxyServer then
          begin
            HttpClient.ProxyParams.ProxyServer := se.ViaProxyServer;
            HttpClient.ProxyParams.ProxyPort := se.ViaProxyPort;
          end
          else
          begin
            HttpClient.ProxyParams.ProxyServer := '';
            HttpClient.ProxyParams.ProxyPort := 0;
          end;
          s := HttpClient.Post(MasterAddress + '/Design', HttpRequest);
          if HttpClient.ResponseCode = 500 then
            raise Exception.Create(s);
        finally
          if HttpClient.Connected then { in case server s-alive }
            HttpClient.Disconnect;
        end;
      finally
        FreeAndNil(HttpRequest);
      end;
    finally
      FreeAndNil(HttpClient);
      RefreshTimer.Enabled := xRefreshEnabled;
    end;
    ProjectDesignFromString(s, '');
    captionFileName := se.projectFileName;
    UpdateCaption;
  finally
    Screen.Cursor := xCursor;
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

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
var
  HttpClient: TIdHTTP;
  HttpRequest: TStringStream;
  s: String;
  xCursor: TCursor;
  xLogList: TLogList;
  X: Integer;
begin
  xCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    RefreshTimer.Enabled := False;
    if not AutoRefreshSlave then
      exit;
    HttpClient := TIdHTTP.Create;
    try
      HttpRequest := TStringStream.Create('');
      with TXml.CreateAsString('LogIncrement', '') do
        try
          AddXml(TXml.CreateAsInteger('Index', se.refreshNr));
          AddXml(TXml.CreateAsString('Check', se.refreshCheck));
          HttpRequest.WriteString(Text);
        finally
          Free;
        end;
      try
        HttpClient.Request.ContentType := 'text/xml';
        HttpClient.Request.CharSet := '';
        try
          if se.doViaProxyServer then
          begin
            HttpClient.ProxyParams.ProxyServer := se.ViaProxyServer;
            HttpClient.ProxyParams.ProxyPort := se.ViaProxyPort;
          end
          else
          begin
            HttpClient.ProxyParams.ProxyServer := '';
            HttpClient.ProxyParams.ProxyPort := 0;
          end;
          s := HttpClient.Post(MasterAddress + '/LogIncrement', HttpRequest);
          if HttpClient.ResponseCode = 500 then
            raise Exception.Create(s);
        finally
          if HttpClient.Connected then { in case server s-alive }
            HttpClient.Disconnect;
        end;
      finally
        FreeAndNil(HttpRequest);
      end;
    finally
      FreeAndNil(HttpClient);
    end;
    xLogList := TLogList.Create;
    se.OpenMessagesLog(s, False, xLogList);
    try
      MessagesVTS.BeginUpdate;
      for X := 0 to xLogList.Count - 1 do
      begin
        se.displayedLogs.SaveLog('', xLogList.LogItems[X]);
      end;
    finally
      MessagesVTS.EndUpdate;
    end;
    // MessagesVTS.FocusedNode := MessagesVTS.GetLast;
    xLogList.Clear;
    FreeAndNil(xLogList);
    RefreshTimer.Enabled := True;
  finally
    Screen.Cursor := xCursor;
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
      TProcedureThread.Create(False, se, doSaveMessagesToDisk);
    end;
  finally
    FreeAndNil(messagesToDiskForm);
  end;
end;

procedure TMainForm.doSaveLogRepliesToDisk;
var
  SwapCursor: TCursor;
  xNode: PVirtualNode;
  xLog: TLog;
  sl: TStringList;
begin
  AcquireLock;
  try
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
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
      Screen.Cursor := SwapCursor;
      ProgressBar.Position := 0;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.doSaveLogRequestsToDisk;
var
  SwapCursor: TCursor;
  xNode: PVirtualNode;
  xLog: TLog;
  sl: TStringList;
begin
  AcquireLock;
  try
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
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
      Screen.Cursor := SwapCursor;
      ProgressBar.Position := 0;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMainForm.doSaveMessagesToDisk;
var
  X: Integer;
  SwapCursor: TCursor;
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
  xFileName, xSeparator, xMsgString: String;
begin
  AcquireLock;
  try
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := WsdlOperation.Messages.Count;
    ProgressBar.Position := 0;
    abortPressed := False;
    AbortToolButton.Enabled := True;
    isBusy := True;
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
              xFileName := saveToDiskDirectory + '\';
              xSeparator := '';
              for X := 0 to xMessage.corBinds.Count - 1 do
              begin
                xFileName := xFileName + xSeparator +
                  xMessage.corBinds.Bindables[X].CorrelationValue;
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
      isBusy := False;
      CheckBoxClick(nil);
      AbortToolButton.Enabled := False;
      abortPressed := False;
      ExecuteAllRequestsToolButton.Down := False;
      ProgressBar.Position := 0;
      Screen.Cursor := SwapCursor;
    finally
      ReleaseLock;
    end;
  end;
end;

procedure TMainForm.DownPageControlChange(Sender: TObject);
begin
  if DownPageControl.ActivePage = MessagesTabSheet then
  begin
    MessagesTabSheet.Caption := logTabCaption; // remove asterix since exceptions can be viewed now
  end;
  if DownPageControl.ActivePage = ExceptionTabSheet then
  begin
    ExceptionTabSheet.Caption := notifyTabCaption; // remove asterix since exceptions can be viewed now
    ExceptionTabSheet.ImageIndex := -1;
  end;
end;

procedure TMainForm.doReadMessagesFromDisk;
var
  f: Integer;
  SwapCursor: TCursor;
  xMessage: TWsdlMessage;
  xFileName, xMsgString: String;
  xPatterns: TStringList;
  nErrors: Integer;
  xXml: TXml;
  mNode: PVirtualNode;
  mData: PMessageTreeRec;
begin
  nErrors := 0;
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  ProgressBar.Min := 0;
  ProgressBar.Position := 0;
  ProgressBar.Max := FileNameList.Count;
  ProgressBar.Position := 0;
  abortPressed := False;
  AbortToolButton.Enabled := True;
  isBusy := True;
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
            mData.Reply := xMessage;
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
      isBusy := False;
      CheckBoxClick(nil);
      AbortToolButton.Enabled := False;
      abortPressed := False;
      ExecuteAllRequestsToolButton.Down := False;
      ProgressBar.Position := 0;
      Screen.Cursor := SwapCursor;
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
        c := 0;
        while (c < copyColumns.Count) and (c < GridView.Header.Columns.Count) do
        begin
          // OnNewText (aVst, xNode, c, copyColumns.Strings[c]);
          if c = 0 then
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
            if c <= xMessage.corBinds.Count then
            begin
              if c < copyColumns.Count then
              begin
                if copyColumns.Strings[c] <> xMessage.corBinds.Bindables[c - 1]
                  .CorrelationValue then
                begin
                  if l = 1 then
                    raise Exception.Create(
                      'Not allowed to change this pattern into ' +
                        copyColumns.Strings[c])
                  else
                  begin
                    xMessage.corBinds.Bindables[c - 1].CorrelationValue :=
                      copyColumns.Strings[c];
                    stubChanged := True;
                  end;
                end;
              end;
            end
            else
            begin
              if (copyColumns.Strings[c] <> '?') and
                (Assigned(xMessage.ColumnXmls.Bindables
                    [c - xMessage.corBinds.Count - 1])) then
              begin
                xBind := xMessage.ColumnXmls.Bindables
                  [c - xMessage.corBinds.Count - 1];
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
  xCursor: TCursor;
begin
  xCursor := Screen.Cursor;
  AcquireLock;
  try
    Screen.Cursor := crHourGlass;
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
          xLog.toBindables;
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
          begin (xMessage.reqBind as TIpmItem)
            .BufferToValues(FoundErrorInBuffer, xLog.RequestBody);
(xMessage.rpyBind as TIpmItem)
            .BufferToValues(FoundErrorInBuffer, xLog.ReplyBody);
          end
          else
          begin
            xXml := TXml.Create;
            try
              try
                xXml.LoadFromString(xLog.RequestBody, nil);
                xLog.Operation.SoapXmlRequestToBindables(xXml, False);
                (xMessage.reqBind as TXml).Reset;
                (xMessage.reqBind as TXml).LoadValues((xLog.Operation.reqBind as TXml), True, True);
              except
              end;
              try
                xXml.LoadFromString(xLog.ReplyBody, nil);
                xLog.Operation.SoapXmlReplyToBindables(xXml, False);
(xMessage.rpyBind as TXml)
                .Reset; (xMessage.rpyBind as TXml)
                .LoadValues((xLog.Operation.rpyBind as TXml), True, True);
              except
              end;
            finally
              xXml.Free;
            end;
          end;
          se.UpdateMessageRow(xLog.Operation, xMessage);
          if xLog.Operation = WsdlOperation then
          begin
            mNode := GridView.AddChild(nil);
            mData := GridView.GetNodeData(mNode);
            mData.Reply := xMessage;
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
    Screen.Cursor := xCursor;
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

procedure TMainForm.LogCoverageReportActionExecute(Sender: TObject);
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
  SwapCursor: TCursor;
begin
  OnlyWhenLicensed;
  SwapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    AcquireLock;
    try
      xXml := se.displayedLogs.PrepareCoverageReportAsXml(allOperations,
        se.ignoreCoverageOn);
    finally
      ReleaseLock;
    end;
  finally
    Screen.Cursor := SwapCursor;
  end;
  try
    Application.CreateForm(TShowXmlCoverageForm, xForm);
    try
      xForm.Caption := '' + _progName + ' - Coverage report';
      xForm.Bind := xXml;
      xForm.initialExpandStyle := esOne;
      xForm.ShowModal;
      if xForm.Changed then
      begin
        if BooleanPromptDialog('Accept changes to Coverage report setting') then
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
  ViewMssgAsTextAction.Enabled := Assigned(WsdlReply);
end;

procedure TMainForm.ViewMssgAsTextActionExecute(Sender: TObject);
var
  xMessage: String;
begin
  EndEdit;
  if not Assigned(WsdlReply) then
    raise Exception.Create('No message selected');
  if WsdlOperation.StubAction = saRequest then
  begin
    WsdlOperation.AcquireLock;
    try
      if WsdlOperation.reqBind is TXml then with WsdlOperation.reqBind as TXml do
      begin
        ResetValues;
        LoadValues((WsdlReply.reqBind as TXml), False, True);
        xMessage := WsdlOperation.StreamRequest(_progName, True, True, True);
      end
      else
      begin
        xMessage := (WsdlReply.reqBind as TIpmItem).ValuesToBuffer(nil);
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
        LoadValues((WsdlReply.rpyBind as TXml), False, True);
        (WsdlOperation.fltBind as TXml).ResetValues;
        (WsdlOperation.fltBind as TXml).LoadValues((WsdlReply.fltBind as TXml), False, True);
        if WsdlReply.fltBind.Checked then
          xMessage := WsdlOperation.StreamFault(_progName, True)
        else
          xMessage := WsdlOperation.StreamReply(_progName, True);
      end
      else
      begin
        xMessage := (WsdlReply.rpyBind as TIpmItem).ValuesToBuffer(nil);
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

procedure TMainForm .ToolBar8Click (Sender : TObject );
begin

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
  EndEdit;
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  SelectElementsForm.Caption :=
    'Maintain list of elements to check expected values';
  try
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.WsdlOperation := WsdlOperation;
    SelectElementsForm.ControlBinds := WsdlOperation.ExpectationBindables;
    SelectElementsForm.DuplicatesAllowed := False;
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
  SelectExpectedElementsAction.Enabled := Assigned(WsdlOperation) and
    (not se.IsActive);
end;

procedure TMainForm.ReportUnexpectedValuesActionUpdate(Sender: TObject);
begin
  ReportUnexpectedValuesAction.Enabled := { }{ not se.IsActive
    and{ } (se.Wsdls.Count > 0);
end;

procedure TMainForm.ReportUnexpectedValuesActionExecute(Sender: TObject);
var
  d, X, nDiffs: Integer;
  xXml, tableXml: TXml;
  xLog: TLog;
  xBind: TCustomBindable;
  showReqRep: Boolean;
  SwapCursor: TCursor;
begin
  OnlyWhenLicensed;
  xXml := TXml.CreateAsString('html', '');
  with xXml do
  begin
    try
      SwapCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        tableXml := AddXml(TXml.CreateAsString('table', ''));
        with tableXml do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('border', '1'));
          { }
          with AddXml(TXml.CreateAsString('tr', '')) do
          begin
            with AddXml(TXml.CreateAsString('td',
                _progName + ' - Unexpected values report')) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
            with AddXml(TXml.CreateAsString('td', DateToStr(Now))) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
          end;
          with AddXml(TXml.CreateAsString('tr', '')) do
          begin
            with AddXml(TXml.CreateAsString('td', ' ')) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
            with AddXml(TXml.CreateAsString('td', ' ')) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
          end;
          { }
          with AddXml(TXml.CreateAsString('tr', '')) do
          begin
            with AddXml(TXml.CreateAsString('td',
                'Messages with unexpected values')) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
            with AddXml(TXml.CreateAsString('td', '_')) do
              AddAttribute(TXmlAttribute.CreateAsString('colspan', '3'));
          end;
          with AddXml(TXml.CreateAsString('tr', '')) do
          begin
            AddXml(TXml.CreateAsString('td', 'Row '));
            AddXml(TXml.CreateAsString('td', 'Sent '));
            AddXml(TXml.CreateAsString('td', 'Correlation '));
            AddXml(TXml.CreateAsString('td', 'Tag '));
            AddXml(TXml.CreateAsString('td', 'Value '));
            AddXml(TXml.CreateAsString('td', 'Reference '));
          end;
          AcquireLock;
          try
            for X := 0 to se.displayedLogs.Count - 1 do
            begin
              xLog := se.displayedLogs.LogItems[X];
              xLog.toBindables;
              se.CheckExpectedValues(xLog, xLog.Operation, True);
              if xLog.HasUnexpectedValue then
              begin
                nDiffs := 0;
                for d := 0 to xLog.Operation.ExpectationBindables.Count - 1 do
                  if xLog.Operation.ExpectationBindables.Bindables[d]
                    .HasUnexpectedValue then
                    Inc(nDiffs);
                showReqRep := True;
                for d := 0 to xLog.Operation.ExpectationBindables.Count - 1 do
                begin
                  xBind := xLog.Operation.ExpectationBindables.Bindables[d];
                  if xBind.HasUnexpectedValue then
                  begin
                    with tableXml.AddXml(TXml.CreateAsString('tr', '')) do
                    begin
                      AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
                      if showReqRep then
                      begin
                        with AddXml(TXml.CreateAsString('td', '')) do
                        begin
                          AddAttribute(TXmlAttribute.CreateAsString('rowspan',
                              IntToStr(nDiffs)));
                          AddAttribute(TXmlAttribute.CreateAsString('valign',
                              'top'));
                          with AddXml(TXml.CreateAsString('a', IntToStr(X + 1)))
                            do
                          begin
                            // AddAttribute(TXmlAttribute.CreateAsString('name', 'Row' + IntToStr (x+1)));
                          end;
                        end;
                        with AddXml(TXml.CreateAsString('td', '')) do
                        begin
                          AddAttribute(TXmlAttribute.CreateAsString('rowspan',
                              IntToStr(nDiffs)));
                          with AddXml(TXml.CreateAsString('a',
                              FormatDateTime('hh:nn:ss', xLog.InboundTimeStamp))
                            ) do
                          begin
                            // AddAttribute(TXmlAttribute.CreateAsString('href', '#Request' + IntToStr (x+1)));
                          end;
                        end;
                        with AddXml(TXml.CreateAsString('td',
                            xLog.CorrelationId)) do
                        begin
                          AddAttribute(TXmlAttribute.CreateAsString('rowspan',
                              IntToStr(nDiffs)));
                        end;
                        showReqRep := False;
                      end;
                      AddXml(TXml.CreateAsString('td', xBind.FullIndexCaption));
                      AddXml(TXml.CreateAsString('td', xBind.Value + '_'));
                      AddXml(TXml.CreateAsString('td', xBind.ExpectedValue + '_')
                        );
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
        Screen.Cursor := SwapCursor;
      end;
      ShowHtml(_progName + ' - Unexpected values report', xXml.asHtmlString);
    finally
      Free;
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
    se.UpdateMessageRow(WsdlOperation, WsdlReply);
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
    if EditXmlXsdBased('Service Options', '', '', '', False,
      serviceOptionsXsd, xXml) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        OptionsFromXml(xXml);
      finally
        ReleaseLock;
      end;
    end;
  end;
end;

procedure TMainForm.LogUsageTimerTimer(Sender: TObject);
begin
  try
    try
      try
        if OpenLogUsageDatabase then
          LogUsage(WindowsUserName);
      finally
        SqlConnector.Connected := False;
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
  xXml := se.cobolOperationsXml;
  if EditXmlXsdBased('Cobol Operations', 'OperationDefs.CobolOperations',
    'CobolOperations.Operation.Name', 'CobolOperations.Operation.Name',
    se.IsActive, OperationDefsXsd, xXml) then
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

procedure TMainForm.CobolOperationsActionUpdate(Sender: TObject);
begin
  CobolOperationsAction.Enabled := not se.IsActive;
end;

procedure TMainForm.ConfigListenersActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := se.Listeners.AsXml;
  if EditXmlXsdBased('Configure Listeners', '', '', '', se.IsActive,
    listenersConfigXsd, xXml) then
  begin
    stubChanged := True;
    se.Listeners.FromXml(xXml, se.HaveStompFrame);
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
  if EditXmlXsdBased('Project log options', 'projectOptions.Log',
    'Log.maxEntries', '', False, projectOptionsXsd, xXml) then
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
        DownPageControl.ActivePage := MessagesTabSheet;
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
  readLog4jEventsAction.Enabled := not se.IsActive and (se.Wsdls.Count > 0) and
    (se.displayedLogs.Count = 0);
end;

procedure TMainForm.readLog4jEventsActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (se.Wsdls.Count = 0) then
    HintStr := HintStr + ' (no WSDLS read)';
  if (se.displayedLogs.Count > 0) then
    HintStr := HintStr + ' (List of messages must be empty)';
end;

procedure TMainForm.OpenLog4jEvents(aString: String; aIsFileName: Boolean;
  aLogList: TLogList);
  function _CreateMessage(aXml: TXml; isRequest: Boolean;
    aOperation: TWsdlOperation): String;
  var
    X: Integer;
  begin
    result := '';
    if isRequest then
    begin
      if aOperation.reqBind is TXml then
        with aOperation.reqBind as TXml do
        begin
          ResetValues;
          for X := 0 to aXml.Items.Count - 1 do
            if X < Items.Count then
              Items.XmlItems[X].LoadValues(aXml.Items.XmlItems[X], False,
                False);
        end;
      result := aOperation.StreamRequest(_progName, True, True, True);
    end
    else
    begin
      if aOperation.rpyBind is TXml then
        with aOperation.rpyBind as TXml do
        begin
          ResetValues;
          for X := 0 to aXml.Items.Count - 1 do
            if X < Items.Count then
              Items.XmlItems[X].LoadValues(aXml.Items.XmlItems[X], False,
                False);
        end;
      result := aOperation.StreamReply(_progName, True);
    end;
    result := result + aOperation.Name + ':' + aXml.Name;
  end;
  procedure _DiscoverOperationFromTag(aTag: String; var isRequest: Boolean;
    var aOperation: TWsdlOperation);
  var
    f: Integer;
  begin
    aOperation := nil;
    if allOperations.Find(NameWithoutPrefix(aTag), f) then
    begin
      aOperation := allOperations.Operations[f];
      isRequest := True;
      exit;
    end;
    if allOperationsRpy.Find(NameWithoutPrefix(aTag), f) then
    begin
      aOperation := allOperations.Operations[f];
      isRequest := False;
      exit;
    end;
  end;
  procedure _DiscoverOperation(aXml: TXml; var isRequest: Boolean;
    var aOperation: TWsdlOperation; var aMessage: String);
    procedure _DiscoverOperationFromString(aText: String;
      var isRequest: Boolean; var aOperation: TWsdlOperation;
      var aMessage: String);
    begin
      try
        aOperation := se.FindOperationOnRequest(nil, '', aText, False);
      Except
      End;
      if Assigned(aOperation) then
      begin
        aMessage := aText;
        isRequest := True;
        exit;
      end;
      try
        aOperation := se.FindOperationOnReply(aText);
      Except
      End;
      if Assigned(aOperation) then
      begin
        aMessage := aText;
        isRequest := False;
        exit;
      end;
    end;

  var
    X: Integer;
  begin
    aOperation := nil;
    if aXml.Items.Count = 0 then
    begin
      _DiscoverOperationFromString(aXml.Value, isRequest, aOperation, aMessage);
      exit;
    end;
    for X := 0 to aXml.Items.Count - 1 do
    begin
      _DiscoverOperationFromTag(aXml.Items.XmlItems[X].Name, isRequest,
        aOperation);
      if Assigned(aOperation) then
      begin
        aMessage := _CreateMessage(aXml, isRequest, aOperation);
        exit;
      end;
    end;
    for X := 0 to aXml.Items.Count - 1 do
    begin
      _DiscoverOperation(aXml.Items.XmlItems[X], isRequest, aOperation,
        aMessage);
      if Assigned(aOperation) then
        exit;
    end;
  end;

var
  xXml: TXml;
  SwapCursor: TCursor;
  X, Y: Integer;
  xLog: TLog;
  xOperation: TWsdlOperation;
  xMessageText: String;
  mainTag: String;
  isRequest: Boolean;
begin
  try
    mainTag := 'log4j_event';
    SwapCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    xXml := TXml.Create;
    aLogList.designSuspect := False;
    xLog := TLog.Create;
    try
      if aIsFileName then
        xXml.LoadFromFile(aString, nil)
      else
        xXml.LoadFromString(aString, nil);
      if (xXml.Items.Count > 0) and (xXml.Items.XmlItems[0].TagName <> mainTag)
        then
      begin
        if not BooleanPromptDialog(Format(
            'Found tag <%s>; Expected <%s>, Continue',
            [xXml.Items.XmlItems[0].TagName, mainTag])) then
          exit;
        mainTag := xXml.Items.XmlItems[0].TagName;
      end;
      for X := 0 to xXml.Items.Count - 1 do
      begin
        with xXml.Items.XmlItems[X] do
        begin
          if TagName = mainTag then
          begin
            for Y := 0 to Items.Count - 1 do
            begin
              _DiscoverOperation(Items.XmlItems[Y], isRequest, xOperation,
                xMessageText);
              if Assigned(xOperation) then
              begin
                xLog.Operation := xOperation;
                if isRequest then
                  xLog.RequestBody := xMessageText
                else
                  xLog.ReplyBody := xMessageText;
              end;
            end;
          end;
          if Assigned(xLog.Operation) then
          begin
            xLog.CorrelationId := xLog.Operation.CorrelationIdAsText('; ');
            // xLog.Exception := ????????????TODO;
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
    Screen.Cursor := SwapCursor;
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
    ResetEnvVars('.*');
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
  xXml := se.swiftMtOperationsXml;
  if EditXmlXsdBased('SwiftMT Operations', 'OperationDefs.SwiftMtOperations',
    'SwiftMtOperations.Operation.Name', 'SwiftMtOperations.Operation.Name',
    se.IsActive, OperationDefsXsd, xXml) then
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

procedure TMainForm.SwiftMtOperationsActionUpdate(Sender: TObject);
begin
  SwiftMtOperationsAction.Enabled := not se.IsActive;
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

procedure TMainForm.RemoveScriptMenuItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    se.Scripts.Objects[Tag].Free;
    se.Scripts.Delete(Tag);
    CreateScriptsSubMenuItems;
    stubChanged := True;
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
    if aBind.Root = WsdlReply.rpyBind then
      WsdlReply.fltBind.Checked := False
    else if Assigned(WsdlReply.rpyBodyBind) then
      WsdlReply.rpyBodyBind.Checked := False;
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

procedure TMainForm .RequestBodyTabSheetContextPopup (Sender : TObject ;
  MousePos : TPoint ; var Handled : Boolean );
begin

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
  for o := 0 to allOperations.Count - 1 do
  begin
    if allOperations.Operations[o] <> WsdlOperation then
    begin
      xEnum := TXsdEnumeration.Create;
      xEnum.Value := allOperations.Operations[o].reqTagName;
      xXsd.sType.Enumerations.AddObject(xEnum.Value, xEnum);
    end;
  end;
  with WsdlOperation do
  begin
    xXml := OptionsAsXml;
    if EditXmlXsdBased('Operation Options', '', '', '', False,
      operationOptionsXsd, xXml) then
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
  CollapseHeaders := False;
  isSlaveMode := False;
  MasterAddress := '';
  AutoRefreshSlave := False;
  se.doLoadFromMasterOnStartUp := False;
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
    xXml := XmlCheckedItemByTag['MasterSlave'];
    if Assigned(xXml) then
    begin
      isSlaveMode := xXml.Items.XmlCheckedBooleanByTagDef['SlaveEnabled',
        isSlaveMode];
      MasterAddress := xXml.Items.XmlCheckedValueByTagDef['MasterAddress',
        MasterAddress];
      AutoRefreshSlave := xXml.Items.XmlCheckedBooleanByTagDef['AutoRefresh',
        AutoRefreshSlave];
      se.doLoadFromMasterOnStartUp := xXml.Items.XmlCheckedBooleanByTagDef
        ['LoadOnStartup', se.doLoadFromMasterOnStartUp];
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

procedure TMainForm .DataTypeDocumentationMemoClick (Sender : TObject );
begin
  OpenUrl(MemoIsLink(DataTypeDocumentationMemo));
end;

procedure TMainForm .MessagesTabControlChange (Sender : TObject );
begin
  UpdateLogTabs(NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode));
end;

procedure TMainForm .MessagesTabControlGetImageIndex (Sender : TObject ;
  TabIndex : Integer ; var ImageIndex : Integer );
var
  aLog: TLog;
begin
  ImageIndex := logValidationTabImageIndex;
end;

procedure TMainForm .MessagesVTSChange (Sender : TBaseVirtualTree ;
  Node : PVirtualNode );
begin

end;

{$ifdef windows}
initialization
  CoInitialize(nil);

finalization
  CoUninitialize;
{$endif}
end.
