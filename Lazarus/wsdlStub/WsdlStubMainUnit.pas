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
  sqldb, LCLIntf, LCLType,
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
   , xmlio
   , Wsdlz
   , Xmlz
   , Xsdz
   , StdCtrls
   , IdUri
   , ComCtrls
   , ExtCtrls
   , FormIniFilez
   , Menus
   , PairSplitter, HtmlView
   , VirtualTrees
   , LazFileUtils
   , FileUtil
   , IpHtml
   , Bind
   , ParserClasses
   , types
   , ClaimListz
   , progressinterface
   , MarkdownUtils
   , MarkdownProcessor
   , HtmlGlobals;

type
  THackControl = class(TWinControl)
  public
    FHandle: HWnd;
    FParentWindow: HWnd;
  end;

  TShowKindOfInformation = (spNotifications, spSnapshots, spMessages);
  TShowKindOfLogData = (slRequestHeaders, slRequestBody, slReplyHeaders, slReplyBody, slException, slValidation);
  TLogPanelIndex = (lpiFocus, lpiThreads);
  TCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
  TProcedure = procedure of Object;
  TProcedureString = procedure(arg: String) of Object;
  TProcedureOperation = procedure(arg: TWsdlOperation) of Object;

  { TMainForm }

  TMainForm = class(TForm)
    AboutApiServerAction: TAction;
    FocusOnOperationMenuItem: TMenuItem;
    DocumentationViewer: THtmlViewer;
    OperationDocumentationViewer: THtmlViewer;
    MenuItem14: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem72: TMenuItem;
    NavigateOperationsPopupMenu: TPopupMenu;
    NvgtView: TVirtualStringTree;
    Splitter2: TSplitter;
    ToggleDebugLogModeAction: TAction;
    ToolButton49: TToolButton;
    ToolButton52: TToolButton;
    ToolButton62: TToolButton;
    WsdlServiceNameEdit: TEdit;
    WsdlNameEdit: TEdit;
    NavigateHierarchyAction: TAction;
    WsdlOperationNameEdit: TEdit;
    procedure AboutApiServerActionExecute(Sender: TObject);
    procedure AboutApiServerActionUpdate(Sender: TObject);
    procedure EditCloudEnvironmentActionUpdate(Sender: TObject);
    procedure FocusOnOperationMenuItemClick(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MessagesVTSColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure MessagesVTSHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure NavigateHierarchyActionExecute(Sender: TObject);
    procedure NavigateHierarchyActionUpdate(Sender: TObject);
    procedure DocumentationViewerHotSpotClick(Sender: TObject;
      const SRC: ThtString; var Handled: Boolean);
    procedure HtmlViewerKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure SQLConnectorLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
    procedure TreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
  private
    fFocusedBind: TCustomBindable;
    fFocusedOperation: TWsdlOperation;
    fFocusedMessage: TWsdlMessage;
    fFocusedDocumentationObject: TObject;
    fIpmDescrType: TIpmDescrType;
    fShowKindOfInformation: TShowKindOfInformation;
    fShowKindOfLogData: TShowKindOfLogData;
    procedure setFocusedBind(Value: TCustomBindable);
    procedure setIpmDescrType(Value: TIpmDescrType);
    procedure setShowKindOfInformation(Value: TShowKindOfInformation);
    procedure setShowKindOfLogData(Value: TShowKindOfLogData);
  published
    AbortMenuItem : TMenuItem ;
    AbortAction : TAction ;
    Action2 : TAction ;
    EditCloudEnvironmentAction: TAction;
    CloudProjectInformationAction: TAction;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem71: TMenuItem;
    ShowProjectInfoAction: TAction;
    MenuItem66: TMenuItem;
    ShowOperationInfoAction: TAction;
    PasteProjectFromClipboardAction: TAction;
    LogsFromHttpGetAction: TAction;
    SetApiServerConnectionAction: TAction;
    SnapshotFromHttpGetAction: TAction;
    CheckReferencedFilesExistInCloudAction: TAction;
    EasterEggPopupMenu: TPopupMenu;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    SaveRemoteApiUiProjectAction: TAction;
    CopyRemoteApiUiProjectAction: TAction;
    SnapshotsFromHttpGetAgainAction: TAction;
    GenerateFunctopnPrototypeListAction: TAction;
    SnapshotsFromHttpGetAction: TAction;
    GenerateJsonSchemaInYaml: TAction;
    Action4: TAction;
    GenerateSwaggerAction: TAction;
    MenuItem30: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    OperationsPopupHelpItem: TMenuItem;
    ToggleTrackDuplicateMessagesAction: TAction;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    CopyToClipboardAs: TMenuItem;
    MenuItem54: TMenuItem;
    CopyToClipboardAsJsonMenuItem: TMenuItem;
    ToolButton53: TToolButton;
    ToolButton75: TToolButton;
    ToolButton79: TToolButton;
    ToolButton80: TToolButton;
    ToolButton81: TToolButton;
    YamlToClipboardMenuItem: TMenuItem;
    ToolButton78: TToolButton;
    OperationBrowseDocumentationAction: TAction;
    ToggleTrackIOAction: TAction;
    ApiByExampleAction: TAction;
    ContextsAction: TAction;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem41: TMenuItem;
    GridColumnWidthMenuItem: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    OpenProjectAction: TAction;
    SaveProjectAsFolderAction: TAction;
    IntrospectDesignAction: TAction;
    EditMessageDocumentationAction: TAction;
    MenuItem37: TMenuItem;
    AddChildElementMenuItem: TMenuItem;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ToggleDoScrollMessagesIntoViewAction: TAction;
    ToolButton36: TToolButton;
    ToolButton68: TToolButton;
    ToolButton69: TToolButton;
    ToolButton71: TToolButton;
    ToolButton72: TToolButton;
    ToolButton73: TToolButton;
    ToolButton74: TToolButton;
    LastMessageToolButton: TToolButton;
    ToolButton76: TToolButton;
    EditMessageAfterScriptAction : TAction ;
    EditMessageScriptAction : TAction ;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35 : TMenuItem ;
    EditMessageScriptMenuItem : TMenuItem ;
    MenuItem38 : TMenuItem ;
    SnapshotsFromFolderAction : TAction ;
    ShowResolvedProperties : TAction ;
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
    NotificationsPanel: TPanel;
    Panel8: TPanel;
    SnapshotsVTS: TVirtualStringTree;
    Splitter7: TSplitter;
    Splitter8: TSplitter;
    SummaryReportAction : TAction ;
    MenuItem26 : TMenuItem ;
    ShowSnapshotDifferencesAction : TAction ;
    LoadRefSavepointMessagesMenuItem : TMenuItem ;
    LoadSavepointMessagesMenuItem : TMenuItem ;
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
    ToolBar1: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    MessagesTabToolBar: TToolBar;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton30: TToolButton;
    ToolButton32: TToolButton;
    ToolButton37: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton42: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton64: TToolButton;
    ToolButton65 : TToolButton ;
    ToolButton66 : TToolButton ;
    ToolButton67: TToolButton;
    ReportOnSnapshotsAction : TAction ;
    ClearSnapshotsAction : TAction ;
    MenuItem4 : TMenuItem ;
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
    TreeView : TVirtualStringTree ;
    LastToolButton : TToolButton ;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    DesignPanelSplitVerticalMenuItem : TMenuItem ;
    MenuItem13 : TMenuItem ;
    OperationRefreshMenuItem : TMenuItem ;
    MenuItem15 : TMenuItem ;
    MenuItem16 : TMenuItem ;
    MenuItem17 : TMenuItem ;
    MenuItem18 : TMenuItem ;
    MenuItem19 : TMenuItem ;
    UnhideOperationMenuItem : TMenuItem ;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    OperationAliasAction : TAction ;
    logChartAction : TAction ;
    LoadTestAction : TAction ;
    CopyLogGridToClipBoardAction : TAction ;
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
    ToolButton7 : TToolButton ;
    ToolButton8 : TToolButton ;
    ToolButton9 : TToolButton ;
    View : TLabel ;
    ViewStyleComboBox : TComboBox ;
    WsdlInfoPanel : TPanel ;
    WsdlLabel : TLabel ;
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
    TreeviewImageList: TImageList;
    InWsdlPropertiesListView: TListView;
    TreePopupMenu: TPopupMenu;
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
    ExportProjectAction: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    SaveFileDialog: TSaveDialog;
    ImportProjectAction: TAction;
    ToolButton5: TToolButton;
    OpenStubCase1: TMenuItem;
    SaveStubCaseas1: TMenuItem;
    N5: TMenuItem;
    ReopenStubCaseAction: TAction;
    ToolButton6: TToolButton;
    ReopenStubcase1: TMenuItem;
    Help1: TMenuItem;
    wsdStubhelp1: TMenuItem;
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
    SaveStubCaseAction: TAction;
    SaveStubCase1: TMenuItem;
    ActionToolBar: TToolBar;
    ActionComboBox: TComboBox;
    ToolButton17: TToolButton;
    RedirectAddressButton: TToolButton;
    Splitter6: TSplitter;
    ToolButton21: TToolButton;
    ClearLogItemsAction: TAction;
    NewStubCaseAction: TAction;
    NewStubCase1: TMenuItem;
    N6: TMenuItem;
    ToolButton15: TToolButton;
    ToolButton23: TToolButton;
    ClearNotificationsAction: TAction;
    SelectMessageColumnsAction: TAction;
    CopyGridAction: TAction;
    PasteGridAction: TAction;
    CopyLogMemoTextToClipBrdAction: TAction;
    ShowHttpReplyAsXMLAction: TAction;
    ShowHttpRequestAsXMLAction: TAction;
    View1: TMenuItem;
    SchemapropertiesMenuItem: TMenuItem;
    ListofOperationsMenuItem: TMenuItem;
    WsdlInformationMenuItem: TMenuItem;
    WsdlPopulateMenuItem: TMenuItem;
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
    LogPopupMenu: TPopupMenu;
    FindAction: TAction;
    FindNextAction: TAction;
    N14: TMenuItem;
    Validate1: TMenuItem;
    CheckTreeAction: TAction;
    Project1: TMenuItem;
    Options2: TMenuItem;
    ProjectOptionsAction: TAction;
    ToolButton48: TToolButton;
    All1: TMenuItem;
    Required1: TMenuItem;
    ShowReplyAsXmlGridAction: TAction;
    ShowRequestAsXmlGridAction: TAction;
    ToggleBetaModeAction: TAction;
    Action1: TAction;
    ShowRequestHeaderAsXmlAction: TAction;
    GridPopupMenu: TPopupMenu;
    Log2DesignAction: TAction;
    Addtodesign1: TMenuItem;
    ViewMssgAsTextAction: TAction;
    CopyCobolDataToClipboardMenuItem: TMenuItem;
    PasteCobolDataFromClipboardMenuItem: TMenuItem;
    ElementvalueMenuItem: TMenuItem;
    AssignExpressionMenuItem: TMenuItem;
    WsdlItemChangeDataTypeMenuItem: TMenuItem;
    DataTypeDependingMenu: TMenuItem;
    ServiceOptionsAction: TAction;
    ServiceMenu: TMenuItem;
    Options3: TMenuItem;
    ConfigListenersAction: TAction;
    ToolButton58: TToolButton;
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
    ConfigLogAction: TAction;
    DelayTimeButton: TToolButton;
    ToggleFileLogAction: TAction;
    RedirectAddressAction: TAction;
    OperationDelayResponseTimeAction: TAction;
    OperationApplySettingsAction: TAction;
    OperationWsaAction: TAction;
    ThrowExceptionAction: TAction;
    Configurelisteners1: TMenuItem;
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
    ToggleNotifyAction: TAction;
    SaveLogRequestsToFileAction: TAction;
    N27: TMenuItem;
    SaveLogRepliesToFileAction: TAction;
    ShowRequestAsHtmlAction: TAction;
    ShowReplyAsHtmlAction: TAction;
    ShowExceptAsHtmlAction: TAction;
    OperationOptionsAction: TAction;
    HideAllOperationsAction: TAction;
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
    WsdlsPopupMenu: TPopupMenu;
    Maintainlistoffreeformatoperations1: TMenuItem;
    Maintainlistofcoboloperations1: TMenuItem;
    MaintainlistofWSDLfiles1: TMenuItem;
    N33: TMenuItem;
    ShowReplyHeaderAsXmlAction: TAction;
    N34: TMenuItem;
    ProjectCleanAction: TAction;
    Clean1: TMenuItem;
    HelpMainMenuAction: TAction;
    N35: TMenuItem;
    GenerateScriptAssignmentAction: TAction;
    ToolButton70: TToolButton;
    AbortToolButton: TToolButton;
    RefreshLogTimer: TTimer;
    ZoomasScriptAssignments1: TMenuItem;
    Generate1: TMenuItem;
    XSDreportinClipBoardSpreadSheet1: TMenuItem;
    SeparatorToolButton: TToolButton;
    procedure FocusNavigatorOnOperation;
    procedure CloudProjectInformationActionExecute(Sender: TObject);
    procedure CloudProjectInformationActionUpdate(Sender: TObject);
    procedure EditCloudEnvironmentActionExecute(Sender: TObject);
    procedure PasteProjectFromClipboardActionExecute(Sender: TObject);
    procedure CheckReferencedFilesExistInCloudActionExecute(Sender: TObject);
    procedure AddChildElementMenuItemClick(Sender: TObject);
    procedure ApiByExampleActionUpdate(Sender: TObject);
    procedure CobolOperationsActionUpdate(Sender: TObject);
    procedure ContextsActionExecute(Sender: TObject);
    procedure CopyRemoteApiUiProjectActionExecute(Sender: TObject);
    procedure CopyToClipboardAsJsonMenuItemClick(Sender: TObject);
    procedure EditMessageAfterScriptActionExecute (Sender : TObject );
    procedure EditMessageAfterScriptActionUpdate (Sender : TObject );
    procedure EditMessageDocumentationActionExecute(Sender: TObject);
    procedure EditMessageScriptActionExecute (Sender : TObject );
    procedure DocumentationViewerHotClick(Sender: TObject);
    procedure ApiByExampleActionExecute(Sender: TObject);
    procedure ApiByExampleActionHint(var HintStr: string; var CanShow: Boolean
      );
    procedure FreeFormatsActionUpdate(Sender: TObject);
    procedure GenerateFunctopnPrototypeListActionExecute(Sender: TObject);
    procedure GenerateFunctopnPrototypeListActionUpdate(Sender: TObject);
    procedure GenerateJsonSchemaInYamlExecute(Sender: TObject);
    procedure GenerateSwaggerActionExecute(Sender: TObject);
    procedure LogsFromHttpGetActionExecute(Sender: TObject);
    procedure LogsFromHttpGetActionHint(var HintStr: string;var CanShow: Boolean
      );
    procedure LogsFromHttpGetActionUpdate(Sender: TObject);
    procedure LogTabControlChange(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem43Click(Sender: TObject);
    procedure MenuItem45Click(Sender: TObject);
    procedure MenuItem47Click(Sender: TObject);
    procedure MenuItem57Click(Sender: TObject);
    procedure MenuItem58Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure MenuItem61Click(Sender: TObject);
    procedure OperationsPopupHelpItemClick(Sender: TObject);
    function EditRemoteServerConnectionParams (aCaption: String): Boolean;
    procedure SaveRemoteApiUiProjectActionExecute(Sender: TObject);
    procedure SetApiServerConnectionActionExecute(Sender: TObject);
    procedure ShowOperationInfoActionExecute(Sender: TObject);
    procedure ShowOperationInfoActionUpdate(Sender: TObject);
    procedure ShowProjectInfoActionExecute(Sender: TObject);
    procedure ShowProjectInfoActionUpdate(Sender: TObject);
    procedure SnapshotFromHttpGetActionExecute(Sender: TObject);
    procedure SnapshotsFromHttpGetAgainActionExecute(Sender: TObject);
    procedure SnapshotsFromHttpGetAgainActionUpdate(Sender: TObject);
    procedure ToggleTrackDuplicateMessagesActionExecute(Sender: TObject);
    procedure YamlToClipboardMenuItemClick(Sender: TObject);
    procedure MessagesTabToolBarResize(Sender: TObject);
    procedure NvgtViewGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure OpenWsdlActionUpdate(Sender: TObject);
    procedure OperationBrowseDocumentationActionExecute(Sender: TObject);
    procedure OperationBrowseDocumentationActionUpdate(Sender: TObject);
    procedure OperationDocumentationViewerClick(Sender: TObject);
    procedure PromptAndSetColumnWidth (aTreeView: TVirtualStringTree);
    procedure GridColumnWidthMenuItemClick(Sender: TObject);
    procedure AbortActionUpdate (Sender : TObject );
    procedure ClearSnapshotsActionExecute (Sender : TObject );
    procedure CopyLogGridToClipBoardActionExecute (Sender : TObject );
    procedure DesignPanelSplitVerticalMenuItemClick (Sender : TObject );
    procedure GridPopupMenuPopup (Sender : TObject );
    procedure TreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
      TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
      const CellRect : TRect );
    procedure LoadTestActionExecute (Sender : TObject );
    procedure LoadTestActionUpdate (Sender : TObject );
    procedure logChartActionExecute (Sender : TObject );
    procedure DesignSplitHorizontalMenuItemClick (Sender : TObject );
    procedure DesignSplitVerticalMenuItemClick (Sender : TObject );
    procedure OperationRefreshMenuItemClick (Sender : TObject );
    procedure MenuItem17Click (Sender : TObject );
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure NvgtViewClick(Sender: TObject);
    procedure NvgtViewGetImageIndex (Sender : TBaseVirtualTree ;
      Node : PVirtualNode ; Kind : TVTImageKind ; Column : TColumnIndex ;
      var Ghosted : Boolean ; var ImageIndex : Integer );
    procedure IntrospectDesignActionExecute(Sender: TObject);
    procedure CheckFolderAndFileNames;
    procedure SaveProjectAsFolderActionExecute(Sender: TObject);
    procedure SaveStubCaseActionUpdate(Sender: TObject);
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
    procedure ToggleDoScrollMessagesIntoViewActionExecute(Sender: TObject);
    procedure ToggleTrackIOActionExecute(Sender: TObject);
    procedure OperationAliasActionExecute (Sender : TObject );
    procedure OperationDelayResponseTimeActionExecute(Sender: TObject);
    procedure NvgtViewPaintText (Sender : TBaseVirtualTree ;
      const TargetCanvas : TCanvas ; Node : PVirtualNode ;
      Column : TColumnIndex ; TextType : TVSTTextType );
    procedure PresentLogMemoTextActionExecute (Sender : TObject );
    procedure PresentLogMemoTextActionUpdate (Sender : TObject );
    procedure ProjectDesignToClipboardActionExecute(Sender: TObject);
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
    procedure SchemasToZipExecute (Sender : TObject );
    procedure ShowGridDifferencesActionExecute (Sender : TObject );
    procedure ShowLogDetailsActionExecute(Sender: TObject);
    procedure RemoveAllMessagesActionUpdate(Sender: TObject);
    procedure RemoveAllMessagesActionExecute(Sender: TObject);
    procedure EnableMessageActionExecute(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure Revalidatemessages1Click(Sender: TObject);
    procedure ConfigListenersActionUpdate(Sender: TObject);
    procedure ConfigListenersActionExecute(Sender: TObject);
    procedure ServiceOptionsActionExecute(Sender: TObject);
    procedure ChangeXmlDataType(aXml: TXml; aDataType: TXsdDataType);
    procedure ChangeDataTypeMenuItemClick(Sender: TObject);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Action1Execute(Sender: TObject);
    procedure ShowRemarksActionExecute(Sender: TObject);
    procedure ToggleDebugLogModeActionExecute(Sender: TObject);
    procedure ShowRequestAsXmlGridActionExecute(Sender: TObject);
    procedure ShowRequestAsXmlGridActionUpdate(Sender: TObject);
    procedure ShowReplyAsXmlGridActionExecute(Sender: TObject);
    procedure Required1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure AbortActionExecute(Sender: TObject);
    procedure GridToolBarResize(Sender: TObject);
    procedure TreeViewFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure ViewStyleComboBoxChange(Sender: TObject);
    function getXmlViewType: TxvViewType;
    procedure ProjectOptionsActionExecute(Sender: TObject);
    procedure CheckTreeActionExecute(Sender: TObject);
    procedure CheckTreeActionUpdate(Sender: TObject);
    procedure Validate1Click(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FindNextActionUpdate(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindActionUpdate(Sender: TObject);
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
    procedure TreePopupMenuPopup(Sender: TObject);
    procedure MessagesVTSClick(Sender: TObject);
    procedure MessagesVTSGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure doExecuteRequest;
    procedure doRevalidateMessages;
    procedure doSaveLogRequestsToDisk;
    procedure doSaveLogRepliesToDisk;
    procedure doSaveMessagesToDisk;
    procedure doReadMessagesFromDisk;
    procedure ExecuteRequestActionExecute(Sender: TObject);
    procedure RemoveEnvironmentAction1Click(Sender: TObject);
    procedure AddEnvironmentActionExecute(Sender: TObject);
    procedure RemoveEnvironmentActionUpdate(Sender: TObject);
    procedure AddEnvironmentActionUpdate(Sender: TObject);
    procedure EditEnvironmentActionExecute(Sender: TObject);
    procedure TreeViewChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure DesignPanelAtTopMenuItemClick(Sender: TObject);
    procedure CheckGridFieldsActionExecute(Sender: TObject);
    procedure CheckGridFieldsActionUpdate(Sender: TObject);
    procedure WsdlInformationMenuItemClick(Sender: TObject);
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
    procedure NvgtViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ExceptionsVTSGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure ExceptionsVTSFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ClearNotificationsActionExecute(Sender: TObject);
    procedure ClearNotificationsActionUpdate(Sender: TObject);
    procedure NewStubCaseActionExecute(Sender: TObject);
    procedure MessagesVTSGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure MessagesVTSFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VTSEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure ClearLogItemsActionExecute(Sender: TObject);
    procedure ClearLogItemsActionUpdate(Sender: TObject);
    procedure WsdlInfoPanelResize(Sender: TObject);
    function CheckHttpAddress (aBind: TObject; aNewValue: String): Boolean;
    procedure RedirectAddressActionExecute(Sender: TObject);
    procedure TreeViewResize(Sender: TObject);
    procedure GridViewExit(Sender: TObject);
    procedure TreeViewExit(Sender: TObject);
    procedure SaveStubCaseActionExecute(Sender: TObject);
    procedure GridViewEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
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
    procedure runScriptActionExecute(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ReopenStubCaseActionExecute(Sender: TObject);
    procedure ImportProjectActionExecute(Sender: TObject);
    procedure ExportProjectActionExecute(Sender: TObject);
    procedure EditScriptButtonClick(Sender: TObject);
    procedure Expand2Click(Sender: TObject);
    procedure XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
    procedure XmlZoomValueAsTextMenuItemClick(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure FullExpand1Click(Sender: TObject);
    procedure WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure XmlAddMenuItemClick(Sender: TObject);
    procedure TreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure TreeViewNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure TreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeViewEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure TreeViewEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ShowFocusedMessageInTreeView;
    procedure setFocusedMessage(const Value: TWsdlMessage);
    procedure setFocusedOperation(const Value: TWsdlOperation);
    procedure Exit1Click(Sender: TObject);
    procedure OpenWsdlActionExecute(Sender: TObject);
    procedure OpenWsdlActionHint(var HintStr: string; var CanShow: Boolean);
    procedure GridViewFocusedNode(aNode: PVirtualNode);
    procedure NvgtViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure GridViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure OperationWsaActionExecute(Sender: TObject);
    procedure OperationWsaActionUpdate(Sender: TObject);
    procedure ThrowExceptionActionExecute(Sender: TObject);
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
    procedure ToggleNotifyActionExecute(Sender: TObject);
    procedure SaveLogRequestsToFileActionUpdate(Sender: TObject);
    procedure SaveLogRepliesToFileActionUpdate(Sender: TObject);
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
    procedure ProjectCleanActionExecute(Sender: TObject);
    procedure GenerateMenuHelpActionExecute(Sender: TObject);
    procedure GenerateScriptAssignmentActionExecute(Sender: TObject);
    procedure ConfigLogActionExecute(Sender: TObject);
    procedure RefreshLog;
    procedure RefreshLogTimerTimer(Sender: TObject);
    procedure ZoomasScriptAssignments1Click(Sender: TObject);
    procedure XSDreportinClipBoardSpreadSheet1Click(Sender: TObject);
    procedure UpdateMessagesView;
    property IpmDescrType: TIpmDescrType read fIpmDescrType write setIpmDescrType;
    property FocusedOperation: TWsdlOperation read fFocusedOperation write
      setFocusedOperation;
    property FocusedMessage: TWsdlMessage read fFocusedMessage write setFocusedMessage;
    property FocusedBind: TCustomBindable read fFocusedBind write setFocusedBind;
    property xmlViewType: TxvViewType read getXmlViewType;
    property ShowKindOfInformation: TShowKindOfInformation read fShowKindOfInformation write setShowKindOfInformation;
    property ShowKindOfLogData: TShowKindOfLogData read fShowKindOfLogData write setShowKindOfLogData;
  private
    fWsdlService: TWsdlService;
    doConfirmTemporaryInactivity: Boolean;
    doStartOnOpeningProject: Boolean;
    ProgressInterface: TProgressInterface;
    editingNode: PVirtualNode;
    notifyTabCaption, MessagesTabCaption: String;
    notifyTabImageIndex: Integer;
    logValidationTabImageIndex: Integer;
    startStopShortCut: TShortCut;
    fLastCaption: String;
    QueueNameList: TJBStringList;
    isOptionsChanged: Boolean;
    doScrollExceptionsIntoView: Boolean;
    DisclaimerAccepted: Boolean;
    CompanyName: String;
    fDoShowDesignAtTop: Boolean;
    grid_x, grid_y: Integer;
    MessagesTabControlWidth, MessagesTabControlMinLeft: Integer;
    fAbortPressed: Boolean;
    doNotify: Boolean;
    GetAuthError: String;
    procedure GridViewUnselect;
    procedure CheckReferencedFilenamesExistsInCloud;
    procedure SnapshotFromRemoteServer (aList: TClaimableObjectList);
    procedure SnapshotsFromRemoteServer;
    procedure GetSnapshotsFromFolder (aList: TSnapshotList; aFolder: String);
    procedure GetSnapshotsFromRemoteServer (slx, sln, slc: TSnapshotList);
    procedure GetSnapshotFromRemoteServer (aSnapshot: TSnapshot);
    procedure ShowHelpDocumentation (aName: String);
    procedure EditContexts;
    function ShowProgressForm: Boolean;
    procedure PositionMessagesTabControl;
    procedure SetOperationZoomPath(aOperation: TWsdlOperation);
    function hintStringFromXsd(aPrefix, aSep, aPostFix: String;
      aXsd: TXsd): String;
    function inImageArea: Boolean;
    procedure ParserError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer; TokenString, Data: String);
    procedure ShowHtml(aCaption, aInfoString: String);
    procedure PopulateXml(aViewType: TxvViewType);
    function getIsRequestAction: Boolean;
    procedure setDoShowDesignAtTop(const Value: Boolean);
    procedure setStubChanged(const Value: Boolean);
    function getWsdl: TWsdl;
    procedure LogServerNotification(const Msg: String);
    procedure LogServerException(const Msg: String; aException: Boolean; E: Exception);
    function HttpActiveHint: String;
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
    procedure FillInWsdlEdits;
    function FillBindTreeView(aTreeView: TVirtualStringTree;
      aBind: TCustomBindable; aParentNode: PVirtualNode): PVirtualNode;
    procedure FillNvgtView(aOperations: TWsdlOperations);
    procedure FillGridView(aTreeView: TVirtualStringTree;
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
    function NodeToMessage(aTreeView: TBaseVirtualTree; aNode: PVirtualNode): TWsdlMessage;
    function NodeToBind(aTreeView: TBaseVirtualTree;
      aNode: PVirtualNode): TCustomBindable;
    // procedure NodeToXml (aTreeView: TBaseVirtualTree; aNode: PVirtualNode; var Xml: TXml; var Attribute: TXmlAttribute);
    procedure RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
    function InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
    procedure ConfirmTemporarelyInactivity(var aPrompt: Boolean);
    function BooleanPromptDialog(aPrompt: String): Boolean;
    function BooleanStringDialog(aPrompt, aDefault: String; var aValue: String): Boolean;
    procedure FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
    procedure EndEdit;
    procedure XmlAnalyserError(Sender: TObject;
      LineNumber, ColumnNumber, Offset: Integer;
      TokenString, Data: String);
    procedure UpdateXmlTreeViewNode(aTreeView: TVirtualStringTree;
      aNode: PVirtualNode);
    procedure ShowInfoForm(aCaption: String; aInfoString: String);
    procedure UpdateInWsdlCheckBoxes;
    procedure ShowReport (aReport: TSnapshot);
    function SaveStubCase: Boolean;
    procedure OpenStubCase;
    procedure ToAllLogList(aLogList: TLogList);
    procedure UpdateReopenList(aList: TJBStringList; aFileName: String);
    procedure PrepareOperation;
    procedure ClearConsole;
    procedure DoColorBindButtons;
    procedure UpdateCaption;
    function OkToOpenStubCase: Boolean;
    procedure FocusOperationsReqVTS;
    procedure ExchangeMessages(fReply, pReply: TWsdlMessage);
    procedure UpdateMessagesGrid;
    procedure UpdateLogCorrelationIds (aWsdlOperation: TWsdlOperation);
    procedure UpdateLogTabs (aLog: TLog);
    procedure RemoveMessageColumns;
    procedure SelectFocusedBindInViews;
    function AddMessage(aCopyNode: PVirtualNode): PVirtualNode;
    procedure PasteGridFromPasteBoard;
    procedure PasteGridOnNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure ShowXmlInGrid(aXml: TXml; aReadOnly: Boolean);
    procedure ShowXmlExtended(aCaption: String; aXml: TXml);
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
    procedure FocusOnFullCaptionOrFirst(aFullCaption: String);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure DebugOperationViewAsXml;
    procedure DebugOperation;
    procedure SynchronizedOnMessageChanged;
    procedure OnMessageChanged(aMessage: TWsdlMessage);
    procedure CheckRpyOrFlt(aBind: TCustomBindable);
    procedure OptionsFromXml(aXml: TXml);
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
    function TestDbsConnection(aXml: TObject): Boolean;
    function TestRemoteServerConnection(aXml: TObject): Boolean;
    procedure ProjectInfoFromRemoteServer;
    function TestProjectFolders(aXml: TObject): Boolean;
    function EditXmlValueAsText(aXml: TObject): Boolean;
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
    fCaptionFileName: String;
    fDoScrollMessagesIntoView: Boolean;
    fdoShowDesignSplitVertical : Boolean ;
    fDoTrackDuplicateMessages: Boolean;
    function getHintStrDisabledWhileActive: String;
    procedure SetCaptionFileName(AValue: String);
    procedure setDoScrollMessagesIntoView(AValue: Boolean);
    procedure setdoShowDesignSplitVertical (AValue : Boolean );
    procedure setDoTrackDuplicateMessages(AValue: Boolean);
    procedure ShowHttpReplyAsXMLActionExecute(Sender: TObject);
    procedure IntrospectDesign;
    function createListOfListsForTypeDefs (aTypeDefs: TXsdDataTypeList): TJBStringList;
    function decorateWithAsterix (aCaption: String; aBoolean: Boolean): String;
  published
  public
    se: TWsdlProject;
    claimedLog: TLog;
    claimedReport: TSnapshot;
    CollapseHeaders: Boolean;
    wsdlStubMessagesFileName, wsdlStubSnapshotsFileName: String;
    log4jEventsFileName: String;
    nStubs: Integer;
    ColumnWidths: TJBStringList;
    WsdlPaths: TJBStringList;
    ReopenCaseList: TJBStringList;
    WindowsUserName: String;
    saveToDiskFileName: String;
    saveToDiskDirectory: String;
    saveToDiskSeparator: String;
    saveToDiskExtention: String;
    FileNameList: TJBStringList;
    scriptPreparedWell: Boolean;
    MainToolBarDesignedButtonCount: Integer;
    StressTestDelayMsMin, StressTestDelayMsMax, StressTestConcurrentThreads, StressTestLoopsPerThread: Integer;
    NumberOfBlockingThreads, NumberOfNonBlockingThreads: Integer;
    SaveNvgtViewOnFocusChanged: TVTFocusChangeEvent;
    SaveGridViewOnFocusChanged: TVTFocusChangeEvent;
    SaveTreeViewOnFocusChanged: TVTFocusChangeEvent;
    procedure DisableViewOnFocusChangeEvents;
    procedure EnableViewOnFocusChangeEvents;
    function setContextProperty (aName: String): String;
    function getContextProperty: String;
    function ActiveAfterPrompt: Boolean;
    function InactiveAfterPrompt: Boolean;
    procedure ShowFocusedBindDocumentation;
    property captionFileName: String read fCaptionFileName write SetCaptionFileName;

    property HintStrDisabledWhileActive
      : String read getHintStrDisabledWhileActive;
    property abortPressed: Boolean read fAbortPressed write SetAbortPressed;
    property doTrackDuplicateMessages: Boolean read fDoTrackDuplicateMessages write setDoTrackDuplicateMessages;
    property doScrollMessagesIntoView: Boolean read fDoScrollMessagesIntoView write setDoScrollMessagesIntoView;
    property isRequestAction: Boolean read getIsRequestAction;
    property doShowDesignAtTop: Boolean read fDoShowDesignAtTop write
      setDoShowDesignAtTop;
    property doShowDesignSplitVertical: Boolean read fdoShowDesignSplitVertical write
      setdoShowDesignSplitVertical;
    property stubChanged: Boolean read getStubChanged write setStubChanged;
    property Wsdl: TWsdl read getWsdl;
    function SelecFolderAndSave: Boolean;
    procedure BeginConsoleUpdate;
    procedure DoUpdateConsole;
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

  PNavigatorTreeRec = ^TNavigatorTreeRec;

  TNavigatorTreeRec = record
    Wsdl: TWsdl;
    Service: TWsdlService;
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
  wsdlListUnit, ErrorFound, ClipBrd, ShowXmlUnit,
  ShowXmlCoverageUnit,logChartzUnit, EditOperationScriptUnit, igGlobals,
  ChooseStringUnit, AboutUnit, StrUtils,
  apiuiconsts,
  DisclaimerUnit,
  PromptUnit, SelectXmlElement, ApplyToUnit, wsaConfigUnit,
  SelectElementsUnit, A2BXmlz,
  ShowLogDifferencesUnit, AddFavouritesUnit,
  LogFilterUnit,
  ShowA2BXmlUnit, FindRegExpDialog,
  XmlGridUnit, IpmGridUnit,
  xmlUtilz {$ifdef windows}, ActiveX{$endif}, EditStamperUnit,
  EditCheckerUnit, Math, vstUtils, DelayTimeUnit, StressTestUnit, xmlxsdparser,
  xmlzConsts, AbZipper
  , exceptionUtils, htmlreportz
  , EditTextUnit
  , EditContextsUnit
  , QueryNewElementUnit
  , SelectProjectFolderUnit
  , progressunit
  , StringListListUnit
  , optionsunit
  ;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TLogColumnEnum =
    ( logRemarksColumn
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
  TMessagesColumnEnum =
    ( messagesColumnBeforeScript
    , messagesColumnAfterScript
    , messagesColumnDocumentation
    );
  const nMessageButtonColumns = 3;

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

function _mainformSetContext (aName: String): String;
begin
  result := MainForm.setContextProperty(aName);
end;

function _mainformGetContext: String;
begin
  result := MainForm.getContextProperty;
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

procedure TMainForm.FoundErrorInBuffer(ErrorString: String; aObject: TObject);
begin
  (aObject as TIpmItem).Value := '?' + _progName + ' Error found: ' + ErrorString;
end;

procedure TMainForm.FreeFormatMemoChange(Sender: TObject);
begin
  if Assigned(FocusedMessage) then
  begin
    if FocusedOperation.StubAction = saRequest then
      FocusedMessage.FreeFormatReq := FreeFormatMemo.Text
    else
      FocusedMessage.FreeFormatRpy := FreeFormatMemo.Text;
    stubChanged := True;
  end;
end;

procedure TMainForm.FreeFormatsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.freeFormatOperationsXml;
  OperationDefsXsd.XsdByCaption ['OperationDefs.FreeFormatOperations.Operation.Annotation']
    .EditProcedure := EditXmlValueAsText;
  if EditXmlXsdBased ( 'Freeformat Operations'
                     , 'OperationDefs.FreeFormatOperations'
                     , 'FreeFormatOperations.Operation.Name'
                     , 'FreeFormatOperations.Operation.Name'
                     , se.IsActive
                     , xXml.Items.Count > 1
                     , esUsed
                     , OperationDefsXsd
                     , xXml
                     , True
                     ) then
  begin
    stubChanged := True;
    BeginConsoleUpdate;
    se.freeFormatOperationsUpdate(xXml);
    IntrospectDesign;
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
  xWsdls: TJBStringList;
  xWsdl: TWsdl;
begin
  if not InactiveAfterPrompt then Exit;
  xWsdls := TJBStringList.Create;
  xWsdls.Sorted := True;
  for w := 0 to se.Wsdls.Count - 1 do
  begin
    if not se.isSpecialWsdl(se.Wsdls.Objects[w] as TWsdl) then
      xWsdls.AddObject(se.Wsdls.Strings[w], se.Wsdls.Objects[w]);
  end;
  try
    Application.CreateForm(TWsdlListForm, WsdlListForm);
    try
      wsdlListForm.remoteServerConnectionXml := se.remoteServerConnectionXml;
      wsdlListForm.EnvVars := se.EnvVars;
      WsdlListForm.ShowOperationsWithEndpointOnly :=
        se.OperationsWithEndpointOnly;
      WsdlListForm.SaveRelativeFilenames := se.SaveRelativeFilenames;
      WsdlListForm.Wsdls := xWsdls;
      WsdlListForm.ShowModal;
      if wsdlListForm.ModalResult = mrOK then
      begin
        if WsdlListForm.stubChanged then
        begin
          stubChanged := True;
          BeginConsoleUpdate;
          se.UpdateWsdlsList(xWsdls);
          IntrospectDesign;
        end;
      end;
    finally
      FreeAndNil(WsdlListForm);
    end;
  finally
    xWsdls.Free;
  end;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FillInWsdlEdits;
begin
  if (FocusedOperation = nil) then
    ActionComboBox.ItemIndex := -1;
  if (FocusedOperation <> nil) then
  begin
    if (FocusedOperation.StubAction = saStub) then
      ActionComboBox.ItemIndex := 0
    else
      ActionComboBox.ItemIndex := 1;
    // SoapActionEdit.Text := FocusedOperation.SoapAction;
    try
      with TMarkdownProcessor.CreateDialect(mdCommonMark) do
      try
        OperationDocumentationViewer.LoadFromString(process(prepareMarkDownText(FocusedOperation.Documentation.Text)));
      finally
        Free;
      end;
    except
    end;
  end;
end;

procedure TMainForm.FillNvgtView(aOperations: TWsdlOperations);
var
  xNode: PVirtualNode;
  xData: PNavigatorTreeRec;
  X: Integer;
  xNvgtViewOnFocusChanged: TVTFocusChangeEvent;
begin
  DisableViewOnFocusChangeEvents;
  try
    NvgtView.EndEditNode;
    NvgtView.Clear;
    NvgtView.RootNodeCount := 0;
    NvgtView.NodeDataSize := SizeOf(TNavigatorTreeRec);
    if aOperations = nil then
      exit;
    for X := 0 to aOperations.Count - 1 do
    begin
      xNode := NvgtView.AddChild(nil);
      xData := NvgtView.GetNodeData(xNode);
      xData.Operation := aOperations.Operations[X];
    end;
    UpdateVisibiltyOfOperations;
    if Assigned (se.LastFocusedOperation) then
      FocusedOperation := se.LastFocusedOperation
    else
      FocusedOperation := NodeToOperation(NvgtView, NvgtView.GetFirst);
  finally
    EnableViewOnFocusChangeEvents;
  end;
end;

procedure TMainForm.FillGridView(aTreeView: TVirtualStringTree;
  aMessages: TWsdlMessages);
var
  xNode: PVirtualNode;
  xData: PMessageTreeRec;
  X: Integer;
  s: String;
begin
  s := FocusedOperation.LastFullCaption;
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
      if FocusedOperation.StubAction = saRequest then
      begin
        xHeaders := FocusedOperation.InputHeaders;
        xXml := FocusedOperation.reqBind as TXml;
      end
      else
      begin
        xHeaders := FocusedOperation.OutputHeaders;
        xXml := FocusedOperation.rpyBind as TXml;
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

procedure TMainForm.setFocusedOperation(const Value: TWsdlOperation);
begin
  if (Value = fFocusedOperation) then Exit;
  fFocusedOperation := Value;
  FocusedMessage := nil;
  try
    DisableViewOnFocusChangeEvents;
    try
      FocusNavigatorOnOperation;
      if Assigned (fFocusedOperation) then
      begin
        se.LastFocusedOperation := Value;
        WsdlNameEdit.Text := Wsdl.FileName;
        IpmDescrType := Value.WsdlService.DescriptionType;
        WsdlServiceNameEdit.Text := Value.WsdlService.Name;
        WsdlOperationNameEdit.Text := Value.Name;
        FillInWsdlEdits;
        GridView.Clear;
        UpdateMessagesGrid;
        FillGridView(GridView, FocusedOperation.Messages);
        DoColorBindButtons;
        EditBetweenScriptMenuItem.Visible := (Value.StubAction = saStub);
        EditBeforeScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
        EditAfterScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
        UpdateVisibiltyTreeView(Value.WsdlService.DescriptionType = ipmDTFreeFormat);
        UpdateMessagesView;
        if Assigned (value.LastFocusedMessage) then
          FocusedMessage := Value.LastFocusedMessage
        else
          FocusedMessage := Value.Messages.Messages[0];
        if Assigned (FocusedMessage) then
          FocusOnFullCaptionOrFirst(FocusedOperation.LastFullCaption);
      end
      else
      begin
        FocusedMessage := nil;
      end;
    finally
      EnableViewOnFocusChangeEvents;
    end;
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
    if ((aXml.Parent as TXml).TypeDef.ContentModel = 'Choice')
    or (Assigned (aXml.Xsd) and (aXml.Xsd.isOneOfGroupLevel > 0))
    then
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

function TMainForm.NodeToMessage(aTreeView: TBaseVirtualTree; aNode: PVirtualNode): TWsdlMessage;
var
  Data: PMessageTreeRec;
begin
  result := nil;
  if Assigned(aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned(Data) then
    begin
      result := Data.Message;
    end;
  end;
end;

procedure TMainForm.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
begin
  try
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
          if Group
          or (    (Assigned(Xsd))
              and (   (TypeDef.ContentModel = 'Empty')
                   or (TypeDef.ElementDefs.Count > 0)
                  )
             ) then
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
      xMessage := NodeToMessage(GridView, GridView.FocusedNode);
      if (xBind is TXml)
      or (xBind is TXmlAttribute) then
      begin
        if Assigned(FocusedOperation)
        and Assigned(xMessage)
        and (   xMessage.reqBind.IsAncestorOf(xBind)
             or (xMessage.reqBind = xBind)
            )
        then
        begin
          with TargetCanvas do
          begin
            Brush.Style := bsSolid;
            // Brush.Color := bgCorrelationItemColor;
            Brush.Color := bgRequestTagNameColumnColor;
            FillRect(CellRect);
          end;
        end;
      end;
    end;
  except
  end;
end;

procedure TMainForm.TreeViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xBind: TCustomBindable;
begin
  FocusedOperation.AcquireLock;
  try
    xBind := NodeToBind(Sender, Node);
    stubChanged := True;
    xBind.Checked := (Node.CheckState = csCheckedNormal);
    if (not xBind.Checked) then
    begin
      if xmlUtil.doCollapseOnUncheck then
        TreeView.FullCollapse(Node)
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
        TreeView.FullExpand(Node);
    end;
    RevalidateXmlTreeView(Sender as TVirtualStringTree);
  finally
    FocusedOperation.ReleaseLock;
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
      Ord (messagesColumnBeforeScript): EditMessageScriptActionExecute(nil);
      Ord (messagesColumnAfterScript): EditMessageAfterScriptActionExecute(nil);
      Ord (messagesColumnDocumentation): EditMessageDocumentationActionExecute(nil);
    end;
  end;
  if (    (Sender = GridView)
      and (inImageArea)
     )
  or (    (Sender = TreeView)
      and (TreeView.FocusedColumn = treeButtonColumn)
     )
  then
  begin
    xBind := NodeToBind(TreeView, TreeView.FocusedNode);
    if xmlUtil.isExtendAdviced(xBind) then
      ExtendRecursivityMenuItemClick(nil)
    else
    begin
      xChanged := xmlUtil.editXml(xBind, Sender = TreeView, False);
      if xChanged then
      begin
        UpdateXmlTreeViewNode(TreeView, TreeView.FocusedNode);
        RevalidateXmlTreeView(TreeView);
      end;
      stubChanged := stubChanged or xChanged;
      TreeView.FocusedColumn := treeValueColumn;
    end;
  end
  else
  begin
    if (Assigned((Sender as TVirtualStringTree).FocusedNode))
    and (   ((Sender as TVirtualStringTree).FocusedColumn = treeValueColumn)
         or (Sender = GridView)
        ) then
      (Sender as TVirtualStringTree).EditNode ( (Sender as TVirtualStringTree).FocusedNode
                                              , (Sender as TVirtualStringTree).FocusedColumn
                                              );
  end;
end;

procedure TMainForm.TreeViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = treeValueColumn then
  begin
    try xmlUtil.CheckValidity(NodeToBind(Sender, Node)); Except End;
  end;
end;

procedure TMainForm.TreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  case Column of
    treeTagColumn, treeButtonColumn:
      begin
        Allowed := False;
      end;
    treeValueColumn:
      begin
        Allowed := FocusedBind.IsEditingAllowed;
      end;
  end;
end;

procedure TMainForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
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

procedure TMainForm.TreeViewGetText(Sender: TBaseVirtualTree;
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
  xMessage := ''; // avoid warning
  try
  {
    if (Key = VK_F8) then
    begin
      xBind := NodeToBind(Sender as TVirtualStringTree,
        (Sender as TVirtualStringTree).FocusedNode);
      if (xBind is TXml) then
        ShowInfoForm('Schema for ' + (xBind as TXml).TypeDef.Name,
          (xBind as TXml).TypeDef.SchemaAsText((xBind as TXml).TypeDef.Name));
    end;
  }
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

procedure TMainForm.TreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  xBind: TCustomBindable;
begin
  FocusedOperation.AcquireLock;
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
    FocusedOperation.ReleaseLock;
  end;
end;

procedure TMainForm.TreeViewPaintText(Sender: TBaseVirtualTree;
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
  with xBind as TXml do
  begin
    if (not Assigned(Xsd)) then
    begin
      TargetCanvas.Font.Color := clRed { clLtGray } ;
      exit;
    end;
    try
      if IsRequired then
      begin
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        if AllChecked(Sender, Node.Parent) then
          if (not Checked)
          and (not isOneOfGroupOk) then
            TargetCanvas.Font.Color := clRed { clLtGray } ;
      end;
    except
      ShowMessage(Xsd.minOccurs);
    end;
  end;
  if xBind is TIpmItem then
  begin
    //
  end;
end;

procedure TMainForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  f: Integer;
  Xml: TXml;
  swapEvent: TVTFocusChangeEvent;
begin
  Sender.Selected[Sender.FocusedNode] := True;
  FocusedBind := NodeToBind(Sender, Sender.FocusedNode);
end;

procedure TMainForm.WsdlItemDelMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    if (LowerCase((xBind as TXml).Xsd.maxOccurs) = 'unbounded') or
      (StrToInt((xBind as TXml).Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isDeleteAllowed(xBind, True) then
        exit;
      TreeView.BeginUpdate;
      try
        xmlUtil.Delete((xBind as TXml));
        TreeView.DeleteNode(TreeView.FocusedNode, True);
        FocusedOperation.AcquireLock;
        try
          se.UpdateMessageRow(FocusedOperation, FocusedMessage);
        finally
          FocusedOperation.ReleaseLock;
        end;
        TreeView.Invalidate;
        GridView.InvalidateNode(GridView.FocusedNode);
        stubChanged := True;
      finally
        TreeView.EndUpdate;
      end;
    end; { if maxOccurs greater than 1 }
  end; { if xml clicked; just to be sure }
end;

procedure TMainForm.ExtendRecursivityMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if (xBind is TXml) then
    with xBind as TXml do
    begin
      ExtendRecursivity;
      UpdateXmlTreeViewNode(TreeView, TreeView.FocusedNode);
      TreeView.FocusedColumn := 0;
      TreeView.FullExpand(TreeView.FocusedNode);
      se.UpdateMessageRow(FocusedOperation, FocusedMessage);
      TreeView.Invalidate;
      GridView.InvalidateNode(GridView.FocusedNode);
      TreeViewFocusChanged(TreeView, TreeView.FocusedNode,
        TreeView.FocusedColumn);
    end;
end;

procedure TMainForm.XmlAddMenuItemClick(Sender: TObject);
var
  Xml: TXml; { current }
  xXml: TXml; { new created }
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    Xml := xBind as TXml;
    if (LowerCase(Xml.Xsd.maxOccurs) = 'unbounded') or
      (StrToInt(Xml.Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isAddAllowed(NodeToBind(TreeView,
          TreeView.FocusedNode), True) then
        exit;
      xXml := AddSibbling(Xml);
      TreeView.FocusedNode := InsertXmlNode(TreeView.FocusedNode,
        xXml);
      TreeView.FocusedColumn := 0;
      TreeView.Expanded[TreeView.FocusedNode] := True;
      FocusedOperation.AcquireLock;
      try
        se.UpdateMessageRow(FocusedOperation, FocusedMessage);
      finally
        FocusedOperation.ReleaseLock;
      end;
      TreeView.Invalidate;
      GridView.InvalidateNode(GridView.FocusedNode);
      stubChanged := True;
    end; { if maxOccurs greater than 1 }
  end; { if xml clicked; just to be sure }
end;

function TMainForm.InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
begin
  result := TreeView.InsertNode(aNode, amInsertAfter);
  FinishXmlNode(result, aXml);
end;

procedure TMainForm.ConfirmTemporarelyInactivity(var aPrompt: Boolean);
begin
  aPrompt := (not se.IsActive)
          or (not doConfirmTemporaryInactivity)
          or BooleanPromptDialog(Format ('%s will temporarely become inactive, OK to continue?', [_ProgName]))
          ;
end;

procedure TMainForm.DoUpdateConsole;
begin
  DisableViewOnFocusChangeEvents;
  try
    captionFileName := ExtractFileName(se.projectFileName);
    ClearConsole;
    PrepareOperation;
    CreateEnvironmentSubMenuItems;
    CreateScriptsSubMenuItems;
    LogUpdateColumns;
    CheckBoxClick(nil);
    FocusedOperation := se.LastFocusedOperation;
  finally
    EnableViewOnFocusChangeEvents;
  end;
end;

procedure TMainForm.BeginConsoleUpdate;
begin
  se.LastFocusedOperation := FocusedOperation;
  FocusedOperation := nil;
  ClearConsole;
end;

function TMainForm.BooleanPromptDialog(aPrompt: String): Boolean;
begin
  result := (MessageDlg(aPrompt, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

function TMainForm.BooleanStringDialog(aPrompt, aDefault: String; var aValue: String): Boolean;
begin
  result := False;
  aValue := aDefault;
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := aPrompt;
    PromptForm.Value := aDefault;
    PromptForm.ShowModal;
    result := (PromptForm.ModalResult = mrOk);
    if Result then
      aValue := PromptForm.Value;
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm.FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
var
  attrNode: PVirtualNode;
  Data: PXmlTreeRec;
  I: Integer;
begin
  Data := TreeView.GetNodeData(aNode);
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
    attrNode := TreeView.AddChild(aNode);
    attrNode.CheckType := ctCheckBox;
    if aXml.Attributes.XmlAttributes[I].Checked then
      attrNode.CheckState := csCheckedNormal
    else
      attrNode.CheckState := csUnCheckedNormal;
    Data := TreeView.GetNodeData(attrNode);
    Data.Bind := aXml.Attributes.XmlAttributes[I];
  end;
  for I := 0 to aXml.Items.Count - 1 do
    FinishXmlNode(TreeView.AddChild(aNode), aXml.Items.XmlItems[I]);
end;

procedure TMainForm.Copytoclipboard1Click(Sender: TObject);
begin
  xmlUtil.CopyToClipboard(tlsXml, NodeToBind(TreeView,
      TreeView.FocusedNode));
end;

procedure TMainForm.EndEdit;
begin
  GridView.EndEditNode;
  TreeView.EndEditNode;
end;

procedure TMainForm.PopulateXml(aViewType: TxvViewType);
begin
  xmlUtil.Populate(NodeToBind(TreeView, TreeView.FocusedNode),
    aViewType);
  stubChanged := True;
  RevalidateXmlTreeView(TreeView);
end;

procedure TMainForm.WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
begin
  try
    xmlUtil.PasteFromClipboard(NodeToBind(TreeView,
        TreeView.FocusedNode));
    UpdateXmlTreeViewNode(TreeView, TreeView.FocusedNode);
    stubChanged := True;
  finally
    RevalidateXmlTreeView(TreeView);
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
  if Assigned(TreeView.FocusedNode) then
    TreeView.FullExpand(TreeView.FocusedNode);
end;

procedure TMainForm.FullCollapse1Click(Sender: TObject);
begin
  if Assigned(TreeView.FocusedNode) then
    TreeView.FullCollapse(TreeView.FocusedNode);
end;

procedure TMainForm.XmlZoomValueAsTextMenuItemClick(Sender: TObject);
var
  editAllowed: Boolean;
begin
  editAllowed := FocusedBind.IsEditingAllowed;
  xmlUtil.ZoomAsText(NodeToBind(TreeView, TreeView.FocusedNode),
    not editAllowed);
  if editAllowed then
  begin
    TreeViewNewText(TreeView, TreeView.FocusedNode,
      treeValueColumn, xmlUtil.NewValue);
  end;
end;

procedure TMainForm.XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
begin
  xmlUtil.ZoomAsXml(NodeToBind(TreeView, TreeView.FocusedNode),
    True);
end;

procedure TMainForm.XSDreportinClipBoardSpreadSheet1Click(Sender: TObject);
var
  xXml: TXml;
  xStrings: TJBStringList;
begin
  if not (NodeToBind(TreeView, TreeView.FocusedNode) is TXml) then
    raise Exception.Create('Only implemented for Xml');
  xXml := (NodeToBind(TreeView, TreeView.FocusedNode) as TXml);
  if not Assigned (xXml.Xsd)  then
    raise Exception.Create('Only possible for XSD based Xml''s');

  xStrings := TJBStringList.Create;
  try
    xXml.Xsd.GenerateReport(xStrings);
    Clipboard.AsText := xStrings.Text;
  finally
    xStrings.Free;
  end;
end;

procedure TMainForm.UpdateMessagesView;
begin
  if Assigned (FocusedOperation) then
  begin
    if doTrackDuplicateMessages then
      FocusedOperation.Messages.SetDuplicates
    else
      FocusedOperation.Messages.ResetDuplicates;
  end;
  FocusedOperation.Messages.SetNameDuplicates;
  GridView.Invalidate;
end;

procedure TMainForm.GridViewUnselect;
var
  xNode: PVirtualNode;
begin
  xNode := GridView.GetFirstSelected;
  while Assigned (xNode) do
  begin
    GridView.Selected[xNode] := False;
    xNode := GridView.GetNextSelected(xNode);
  end;
end;

procedure TMainForm.ShowHelpDocumentation(aName: String);
var
  xFileName: String;
begin
  xFileName := SetDirSeparators ( ExtractFilePath(ParamStr(0))
                                + 'Documentation'
                                + DirectorySeparator
                                + _progName
                                + '_'
                                + aName
                                + '_hlp.htm'
                                );
  if not LazFileUtils.FileExistsUTF8(xFileName) then
    raise Exception.Create('Could not find helpfile: ' + xFileName);
  if not OpenDocument(xFileName) then
    raise Exception.Create('Could not open ' + xFileName);
end;

procedure TMainForm.EditContexts;
begin
  ContextsActionExecute(nil);
end;

function TMainForm.ShowProgressForm: Boolean;
var
  swabEnabled: Boolean;
begin
  result := False;
  swabEnabled := RefreshLogTimer.Enabled;
  RefreshLogTimer.Enabled := False;
  try
    Application.CreateForm(TProgressForm, ProgressForm);
    try
      ProgressForm.AcquireLock := se.AcquireLogLock;
      ProgressForm.ReleaseLock := se.ReleaseLogLock;
      ProgressForm.ProgressInterface := se.ProgressInterface;
      ProgressForm.ShowModal;
      if se.ProgressInterface.doUpdateConsole then
        DoUpdateConsole
      else
        UpdateCaption;
    finally
      ProgressForm.Free;
    end;
    result := not se.ProgressInterface.ExceptionRaised;
    if se.ProgressInterface.ExceptionRaised then with se.ProgressInterface do
    begin
      if MessageDlg( ExceptionMessage + LineEnding +  LineEnding + 'Show extra information?'
                   , mtError
                   , [mbNo, mbYes]
                   , 0
                   ) = mrYes then
        ShowText ( 'Exception details'
                 , ExceptionMessage
                 + LineEnding
                 + LineEnding
                 + ExceptionStackTrace
                 );
    end;
  finally
    RefreshLogTimer.Enabled:=swabEnabled;
  end;
end;

procedure TMainForm.PositionMessagesTabControl;
var
  L: Integer;
begin
  L := MessagesTabToolBar.Width - MessagesTabControlWidth;
  if L < MessagesTabControlMinLeft then
    L := MessagesTabControlMinLeft;
  MessagesTabControl.Left := L;
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
  if not (NodeToBind(TreeView, TreeView.FocusedNode) is TXml) then
    raise Exception.Create('Only implemented for Xml');
  ShowInfoForm( 'ScriptAssignments'
              , (NodeToBind(TreeView, TreeView.FocusedNode) as TXml).asAssignments
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
  SetOperationZoomPath(FocusedOperation);
end;

procedure TMainForm.OperationZoomOnActionUpdate(Sender: TObject);
begin
  OperationZoomOnAction.Enabled := (Assigned(FocusedOperation)) and
    (Assigned(FocusedOperation.reqBind) or Assigned(FocusedOperation.rpyBind));
end;

procedure TMainForm.Expand2Click(Sender: TObject);
begin
  if Assigned(TreeView.FocusedNode) then
  begin
    TreeView.FullCollapse(nil);
    TreeView.Expanded[TreeView.FocusedNode] := True;
  end;
end;

procedure TMainForm.AfterRequestScriptButtonClick(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(FocusedOperation) then
    Raise Exception.Create('First get a Wsdl');
  xOperation := TWsdlOperation.Create(FocusedOperation);
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
        FocusedOperation.AfterScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
        try FocusedOperation.PrepareBefore; Except end;
        try FocusedOperation.PrepareAfter; Except end;
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
  if not Assigned(FocusedOperation) then
    Raise Exception.Create('First get a Wsdl');
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(FocusedOperation);
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
          FocusedOperation.BeforeScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try FocusedOperation.PrepareBefore; Except end;
          try FocusedOperation.PrepareAfter; Except end;
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
  xNode := TreeView.GetFirst;
  while Assigned(xNode) do
  begin
    xData := TreeView.GetNodeData(xNode);
    if Assigned(xData.Bind) then
    begin
      if xData.Bind.Checked then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    xNode := TreeView.GetNext(xNode);
  end;
end;

procedure TMainForm.ExportProjectActionExecute(Sender: TObject);
begin
  EndEdit;
  SaveFileDialog.DefaultExt := 'wsdlStub';
  SaveFileDialog.Filter := 'wsdlStub Case (*.wsdlStub)|*.wsdlStub';
  SaveFileDialog.Title := 'Export project';
  if SaveFileDialog.Execute then
  begin
    se.projectFileName := SaveFileDialog.FileName;
    SaveStubCase;
  end;
end;

function TMainForm.SaveStubCase: Boolean;
begin
  result := False;
  captionFileName := ExtractFileName(se.projectFileName);
  if ExtractFileExt(se.projectFileName) = _ProjectOldFileExtention then
  begin
    TProcedureThread.Create(False, False, se, se.ExportToFile);
    result := ShowProgressForm;
  end
  else
  begin
    if ExtractFileExt(se.projectFileName) = _ProjectFileExtention then
    begin
      CheckFolderAndFileNames;
      TProcedureThread.Create(False, False, se, se.SaveWithFolders);
      UpdateReopenList(ReopenCaseList, se.projectFileName);
      result := ShowProgressForm;
    end
    else
    begin
      raise Exception.Create('unsupported fileextention or not a folder;' + se.projectFileName);
    end;
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

procedure TMainForm.ImportProjectActionExecute(Sender: TObject);
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
      OpenStubCase;
    end;
  end;
end;

procedure TMainForm.OpenStubCase;
var
  f: Integer;
begin
  FocusedOperation := nil;
  se.doStartOnOpeningProject := doStartOnOpeningProject;
  if (ExtractFileExt(se.projectFileName) <> _ProjectFileExtention)
  and (ExtractFileExt(se.projectFileName) <> _ProjectOldFileExtention) then
     raise Exception.Create('Unsupported filename: ' + se.projectFileName);
  BeginConsoleUpdate;
  captionFileName := ExtractFileName(se.projectFileName);
  if ExtractFileExt(se.projectFileName) = _ProjectFileExtention then
  begin
    TProcedureThread.Create(False, False, se, se.OpenFromFolders);
    UpdateReopenList(ReopenCaseList, se.projectFileName);
  end;
  if ExtractFileExt(se.projectFileName) = _ProjectOldFileExtention then
    TProcedureThread.Create(False, False, se, se.ImportFromFile);
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
        Sleep(1000); // allow threads some time
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
        Sleep(2000); // allow threads some time
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
        Sleep(1000); // allow threads some time
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
      if not Assigned(FocusedOperation) then
        raise Exception.Create(
          'Reload refused because instance has no project loaded');
      if not se.IsActive then
        raise Exception.Create
          ('Reload refused because instance of ' + _progName +
            ' is inactive');
      se.Activate(False);
      OpenStubCase;
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
    NvgtViewFocusChanged(NvgtView,
      NvgtView.FocusedNode, NvgtView.FocusedColumn);
  end;
end;

procedure TMainForm.ProjectDesignFromString(aString, aMainFileName: String);
begin
  { }
  TreeView.BeginUpdate;
  GridView.BeginUpdate;
  NvgtView.BeginUpdate;
  { }
  try
    GridView.Clear;
    ClearConsole;
    XmlUtil.PushCursor (crHourGlass);
    try
      se.ProjectDesignFromString(aString, aMainFileName);
      AcquireLock;
      try
        PrepareOperation;
        CreateEnvironmentSubMenuItems;
        CreateScriptsSubMenuItems;
        LogUpdateColumns;
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
    FocusedOperation := se.LastFocusedOperation;
    NvgtView.EndUpdate;
    GridView.EndUpdate;
    TreeView.EndUpdate;
    CheckBoxClick(nil);
  end;
end;

procedure TMainForm.ReopenStubCaseActionExecute(Sender: TObject);
var
  X: Integer;
  ChoosenString: String;
  FileName: String;
begin
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
      ChooseStringForm.ConfirmPromptCallBack := ConfirmTemporarelyInactivity;
      ChooseStringForm.AllowRemovingEntries := True;
      ChooseStringForm.ShowModal;
      if ChooseStringForm.ModalResult = mrOk then
      begin
        ChoosenString := ChooseStringForm.ChoosenString;
        FileName := ExpandRelativeFileName ( LazFileUtils.GetCurrentDirUTF8 + DirectorySeparator
                                           , Copy(ChoosenString, 3, Length(ChoosenString) - 2)
                                           );
        ReopenCaseList.Clear;
        for X := 0 to ChooseStringForm.ListBox.Count - 1 do
        begin
          ReopenCaseList.Add (Copy (ChooseStringForm.ListBox.Items.Strings[x], 3, MaxInt));
        end;
        if not LazFileUtils.DirectoryExistsUTF8(FileName) then
          raise Exception.Create ( FileName
                                 + ' is not a folder or does not exist'
                                 );
        se.projectFileName := FileName;
        OpenStubCase;
      end;
    finally
      FreeAndNil(ChooseStringForm);
    end;
  end;
end;

procedure TMainForm.UpdateReopenList(aList: TJBStringList; aFileName: String);
var
  x: Integer;
begin
  for x := 0 to aList.Count - 1 do
  begin
    if aList.Strings[x] = aFileName then
    begin
      aList.Delete(x);
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
  Node := NvgtView.GetFirst;
  while Assigned(Node) do
  begin
    xOperation := NodeToOperation(NvgtView, Node);
    NvgtView.IsVisible[Node] := not xOperation.HiddenFromUI;
    if xOperation.HiddenFromUI then
    begin
      xMenuItem := TMenuItem.Create(nil);
      xMenuItem.Tag := PtrInt(xOperation);
      xMenuItem.Caption := xOperation.Alias;
      xMenuItem.OnClick := UnhideOperationMenuItemClick;
      UnhideOperationMenuItem.Add(xMenuItem);
    end;
    Node := NvgtView.GetNext(Node);
  end;
  UnhideOperationMenuItem.Enabled := (UnhideOperationMenuItem.Count > 0);
  UnhideAllOperationsAction.Enabled := UnhideOperationMenuItem.Enabled;
end;

procedure TMainForm.UpdateVisibiltyTreeView(aFreeFormat: Boolean);
begin
  if aFreeFormat then
  begin
    TreeView.Align := alLeft;
    TreeView.Visible := False;
    FreeFormatMemo.Align := alClient;
    FreeFormatMemo.Visible := True;
  end
  else
  begin
    FreeFormatMemo.Align := alRight;
    FreeFormatMemo.Visible := False;
    TreeView.Align := alClient;
    TreeView.Visible := True;
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
    TreeView.Invalidate;
  end;
end;

procedure TMainForm.StartNonBlockingThreadEvent ;
begin
  if NumberOfNonBlockingThreads = 0 then
  begin
    ShowKindOfInformation := spMessages;
    abortPressed := False;
  end;
  Inc (NumberOfNonBlockingThreads);
end;

procedure TMainForm.TerminateNonBlockingThreadEvent ;
begin
  Dec (NumberOfNonBlockingThreads);
  if (NumberOfNonBlockingThreads <= 0) then
  begin
    ShowKindOfInformation := spMessages;
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
begin
  OpenURL(apiuiconsts.apiuiGui);
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
  if False
  and (not se.Licensed)
  then
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
  se.Licensed := True;
  WindowsUserName := getUserName;
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

procedure TMainForm.OpenProjectCommand(aProject: String); // on remote control
begin
  AcquireLock;
  try
    if stubChanged then
      raise Exception.Create
        ('Reload refused because instance of ' + _progName +
          ' has unsaved changes in design');
    se.Activate(False);
    se.projectFileName := aProject;
    se.isBusy := True;
    OpenStubCase;
    while True do
    begin
      Sleep (500);
      if not se.isBusy then
        Exit;
    end;
  finally
    ReleaseLock;
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
  WsdlOperationNameEdit.Clear;
  WsdlServiceNameEdit.Clear;
  FillNvgtView(allAliasses);
  if se.scriptErrorCount > 0 then
    ShowMessage(IntToStr(se.scriptErrorCount) +
        ' Script(s) found with errors, see Exceptions log');
end;

function TMainForm.getWsdl: TWsdl;
begin
  result := FocusedOperation.Wsdl;
end;

procedure TMainForm.ClearConsole;
var
  xEnabledFocusEvents: Boolean;
begin
  xEnabledFocusEvents := Assigned (NvgtView.OnFocusChanged);
  DisableViewOnFocusChangeEvents;
  try
    RemoveMessageColumns;
    SnapshotsVTS.Clear;
    SnapshotsVTS.Header.SortColumn := -1;
    SnapshotsVTS.Header.SortDirection := sdAscending;
    MessagesVTS.Clear;
    MessagesVTS.Header.SortColumn := -1;
    MessagesVTS.Header.SortDirection := sdAscending;
    LogMemo.Clear;
    GridView.Clear;
    NvgtView.Clear;
    InWsdlPropertiesListView.Clear;
    OperationDocumentationViewer.LoadFromString('');
    TreeView.Clear;
    WsdlServiceNameEdit.Text := '';
    WsdlOperationNameEdit.Text := '';
    WsdlNameEdit.Text := '';
    StatusPanel.Caption := '';
    while MessagesVTS.Header.Columns.Count > Ord(logStdColumnCount) do
      MessagesVTS.Header.Columns.Delete(MessagesVTS.Header.Columns.Count - 1);
  finally
    if xEnabledFocusEvents then
      EnableViewOnFocusChangeEvents;
  end;
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
    setTreeviewColors(TreeView);
  except
  end;
  try
    setTreeviewColors(GridView);
  except
  end;
  ToggleDoScrollMessagesIntoViewAction.Checked := doScrollMessagesIntoView;
  ActionComboBox.Enabled := Assigned(FocusedOperation);
  WsdlItemAddMenuItem.Enabled := True;
  WsdlPasteFromClipboardMenuItem.Enabled := True;
  WsdlPopulateMenuItem.Enabled := True;
  FreeFormatMemo.ReadOnly := se.IsActive and False;
  if se.IsActive then
  begin
    ShowKindOfInformation := spMessages;
    ShowKindOfLogData := slRequestBody;
    if se.IsActive then
    begin
      Application.Title := '' + _progName + ' (Active)';
      startAction.ShortCut := 0;
      stopAction.ShortCut := startStopShortCut;
      startStopButton.Action := stopAction;
    end;
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
//OptionsAction.Enabled := (not se.IsActive);
  OptionsAction.Enabled := True;
end;

function TMainForm.OptionsAsXml: TXml;
begin
  result := TXml.CreateAsString('Options', '');
  with result.AddXml(TXml.CreateAsString('General', '')) do
  begin
    AddXml(TXml.CreateAsBoolean('StartAfterOpeningProject', doStartOnOpeningProject));
    AddXml(TXml.CreateAsBoolean('ConfirmRemovals', xmlUtil.doConfirmRemovals));
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
    with result.AddXml(TXml.CreateAsString('Colors', '')) do
    begin
      with AddXml(TXml.CreateAsString('Xml', '')) do
      begin
        AddXml(TXml.CreateAsString('UnassignedValues',
            ColorToHtml(bgNilValueColor)));
        AddXml(TXml.CreateAsString('CorrelationValues',
            ColorToHtml(bgCorrelationItemColor)));
        AddXml(TXml.CreateAsString('RequestTagNameColumn',
            ColorToHtml(bgRequestTagNameColumnColor)));
      end;
    end;
  end;
end;

procedure TMainForm.OptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  xXml := OptionsAsXml;
  try
    if EditXmlXsdBased('Options', '', '', '', False, False, esUsed, optionsXsd, xXml, True, ConfirmTemporarelyInactivity) then
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
var
  xStubAction: TStubAction;
begin
  xStubAction := saStub;
  if ActionComboBox.ItemIndex <> 0 then
    xStubAction := saRequest;
  if Assigned(FocusedOperation) then
  begin
    if (FocusedOperation.StubAction <> xStubAction) then
    begin
      FocusedOperation.StubAction := xStubAction;
      NvgtView.OnFocusChanged ( NvgtView
                                       , NvgtView.FocusedNode
                                       , 0
                                       );
    end;
    FocusedOperation.StubAction := xStubAction;
    stubChanged := True;
    NvgtView.Invalidate;
    OperationDelayResponseTimeAction.Visible := (FocusedOperation.StubAction <> saRequest);
    if (FocusedOperation.DelayTimeMsMin = 0)
    and (FocusedOperation.DelayTimeMsMax = 0)
    then
      OperationDelayResponseTimeAction.ImageIndex := 60
    else
      OperationDelayResponseTimeAction.ImageIndex := 61;
    RedirectAddressAction.Visible := (FocusedOperation.StubAction = saRequest);
    if FocusedOperation.StubAction = saStub then
      EditMessageScriptAction.Caption := 'Edit Message Script'
    else
      EditMessageScriptAction.Caption := 'Edit Message Before Script';
    EditBetweenScriptMenuItem.Visible := (FocusedOperation.StubAction = saStub);
    EditBeforeScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
    EditAfterScriptMenuItem.Visible := not EditBetweenScriptMenuItem.Visible;
    ShowFocusedMessageInTreeView;
    TreeView.FocusedNode := TreeView.GetFirst;
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
  if Column = nMessageButtonColumns then
    Exit;
  if not Assigned(FocusedOperation) then
    exit;
  xMessage := NodeToMessage(Sender, Node);
  if not Assigned(xMessage) then
    exit;
  try
    case Kind of
      ikNormal, ikSelected:
        begin
          if Column < nMessageButtonColumns then
          begin
            case Column of
              Ord (messagesColumnBeforeScript):
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
                Ord (messagesColumnAfterScript):
                  begin
                    if FocusedOperation.StubAction <> saStub then
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
                Ord (messagesColumnDocumentation):
                  begin
                    if (xMessage.Documentation <> '') then
                    begin
                      if xMessage.DocumentationEdited then
                        ImageIndex := 33
                      else
                        ImageIndex := 34;
                    end
                    else
                      ImageIndex := 32;
                  end;
              end;
            exit;
          end;
          if Column < (nMessageButtonColumns + xMessage.CorrelationBindables.Count + 1) then
            Exit;
          xBind := xMessage.ColumnXmls.Bindables
            [Column - nMessageButtonColumns - xMessage.CorrelationBindables.Count - 1];
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
  CellText := '';
  xMessage := NodeToMessage(Sender, Node);
  if not Assigned(xMessage) then Exit;
  if Column < nMessageButtonColumns then Exit;
  try
    if Column = nMessageButtonColumns then
      CellText := xMessage.Name
    else
    begin
      if (Column - nMessageButtonColumns) <= xMessage.CorrelationBindables.Count then
        try
          if Assigned (xMessage.CorrelationBindables.Bindables[Column - nMessageButtonColumns - 1]) then
            CellText := xMessage.CorrelationBindables.Bindables[Column - nMessageButtonColumns - 1].CorrelationValue
          else
            CellText := '?';
        except
        end
      else
      begin
        xBind := xMessage.ColumnXmls.Bindables
          [Column - nMessageButtonColumns - xMessage.CorrelationBindables.Count - 1];
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
      xOrgMessage := FocusedOperation.Messages.Messages[0];
      if (FocusedOperation.StubAction = saRequest) then
        xMessage := TWsdlMessage.CreateRequest(FocusedOperation,
          'Request' + IntToStr(FocusedOperation.Messages.Count),
          'Pattern' + IntToStr(FocusedOperation.Messages.Count),
          xOrgMessage.Documentation)
      else
        xMessage := TWsdlMessage.CreateReply(FocusedOperation,
          'Reply' + IntToStr(FocusedOperation.Messages.Count),
          'Pattern' + IntToStr(FocusedOperation.Messages.Count),
          xOrgMessage.Documentation);
      if FocusedOperation.reqBind is TIpmItem then
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
        se.UpdateMessageRow(FocusedOperation, xMessage);
        if Assigned(FocusedOperation.FaultMessages) then
        begin
          xMessage.fltBind.Name := xOrgMessage.fltBind.Name;
(xMessage.fltBind as TXml)
          .Xsd := FocusedOperation.FaultXsd; (xMessage.fltBind as TXml)
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
      xMessage := NodeToMessage(Sender, Node);
    end;
  end
  else
  begin
    xMessage := NodeToMessage(Sender, Node);
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
  FocusedOperation.AcquireLock;
  try
    xMessage := NodeToMessage(Sender, Node);
    if Column = nMessageButtonColumns then
    begin
      if not xmlio.isFileNameAllowed(NewText) then
        _RaiseError(Format ('"%s" not allowed as filename', [NewText]));
      if NewText <> xMessage.Name then
      begin
        if Node = Sender.GetFirst then
          _RaiseError('Not allowed to change this reply name into ' + NewText)
        else
        begin
          xMessage.Name := NewText;
          FocusedOperation.Messages.SetNameDuplicates;
          GridView.InvalidateColumn(Column);
          stubChanged := True;
        end;
      end;
    end
    else
    begin
      if (Column - nMessageButtonColumns) <= xMessage.CorrelationBindables.Count then
      begin
        xNode := GridView.GetFirstSelected;
        while Assigned(xNode) do
        begin
          xMessage := NodeToMessage(Sender, xNode);
          if NewText <> xMessage.CorrelationBindables.Bindables[Column - nMessageButtonColumns - 1]
            .CorrelationValue then
          begin
            if xNode = Sender.GetFirst then
            begin
              Node := xNode;
              _RaiseError('Not allowed to change this pattern into ' + NewText);
            end;
            xMessage.CorrelationBindables.Bindables[Column - nMessageButtonColumns - 1].CorrelationValue := NewText;
            stubChanged := True;
          end;
          xNode := GridView.GetNextSelected(xNode);
        end;
      end
      else
      begin
        { }{
          if (Assigned (xMessage.ColumnXmls.Bindables[Column - xMessage.CorrelationBindables.Count - 1])) then
          TreeView.OnNewText ( TreeView
          , editingNode
          , treeValueColumn
          , NewText
          );
          { }
        xNode := GridView.GetFirstSelected;
        while Assigned(xNode) do
        begin
          xMessage := NodeToMessage(Sender, xNode);
          xBind := xMessage.ColumnXmls.Bindables
            [Column - nMessageButtonColumns - xMessage.CorrelationBindables.Count - 1];
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
        RevalidateXmlTreeView(TreeView);
      end;
    end;
  finally
    FocusedOperation.ReleaseLock;
  end;
end;

procedure TMainForm.GridViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  editingNode := TreeView.FocusedNode;
  if Column < nMessageButtonColumns then
  begin
    Allowed := False;
    Exit;
  end;
  if Column = nMessageButtonColumns then
  begin
    Allowed := (Node <> Sender.GetFirst) and (GridView.SelectedCount = 1);
    exit;
  end;
  if (Column - nMessageButtonColumns) <= FocusedMessage.CorrelationBindables.Count then
  begin
    Allowed := (Node <> Sender.GetFirst)
           and (Assigned (FocusedMessage.CorrelationBindables.Bindables[Column- nMessageButtonColumns - 1]))
             ;
    exit;
  end;
  Allowed := FocusedBind.IsEditingAllowed;
end;

procedure TMainForm.SelectCorrelationElementActionUpdate(Sender: TObject);
begin
  SelectCorrelationElementAction.Enabled := Assigned(FocusedOperation);
  // and (FocusedOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
  // and (FocusedOperation.StubAction <> saRequest)
                                          ;
end;

procedure TMainForm.SelectCorrelationElementActionExecute(Sender: TObject);
var
  swapBindable: TCustomBindable;
begin
  with FocusedOperation do
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
    SelectElementsForm.WsdlOperation := FocusedOperation;
    SelectElementsForm.ControlBinds := FocusedOperation.CorrelationBindables.Clone;
    SelectElementsForm.ShowModal;
    if SelectElementsForm.ModalResult = mrOK then
    begin
      FocusedOperation.AcquireLock;
      try
        FocusedOperation.CorrelationBindables.ClearListOnly;
        FocusedOperation.CorrelationBindables.Free;
        FocusedOperation.CorrelationBindables := SelectElementsForm.ControlBinds;
        se.UpdateReplyColumns(FocusedOperation);
        UpdateMessagesGrid;
        UpdateLogCorrelationIds (FocusedOperation);
        stubChanged := True;
      finally
        FocusedOperation.ReleaseLock;
      end;
    end
    else
    begin
      with SelectElementsForm.ControlBinds do
      begin
        ClearListOnly;
        Free;
      end;
    end;
  finally
    GridView.EndUpdate;
    FreeAndNil(SelectElementsForm);
    DoColorBindButtons;
  end;
end;

procedure TMainForm.AddMessageActionUpdate(Sender: TObject);
begin
  AddMessageAction.Enabled := Assigned(FocusedOperation)
                          and Assigned (GridView.FocusedNode)
                            ;
end;

procedure TMainForm.AddMessageActionExecute(Sender: TObject);
var
  xMessage: TWsdlMessage;
  cNode, nNode, sNode: PVirtualNode;
  n: Integer;
begin
  FocusedOperation.AcquireLock;
  try
    nNode := nil;
    sNode := nil;
    cNode := GridView.GetFirst;
    n := FocusedOperation.Messages.Count;
    while Assigned(cNode) and (n > 0) do
    begin
      if GridView.Selected[cNode] then
      begin
        GridView.Selected[cNode] := False;
        xMessage := NodeToMessage(GridView, cNode);
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
    UpdateMessagesView;
    GridView.SetFocus;
    DoColorBindButtons;
    if Assigned(sNode) then
      GridView.FocusedNode := sNode;
  finally
    FocusedOperation.ReleaseLock;
  end;
end;

function TMainForm.AddMessage(aCopyNode: PVirtualNode): PVirtualNode;
var
  xOrgMessage, xNewMessage: TWsdlMessage;
  xData: PMessageTreeRec;
begin
  result := nil;
  xOrgMessage := NodeToMessage(GridView, aCopyNode);
  if (FocusedOperation.StubAction = saRequest) then
    xNewMessage := TWsdlMessage.CreateRequest(FocusedOperation,
      'Request' + IntToStr(FocusedOperation.Messages.Count),
      'Pattern' + IntToStr(FocusedOperation.Messages.Count),
      xOrgMessage.Documentation)
  else
    xNewMessage := TWsdlMessage.CreateReply(FocusedOperation,
      'Reply' + IntToStr(FocusedOperation.Messages.Count),
      'Pattern' + IntToStr(FocusedOperation.Messages.Count),
      xOrgMessage.Documentation);
  xNewMessage.DocumentationEdited := False;
  xNewMessage.BeforeScriptLines.Text := xOrgMessage.BeforeScriptLines.Text;
  xNewMessage.AfterScriptLines.Text := xOrgMessage.AfterScriptLines.Text;
  if FocusedOperation.DescriptionType = ipmDTFreeFormat then
  begin
    xNewMessage.FreeFormatReq := xOrgMessage.FreeFormatReq;
    xNewMessage.FreeFormatRpy := xOrgMessage.FreeFormatRpy;
  end;
  if FocusedOperation.reqBind is TIpmItem then
  begin
    (xNewMessage.reqBind as TIpmItem).LoadValues(xOrgMessage.reqBind as TIpmItem);
    (xNewMessage.rpyBind as TIpmItem).LoadValues(xOrgMessage.rpyBind as TIpmItem);
    (xNewMessage.fltBind as TIpmItem).LoadValues(xOrgMessage.fltBind as TIpmItem);
    se.UpdateMessageRow(FocusedOperation, xNewMessage);
  end
  else
  begin
    (xNewMessage.reqBind as TXml).LoadValues(xOrgMessage.reqBind as TXml, False, True);
    (xNewMessage.rpyBind as TXml).LoadValues(xOrgMessage.rpyBind as TXml, False, True);
    se.UpdateMessageRow(FocusedOperation, xNewMessage);
    if Assigned(FocusedOperation.FaultMessages) then
    begin
      xNewMessage.fltBind.Name := xOrgMessage.fltBind.Name;
      (xNewMessage.fltBind as TXml).Xsd := FocusedOperation.FaultXsd;
      (xNewMessage.fltBind as TXml).LoadValues(xOrgMessage.fltBind as TXml, True);
    end;
  end;
  result := GridView.AddChild(nil);
  xData := GridView.GetNodeData(result);
  xData.Message := xNewMessage;
end;

procedure TMainForm.GridViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.Selected[Sender.FocusedNode] := True;
  FocusedMessage := NodeToMessage(GridView, GridView.FocusedNode);
  if (Column - nMessageButtonColumns) > FocusedMessage.CorrelationBindables.Count then
    FocusedBind := FocusedMessage.ColumnXmls.Bindables[ Column
                                                      - nMessageButtonColumns
                                                      - FocusedMessage.CorrelationBindables.Count
                                                      - 1
                                                      ]
  else
    FocusedBind := NodeToBind(TreeView, TreeView.GetFirst);
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
  xMessage, nMessage: TWsdlMessage;
  cNode, nNode: PVirtualNode;
  m: Integer;
begin
  DisableViewOnFocusChangeEvents;
  try
    FocusedOperation.AcquireLock;
    try
      cNode := GridView.GetFirst;
      nMessage := nil;
      if Assigned(cNode) then
        cNode := GridView.GetNext(cNode); // never delete default
      while Assigned(cNode) do
      begin
        nNode := GridView.GetNext(cNode);  // because it still can
        if GridView.Selected[cNode] then
        begin
          xMessage := NodeToMessage(GridView, cNode);
          if Assigned (nNode) then
            nMessage := NodeToMessage(GridView, nNode);
          GridView.DeleteNode(cNode, True);
          if Assigned(xMessage) then
          begin
            FocusedOperation.Messages.DeleteMessage(xMessage);
            stubChanged := True;
          end;
        end;
        cNode := nNode;
      end;
      UpdateMessagesView;
      GridView.SetFocus;
      if Assigned (nMessage) then
        FocusedMessage := nMessage
      else
        FocusedMessage := NodeToMessage(GridView, GridView.GetLast);
      DoColorBindButtons;
    finally
      FocusedOperation.ReleaseLock;
    end;
  finally
    EnableViewOnFocusChangeEvents;
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
  FocusedOperation.AcquireLock;
  DisableViewOnFocusChangeEvents;
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
        Selected[pNode] := True;
        Selected[fNode] := False;
        fNode := GetNextSelected(fNode)
      end;
      GridView.FocusedNode := GetFirstSelected;
    end;
    UpdateMessagesView;
    stubChanged := True;
  finally
    EnableViewOnFocusChangeEvents;
    FocusedOperation.ReleaseLock;
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
  DisableViewOnFocusChangeEvents;
  FocusedOperation.AcquireLock;
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
          Selected[nNode] := True;
          Selected[fNode] := False;
        end;
        fNode := GetPrevious(fNode);
      end;
      GridView.FocusedNode := GetFirstSelected;
    end;
    UpdateMessagesView;
    stubChanged := True;
  finally
    FocusedOperation.ReleaseLock;
    EnableViewOnFocusChangeEvents;
  end;
end;

procedure TMainForm.DoColorBindButtons;
begin
  if ((FocusedOperation.CorrelationBindables.Count > 0) or
      (FocusedOperation.StubAction = saRequest)) then
    SelectCorrelationElementAction.ImageIndex := 8
  else
    SelectCorrelationElementAction.ImageIndex := 7;
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
  xMessage, fMessage: TWsdlMessage;
  xBind: TCustomBindable;
  expXml: TXml;
begin
  try
    xMessage := NodeToMessage(Sender, Node);
    if not Assigned(xMessage) then
      exit;
    if Column < nMessageButtonColumns then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := _decColor(self.Color);
        FillRect(CellRect);
      end;
      exit;
    end;
    if Column <= nMessageButtonColumns + xMessage.CorrelationBindables.Count then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := _decColor(bgCorrelationItemColor);
        if Column = nMessageButtonColumns then
        begin
          fMessage := NodeToMessage(Sender, Sender.FocusedNode);
          if Assigned (fMessage)
          and (fMessage <> xMessage)
          and (   (xMessage.Duplicates = fMessage)
               or (xMessage = fMessage.Duplicates)
               or (    Assigned (xMessage.Duplicates)
                   and (xMessage.Duplicates = fMessage.Duplicates)
                  )
              ) then
          begin
            Brush.Color := DecColor(Brush.Color, 22);
          end;
        end;
        FillRect(CellRect);
      end;
      exit;
    end;
    try
      xBind := xMessage.ColumnXmls.Bindables
        [Column - nMessageButtonColumns - xMessage.CorrelationBindables.Count - 1];
    except
      exit;
    end;
    if ((xBind is TXml) or (xBind is TXmlAttribute))
    and Assigned (FocusedOperation) then
    begin
      if FocusedOperation.StubAction = saRequest then
        expXml := xMessage.rpyBind as TXml
      else
        expXml := xMessage.reqBind as TXml;
      if expXml.IsAncestorOf(xBind) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := _decColor(bgRequestTagNameColumnColor);
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
  if (Column - nMessageButtonColumns) > FocusedOperation.CorrelationBindables.Count then
  begin
    TreeView.OnEdited(TreeView, editingNode, treeValueColumn);
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
        SaveStubCase
      else
        result := SelecFolderAndSave;
    end;
    if (ret = mrCancel) then
      result := False;
  end;
end;

procedure TMainForm.SaveStubCaseActionExecute(Sender: TObject);
begin
  EndEdit;
  if not se.stubRead then
    raise Exception.Create('This function should be disabled...')
  else
    SaveStubCase;
end;

procedure TMainForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
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
  result := True;    // avoids losing data entry, warning only
  with TIdUri.Create(aNewValue) do
  try
    if FocusedOperation.isOpenApiService then
    begin
      if (Path + Document <> '/') then
      begin
        ShowMessage (Format ('no path (%s) allowed on OpenApi service', [Path + Document]));
        Exit;
      end;
    end;
    Result := True;
  finally
    free;
  end;
end;

procedure TMainForm.RedirectAddressActionExecute(Sender: TObject);
var
  xXml: TXml;
  x, y: Integer;
  xEnumeration: TXsdEnumeration;
begin
  if not Assigned(FocusedOperation) then
    raise Exception.Create('No operation selected');
  with FocusedOperation do
  begin
    xXml := endpointConfigAsXml;
    try
      endpointConfigXsd.FindXsd('endpointConfig.Http.Verb').isReadOnly := (WsdlOperation.isOpenApiService);
//    endpointConfigXsd.FindXsd('endpointConfig.Http.Address').CheckNewValue := CheckHttpAddress;
      if Assigned (WsdlOperation.Wsdl.Servers) then
      begin
        for x := 0 to WsdlOperation.Wsdl.Servers.Count - 1 do
        with WsdlOperation.Wsdl.Servers do
        begin
          xEnumeration := TXsdEnumeration.Create;
          xEnumeration.Value := Strings[x];
          endpointConfigXsd.FindXsd('endpointConfig.Http.Address').sType.Enumerations.AddObject(xEnumeration.Value, xEnumeration);
        end;
      end;
      if EditXmlXsdBased('Configure Endpoint', '', '', '', False, False, esUsed,
        endpointConfigXsd, xXml, True) then
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
      with endpointConfigXsd.FindXsd('endpointConfig.Http.Address').sType.Enumerations do
      begin
        for x:= 0 to Count - 1 do
          Objects[x].Free;
        Clear;
      end;
    end;
  end;
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
  lft := _adjust(lft, wdth div 2, spc, WsdlLabel, WsdlNameEdit);
  lft := _adjust(lft, wdth div 4, spc, ServiceLabel, WsdlServiceNameEdit);
  lft := _adjust(lft, wdth - lft, spc, OperationLabel, WsdlOperationNameEdit);
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
  case ShowKindOfLogData of
    slRequestHeaders: LogMemo.Text := aLog.RequestHeaders;
    slRequestBody: LogMemo.Text := aLog.RequestBody;
    slReplyHeaders: LogMemo.Text := aLog.ReplyHeaders;
    slReplyBody: LogMemo.Text := aLog.ReplyBody;
    slException: LogMemo.Text := aLog.Exception;
    slValidation:
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
        ttStomp:
          begin
            MessagesTabControl.Tabs[Ord(slRequestHeaders)] := 'Stomp Request Headers';
            MessagesTabControl.Tabs[Ord(slRequestBody)] := 'Stomp Request Body';
            MessagesTabControl.Tabs[Ord(slReplyHeaders)] := 'Stomp Reply Headers';
            MessagesTabControl.Tabs[Ord(slReplyBody)] := 'Stomp Reply Body';
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
        FocusedMessage := nil;
        GridViewUnselect;
        xLog.Operation.LastFocusedMessage := xLog.Mssg;
        FocusedOperation := xLog.Operation;
        FocusedMessage := FocusedOperation.LastFocusedMessage;
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
        logActionColumn: CellText := xLog.StubActionAsString;
        logVerbColumn: CellText := xLog.httpCommand;
        logStatusColumn: CellText := IntToStr(xLog.httpResponseCode);
        logServiceColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.WsdlService.Name
          else
            CellText := xLog.ServiceName;
        logOperationColumn:
          if Assigned(xLog.Operation) then
            CellText := xLog.Operation.Alias
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

function TMainForm.TestDbsConnection(aXml: TObject): Boolean;
var
  xXml: TXml;
begin
  result := False;
  xXml := TXml.Create;
  try
    xXml.CopyDownLine((aXml as TXml).Parent as TXml, True);
    xXml.ResolveAliasses;
    with TSQLConnector.Create(nil) do
    try
      Connected := False;
      LoginPrompt := False;
      ConnectorType := xXml.Items.XmlValueByTag['Type'];
      DatabaseName := xXml.Items.XmlValueByTag['DatabaseName'];
      if (ConnectorType = 'SQLite3')
      and (se.projectFileName <> '')
      and (DatabaseName <> '')
      and (DatabaseName[1] = '.')
      then
        DatabaseName := ExpandRelativeFileName(se.projectFileName, DatabaseName);
      HostName := xXml.Items.XmlValueByTag['HostName'];
      UserName := xXml.Items.XmlValueByTag['UserName'];
      Password := xmlz.DecryptString(xXml.Items.XmlValueByTag['Password']);
      Params.Text := ReplaceStrings( xXml.Items.XmlValueByTag['Params']
                                   , '%pwd%'
                                   , Password
                                   , false
                                   , false
                                   );
       try
         Connected := True;
         ShowMessage ('Database ' + DatabaseName + ' connected OK');
         Connected := False;
         result := True;
       except
         on E: Exception do
         begin
           raise Exception.Create('Exception connecting database: ' + e.Message);
         end;
       end;
    finally
      Free;
    end;
  finally
    xXml.Free;
  end;
end;

function TMainForm.TestRemoteServerConnection(aXml: TObject): Boolean;
begin
  result := False;
  with TXml.Create do
  try
    try
      CopyDownLine((aXml as TXml).Parent as TXml, True);
      ResolveAliasses;
      xmlio.apiUiServerDialog(thisXml, '/apiUi/api/testconnection', '', 'GET', 'application/json');
      ShowMessage(Format('Remote apiUi server (%s) connected OK', [Items.XmlValueByTag['Address']]));
      result := True;
    except
      on e: exception do
        ShowMessage(Format ('exception: %s%strying to connect to %s', [e.Message, LineEnding, Items.XmlValueByTag['Address']]));
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.ProjectInfoFromRemoteServer;
var
  xXml: TXml;
begin
  if not Assigned (se) then
    raise Exception.Create('ProjectInfoFromRemoteServer requires an assigned Project');
  if not Assigned (se.remoteServerConnectionXml) then
    raise Exception.Create('ProjectInfoFromRemoteServer requires a Remote Server Connection');
  xXml := TXml.Create;
  try
    try
      xXml.LoadJsonFromString ( xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                                                        , '/apiUi/api/project/information'
                                                        , ''
                                                        , 'GET'
                                                        , 'application/json'
                                                        )
                          , nil
                          );
      xXml.Name := 'projectInformation';
      ShowXmlExtended('Information from remote project', xXml);
    except
      on e: exception do
        ShowMessage(e.Message);
    end;
  finally
    FreeAndNil(xXml);
  end;
end;

function TMainForm.TestProjectFolders(aXml: TObject): Boolean;
var
  xXml: TXml;
  xCurrentFolder, xReferenceFolder,xReportsFolder, xMessage: String;
begin
  result := True;
  xMessage := '';
  xXml := TXml.Create;
  try
    xXml.CopyDownLine((aXml as TXml).Parent as TXml, True);
    xXml.ResolveAliasses;
    with xXml.Items do
    begin
      xCurrentFolder := XmlCheckedValueByTag['current'];
      xReferenceFolder := XmlCheckedValueByTag['reference'];
      xReportsFolder := XmlCheckedValueByTagDef['reports', xCurrentFolder];
    end;
    if xCurrentFolder <> '' then
    begin
      if not LazFileUtils.DirectoryExistsUTF8(xCurrentFolder) then
      begin
        xMessage := xMessage + ifthen(Result, '', LineEnding) + Format ('Folder (current) %s does not exist', [xCurrentFolder]);
        Result := False;
      end;
    end;
    if xReferenceFolder <> '' then
    begin
      if not LazFileUtils.DirectoryExistsUTF8(xReferenceFolder) then
      begin
        xMessage := xMessage + ifthen(Result, '', LineEnding) + Format ('Folder (reference) %s does not exist', [xReferenceFolder]);
        Result := False;
      end;
    end;
    if xReportsFolder <> '' then
    begin
      if not LazFileUtils.DirectoryExistsUTF8(xReportsFolder) then
      begin
        xMessage := xMessage + ifthen(Result, '', LineEnding) + Format ('Folder (reports) %s does not exist', [xReportsFolder]);
        Result := False;
      end;
    end;
    if (xCurrentFolder <> '')
    and (xCurrentFolder = xReferenceFolder) then
    begin
      xMessage := xMessage + ifthen(Result, '', LineEnding) + 'CurrentFolder and ReferenceFolder can not be the same';
      Result := False;
    end;
    if Result then
      xMessage := 'Folders seem OK';
    ShowMessage(xMessage);
  finally
    xXml.Free;
  end;
end;

function TMainForm.EditXmlValueAsText(aXml: TObject): Boolean;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    with EditTexttForm do
    try
      Application.CreateForm(TEditTexttForm, EditTexttForm);
      Caption := (aXml as TXml).FullCaption;
      ScriptEdit.Lines.Text := (aXml as TXml).Value;
      ShowModal;
      if (ModalResult = mrOk)
      and (ScriptEdit.Lines.Text <> (aXml as TXml).Value)
      then
      begin
        result := True;
        (aXml as TXml).Value := ScriptEdit.Lines.Text;
        (aXml as TXml).Checked := True;
      end;
    finally
      FreeAndNil(EditTexttForm);
    end;
  finally
    XmlUtil.PopCursor;
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
                       , True
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
    FocusedOperation := nil;
    UpdateCaption;
    UpdateVisibiltyOfOperations;
    se.Clear;
  end;
end;

procedure TMainForm.ClearNotificationsActionUpdate(Sender: TObject);
begin
  ClearNotificationsAction.Enabled := (se.displayedExceptions.Count > 0);
end;

procedure TMainForm.ClearNotificationsActionExecute(Sender: TObject);
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
    ExceptionStatusBar.Panels.Items[0].Text := '[' + IntToStr(Sender.FocusedNode.Index + 1)
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
  Data: PNavigatorTreeRec;
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

procedure TMainForm.NvgtViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.Selected[Sender.FocusedNode] := True;
  FocusedOperation := NodeToOperation(Sender, Node);
end;

procedure TMainForm.NvgtViewGetText(Sender: TBaseVirtualTree;
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
  if Assigned(FocusedOperation) then
  begin
    reqWsaXml := TXml.Create(-10000, _WsdlWsaXsd);
    rpyWsaXml := TXml.Create(-10000, _WsdlWsaXsd);
    try
      reqWsaXml.CheckDownLine(False);
      reqWsaXml.LoadValues(FocusedOperation.reqWsaXml, False, True);
      rpyWsaXml.CheckDownLine(False);
      rpyWsaXml.LoadValues(FocusedOperation.rpyWsaXml, False, True);
      Application.CreateForm(TwsaConfigForm, wsaConfigForm);
      try
        wsaConfigForm.doReadOnly := se.IsActive;
        wsaConfigForm.Caption := 'Configure WS-Addressing';
        wsaConfigForm.wsaEnabled := FocusedOperation.wsaEnabled;
        wsaConfigForm.wsaSpecificMustUnderstand :=
          FocusedOperation.wsaSpecificMustUnderstand;
        wsaConfigForm.wsaMustUnderstand := FocusedOperation.wsaMustUnderstand;
        wsaConfigForm.wsaTypeComboBox.Text := FocusedOperation.wsaType;
        { }
        if FocusedOperation.StubAction = saRequest then
          wsaConfigForm.wsaXml := reqWsaXml;
        { }
        wsaConfigForm.ShowModal;
        if wsaConfigForm.ModalResult = mrOk then
        begin
          FocusedOperation.wsaEnabled := wsaConfigForm.wsaEnabled;
          FocusedOperation.wsaSpecificMustUnderstand :=
            wsaConfigForm.wsaSpecificMustUnderstand;
          FocusedOperation.wsaMustUnderstand := wsaConfigForm.wsaMustUnderstand;
          FocusedOperation.wsaType := wsaConfigForm.wsaTypeComboBox.Text;
          FocusedOperation.reqWsaXml.CheckDownLine(False);
          FocusedOperation.reqWsaXml.LoadValues(reqWsaXml, False, True);
          FocusedOperation.rpyWsaXml.CheckDownLine(False);
          FocusedOperation.rpyWsaXml.LoadValues(rpyWsaXml, False, True);
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
  OperationWsaAction.Enabled := Assigned(_WsdlWsaXsd)
                            and Assigned(FocusedOperation)
                              ;
end;

procedure TMainForm.FocusOperationsReqVTS;
var
  xNode: PVirtualNode;
begin
  xNode := NvgtView.GetFirst;
  while not(xNode = nil) do
  begin
    if NodeToOperation(NvgtView, xNode) = FocusedOperation then
    begin
      NvgtView.FocusedNode := xNode;
      NvgtView.Selected[xNode] := True;
      exit;
    end;
    xNode := NvgtView.GetNext(xNode);
  end;
end;

procedure TMainForm.ExchangeMessages(fReply, pReply: TWsdlMessage);
var
  f, p: Integer;
begin
  f := FocusedOperation.Messages.IndexOfObject(fReply);
  p := FocusedOperation.Messages.IndexOfObject(pReply);
  if (f > -1) and (p > -1) then
  begin
    FocusedOperation.Messages.Objects[f] := pReply;
    FocusedOperation.Messages.Objects[p] := fReply;
  end;
end;

procedure TMainForm.SelectMessageColumnsActionUpdate(Sender: TObject);
begin
  SelectMessageColumnsAction.Enabled := Assigned(FocusedOperation);
end;

procedure TMainForm.SelectMessageColumnsActionExecute(Sender: TObject);
begin
  Application.CreateForm(TSelectElementsForm, SelectElementsForm);
  try
    GridView.BeginUpdate;
    SelectElementsForm.doShowReq := True;
    SelectElementsForm.doShowRpy := True;
    SelectElementsForm.GroupAllowed := True;
    SelectElementsForm.WsdlOperation := FocusedOperation;
    SelectElementsForm.ControlBinds := FocusedOperation.Messages.Messages[0].ColumnXmls.Clone;
    SelectElementsForm.ShowModal;
    if SelectElementsForm.modalResult = mrOk then
    begin
      FocusedOperation.AcquireLock;
      try
        FocusedOperation.Messages.Messages[0].ColumnXmls.ClearListOnly;
        FocusedOperation.Messages.Messages[0].ColumnXmls.Free;
        FocusedOperation.Messages.Messages[0].ColumnXmls := SelectElementsForm.ControlBinds;
        se.UpdateReplyColumns(FocusedOperation);
        UpdateMessagesGrid;
        stubChanged := True;
      finally
        FocusedOperation.ReleaseLock;
      end;
    end
    else
    begin
      SelectElementsForm.ControlBinds.ClearListOnly;
      SelectElementsForm.ControlBinds.Free;
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
  GridView.FocusedColumn := nMessageButtonColumns;
  for c := GridView.Header.Columns.Count - 1 downto nMessageButtonColumns do
    ColumnWidths.Values[GridView.Header.Columns[c].Text] :=
      IntToStr (GridView.Header.Columns[c].Width);
  if Assigned(FocusedOperation) then
  begin
    try
      while GridView.Header.Columns.Count >
        (nMessageButtonColumns + 1 + FocusedOperation.CorrelationBindables.Count +
          FocusedOperation.Messages.Messages[0].ColumnXmls.Count) do
        GridView.Header.Columns.Delete(GridView.Header.Columns.Count - 1);
      while GridView.Header.Columns.Count <
        (nMessageButtonColumns + 1 + FocusedOperation.CorrelationBindables.Count +
          FocusedOperation.Messages.Messages[0].ColumnXmls.Count) do
        GridView.Header.Columns.Add;
    except
    end;
  end;
end;

procedure TMainForm.UpdateMessagesGrid;
  function _LastCaption (aCaption: String): String;
  var
    x, p: Integer;
  begin
    result := '';
    p := 1;
    for x := 1 to Length (aCaption) do
      if aCaption [x] = '.' then
        p := x + 1;
    result := Copy (aCaption, p, MaxInt);
  end;

var
  X, c: Integer;
  vc: TVirtualTreeColumn;
begin
  RemoveMessageColumns;
  c := nMessageButtonColumns;
  vc := GridView.Header.Columns.Items[c];
  if FocusedOperation.StubAction = saRequest then
    vc.Text := 'Request'
  else
    vc.Text := 'Reply';
  vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
  Inc(c);
  if FocusedOperation.Messages.Count > 0 then
  begin
    with FocusedOperation.Messages.Messages[0] do
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
        vc.Text := _LastCaption(ColumnXmls.Strings[X]);
        vc.Width := StrToIntDef(ColumnWidths.Values[vc.Text], 50);
        Inc(c);
      end;
    end;
  end;
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
      toBindables(Operation);
      CorrelationId := Operation.CorrelationIdAsText('; ');
    end;
  end;
  finally
    MessagesVTS.Invalidate;
    se.ReleaseLogLock;
  end;
end;

procedure TMainForm.SelectFocusedBindInViews;
  procedure _ForceVisibility(aNode: PVirtualNode);
  begin
    if aNode = aNode.NextSibling then
      exit;
    _ForceVisibility(aNode.Parent);
    TreeView.IsVisible[aNode] := True;
  end;

var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
  f: Integer;
begin
  xNode := TreeView.GetFirst; // search from begin
  while not(xNode = nil) do
  begin
    xBind := NodeToBind(TreeView, xNode);
    if xBind = FocusedBind then
    begin
      _ForceVisibility(xNode);
      TreeView.InvalidateNode(xNode);
      TreeView.FocusedNode := xNode;
      TreeView.FocusedColumn := treeValueColumn;
      TreeView.Selected [xNode] := True;
      if Assigned (FocusedMessage)
      and Assigned(FocusedMessage.ColumnXmls) then
      begin
        f := FocusedMessage.ColumnXmls.IndexOfObject(FocusedBind);
        if f > -1 then
        begin
          GridView.FocusedColumn := f + 1 + nMessageButtonColumns + FocusedOperation.CorrelationBindables.Count;
        end;
      end;
      exit;
    end;
    xNode := TreeView.GetNext(xNode);
  end;
end;

procedure TMainForm.ShowFocusedMessageInTreeView;
var
  swapMemoEvent: TNotifyEvent;
begin
  swapMemoEvent := FreeFormatMemo.OnChange;
  TreeView.BeginUpdate;
  try
    FreeFormatMemo.OnChange := nil;
    TreeView.Clear;
    if Assigned(FocusedMessage) then
    begin
      if FocusedOperation.WsdlService.DescriptionType in [ipmDTFreeFormat] then
      begin
        if FocusedOperation.StubAction = saRequest then
          FreeFormatMemo.Text := FocusedMessage.FreeFormatReq
        else
          FreeFormatMemo.Text := FocusedMessage.FreeFormatRpy;
      end
      else
      begin
        if FocusedOperation.StubAction = saRequest then
        begin
          FillBindTreeView(TreeView, FocusedMessage.reqBind, nil);
          FillBindTreeView(TreeView, FocusedMessage.rpyBind, nil);
        end
        else
        begin
          FillBindTreeView(TreeView, FocusedMessage.rpyBind, nil);
          FillBindTreeView(TreeView, FocusedMessage.reqBind, nil);
        end;
      end;
    end;
  finally
    FreeFormatMemo.OnChange := swapMemoEvent;
    TreeView.EndUpdate;
    GridView.InvalidateColumn(nMessageButtonColumns);
    if Assigned(TreeView.FocusedNode) then
      TreeView.ScrollIntoView(TreeView.FocusedNode, False, False);
  end;
end;

procedure TMainForm.setFocusedMessage(const Value: TWsdlMessage);
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
  if Value = fFocusedMessage then Exit;
  fFocusedMessage := Value;
  if Assigned (FocusedOperation)
  and Assigned (Value) then
    FocusedOperation.LastFocusedMessage := Value;
  DisableViewOnFocusChangeEvents;
  try
    xNode := GridView.GetFirst;
    while Assigned(xNode) do
    begin
      xMessage := NodeToMessage(GridView, xNode);
      if xMessage = Value then
      begin
        GridView.Selected[xNode] := True;
        GridView.FocusedNode := xNode;
        ShowFocusedMessageInTreeView;
        exit;
      end;
      xNode := GridView.GetNext(xNode);
    end;
  finally
    EnableViewOnFocusChangeEvents;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  X, wBttn: Integer;
  xIniFile: TFormIniFile;
  xXml: TXml;
  xKey: Word;
  xShift : TShiftState;
  xMenuItem, sMenuItem: TMenuItem;
begin
  fShowKindOfInformation := TShowKindOfInformation (-1);
  SaveNvgtViewOnFocusChanged := NvgtView.OnFocusChanged;
  SaveGridViewOnFocusChanged := GridView.OnFocusChanged;
  SaveTreeViewOnFocusChanged := TreeView.OnFocusChanged;
  doConfirmTemporaryInactivity := False;
  MessagesTabControlWidth := MessagesTabControl.Width;
  MessagesTabControlMinLeft := LastMessageToolButton.Left + LastMessageToolButton.Width + 1;
  (MessagesTabControl as TWinControl).Color := Self.Color;
  MessagesTabCaption := LogTabControl.Tabs [Ord (spMessages)];
  notifyTabCaption := LogTabControl.Tabs [Ord (spNotifications)];
  notifyTabImageIndex := 66;
  se := TWsdlProject.Create;
  se.hasGui := True;
  ProgressInterface := TProgressInterface.Create;
  se.ProgressInterface := ProgressInterface;
  se.EditContexts := EditContexts;
  RefreshLogTimer.Enabled := True;
  NumberOfBlockingThreads := 0;
  se.OnBooleanDialog := BooleanPromptDialog;
  se.OnStartBlockingThread := StartBlockingThreadEvent;
  se.OnTerminateBlockingThread := TerminateBlockingThreadEvent;
  se.OnStartNonBlockingThread := StartNonBlockingThreadEvent;
  se.OnTerminateNonBlockingThread := TerminateNonBlockingThreadEvent;
  se.OnQuitEvent := QuitCommand;
  se.Notify := Notify;
  se.LogServerMessage := LogServerException;
  se.OnDebugOperationEvent := DebugOperation;
  se.FoundErrorInBuffer := FoundErrorInBuffer;
  se.OnReactivateEvent := ReactivateCommand;
  se.OnRestartEvent := RestartCommand;
  se.OnReloadDesignEvent := ReloadDesignCommand;
  DecryptString := doDecryptString;
  EncryptString := doEncryptString;
  xmlUtil.doExpandFull := True;
  ColumnWidths := TJBStringList.Create;
  QueueNameList := TJBStringList.Create;
  QueueNameList.Sorted := True;
  QueueNameList.Duplicates := dupIgnore;
  Application.OnException := HandleException;
  Randomize;
  startStopShortCut := startAction.ShortCut;
  wBttn := MessagesVTS.Header.Columns[Ord(logRemarksColumn)].Width;
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
  NvgtView.Header.Columns[0].Width := wBttn;
  NvgtView.Header.Columns[1].Width := wBttn;
  for x := 0 to nMessageButtonColumns - 1 do
    GridView.Header.Columns[x].Width := wBttn;
  se.projectFileName := xIniFile.StringByName['WsdlStubFileName'];
  wsdlStubMessagesFileName := xIniFile.StringByName['WsdlStubMessagesFileName'];
  wsdlStubSnapshotsFileName := xIniFile.StringByName['wsdlStubSnapshotsFileName'];
  DisclaimerAccepted := xIniFile.BooleanByName['DisclaimerAccepted'];
  ListofOperationsMenuItem.Checked := xIniFile.BooleanByNameDef
    ['ListofOperationsVisible', True];
  ScriptPanel.Visible := ListofOperationsMenuItem.Checked;
  ScriptSplitter.Visible := ListofOperationsMenuItem.Checked;
  SchemapropertiesMenuItem.Checked := xIniFile.BooleanByNameDef
    ['SchemaPropertiesVisible', True];
  XsdPanel.Visible := SchemapropertiesMenuItem.Checked;
  xsdSplitter.Visible := SchemapropertiesMenuItem.Checked;
  WsdlInformationMenuItem.Checked := xIniFile.BooleanByNameDef ['WsdlInformationVisible', True];
  WsdlInfoPanel.Visible := WsdlInformationMenuItem.Checked;
  se.LogFilter.FilterStyle := TLogFilterStyle (xIniFile.IntegerByNameDef['LogFilter.FilterStyle', 0]);
  se.LogFilter.MatchAny := xIniFile.BooleanByNameDef['LogFilter.MatchAny', False];
  se.LogFilter.StubActionEnabled := xIniFile.BooleanByNameDef ['LogFilter.StubActionEnabled', False];
  se.LogFilter.StubActionEquals := xIniFile.BooleanByNameDef ['LogFilter.StubActionEquals', True];
  se.LogFilter.StubAction := TStubAction (xIniFile.IntegerByNameDef['LogFilter.StubAction', 0]);
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
  se.LogFilter.RemarksEnabled := xIniFile.BooleanByNameDef
    ['LogFilter.RemarksEnabled', False];
  CollapseHeaders := xIniFile.BooleanByNameDef['CollapseHeaders', True];
  TreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  TreeView.RootNodeCount := 0;
  NvgtView.NodeDataSize := SizeOf(TNavigatorTreeRec);
  NvgtView.RootNodeCount := 0;
  GridView.NodeDataSize := SizeOf(TMessageTreeRec);
  GridView.RootNodeCount := 0;
  SnapshotsVTS.NodeDataSize := SizeOf(TSnapshotTreeRec);
  SnapshotsVTS.RootNodeCount := 0;
  MessagesVTS.NodeDataSize := SizeOf(TLogTreeRec);
  MessagesVTS.RootNodeCount := 0;
  ExceptionsVTS.NodeDataSize := SizeOf(TExceptionTreeRec);
  ExceptionsVTS.RootNodeCount := 0;
  FileNameList := TJBStringList.Create;
  ReopenCaseList := TJBStringList.Create;
  ReopenCaseList.Text := xIniFile.StringByName['RecentFiles'];
  WsdlPaths := TJBStringList.Create;
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
  se.CompareLogOrderBy := TCompareLogOrderBy
    (xIniFile.IntegerByNameDef['CompareLogOrderBy', Ord(clTimeStamp)]);
  se.ShowLogCobolStyle := TShowLogCobolStyle
    (xIniFile.IntegerByNameDef['ShowLogCobolStyle', Ord(slCobol)]);
  ColumnWidths.Text := xIniFile.StringByNameDef['ColumnWidths', ''];
  doShowDesignAtTop := xIniFile.BooleanByNameDef['doShowDesignAtTop', True];
  bgCorrelationItemColor := xIniFile.IntegerByNameDef['bgCorrelationItemColor',
    bgCorrelationItemColor];
  bgRequestTagNameColumnColor := xIniFile.IntegerByNameDef['RequestTagNameColumn',
    bgRequestTagNameColumnColor];
  bgNilValueColor := xIniFile.IntegerByNameDef['bgNilValueColor',
    bgNilValueColor];
  DesignPanelAtTopMenuItem.Checked := doShowDesignAtTop;
  xXml := TXml.Create;
  try
    xXml.LoadFromString(xIniFile.StringByName['Options'], nil);
    OptionsFromXml(xXml);
  finally
    xXml.Free;
  end;
  if hasOption(contextOpt) then
    setContextProperty(getOption(contextOpt))
  else
    setContextProperty(xIniFile.StringByNameDef['Context', '']);
  xIniFile.Free;
  wsdlStubInitialise;
  se.stubRead := False;
  stubChanged := False;
  nStubs := 0;
  logChartToolButton.Visible := (WindowsUserName = 'Jan')
                             or (WindowsUserName = 'BouwmanJW')
                              ;
  HelpAction.Caption := 'Help on ' + _progName;
  UpdateVisibiltyOfOperations;
  _OnParseErrorEvent := ParserError;
  _WsdlOnMessageChange := OnMessageChanged;
  _WsdlSQLConnectorLog := SQLConnectorLog;
  try
    UpdateVisibiltyTreeView(False);
  except
  end;
  xmlUtil.AcquireLock := AcquireLock;
  xmlUtil.ReleaseLock := ReleaseLock;
  _OnBeginUpdate := BeginConsoleUpdate;
  Xmlz.OnNotify := LogServerNotification;
  sMenuItem := TMenuItem.Create(self);
  for x := 0 to alGeneral.ActionCount - 1 do with alGeneral.Actions[x] as TCustomAction do
  begin
    xMenuItem := TMenuItem.Create(self);
    xMenuItem.Action := alGeneral.Actions[x];
    ShortCutToKey(ShortCut, xKey, xShift);
    if xKey <> 0 then
      EasterEggPopupMenu.Items.Add(xMenuItem)
    else
      sMenuItem.Add(xMenuItem);
  end;
  EasterEggPopupMenu.Items.Add (sMenuItem);
  IpmDescrType := ipmDTXml;
  if hasOption(openSslLocOpt) then
    openSslCertsFolder := getOption(openSslLocOpt);
  if hasOption(projectOpt) then
  begin
    Update;
    se.projectFileName := ExpandRelativeFileName ( LazFileUtils.GetCurrentDirUTF8 + DirectorySeparator
                                                 , getOption(projectOpt)
                                                 );
    OpenStubCase;
    se.Activate(True);
    CheckBoxClick(nil);
    if hasOption(scriptOpt) then
    begin
      ExecuteScript(se, getOption(scriptOpt));
    end;
  end;
  MainToolBarDesignedButtonCount := MainToolBar.ButtonCount;
  CreateScriptsSubMenuItems;
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
  DisableViewOnFocusChangeEvents;
  ClearConsole;
  xIniFile := TFormIniFile.Create(self, False);
  xIniFile.StringByName['Context'] := getContextProperty;
  xIniFile.BooleanByName['DisclaimerAccepted'] := DisclaimerAccepted;
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
  xIniFile.BooleanByName['doScrollMessagesIntoView'] := doScrollMessagesIntoView;
  xIniFile.IntegerByName['GridDataPanelWidth'] := GridDataPanel.Width;
  xIniFile.IntegerByName['GridDataPanelHeight'] := GridDataPanel.Height;
  xIniFile.StringByName['WsdlStubFileName'] := se.projectFileName;
  xIniFile.StringByName['WsdlStubMessagesFileName'] := wsdlStubMessagesFileName;
  xIniFile.StringByName['wsdlStubSnapshotsFileName'] := wsdlStubSnapshotsFileName;
  xIniFile.IntegerByName['LogFilter.FilterStyle'] := Ord
    (se.LogFilter.FilterStyle);
  xIniFile.BooleanByName['LogFilter.MatchAny'] := se.LogFilter.MatchAny;
  xIniFile.BooleanByName['LogFilter.StubActionEnabled'] :=
    se.LogFilter.StubActionEnabled;
  xIniFile.BooleanByName['LogFilter.StubActionEquals'] :=
    se.LogFilter.StubActionEquals;
  xIniFile.IntegerByName['LogFilter.StubAction'] := Ord(se.LogFilter.StubAction);
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
  ColumnWidths.Free;
  FreeAndNil(ProgressInterface);
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
  ShowKindOfInformation := spMessages;
  MessagesTabControl.TabIndex := Ord (slRequestBody);
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
  xmlio.OnNotify := LogServerNotification;
end;

procedure TMainForm.GridViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TCustomBindable;
  Xml: TXml;
  XmlAttr: TXmlAttribute;
  xMessage: TWsdlMessage;
  x, n: Integer;
begin
  xMessage := NodeToMessage(Sender, Node);
  if Column < nMessageButtonColumns then exit;
  if Assigned (xMessage.Duplicates) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];

  if (Column = nMessageButtonColumns) then
  begin
    if Assigned (xMessage.DuplicatesName)
//  or (not xmlio.isFileNameAllowed(xMessage.Name))
    then
      TargetCanvas.Font.Color := clRed;
    exit;
  end;
  if (Column - nMessageButtonColumns) <= xMessage.CorrelationBindables.Count then
    exit;
  xBind := nil;
  try
    xBind := xMessage.ColumnXmls.Bindables
      [Column - nMessageButtonColumns - xMessage.CorrelationBindables.Count - 1];
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
    ClipBoard.AsText := vstToGrid(GridView, GridViewGetText, nMessageButtonColumns);
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.CopyGridActionUpdate(Sender: TObject);
begin
  try
    CopyGridAction.Enabled := Assigned(FocusedOperation)
                            ;
  except
  end;
end;

procedure TMainForm.PasteGridActionUpdate(Sender: TObject);
begin
  PasteGridAction.Enabled := Assigned(FocusedOperation)
                           ;
end;

procedure TMainForm.PasteGridActionExecute(Sender: TObject);
var
  swapNode: PVirtualNode;
  swapColumn: Integer;
begin
  if not ClipBoard.HasFormat(CF_TEXT) then
    raise Exception.Create('Clipboard does not contain text');
  FocusedOperation.AcquireLock;
  try
    GridView.BeginUpdate;
    TreeView.BeginUpdate;
    swapNode := GridView.FocusedNode;
    swapColumn := GridView.FocusedColumn;
    XmlUtil.PushCursor (crHourGlass);
    // vstFromGrid(GridView, ClipBoard.AsText, PasteGridOnNewText);
    PasteGridFromPasteBoard;
    DoColorBindButtons;
  finally
    FocusedOperation.ReleaseLock;
    GridViewFocusedNode(swapNode);
    GridView.FocusedColumn := swapColumn;
    UpdateMessagesView;
    GridView.EndUpdate;
    TreeView.Invalidate;
    TreeView.EndUpdate;
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

procedure TMainForm.ShowXmlExtended(aCaption: String;aXml: TXml);
var
  xXsdDescr: TXsdDescr;
begin
  try
    if not Assigned (aXml.Xsd) then
    begin
      xXsdDescr := TXsdDescr.Create;
      CreateXsdFromXml(xXsdDescr, aXml, True);
    end;
    Application.CreateForm(TShowXmlForm, ShowXmlForm);
    ShowXmlForm.Caption := aCaption;
    ShowXmlForm.isReadOnly := True;
    ShowXmlForm.isCheckedOnly := True;
    ShowXmlForm.Bind := aXml;
    ShowXmlForm.ShowModal;
  finally
    FreeAndNil(ShowXmlForm);
    FreeAndNil(xXsdDescr);
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
    xXsdDescr := TXsdDescr.Create;
    try
      xXml := TXml.Create;
      try
        XmlUtil.PushCursor (crHourGlass);
        try
          xXml.LoadFromString(aText, nil);
          CreateXsdFromXml(xXsdDescr, xXml, True);
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
  xXsdDescr := TXsdDescr.Create;
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
        CreateXsdFromXml(xXsdDescr, xXml, True);
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
  if Assigned (claimedLog.Operation)
  and (claimedLog.Operation.isOpenApiService) then
  begin
    with claimedLog.replyAsXml do
    try
      ShowTextAsXml('Reply as XML', AsText(False, 0, False, False));
    finally
      Free;
    end;
    exit;
  end;
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.ReplyBody, '');
  if (xString <> '') then
  begin
    if Assigned(claimedLog.Mssg) then
    begin
      case claimedLog.Operation.WsdlService.DescriptionType of
        ipmDTFreeFormat:
          ShowInfoForm('Reply freeformat', xString);
        ipmDTCobol:
          begin
            if se.ShowLogCobolStyle = slCobol then
            begin (claimedLog.Operation.rpyBind as TIpmItem)
              .BufferToValues(FoundErrorInBuffer, xString);
              ShowIpm('Cobol view on Reply',
                claimedLog.Operation.rpyBind as TIpmItem);
            end
            else
            begin
              with claimedLog.replyAsXml do
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
        ipmDTJson:
          ShowTextAsXml('Reply as XML', xString);
      end;
    end
    else
      ShowTextAsXml('Reply as XML', xString);
  end;
end;

procedure TMainForm.IntrospectDesign;
begin
  captionFileName := ExtractFileName(se.projectFileName);
  TProcedureThread.Create(False, False, se, se.IntrospectProject);
end;

function TMainForm.createListOfListsForTypeDefs (aTypeDefs: TXsdDataTypeList): TJBStringList ;
var
  x, f: Integer;
begin
  result := TJBStringList.Create;
  result.Sorted := True;
  for x := 0 to aTypeDefs.Count - 1 do with aTypeDefs.XsdDataTypes[x] do
  begin
    if NameSpace <> '' then
    begin
      if not result.Find(NameSpace, f) then
        f := result.AddObject (NameSpace, TJBStringList.Create);
      (result.Objects[f] as TJBStringList).Add (Name);
    end;
  end;
end;

function TMainForm.decorateWithAsterix(aCaption: String; aBoolean: Boolean
  ): String;
const decorationString = ' *';
begin
  result := aCaption;
  if aBoolean then
  begin
    if RightStr(aCaption, 2) <> decorationString then
      result := result + decorationString;
  end
  else
  begin
    if RightStr(aCaption, 2) = decorationString then
      result := LeftStr (result, Length (Result) - Length (decorationString));
  end;
end;

procedure TMainForm.DisableViewOnFocusChangeEvents;
begin
  NvgtView.OnFocusChanged := nil;
  GridView.OnFocusChanged := nil;
  TreeView.OnFocusChanged := nil;
end;

procedure TMainForm.EnableViewOnFocusChangeEvents;
begin
  NvgtView.OnFocusChanged := SaveNvgtViewOnFocusChanged;
  GridView.OnFocusChanged := SaveGridViewOnFocusChanged;
  TreeView.OnFocusChanged := SaveTreeViewOnFocusChanged;
end;

function TMainForm.setContextProperty(aName: String): String;
begin
  result := se.projectContext;
  se.projectContext := aName;
  xmlio.ProjectContext := aName;
end;

function TMainForm.getContextProperty: String;
begin
  result := se.projectContext;
end;

function TMainForm.ActiveAfterPrompt : Boolean ;
begin
  result := False;
  if Assigned (se) then
  begin
    if not se.IsActive then
    begin
      if BooleanPromptDialog(_ProgName + ' not active' + LineEnding + 'Activate now') then
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
      if BooleanPromptDialog(_ProgName + ' is active' + LineEnding + 'Deactivate now') then
        stopActionExecute (self);
    end;
    result := not se.IsActive;
  end;
end;

function TMainForm.SelecFolderAndSave: Boolean;
begin
  result := False;
  Application.CreateForm(TSelectProjectFolderForm, SelectProjectFolderForm);
  with SelectProjectFolderForm do
  try
    Caption := 'Save service virtualisation project (specify a folder with a name ending on '
             + _ProjectFileExtention
             + ')';
    ProjectFolderNameEdit.Text := se.projectFileName;
    ShowModal;
    if ModalResult = mrOK then
    begin
      if ExtractFileExt(FolderName) <> _ProjectFileExtention then
        raise Exception.Create(Format ('Projectname must end with "%s";%s%s', [_ProjectFileExtention, LineEnding, FolderName]));
      if LazFileUtils.FileExistsUTF8(LazFileUtils.AppendPathDelim(FolderName) + _ProjectFileName) then
        if not BooleanPromptDialog('A project named ' + FolderName +  ' already exists' + LineEnding + 'Continue') then
          Exit;
      se.projectFileName := FolderName;
      SaveStubCase;
      result := True;
    end;
  finally
    SelectProjectFolderForm.Free;
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
      se.FindOpenApiOnLog(claimedLog);
      ShowXmlForm.Bind := claimedLog.requestAsXml;
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
        ipmDTCobol:
          begin
            if se.ShowLogCobolStyle = slCobol then
            begin (claimedLog.Operation.reqBind as TIpmItem)
              .BufferToValues(FoundErrorInBuffer, xString);
              ShowIpm('Cobol view on Request',
                claimedLog.Operation.reqBind as TIpmItem);
            end
            else
            begin
              with claimedLog.requestAsXml do
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

procedure TMainForm.WsdlInformationMenuItemClick(Sender: TObject);
begin
  WsdlInformationMenuItem.Checked := not WsdlInformationMenuItem.Checked;
  WsdlInfoPanel.Visible := WsdlInformationMenuItem.Checked;
end;

procedure TMainForm.SaveLogRepliesToFileActionUpdate(Sender: TObject);
begin
  SaveLogRepliesToFileAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.SaveLogRequestsToFileActionUpdate(Sender: TObject);
begin
  SaveLogRequestsToFileAction.Enabled := (se.displayedLogs.Count > 0);
end;

procedure TMainForm.TestBeforeScriptActionExecute(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(se) then
    exit;
  if not ActiveAfterPrompt then
    exit;
  if Assigned(FocusedOperation) then
  begin
    xOperation := TWsdlOperation.Create(FocusedOperation);
    try
      xOperation.CorrelatedMessage := FocusedMessage;
      xOperation.ReqBindablesFromWsdlMessage(FocusedMessage);
      xOperation.RpyBindablesFromWsdlMessage(FocusedMessage);
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
    ShowKindOfInformation := spMessages;
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
      xForm.checkValueAgainst.Text := se.checkValueAgainst.Text;
      xForm.ignoreAddingon.Text := se.ignoreAddingOn.Text;
      xForm.ignoreRemovingOn.Text := se.ignoreRemovingOn.Text;
      for x := 0 to xForm.ignoreOrderOn.Count - 1 do
        xForm.ignoreOrderOn.Objects[x].Free;
      xForm.ignoreOrderOn.Text := se.ignoreOrderOn.Text;
      for x := 0 to xForm.ignoreOrderOn.Count - 1 do
      begin
        xForm.ignoreOrderOn.Objects[x] := TJBStringList.Create;
        (xForm.ignoreOrderOn.Objects[x] as TJBStringList).Text :=
          (se.ignoreOrderOn.Objects[x] as TJBStringList).Text;
      end;
      xForm.regressionSortColumns.Text := se.regressionSortColumns.Text;
      xForm.ShowModal;
      if xForm.configChanged then
      begin
        if BooleanPromptDialog('Accept changes to Regression report settings') then
        begin
          se.CompareLogOrderBy := xForm.compareLogOrderBy;
          se.ignoreDifferencesOn.Text := xForm.ignoreDifferencesOn.Text;
          se.checkValueAgainst.Text := xForm.checkValueAgainst.Text;
          se.ignoreAddingOn.Text := xForm.ignoreAddingon.Text;
          se.ignoreRemovingOn.Text := xForm.ignoreRemovingOn.Text;
          for x := 0 to se.ignoreOrderOn.Count - 1 do
            se.ignoreOrderOn.Objects[x].Free;
          se.ignoreOrderOn.Text := xForm.ignoreOrderOn.Text;
          for x := 0 to se.ignoreOrderOn.Count - 1 do
          begin
            se.ignoreOrderOn.Objects[x] := TJBStringList.Create;
            (se.ignoreOrderOn.Objects[x] as TJBStringList).Text
              := (xForm.ignoreOrderOn.Objects[x] as TJBStringList).Text;
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
  CheckGridFieldsAction.Enabled := Assigned(FocusedOperation)
                               and (FocusedOperation.Messages.Count > 0)
                               and (FocusedOperation.Messages.Messages[0].ColumnXmls.Count > 0)
                                 ;
end;

procedure TMainForm.CheckGridFieldsActionExecute(Sender: TObject);
var
  X, Y, IntrospectDesignAction: Integer;
  xString: String;
  xNode: PVirtualNode;
begin
  xString := ''; //avoid warning
  try
    XmlUtil.PushCursor (crHourGlass);
    with FocusedOperation.Messages do
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
              IntrospectDesignAction := X;
              while IntrospectDesignAction > 0 do
              begin
                xNode := GridView.GetNext(xNode);
                Dec(IntrospectDesignAction);
              end;
              GridView.Selected[xNode] := True;
              GridViewFocusedNode(xNode);
              GridView.FocusedColumn :=
                Y + FocusedOperation.CorrelationBindables.Count + 1;
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

procedure TMainForm.TreeViewChecking(Sender: TBaseVirtualTree;
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
var
  xXml, eXml: TXml;
begin
  xXml := se.EnvVarsAsXml;
  eXml := TXml.Create(-1000, namevaluepairsXsd);
  eXml.Name := xXml.Name;
  eXml.LoadValues(xXml, False);
  try
    if XmlUtil.editXml(eXml, True, False) then
      se.EnvVarsFromXml(eXml);
  finally
    FreeAndNil(xXml);
    FreeAndNil(eXml);
  end;
end;

procedure TMainForm.AddEnvironmentActionUpdate(Sender: TObject);
begin
  AddEnvironmentAction.Enabled := (se.EnvVars.Count > 0);
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
  while EnvironmentMenuItem.Count > 8 do
    EnvironmentMenuItem.Delete(8);
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
  TProcedureThread.Create(False, False, 0, se, se.ScriptExecute, xScript as TObject);
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
  if Assigned(FocusedOperation) then
    result := (FocusedOperation.StubAction = saRequest)
  else
    result := False;
end;

function TMainForm.doDecryptString(aString: AnsiString): AnsiString;
begin
  result := DecryptPassword(aString);
end;

function TMainForm.doEncryptString(aString: AnsiString): AnsiString;
begin
  result := EncryptPassword(aString);
end;

procedure TMainForm.doExecuteRequest;
var
  xOperation: TWsdlOperation;
begin
  if not Assigned (FocusedOperation) then
    raise Exception.Create ('TMainForm.doExecuteRequest: wsdlOp ...');
  FocusedOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(FocusedOperation);
  finally
    FocusedOperation.ReleaseLock;
  end;
  try
    xOperation.CorrelatedMessage := FocusedMessage;
    se.SendMessage(xOperation, FocusedMessage, '');
  finally
    xOperation.Free;
  end;
end;

procedure TMainForm.ExecuteRequestActionExecute(Sender: TObject);
begin
  if not ActiveAfterPrompt then exit;
  ShowKindOfInformation := spMessages;
  TProcedureThread.Create(False, True, se, doExecuteRequest);
end;

procedure TMainForm.ExecuteAllRequests;
var
  X: Integer;
  xOperation: TWsdlOperation;
begin
  FocusedOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(FocusedOperation); // fresh copy
  finally
    FocusedOperation.ReleaseLock;
  end;
  try
    se.AcquireLogLock;
    se.ProgressMax := xOperation.Messages.Count;
    se.ReleaseLogLock;
    for X := 0 to xOperation.Messages.Count - 1 do
    begin
      if abortPressed then
        Break;
      se.AcquireLogLock;
      se.ProgressPos := X + 1;
      se.ReleaseLogLock;
      try
        se.SendMessage(xOperation, xOperation.Messages.Messages[X], '');
      except
      end;
    end;
  finally
    FreeAndNil(xOperation);
  end;
end;

procedure TMainForm.ExecuteLoadTest;
var
  X, y: Integer;
  xOperation: TWsdlOperation;
  doSleep: Boolean;
begin
  doSleep := False;
  FocusedOperation.AcquireLock;
  try
    xOperation := TWsdlOperation.Create(FocusedOperation);
  finally
    FocusedOperation.ReleaseLock;
  end;
  try
    for y := 0 to StressTestLoopsPerThread - 1 do
    begin
      for X := 0 to xOperation.Messages.Count - 1 do
      begin
        if abortPressed then Exit;
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
  finally
    FreeAndNil(xOperation);
  end;
end;

procedure TMainForm.ExecuteAllRequestsActionExecute(Sender: TObject);
begin
  if not ActiveAfterPrompt then exit;
  ShowKindOfInformation := spMessages;
  TProcedureThread.Create(False, True, se, ExecuteAllRequests);
end;

procedure TMainForm.WsdlItemChangeDataTypeMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
  xTypeDef: TXsdDataType;
  x, f: Integer;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
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
        IntrospectDesign;
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
          if Assigned(xLog)
          and (   (xLog.Exception <> '')
               or (    (   (xLog.TransportType = ttHttp)
                        or (xLog.TransportType = ttHttps)
                       )
                   and (   (xLog.httpResponseCode < 200)
                        or (xLog.httpResponseCode > 299)
                       )
                  )
              ) then
            ImageIndex := 84
          else
            ImageIndex := -1;
        end;
      logRequestTreeColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (   (    (Assigned(xLog))
                  and (xLog.RequestBody <> '')
                 )
              or (    (Assigned (xLog))
                  and (Assigned (xLog.Operation))
                  and (xLog.Operation.isOpenApiService)
                 )
             ) then
            if (not xLog.RequestValidated) then
              ImageIndex := 40 // 108
            else if xLog.RequestValidateResult = '' then
              ImageIndex := 39 // 109
            else
              ImageIndex := 25; // 110;
        end;
      logReplyTreeColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.ReplyBody <> '') then
            if (not xLog.ReplyValidated) then
              ImageIndex := 40 // 108
            else if xLog.ReplyValidateResult = '' then
              ImageIndex := 39 // 109
            else
              ImageIndex := 25; // 110;
        end;
      logRequestGridColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.RequestBody <> '') then
            ImageIndex := 46; // 76;
        end;
      logReplyGridColumn:
        begin
          xLog := NodeToMsgLog(False,Sender as TVirtualStringTree, Node);
          if (Assigned(xLog)) and (xLog is TLog) and (xLog.ReplyBody <> '') then
            ImageIndex := 46; // 76;
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

procedure TMainForm.TreePopupMenuPopup(Sender: TObject);
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
  xEnableNumberReferrable: Boolean;
  xExtRecurVisible: Boolean;
  xAddChildVisible: Boolean;
  xRootBase: TXsdDataType;
begin
  EndEdit;
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  xEnableAddMenuItems := False;
  xEnableDelMenuItems := False;
  xAddChildVisible := False;
  xExtRecurVisible := False;
  xEnableCheck := False;
  xEnableStamp := False;
  xEnableNumberReferrable := False;
  if xBind is TXml then
    with xBind as TXml do
    begin
      xEnableAddMenuItems := Assigned(Xsd) and (Xsd.maxOccurs <> '1');
      xEnableNumberReferrable := xEnableAddMenuItems;
      xEnableDelMenuItems := Assigned(Xsd) and
        ((xBind as TXml).Xsd.maxOccurs <> '1') and
        ((xBind as TXml).IndexOfRepeatableItem >= 1);
      if Assigned(TypeDef) then
      begin
        if (TypeDef.IsComplex and (TypeDef.ElementDefs.Count = 0))
        or ((TypeDef.IsBuiltIn) and (TypeDef.BaseDataTypeName = 'anyType'))
        or (TypeDef.Manually) then
          xAddChildVisible := True;
        if (Items.Count = 0) and (TypeDef.ElementDefs.Count > 0) then
          xExtRecurVisible := True;
      end;
      xAddChildVisible := xAddChildVisible { }{ and DebugLogMode{ } ;
      xEnableCheck := Items.Count > 0;
      xEnableStamp := Items.Count = 0;
    end;
  CopyCobolDataToClipboardMenuItem.Visible := (xBind is TIpmItem);
  PasteCobolDataFromClipboardMenuItem.Visible := (xBind is TIpmItem);
  WsdlItemAddMenuItem.Enabled := xEnableAddMenuItems;
  WsdlItemDelMenuItem.Enabled := xEnableDelMenuItems;
  AddChildElementMenuItem.Visible := xAddChildVisible;
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
        Assigned(FocusedOperation)
    and (FocusedOperation.StubAction = saRequest)
    and Assigned(FocusedMessage)
    and (NumberOfBlockingThreads < 1)
    ;
end;

procedure TMainForm.ExecuteAllRequestsActionUpdate(Sender: TObject);
begin
  ExecuteAllRequestsAction.Enabled := Assigned(FocusedOperation)
                                  and (FocusedOperation.StubAction = saRequest)
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
      se.displayedSnapshots.AddObject(xReport.Name, xReport);
      result := True;
      xNode := SnapshotsVTS.AddChild(nil);
      xData := SnapshotsVTS.GetNodeData(xNode);
      xData.Report := xReport;
    end;
    se.toDisplaySnapshots.Clear;
    if ShowKindOfInformation = spSnapshots then
      SnapshotsVTS.Invalidate;
  end;
var
  logAdded, exceptionAdded, uiInvalidated: Boolean;
  xFocusedOperation: TWsdlOperation;
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
    if Assigned (se.ProgressInterface) then
    begin
      if se.ProgressInterface.doUpdateConsole then
      begin
        se.ProgressInterface.doUpdateConsole := False;
        DoUpdateConsole;
      end;
      if se.ProgressInterface.doShowProgress then
      begin
        // pass control to another form, so this one should keep quitfor the mean time
        RefreshLogTimer.Enabled := False;
        se.ReleaseLogLock;
        ShowProgressForm;
        se.AcquireLogLock;
        se.ProgressInterface.doUpdateConsole := False;
        RefreshLogTimer.Enabled := True;
      end;
    end
  finally
    se.ReleaseLogLock;
  end;
  if uiInvalidated then
  begin
    xFocusedOperation := FocusedOperation;
    FocusedOperation := nil;
    FocusedOperation := xFocusedOperation;
  end;
  if logAdded then
    if doScrollMessagesIntoView then
      MessagesVTS.ScrollIntoView(MessagesVTS.GetLast, True, False);
  if exceptionAdded then
  begin
    if ShowKindOfInformation <> spNotifications then
    begin
      LogTabControl.Tabs [Ord (spNotifications)] := notifyTabCaption + ' [*]';
    end;
    if doScrollExceptionsIntoView then
    begin
      ExceptionsVTS.ScrollIntoView(ExceptionsVTS.GetLast, True, False);
      ShowKindOfInformation := spNotifications;
    end;
  end;
end;

procedure TMainForm.RefreshLogTimerTimer(Sender: TObject);
begin
  RefreshLog;
end;

function TMainForm.getHintStrDisabledWhileActive: String;
begin
  result := '';
  if Assigned(se) and se.IsActive then
    result := ' (disabled while Active)';
end;

procedure TMainForm.SetCaptionFileName(AValue: String);
begin
  if fCaptionFileName = AValue then Exit;
  fCaptionFileName := AValue;
  UpdateCaption;
end;

procedure TMainForm.setDoScrollMessagesIntoView(AValue: Boolean);
begin
  fDoScrollMessagesIntoView := AValue;
  CheckBoxClick(nil);
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

procedure TMainForm.setDoTrackDuplicateMessages(AValue: Boolean);
begin
  if fDoTrackDuplicateMessages=AValue then Exit;
  fDoTrackDuplicateMessages:=AValue;
  ToggleTrackDuplicateMessagesAction.Checked := fDoTrackDuplicateMessages;
  UpdateMessagesView;
end;

procedure TMainForm.LogPopupMenuPopup(Sender: TObject);
var
  xLog: TLog;
  n: Integer;
begin
  n := MessagesVTS.SelectedCount;
  xLog := NodeToMsgLog(False,MessagesVTS, MessagesVTS.FocusedNode);
  ShowLogDifferencesAction.Enabled := (n = 2);
  Log2DesignAction.Enabled := (n > 0);
  DisplayedcolumnMenuItem.Enabled := Assigned (xLog)
                                 and Assigned (xLog.Operation)
                                 and (n = 1)
                                 and (MessagesVTS.FocusedColumn >= Ord (logStdColumnCount))
                                   ;
end;

procedure TMainForm.FindActionUpdate(Sender: TObject);
begin
  FindAction.Enabled := (Assigned(FocusedMessage));
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
        CurItem := TreeView.GetNext(TreeView.FocusedNode);
      if (CurItem = nil) // if next object is nil
        or (xmlUtil.SearchScope = 1) then // or search entire scope
        CurItem := TreeView.GetFirst; // search from begin
      while not(CurItem = nil) and not Found do
      begin
        if xmlUtil.SearchIn = treeButtonColumn then
          xmlUtil.SearchIn := treeValueColumn;
        TreeViewGetText(TreeView, CurItem, xmlUtil.SearchIn,
          ttNormal, xNodeText);
        Found := StringMatchesMask(xNodeText, xmlUtil.SearchString, False,
          xmlUtil.SearchUseRegExp);
        if not Found then
          CurItem := TreeView.GetNext(CurItem);
      end;
      if not Found then
        ShowMessage(xmlUtil.SearchString + ' not found')
      else
      begin
        TreeView.FocusedNode := CurItem;
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
  FindNextAction.Enabled := (Assigned(FocusedMessage)) and
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
    CurNode := TreeView.GetNext(TreeView.FocusedNode);
    while not(CurNode = nil) and not Found do
    begin
      TreeViewGetText(TreeView, CurNode, xmlUtil.SearchIn,
        ttNormal, xNodeText);
      Found := StringMatchesMask(xNodeText, xmlUtil.SearchString, False,
        xmlUtil.SearchUseRegExp);
      if not Found then
        CurNode := TreeView.GetNext(CurNode);
    end;
    if not Found then
      ShowMessage(xmlUtil.SearchString + ' not found')
    else
    begin
      TreeView.FocusedNode := CurNode;
    end;
  end;
  {
    EnableActions;
  }
end;

procedure TMainForm.Validate1Click(Sender: TObject);
begin
  xmlUtil.Validate(NodeToBind(TreeView, TreeView.FocusedNode));
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

procedure TMainForm.FocusOnFullCaptionOrFirst(aFullCaption: String);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  if aFullCaption <> '' then
  begin
    xNode := TreeView.GetFirst;
    while Assigned(xNode) do
    begin
      xBind := NodeToBind(TreeView, xNode);
      if (xBind.FullCaption = aFullCaption) then
      begin
        FocusedBind := xBind;
        exit;
      end;
      xNode := TreeView.GetNext(xNode);
    end;
  end;
  FocusedBind := NodeToBind (TreeView, TreeView.GetFirst);
end;

procedure TMainForm.CheckTreeActionUpdate(Sender: TObject);
begin
  CheckTreeAction.Enabled := (Assigned(FocusedOperation))
                         and (FocusedOperation.Messages.Count > 0)
                         and (FocusedOperation.WsdlService.DescriptionType <> ipmDTFreeFormat)
                           ;
end;

procedure TMainForm.CheckTreeActionExecute(Sender: TObject);
var
  xNode, lastNode: PVirtualNode;
  xBind: TCustomBindable;
  xMessage: String;
begin
  xMessage := ''; // avoid warning
  try
    XmlUtil.PushCursor (crHourGlass);
    xNode := TreeView.GetFirst;
    xBind := NodeToBind(TreeView, xNode);
    if (xBind is TIpmItem) then
      raise Exception.Create('Not implemented for Cobol');
    if FocusedOperation.WsdlService.DescriptionType = ipmDTFreeFormat then
      raise Exception.Create('Not implemented for FreeFormat');
    if (not(xBind as TXml).TypeDef.IsValidXml((xBind as TXml), xMessage)) then
    // at least one error; try to come close
    // might be an assignment
    begin
      while Assigned(xNode) and ((xNode.Parent <> TreeView.RootNode) or
          (xNode = TreeView.GetFirst)) do
      begin // first check elements
        lastNode := xNode; // remember for next testloop
        xBind := NodeToBind(TreeView, xNode);
        if (xBind is TXml)
        and (xBind as TXml).CheckedAllUp
        and (not(xBind as TXml).Group)
        and (not xBind.IsExpression)
        and (not(xBind as TXml).IsValueValidAgainstXsd(xMessage)) then
        begin
          TreeView.FocusedNode := xNode;
          TreeView.FocusedColumn := treeValueColumn;
          TreeView.SetFocus;
          Raise Exception.Create(xMessage);
        end;
        if (xBind is TXml) // check for missing mandatory element or group
        and (not(xBind as TXml).Checked)
        and ((xBind as TXml).Xsd.minOccurs <> '0')
        and ((xBind as TXml).Xsd.maxOccurs = '1') // better not check in case more ocurrences possible
        and (   (not Assigned((xBind as TXml).Parent))
             or (    (xBind as TXml).Parent.CheckedAllUp
                 and (LowerCase((xBind.Parent as TXml).Xsd.sType.ContentModel) <> 'choice')
                )
            ) then
        begin
          TreeView.FocusedNode := xNode;
          TreeView.FocusedColumn := treeValueColumn;
          TreeView.SetFocus;
          Raise Exception.Create((xBind as TXml).TagName + ' expected');
        end;
        xNode := TreeView.GetNext(xNode);
      end;

      // xNode := TreeView.GetLast; // check groups, from last to first to be most specific
      xNode := lastNode; // check groups, from last to first to be most specific
      while Assigned(xNode) do
      begin
        xBind := NodeToBind(TreeView, xNode);
        if (xBind is TXml)
        and (xBind as TXml).CheckedAllUp
        and ((xBind as TXml).Group)
        and (not(xBind as TXml).TypeDef.IsValidXml ((xBind as TXml), xMessage)) then
        begin
          TreeView.FocusedNode := xNode;
          TreeView.FocusedColumn := TreeView.Header.MainColumn;
          TreeView.SetFocus;
          Raise Exception.Create(xMessage);
        end;
        xNode := TreeView.GetPrevious(xNode);
      end;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ProjectOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
  xXsd: TXsd;
  xWasActive: Boolean;
begin
  xXsd := projectOptionsXsd.XsdByCaption ['projectOptions.DatabaseConnection.TestConnection'];
  xXsd.EditProcedure := TestDbsConnection;
  xXsd.isCheckboxDisabled := True;
  xXsd := projectOptionsXsd.XsdByCaption ['projectOptions.General.projectFolders.TestFolders'];
  xXsd.EditProcedure := TestProjectFolders();
  xXsd.isCheckboxDisabled := True;
  xXml := se.ProjectOptionsAsXml(False, '');
  try
    if EditXmlXsdBased ('Project Options'
                       , ''
                       , ''
                       , ''
                       , False
                       , False
                       , esOne
                       , projectOptionsXsd
                       , xXml
                       , True
                       , ConfirmTemporarelyInactivity
                       ) then
    begin
      AcquireLock;
      try
        stubChanged := True;
        xWasActive := se.IsActive;
        if xWasActive then
        begin
          se.Activate (False);
          CheckBoxClick(nil);
        end;
        se.ProjectOptionsFromXml(xXml);
        LogUpdateColumns;
        if xWasActive then
        begin
          se.Activate (True);
          CheckBoxClick(nil);
        end;
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
  if MessageDlg( E.Message + LineEnding +  LineEnding + 'Show extra information?'
               , mtError
               , [mbNo, mbYes]
               , 0
               ) = mrYes then
    ShowText('Exception details', E.Message + LineEnding + LineEnding + s);
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

function TMainForm.getXmlViewType: TxvViewType;
begin
  result := TxvViewType(ViewStyleComboBox.ItemIndex);
end;

procedure TMainForm.ViewStyleComboBoxChange(Sender: TObject);
begin
  if Assigned(Wsdl) then
  begin
    RevalidateXmlTreeView(TreeView);
    TreeView.ScrollIntoView(TreeView.FocusedNode, True, False);
  end;
end;

procedure TMainForm.TreeViewFocusChanging(Sender: TBaseVirtualTree;
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
  xXsdDescr: TXsdDescr;
begin
  xXsdDescr := nil;
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.ReplyBody, '');
  try
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
          with claimedLog.replyAsXml do
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
        if claimedLog.Operation.WsdlService.DescriptionType = ipmDTFreeFormat then
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
        if claimedLog.Operation.Wsdl.isOpenApiService then
        begin
          with claimedLog.replyAsXml do
          try
            ShowXmlInGrid (thisXml, True);
          finally
            Free;
          end;
          exit;
        end;
      end;
      xmlUtil.presentString('Reply', claimedLog.ReplyBody);
    end;
  finally
    FreeAndNil(xXsdDescr);
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
          aXml.AddXml(requestAsXml);
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
          aXml.AddXml(replyAsXml);
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
          xXml := xLog.requestAsXml
        else
          xXml := xLog.replyAsXml;
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
  xXsdDescr: TXsdDescr;
begin
  xXsdDescr := nil;
  xString := IfThen(Assigned(claimedLog) and (claimedLog is TLog), claimedLog.RequestBody, '');
  if (xString <> '') then
  try
    if Assigned(claimedLog.Mssg) and Assigned(claimedLog.Operation)
    and (claimedLog.Operation.reqBind is TIpmItem) then
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
        with claimedLog.requestAsXml do
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
      if claimedLog.Operation.WsdlService.DescriptionType = ipmDTFreeFormat then
      begin
        xmlUtil.presentString('Request', claimedLog.RequestBody);
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
      if Assigned(claimedLog.Operation)
      and (claimedLog.Operation.isOpenApiService) then
      begin
        with claimedLog.requestAsXml do
        try
          ShowXmlInGrid (thisXml, True);
        finally
          Free;
        end;
        exit;
      end;
    end;
    xmlUtil.presentString('Request', claimedLog.RequestBody);
  finally
    FreeAndNil(xXsdDescr);
  end;
end;

procedure TMainForm.ToggleDebugLogModeActionExecute(Sender: TObject);
begin
  DebugLogMode := not DebugLogMode;
end;

procedure TMainForm.SetAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
  se.abortPressed := Value;
  MessagesStatusBar.Invalidate;
end;

procedure TMainForm.ShowRemarksActionExecute(Sender: TObject);
begin
  if Assigned (claimedLog)
  and (claimedLog.Remarks <> '') then
    xmlUtil.presentString('Remarks', claimedLog.Remarks);
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
//XmlUtil.presentAsText('AllOps', allOperations.Text);
  SjowMessage('se.PathFormats' + LineEnding + se.PathFormats.Text);
  SjowMessage('se.PathInfos' + LineEnding + se.PathInfos.Text);
  SjowMessage('se.PathRegexps' + LineEnding + se.PathRegexps.Text);
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
  xXml := NodeToBind(TreeView, TreeView.FocusedNode) as TXml;
  xStmnt := 'with ' + IfThen(xXml.Root = FocusedOperation.reqBind, 'Req.', 'Rpy.')
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
             + _ProgName
             + 'Menu.xsd'
             ;
  xXsdDescr := TXsdDescr.Create;
  try
    try
      xXsdDescr.LoadXsdFromFile(xFileName, nil, nil);
      xXml := TXml.Create(0, xXsdDescr.TypeDef.ElementDefs.Xsds[0]);
      try
        EditXmlXsdBased('Press Ctrl_Alt_H to generate Help file', 'Menu',
          'Menu', '', True, False, esUsed, xXsdDescr.TypeDef.ElementDefs.Xsds[0], xXml, True);
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

procedure TMainForm.GridViewFocusedNode(aNode: PVirtualNode);
begin
  if (toMultiSelect in GridView.TreeOptions.SelectionOptions) then
  begin
    GridView.TreeOptions.SelectionOptions := GridView.TreeOptions.SelectionOptions - [toMultiSelect];
    GridView.Selected[aNode] := True;
    GridView.TreeOptions.SelectionOptions := GridView.TreeOptions.SelectionOptions + [toMultiSelect];
  end
  else
  GridView.Selected[aNode] := True;
  GridView.FocusedNode := aNode;
  ShowFocusedMessageInTreeView;
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
      CanClose := False;
  end;
end;

procedure TMainForm.DebugOperationViewAsXml;
begin
  xmlUtil.ViewAsXml(se.DebugOperation.rpyBind, False);
end;

procedure TMainForm.DebugBeforeScriptActionExecute(Sender: TObject);
var
  xOperation: TWsdlOperation;
begin
  if not Assigned(se) then
    exit;
  if Assigned(FocusedOperation) then
  begin
    xOperation := TWsdlOperation.Create(FocusedOperation);
    try
      xOperation.CorrelatedMessage := FocusedMessage;
      ShowInfoForm('debug Before', xOperation.BeforeActivatorDebugString);
    finally
      xOperation.Free;
    end;
  end;
end;

procedure TMainForm.DebugOperation;
begin
  SynchronizeMethode(DebugOperationViewAsXml);
end;

procedure TMainForm.doSaveLogRepliesToDisk;
var
  xNode: PVirtualNode;
  xLog: TLog;
  sl: TJBStringList;
begin
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := se.displayedLogs.Count + 20;
    ProgressBar.Position := 0;
    try
      sl := TJBStringList.Create;
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
  sl: TJBStringList;
begin
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := se.displayedLogs.Count + 20;
    ProgressBar.Position := 0;
    try
      sl := TJBStringList.Create;
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
  AcquireLock;
  try
    XmlUtil.PushCursor (crHourGlass);
    ProgressBar.Min := 0;
    ProgressBar.Position := 0;
    ProgressBar.Max := FocusedOperation.Messages.Count;
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
          xMessage := NodeToMessage(GridView, xNode);
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
              if FocusedOperation.StubAction = saRequest then
              begin
                if FocusedOperation.reqBind is TIpmItem then
                begin
                  FocusedOperation.reqIpm.LoadValues(xMessage.reqIpm);
                  FocusedOperation.ExecuteBefore;
                  FocusedOperation.ExecuteReqStampers;
                  xMsgString := FocusedOperation.reqIpm.ValuesToBuffer(nil)
                end
                else
                begin (FocusedOperation.reqBind as TXml)
                  .ResetValues; (FocusedOperation.reqBind as TXml)
                  .LoadValues(xMessage.reqBind as TXml, True, True);
                  FocusedOperation.ExecuteBefore;
                  FocusedOperation.ExecuteReqStampers;
                  xMsgString := FocusedOperation.StreamRequest(_progName, True,
                    True, True);
                end;
              end
              else
              begin
                if FocusedOperation.rpyBind is TIpmItem then
                begin
                  FocusedOperation.rpyIpm.LoadValues(xMessage.rpyIpm);
                  FocusedOperation.ExecuteBefore;
                  FocusedOperation.ExecuteRpyStampers;
                  xMsgString := FocusedOperation.rpyIpm.ValuesToBuffer(nil)
                end
                else
                begin (FocusedOperation.rpyBind as TXml)
                  .ResetValues; (FocusedOperation.rpyBind as TXml)
                  .LoadValues(xMessage.rpyBind as TXml, True, True);
                  FocusedOperation.ExecuteBefore;
                  FocusedOperation.ExecuteRpyStampers;
                  xMsgString := FocusedOperation.PrepareReply(_progName, True);
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
  if ShowKindOfInformation = spMessages then
  begin
    LogTabControl.Tabs[Ord (spMessages)] := MessagesTabCaption; // remove asterix since exceptions can be viewed now
  end;
  if ShowKindOfInformation = spNotifications then
  begin
    LogTabControl.Tabs[Ord (spNotifications)] := notifyTabCaption; // remove asterix since exceptions can be viewed now
  end;
end;

procedure TMainForm.doReadMessagesFromDisk;
var
  f: Integer;
  xMessage: TWsdlMessage;
  xFileName, xMsgString: String;
  xPatterns: TJBStringList;
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
  xPatterns := TJBStringList.Create;
  try
    try
      for f := 0 to FileNameList.Count - 1 do
      begin
        if abortPressed then
          Break;
        FocusedOperation.AcquireLock;
        try
          ProgressBar.Position := ProgressBar.Position + 1;
          try
            xFileName := ExtractFileName(FileNameList.Strings[f]);
            xFileName := Copy(xFileName, 1,
              Length(xFileName) - Length
                (ExtractFileExt(FileNameList.Strings[f])));
            if (ExplodeStr(xFileName, saveToDiskSeparator[1], xPatterns, True,
                False) <> FocusedOperation.CorrelationBindables.Count) then
              raise Exception.Create('Filename: ' + xFileName +
                  ' does not fit correlation');
            xMsgString := ReadStringFromFile(FileNameList.Strings[f], nil);
            if FocusedOperation.StubAction = saRequest then
            begin
              xMessage := TWsdlMessage.CreateRequest(FocusedOperation,
                'Request' + IntToStr(FocusedOperation.Messages.Count),
                xPatterns.Text,
                xsdNowAsDateTime + '  Read from ' + FileNameList.Strings[f]);
              if xMessage.reqBind is TIpmItem then
                with xMessage.reqBind as TIpmItem do
                  BufferToValues(FoundErrorInBuffer, xMsgString)
              else
              begin
                xXml := TXml.Create;
                try
                  xXml.LoadFromString(xMsgString, nil);
                  FocusedOperation.XmlRequestToBindables (xXml, False);
                  (xMessage.reqBind as TXml).Reset;
                  (xMessage.reqBind as TXml).LoadValues((FocusedOperation.reqBind as TXml), True, True);
                finally
                  xXml.Free;
                end;
              end;
            end
            else
            begin
              xMessage := TWsdlMessage.CreateReply(FocusedOperation,
                'Reply' + IntToStr(FocusedOperation.Messages.Count),
                xPatterns.Text,
                xsdNowAsDateTime + '  Read from ' + FileNameList.Strings[f]);
              if xMessage.rpyBind is TIpmItem then
                 (xMessage.rpyBind as TIpmItem).BufferToValues(FoundErrorInBuffer, xMsgString)
              else
              begin
                xXml := TXml.Create;
                try
                  xXml.LoadFromString(xMsgString, nil);
                  FocusedOperation.XmlReplyToBindables (xXml, False);
                  (xMessage.rpyBind as TXml).Reset;
                  (xMessage.rpyBind as TXml).LoadValues((FocusedOperation.rpyBind as TXml), True, True);
                finally
                  xXml.Free;
                end;
              end;
            end;
            se.UpdateMessageRow(FocusedOperation, xMessage);
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
          FocusedOperation.ReleaseLock;
        end;
      end;
    finally
      UpdateMessagesView;
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

procedure TMainForm.doRevalidateMessages;
var
  xNode: PVirtualNode;
  xMessage: TWsdlMessage;
begin
  FocusedOperation.AcquireLock;
  try
    xNode := GridView.GetFirstSelected;
    while Assigned(xNode) do
    begin
      xMessage := NodeToMessage(GridView, xNode);
      xNode := GridView.GetNextSelected(xNode);
    end;
    GridView.InvalidateColumn(0);
  finally
    FocusedOperation.ReleaseLock;
  end;
end;

procedure TMainForm.PasteGridFromPasteBoard;
  function TabSepLineToStringGrid(aLine: String): TJBStringList;
  var
    c: Integer;
    col: String;
  begin
    result := TJBStringList.Create;
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
  copyLines, copyColumns: TJBStringList;
  l, c: Integer;
  xMessage: TWsdlMessage;
  xBind: TCustomBindable;
begin
  copyLines := TJBStringList.Create;
  try
    copyLines.Text := ClipBoard.AsText;
    // first check if first line is columnheader line
    copyColumns := TabSepLineToStringGrid(copyLines.Strings[0]);
    c := 0;
    while (c < copyColumns.Count) and (c + nMessageButtonColumns < GridView.Header.Columns.Count) do
    begin
      if (copyColumns.Strings[c] <> GridView.Header.Columns.Items[c + nMessageButtonColumns].Text) then
        raise Exception.Create('Columnheaders do not match, Operation aborted');
      Inc(c);
    end;

    l := 1; // line zero contains columnheaders
    while (l < copyLines.Count) do
    begin
      if l <= FocusedOperation.Messages.Count then
        xMessage := FocusedOperation.Messages.Messages[l - 1]
      else
        xMessage := NodeToMessage(GridView, AddMessage(GridView.GetFirst));
      copyColumns := TabSepLineToStringGrid(copyLines.Strings[l]);
      try
        c := 0;
        while (c < copyColumns.Count) and (c + nMessageButtonColumns < GridView.Header.Columns.Count) do
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
            if c <= xMessage.CorrelationBindables.Count then
            begin
              if c < copyColumns.Count then
              begin
                if copyColumns.Strings[c] <> xMessage.CorrelationBindables.Bindables[c - 1]
                  .CorrelationValue then
                begin
                  if l = 1 then
                    raise Exception.Create(
                      'Not allowed to change this pattern into ' +
                        copyColumns.Strings[c])
                  else
                  begin
                    xMessage.CorrelationBindables.Bindables[c - 1].CorrelationValue :=
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
                    [c - xMessage.CorrelationBindables.Count - 1])) then
              begin
                xBind := xMessage.ColumnXmls.Bindables [c - xMessage.CorrelationBindables.Count - 1];
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
        if Assigned(xLog)
        and Assigned(xLog.Operation)
        and (xLog.Exception = '') then
        begin
          xLog.toBindables(xLog.Operation);
          if xLog.Operation.StubAction = saRequest then
          begin
            xMessage := TWsdlMessage.CreateRequest
                        ( xLog.Operation
                        , 'Request' + IntToStr(xLog.Operation.Messages.Count)
                        , xLog.Operation.CorrelationBindables.ValueText
                        , 'Logged at ' + DateTimeToStr(xLog.InboundTimeStamp)
                        );
          end
          else
          begin
            xMessage := TWsdlMessage.CreateReply
                       ( xLog.Operation
                       , 'Reply' + IntToStr(xLog.Operation.Messages.Count)
                       , xLog.Operation.CorrelationBindables.ValueText
                       , 'Logged at ' + DateTimeToStr(xLog.InboundTimeStamp)
                       );
          end;
          xMessage.DocumentationEdited := True;
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
          if xLog.Operation = FocusedOperation then
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
  procedure _updateIgnoreCoverageOn(xXml: TXmlCvrg; sl: TJBStringList);
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
  LogCoverageReportAction.Enabled := Assigned(FocusedOperation);
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
                    , True
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
  ViewMssgAsTextAction.Enabled := Assigned(FocusedMessage);
end;

procedure TMainForm.ViewMssgAsTextActionExecute(Sender: TObject);
var
  xMessage: String;
begin
  EndEdit;
  if not Assigned(FocusedMessage) then
    raise Exception.Create('No message selected');
  if FocusedOperation.StubAction = saRequest then
  begin
    FocusedOperation.AcquireLock;
    try
      if FocusedOperation.reqBind is TXml then with FocusedOperation.reqBind as TXml do
      begin
        ResetValues;
        LoadValues((FocusedMessage.reqBind as TXml), True, True);
        xMessage := FocusedOperation.StreamRequest(_progName, True, True, True);
      end
      else
      begin
        xMessage := (FocusedMessage.reqBind as TIpmItem).ValuesToBuffer(nil);
      end;
    finally
      FocusedOperation.ReleaseLock;
    end;
    ShowInfoForm('Request', xMessage);
  end
  else
  begin
    try
      FocusedOperation.AcquireLock;
      if FocusedOperation.rpyBind is TXml then with FocusedOperation.rpyBind as TXml do
      begin
        ResetValues;
        LoadValues((FocusedMessage.rpyBind as TXml), False, True);
        (FocusedOperation.fltBind as TXml).ResetValues;
        (FocusedOperation.fltBind as TXml).LoadValues((FocusedMessage.fltBind as TXml), False, True);
        if FocusedMessage.fltBind.Checked then
          xMessage := FocusedOperation.StreamFault(_progName, True)
        else
          xMessage := FocusedOperation.PrepareReply(_progName, True);
      end
      else
      begin
        xMessage := (FocusedMessage.rpyBind as TIpmItem).ValuesToBuffer(nil);
      end;
    finally
      FocusedOperation.ReleaseLock;
    end;
    ShowInfoForm('Reply', xMessage);
  end;
end;

procedure TMainForm.CopyCobolDataToClipboardMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := (NodeToBind(TreeView,
      TreeView.FocusedNode) as TIpmItem).ValuesToBuffer(nil);
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
          AddXml (fLog.requestAsXml);
        with AddXml (TXml.CreateAsString('Rpy', '')) do
          AddXml (fLog.replyAsXml);
        fXml.SeparateNsPrefixes;
        a2bExpandWhenValueIsJsonOrYaml(fXml);
      end;
      nXml := TXml.CreateAsString('nextSelected', '');
      with nXml do
      begin
        AddXml (TXml.CreateAsTimeStamp('inboundTimestamp', nLog.InboundTimeStamp));
        AddXml (TXml.CreateAsTimeStamp('outboundTimestamp', nLog.OutboundTimeStamp));
        with AddXml (TXml.CreateAsString('Req', '')) do
          AddXml (nLog.requestAsXml);
        with AddXml (TXml.CreateAsString('Rpy', '')) do
          AddXml (nLog.replyAsXml);
        nXml.SeparateNsPrefixes;
        a2bExpandWhenValueIsJsonOrYaml(nXml);
      end;
    finally
      fLog.Disclaim;
      nLog.Disclaim;
    end;
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

procedure TMainForm.ShowShortCutActionsActionExecute (Sender : TObject );
var
  sl: TJBStringList;
  x: Integer;
  xKey: Word;
  xShift : TShiftState;
  xKeyString: String;
  xPopUpMenu: TPopupMenu;
  xMenuItem: TMenuItem;
begin
{
  sl := TJBStringList.Create;
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
}
  EasterEggPopupMenu.PopUp(10, 10);
end;

procedure TMainForm.PasteCobolDataFromClipboardMenuItemClick(Sender: TObject);
begin (NodeToBind(TreeView, TreeView.FocusedNode) as TIpmItem)
  .BufferToValues(nil, ClipBoard.AsText);
  TreeView.Invalidate;
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
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
    try
      SelectXmlElementForm.doShowReq := True;
      SelectXmlElementForm.doShowRpy := True;
      SelectXmlElementForm.WsdlOperation := FocusedOperation;
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
        FocusedOperation.PrepareRpyStamper(xBind);
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
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    FocusedOperation.BindChecker(xBind);
    Application.CreateForm(TEditCheckerForm, EditCheckerForm);
    try
      EditCheckerForm.ScriptMemo.ReadOnly := False;
      EditCheckerForm.WsdlOperation := FocusedOperation;
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
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if Assigned(xBind) then
  begin
    Application.CreateForm(TEditStamperForm, EditStamperForm);
    try
      EditStamperForm.ScriptMemo.ReadOnly := False;
      EditStamperForm.WsdlOperation := FocusedOperation;
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
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    ChangeXmlDataType(xBind as TXml, TXsdDataType((Sender as TMenuItem).Tag));
    UpdateXmlTreeViewNode(TreeView, TreeView.FocusedNode);
    TreeView.FocusedColumn := 0;
    TreeView.Expanded[TreeView.FocusedNode] := True;
    se.UpdateMessageRow(FocusedOperation, FocusedMessage);
    TreeView.Invalidate;
    GridView.InvalidateNode(GridView.FocusedNode);
    TreeViewFocusChanged(TreeView, TreeView.FocusedNode,
      TreeView.FocusedColumn);
  end;
end;

procedure TMainForm.ServiceOptionsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned (FocusedOperation) then
    raise Exception.Create('No service selected');
  with FocusedOperation.WsdlService do
  begin
    xXml := OptionsAsXml;
    try
      if EditXmlXsdBased('Service Options', '', '', '', False, False, esUsed, serviceOptionsXsd, xXml, True) then
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

procedure TMainForm.CobolOperationsActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  xXml := se.cobolOperationsXml;
  OperationDefsXsd.XsdByCaption ['OperationDefs.CobolOperations.Operation.Annotation']
    .EditProcedure := EditXmlValueAsText;
  if EditXmlXsdBased ( 'Cobol Operations'
                     , 'OperationDefs.CobolOperations'
                     , 'CobolOperations.Operation.Name'
                     , 'CobolOperations.Operation.Name'
                     , se.IsActive
                     , xXml.Items.Count > 1
                     , esUsed
                     , OperationDefsXsd
                     , xXml
                     , True
                     ) then
  begin
    stubChanged := True;
    BeginConsoleUpdate;
    se.cobolOperationsUpdate(xXml, se.projectFileName);
    IntrospectDesign;
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
  xWasActive: Boolean;
begin
  xXml := TXml.Create;
  try
    xXml.CopyDownLine(se.Listeners.SpecificationXml, True);
    if EditXmlXsdBased('Configure Listeners', '', '', '', False, False, esUsed, listenersConfigXsd, xXml, True, ConfirmTemporarelyInactivity) then
    begin
      stubChanged := True;
      se.Listeners.SpecificationXml.CopyDownLine(xXml, True);
      xWasActive := se.IsActive;
      if xWasActive then
      begin
        se.Activate(False);
        CheckBoxClick(nil);
      end;
      se.Listeners.FromXml(se.HaveStompFrame);
      if xWasActive then
      begin
        se.Activate(True);
        CheckBoxClick(nil);
      end;
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
      'Log.maxEntries', '', False, False, esUsed, projectOptionsXsd, xXml, True) then
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

procedure TMainForm.Revalidatemessages1Click(Sender: TObject);
var
  o, m: Integer;
begin
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

procedure TMainForm.SynchronizedOnMessageChanged;
begin
  GridView.InvalidateColumn(0);
end;

procedure TMainForm.OnMessageChanged(aMessage: TWsdlMessage);
begin
  SynchronizeMethode(SynchronizedOnMessageChanged);
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
      ShowXmlExtended('LogDetails', aXml.Items.XmlItems[0])
    else
      ShowXmlExtended('LogDetails', aXml);
  finally
    aXml.Free;
  end;
end;

procedure TMainForm.CheckRpyOrFlt(aBind: TCustomBindable);
begin
  if (not(aBind is TIpmItem))
  and (FocusedOperation.StubAction <> saRequest)
  and (not FocusedOperation.isOpenApiService)
  and (aBind.Root <> FocusedMessage.reqBind) then
  begin
    if aBind.Root = FocusedMessage.rpyBind then
      FocusedMessage.fltBind.Checked := False
    else if Assigned(FocusedMessage.rpyBodyBind) then
      FocusedMessage.rpyBodyBind.Checked := False;
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
  if not Assigned(FocusedOperation) then
    raise Exception.Create('no operation');
  Application.CreateForm(TDelayTimeForm, DelayTimeForm);
  try
    DelayTimeForm.Caption := 'Delay response for ' + FocusedOperation.Name +
      ' (ms)';
    DelayTimeForm.DelayMsMin := FocusedOperation.DelayTimeMsMin;
    DelayTimeForm.DelayMsMax := FocusedOperation.DelayTimeMsMax;
    DelayTimeForm.ShowModal;
    if DelayTimeForm.ModalResult = mrOk then
    begin
      FocusedOperation.DelayTimeMsMin := DelayTimeForm.DelayMsMin;
      FocusedOperation.DelayTimeMsMax := DelayTimeForm.DelayMsMax;
      if (FocusedOperation.DelayTimeMsMin = 0) and
        (FocusedOperation.DelayTimeMsMax = 0) then
        OperationDelayResponseTimeAction.ImageIndex := 60
      else
        OperationDelayResponseTimeAction.ImageIndex := 61;
      case DelayTimeForm.ApplyToRadioGroup.ItemIndex of
        1:
          begin
            for o := 0 to FocusedOperation.WsdlService.Operations.Count - 1 do
              _Apply(FocusedOperation.WsdlService.Operations.Operations[o],
                FocusedOperation);
          end;
        2:
          begin
            for s := 0 to Wsdl.Services.Count - 1 do
            begin
              for o := 0 to Wsdl.Services.Services[s].Operations.Count - 1 do
                _Apply(Wsdl.Services.Services[s].Operations.Operations[o],
                  FocusedOperation);
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
                    FocusedOperation);
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

procedure TMainForm .NvgtViewPaintText (
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
        if UpperCase(allOperations.Operations[o].Alias) = UpperCase(xOperation.Alias) then
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
  xmlUtil.presentString(MessagesTabControl.Tabs[Ord (ShowKindOfLogData)], LogMemo.Text);
end;

procedure TMainForm .PresentLogMemoTextActionUpdate (Sender : TObject );
begin
  PresentLogMemoTextAction.Enabled := (LogMemo.Lines.Count > 0);
end;

procedure TMainForm.ProjectDesignToClipboardActionExecute(Sender: TObject);
begin
  Clipboard.AsText := se.ProjectDesignAsString;
end;

procedure TMainForm.ReportOnSnapshots (aList: TClaimableObjectList);
var
  x: Integer;
begin
  with aList as TSnapshotList do
    for x := 0 to Count - 1 do
      if not se.abortPressed then
        SnapshotItems[x].doReport;
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
  XmlUtil.presentAsHTML(_ProgName + ' - Test summary report', htmlReportTestSummary(se, aList as TSnapshotList));
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

procedure TMainForm .SchemasToZipExecute (Sender : TObject );
  procedure _wsdlZipper (aZipFileName: String);
  var
    x, w, n, f: Integer;
    slFileNames, slNames: TJBStringList;
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
      slFileNames := TJBStringList.Create;
      try
        slFileNames.Sorted := True;
        slFileNames.Duplicates := dupError;
        for w := 0 to se.Wsdls.Count - 1 do with se.Wsdls.Objects[w] as TWsdl do
        begin
          for n := 0 to XsdDescr.ReadFileNames.Count - 1 do
            if not slFileNames.Find(XsdDescr.ReadFileNames.Strings[n], f) then
              slFileNames.Add(XsdDescr.ReadFileNames.Strings[n]);
        end;
        slNames := TJBStringList.Create;
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
              xXml.LoadFromFile(slFileNames.Strings[n], nil, nil);
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

procedure TMainForm.ShowGridDifferencesActionExecute (Sender : TObject );
var
  fNode, nNode: PVirtualNode;
  fData, nData: PMessageTreeRec;
  fMessage, nMessage: TWsdlMessage;
  fXml, nXml: TXml;
  xA2B: TA2BXml;
  xForm: TShowA2BXmlForm;
begin
  if FocusedOperation.DescriptionType in [ipmDTFreeFormat] then
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
            with AddXml (TXml.CreateAsString(FocusedOperation.reqTagName, '')) do
              CopyDownLine(fMessage.reqBind as TXml, True);
          if (fMessage.reqBind is TIpmItem) then
            AddXml((fMessage.reqBind as TIpmItem).AsXml);
        end;
        with AddXml (TXml.CreateAsString('Rpy', '')) do
        begin
          if fMessage.rpyBind is TXml then
            with AddXml (TXml.CreateAsString(FocusedOperation.rpyTagName, '')) do
              CopyDownLine(fMessage.rpyBind as TXml, True);
          if (fMessage.rpyBind is TIpmItem) then
            AddXml((fMessage.rpyBind as TIpmItem).AsXml);
        end;
        a2bExpandWhenValueIsJsonOrYaml(fXml);
      end;
      nXml := TXml.CreateAsString('compare', '');
      with nXml do
      begin
        AddXml (TXml.CreateAsString('name', nMessage.Name));
        with AddXml (TXml.CreateAsString('Req', '')) do
        begin
          if nMessage.reqBind is TXml then
            with AddXml (TXml.CreateAsString(FocusedOperation.reqTagName, '')) do
              CopyDownLine(nMessage.reqBind as TXml, True);
          if (nMessage.reqBind is TIpmItem) then
            AddXml((nMessage.reqBind as TIpmItem).AsXml);
        end;
        with AddXml (TXml.CreateAsString('Rpy', '')) do
        begin
          if nMessage.rpyBind is TXml then
            with AddXml (TXml.CreateAsString(FocusedOperation.rpyTagName, '')) do
              CopyDownLine(nMessage.rpyBind as TXml, True);
          if (nMessage.rpyBind is TIpmItem) then
            AddXml((nMessage.rpyBind as TIpmItem).AsXml);
        end;
        a2bExpandWhenValueIsJsonOrYaml(nXml);
      end;
    finally
    end;
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
  xOperation: TWsdlOperation;
begin
  if not Assigned(FocusedOperation) then
    raise Exception.Create('No operation selected');
  xXsd := operationOptionsXsd.XsdByCaption ['operationOptions.scripts.invoke.operations.name'];
  while xXsd.sType.Enumerations.Count > 0 do
  begin
    xXsd.sType.Enumerations.Objects[0].Free;
    xXsd.sType.Enumerations.Delete(0);
  end;
  for o := 0 to allAliasses.Count - 1 do
  begin
    if allAliasses.Operations[o] <> FocusedOperation then
    begin
      xEnum := TXsdEnumeration.Create;
      xEnum.Value := allAliasses.Operations[o].Alias;
      xXsd.sType.Enumerations.AddObject(xEnum.Value, xEnum);
    end;
  end;
  xOperation := FocusedOperation;
  with FocusedOperation do
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
                         , True
                         ) then
      begin
        AcquireLock;
        try
          stubChanged := True;
          OptionsFromXml(xXml);
          BeginConsoleUpdate;
          TProcedureThread.Create(False, False, se, se.PrepareAllOperationsShowingProgress);
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
  OperationOptionsAction.Enabled := Assigned (FocusedOperation);
end;

procedure TMainForm.OptionsFromXml(aXml: TXml);
var
  xXml, yXml: TXml;
  xWasActive: Boolean;
begin
  if not Assigned(aXml) then
    exit;
  xWasActive := se.IsActive;
  if se.IsActive then
  begin
    se.Activate(False);
    CheckBoxClick(nil)
  end;
  doStartOnOpeningProject := True;
  xmlUtil.doConfirmRemovals := True;
  doConfirmTemporaryInactivity := False;
  xmlUtil.doCollapseOnUncheck := True;
  xmlUtil.doExpandOnCheck := True;
  doScrollExceptionsIntoView := False;
  // se.HTTPServer.KeepAlive := True;
  se.HTTPServer.ListenQueue := 15;
  se.HTTPServer.MaxConnections := 15;
  se.doViaProxyServer := False;
  se.ViaProxyServer := '';
  se.ViaProxyPort := 8081;
  xsdValidateAssignmentsAgainstSchema := False;
  CollapseHeaders := False;
  xmlSetDefaultColors;

  if not aXml.Checked then
    exit;
  with aXml.Items do
  begin
    xXml := XmlCheckedItemByTag['General'];
    if Assigned(xXml) then
    begin
      doStartOnOpeningProject := xXml.Items.XmlCheckedBooleanByTagDef ['StartAfterOpeningProject', doStartOnOpeningProject];
      xmlUtil.doConfirmRemovals := xXml.Items.XmlCheckedBooleanByTagDef ['ConfirmRemovals', xmlUtil.doConfirmRemovals];
      doConfirmTemporaryInactivity := False;
      doScrollExceptionsIntoView := xXml.Items.XmlCheckedBooleanByTagDef ['ScrollExceptionsIntoView', doScrollExceptionsIntoView];
      xsdValidateAssignmentsAgainstSchema :=
        xXml.Items.XmlCheckedBooleanByTagDef['CheckScriptAssignments',
        xsdValidateAssignmentsAgainstSchema];
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
    xXml := XmlCheckedItemByTag['Colors'];
    if Assigned(xXml) then
    begin
      yXml := xXml.Items.XmlCheckedItemByTag['Xml'];
      if Assigned(yXml) then
        with yXml.Items do
        begin
          bgCorrelationItemColor :=
            HtmlToColor (XmlCheckedValueByTagDef['CorrelationValues', ColorToHtml(bgCorrelationItemColor)]);
          bgRequestTagNameColumnColor :=
            HtmlToColor (XmlCheckedValueByTagDef['RequestTagNameColumn', ColorToHtml(bgRequestTagNameColumnColor)]);
          bgNilValueColor :=
            HtmlToColor (XmlCheckedValueByTagDef['UnassignedValues', ColorToHtml(bgNilValueColor)]);
          bgElementValueColor :=
            HtmlToColor (XmlCheckedValueByTagDef['ElementValues', ColorToHtml(bgElementValueColor)]);
        end;
    end;
  end;
  if xWasActive then
  begin
    se.Activate(True);
    CheckBoxClick(nil)
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

procedure TMainForm .TreeViewAfterCellPaint (Sender : TBaseVirtualTree ;
  TargetCanvas : TCanvas ; Node : PVirtualNode ; Column : TColumnIndex ;
  const CellRect : TRect );
var
  IntrospectDesignAction: TRect;
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
        IntrospectDesignAction := Sender.GetDisplayRect(Node, Column, true);
        TreeviewImageList.Draw(TargetCanvas, IntrospectDesignAction.Right - 16, CellRect.Top, 31);
      end;
    end;
  end;
end;

procedure TMainForm.LoadTestActionExecute (Sender : TObject );
var
  x: Integer;
begin
  if not ActiveAfterPrompt then exit;
  ShowKindOfInformation := spMessages;
  Application.CreateForm(TStressTestForm, StressTestForm);
  try
    StressTestForm.Caption := 'Loadtest operation: ' + FocusedOperation.Name;
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
        Assigned(FocusedOperation)
    and (FocusedOperation.StubAction = saRequest)
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

procedure TMainForm .OperationRefreshMenuItemClick (Sender : TObject );
var
  xOperation: TWsdlOperation;
begin
  xOperation := FocusedOperation;
  FillNvgtView(allAliasses);
  FocusedOperation := xOperation;
end;

procedure TMainForm .MenuItem17Click (Sender : TObject );
begin
  if Assigned (FocusedOperation) then
  begin
    FocusedOperation.HiddenFromUI := True;
    UpdateVisibiltyOfOperations;
    stubChanged := True;
  end;
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
begin
  if not InactiveAfterPrompt then Exit;
  if OkToOpenStubCase then
  begin
    Application.CreateForm(TSelectProjectFolderForm, SelectProjectFolderForm);
    with SelectProjectFolderForm do
    try
      Caption := 'Select service virtualisation project to open (a folder with a name ending on '
               + _ProjectFileExtention
               + ')';
      FolderName := se.projectFileName;
      ShowModal;
      if ModalResult = mrOK then
      begin
        if ExtractFileExt(FolderName) <> _ProjectFileExtention then
          raise Exception.CreateFmt ( 'Illegal filename "%s"%sMust end with "%s"'
                                    , [FolderName, LineEnding, _ProjectFileExtention]
                                    );
        if not LazFileUtils.DirectoryExistsUTF8(FolderName) then
          raise Exception.CreateFmt ( 'Project "%s" not found'
                                    , [FolderName]
                                    );
        se.projectFileName := FolderName;
        OpenStubCase;
      end;
    finally
      SelectProjectFolderForm.Free;
    end;
  end;
end;

procedure TMainForm.NvgtViewClick(Sender: TObject);
begin
  if not Assigned (NvgtView.FocusedNode) then Exit;
  case NvgtView.FocusedColumn of
    Ord (operationsColumnBeforeScript): EditScriptButtonClick(nil);
    Ord (operationsColumnAfterScript): AfterRequestScriptButtonClick(nil);
  end;
end;

procedure TMainForm .NvgtViewGetImageIndex (
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

procedure TMainForm.IntrospectDesignActionExecute(Sender: TObject);
begin
  IntrospectDesign;
end;

procedure TMainForm.SaveProjectAsFolderActionExecute(Sender: TObject);
begin
  OnlyWhenLicensed;
  SelecFolderAndSave;
end;

procedure TMainForm.SaveStubCaseActionUpdate(Sender: TObject);
begin
  SaveStubCaseAction.Enabled := se.stubRead;
end;

procedure TMainForm.CheckFolderAndFileNames;
var
  w1, w0, s1, s0, o1, o0: Integer;
  wName, sName, oName, mName: String;
  xOk: Boolean;
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    Application.CreateForm(TPromptForm, PromptForm);
    try
      for w1 := 0 to se.Wsdls.Count - 1 do
      begin
        with se.Wsdls.Objects[w1] as TWsdl do
        begin
          if FileAlias = '' then FileAlias := Name;
          FileAlias:=xmlio.makeFileNameAllowed(FileAlias);
          PromptForm.PromptEdit.Text := FileAlias;
          xOk := xmlio.isFileNameAllowed(FileAlias);
          if xOk then
          begin
            for w0 := 0 to w1 - 1 do
            begin
              if UpperCase ((se.Wsdls.Objects[w0] as TWsdl).FileAlias) = UpperCase(FileAlias) then
              begin
                ShowMessage (Format ('"%s" duplicates name for %s', [FileAlias, (se.Wsdls.Objects[w0] as TWsdl).FileName]));
                xok := False;
              end;
            end;
          end;
          PromptForm.PromptEdit.Text := FileAlias;
          while not xOk do
          begin
            PromptForm.Caption := 'Name for Wsdl or OpenAPI: ' + FileName;
            PromptForm.Numeric := False;
            PromptForm.ShowModal;
            if PromptForm.ModalResult = mrCancel then
              raise Exception.Create('aborted by user');
            if PromptForm.ModalResult = mrOk then
            begin
              xOk := xmlio.isFileNameAllowed(PromptForm.PromptEdit.Text);
              if not xOk then
                ShowMessage (Format('"%s" invalid for filename', [PromptForm.PromptEdit.Text]))
              else
              begin
                FileAlias := PromptForm.PromptEdit.Text;
                for w0 := 0 to w1 - 1 do
                begin
                  if UpperCase ((se.Wsdls.Objects[w0] as TWsdl).FileAlias) = UpperCase(FileAlias) then
                  begin
                    ShowMessage (Format ('"%s" duplicates name for %s', [FileAlias, (se.Wsdls.Objects[w0] as TWsdl).FileName]));
                    Name := '';
                  end;
                end;
                stubChanged := stubChanged or xOk;
              end;
            end;
          end;
          wName := FileAlias;
          for s1 := 0 to Services.Count - 1 do
          begin
            with Services.Services[s1] do
            begin
              if FileAlias = '' then FileAlias := Name;
              FileAlias:=xmlio.makeFileNameAllowed(FileAlias);
              if Length (FileAlias) > 30 then
                FileAlias:=Copy (FileAlias, 1, 27) + IntToStr (s1);
              PromptForm.PromptEdit.Text := FileAlias;
              xOk := xmlio.isFileNameAllowed(FileAlias);
              while not xOk do
              begin
                PromptForm.Caption := 'Filename for Service: ' + wName + '/' + Name;
                PromptForm.Numeric := False;
                PromptForm.ShowModal;
                if PromptForm.ModalResult = mrCancel then
                  raise Exception.Create('aborted by user');
                if PromptForm.ModalResult = mrOk then
                begin
                  xOk := xmlio.isFileNameAllowed(PromptForm.PromptEdit.Text);
                  if not xOk then
                    ShowMessage (Format('"%s" invalid for filename', [PromptForm.PromptEdit.Text]))
                  else
                  begin
                    FileAlias := PromptForm.PromptEdit.Text;
                    for s0 := 0 to s1 - 1 do
                    begin
                      if UpperCase (Services.Services[s0].FileAlias) = UpperCase(FileAlias) then
                      begin
                        ShowMessage (Format ('"%s" duplicates name for %s', [Name, Services.Services[s0].Name]));
                        xOk := False;
                      end;
                    end;
                    stubChanged := stubChanged or xOk;
                  end;
                end;
              end;
              sName := FileAlias;
              for o1 := 0 to Operations.Count - 1 do
              begin
                with Operations.Operations[o1] do
                begin
                  if FileAlias = '' then FileAlias := Alias;
                  if FileAlias = '' then FileAlias := Name;
                  FileAlias:=xmlio.makeFileNameAllowed(FileAlias);
                  if Length (FileAlias) > 30 then
                    FileAlias:=Copy (FileAlias, 1, 27) + IntToStr (o1);
                  PromptForm.PromptEdit.Text := FileAlias;
                  xOk := xmlio.isFileNameAllowed(FileAlias);
                  while not xOk do
                  begin
                    PromptForm.Caption := 'Filename for Service: ' + wName + '/' + Name;
                    PromptForm.Numeric := False;
                    PromptForm.ShowModal;
                    if PromptForm.ModalResult = mrCancel then
                      raise Exception.Create('aborted by user');
                    if PromptForm.ModalResult = mrOk then
                    begin
                      xOk := xmlio.isFileNameAllowed(PromptForm.PromptEdit.Text);
                      if not xOk then
                        ShowMessage (Format('"%s" invalid for filename', [PromptForm.PromptEdit.Text]))
                      else
                      begin
                        FileAlias := PromptForm.PromptEdit.Text;
                        for o0 := 0 to o1 - 1 do
                        begin
                          if UpperCase (Operations.Operations[o0].FileAlias) = UpperCase(FileAlias) then
                          begin
                            ShowMessage (Format ('"%s" duplicates name for %s', [Name, Operations.Operations[o0].Name]));
                            xOk := False;
                          end;
                        end;
                        stubChanged := stubChanged or xOk;
                      end;
                    end;
                  end;
                  oName:=FileAlias;
                  { TODO : operation.messages.filenames.... }
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(PromptForm);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.ShowResolvedPropertiesExecute (Sender : TObject );
var
  x, r, c: Integer;
  xXml: TXml;
begin
  if Assigned (se.projectContexts)
  and (se.projectContexts.RowCount > 1) then
  begin
    xXml := TXml.CreateAsString('resolvedProperties', '');
    try
      for r := 1 to se.projectContexts.RowCount - 1 do
      begin
        if se.projectContexts.CellValue[0, r] = se.projectContext then
        begin
          with xXml.AddXml(TXml.CreateAsString('property', '')) do
          begin
            AddXml (TXml.CreateAsString ( 'key', '${context}'));
            AddXml (TXml.CreateAsString ( 'value', se.projectContext));
          end;
          for c := 1 to se.projectContexts.ColCount - 1 do
          begin
            with xXml.AddXml(TXml.CreateAsString('property', '')) do
            begin
              AddXml (TXml.CreateAsString ( 'key', '${' + se.projectContexts.CellValue[c, 0] + '}'));
              AddXml (TXml.CreateAsString ( 'value', se.projectContexts.CellValue[c, r]));
              AddXml (TXml.CreateAsString ( 'resolvesTo'
                                          , xmlio.resolveAliasses ( '${' + se.projectContexts.CellValue[c, 0] + '}'
                                                                  )
                                          )
                     );
            end;
          end;
        end;
      end;
      ShowXml('project Properties', xXml);
    finally
      xXml.Free;
    end;
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
                                 , xmlio.ReadStringFromFile (xReport.FileName, nil)
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

procedure TMainForm.GetSnapshotsFromFolder (aList: TSnapshotList; aFolder: String);
var
  x: Integer;
  xName: String;
  s: TRegressionSnapshot;
begin
  with FindAllFiles(aFolder, '*.xml', False) do
  try
    for x := 0 to Count - 1 do
    begin
      xName := LazFileUtils.ExtractFileNameOnly(Strings[x]);
      s := TRegressionSnapshot.Create ( xName
                                      , se.CurrentFolder + DirectorySeparator + xName + '.xml'
                                      , se.ReferenceFolder + DirectorySeparator + xName + '.xml'
                                      );
      s.OnReport := se.doRegressionReport;
      s.timeStamp := xmlio.GetFileChangedTime(s.FileName);
//    aList.SaveObject(xsdFormatDateTime(s.timeStamp, @TIMEZONE_UTC), s);
      aList.SaveObject(xName, s);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .SnapshotsFromFolderActionExecute (Sender : TObject );
var
  x: Integer;
  xName: String;
  sl: TSnapshotList;
  s: TSnapshot;
begin
  ClearSnapshotsActionExecute (nil);
  if se.displayedSnapshots.Count = 0 then
  begin
    sl := TSnapshotList.Create;
    try
      sl.Sorted := True;
      sl.Duplicates := dupAccept;
      GetSnapshotsFromFolder(sl, se.CurrentFolder);
      se.AcquireLogLock;
      try
        for x := 0 to sl.Count - 1 do
          se.toDisplaySnapshots.AddObject(sl.SnapshotItems[x].Name, sl.SnapshotItems[x]);
      finally
        se.ReleaseLogLock;
      end;
      sl.Clear;
    finally
      sl.Free;
    end;
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

procedure TMainForm.DocumentationViewerHotClick(Sender: TObject);
begin
  OpenUrl((Sender as TIpHtmlPanel).HotURL);
end;

procedure TMainForm.ApiByExampleActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not InactiveAfterPrompt then Exit;
  OperationDefsXsd.XsdByCaption ['OperationDefs.ApiByExampleOperations.Service.Operation.Annotation']
    .EditProcedure := EditXmlValueAsText;
  xXml := se.ApiByExampleOperationsXml('');
  try
    if EditXmlXsdBased ( 'API By Example Operations'
                       , 'OperationDefs.ApiByExampleOperations'
                       , 'ApiByExampleOperations.Operation.Name'
                       , 'ApiByExampleOperations.Operation.Name'
                       , se.IsActive
                       , xXml.Items.Count > 1
                       , esUsed
                       , OperationDefsXsd
                       , xXml
                       , True
                       ) then
    begin
      stubChanged := True;
      BeginConsoleUpdate;
      se.ApiByExampleOperationsUpdate(xXml, se.projectFileName);
      IntrospectDesign;
    end;
  finally
    xXml.Free;
  end;
end;

procedure TMainForm.ApiByExampleActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Maintain list of ApiByExample operations ' + HttpActiveHint;
end;

procedure TMainForm.FreeFormatsActionUpdate(Sender: TObject);
begin
  if Assigned (se) then
    FreeFormatsAction.Caption := decorateWithAsterix (FreeFormatsAction.Caption, se.hasFreeformatOperations);
end;

procedure TMainForm.GenerateFunctopnPrototypeListActionExecute(Sender: TObject);
begin
  ShowInfoForm( 'ScriptAssignments'
              , FocusedOperation.FunctionPrototypes(True).Text
              );
end;

procedure TMainForm.GenerateFunctopnPrototypeListActionUpdate(Sender: TObject);
begin
  GenerateFunctopnPrototypeListAction.Enabled := Assigned(FocusedOperation);
end;

procedure TMainForm.GenerateJsonSchemaInYamlExecute(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if (xBind is TXml) and (Assigned((xBind as TXml).Xsd)) then
  begin
    with (xBind as TXml).Xsd.SchemaAsJson as TXml do
    try
      Clipboard.AsText := StreamYAML(0, True);
    finally
      Free;
    end;
  end
  else
    ShowMessage ('Action only implemented for XMLs that have an Schema');
end;

procedure TMainForm.GenerateSwaggerActionExecute(Sender: TObject);
var
  xHost, xPath, xProtocol: String;
  o: Integer;
  xOper: TWsdlOperation;
begin
  if not Assigned (FocusedOperation) then
    raise Exception.Create('No service selected');
//  if (FocusedOperation.DescriptionType <> ipmDTWsdl) then
  //  raise Exception.Create('Only meant for soap services');
  with TXml.CreateAsString('swagger', '') do
  try
    AddXml(TXml.CreateAsString('swagger','2.0'));
    with AddXml(TXml.CreateAsString('info', '')) do
    begin
      AddXml(TXml.CreateAsString('description', ifthen(FocusedOperation.Wsdl.Description <> '',FocusedOperation.Wsdl.Description, 'your description here')));
      AddXml(TXml.CreateAsString('version', 'version'));
      AddXml(TXml.CreateAsString('title', 'your title here'));
    end;
    try
      with TIdURI.Create(FocusedOperation.SoapAddress) do
      try
        xHost := Host;
        xProtocol := Protocol;
      finally
        free;
      end;
    except
      xHost := 'unknownHost';
      xProtocol := 'http';
    end;
    AddXml(TXml.CreateAsString('host',xHost));
    with AddXml(TXml.CreateAsString('schemes','')) do
    begin
      jsonType := jsonArray;
      AddXml(TXml.CreateAsString('_', xProtocol));
    end;
    AddXml(TXml.CreateAsString('basePath', '/your/BasePath/here'));
    with AddXml(TXml.CreateAsString('paths','')) do
    begin
      for o := 0 to FocusedOperation.WsdlService.Operations.Count - 1 do
      begin
        xOper := FocusedOperation.WsdlService.Operations.Operations[o];
        try
          with TIdURI.Create(FocusedOperation.SoapAddress) do
          try
            xPath := Document;
          finally
            free;
          end;
        except
          xPath := '/unknownPath';
        end;
        if xPath = '' then
          xPath := xOper.Alias;
        if xOper.reqXml.Items.Count > 0 then
        begin
          with SeparatedStringList(nil, xOper.reqXml.Items.XmlItems[xOper.reqXml.Items.Count - 1].NameSpace, '/') do
          try
            if Count > 2 then
              xPath := Strings[Count - 3] + '/' + Strings[Count - 2];
          finally
            Free;
          end;
        end;
        with AddXml (TXml.CreateAsString('/' + xPath, '')) do
        begin
          with AddXml (TXml.CreateAsString('post', '')) do
          begin
            AddXml (TXml.CreateAsString('summary', 'summary'));
            AddXml (TXml.CreateAsString('description', xOper.Documentation.Text));
            AddXml (TXml.CreateAsString('operationId', xOper.Alias));
            with AddXml (TXml.CreateAsString('consumes', '')) do
            begin
              jsonType := jsonArray;
              AddXml(TXml.CreateAsString('_', 'application/json'))
            end;
            with AddXml (TXml.CreateAsString('produces', '')) do
            begin
              jsonType := jsonArray;
              AddXml(TXml.CreateAsString('_', 'application/json'))
            end;
            with AddXml (TXml.CreateAsString('parameters', '')) do
            begin
              jsonType := jsonArray;
              with AddXml(TXml.CreateAsString('_', '')) do
              begin
                AddXml (TXml.CreateAsString('name', 'rabobank-apikey'));
                AddXml (TXml.CreateAsString('in', 'header'));
                AddXml (TXml.CreateAsString('decsription', 'rabobank-apikey'));
                AddXml (TXml.CreateAsString('required', 'true'));
                AddXml (TXml.CreateAsString('type', 'string'));
              end;
              if xOper.reqXml.Items.Count > xOper.InputHeaders.Count then
              with AddXml(TXml.CreateAsString('_', '')) do
              begin
                AddXml (TXml.CreateAsString('name', 'body'));
                AddXml (TXml.CreateAsString('in', 'body'));
                AddXml (TXml.CreateAsString('decsription', xOper.Documentation.Text));
                AddXml (TXml.CreateAsString('required', 'true'));
                with AddXml (TXml.CreateAsString('schema', '')) do
                begin
                  AddXml (TXml.CreateAsString('$ref', '#/definitions/' + xOper.reqXml.Items.XmlItems[xOper.InputHeaders.Count].Xsd.ElementName));
                end;
              end;
            end;
            with AddXml (TXml.CreateAsString('responses', '')) do
            begin
              if xOper.rpyXml.Items.Count > xOper.OutputHeaders.Count then
              begin
                with AddXml(TXml.CreateAsString('200', '')) do
                begin
                  AddXml (TXml.CreateAsString('decsription', 'Success response'));
                  with AddXml (TXml.CreateAsString('schema', '')) do
                  begin
                    AddXml (TXml.CreateAsString('$ref', '#/definitions/' + xOper.rpyXml.Items.XmlItems[xOper.OutputHeaders.Count].Xsd.ElementName));
                  end;
                end;
              end;
              if  (Assigned (xOper.FaultXsd))
              and (Assigned (xOper.FaultXsd.sType))
              and (xOper.FaultXsd.sType.ElementDefs.Count > 0) then
              begin
                with AddXml(TXml.CreateAsString('500', '')) do
                begin
                  AddXml (TXml.CreateAsString('decsription', 'Fault response'));
                  with AddXml (TXml.CreateAsString('schema', '')) do
                  begin
                    AddXml (TXml.CreateAsString('$ref', '#/definitions/' + xOper.FaultXsd.sType.ElementDefs.Xsds[0].ElementName));
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    with AddXml(TXml.CreateAsString('definitions','')) do
    begin
      for o := 0 to FocusedOperation.WsdlService.Operations.Count - 1 do
      begin
        xOper := FocusedOperation.WsdlService.Operations.Operations[o];
        if xOper.reqXml.Items.Count > xOper.InputHeaders.Count then
          AddXml (xOper.reqXml.Items.XmlItems[xOper.InputHeaders.Count].Xsd.SchemaAsJson as TXml);
        if xOper.rpyXml.Items.Count > xOper.OutputHeaders.Count then
          AddXml (xOper.rpyXml.Items.XmlItems[xOper.OutputHeaders.Count].Xsd.SchemaAsJson as TXml);
        if (Assigned (xOper.FaultXsd))
        and (Assigned (xOper.FaultXsd.sType))
        and (xOper.FaultXsd.sType.ElementDefs.Count > 0) then
          AddXml (xOper.FaultXsd.sType.ElementDefs.Xsds[0].SchemaAsJson as TXml);
      end;
    end;
//  Clipboard.AsText := StreamJSON(0, True)
    Clipboard.AsText := StreamYAML(0, True)
  finally
    free;
  end;
end;

procedure TMainForm.LogsFromHttpGetActionExecute(Sender: TObject);
begin
  TProcedureThread.Create(False, True, se, se.LogsFromRemoteServer);
end;

procedure TMainForm.LogsFromHttpGetActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  HintStr := 'Append logs from remote apiServer.';
  if not LogsFromHttpGetAction.Enabled then
    HintStr := HintStr + ' (remote apiServer connection not yet specified, see menu Project.)';
end;

procedure TMainForm.LogsFromHttpGetActionUpdate(Sender: TObject);
begin
  LogsFromHttpGetAction.Enabled := Assigned (se) and se.remoteServerConnectionEnabled;
end;

procedure TMainForm .EditMessageScriptActionExecute (Sender : TObject );
var
  xOperation: TWsdlOperation;
  xScriptName: String;
begin
  if not Assigned(FocusedOperation) then
    Raise Exception.Create('First get a Wsdl');
  if not Assigned (FocusedMessage) then Exit;
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(FocusedOperation);
    if xOperation.StubAction = saStub then
      xScriptName := Format('%s / %s  / Main Script', [FocusedOperation.Alias, FocusedMessage.Name])
    else
      xScriptName := Format('%s / %s  / Before Script', [FocusedOperation.Alias, FocusedMessage.Name]);
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xScriptName;
        EditOperationScriptForm.After := False;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ScriptEdit.Lines.Text := FocusedMessage.BeforeScriptLines.Text;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          FocusedMessage.BeforeScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try FocusedMessage.CheckBefore; Except end;
          try FocusedMessage.CheckAfter; Except end;
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
  if not Assigned(FocusedOperation) then
    Raise Exception.Create('First get a Wsdl');
  if not Assigned (FocusedMessage) then Exit;
  XmlUtil.PushCursor (crHourGlass);
  try
    xOperation := TWsdlOperation.Create(FocusedOperation);
    if xOperation.StubAction = saStub then
      xScriptName := Format('%s / %s  / Main Script', [FocusedOperation.Alias, FocusedMessage.Name])
    else
      xScriptName := Format('%s / %s  / After Script', [FocusedOperation.Alias, FocusedMessage.Name]);
    try
      Application.CreateForm(TEditOperationScriptForm, EditOperationScriptForm);
      try
        EditOperationScriptForm.ScriptName := xScriptName;
        EditOperationScriptForm.After := True;
        EditOperationScriptForm.WsdlOperation := xOperation;
        EditOperationScriptForm.ScriptEdit.Lines.Text := FocusedMessage.AfterScriptLines.Text;
        EditOperationScriptForm.ShowModal;
        if EditOperationScriptForm.ModalResult = mrOk then
        begin
          stubChanged := True;
          FocusedMessage.AfterScriptLines.Text := EditOperationScriptForm.ScriptEdit.Lines.Text;
          try FocusedMessage.CheckBefore; Except end;
          try FocusedMessage.CheckAfter; Except end;
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

procedure TMainForm.AddChildElementMenuItemClick(Sender: TObject);
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
  x, m, f: Integer;
  xXml, dxml: TXml;
  xBind: TCustomBindable;
  nTypeDef, oTypeDef, cTypeDef: TXsdDataType;
  xWsdl: TWsdl;
  cXsd, xXsd: TXsd;
  xPath: String;
begin
  xBind := NodeToBind(TreeView, TreeView.FocusedNode);
  if not Assigned(xBind) then
    raise Exception.Create('no element selected');
  if not(xBind is TXml) then
    raise Exception.Create('operation only valid on XML elements');
  xXml := xBind as TXml;
  if not Assigned(xXml.Xsd) then
    raise Exception.Create('opeation requires an XSD on the selected element');

  Application.CreateForm(TQueryNewElementForm, QueryNewElementForm);
  with QueryNewElementForm do
  try
    ShowModal;
    if ModalResult = mrOK then
    begin
      xWsdl := FocusedOperation.Wsdl;
      if not xWsdl.ExtraXsds.Find(FileName, f) then
      begin
        xWsdl.ExtraXsds.Add (FileName);
        xWsdl.LoadExtraXsds (se.OnBeforeFileRead);
      end;
      if ElementOrTypeDefRef = etElementRef then
      begin
        cXsd := Wsdl.XsdDescr.FindElement(Namespace, Name);
        if not Assigned (cXsd) then
          raise Exception.Create ('procedure TMainForm.AddChildElementRefMenuItemClick(Sender: TObject): cXsd not assigned');
        cTypeDef := cXsd.sType;
        if not Assigned (cTypeDef) then
          raise Exception.Create ('procedure TMainForm.AddChildElementRefMenuItemClick(Sender: TObject): cTypeDef not assigned');
      end
      else
      begin
        cTypeDef := Wsdl.XsdDescr.FindTypeDef(Namespace, Name);
        if not Assigned (cTypeDef) then
          raise Exception.Create ('procedure TMainForm.AddChildElementRefMenuItemClick(Sender: TObject): cTypeDef not assigned');
      end;
      oTypeDef := xXml.Xsd.sType;
      xXsd := xXml.Xsd.AddElementDef ( Wsdl.XsdDescr
                                     , TagName
                                     , cTypeDef
                                     );
      xXsd._RefNameSpace := Namespace;
      xXsd._RefElementName := Name;
      xXsd._ElementOrTypeDefRef := ElementOrTypeDefRef;
      nTypeDef := xXml.Xsd.sType;
      xPath := IfThen(FocusedMessage.reqBind.IsAncestorOf(xXml), 'Req.', 'Rpy.')
             + xXml.FullCaption
             ;
      if not oTypeDef.Manually then
        FocusedOperation.BindablesWithAddedElement.AddObject (xPath, FocusedOperation.FindBind(xPath));
      xXml.Checked := True;
      _updateTypedef ( FocusedOperation
                     , xPath
                     , nTypeDef
                     , xxsd
                     );
      for m := 0 to FocusedOperation.Messages.Count - 1 do
      begin
        _updateTypedef ( FocusedOperation.Messages.Messages[m]
                       , xPath
                       , nTypeDef
                       , xxsd
                       );
      end;
      GridView.OnFocusChanged(GridView, GridView.FocusedNode, GridView.FocusedColumn);
      stubChanged := True;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.CheckReferencedFilenamesExistsInCloud;
var
  x: Integer;
  s, xFileName, xRelativeFilename: String;
begin
  s := '';
  try
    try
      if Assigned (se)
      and Assigned (se.referencedFilenames) then
      begin
        s := s + xmlio.osDirectorySeparators(se.projectFileName) + LineEnding;
        SjowMessage(se.projectFileName + LineEnding + se.referencedFilenames.Text);
        for x := 0 to se.referencedFilenames.Count - 1 do
        begin
          xFileName := se.referencedFilenames.Strings[x];
          if (AnsiStartsText('HTTP://', xFileName))
          or (AnsiStartsText('HTTPS://', xFileName)) then
          begin
            if not xmlio.urlexists(xFileName) then
              raise Exception.CreateFmt('URL not found/reached: %', [xFileName]);
          end
          else
          begin
            s := s + ExtractRelativeFileName ( xmlio.osDirectorySeparators(se.projectFileName)
                                             , xmlio.osDirectorySeparators (xFileName)
                                             )
                   + LineEnding
                   ;
            ReadStringFromFile ( ExtractRelativeFileName ( xmlio.osDirectorySeparators(se.projectFileName)
                                                         , xmlio.osDirectorySeparators (xFileName)
                                                         )
                               , nil
                               );
//          if not FileExistsUTF8(xmlio.osDirectorySeparators(xFileName)) then
//            raise Exception.CreateFmt('File not found: %', [osDirectorySeparators(xFileName)]);
          end;
        end;
        ShowMessage('All referenced files/urls found (Local/Absolute)');
      end;
    except
      on e: Exception do
        Raise Exception.CreateFmt ('Not all referenced diskfiles found (Local/Absolute)%s%s', [LineEnding, e.Message])
    end;
  finally
    SjowMessage(s);
  end;
end;

procedure TMainForm.SnapshotFromRemoteServer (aList: TClaimableObjectList);
var
  x: Integer;
begin
  with aList as TSnapshotList do
  for x := 0 to Count - 1 do
    GetSnapshotFromRemoteServer (SnapshotItems[x]);
end;

procedure TMainForm.CheckReferencedFilesExistInCloudActionExecute(Sender: TObject);
begin
  CheckReferencedFilenamesExistsInCloud;
end;

procedure TMainForm.PasteProjectFromClipboardActionExecute(Sender: TObject);
begin
  if EditRemoteServerConnectionParams('Remote apiUi server connection') then
  begin
    BeginConsoleUpdate;
    captionFileName := se.RemoteServerUrl;
    TProcedureThread.Create(False, False, se, se.OpenProjectFromString, Clipboard.AsText);
  end;
end;

procedure TMainForm.CloudProjectInformationActionUpdate(Sender: TObject);
begin
  CloudProjectInformationAction.Enabled := Assigned (se) and se.remoteServerConnectionEnabled;
end;

procedure TMainForm.EditCloudEnvironmentActionExecute(Sender: TObject);
var
  xXml, eXml: TXml;
begin
  if not Assigned (se) then
    raise Exception.Create('Edit RemoteServer Environment requires an assigned Project');
  if not Assigned (se.remoteServerConnectionXml) then
    raise Exception.Create('Edit RemoteServer Environment requires a Remote Server Connection');
  xXml := TXml.Create;
  try
    try
      xXml.LoadJsonFromString ( xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                                                        , '/apiUi/api/envvars'
                                                        , ''
                                                        , 'GET'
                                                        , 'application/json'
                                                        )
                          , nil
                          );
      eXml := TXml.Create(-1000, namevaluepairsXsd);
      eXml.Name := xXml.Name;
      eXml.LoadValues(xXml, False);
      try
        if XmlUtil.editXml(eXml, True, False) then
          xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                                  , '/apiUi/api/envvars'
                                  , ''
                                  , 'POST'
                                  , 'application/json'
                                  , eXml.StreamJSON(0, True)
                                  )
      finally
        FreeAndNil(xXml);
        FreeAndNil(eXml);
      end;
    except
      on e: exception do
        ShowMessage(e.Message);
    end;
  finally
    FreeAndNil(xXml);
  end;
end;

procedure TMainForm.NavigateHierarchyActionExecute(Sender: TObject);
var
  w, s, o: Integer;
  wMenuItem, sMenuItem, oMenuItem: TMenuItem;
begin
  while NavigateOperationsPopupMenu.Items.Count > 0 do
    NavigateOperationsPopupMenu.Items.Delete(0);
  for w := 0 to se.Wsdls.Count - 1 do with se.Wsdls.Objects[w] as TWsdl do
  begin
    if Services.Count > 0 then
    begin
      wMenuItem := TMenuItem.Create(Self);
      if FileName = '' then
        wMenuItem.Caption := FileAlias
      else
        wMenuItem.Caption := LazFileUtils.ExtractFileNameOnly(FileName);
      NavigateOperationsPopupMenu.Items.Add(wMenuItem);
      for s := 0 to Services.Count - 1 do with Services.Services[s] do
      begin
        if not se.isSpecialWsdl(thisWsdl) then
        begin
          sMenuItem := TMenuItem.Create(Self);
          sMenuItem.Caption := Name;
          wMenuItem.Add(sMenuItem);
        end
        else
        begin
          sMenuItem := wMenuItem;
          sMenuItem.Caption := thisWsdl.Name;
        end;
        for o := 0 to Operations.Count - 1 do with Operations.Operations[o] do
        begin
          oMenuItem := TMenuItem.Create(Self);
          oMenuItem.Caption := Name;
          sMenuItem.Add(oMenuItem);
          oMenuItem.OnClick := FocusOnOperationMenuItemClick;
          oMenuItem.Tag := PtrInt(thisOperation);
        end;
      end;
    end;
  end;
  NavigateOperationsPopupMenu.PopUp;
end;

procedure TMainForm.NavigateHierarchyActionUpdate(Sender: TObject);
begin
  NavigateHierarchyAction.Enabled := (Assigned (se)) and (se.Wsdls.Count > 0);
end;

procedure TMainForm.DocumentationViewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Handled := OpenURL(SRC);
end;

procedure TMainForm.HtmlViewerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = Word('C')) and (Shift = [ssCtrl]) then
    (Sender as THtmlViewer).CopyToClipboard;
end;

procedure TMainForm.SQLConnectorLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
var
  s: String;
begin
  if not DebugLogMode then Exit;
  case EventType of
    detCustom: s := 'detCustom';
    detPrepare: s := 'detPrepare';
    detExecute: s := 'detExecute';
    detFetch: s := 'detFetch';
    detCommit: s := 'detCommit';
    detRollBack: s := 'detRollBack';
    detParamValue: s := 'detParamValue';
    detActualSQL: s := 'detActualSQL';
  end;
  try SjowMessage(s + ': ' + Msg); except end;
end;

procedure TMainForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TMainForm.FocusOnOperationMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then with Sender as TMenuItem do
    FocusedOperation := TWsdlOperation(tag);
end;

procedure TMainForm.MenuItem14Click(Sender: TObject);
begin
    if not (FocusedBind is TXml) then
      raise Exception.Create('Only implemented for Xml');
    with (FocusedBind as TXml).Xsd.SchemaAsJson as TXml do
    try
      ShowInfoForm('YAML schema', thisXml.StreamYAML(0, True));
    finally
      Free;
    end;
end;

procedure TMainForm.EditCloudEnvironmentActionUpdate(Sender: TObject);
begin
  EditCloudEnvironmentAction.Enabled := Assigned (se) and se.remoteServerConnectionEnabled;
end;

procedure TMainForm.AboutApiServerActionUpdate(Sender: TObject);
begin
  AboutApiServerAction.Enabled := Assigned (se) and se.remoteServerConnectionEnabled;
end;

procedure TMainForm.AboutApiServerActionExecute(Sender: TObject);
begin
  if not Assigned (se) then
    raise Exception.Create('ProjectInfoFromRemoteServer requires an assigned Project');
  if not Assigned (se.remoteServerConnectionXml) then
    raise Exception.Create('ProjectInfoFromRemoteServer requires a Remote Server Connection');
  with TXml.Create do
  try
    try
      LoadJsonFromString ( xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                                                   , '/apiUi/api/about'
                                                   , ''
                                                   , 'GET'
                                                   , 'application/json'
                                                   )
                          , nil
                          );
      Name := 'about';
      ShowXmlExtended('about apiUi in the cloud', thisXml);
    except
      on e: exception do
        ShowMessage(e.Message);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.MessagesVTSColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TMainForm.MessagesVTSHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  XmlUtil.PushCursor (crHourGlass);
  try
    if Sender.SortColumn = HitInfo.Column then
    begin
      if Sender.SortDirection = sdAscending then
        Sender.SortDirection := sdDescending
      else
        Sender.SortDirection := sdAscending;
    end
    else
    begin
      Sender.SortColumn := HitInfo.Column;
      Sender.SortDirection := sdAscending;
    end;
    with Sender.Treeview do
    begin
      SortTree(HitInfo.Column, Sender.SortDirection, True);
      ScrollIntoView(FocusedNode, False, False);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.setIpmDescrType(Value: TIpmDescrType);
begin
  if fIpmDescrType = Value then
    Exit;
  fIpmDescrType := Value;
  if fIpmDescrType = ipmDTFreeFormat then
  begin
    TreeView.Align := alLeft;
    TreeView.Visible := False;
    FreeFormatMemo.Align := alClient;
    FreeFormatMemo.Visible := True;
  end
  else
  begin
    FreeFormatMemo.Align := alRight;
    FreeFormatMemo.Visible := False;
    TreeView.Align := alClient;
    TreeView.Visible := True;
  end;
end;

procedure TMainForm.setShowKindOfInformation(Value: TShowKindOfInformation);
var
  saveOnEvent: TNotifyEvent;
begin
  if fShowKindOfInformation = Value then Exit;
  fShowKindOfInformation := Value;
  saveOnEvent := LogTabControl.OnChange;
  try
    LogTabControl.TabIndex := Ord (Value);
    NotificationsPanel.Visible := False;
    SnapshotsPanel.Visible := False;
    MessagesPanel.Visible := False;
    case LogTabControl.TabIndex of
      Ord (spNotifications):
      begin
        LogTabControl.Tabs [Ord (spNotifications)] := notifyTabCaption;
        NotificationsPanel.Visible := True;
      end;
      Ord (spSnapshots): SnapshotsPanel.Visible := True;
      Ord (spMessages):
      begin
        MessagesPanel.Visible := True;
        PositionMessagesTabControl;
      end;
    end;
  finally
    LogTabControl.OnChange:=saveOnEvent;
  end;
end;

procedure TMainForm.setShowKindOfLogData(Value: TShowKindOfLogData);
var
  saveOnEvent: TNotifyEvent;
begin
  if fShowKindOfLogData = Value then Exit;
  fShowKindOfLogData := Value;
  saveOnEvent := MessagesTabControl.OnChange;
  try
    MessagesTabControl.TabIndex := Ord (Value);
    UpdateLogTabs (NodeToMsgLog (False, MessagesVTS, MessagesVTS.FocusedNode));
  finally
    MessagesTabControl.OnChange := saveOnEvent;
  end;
end;

procedure TMainForm.ShowFocusedBindDocumentation;
var
  xObject: TObject;
begin
  xObject := FocusedBind;
  if Assigned (FocusedBind) then
  begin
    if FocusedBind is TXml then with FocusedBind as Txml do
      xObject := Xsd
    else
      if FocusedBind is TXmlAttribute then with FocusedBind as TXmlAttribute do
        xObject := XsdAttr;
  end;
  if xObject = fFocusedDocumentationObject then Exit;
  fFocusedDocumentationObject := xObject;
  xmlUtil.ListXsdProperties(InWsdlPropertiesListView, FocusedBind);
  try
    xmlUtil.ListXsdDocumentation(DocumentationViewer, FocusedBind, False, False);
  except
  end;
end;

procedure TMainForm.setFocusedBind(Value: TCustomBindable);
begin
  if fFocusedBind = Value then Exit;
  fFocusedBind := Value;
  if fFocusedBind = nil then
  begin
    InWsdlPropertiesListView.Clear;
    DocumentationViewer.LoadFromString('');
    StatusPanel.Caption := '';
    Exit;
  end;
  DisableViewOnFocusChangeEvents;
  try
    FocusedOperation.LastFullCaption := FocusedBind.FullCaption;
    SelectFocusedBindInViews;
    ShowFocusedBindDocumentation;
    if FocusedBind is TIpmItem then
      StatusPanel.Caption := '[' + IntToStr((FocusedBind as TIpmItem).Offset + 1)
        + ':' + IntToStr((FocusedBind as TIpmItem).Bytes) + '] ' + FocusedBind.FullIndexCaption
    else
      StatusPanel.Caption := FocusedBind.FullCaption;
  finally
    EnableViewOnFocusChangeEvents;
  end;
end;

procedure TMainForm.FocusNavigatorOnOperation;
var
  xNode: PVirtualNode;
begin
  with NvgtView do
  begin
    xNode := GetFirst;
    while Assigned(xNode) do
    begin
      if NodeToOperation(NvgtView, xNode) = FocusedOperation then
      begin
        FocusedNode := xNode;
        Selected[FocusedNode] := True;
      end;
      xNode := GetNext(xNode);
    end;
  end;
end;

procedure TMainForm.CloudProjectInformationActionExecute(Sender: TObject);
begin
  ProjectInfoFromRemoteServer;
end;

procedure TMainForm.ApiByExampleActionUpdate(Sender: TObject);
begin
  if Assigned (se) then
    ApiByExampleAction.Caption := decorateWithAsterix (ApiByExampleAction.Caption, se.hasApiByExplampleOperations);
end;

procedure TMainForm.CobolOperationsActionUpdate(Sender: TObject);
begin
  if Assigned (se) then
    CobolOperationsAction.Caption := decorateWithAsterix (CobolOperationsAction.Caption, se.hasCobolOperations);
end;

procedure TMainForm.ContextsActionExecute(Sender: TObject);
var
  c, r: Integer;
begin
  Application.CreateForm(TEditContextsForm, EditContextsForm);
  with EditContextsForm do
  try
    ContextComboBox.Text := se.projectContext;
    Contexts := TStringListList.Create(se.projectContexts);
    try
      ShowModal;
      if (ModalResult = mrOK) then
      with Contexts do
      begin
        se.projectContexts.RowCount := RowCount;
        se.projectContexts.ColCount := ColCount;
        for r := 0 to RowCount - 1 do
          for c := 0 to ColCount - 1 do
          begin
            se.projectContexts.CellValue[c, r] := CellValue[c, r];
            se.projectContexts.CellObject[c, r] := CellObject[c, r];
          end;
        stubChanged := stubChanged or isChanged;
        if ContextComboBox.Text <> se.projectContext then
        begin
          se.projectContext := ContextComboBox.Text;
          stubChanged := True;
        end;
      end;
    finally
      Contexts.Free;
    end;
  finally
    EditContextsForm.Free;
  end;
end;

procedure TMainForm.CopyRemoteApiUiProjectActionExecute(Sender: TObject);
begin
  if EditRemoteServerConnectionParams('Remote apiUi server connection') then
  begin
    BeginConsoleUpdate;
    captionFileName := se.RemoteServerUrl;
    TProcedureThread.Create(False, False, se, se.OpenFromServerUrl);
  end;
end;

procedure TMainForm.CopyToClipboardAsJsonMenuItemClick(Sender: TObject);
begin
  xmlUtil.CopyToClipboard(tlsJson, NodeToBind(TreeView,
      TreeView.FocusedNode));
end;

procedure TMainForm.EditMessageAfterScriptActionUpdate (Sender : TObject );
begin
  if Assigned (FocusedOperation) then
    EditMessageAfterScriptAction.Enabled := (FocusedOperation.StubAction <> saStub);
end;

procedure TMainForm.EditMessageDocumentationActionExecute(Sender: TObject);
begin
  if not Assigned(FocusedOperation) then Exit;
  if not Assigned (FocusedMessage) then Exit;
  XmlUtil.PushCursor (crHourGlass);
  try
    with EditTexttForm do
    try
      Application.CreateForm(TEditTexttForm, EditTexttForm);
      Caption := Format('%s / %s  / Documentation', [FocusedOperation.Alias, FocusedMessage.Name]);
      ScriptEdit.Lines.Text := FocusedMessage.Documentation;
      ShowModal;
      if (ModalResult = mrOk)
      and (ScriptEdit.Lines.Text <> FocusedMessage.Documentation)
      then
      begin
        stubChanged := True;
        FocusedMessage.Documentation := ScriptEdit.Lines.Text;
        FocusedMessage.DocumentationEdited := True;
      end;
      FillInWsdlEdits;
    finally
      FreeAndNil(EditTexttForm);
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.LogTabControlChange(Sender: TObject);
begin
  ShowKindOfInformation := TShowKindOfInformation (LogTabControl.TabIndex);
end;

procedure TMainForm.MenuItem33Click(Sender: TObject);
begin
   Clipboard.AsText := NodeToBind(TreeView, TreeView.FocusedNode).Name;
end;

procedure TMainForm.MenuItem34Click(Sender: TObject);
begin
  Clipboard.AsText := NodeToBind(TreeView, TreeView.FocusedNode).FullCaption;
end;

procedure TMainForm.MenuItem43Click(Sender: TObject);
begin
  PromptAndSetColumnWidth(TreeView);
end;

procedure TMainForm.MenuItem45Click(Sender: TObject);
begin
  PromptAndSetColumnWidth(MessagesVTS);
end;

procedure TMainForm.MenuItem47Click(Sender: TObject);
begin
  PromptAndSetColumnWidth(SnapshotsVTS);
end;

procedure TMainForm.MenuItem57Click(Sender: TObject);
begin
  OpenURL(apiuiconsts.apiuiSnapshots);
end;

procedure TMainForm.MenuItem58Click(Sender: TObject);
begin
  ShowHelpDocumentation('Grid_Popup_Menu');
end;

procedure TMainForm.MenuItem60Click(Sender: TObject);
begin
  OpenURL(apiuiconsts.apiuiMessageTreeview);
end;

procedure TMainForm.MenuItem61Click(Sender: TObject);
begin
  ShowHelpDocumentation('Log_Popup_Menu');
end;

procedure TMainForm.OperationsPopupHelpItemClick(Sender: TObject);
begin
  OpenURL (apiuiconsts.apiuiOperationContextMenu);
end;

procedure TMainForm.GetSnapshotsFromRemoteServer (slx, sln, slc: TSnapshotList);
var
  x, f: Integer;
  xXml, dXml: TXml;
  snapshot: TSnapshot;
  xName, xUrl: String;
  xDateTime: TDateTime;
begin
  xXml := TXml.Create;
  xUrl := se.remoteServerConnectionXml.items.XmlValueByTag['Address'];
  try
    xXml.LoadJsonFromString ( xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                                                      , '/apiUi/api/snapshots'
                                                      , ''
                                                      , 'GET'
                                                      , 'application/json'
                                                      )
                            , nil
                            );
    dXml := xXml.FindXml('json.snapshots');
    if not Assigned (dXml) then
      raise Exception.Create('unexpected result from get:' + xUrl + '/apiUi/api/snapshots');
    for x := 0 to dXml.Items.Count - 1 do
    with dXml.Items.XmlItems[x] do
    begin
      xName := Items.XmlValueByTag['name'];
      if slx.Find (xName, f) then
      begin
        snapshot := slx.SnapshotItems[f];
        xDateTime := XmlToDateTime(Items.XmlValueByTag['createdOn']);
        if xDateTime > snapshot.timeStamp then
        begin
          slc.SaveObject (Items.XmlValueByTag['createdOn'], snapshot);
          xmlio.apiUiServerDownload ( se.remoteServerConnectionXml
                                    , '/apiUi/api/snapshots/download/' + urlPercentEncode(xName)
                                    , snapshot.FileName
                                    );
          xmlio.SetFileChangedTime (snapshot.FileName, xDateTime);
        end;
      end
      else
      begin
        snapshot := TRegressionSnapshot.Create ( xName
                                               , se.CurrentFolder + DirectorySeparator + xName + '.xml'
                                               , se.ReferenceFolder + DirectorySeparator + xName + '.xml'
                                               )
                                               ;
        snapshot.timeStamp := XmlToDateTime(Items.XmlValueByTag['createdOn']);
        snapshot.OnReport := se.doRegressionReport;
        sln.SaveObject(xName, snapshot);
        xmlio.apiUiServerDownload ( se.remoteServerConnectionXml
                                  , '/apiUi/api/snapshots/download/' + urlPercentEncode(xName)
                                  , snapshot.FileName
                                  );
      end;
    end;
  finally
    FreeAndNil(xXml);
  end;
end;

procedure TMainForm.GetSnapshotFromRemoteServer(aSnapshot: TSnapshot);
var
  xTimeStamp: TDateTime;
begin
  xTimeStamp := now();
  try
    xmlio.apiUiServerDownload ( se.remoteServerConnectionXml
                              , '/apiUi/api/snapshots/download/' + urlPercentEncode(aSnapshot.Name)
                              , aSnapshot.FileName
                              );
    aSnapshot.Status := rsUndefined;
    aSnapshot.timeStamp := xTimeStamp;
  except
    on e: Exception do
    begin
      aSnapshot.Message := e.Message;
      aSnapshot.Status := rsException;
    end;
  end;
end;

procedure TMainForm.SnapshotsFromRemoteServer;
var
  x: Integer;
  xName: String;
  slx, sln, slc: TSnapshotList;
  s: TSnapshot;
begin
  slx := TSnapshotList.Create;  // existing, displayed
  sln := TSnapshotList.Create;  // new ones
  slc := TSnapshotList.Create;  // changed ones (subset of slx)
  try
    try
      sln.Sorted := True;
      sln.Duplicates := dupAccept;
      slx.Sorted := True;
      slx.Duplicates := dupAccept;
      se.AcquireLogLock;
      try
        for x := 0 to se.displayedSnapshots.Count - 1 do
          slx.AddObject(se.displayedSnapshots.SnapshotItems[x].Name, se.displayedSnapshots.SnapshotItems[x]);
      finally
        se.ReleaseLogLock;
      end;
      GetSnapshotsFromRemoteServer (slx, sln, slc);
      se.AcquireLogLock;
      try
        for x := 0 to slc.Count - 1 do with slc.SnapshotItems[x] do
        begin
          Status := rsUndefined;
          timeStamp := XmlToDateTime (slc.Strings[x]);
        end;
        for x := 0 to sln.Count - 1 do
          se.toDisplaySnapshots.AddObject(sln.SnapshotItems[x].Name, sln.SnapshotItems[x]);
      finally
        se.ReleaseLogLock;
      end;
      slx.Clear;
      slc.Clear;
      sln.Clear;
    except
      on e: Exception do
        LogServerException (e.Message, True, e);
    end;
  finally
    FreeAndNil(slx);
    FreeAndNil(sln);
    FreeAndNil(slc);
  end;
end;

function TMainForm.EditRemoteServerConnectionParams(aCaption: String): Boolean;
var
  xXsd: TXsd;
begin
  result := False;
  xXsd := remoteServerConnectionXsd.XsdByCaption ['remoteServerConnection.TestConnection'];
  xXsd.EditProcedure := TestRemoteServerConnection;
  xXsd.isCheckboxDisabled := True;
  with se.remoteServerConnectionAsXml do
  try
    if EditXmlXsdBased (aCaption
                       , ''
                       , ''
                       , ''
                       , False
                       , False
                       , esOne
                       , remoteServerConnectionXsd
                       , thisXml
                       , False
                       ) then
    begin
      se.remoteServerConnectionFromXml(thisXml);
      stubChanged := True;
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.SaveRemoteApiUiProjectActionExecute(Sender: TObject);
var
  saveSaveRelativeFileNames: Boolean;
begin
  saveSaveRelativeFileNames := se.SaveRelativeFileNames;
  se.SaveRelativeFileNames := False;
  try
    if EditRemoteServerConnectionParams('Upload apiUi projectdesign to cloud instance') then
    begin
      xmlio.apiUiServerDialog ( se.remoteServerConnectionXml
                              , '/apiUi/api/projectdesign'
                              , ''
                              , 'POST'
                              , 'application/json'
                              , se.ProjectDesignAsString
                              );
    end;
  finally
    se.SaveRelativeFileNames := saveSaveRelativeFileNames;
  end;
end;

procedure TMainForm.SetApiServerConnectionActionExecute(Sender: TObject);
begin
  EditRemoteServerConnectionParams('Remote apiUi server connection');
end;

procedure TMainForm.ShowOperationInfoActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned (FocusedOperation) then
    raise Exception.Create('ShowOperationInfoAction requires a selected Operation');
  try
    xXml := FocusedOperation.InformationAsXml;
    ShowXmlExtended('Information for ' + FocusedOperation.Alias, xXml);
  finally
    FreeAndNil(xXml);
  end;
end;

procedure TMainForm.ShowOperationInfoActionUpdate(Sender: TObject);
begin
  ShowOperationInfoAction.Enabled := Assigned (FocusedOperation);
end;

procedure TMainForm.ShowProjectInfoActionExecute(Sender: TObject);
var
  xXml: TXml;
begin
  if not Assigned (se) then
    raise Exception.Create('ShowProjectInfoAction requires an assigned Project');
  try
    xXml := se.InformationAsXml;
    ShowXmlExtended('Information for project ' + Caption, xXml);
  finally
    FreeAndNil(xXml);
  end;
end;

procedure TMainForm.ShowProjectInfoActionUpdate(Sender: TObject);
begin
  ShowProjectInfoAction.Enabled := Assigned (se);
end;

procedure TMainForm.SnapshotFromHttpGetActionExecute(Sender: TObject);
var
  xReport: TSnapshot;
  xNode: PVirtualNode;
  xSnapshotList: TSnapshotList;
begin
  xSnapshotList := TSnapshotList.Create;
  xNode := SnapshotsVTS.GetFirstSelected;
  while Assigned (xNode) do
  begin
    xReport := NodeToSnapshot(True, SnapshotsVTS, xNode);
    xSnapshotList.SaveObject(xReport.Name, xReport);
    xNode := SnapshotsVTS.GetNextSelected(xNode);
  end;
  TProcedureThread.Create(False, True, se, SnapshotFromRemoteServer, xSnapshotList);
end;

procedure TMainForm.SnapshotsFromHttpGetAgainActionExecute(Sender: TObject);
begin
  TProcedureThread.Create(False, True, se, SnapshotsFromRemoteServer);
end;

procedure TMainForm.SnapshotsFromHttpGetAgainActionUpdate(Sender: TObject);
begin
  SnapshotsFromHttpGetAgainAction.Enabled := Assigned (se) and se.remoteServerConnectionEnabled;
end;

procedure TMainForm.ToggleTrackDuplicateMessagesActionExecute(Sender: TObject);
begin
  doTrackDuplicateMessages := not doTrackDuplicateMessages;
end;

procedure TMainForm.YamlToClipboardMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
begin
  xmlUtil.CopyToClipboard(tlsYaml, NodeToBind(TreeView,
      TreeView.FocusedNode));
end;

procedure TMainForm.MessagesTabToolBarResize(Sender: TObject);
begin
  PositionMessagesTabControl;
end;

procedure TMainForm.NvgtViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  xOperation: TWsdlOperation;
begin
  try
    xOperation := NodeToOperation(Sender, Node);
    if Assigned(xOperation) then
    begin;
      LineBreakStyle := hlbDefault;
      HintText := xOperation.Documentation.Text;
    end;
  finally
  end;
end;

procedure TMainForm.OpenWsdlActionUpdate(Sender: TObject);
begin
  if Assigned (se) then
    OpenWsdlAction.Caption := decorateWithAsterix (OpenWsdlAction.Caption, se.hasFormalOperations);
end;

procedure TMainForm.OperationBrowseDocumentationActionExecute(Sender: TObject);
begin
  OperationDocumentationViewerClick(nil);
end;

procedure TMainForm.OperationBrowseDocumentationActionUpdate(Sender: TObject);
begin
  OperationBrowseDocumentationAction.Enabled := Assigned(FocusedOperation)
                                            and (FocusedOperation.Documentation.Count > 0);
end;

procedure TMainForm.OperationDocumentationViewerClick(Sender: TObject);
begin
   if Assigned (FocusedOperation) then
  begin
    XmlUtil.PushCursor (crHourGlass);
    try
      with TMarkdownProcessor.CreateDialect(mdCommonMark) do
      try
        XmlUtil.presentAsHTML(FocusedOperation.Alias, process(prepareMarkDownText(FocusedOperation.Documentation.Text)));
      finally
        Free;
      end;
    finally
      XmlUtil.PopCursor;
    end;
  end;
end;

procedure TMainForm.PromptAndSetColumnWidth(aTreeView: TVirtualStringTree);
var
  n, w: Integer;
begin
  n := aTreeView.FocusedColumn;
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Column width';
    PromptForm.PromptEdit.Text := IntToStr (aTreeView.Header.Columns[n].Width);
    PromptForm.Numeric := True;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
      aTreeView.Header.Columns[n].Width := StrToInt(PromptForm.PromptEdit.Text);
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm.GridColumnWidthMenuItemClick(Sender: TObject);
begin
  PromptAndSetColumnWidth(GridView);
end;

procedure TMainForm .AbortActionUpdate (Sender : TObject );
begin
  AbortAction.Enabled := (NumberOfBlockingThreads > 0)
                      or (NumberOfNonBlockingThreads > 0)
                       ;
end;

procedure TMainForm .MessagesTabControlChange (Sender : TObject );
begin
  ShowKindOfLogData := TShowKindOfLogData(MessagesTabControl.TabIndex);
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
  x1, x2: Extended;
  log1, log2: TLog;
begin
  Result := 0;
  s1 := '';
  s2 := '';
  case TLogColumnEnum(Column) of
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
      logDurationColumn:
        begin
          log1 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node1);
          if Assigned (log1) then with log1 do
          begin
            x1 := 24 * 60 * (OutBoundTimeStamp - InboundTimeStamp);
            if x1 < 0 then
              x1 := -1 * x1;
          end;
          log2 := NodeToMsgLog(False,Sender as TVirtualStringTree, Node2);
          if Assigned (log2) then with log2 do
          begin
            x2 := 24 * 60 * (OutBoundTimeStamp - InboundTimeStamp);
            if x2 < 0 then
              x2 := -1 * x2;
          end;
          if  x1 < x2 then
            result := -1;
          if x1 > x2 then
            result := 1;
          Exit;
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

procedure TMainForm.ToggleDoScrollMessagesIntoViewActionExecute(Sender: TObject);
begin
  se.AcquireLogLock;
  try
    doScrollMessagesIntoView := not doScrollMessagesIntoView;
  finally
    se.ReleaseLogLock;
  end;
end;

procedure TMainForm.ToggleTrackIOActionExecute(Sender: TObject);
begin
  xmlio.doTrackXmlIO := not xmlio.doTrackXmlIO;
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
    PromptForm.Pattern := S_ALIAS_VALID_PATTERN;
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
        NvgtView.Invalidate;
        TreeView.Invalidate;
      end;
    end;
  finally
    FreeAndNil(PromptForm);
  end;
end;

procedure TMainForm .OperationAliasActionExecute (Sender : TObject );
begin
  if not Assigned(FocusedOperation) then
    raise Exception.Create('No operation selected');
  PromptForOperationAlias(FocusedOperation);
end;

initialization
{$ifdef windows}
  CoInitialize(nil);
{$endif}
  _WsdlSaveLogs := _SaveLogs;
  _wsdlSetContext := _mainformSetContext;
  _wsdlGetContext := _mainformGetContext;
finalization
{$ifdef windows}
  CoUninitialize;
{$endif}
end.
