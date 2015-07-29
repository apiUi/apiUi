unit l4jExplorerUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  AdoDB, Windows,
{$ELSE}
  sqldb, LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils
   , Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, VirtualTrees
   , Dialogs
   , FormIniFilez, ToolWin, ActnList, Menus, ImgList , FileUtil
   , FilterDialog
   , Xmlz
   , Xsdz
   , ParserClasses
   , AbUnzper
   , Express
   , Bind
   , ActiveX
   ;

Type
  PTreeRec = ^TTreeRec;
  TTreeRec = record
    Index: Integer;
  end;

type
  TLogType = class (TObject)
    public
      Name: AnsiString;
      sTag: AnsiString;
      eTag: AnsiString;
      eyeCatchers: TXml;
  end;
  TLogTypes = class (TStringList)
  private
    function GetLogType(Index: integer): TLogType;
  published
    public
    property Types [Index: integer]: TLogType read GetLogType;
  end;

type
  TColumnType = (ctAttribute, ctElement, ctEscXmlElement, ctIndex, ctElapsed, ctSize, ctHasString, ctHasXmlValue);
  TDisplayedColumn = class (TObject)
    public
      Header, Key: String;
      ColumnType: TColumnType;
  end;

type
  Tl4jExplorerForm = class;
  TOnHaveString = procedure (aString: String
                            ) of Object;
  TOnUpdateStatus = procedure (aNumber, aTotal: Integer
                              ) of Object;


 TMsg = class (TObject)
  private
    function getAsText: String;
public
  FirstTimeStamp: String;
  events: String;
  property AsText: String read getAsText;
  constructor Create;
  destructor Destroy; override;
end;

 TMsgList = class (TStringList)
  private
    function getMsg(Index: Integer): TMsg;
    procedure setMsg(Index: Integer; const Value: TMsg);
    function getAsText: String;
  published
public
  property Msg [Index: Integer]: TMsg read getMsg write setMsg;
  property AsText: String read getAsText;
  procedure Clear; override;
  constructor Create;
end;

 TSl = class (TStringList)
public
  procedure xpNeedData (Sender: TObject; var aMoreData: Boolean; var aData: String);
end;

 TCustomThread = class(TThread)
  private
    fNumber, fTotal: Integer;
    fStatusText, fMessage: AnsiString;
    fForm: Tl4jExplorerForm;
    fString: AnsiString;
    fEnabled: Boolean;
    fAbortPressed: Boolean;
    procedure fSynchronisedShowMessage;
    procedure fSynchronisedHaveData;
    procedure fSynchronisedStatusUpdate;
    procedure fSynchronisedEnableAbortButton;
    procedure fSynchronisedAdjustDisplayedColumns;
    procedure setAbortPressed(const Value: Boolean);
  protected
    procedure UpdateStatus (aNumber, aTotal: Integer; aText: AnsiString);
  public
    property abortPressed: Boolean read fAbortPressed write setAbortPressed;
  end;

  TSearchThread = class(TCustomThread)
  private
    fFilenames: TStringList;
    fFilter1, fFilter2, fFilter3, fFilter4: AnsiString;
              fHasNot2, fHasNot3, fHasNot4: Boolean;
    function StringPassesFilter (aString: AnsiString): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create ( aForm: Tl4jExplorerForm
                       ; aFileNames: TStrings
                       ; aFilter1, aFilter2, aFilter3, aFilter4: AnsiString
                       ; aHasnot2, aHasNot3, aHasNot4: Boolean
                       );
  end;

  TQueryThread = class(TCustomThread)
  private
    fConnString, fQuery: String;
  protected
    procedure Execute; override;
  public
    constructor Create ( aForm: Tl4jExplorerForm
                       ; aConnString, aQuery: String
                       ; aParam1, aParam2, aParam3, aParam4: String
                       );
  end;


  Tl4jExplorerForm = class(TForm)
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
    OpenFileAction: TAction;
    ToolButton11: TToolButton;
    ProgressBar: TProgressBar;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    AbortAction: TAction;
    ToolButton14: TToolButton;
    AllXmlAction: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenLog4Jevents1: TMenuItem;
    Savelogevents1: TMenuItem;
    N1: TMenuItem;
    CloseAction1: TMenuItem;
    Edit1: TMenuItem;
    CopyAction1: TMenuItem;
    Search1: TMenuItem;
    Paste1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Extra1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    License1: TMenuItem;
    Openconfig1: TMenuItem;
    Saveconfig1: TMenuItem;
    Saveconfigas1: TMenuItem;
    N2: TMenuItem;
    Editconfig1: TMenuItem;
    Splitter1: TSplitter;
    View1: TMenuItem;
    ElementAttribute1: TMenuItem;
    WraptekstMenuItem: TMenuItem;
    StatusBar: TStatusBar;
    N3: TMenuItem;
    Command1: TMenuItem;
    Save1: TMenuItem;
    EditDisplayedColumnsAction: TAction;
    ToolButton15: TToolButton;
    Editdisplayedcolumns1: TMenuItem;
    QueryDbAction: TAction;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    QueryDbAction1: TMenuItem;
    N4: TMenuItem;
    procedure Save1Click(Sender: TObject);
    procedure Command1Click(Sender: TObject);
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
      var CellText: String);
    procedure FormDestroy(Sender: TObject);
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
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewDblClick(Sender: TObject);
    procedure EditDisplayedColumnsActionExecute(Sender: TObject);
    procedure QueryDbActionExecute(Sender: TObject);
  private
    fReadOnly: Boolean;
    IniFile: TFormIniFile;
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
    procedure AdjustDisplayedColumns (aXml: TXml);
    function getDoWrapText: Boolean;
    procedure setDoWrapText(const Value: Boolean);
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    function GetEndurance (aString: String): String;
    function GetAtt (aKey, aString: String): String;
    function GetElm (aKey, aString: String): String;
    function GetEscXmlElm (aKey, aString: String): String;
    function GetHasString (aDc: TDisplayedColumn; aString: String): String;
    function GetHasXmlValue (aDc: TDisplayedColumn; aString: String): String;
    function GetText (aData: TStringList; aIndex, aColumn: Integer): String;
    function Filter (Data: TStringList; aIndex: Integer): Boolean;
    procedure l4jInit;
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
  l4jExplorerForm: Tl4jExplorerForm;
  xpMoreData, xpFetched: Boolean;
  xpScript: String;
  Msgs: TMsgList;
  TimeStamp, MessageId, ServiceRequestorId, ServiceId, EventType, EventData, Dummy: String;
  fParam1, fParam2, fParam3, fParam4: String;
  xp: TExpress;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  FindRegExpDialog
   , igGlobals
   , ClipBrd
   , xmlUtilz
   , strUtils
   , IdSync
   , IpmGunLicense
   , ErrorFound
   , cbAbout
   , ShowXmlUnit
   , XmlGridUnit
   , PromptUnit
   , CommandDialog
   , DbFilterDialog;

const treeTagColumn = 0;
const treeValueColumn = 2;
const treeButtonColumn = 1;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
procedure sqlLoop;
var
  Msg: TMsg;
  f, y: Integer;
  s: AnsiString;
begin
  if (not xpFetched)
{ TODO : abortfie
 }
// and (not xp.abortPressed)
  then
    l4jExplorerForm.Thread.UpdateStatus(5, 10, 'Fetched first data...');
  xpFetched := True;
  if Msgs.Find(MessageId, f) then
    Msg := Msgs.Msg[f]
  else
  begin
    Msg := TMsg.Create;
    Msgs.AddObject(MessageId, Msg);
  end;
  with Msg do
  begin
    if (TimeStamp < FirstTimeStamp) then
      FirstTimeStamp := TimeStamp;
    s := '<' + EventType + 'Info>'
       + '<TimeStamp>' + TimeStamp + '</TimeStamp>'
       + '<MessageId>' + MessageId + '</MessageId>'
       + '<ServiceRequestorId>' + ServiceRequestorId + '</ServiceRequestorId>'
       + '<ServiceId>' + ServiceId + '</ServiceId>'
       ;
    for f := 0 to xp.uwaLoopQry.Fields.Count - 1 do
    begin
      with xp.uwaLoopQry.Fields.Fields[f] do
      begin
        if (DisplayName <> 'TimeStamp')
        and (DisplayName <> 'MessageId')
        and (DisplayName <> 'TIMESTAMP')
        and (DisplayName <> 'MESSAGEID')
        and (DisplayName <> 'LOG_TIME')
        and (DisplayName <> 'REQUESTER_ID')
        and (DisplayName <> 'EVENT_TYPE')
        and (DisplayName <> 'EVENT_DATA')
        then
          s := s
             + '<' + DisplayName + '>'
             + DisplayText
             + '</' + DisplayName + '>'
             ;
      end;
    end;
    s := s
       + '<EventType>' + EventType + '</EventType>'
       + '</' + EventType + 'Info>'
       + CRLF
       + '<' + EventType + '>' + EventData + '</' + EventType + '>'
       + CRLF
       ;
    events := events + s;
  end;
end;

function ReadStringFromFile (aFileName: String): AnsiString;
var
  x: Integer;
  ss: TMemoryStream;
  aUnzipper: TAbUnZipper;
  xExt: String;
begin
  xExt := UpperCase(ExtractFileExt(aFileName));
  if (xExt = '.ZIP')
  or (xExt = '.GZ')
  then
  begin
    ss := TMemoryStream.Create;
    try
      aUnzipper := TAbUnZipper.Create(nil);
      try
        aUnzipper.FileName := aFileName;
        for x := 0 to aUnzipper.Count - 1 do
          aUnzipper.ExtractToStream(aUnzipper.Items[x].FileName, ss);
        aUnzipper.CloseArchive;
      finally
        aUnzipper.Free;
      end;
      ss.Position := 0;
      try
        SetLength(Result, ss.Size);
        ss.Read(Pointer(Result)^, ss.Size);
      except
        Result := '';  // Deallocates memory
        raise;
      end;
    finally
      ss.Free;
    end;
  end
  else
  begin
    with TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) do
    begin
      try
        SetLength(Result, Size);
        Read(Pointer(Result)^, Size);
      except
        Result := '';  // Deallocates memory
        Free;
        raise;
      end;
      Free;
    end;
  end;
end;

procedure Tl4jExplorerForm.AdjustDisplayedColumns(aXml: TXml);
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
{}{
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
  xDc.Header := 'ServiceRequestorId';
  xDc.Key := 'ServiceRequestorId';
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
{}
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

procedure Tl4jExplorerForm.FormCreate(Sender: TObject);
var
  xXml: TXml;
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  TreeView.NodeDataSize := SizeOf (TTreeRec);
  TreeView.RootNodeCount := 0;
//CloseAction.ShortCut := VK_ESCAPE;
  isChanged := False;
  DisplayedColumns := TStringList.Create;
  ColumnWidths := TStringList.Create;
  ColumnWidths.Text := IniFile.ReadString ('ColumnWidths');
  SaveLog4JFileName := IniFile.StringByNamedef ['SaveLog4JFileName', ''];
  ReadLog4JFileName := IniFile.StringByNamedef ['ReadLog4JFileName', ''];
  ShowDetailed := IniFile.StringByNamedef ['ShowDetailed', ''];
  doWrapText := IniFile.BooleanByNamedef ['doWrapText', False];
  WraptekstMenuItemClick (nil);
  FilterDlg.Caption := 'Configure filter';
  Data := TStringList.Create;
  StatusBar.Visible := False;
  ProgressBar.Visible := False;
  logTypes := TLogTypes.Create;
  iniXml := TXml.Create;
  l4jInit;
end;

procedure Tl4jExplorerForm.XmlTreeViewGetText ( Sender: TBaseVirtualTree
                                      ; Node: PVirtualNode
                                      ; Column: TColumnIndex
                                      ; TextType: TVSTTextType
                                      ; var CellText: String
                                      );
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData (Node);
  CellText := GetText (Data, xData.Index, Column);
end;

procedure Tl4jExplorerForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  TreeView.Clear;
  for x := 0 to DisplayedColumns.Count - 1 do
    DisplayedColumns.Objects [x].Free;
  DisplayedColumns.Free;
  for x := TreeView.Header.Columns.Count - 1  downto 0 do
    ColumnWidths.Values [TreeView.Header.Columns.Items[x].Text]
    := IntToStr (TreeView.Header.Columns.Items[x].Width);
  IniFile.StringByName ['SaveLog4JFileName'] := SaveLog4JFileName;
  IniFile.StringByName ['ReadLog4JFileName'] := ReadLog4JFileName;
  IniFile.StringByName  ['configFileName'] := configFileName;
  IniFile.StringByName ['ShowDetailed'] := ShowDetailed;
  IniFile.BooleanByName ['doWrapText'] := doWrapText;
  IniFile.WriteString ('ColumnWidths', ColumnWidths.Text);
  IniFile.WriteString ('DisplayedColumns', DisplayedColumnsXml.Text);
  IniFile.Save;
  IniFile.Free;
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
end;

procedure Tl4jExplorerForm.ActionList1Update(Action: TBasicAction;
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

procedure Tl4jExplorerForm.FindActionExecute(Sender: TObject);
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
          Found := StringMatchesMask ( Data.Strings [xData.Index]
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

procedure Tl4jExplorerForm.FindNextActionExecute(Sender: TObject);
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
        Found := StringMatchesMask ( Data.Strings [xData.Index]
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

procedure Tl4jExplorerForm.TreeViewPopupMenuPopup(Sender: TObject);
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

procedure Tl4jExplorerForm.UseReadDisplayedColumns;
begin
  DisplayedColumnsXml.Free;
  DisplayedColumnsXml := TXml.Create (0, DisplayedColumnsXsd);
  DisplayedColumnsXml.LoadValues(readDisplayedColumnsXml, False, True);
  DisplayedColumnsChanged := True;
  AdjustDisplayedColumns(DisplayedColumnsXml);
end;

procedure Tl4jExplorerForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView.FocusedNode := TreeView.GetNodeAt(X, Y);
end;

procedure Tl4jExplorerForm.TreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure Tl4jExplorerForm.TreeViewFocusChanged(Sender: TBaseVirtualTree;
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
      s := GetAtt (ShowDetailed, Data.Strings[xData.Index]);
    end;
    Memo.Text := s;
  finally
    Screen.Cursor := xCursor;
  end;
end;

procedure Tl4jExplorerForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure Tl4jExplorerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

function Tl4jExplorerForm.findLogType(aString: AnsiString): TLogType;
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

function Tl4jExplorerForm.getReadOnly: Boolean;
begin
  result := fReadOnly;
end;

procedure Tl4jExplorerForm.setReadOnly(const Value: Boolean);
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

procedure Tl4jExplorerForm.TreeViewExit(Sender: TObject);
begin
  TreeView.EndEditNode;
end;

procedure Tl4jExplorerForm.TreeViewGetImageIndex(Sender: TBaseVirtualTree;
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

procedure Tl4jExplorerForm.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData (Node);
  CellText := GetText (Data, xData.Index, Column);
end;

procedure Tl4jExplorerForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
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

procedure Tl4jExplorerForm.TreeViewClick(Sender: TObject);
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

procedure Tl4jExplorerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TreeView.EndEditNode;
end;

procedure Tl4jExplorerForm.ZoomMenuItemClick(Sender: TObject);
var
  xData: PTreeRec;
begin
  if Assigned (TreeView.FocusedNode) then
  begin
    xData := TreeView.GetNodeData (TreeView.FocusedNode);
    xmlUtil.presentString ('', Data.Strings[xData.Index]);
  end;
end;

procedure Tl4jExplorerForm.FormShow(Sender: TObject);
var
  x: Integer;
begin
  if not Assigned (Data) then
    raise Exception.Create ('arg Data not assigned');
  AdjustDisplayedColumns(DisplayedColumnsXml);
  TreeView.RootNodeCount := Data.Count;
  TreeView.Header.SortColumn := 0;
end;

procedure Tl4jExplorerForm.FilterActionExecute(Sender: TObject);
var
  xNode: PVirtualNode;
  xPasses: Boolean;
  xCursor: TCursor;
  xData: PTreeRec;
begin
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
        xPasses := Filter (Data, xData.Index);
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

procedure Tl4jExplorerForm.ZoomMenuActionClick(Sender: TObject);
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
                            , prepareDataToZoom(Data.Strings [xData.Index])
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
        s := s + prepareDataToZoom(Data.Strings [xData.Index]);
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

procedure Tl4jExplorerForm.TreeViewCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  s1, s2: String;
  d1, d2: PTreeRec;
begin
  Result := 0;
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
  s1 := GetText (Data, d1.Index, Column);
  s2 := GetText (Data, d2.Index, Column);
  if  s1 < s2 then
    result := -1;
  if s1 > s2 then
    result := 1;
end;

procedure Tl4jExplorerForm.TreeViewDblClick(Sender: TObject);
begin
  ZoomMenuActionClick(nil);
end;

procedure Tl4jExplorerForm.TreeViewHeaderClick(Sender: TVTHeader;
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

procedure Tl4jExplorerForm.TreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  xData: PTreeRec;
begin
  xData := TreeView.GetNodeData(Node);
  xData.Index := Node.Index;
end;

procedure Tl4jExplorerForm.WriteXmlActionExecute(Sender: TObject);
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
        s := '<xmlContainer>' + DisplayedColumnsXml.AsText (False, -1, True, False);
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

procedure Tl4jExplorerForm.OpenFileActionExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := ReadLog4JFileName;
    Options := Options + [ofAllowMultiSelect];
    if not Execute then
      Exit;
    if Files.Count > 0 then
      ReadLog4JFileName := Files.Strings[0];
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
    Free;
  end;
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
  fForm.ProgressBar.Visible := fEnabled;
  fForm.StatusBar.Visible := fEnabled;
end;

procedure TCustomThread.fSynchronisedHaveData;
begin
//fForm.Data.Add (fString);
  fForm.TreeView.RootNodeCount := fForm.Data.Count;
end;

procedure TCustomThread.fSynchronisedShowMessage;
begin
  ShowMessage (fMessage);
end;

procedure TCustomThread.fSynchronisedStatusUpdate;
begin
  fForm.ProgressBar.Max := fTotal;
  fForm.ProgressBar.Position := fNumber;
  fForm.StatusBar.SimpleText := fStatusText;
end;

procedure TCustomThread.setAbortPressed(const Value: Boolean);
begin
  fAbortPressed := Value;
  if fAbortPressed
  and Assigned (xp) then
  { TODO : abortfie }
//    xp.abortPressed := True
    ;
end;

function TSearchThread.StringPassesFilter(aString: AnsiString): Boolean;
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

procedure TCustomThread.UpdateStatus(aNumber, aTotal: Integer; aText: AnsiString);
begin
  fNumber := aNumber;
  fTotal := aTotal;
  fStatusText := aText;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedStatusUpdate);
    finally
      Free;
    end;
end;

procedure Tl4jExplorerForm.AbortActionExecute(Sender: TObject);
begin
  if Assigned (Thread) then
  begin
    Thread.abortPressed := True;
    AbortAction.Enabled := False;
  end;
end;

procedure Tl4jExplorerForm.CopyActionExecute(Sender: TObject);
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
          s := s + xSep  + '"' + GetText (Data, xData.Index, c) + '"';
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

procedure Tl4jExplorerForm.AllXmlActionExecute(Sender: TObject);
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

procedure Tl4jExplorerForm.AllXmlActionUpdate(Sender: TObject);
begin
  AllXmlAction.Enabled := (numberVisible < 500);
end;

procedure Tl4jExplorerForm.AllXmlActionHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if not AllXmlAction.Enabled then
    HintStr := HintStr + ' (disabled because of number of events)';
end;

function Tl4jExplorerForm.GetEndurance(aString: String): String;
var
  reqTime, repTime: TDateTime;
  reqTimeAsStr, repTimeAsStr: String;
  ms: Extended;
begin
  result := '';
{ TODO : GetEndurance }
{$ifdef GETENDURANCE}
  try
  with TXSDateTime.Create do
    try
      reqTimeAsStr := GetAtt ('<ServiceRequestTimestamp>', aString);
      repTimeAsStr := GetAtt ('<ServiceReplyTimestamp>', aString);
      XSToNative(Copy (reqTimeAsStr, 1, 20) + '000'); // convert from String
      reqTime := AsDateTime; // convert to TDateTime
      XSToNative(Copy (repTimeAsStr, 1, 20) + '000'); // convert from String
      repTime := AsDateTime; // convert to TDateTime
      ms := (repTime - reqTime) * 24 * 60 * 60 * 1000
          + StrToInt(Copy (repTimeAsStr, 21, 3))
          - StrToInt(Copy (reqTimeAsStr, 21, 3))
          ;
      result := Format ('%.3f', [ms / 1000]);
    finally
      Free;
    end;
  except
  end;
  {$endif}
end;


function Tl4jExplorerForm.GetEscXmlElm(aKey, aString: String): String;
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

function Tl4jExplorerForm.GetHasString(aDc: TDisplayedColumn;
  aString: String): String;
begin
  if Pos (aDc.Key, aString) > 0 then
    result := aDc.Header
  else
    result := '';
end;

function Tl4jExplorerForm.GetHasXmlValue(aDc: TDisplayedColumn;
  aString: String): String;
begin
  if Pos ('>' + aDc.Key + '<', aString) > 0 then
    result := aDc.Header
  else
    result := '';
end;

function Tl4jExplorerForm.GetElm(aKey, aString: String): String;
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

function Tl4jExplorerForm.GetAtt (aKey, aString: String): String;
var
  x: Integer;
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
  x := Pos (aKey, aString);
  if x < 1 then
    if aKey[1] = '<' then
      x := Pos (':' + Copy (aKey, 2, Length (aKey) - 2), aString); // maybe a nsprefix, maybe followed by an xml attribute
  if x < 1 then
    x := Length (aString) + 1;
  if aKey [1] = '<' then
  begin
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
  end
  else
  begin
    while (x < Length (aString))
    and (aString[x] <> '"') do
      Inc (x);
    Inc (x);
    while (x < Length (aString))
    and (aString[x] <> '"') do
    begin
      result := result + aString [x];
      Inc (x);
    end;
  end;
end;

function Tl4jExplorerForm.GetText(aData: TStringList; aIndex,
  aColumn: Integer): String;
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

procedure Tl4jExplorerForm.l4jInit;
var
  x: Integer;
  xXml: TXml;
  xLogType: TLogType;
begin
  xmlUtil.doExpandFull := True;
  _xmlLicensed := False;
  try
    l4jDbName := '';
    l4jIniFileName := Copy ( ParamStr(0)
                           , 1
                           , Length (ParamStr(0)) - 4
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
      l4jXsdDescr := TXsdDescr.Create(1);
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
    xXml := TXml.Create;
    xXml.LoadFromString(IniFile.ReadString ( 'DisplayedColumns')
 {
                                           , '<DisplayedColumns>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>Level</Key>'
                                           + '    <ColumnHeader>Level</ColumnHeader>'
                                           + '    <Type>Attribute</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>ServiceId</Key>'
                                           + '    <ColumnHeader>ServiceId</ColumnHeader>'
                                           + '    <Type>Element</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>ServiceRequestorId</Key>'
                                           + '    <ColumnHeader>ServiceRequestorId</ColumnHeader>'
                                           + '    <Type>Element</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>MessageId</Key>'
                                           + '    <ColumnHeader>MessageId</ColumnHeader>'
                                           + '    <Type>Element</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>FaultCode</Key>'
                                           + '    <ColumnHeader>FaultCode</ColumnHeader>'
                                           + '    <Type>Element</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '  <DisplayedColumn>'
                                           + '    <Key>FaultDescription</Key>'
                                           + '    <ColumnHeader>FaultDescription</ColumnHeader>'
                                           + '    <Type>Element</Type>'
                                           + '    <Enabled>true</Enabled>'
                                           + '  </DisplayedColumn>'
                                           + '</DisplayedColumns>'
                                           )
}
                       , nil
                       );
    DisplayedColumnsXml := TXml.Create(0, DisplayedColumnsXsd);
    DisplayedColumnsXml.LoadValues(xXml, False, False);
    readDisplayedColumnsXml := TXml.Create;

    if FileExistsUTF8(l4jDbName) { *Converted from FileExists* } then
    begin
      _xmlLicensed := ValidateLicense (IniFile, 'l4j', l4jDbName);
      LogUsage(l4jDbName);
    end;
    configFileName := IniFile.StringByNameDef  ['configFileName', ''];
  //DragAcceptFiles(Self.Handle, True);
    _OnParseErrorEvent := ParserError;
    if configFileName <> '' then
      readConfig (configFileName);
  finally
    if not _xmlLicensed then
      ShowMessage ( 'Since you are not a licensed user,'
                  + #$D#$A
                  + 'l4j will only show the first part of values.'
                  );
  end;
end;


procedure Tl4jExplorerForm.ParserError(Sender: TObject; LineNumber,
  ColumnNumber, Offset: Integer; TokenString, Data: String);
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

function Tl4jExplorerForm.prepareDataToZoom(s: String): String;
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

procedure Tl4jExplorerForm.QueryDbActionExecute(Sender: TObject);
begin
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
end;

procedure Tl4jExplorerForm.readConfig(aFileName: String);
var
  xXml: TXml;
begin
  configFileName := aFileName;
  xXml := TXml.Create;
  try
    xXml.LoadFromFile (configFileName, nil);
    configFromXml (xXml);
    configRead := True;
    configChanged := False;
  finally
    xXml.Free;
  end;
end;

procedure Tl4jExplorerForm.configFromXml(aXml: TXml);
begin
  if aXml.Name <> 'xmlExplorerConfig' then
    raise Exception.Create ('Not an xmlExplorer config file');
  xmlUtil.IpmDescrsFromXml (configFileName, aXml.Items.XmlItemByTag['RecordDescriptors']);
end;

function Tl4jExplorerForm.Filter(Data: TStringList; aIndex: Integer): Boolean;
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

procedure Tl4jExplorerForm.License1Click(Sender: TObject);
begin
  UpdateLicense  (IniFile, 'l4j', l4jDbName);
  _xmlLicensed := ValidateLicense (IniFile, 'l4j', l4jDbName);
end;

procedure Tl4jExplorerForm.About1Click(Sender: TObject);
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

procedure Tl4jExplorerForm.Openconfig1Click(Sender: TObject);
begin
  OnlyWhenLicensed;
  with TOpenDialog.Create (nil) do
  try
    DefaultExt := 'xmlExplorer';
    FileName := configFileName;
    Filter := 'xmlExplorer config (*.xmlExplorer)|*.xmlExplorer';
    Title := 'Open xmlExplorer config';
    Options := Options + [ofFileMustExist];
    if Execute then
    begin
      readConfig (configFileName);
    end;
  finally
    Free;
  end;
end;

procedure Tl4jExplorerForm.OnlyWhenLicensed;
begin
  if (GetUserName <> 'BouwmanJW')
  and (GetUserName <> 'Bouwman')
  and (not _xmlLicensed) then
    raise Exception.Create ('Disabled because you are not a licensed user.');
end;

procedure Tl4jExplorerForm.Saveconfig1Click(Sender: TObject);
begin
  OnlyWhenLicensed;
  saveConfig (configFileName);
end;

procedure Tl4jExplorerForm.saveConfig(aFileName: String);
  procedure _ToRelativeFileNames(aFileName: String; aXml: TXml);
  var
    x: Integer;
  begin
    if aXml.TagName = 'DescriptorFileName' then
      aXml.Value := ExtractRelativeFileName (aFileName, aXml.Value)
    else
    begin
      for x := 0 to aXml.Items.Count - 1 do
        _ToRelativeFileNames(aFileName, aXml.Items.XmlItems[x]);
    end;
  end;

var
  xXml: TXml;
begin
  xXml := configAsXml;
  try
    _ToRelativeFileNames(aFileName, xXml);
    SaveStringToFile (aFileName, xXml.AsText(False, 0, False, False));
    configRead := True; // well, looks like
    configChanged := False;
  finally
    xXml.free;
  end;
end;

function Tl4jExplorerForm.configAsXml: TXml;
begin
  result := TXml.CreateAsString ('xmlExplorerConfig', '');
  result.AddXml (xmlUtil.IpmDescrsAsXml);
end;

procedure Tl4jExplorerForm.Saveconfigas1Click(Sender: TObject);
begin
  OnlyWhenLicensed;
  with TSaveDialog.Create (nil) do
  try
    DefaultExt := 'xmlExplorer';
    FileName := configFileName;
    Filter := 'xmlExplorer config (*.xmlExplorer)|*.xmlExplorer';
    Title := 'Save xmlExplorer config';
    Options := Options + [ofOverWritePrompt];
    if Execute then
    begin
      configFileName := FileName;
      saveConfig (configFileName);
    end;
  finally
    Free;
  end;
end;

procedure Tl4jExplorerForm.Editconfig1Click(Sender: TObject);
var
  nXml, cXml: TXml;
begin
  if Assigned (configXsd) then
  begin
    nXml := TXml.Create (0, configXsd);
    try
      cXml := configAsXml;
      try
        nXml.LoadValues (cXml, False, False);
        Application.CreateForm(TShowXmlForm, ShowXmlForm);
        try
          ShowXmlForm.Caption := 'l4j config ' + configFileName;
          ShowXmlForm.Bind := nXml;
          ShowXmlForm.isReadOnly := False;
          ShowXmlForm.ShowModal;
          if (ShowXmlForm.modalResult = mrOk)
          or (True) then
          begin
            cXml.CopyDownLine (nXml, True);
            configFromXml (cXml);
            configChanged := True;
          end;
        finally
          FreeAndNil (ShowXmlForm);
        end;
      finally
        cXml.Free;
      end;
    finally
      nXml.Free;
    end;
  end;
end;

procedure Tl4jExplorerForm.EditDisplayedColumnsActionExecute(Sender: TObject);
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

procedure Tl4jExplorerForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := OkToOpenCase;
end;

function Tl4jExplorerForm.OkToOpenCase: Boolean;
var
  ret: Word;
begin
  result := True;
  if configChanged then
  begin
    ret := MessageDlg ('Save changes to config?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if (ret = mrYes) then
    begin
      OnlyWhenLicensed;
      if configRead then
        saveConfig (configFileName)
      else
      begin
        with TSaveDialog.Create (nil) do
        try
          DefaultExt := 'xmlExplorer';
          FileName := configFileName;
          Filter := 'xmlExplorer config (*.xmlExplorer)|*.xmlExplorer';
          Title := 'Save xmlExplorer config';
          if Execute then
          begin
            configFileName := FileName;
            saveConfig (configFileName);
          end
          else
            Result := False;
        finally
          Free;
        end;
      end;
    end;
    if (ret = mrCancel) then
      Result := False;
  end;
end;

procedure Tl4jExplorerForm.ElementAttribute1Click(Sender: TObject);
begin
  Application.CreateForm(TPromptForm, PromptForm);
  try
    PromptForm.Caption := 'Show <element> or attribute';
    PromptForm.PromptEdit.Text := ShowDetailed;
    PromptForm.Numeric := False;
    PromptForm.ShowModal;
    if PromptForm.ModalResult = mrOk then
      ShowDetailed := PromptForm.PromptEdit.Text;
  finally
    FreeAndNil (PromptForm);
  end;
end;

procedure Tl4jExplorerForm.WraptekstMenuItemClick(Sender: TObject);
begin
  if DoWrapText then
    Memo.ScrollBars := ssVertical
  else
    Memo.ScrollBars := ssBoth;
end;

function Tl4jExplorerForm.getDoWrapText: Boolean;
begin
  result := WraptekstMenuItem.Checked;
end;

procedure Tl4jExplorerForm.setDoWrapText(const Value: Boolean);
begin
  WraptekstMenuItem.Checked := Value;
end;

procedure Exec(const CommandLine: string);
{ TODO : exec }
{
var
  StartupInfo: Windows.TStartupInfo;        // start-up info passed to process
  ProcessInfo: Windows.TProcessInformation; // info about the process
}
begin
{
  // Initialise startup info structure to 0, and record length
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  // Execute application commandline
  Windows.CreateProcess ( nil
                        , PChar(CommandLine)
                        , nil
                        , nil
                        , False
                        , 0
                        , nil
                        , nil
                        , StartupInfo
                        , ProcessInfo
                        );
}
end;

function ExecAndWait(const CommandLine: string) : Boolean;
{ TODO : execandwait }
{
var
  StartupInfo: Windows.TStartupInfo;        // start-up info passed to process
  ProcessInfo: Windows.TProcessInformation; // info about the process
  ProcessExitCode: Windows.DWord;           // process's exit code
}
begin
end;

procedure Tl4jExplorerForm.Command1Click(Sender: TObject);
begin
  CommandDlg.ShowModal;
  if CommandDlg.ModalResult = mrOk then
  begin
    if CommandDlg.WaitCheckBox.Checked then
      ExecAndWait ( CommandDlg.FileNameEdit.Text
                  + ' '
                  + CommandDlg.ArgsEdit.Text
                  )
    else
      Exec ( CommandDlg.FileNameEdit.Text
           + ' '
           + CommandDlg.ArgsEdit.Text
           );
  end;
end;

procedure Tl4jExplorerForm.Save1Click(Sender: TObject);
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
        xNode := TreeView.GetFirstSelected;
        while Assigned (xNode) do
        begin
          xData := TreeView.GetNodeData (xNode);
          s := s + Data.Strings [xData.Index];
          xNode := TreeView.GetNextSelected (xNode);
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

{ TLogTypes }

function TLogTypes.getLogType(Index: integer): TLogType;
begin
  result := TLogType (Objects [index]);
end;

{ TSearchThread }

constructor TSearchThread.Create ( aForm: Tl4jExplorerForm
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
  procedure _Extract (var s: AnsiString);
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
          SynchronizeMethod (fSynchronisedHaveData);
        finally
          Free;
        end;
    end;
  end;
  procedure _ExtractDisplayedColumns (var s: AnsiString);
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
            SynchronizeMethod (fSynchronisedAdjustDisplayedColumns);
          finally
            Free;
          end;
      end;
    end;
  end;
var
  x, y, yC: Integer;
  s: AnsiString;
  ss: TMemoryStream;
  aUnzipper: TAbUnZipper;
  xExt: String;
begin
  fEnabled := True;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedEnableAbortButton);
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
 {}
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
                  UpdateStatus ( x + 1
                               , fFileNames.Count + 1
                               , fFileNames.Strings[x]
                               + ' ['
                               + aUnzipper.Items[y].FileName
                               + ']'
                               );
                  aUnzipper.ExtractToStream(aUnzipper.Items[y].FileName, ss);
                  ss.Position := 0;
                  SetLength(s, ss.Size);
                  ss.Read(Pointer(s)^, ss.Size);
                  if (x = 0) and (y = 0) then
                    _ExtractDisplayedColumns (s);
                  _Extract (s);
                  s := '';
                except
                  on e: Exception do
                  begin
                    s := '';  // Deallocates memory
                    fMessage := e.Message;
                    with TIdSync.Create do
                      try
                        SynchronizeMethod (fSynchronisedShowMessage);
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
              _ExtractDisplayedColumns(s);
            _Extract (s);
            s := '';
          except
            on e: Exception do
            begin
              s := '';  // Deallocates memory
              fMessage := e.Message;
              with TIdSync.Create do
                try
                  SynchronizeMethod (fSynchronisedShowMessage);
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
        SynchronizeMethod (fSynchronisedEnableAbortButton);
      finally
        Free;
      end;
  end;
end;

{ TQueryThread }

constructor TQueryThread.Create(aForm: Tl4jExplorerForm; aConnString,
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
  sl: TSL;
  db: TADOConnection;
  cs, qry: String;
  x: Integer;
begin
  fEnabled := True;
  with TIdSync.Create do
    try
      SynchronizeMethod (fSynchronisedEnableAbortButton);
    finally
      Free;
    end;
  try
    try
      UpdateStatus (1, 10, 'Preparing...');
      with TXml.Create do
      try
        LoadFromFile(fConnString, nil);
        if TagName <> 'DataSource' then
          raise Exception.CreateFmt('%s does not contain valid ConnectionString data', [fConnString]);
        cs := ReplaceStrings( Items.XmlValueByTag['ConnectionString']
                            , '%pwd%'
                            , XmlUtil.SimpleEncrypt(Items.XmlValueByTag['Password'])
                            , false
                            , false
                            );
      finally
        Free;
      end;
      xpScript := 'Exec sql ' + fQuery + 'loop { sqlLoop(); };';
      xpMoreData := True;
      xpFetched := False;
      if abortPressed then Exit;
      CoInitialize(nil);
      try
        db := TADOConnection.Create(nil);
        try
          db.ConnectionString := cs;
          db.Connected := True;
          xp := TExpress.Create(nil);
          sl := TSL.Create;
          try
            xp.BindString('ws.TimeStamp', TimeStamp);
            xp.BindString('ws.MessageId', MessageId);
            xp.BindString('ws.ServiceRequestorId', ServiceRequestorId);
            xp.BindString('ws.ServiceId', ServiceId);
            xp.BindString('ws.EventType', EventType);
            xp.BindString('ws.EventData', EventData);
            xp.BindString('ws.Dummy', Dummy);
            xp.BindString('ws.Param1', fParam1);
            xp.BindString('ws.Param2', fParam2);
            xp.BindString('ws.Param3', fParam3);
            xp.BindString('ws.Param4', fParam4);
            xp.BindFunction('sqlloop', @sqlLoop, VFV, '()');
            xp.OnNeedData := sl.xpNeedData;
            xp.Database := db;
            if abortPressed then Exit;
            xp.Prepare;
            if abortPressed then Exit;
            Msgs := TMsgList.Create;
            try
              Msgs.Sorted := True;
              UpdateStatus (2, 10, 'Querying datasource...');
              xp.Execute;
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
                  SynchronizeMethod (fSynchronisedHaveData);
                finally
                  Free;
                end;
              UpdateStatus (9, 10, 'Cleaning up...');
            finally
              Msgs.Clear;
              Msgs.Free;
            end;
          finally
            FreeAndNil(xp);
            FreeAndNil(sl);
          end;
        finally
          FreeAndNil(db);
        end;
      finally
        CoUninitialize;
      end;
    except
      on e: Exception do
      begin
        fMessage := e.Message;
        with TIdSync.Create do
          try
            SynchronizeMethod (fSynchronisedShowMessage);
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
        SynchronizeMethod (fSynchronisedEnableAbortButton);
      finally
        Free;
      end;
  end;
end;

{ TMsg }

constructor TMsg.Create;
begin
  FirstTimeStamp := 'Z';
end;

destructor TMsg.Destroy;
begin
  inherited;
end;

function TMsg.getAsText: String;
var
  x: Integer;
begin
  result := '<log4j_event timestamp="' + FirstTimeStamp + '">'
          + events
          + '</log4j_event>'
          + CRLF;
          ;
end;

procedure TSl.xpNeedData(Sender: TObject; var aMoreData: Boolean; var aData: String);
begin
  aMoreData := xpMoreData;
  aData := xpScript;
  xpMoreData := False;
end;

{ TMsgList }

procedure TMsgList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Msg[x].Free;
  inherited;
end;

constructor TMsgList.Create;
begin
  Sorted := True;
  Duplicates := dupError;
end;

function TMsgList.getAsText: String;
var
  x: Integer;
begin
  result := '<log4j_events>';
  for x := 0 to Count - 1 do
    result := result + Msg[x].AsText;
  result := result + '</log4j_events>';
end;

function TMsgList.getMsg(Index: Integer): TMsg;
begin
  result := (Objects[Index] as TMsg);
end;

procedure TMsgList.setMsg(Index: Integer; const Value: TMsg);
begin
  Objects[Index] := Value as TMsg;
end;

end.

