{.$define XMLDOM}
{.$define JANBOLICENSE}
unit xmlExplorerUnit;

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
  Dialogs, ComCtrls, ToolWin, ImgList, FormIniFilez, ActnList, Menus, FileUtil,
  Xmlz, Xsdz, ParserClasses, FilterDialog, OpenTwoFoldersUnit,
  ShowFolderDifferencesUnit
   ;

type

  { TMainForm }

  TMainForm = class(TForm)
    PasteAction: TAction;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ExitButton: TToolButton;
    ImageList: TImageList;
    ProgressBar: TProgressBar;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    openFileAction: TAction;
    editConfigAction: TAction;
    configAction1: TMenuItem;
    About1: TMenuItem;
    License1: TMenuItem;
    openLog4JAction: TAction;
    FolderAction: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Comparefolders1: TMenuItem;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure openLog4JActionExecute(Sender: TObject);
    procedure License1Click(Sender: TObject);
    procedure openFileActionExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure HelpAboutButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FolderActionExecute(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fConfigChanged: Boolean;
    xmlExplorerIniFileName, xmlExplorerDbName: String;
    function getXmlExplorerLicensed: Boolean;
    procedure OnGetLog4JText( aList: TStringList
                            ; aIndex, aColumn: Integer
                            ; var aText: WideString
                            );
    procedure OnFilter ( aList: TStringList
                       ; aIndex: Integer
                       ; var aPasses: Boolean
                       );
    function StringPassesFilter (aString: String): Boolean;
    procedure setXmlExplorerLicensed(const Value: Boolean);
    procedure setConfigChanged(const Value: Boolean);
    procedure OnlyWhenLicensed;
    procedure FinishedEvent(Sender: TObject);
    procedure IgnoreChanged (Sender: TObject);
  public
    FolderName1, FolderName2: String;
    configRead: Boolean;
    configFileName: String;
    xmlExplorerXsdFileName: String;
    xmlExplorerXsdDescr: TXsdDescr;
    configXsd: TXsd;
    SortSpecs: TStringList;
    ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn: TStringList;
    property xmlExplorerLicensed: Boolean read getXmlExplorerLicensed write setXmlExplorerLicensed;
    property configChanged: Boolean read fConfigChanged write setConfigChanged;
    procedure xmlExplorerInit;
    procedure ParserError(Sender: TObject; LineNumber, ColumnNumber,
      Offset: Integer; TokenString, Data: String);
    procedure configFromXml (aFileName: String; aXml: TXml);
    procedure ShowProgress (Sender: TObject; aProgress, aProgressMax: Integer);
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  xmlUtilz
   , ShowXmlUnit
   , ShowStringListUnit
   , ClipBrd
   , cbAbout
   , igGlobals
   , ErrorFound
// , IpmGunLicense
   , StrUtils
   ;
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

procedure TMainForm.FormShow(Sender: TObject);
var
  x, w: Integer;
begin
{}{
  w := 0;
  for x := 0 to ToolBar.ComponentCount - 1 do
    w := w + (ToolBar.Components[x] as TCustomControl).Width;
  Width := w + 100;
  Height := 88;
{}
{}{
  Width := 102;
{}
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  x, w, h: Integer;
  sl: TStringList;
begin
  w := Width;
  h := Height;
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  Width := w;
  Height := h;
  FolderName1 := IniFile.StringByName['FolderName1'];
  FolderName2 := IniFile.StringByName['FolderName2'];
  xmlUtil.doExpandFull := True;
  ignoreDifferencesOn := TStringList.Create;
  ignoreDifferencesOn.Sorted := True;
  ignoreDifferencesOn.Duplicates := dupIgnore;
  ignoreDifferencesOn.Text := IniFile.StringByName['ignoreDifferencesOn'];
  ignoreDifferencesOn.OnChange := IgnoreChanged;

  ignoreAddingOn := TStringList.Create;
  ignoreAddingOn.Sorted := True;
  ignoreAddingOn.Duplicates := dupIgnore;
  ignoreAddingOn.Text := IniFile.StringByName['ignoreAddingOn'];
  ignoreAddingOn.OnChange := IgnoreChanged;

  ignoreRemovingOn := TStringList.Create;
  ignoreRemovingOn.Sorted := True;
  ignoreRemovingOn.Duplicates := dupIgnore;
  ignoreRemovingOn.Text := IniFile.StringByName['ignoreRemovingOn'];
  ignoreRemovingOn.OnChange := IgnoreChanged;

  ignoreOrderOn := TStringList.Create;
  ignoreOrderOn.Sorted := True;
  ignoreOrderOn.Duplicates := dupIgnore;
  with TXml.Create do
  begin
    LoadFromString(IniFile.StringByName['ignoreOrderOn'], nil);
    if Name = 'ignoreOrderOn' then
    begin
      for x := 0 to Items.Count - 1 do
      begin
        with Items.XmlItems[x].Items do
        begin
          sl := TStringList.Create;
          sl.Text := XmlValueByTag['Keys'];
          ignoreOrderOn.AddObject(XmlValueByTag['Id'], sl);
        end;
      end;
    end;
  end;
  ignoreOrderOn.OnChange := IgnoreChanged;

  SortSpecs := TStringList.Create;
  xmlExplorerInit;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  x: Integer;
begin
  IniFile.StringByName  ['FolderName1'] := FolderName1;
  IniFile.StringByName  ['FolderName2'] := FolderName2;
  IniFile.StringByName ['ignoreDifferencesOn'] := ignoreDifferencesOn.Text;
  IniFile.StringByName ['ignoreAddingOn'] := ignoreAddingOn.Text;
  IniFile.StringByName ['ignoreRemovingOn'] := ignoreRemovingOn.Text;
  with TXml.CreateAsString('ignoreOrderOn', '') do
  begin
    for x := 0 to ignoreOrderOn.Count - 1 do
      with AddXml(TXml.CreateAsString('Element', '')) do
      begin
        AddXml(TXml.CreateAsString('Id', ignoreOrderOn.Strings[x]));
        AddXml(TXml.CreateAsString('Keys', (ignoreOrderOn.Objects[x] as TStringList).Text));
      end;
    IniFile.StringByName ['ignoreOrderOn'] := Text;
  end;
  IniFile.Save;
  IniFile.Free;
  FreeAndNil (SortSpecs);
  FreeAndNil (ignoreDifferencesOn);
  FreeAndNil (ignoreAddingOn);
  FreeAndNil (ignoreRemovingOn);
  with ignoreOrderOn do
  begin
    for x := 0 to Count - 1 do
      if Assigned (Objects[x]) then
        Objects[x].Free;
    Clear;
  end;
  FreeAndNil (ignoreOrderOn);
end;

procedure TMainForm.PasteActionExecute(Sender: TObject);
begin
  xmlUtil.presentString ('Clipboard as Text', Clipboard.AsText);
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HelpAboutButtonClick(Sender: TObject);
var
  xForm: TAboutBox;
begin
  Application.CreateForm(TAboutBox, xForm);
  try
    xForm.ShowModal;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TMainForm.IgnoreChanged(Sender: TObject);
begin
  configChanged := True;
end;

procedure TMainForm.ShowProgress(Sender: TObject; aProgress,
  aProgressMax: Integer);
begin
  ProgressBar.Max := aProgressMax;
  ProgressBar.Position := aProgress;
end;

procedure TMainForm.FolderActionExecute(Sender: TObject);
var
  xForm: TOpenTwoFoldersForm;
  dForm: TShowFolderDifferencesForm;
begin
  ShowProgress (nil, 0, 4);
  Application.CreateForm(TOpenTwoFoldersForm, xForm);
  try
    xForm.FolderName1 := FolderName1;
    xForm.FolderName2 := FolderName2;
    xForm.Caption := 'xmlExplorer - Show differences in folders';
    xForm.ShowModal;
    if xForm.ModalResult = mrOK then
    begin
      FolderName1 := xForm.FolderName1;
      FolderName2 := xForm.FolderName2;
      ShowProgress (nil, 1, 4);
      Application.CreateForm(TShowFolderDifferencesForm, dForm);
      try
        dForm.FolderName1 := FolderName1;
        dForm.FolderName2 := FolderName2;
        dForm.Caption := xForm.Caption;
        dForm.ignoreDifferencesOn := ignoreDifferencesOn;
        dForm.ignoreOrderOn := ignoreOrderOn;
        dForm.ignoreAddingOn := ignoreAddingOn;
        dForm.ignoreRemovingOn := ignoreRemovingOn;
        dForm.orderGroupsOn := SortSpecs;
        dForm.FinishedEvent := FinishedEvent;
        dForm.ShowModal;
      finally
        FreeAndNil (dForm);
      end;
    end;
  finally
    FreeAndNil (xForm);
    ShowProgress (nil, 0, 4);
  end;
end;

procedure TMainForm.OnlyWhenLicensed;
begin
  if (GetUserName <> 'BouwmanJW')
  and (GetUserName <> 'Bouwman')
  and (not xmlExplorerLicensed) then
    raise Exception.Create ('Disabled because you are not a licensed user.');
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.configFromXml(aFileName: String; aXml: TXml);
var
  xXml, yXml: TXml;
  x, y: Integer;
  sl: TStringList;
begin
  if aXml.Name <> 'xmlExplorerConfig' then
    raise Exception.Create ('Not an xmlExplorer config file');
  xmlUtil.IpmDescrsFromXml (aFileName, aXml.Items.XmlItemByTag['RecordDescriptors']);
  XmlUtil.ignoreDifferencesOn.Clear;
  xXml := aXml.Items.XmlItemByTag['CompareSpecs'];
  if Assigned (xXml) then
  begin
    xmlUtil.IgnoreOrderOfElements := xXml.Items.XmlBooleanByTagDef['IgnoreOrderOfElements', False];
    for x := 0 to xXml.Items.Count - 1 do
    begin
      if xXml.Items.XmlItems[x].Name = 'OrderGroup' then
      begin
        sl := TStringList.Create;
        SortSpecs.AddObject(xXml.Items.XmlItems[x].Items.XmlValueByTag['RecurringGroup'], sl);
        yXml := xXml.Items.XmlItems[x].Items.XmlItemByTag['SortKeys'];
        if Assigned (yXml) then
        begin
          for y := 0 to yXml.Items.Count - 1 do
            if yXml.Items.XmlItems[y].Name = 'Element' then
              sl.Add(yXml.Items.XmlItems[y].Value);
        end;
      end;
      if xXml.Items.XmlItems[x].Name = 'IgnoreDifferences' then
        with xXml.Items.XmlItems[x] do
          for y := 0 to Items.Count - 1 do
            if Items.XmlItems[y].Name = 'Element' then
              XmlUtil.ignoreDifferencesOn.Add(Items.XmlItems[y].Value);
    end;

  end;
{}{
  if (not xmlExplorerLicensed)
  and (xmlUtil.IpmDescrs.Count > 2) then
  begin
    while (xmlUtil.IpmDescrs.Count > 2) do
    begin
      xmlUtil.IpmDescrs.IpmDescrs [xmlUtil.IpmDescrs.Count - 1].Free;
      xmlUtil.IpmDescrs.Delete (xmlUtil.IpmDescrs.Count - 1);
    end;
    ShowMessage ( 'You are not a licensed user of xmlExplorer'
                + #$D#$A
                + #$D#$A
                + 'Therefore the number of record descriptors is limited to 2.'
                + #$D#$A
                + #$D#$A
                + 'Please contact your xmlExplorer provider.'
                );
  end;
{}
end;

procedure TMainForm.openFileActionExecute(Sender: TObject);
var
  xForm: TShowXmlForm;
  xXml: TXml;
  xXsdDescr: TXsdDescr;
begin
  ShowProgress (nil, 0, 8);
  with TOpenDialog.Create (nil) do
  try
    Options := Options + [ofFileMustExist];
    if Execute then
    begin
      ShowProgress (nil, 1, 8);
      try
        xmlUtil.presentString ( FileName
                              , ReadStringFromFile (FileName)
                              );
      finally
        ShowProgress (nil, 0, 8);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.setConfigChanged(const Value: Boolean);
begin
  fConfigChanged := Value;
  if Value then
    Caption := 'xmlExplorer *'
  else
    Caption := 'xmlExplorer';
end;

procedure TMainForm.ParserError(Sender: TObject; LineNumber, ColumnNumber,
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

procedure TMainForm.xmlExplorerInit;
begin
  xmlExplorerLicensed := False;
  xmlExplorerDbName := '';
  xmlExplorerIniFileName := Copy ( ParamStr(0)
                         , 1
                         , Length (ParamStr(0)) - 4
                         )
                   + 'Ini.xml'
                   ;
  if not FileExistsUTF8(xmlExplorerIniFileName) { *Converted from FileExists* } then exit;
  with TXml.Create do
  try
    LoadFromFile(xmlExplorerIniFileName, nil);
    if not (TagName = 'xmlExplorerIni') then
      raise Exception.Create(xmlExplorerIniFileName + ': Not a valid INI Xmlfile');
    xmlExplorerDbName := ExpandRelativeFileName ( ParamStr(0)
                                                , Items.XmlValueByTag ['licenseDatabase']
                                                );
    xmlExplorerXsdFileName := ExpandRelativeFileName ( ParamStr(0)
                                                     , Items.XmlValueByTag ['xmlExplorerXsd']
                                                     );
  finally
    Free;
  end;

  if FileExistsUTF8(xmlExplorerXsdFileName) { *Converted from FileExists* } then
  begin
    xmlExplorerXsdDescr := TXsdDescr.Create(1);
    try
      xmlExplorerXsdDescr.LoadXsdFromFile (xmlExplorerXsdFileName, nil);
  {$ifdef XMLDOM}
      if xmlExplorerXsdDescr.TypeDef.ElementDefs.Count = 1 then
        configXsd := xmlExplorerXsdDescr.TypeDef.ElementDefs.Xsds [0];
  {$endif}
    except
      ShowMessage ('Could not parse ' + xmlExplorerXsdFileName);
    end;
  end;

  {$ifdef JANBOLICENSE}
  if FileExistsUTF8(xmlExplorerDbName) { *Converted from FileExists* } then
    xmlExplorerLicensed := ValidateLicense (IniFile, 'xmlExplorer', xmlExplorerDbName);
  {$else}
  xmlExplorerLicensed := True;
  {$endif}
  if not xmlExplorerLicensed then
    ShowMessage ( 'Since you are not a licensed user,'
                + #$D#$A
                + 'xmlExplorer will only show the first part of values.'
                );
  configFileName := IniFile.StringByNameDef  ['configFileName', ''];
  xmlUtil.OnProgress := ShowProgress;
  xmlUtil.doEnableCompare := True;
  _OnParseErrorEvent := ParserError;
end;

procedure TMainForm.License1Click(Sender: TObject);
begin
  {$ifdef JANBOLICENSE}
  UpdateLicense  (IniFile, 'xmlExplorer', xmlExplorerDbName);
  xmlExplorerLicensed := ValidateLicense (IniFile, 'xmlExplorer', xmlExplorerDbName);
  {$else}
  xmlExplorerLicensed := True;
  {$endif}
end;

procedure TMainForm.setXmlExplorerLicensed(const Value: Boolean);
begin
  _xmllicensed := Value;
end;

function TMainForm.getXmlExplorerLicensed: Boolean;
begin
  result := _xmllicensed;
end;

procedure TMainForm.OnGetLog4JText(aList: TStringList; aIndex,
  aColumn: Integer; var aText: WideString);
  function _getAtt (aKey, aString: String): String;
  var
    x: Integer;
  begin
    result := '';
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
begin
  case aColumn of
   -1: aText := aList.Strings[aIndex];
    0: aText := IntToStr (aIndex);
    1: aText := IntToStr (Length (aList.Strings[aIndex]));
    2: aText := _getAtt ('timestamp', aList.Strings[aIndex]);
    3: aText := _getAtt ('level', aList.Strings[aIndex]);
    4: aText := _getAtt ('<ServiceId>', aList.Strings[aIndex]);
    5: aText := _getAtt ('<ServiceRequestorId>', aList.Strings[aIndex]);
    6: aText := _getAtt ('<MessageId>', aList.Strings[aIndex]);
    7: aText := _getAtt ('<FaultCode>', aList.Strings[aIndex]);
    8: aText := _getAtt ('<FaultDescription>', aList.Strings[aIndex]);
  end;
end;

function TMainForm.StringPassesFilter(aString: String): Boolean;
begin
  result := (   (FilterDlg.FindEdit0.Text = '')
             or (Pos (FilterDlg.FindEdit0.Text, aString) > 0)
            )
        and (   (FilterDlg.FindEdit1.Text = '')
             or (Pos (FilterDlg.FindEdit1.Text, aString) > 0)
            )
        and (   (FilterDlg.FindEdit2.Text = '')
             or (Pos (FilterDlg.FindEdit2.Text, aString) > 0)
            )
          ;
end;

procedure TMainForm.FinishedEvent(Sender: TObject);
begin
  ShowProgress (nil, 0, 8);
end;

procedure TMainForm.OnFilter(aList: TStringList; aIndex: Integer;
  var aPasses: Boolean);
begin
  aPasses := StringPassesFilter(aList.Strings[aIndex]);
end;

procedure TMainForm.openLog4JActionExecute(Sender: TObject);
var
  s, ss: String;
  sl: TStringList;
  x: Integer;
  sp, ep, xp: PChar;
  xOptions: TStringSearchOptions;
  sSearchString, eSearchString, xSearchString: String;
  xForm: TShowStringListForm;
begin
  XmlUtil.PushCursor(crHourGlass);
  sSearchString := '<log4j_event';
  eSearchString := '</log4j_event>';
  xOptions := [soDown, soMatchCase];
  try
    with TOpenDialog.Create(nil) do
    try
      Options := Options + [ofAllowMultiSelect];
      if not Execute then
        Exit;
      FilterDlg.Caption := 'Configure filter';
      FilterDlg.ShowModal;
      if FilterDlg.ModalResult <> mrOk then
        Exit;
      ShowProgress (nil, 1, 4 + Files.Count);
      if FilterDlg.FindEdit0.Text = '' then
        xSearchString := eSearchString
      else
        xSearchString := FilterDlg.FindEdit0.Text;
      try
        ShowProgress (nil, 2, 4 + Files.Count);
        sl := TStringList.Create;
        try
          for x := 0 to Files.Count - 1 do
          begin
            s := ReadStringFromFile (Files.Strings[x]);
            ep := @s[1];
            xp := strUtils.SearchBuf(ep, Length(s), 0, 0, xSearchString, xOptions);
            while Assigned (xp) do
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
                  ss := Copy (s, sp - @s[1] + 1, ep - sp);
                  if StringPassesFilter (ss) then
                    sl.Add (ss);
                  xp := strUtils.SearchBuf(ep, Length (s) - (ep - @s[1]), 0, 0, xSearchString, xOptions);
                end;
              end;
            end;
            s := '';
            ShowProgress (nil, 2 + x, 4 + Files.Count);
          end;
          Application.CreateForm(TShowStringListForm, xForm);
          try
            xForm.Columns := TStringList.Create;
            try
              xForm.Columns.Add ('number');
              xForm.Columns.Add ('size');
              xForm.Columns.Add ('timestamp');
              xForm.Columns.Add ('level');
              xForm.Columns.Add ('ServiceId');
              xForm.Columns.Add ('ServiceRequestorId');
              xForm.Columns.Add ('MessageId');
              xForm.Columns.Add ('FaultCode');
              xForm.Columns.Add ('FaultDescription');
              xForm.Data := sl;
              xForm.OnGetText := OnGetLog4JText;
              xForm.OnFilter := OnFilter;
              ShowProgress (nil, 0, 4 + Files.Count);
              xForm.ShowModal;
            finally
              xForm.Columns.Free;
            end;
          finally
            FreeAndNil (xForm);
          end;
        finally
          ShowProgress (nil, 3, 4);
          for x := 0 to sl.Count - 1 do
            sl.Strings[x] := '';
          sl.Clear;
          sl.free;
        end;
      finally
        s := '';
        ShowProgress (nil, 0, 4 + Files.Count);
      end;
    finally
      for x := 0 to sl.Count - 1 do
        sl.Strings[x] := '';
      sl.Clear;
      sl.free;
    end;
  finally
    XmlUtil.PopCursor;
  end;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  xForm: TShowXmlForm;
  x: Integer;
  xXml: TXml;
  xXsdDescr: TXsdDescr;
begin
  Application.BringToFront;
  SetForegroundWindow(Self.Handle);
//if DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) > 1 then
//  Raise Exception.Create ('reqExplorer can not open multiple files');
  if Low(FileNames) = High(FileNames) then
  begin
    x:=Low(FileNames);
    ShowProgress (nil, 1, 8);
    try
      xmlUtil.presentString ( FileNames[x]
                            , ReadStringFromFile (FileNames[x])
                            );
    finally
      ShowProgress (nil, 0, 8);
    end;
    exit;
  end;
  ShowProgress (nil, 1, 4);
  try
    xXml := TXml.Create;
    try
      xXml.Name := 'xmlContainer';
      for x := Low(FileNames) to High(FileNames) do
      begin
        with xXml.AddXml (TXml.CreateAsString ('File', '')) do
        begin
          AddXml (TXml.CreateAsString('FileName', FileNames[x]));
          with AddXml (TXml.Create) do
          begin
            LoadFromFile(FileNames[x], nil);
            if Name = '' then
              raise Exception.Create('No XML in ' + FileNames[x]);
          end;
        end;
      end;
      ShowProgress (nil, 2, 4);
      xXsdDescr := TXsdDescr.Create(1);
      xmlUtil.CreateXsdFromXml (xXsdDescr, xXml, True);
      try
        Application.CreateForm(TShowXmlForm, xForm);
        try
          xForm.Caption := 'Dropped files';
          xForm.Bind := xXml;
          ShowProgress (nil, 0, 4);
          xForm.isReadOnly := True;
          xForm.ShowModal;
        finally
          FreeAndNil (xForm);
        end;
      finally
        xXsdDescr.Free;
      end;
    finally
      xXml.Free;
    end;
  finally
    ShowProgress (nil, 0, 4);
  end;
end;

end.
