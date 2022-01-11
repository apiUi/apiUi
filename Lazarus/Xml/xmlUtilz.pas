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
unit xmlUtilz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, Forms, Controls, ComCtrls, StdCtrls, Graphics, LazFileUtils
   , xmlio
   , Bind
   , Xmlz
   , Xsdz
   , Ipmz
   , ParserClasses
   {$ifndef NoGUI}
   , HtmlView
   {$endif}
   , MarkdownUtils
   , MarkdownProcessor
   ;



const cursorStackSize = 32;
type
  TProcedure = procedure of Object;
  TOnProgress = procedure ( Sender: TObject
                          ; aProgress, aProgressMax: Integer
                          ) of Object;
  TSchemaType = (stJsonType, stWsdlType, stXsdType);
  TLanguageStreamType = (tlsXml, tlsJson, tlsYaml);
  TImageIndex = (iiTree, iiGrid, iiUnchecked, iiChecked, iiDateTime,  iiDate, iiTime, iiEnumeration, iiExtendLevel, iiUrl);

type

{ TXmlUtil }

 TXmlUtil = class(TObject)
private
  pdfFileCounter, rtfFileCounter, docxFileCounter, htmlFileCounter: Integer;
    fAcquireLock: TProcedure;
    fReleaseLock: TProcedure;
    cursorStack: array [0..cursorStackSize] of TCursor;
    cursorStackIndex: Integer;
  procedure fShowProgress (aSender: TObject; aProgress, aProgressMax: Integer);
    function getAcquireLock: TProcedure;
    function getReleaseLock: TProcedure;
    function BooleanAsString(aBoolean: Boolean): String;
  procedure _ignoreNilProc;
public
  NewValue: String;
  newChecked: Boolean;
  doConfirmRemovals: Boolean;
  doCollapseOnUncheck: Boolean;
  doExpandOnCheck: Boolean;
  doExpandFull: Boolean;
  doEnableCompare: Boolean;
  SearchString: String;
  SearchScope: Integer;
  SearchIn: Integer;
  SearchUseRegExp: Boolean;
  OnProgress: TOnprogress;
  IpmDescrs: TIpmDescrs;
  IgnoreOrderOfElements: Boolean;
  ignoreDifferencesOn: TJBStringList;
  property AcquireLock: TProcedure read getAcquireLock write fAcquireLock;
  property ReleaseLock: TProcedure read getReleaseLock write fReleaseLock;
  function SimpleEncrypt(Source: AnsiString): AnsiString;
  function FindIpmDescr (aString: String): TIpmDescr;
  procedure IpmDescrsFromXml (aFileName: String; aXml: TXml);
  function IpmDescrsAsXml: TXml;
  procedure EmbeddedXml (aXml: TXml);
  procedure ShowSoapBodyInGrid (aSoapMessageString: String);
  function isDeleteAllowed (aBind: TCustomBindable; doPrompt: Boolean): Boolean;
  function Delete (aXml: TXml): TXml;
  function isAddAllowed (aBind: TCustomBindable; doPrompt: Boolean): Boolean;
  procedure SaveXmlWithFileNames (aFileName: String; aXml: TXml; aUseRelativeNames, aOnlyWhenChecked: Boolean);
  function CheckAndPromptFileNames(aFileName: String; aXml: TXml; aOnlyWhenChecked: Boolean): Boolean;
  procedure ShowInfoForm (aCaption: String; aInfoString: String);
  function isCheckAllowed (aBind: TCustomBindable): Boolean;
  function isDateTime (aBind: TCustomBindable): Boolean;
  function isDate (aBind: TCustomBindable): Boolean;
  function isTime (aBind: TCustomBindable): Boolean;
  function isEnumeration (aBind: TCustomBindable): Boolean;
  function isHelpSelection (aBind: TCustomBindable): Boolean;
  function isBoolean (aBind: TCustomBindable): Boolean;
  function isEditAllowed (aBind: TCustomBindable): Boolean;
  function isEditSupported (aBind: TCustomBindable): Boolean;
  function isExtendAdviced (aBind: TCustomBindable): Boolean;
  function isTreeAdviced (aBind: TCustomBindable): Boolean;
  function isGridAdviced (aBind: TCustomBindable): Boolean;
  function isPasswordType (aBind: TCustomBindable): Boolean;
  function getImageImdex (aBind: TCustomBindable; aOffset: Integer): Integer;
  function editXml (aBind: TCustomBindable; aDoUseGrid, aReadOnly: Boolean): Boolean;
  procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
  procedure CheckValidity(aBind: TCustomBindable);
  procedure CopyToClipboard (aLanguageStreamType: TLanguageStreamType; aBind: TCustomBindable);
  procedure PasteFromClipboard (aBind: TCustomBindable);
  procedure Populate (aBind: TCustomBindable; aViewType: TxvViewType);
  procedure Validate (aBind: TCustomBindable);
  procedure ZoomAsBase64 (aBind: TCustomBindable);
  function ShowB64EncodedDocument (aXml: TXml): Boolean;
  procedure presentString (aCaption, aString: String);
  procedure presentAsRTF (aCaption, aString: String);
  procedure presentAsPDF (aCaption, aString: String);
  procedure presentAsDOCX (aCaption, aString: String);
  procedure presentAsHTML (aCaption, aString: String);
  procedure presentAsText (aCaption, aString: String);
  procedure presentAsIpm (aIpm: TIpmItem; aCaption, aString: String);
  procedure ZoomAsPDF (aBind: TCustomBindable);
  procedure ZoomAsText (aBind: TCustomBindable; aReadOnly: Boolean);
  procedure ZoomAsXml (aBind: TCustomBindable; aReadOnly: Boolean);
  function BindCaption (aBind: TCustomBindable): String;
  function ViewAsXml (aBind: TCustomBindable; aReadOnly: Boolean): Boolean;
  function ViewAsGrid (aBind: TCustomBindable; aReadOnly: Boolean): Boolean;
  procedure ListXsdProperties (aListView: TListView; aBind: TCustomBindable);
  procedure ListXsdEnumerations (aListView: TListView; aBind: TCustomBindable);
  procedure ListXsdDocumentation ( aMemo: TMemo
                                 ; aBind: TCustomBindable
                                 ; aShowPath: Boolean
                                 ; aShowValue: Boolean
                                 ); overload;
  procedure ListXsdDocumentation ( aHtmlViewer: THtmlViewer
                                 ; aBind: TCustomBindable
                                 ; aShowPath: Boolean
                                 ; aShowValue: Boolean
                                 ); overload;
  procedure PushCursor (aNewCursor: TCursor);
  procedure PopCursor;
  constructor Create;
  destructor Destroy;
end;

function IsExistingFile (aRefName, aFileName: String): Boolean;
function IsExistingFolder (aRefName, aFolderName: String): Boolean;
function CheckAndPromptForExistingFile (aCaption, aRefName, aFileName: String): String;
function CheckAndPromptForExistingFolder (aCaption, aRefName, aFolderName: String): String;
function EditXmlXsdBased ( aCaption, aXsdPath, aInitialFocus, aValidateDuplicatesOn: String
                         ; aReadOnly, aAsGrid: Boolean
                         ; initialExpandStyle: TBindExpandStyle
                         ; aRootXsd: TXsd
                         ; aXml: TXml
                         ; aOnlyWhenChanged: Boolean
                         ; aConfirmCallBack: TProcedureBoolean = nil
                         ): Boolean;
procedure ShowText (aCaption, aText: String);
procedure ShowXml (aCaption: String; aXml: TXml);


var
  XmlUtil: TXmlUtil;

implementation

uses
{$IFnDEF FPC}
  ShellApi, Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Dialogs
{$ifndef NoGUI}
   , ShowXmlUnit
{$endif}
   , ShowTextUnit
   , ChooseEnumUnit
   , xsdDateTimeFormUnit
   , EditListValuesUnit
   , igGlobals
   , StrUtils
   , SysUtils
   , ClipBrd
   , XmlGridUnit
// , XsBuiltIns
   , base64
   ;

procedure ShowText (aCaption, aText: String);
var
  xForm: TShowTextForm;
begin
  Application.CreateForm(TShowTextForm, xForm);
  try
    xForm.Caption := aCaption;
    xForm.Memo.Lines.Text := aText;
    xForm.ShowModal;
  finally
    FreeAndNil(xForm);
  end;
end;

procedure ShowXml(aCaption: String; aXml: TXml);
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

function EditXmlXsdBased ( aCaption, aXsdPath, aInitialFocus, aValidateDuplicatesOn: String
                         ; aReadOnly, aAsGrid: Boolean
                         ; initialExpandStyle: TBindExpandStyle
                         ; aRootXsd: TXsd
                         ; aXml: TXml
                         ; aOnlyWhenChanged: Boolean
                         ; aConfirmCallBack: TProcedureBoolean = nil
                         ): Boolean;
var
  cnfXml: TXml;
  savexsdMaxDepthBillOfMaterials: Integer;
  xForm: TShowXmlForm;
  gForm: TXmlGridForm;
begin
  result := False;
  if aXsdPath = '' then
    aXsdPath := aRootXsd.ElementName;
  savexsdMaxDepthBillOfMaterials := xsdMaxDepthBillOfMaterials;
  try
    cnfXml := TXml.Create (-10000, aRootXsd.FindXsd(aXsdPath));
  finally
    xsdMaxDepthBillOfMaterials := savexsdMaxDepthBillOfMaterials;
  end;
  try
    cnfXml.CheckDownLine (False);
//  aXml.CheckDownline(True);
    cnfXml.LoadValues (aXml, False, True);
    if aAsGrid then
    begin
      Application.CreateForm(TXmlGridForm, gForm);
      try
        gForm.Caption := gForm.progName + ' - ' + aCaption;
        gForm.Xml := cnfXml;
        gForm.isReadOnly := aReadOnly;
        gForm.initialExpandStyle := initialExpandStyle;
        gForm.doShowCancelButton := True;
        gForm.ValidateDuplicatesOn := aValidateDuplicatesOn;
        gForm.ConfirmCallBack := aConfirmCallBack;
        gForm.ShowModal;
        result := (gForm.ModalResult = mrOk)
              and (   gForm.stubChanged
                   or (not aOnlyWhenChanged)
                  );
        if result then
        begin
          aXml.CheckDownline(False);
          aXml.LoadValues(cnfXml, True, True);
        end;
      finally
        gForm.Free;
      end;
    end
    else
    begin
      Application.CreateForm(TShowXmlForm, xForm);
      try
        xForm.Caption := xForm.progName + ' - ' + aCaption;
        xForm.Bind := cnfXml;
        xForm.isReadOnly := aReadOnly;
        xForm.initialExpandStyle := initialExpandStyle;
        xForm.doShowCancelButton := True;
        xForm.InitialFocusOn := aInitialFocus;
        xForm.ValidateDuplicatesOn := aValidateDuplicatesOn;
        xForm.ConfirmCallBack := aConfirmCallBack;
        xForm.ShowModal;
        result := (xForm.ModalResult = mrOk)
              and (   xForm.isChanged
                   or (not aOnlyWhenChanged)
                  );
        if result then
        begin
          aXml.CheckDownline(False);
          aXml.LoadValues(cnfXml, True, True);
        end;
      finally
        xForm.Free;
      end;
    end;
  finally
    cnfXml.Free;
  end;
end;

function IsExistingFile (aRefName, aFileName: String): Boolean;
begin
  result := FileExistsUTF8(uncFilename(ExpandRelativeFileName (aRefName, aFileName)));
end;

function IsExistingFolder (aRefName, aFolderName: String): Boolean;
begin
  result := DirectoryExistsUTF8(uncFilename(ExpandRelativeFileName (aRefName, aFolderName)));
end;

function CheckAndPromptForExistingFolder (aCaption , aRefName ,
  aFolderName : String ): String ;
begin
  result := ExpandRelativeFileName (aRefName, aFolderName);
  while not DirectoryExistsUTF8(result) { *Converted from FileExists* } do
  begin
    if (MessageDlg ( aCaption
                   + CRLF
                   + CRLF
                   + 'Folder: '
                   + result
                   + CRLF
                   + 'does not exist, try to select another one'
                   , mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      with TSelectDirectoryDialog.Create(nil) do
      try
        FileName := result;
        Title := 'Try to select another folder';
        if Execute then
        begin
          result := FileName;
        end;
      finally
        Free;
      end;
    end
    else
      raise Exception.Create('Folder: ' + result + ' not found');
  end;
end;

function CheckAndPromptForExistingFile (aCaption, aRefName, aFileName: String): String;
begin
  result := ExpandRelativeFileName (aRefName, aFileName);
  while not FileExistsUTF8(result) { *Converted from FileExists* } do
  begin
    if (MessageDlg ( aCaption
                   + CRLF
                   + CRLF
                   + 'File: '
                   + result
                   + CRLF
                   + 'does not exist, try to open another one'
                   , mtConfirmation, [mbYes, mbNo], 0
                   ) = mrYes
       ) then
    begin
      with TOpenDialog.Create(nil) do
      try
        FileName := result;
        Title := 'Try to open another file';
        if Execute then
        begin
          result := FileName;
        end;
      finally
        Free;
      end;
    end
    else
      raise Exception.Create('File: ' + result + ' not found');
  end;
end;

{ TXmlUtil }

procedure TXmlUtil.ShowInfoForm(aCaption: String; aInfoString: String);
begin
  ShowText (aCaption, aInfoString);
end;

procedure TXmlUtil.SaveXmlWithFileNames (aFileName: String; aXml: TXml; aUseRelativeNames, aOnlyWhenChecked: Boolean);
begin
  with TXml.Create (0, aXml.Xsd) do
  try
    LoadValues(aXml, False, aOnlyWhenChecked);
    if aUseRelativeNames then
      SetFileNamesRelative(aFileName);
    with TJBStringList.Create do
    try
      Text := AsText(False,0,True,False);
      SaveToFile(aFileName);
    finally
      Free;
    end;
  finally
    Free;
  end;
end;

function TXmlUtil.CheckAndPromptFileNames(aFileName: String; aXml: TXml; aOnlyWhenChecked: Boolean): Boolean;
  function _check (aFileName: String; aXml: TXml; aOnlyWhenChecked: Boolean): Boolean;
  var
    x: Integer;
  begin
    result := false;
    if aOnlyWhenChecked
    and (not aXml.Checked)
      then Exit;
    if Assigned (aXml.TypeDef) then
    begin
      if (aXml.TypeDef.Name = 'FileNameType') then
      begin
        aXml.Value := uncFilename(ExpandRelativeFileName (aFileName, aXml.Value)); // this is not a modification
        result := not IsExistingFile(aFileName, aXml.Value);
        if result then
          aXml.Value := CheckAndPromptForExistingFile(aXml.FullIndexCaption, aFileName, aXml.Value);
      end;
      if (aXml.TypeDef.Name = 'FolderNameType') then
      begin
        aXml.Value := uncFilename(ExpandRelativeFileName (aFileName, aXml.Value)); // this is not a modification
        result := not IsExistingFolder(aFileName, aXml.Value);
        if result then
          aXml.Value := CheckAndPromptForExistingFolder(aXml.FullIndexCaption, aFileName, aXml.Value);
      end;
    end;
    for x := 0 to aXml.Items.Count - 1 do
      if _check(aFileName, aXml.Items.XmlItems[x], aOnlyWhenChecked) then
        result := True;
  end;
begin
  result := false;
  if not Assigned (aXml) then Exit;
  if not Assigned (aXml.TypeDef) then Exit;
  result := _check(aFileName, aXml, aOnlyWhenChecked);
end;

function TXmlUtil.ShowB64EncodedDocument(aXml: TXml): Boolean;
var
  x: Integer;
begin
  result := False;
  if (Copy(aXml.Value, 1, 6) = 'JVBERi')
  or AnsiStartsStr(base64DocxStartStr, aXml.Value)
  or AnsiStartsStr(base64RtfStartStr, aXml.Value)
  or (UpperCase(Copy(aXml.Value, 1, 7)) = 'HTTP://')
  or (UpperCase(Copy(aXml.Value, 1, 8)) = 'HTTPS://') then
  begin
    presentString(aXml.Name, aXml.Value);
    result := True;
  end
  else
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      result := ShowB64EncodedDocument (aXml.Items.XmlItems[x]);
      if result then exit;
    end;
  end;
end;

procedure TXmlUtil.ShowSoapBodyInGrid (aSoapMessageString: String);
var
  x, swapMaxDepthXmlGen: Integer;
  xXml, sXml: TXml;
  xForm: TXmlGridForm;
  xXsdPropertiesVisible: Boolean;
  xXsdDescr: TXsdDescr;
begin
  swapMaxDepthXmlGen := xsdMaxDepthXmlGen;
  xsdMaxDepthXmlGen := defaultXsdMaxDepthXmlGen;
  sXml := TXml.Create;
  try
    sXml.LoadFromString(aSoapMessageString, nil);
    if sXml.Name = '' then
    try
      sXml.LoadJsonFromString(aSoapMessageString, nil);
    except
      ShowInfoForm('failed parsing', aSoapMessageString);
      Exit;
    end;
    xXml := sXml;
    if NameWithoutPrefix(sXml.TagName) = 'Envelope' then
    begin
      for x := 0 to sXml.Items.Count - 1 do
      begin
        xXml := sXml.Items.XmlItems [x];
        if (NameWithoutPrefix(xXml.TagName) = 'Body')
        and (xXml.Items.Count = 1) then
        begin
          xXml := xXml.Items.XmlItems[0];
          if xmlUtil.ShowB64EncodedDocument (xXml) then
            exit;
        end;
      end;
    end;
    xXsdDescr := TXsdDescr.Create;
    CreateXsdFromXml(xXsdDescr, xXml, True);
    try
      try
        Application.CreateForm(TXmlGridForm, xForm);
        xXsdPropertiesVisible := xForm.XsdPropertiesVisible;
        try
          xForm.isReadOnly := True;
          xForm.Xml := xXml;
          xForm.XsdPropertiesVisible := False;
          xForm.ShowPropertiesAction.Enabled := False;
          xForm.ShowModal;
        finally
          xForm.XsdPropertiesVisible := xXsdPropertiesVisible;
          FreeAndNil (xForm);
        end;
      finally
      end;
    finally
      xXsdDescr.Free;
    end;
  finally
    xsdMaxDepthXmlGen := swapMaxDepthXmlGen;
    sXml.Free;
  end;
end;

function TXmlUtil.SimpleEncrypt(Source: AnsiString): AnsiString;
var
  Index: Integer;
  EncryptionSeed: AnsiString;
begin
  EncryptionSeed := 'th^ruh54bdkjbkjb4k458&*';
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := AnsiChar((Ord(EncryptionSeed[Index mod Length(EncryptionSeed)]) xor Ord(Source[Index])));
end;

procedure TXmlUtil.CopyToClipboard(aLanguageStreamType: TLanguageStreamType; aBind: TCustomBindable);
begin
  if aBind is TXmlAttribute then
    raise Exception.Create('Not implemented for XML attributes');
  if aBind is TXml then
  with aBind as TXml do
  begin
    case (aLanguageStreamType) of
      tlsXml:
        Clipboard.AsText := AsText ( False
                                   , 0
                                   , True
                                   , False
                                   );
      tlsJson: Clipboard.AsText :=  StreamJSON(0, True);
      tlsYaml: Clipboard.AsText :=  StreamYAML(0, True);
      else
        Clipboard.AsText :=  StreamJSON(0, True);
    end;
  end;
  if aBind is TIpmItem then
  begin
    with (aBind as TIpmItem).AsXml do
    begin
      Clipboard.AsText := AsText ( False
                                 , 0
                                 , True
                                 , False
                                 );
      Free;
    end;
  end;
end;

function TXmlUtil.Delete(aXml: TXml): TXml;
begin
  if Assigned (aXml.Parent) then
  begin
    AcquireLock;
    try
      result := (aXml.Parent as TXml).DeleteChild(aXml);
    finally
      ReleaseLock;
    end;
  end
  else
  begin
    raise Exception.Create('Not allowed to delete root');
{
    aXml.Free;
    result := nil;
}
  end;
end;

function TXmlUtil.editXml(aBind: TCustomBindable; aDoUseGrid, aReadOnly: Boolean): Boolean;
  function _editElementValue: Boolean;
  var
    xChooseEnumForm: TChooseEnumForm;
    xDateTimeForm: TxsdDateTimeForm;
    xInfoForm: TShowTextForm;
  begin
    result := False;
    if (    (aBind is TXmlAttribute)
        and (Assigned ((aBind as TXmlAttribute).XsdAttr))
        and ((aBind as TXmlAttribute).XsdAttr.Enumerations.Count > 0)
        and (not aReadOnly)
       )
    then begin
      Application.CreateForm(TChooseEnumForm,xChooseEnumForm);
      try
        xChooseEnumForm.Enums := (aBind as TXmlAttribute).XsdAttr.Enumerations;
        xChooseEnumForm.ChoosenString := (aBind as TXmlAttribute).Value;
        xChooseEnumForm.Caption := 'Choose value for ' + (aBind as TXmlAttribute).Name;
        xChooseEnumForm.ShowModal;
        result := (xChooseEnumForm.ModalResult = mrOk);
        NewValue := xChooseEnumForm.ChoosenString;
      finally
        xChooseEnumForm.Free;
      end;
    end;
    if (    (aBind is TXml)
        and (Assigned ((aBind as TXml).Xsd))
        and ((aBind as TXml).TypeDef.Enumerations.Count > 0)
        and (not aReadOnly)
       )
    then begin
      Application.CreateForm(TChooseEnumForm,xChooseEnumForm);
      try
        xChooseEnumForm.Enums := (aBind as TXml).TypeDef.Enumerations;
        xChooseEnumForm.ChoosenString := (aBind as TXml).Value;
        xChooseEnumForm.Caption := 'Choose value for ' + (aBind as TXml).Tagname;
        xChooseEnumForm.ShowModal;
        result := (xChooseEnumForm.ModalResult = mrOk);
        NewValue := xChooseEnumForm.ChoosenString;
      finally
        xChooseEnumForm.Free;
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and ((aBind as TXml).TypeDef.BaseDataTypeName = 'boolean')
    and (not (aBind as TXml).Xsd.isReadOnly)
{}
    and (not aReadOnly)
{}
    then begin
{}
      if (aBind.Value <> 'true')
      and (aBind.Value <> '1') then
        NewValue := 'true'
      else
        NewValue := 'false';
      result := True;
{}{
      Application.CreateForm(TChooseStringForm,xChooseStringForm);
      try
        xChooseStringForm.ListBox.Clear;
        xChooseStringForm.ListBox.Items.Add ('false');
        xChooseStringForm.ListBox.Items.Add ('true');
        xChooseStringForm.ListBox.Items.Add ('0');
        xChooseStringForm.ListBox.Items.Add ('1');
        xChooseStringForm.ChoosenString := (aBind as TXml).Value;
        xChooseStringForm.Caption := 'Choose value for ' + (aBind as TXml).Tagname;
        xChooseStringForm.ShowModal;
        result := (xChooseStringForm.ModalResult = mrOk);
        NewValue := xChooseStringForm.ChoosenString;
      finally
        xChooseStringForm.Free;
      end;
{}
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (   ((aBind as TXml).TypeDef.BaseDataTypeName = 'date')
         or ((aBind as TXml).TypeDef.BaseDataTypeName = 'dateTime')
        )
    and (not aReadOnly) then
    begin
      Application.CreateForm(TxsdDateTimeForm, xDateTimeForm);
      try
        xDateTimeForm.Caption := 'Enter value for ' + (aBind as TXml).Tagname;
        if ((aBind as TXml).TypeDef.BaseDataTypeName = 'dateTime') then
          xDateTimeForm.dtFormat := dtfDateTime;
        if ((aBind as TXml).TypeDef.BaseDataTypeName = 'date') then
          xDateTimeForm.dtFormat := dtfDate;
        xDateTimeForm.XsdDateTime := (aBind as TXml).Value;
        xDateTimeForm.ShowModal;
        result := (xDateTimeForm.ModalResult = mrOk);
        NewValue := xDateTimeForm.xsdDateTime;
      finally
        xDateTimeForm.Free;
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (   ((aBind as TXml).TypeDef.BaseDataTypeName = 'base64Binary')
        )
    and (not aReadOnly) then
    begin
      Application.CreateForm(TShowTextForm, xInfoForm);
      try
        xInfoForm.Caption := 'Enter value for ' + (aBind as TXml).Tagname;
        xInfoForm.Memo.Lines.Text := DecodeStringBase64 ((aBind as TXml).Value);
        xInfoForm.EditAllowed := not ReadOnly;
        xInfoForm.ShowModal;
        result := (xInfoForm.ModalResult = mrOK)
              and (not ReadOnly);
        NewValue := EncodeStringBase64 (xInfoForm.Memo.Lines.Text);
      finally
        xInfoForm.Free;
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (   ((aBind as TXml).TypeDef.Name = 'FileNameType')
         or ((aBind as TXml).TypeDef.Name = 'file')
        )
    and (not ReadOnly) then
    begin
      with TOpenDialog.Create (nil) do
      try
        Title := 'Filename for ' + (aBind as TXml).Tagname;
        FileName := (aBind as TXml).Value;
        result := Execute;
        NewValue := FileName;
      finally
        Free;
      end;
    end;
    if (aBind is TXml)
    and Assigned ((aBind as TXml).Xsd)
    and (   ((aBind as TXml).TypeDef.Name = 'NameValuePairsStringType')
        ) then
    begin
      Application.CreateForm(TEditListValuesForm, EditListValuesForm);
      try
        EditListValuesForm.Caption := 'Edit environment variables';
        EditListValuesForm.isReadOnly := ReadOnly;
        EditListValuesForm.ValueListEditor.Strings.Text := (aBind as TXml).Value;
        EditListValuesForm.ShowModal;
        Result := (EditListValuesForm.ModalResult = mrOk);
        NewValue := EditListValuesForm.ValueListEditor.Strings.Text;
      finally
        FreeAndNil(EditListValuesForm);
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (   ((aBind as TXml).TypeDef.Name = 'FolderNameType')
        )
    and (not ReadOnly) then
    begin
      with TSelectDirectoryDialog.Create (nil) do
      try
        Title := 'Foldername for ' + (aBind as TXml).Tagname;
        FileName := (aBind as TXml).Value;
        result := Execute;
        NewValue := FileName;
      finally
        Free;
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (   ((aBind as TXml).TypeDef.Name = 'htmlColorType')
        )
    and (not ReadOnly) then
    begin
      with TColorDialog.Create (nil) do
      try
        Color := HtmlToColor ((aBind as TXml).Value);
//      Options := Options + [cdFullOpen, cdAnyColor];
        result := Execute;
        NewValue := ColorToHtml (Color);
      finally
        Free;
      end;
    end;
    if (aBind is TXml)
    and (Assigned ((aBind as TXml).Xsd))
    and (Assigned ((aBind as TXml).Xsd.sType.OnDoSelectValue))
    and (not ReadOnly) then
    begin
      (aBind as TXml).Xsd.sType.OnDoSelectValue(result, NewValue, (aBind as TXml).Value);
    end;
    if result then
    begin
      AcquireLock;
      try
        result := False; // is it really a change in the xml ? ? ?
        if NewValue = '&nil' then
        begin
          if aBind.Checked then
          begin
            aBind.Checked := False;
            result := True;
          end;
        end
        else
        begin
          if (NewValue <> aBind.Value)
          or (not aBind.CheckedAllUp) then
          begin
            aBind.Value := NewValue;
            aBind.Checked := True;
            result := True;
          end;
        end;
      finally
        ReleaseLock;
      end;
    end;
  end;
begin
  result := False;
  if (aBind is TXmlAttribute)
  and (Assigned ((aBind as TXmlAttribute).XsdAttr)) then
    result := _editElementValue;
  if (aBind is TXml)
  and (Assigned ((aBind as TXml).Xsd)) then
  begin
    if Assigned ((aBind as TXml).Xsd.EditProcedure) then
    begin
      result := (aBind as TXml).Xsd.EditProcedure(aBind);
      exit;
    end;
    if (aBind as TXml).TypeDef.ElementDefs.Count = 0 then
    begin
      result := _editElementValue;
      exit;
    end;
    if XmlUtil.isExtendAdviced (aBind) then
    begin
      result := xmlUtil.ViewAsXml(aBind, aReadOnly);
      exit;
    end;
    if XmlUtil.isGridAdviced (aBind)
    and aDoUseGrid then
    begin
      result := xmlUtil.ViewAsGrid(aBind, aReadOnly);
      exit;
    end;
    if XmlUtil.isTreeAdviced (aBind) then
    begin
      result := xmlUtil.ViewAsXml(aBind, aReadOnly);
      exit;
    end;
    result := xmlUtil.ViewAsXml(aBind, aReadOnly);
  end;
end;

function TXmlUtil.isAddAllowed(aBind: TCustomBindable; doPrompt: Boolean): Boolean;
begin
  result := False;
  if not (aBind is TXml) then Exit;
  result := (LowerCase ((aBind as TXml).Xsd.maxOccurs) = 'unbounded')
         or (((aBind as TXml).Parent as TXml).NumberOfSubItemsWithTag((aBind as TXml).TagName, True)
                < StrToInt ((aBind as TXml).Xsd.maxOccurs));
  if not Result
  and doPrompt then
    result := (MessageDlg ('Number of items will exceed maxOccurs. Continue', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

function TXmlUtil.isBoolean(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and ((aBind as TXml).TypeDef.BaseDataTypeName = 'boolean')
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'boolean')
                )
            )
        ;
end;

function TXmlUtil.isDateTime(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and ((aBind as TXml).TypeDef.BaseDataTypeName = 'dateTime')
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'dateTime')
                )
            )
        ;
end;

function TXmlUtil.isDate(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and ((aBind as TXml).TypeDef.BaseDataTypeName = 'date')
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'date')
                )
            )
        ;
end;

function TXmlUtil.isTime(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and ((aBind as TXml).TypeDef.BaseDataTypeName = 'time')
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'time')
                )
            )
        ;
end;

function TXmlUtil.isHelpSelection(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and Assigned ((aBind as TXml).TypeDef.OnDoSelectValue)
                )
            )
        ;
end;

function TXmlUtil.isEnumeration(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and ((aBind as TXml).TypeDef.Enumerations.Count > 0)
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and ((aBind as TXmlAttribute).XsdAttr.Enumerations.Count > 0)
                )
            )
        ;
end;

function TXmlUtil.isDeleteAllowed(aBind: TCustomBindable;
  doPrompt: Boolean): Boolean;
var
  n : Integer;
begin
  result := False;
  if not (aBind is TXml) then Exit;
  if not Assigned ((aBind as TXml).Parent) then Exit;
  n := (aBind as TXml).IndexOfRepeatableItem;
  result := ((aBind as TXml).Xsd.maxOccurs <> '1')
     and (n > StrToIntDef ((aBind as TXml).Xsd.minOccurs, 0))
     and (n >= 1)
       ;
  if (not Result)
  and (n > 1)
  and doPrompt then
    result := (MessageDlg ( 'Number of items will be less then '
                          + IntToStr (n)
                          + '. Continue'
                          , mtConfirmation
                          , [mbYes, mbNo], 0
                          ) = mrYes);
end;

function TXmlUtil.isExtendAdviced(aBind: TCustomBindable): Boolean;
begin
  result := False;
  if aBind is TXml then with aBind as TXml do
    if Assigned (TypeDef) then
      result := (Items.Count = 0)
            and (TypeDef.ElementDefs.Count > 0)
              ;
end;

function TXmlUtil.isTreeAdviced(aBind: TCustomBindable): Boolean;
begin
  result := False;
  if (aBind is TXml)
  and Assigned (aBind.Parent)
  and Assigned ((aBind as TXml).Xsd)
  and Assigned ((aBind as TXml).TypeDef)
  and ((aBind as TXml).TypeDef.ElementDefs.Count > 0) then
    result := True;
end;

function TXmlUtil.isGridAdviced(aBind: TCustomBindable): Boolean;
  function _aExtendAdviced (aXml: TXml): Boolean;
  var
    x: Integer;
  begin
    result := False;
    for x := 0 to aXml.Items.Count - 1 do
    begin
      result := XmlUtil.isExtendAdviced (aXml.Items.XmlItems[x]);
      if result then
        exit;
    end;
  end;
var
  x: Integer;
begin
  result := False;
  if (aBind is TXml)
  and Assigned ((aBind as TXml).Xsd) then with aBind as TXml do
    for x := 0 to TypeDef.ElementDefs.Count - 1 do
      if TypeDef.ElementDefs.Xsds [x].maxOccurs <> '1' then
      begin
        result := not _aExtendAdviced (aBind as TXml);
        Exit;
      end;
end;

function TXmlUtil.isPasswordType(aBind: TCustomBindable): Boolean;
begin
  result := (aBind is TXml)
        and Assigned ((aBind as TXml).Xsd)
        and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
        and ((aBind as TXml).TypeDef.Name = 'passwordType')
          ;
end;

function TXmlUtil.isEditSupported(aBind: TCustomBindable): Boolean;
begin
  result := (   (    (aBind is TXml)
                 and Assigned ((aBind as TXml).Xsd)
                 and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
                 and (   ((aBind as TXml).TypeDef.BaseDataTypeName = 'date')
                      or ((aBind as TXml).TypeDef.BaseDataTypeName = 'dateTime')
                      or ((aBind as TXml).TypeDef.BaseDataTypeName = 'boolean')
                      or ((aBind as TXml).TypeDef.BaseDataTypeName = 'base64Binary')
                      or ((aBind as TXml).TypeDef.BaseDataTypeName = 'file')
                      or ((aBind as TXml).TypeDef.Name = 'FileNameType')
                      or ((aBind as TXml).TypeDef.Name = 'FolderNameType')
                      or ((aBind as TXml).TypeDef.Name = 'htmlColorType')
                      or ((aBind as TXml).TypeDef.Name = 'NameValuePairsType')
                      or Assigned ((aBind as TXml).TypeDef.OnDoSelectValue)
                      or ((aBind as TXml).TypeDef.Enumerations.Count > 0)
                      or Assigned ((aBind as TXml).Xsd.EditProcedure)
                     )
                )
             or (    (aBind is TXmlAttribute)
                 and Assigned ((aBind as TXmlAttribute).XsdAttr)
                 and (   ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'date')
                      or ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'dateTime')
                      or ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'boolean')
                      or ((aBind as TXmlAttribute).XsdAttr.BaseDataTypeName = 'base64Binary')
                      or ((aBind as TXmlAttribute).XsdAttr.Name = 'FileNameType')
                      or ((aBind as TXmlAttribute).XsdAttr.Name = 'FolderNameType')
                      or ((aBind as TXmlAttribute).XsdAttr.Name = 'htmlColorType')
                      or ((aBind as TXmlAttribute).XsdAttr.Enumerations.Count > 0)
                     )
                )
            )
        ;
end;

procedure TXmlUtil.PasteFromClipboard(aBind: TCustomBindable);
var
  hXml: TXml;
begin
  if aBind is TXmlAttribute then
    raise Exception.Create('Not implemented for XML attributes');
  if aBind is TXml then
  begin
    hXml := TXml.Create;
    try
      hXml.LoadFromString(Clipboard.AsText, nil);
      if hXml.Name = '' then
        hXml.LoadJsonFromString(Clipboard.AsText, nil);
      if hXml.Name <> '' then
      begin
        AcquireLock;
        try
          (aBind as TXml).Reset;
          hXml.TagName := (aBind as TXml).TagName;
          (aBind as TXml).LoadValues(hXml, False);
          (aBind as TXml).Checked := (aBind as TXml).Checked; // so in case Checked, Parents will also get checked
        finally
          ReleaseLock;
        end;
      end;
    finally
      FreeAndNil (hXml);
    end;
  end;
  if aBind is TIpmItem then
  begin
    hXml := TXml.Create;
    try
      AcquireLock;
      try
        hXml.LoadFromString(Clipboard.AsText, nil);
        hXml.Name := (aBind as TIpmItem).Name;
        (aBind as TIpmItem).ResetLoaded (True);
        (aBind as TIpmItem).LoadValues (hXml);
      finally
        ReleaseLock;
      end;
    finally
      FreeAndNil (hXml);
    end;
  end;
end;

procedure TXmlUtil.Populate(aBind: TCustomBindable; aViewType: TxvViewType);
begin
  if aBind is TXmlAttribute then
    raise Exception.Create('Not implemented for XML attributes');
  if not Assigned (aBind) then
    raise Exception.Create(' is Nil');
  AcquireLock;
  try
    aBind.Populate (aViewType);
  finally
    ReleaseLock;
  end;
end;

procedure TXmlUtil.Validate(aBind: TCustomBindable);
begin
  if aBind is TXmlAttribute then
    raise Exception.Create ('Not implemented for XML atributes');
  if not Assigned (aBind) then
    raise Exception.Create('XML should be not nil');
  if not aBind.IsValueValid then
    ShowMessage (aBind.ValidationMesssage)
  else
    ShowMessage (aBind.Name + ' validated OK');
end;

procedure TXmlUtil.FoundErrorInBuffer(ErrorString: String;
  aObject: TObject);
begin
  if (aObject is TIpmItem) then
    (aObject as TIpmItem).Value := '?Error: ' + ErrorString;
end;

procedure TXmlUtil.presentAsIpm(aIpm: TIpmItem; aCaption, aString: String);
var
  xForm: TShowXmlForm;
begin
  if not (aIpm is TIpmItem) then
    raise Exception.Create ('TXmlUtil.presentAsIpm: Illegal CCB argument');
  aIpm.BufferToValues (FoundErrorInBuffer, aString);
  Application.CreateForm(TShowXmlForm, xForm);
  try
    xForm.Caption := aCaption;
    xForm.Bind := aIpm;
    xForm.isReadOnly := True;
    xForm.ignoreDifferencesOn := ignoreDifferencesOn;
    xForm.ShowModal;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TXmlUtil.presentAsText(aCaption, aString: String);
var
  xForm: TShowTextForm;
  xIpmDescr: TIpmDescr;
begin
  if systemStarting then Exit; // a Lazarus execuatble halts when not yet ...
  xIpmDescr := FindIpmDescr (aString);
  if Assigned (xIpmDescr) then
  begin
    presentAsIpm (xIpmDescr.IpmItem, aCaption, aString);
    exit;
  end;
  Application.CreateForm(TShowTextForm, xForm);
  try
    xForm.Caption := aCaption;
    xForm.Memo.Lines.Text := aString;
    xForm.EditAllowed := False;
    fShowProgress (Self, 0, 4);
    xForm.ShowModal;
  finally
    xForm.Free;
  end;
end;

procedure TXmlUtil.ZoomAsText(aBind: TCustomBindable; aReadOnly: Boolean);
var
  xCaption: String;
  xForm: TShowTextForm;
begin
  if not Assigned (aBind) then
    raise Exception.Create('not assigned');
  if aBind is TXml then
  begin
    NewValue := (aBind as TXml).Value;
    xCaption := (aBind as TXml).FullCaption;
    if Assigned ((aBind as TXml).Xsd)
    and ((aBind as TXml).TypeDef.BaseDataTypeName = 'base64Binary') then
      NewValue := DecodeStringBase64(NewValue);
  end;
  if aBind is TXmlAttribute then
  begin
    NewValue := (aBind as TXmlAttribute).Value;
    xCaption := xCaption
              + '.'
              + (aBind as TXmlAttribute).Name;
  end;
  Application.CreateForm(TShowTextForm, xForm);
  try
    xForm.Caption := xCaption;
    xForm.Memo.Lines.Text := NewValue;
    xForm.EditAllowed := not aReadOnly;
    xForm.ShowModal;
    if xForm.ModalResult = mrOK then
    begin
      if (aBind is TXml)
      and Assigned ((aBind as TXml).Xsd)
      and ((aBind as TXml).TypeDef.BaseDataTypeName = 'base64Binary') then
        NewValue := EncodeStringBase64(xForm.Memo.Lines.Text)
      else
        NewValue := xForm.Memo.Lines.Text;
    end;
  finally
    xForm.Free;
  end;
end;

procedure TXmlUtil.ZoomAsXml(aBind: TCustomBindable; aReadOnly: Boolean);
var
  xCaption: String;
  s: String;
  xForm: TShowXmlForm;
  hXml: TXml;
  xXsdDescr: TXsdDescr;
begin
  if not Assigned (aBind) then
    raise Exception.Create('not assigned');
  if aBind is TXml then
  begin
    s := (aBind as TXml).Value;
    xCaption := (aBind as TXml).FullCaption;
    if Assigned ((aBind as TXml).Xsd)
    and ((aBind as TXml).TypeDef.BaseDataTypeName = 'base64Binary') then
      s := DecodeStringBase64(s);
  end;
  if aBind is TXmlAttribute then
  begin
    s := (aBind as TXmlAttribute).Value;
    xCaption := xCaption
              + '.'
              + (aBind as TXmlAttribute).Name;
  end;
  hXml := TXml.Create;
  xXsdDescr := TXsdDescr.Create;
  try
    hXml.LoadFromString(s, nil);
    CreateXsdFromXml(xXsdDescr, hXml, True);
    try
      Application.CreateForm(TShowXmlForm, xForm);
      try
        xForm.Caption := xCaption;
        xForm.Bind := hXml;
        xForm.isReadOnly := aReadOnly;
        xForm.ignoreDifferencesOn := ignoreDifferencesOn;
        xForm.ShowModal;
      finally
        xForm.Free;
      end;
    finally
      xXsdDescr.Free;
    end;
  finally
    hXml.Free;
  end;
end;

procedure TXmlUtil.ListXsdProperties(aListView: TListView; aBind: TCustomBindable);
  procedure AddProperty (aKey: String; aValue: String);
  var
    ListItem: TListItem;
  begin
    if aValue <> '' then
    begin
      ListItem := aListView.Items.Add;
      ListItem.Caption := aKey;
      ListItem.SubItems.Add(aValue);
    end;
  end;
  procedure AddIntegerProperty (aKey: String; aValue: Integer);
  begin
    AddProperty(aKey, IntToStr (aValue))
  end;
  procedure AddBooleanProperty (aKey: String; aValue: Boolean);
  begin
    if aValue then
      AddProperty(aKey, 'true')
    else
      AddProperty(aKey, 'false');
  end;
var
  xDataType: TXsdDataType;
begin
  aListView.Clear;
  xDataType := nil;
  if (aBind is TXml) then
  begin
    if Assigned ((aBind as TXml).Xsd)
    and Assigned ((aBind as TXml).Xsd.sType) then
    begin
//    AddProperty('NameSpace (TypeDef)', (aBind as TXml).TypeDef.NameSpace);
      if (aBind as TXml).Xsd.ParametersType <> oppBody then
      begin
        case (aBind as TXml).Xsd.ParametersType of
          oppFormData: AddProperty('in', 'formData');
          oppBody: AddProperty('in', 'body');
          oppHeader: AddProperty('in', 'header');
          oppPath: AddProperty('in', 'path');
          oppQuery: AddProperty('in', 'query');
        end;
      end;
      AddProperty('MediaType', (aBind as TXml).Xsd.MediaType);
      AddProperty('NameSpace', (aBind as TXml).Xsd.ElementNameSpace);
      AddProperty('ContentModel', (aBind as TXml).TypeDef.ContentModel);
      AddProperty('DerivationMethod', (aBind as TXml).TypeDef.DerivationMethod);
      AddProperty('minOccurs', (aBind as TXml).Xsd.minOccurs);
      AddProperty('maxOccurs', (aBind as TXml).Xsd.maxOccurs);
      AddProperty('SourceFileElement', (aBind as TXml).Xsd.SourceFileName);
      xDataType := (aBind as TXml).TypeDef;
    end;
  end;
  if (aBind is TXmlAttribute) then
  begin
    if Assigned ((aBind as TXmlAttribute).XsdAttr) then
    begin
      AddProperty('Use', (aBind as TXmlAttribute).XsdAttr.Use);
      AddProperty('SourceFileAttrib', (aBind as TXmlAttribute).XsdAttr.SourceFileName);
    end;
    xDataType := (aBind as TXmlAttribute).XsdAttr;
  end;
  if Assigned (xDataType) then
  begin
    if xDataType._Error then
      AddProperty ('Error', 'Parse failure');
    AddProperty ('DataType', xDataType.Name);
    AddProperty ('JsonType', JsonTypeNames[xDataType.jsonType]);
    AddProperty ('BaseDataType', xDataType.BaseDataTypeName);
    AddProperty ('Length', xDataType.Length);
    AddProperty ('minLength', xDataType.minLength);
    AddProperty ('maxLength', xDataType.maxLength);
    AddProperty ('Pattern', xDataType.Pattern);
    AddProperty ('Whitespace', xDataType.Whitespace);
    AddProperty ('MaxInclusive', xDataType.MaxInclusive);
    AddProperty ('MaxExclusive', xDataType.MaxExclusive);
    AddProperty ('MinInclusive', xDataType.MinInclusive);
    AddProperty ('MinExclusive', xDataType.MinExclusive);
    AddProperty ('Numeric', xDataType.Numeric);
    AddProperty ('TotalDigits', xDataType.TotalDigits);
    AddProperty ('FractionalDigits', xDataType.FractionalDigits);
    AddProperty ('SourceFileDataType', xDataType.SourceFileName);
  end;
  if aBind is TIpmItem then
  begin
    AddProperty('Name', (aBind as TIpmItem).Name);
    AddProperty('PictureClause', (aBind as TIpmItem).PictureClause);
    AddIntegerProperty('Occurrence', (aBind as TIpmItem).Occurrence);
    AddIntegerProperty('Occurs', (aBind as TIpmItem).Occurs);
    AddBooleanProperty('Numeric', (aBind as TIpmItem).Numeric);
    AddBooleanProperty('HasComp', (aBind as TIpmItem).HasComp);
    AddBooleanProperty('Comp', (aBind as TIpmItem).Comp);
    AddBooleanProperty('Display', (aBind as TIpmItem).Display);
    AddIntegerProperty('InputLength', (aBind as TIpmItem).InputLength);
    AddIntegerProperty('Bytes', (aBind as TIpmItem).Bytes);
    AddIntegerProperty('Offset', (aBind as TIpmItem).Offset);
    AddIntegerProperty('Precision', (aBind as TIpmItem).Precision);
    AddBooleanProperty('Signed', (aBind as TIpmItem).Signed);
    AddBooleanProperty('SignLeading', (aBind as TIpmItem).SignLeading);
    AddBooleanProperty('SignSeparate', (aBind as TIpmItem).SignSeparate);
    AddIntegerProperty('Level', (aBind as TIpmItem).Level);
    AddBooleanProperty('Group', (aBind as TIpmItem).Group);
    AddIntegerProperty('minOccurs', (aBind as TIpmItem).minOccurs);
  end;
end;

procedure TXmlUtil.ListXsdDocumentation( aMemo: TMemo
                                       ; aBind: TCustomBindable
                                       ; aShowPath: Boolean
                                       ; aShowValue: Boolean
                                       );
var
  s: String;
begin
  aMemo.Clear;
  s := '';
  if aShowPath then
    s := s + 'Path: ' + aBind.GetFullCaption + #$A#$D;
  if aShowValue then
    s := s + 'Value: ' + aBind.Value + #$A#$D;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Documentation.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).DocumentationText;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Appinfo.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).AppinfoText;
  aMemo.Lines.Text := s;
end;

procedure TXmlUtil.ListXsdDocumentation(aHtmlViewer: THtmlViewer;
  aBind: TCustomBindable; aShowPath: Boolean; aShowValue: Boolean);
var
  s: String;
begin
  s := '';
  if aShowPath then
    s := s + 'Path: ' + aBind.GetFullCaption + '  ' + LineEnding;
  if aShowValue then
    s := s + 'Value: ' + aBind.Value + '  ' + LineEnding;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Documentation.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).DocumentationText;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Appinfo.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).AppinfoText;
  with TMarkdownProcessor.createDialect(mdCommonMark) do
  try
    UnSafe := false;
    //    aHtmlViewer.LoadFromString(prepareMarkDownText(process(s)));
    //    s := '# EEN ' + LineEnding + 'een txt' + LineEnding + '## twee' + LineEnding + 'tweede tekst';
    aHtmlViewer.LoadFromString(process(prepareMarkDownText(s)));
  finally
    Free;
  end;
{
  try
    with TMarkdownProcessor.createDialect(mdCommonMark) do
    try
      aHtmlViewer.SetHtmlFromStr(process(s)));
    finally
      free;
    end;
  except
  end;
}
end;

{
procedure TXmlUtil .ListXsdDocumentation (aMemo : TMemo ;
  aBind : TCustomBindable ; aShowPath : Boolean ; aShowValue : Boolean );
var
  s: String;
begin
  s := '';
  if aShowPath then
    s := s + 'Path: ' + aBind.GetFullCaption + CRLF;
  if aShowValue then
    s := s + 'Value: ' + aBind.Value + CRLF;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Documentation.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).DocumentationText;
  if aBind is TXmlAttribute then
    s := s + (aBind as TXmlAttribute).XsdAttr.Appinfo.Text;
  if aBind is TXml then
    s := s + (aBind as TXml).AppinfoText;
  aMemo.Text := s;
  aMemo.ParentColor := True;
  aMemo.Color := clBtnFace;
  MemoShowLinks(aMemo);
end;
}

procedure TXmlUtil.PushCursor (aNewCursor: TCursor);
begin
  cursorStack[cursorStackIndex] := Screen.Cursor;
  if cursorStackIndex < cursorStackSize then
    Inc (cursorStackIndex);
  Screen.Cursor := aNewCursor;
end;

procedure TXmlUtil.PopCursor;
begin
  if cursorStackIndex > 0 then
    Dec (cursorStackIndex);
  Screen.Cursor := cursorStack[cursorStackIndex];
end;

procedure TXmlUtil.ListXsdEnumerations(aListView: TListView; aBind: TCustomBindable);
var
  xDataType: TXsdDataType;
  x: Integer;
begin
  aListView.Clear;
  xDataType := nil;
  if aBind is TXmlAttribute then
    xDataType := (aBind as TXmlAttribute).XsdAttr
  else
    if aBind is TXml then
      xDataType := (aBind as TXml).TypeDef;
  if Assigned (xDataType) then
  begin
    for x := 0 to xDataType.Enumerations.Count - 1 do
      aListView.Items.Add.Caption := xDataType.Enumerations.Strings [x];
    if xDataType.BaseDataTypeName = 'boolean' then
    begin
      aListView.Items.Add.Caption := 'true';
      aListView.Items.Add.Caption := 'false';
      aListView.Items.Add.Caption := '1';
      aListView.Items.Add.Caption := '0';
    end;
  end;
end;

procedure TXmlUtil._ignoreNilProc;
begin
//
end;

function TXmlUtil.ViewAsGrid(aBind: TCustomBindable;
  aReadOnly: Boolean): Boolean;
var
  xForm: TXmlGridForm;
  xXml: TXml;
begin
  result := False;
  if not Assigned (aBind) then
    raise Exception.Create('No argument for ViewAsGrid');
  if not (aBind is TXml) then
    raise Exception.Create('Argument for ViewAsGrid is not an Xml element');
  Application.CreateForm(TXmlGridForm, xForm);
  try
    xForm.Caption := aBind.FullIndexCaption;
    xForm.doShowCancelButton := not aReadOnly;
    xForm.IsReadOnly := aReadonly;
    if aReadOnly then
    begin
      xForm.Xml := aBind as TXml;
      xForm.ShowModal;
      result := False;
    end
    else
    begin
      xXml := TXml.Create;
      try
        xXml.CopyDownLine(aBind as TXml, False);
        xForm.Xml := xXml;
        xForm.ShowModal;
        result := xForm.StubChanged;
        if result then
        begin
          AcquireLock;
          with aBind as TXml do
          try
            ResetValues;
            LoadValues(xXml, True, False);
            Checked := True;
          finally
            ReleaseLock;
          end;
        end;
      finally
        xXml.Free;
      end;
    end;
  finally
    xForm.Free;
  end;
end;

function TXmlUtil.ViewAsXml(aBind: TCustomBindable; aReadOnly: Boolean
  ): Boolean;
var
  xForm: TShowXmlForm;
  xXml: TXml;
  xIpm: TIpmItem;
begin
  result := False;
  if not Assigned (aBind) then
    raise Exception.Create('No argument for ViewAsXml');
  Application.CreateForm(TShowXmlForm, xForm);
  try
    xForm.Caption := aBind.FullIndexCaption;
    xForm.doShowCancelButton := not aReadOnly;
    xForm.IsReadOnly := aReadonly;
    xForm.ignoreDifferencesOn := ignoreDifferencesOn;
    if aBind.totalNumberOfSubElements > 40 then
      xForm.initialExpandStyle := esOne;
    if {}{}aReadOnly{}{ True{} then
    begin
      xForm.Bind := aBind;
      xForm.ShowModal;
      result := False;
    end
    else
    begin
      if aBind is TIpmItem then
      begin
        xIpm := TIpmItem.Create(aBind as TIpmItem);
        try
          xForm.Bind := xIpm;
          xForm.ShowModal;
          result := xForm.isChanged;
          if result then
          begin
//          AcquireLock;
            try
              (aBind as TIpmItem).BufferToValues(nil, xIpm.ValuesToBuffer(nil));
            finally
//            ReleaseLock;
            end;
          end;
        finally
          xIpm.Free;
        end;
      end;
      if aBind is TXml then
      begin
        xXml := TXml.Create;
        try
          xXml.CopyDownLine (aBind as TXml, False);
          xForm.Bind := xXml;
          xForm.ShowModal;
          result := xForm.isChanged;
          if result then
          begin
//          AcquireLock;
            with aBind as TXml do
            try
              ResetValues;
              LoadValues(xXml, True, True);
              Checked := True;
            finally
//            ReleaseLock;
            end;
          end;
        finally
          xXml.Free;
        end;
      end;
    end;
  finally
    xForm.Free;
  end;
end;

procedure TXmlUtil.ZoomAsBase64(aBind: TCustomBindable);
begin
  if Assigned (aBind) then
    ShowText(aBind.Name + ' as Base64 Text', DecodeStringBase64(aBind.Value));
end;

procedure TXmlUtil.presentAsRTF(aCaption, aString: String);
var
  TempFileName: String;
begin
TempFileName :=
{$ifdef WINDOWS}
                GetEnvironmentVariable ('Temp')
{$else}
                '/tmp'
{$endif}
                + DirectorySeparator
                + 'temp'
                +  IntToStr (rtfFileCounter mod 100)
                + '.rtf'
                ;
  Inc (rtfFileCounter);
  fShowProgress (self, 1, 4);
  SaveStringToFile(TempFileName, DecodeStringBase64 (aString));
  fShowProgress (self, 2, 4);
  try
    if not OpenDocument(PChar (TempFileName)) then
      raise Exception.Create ('Could not open ' + TempFileName);
  finally
    fShowProgress (self, 0, 4);
  end;
end;

procedure TXmlUtil.presentAsPDF(aCaption, aString: String);
var
  TempFileName: String;
begin
TempFileName :=
{$ifdef WINDOWS}
                GetEnvironmentVariable ('Temp')
{$else}
                '/tmp'
{$endif}
                + DirectorySeparator
                + 'temp'
                +  IntToStr (pdfFileCounter mod 100)
                + '.pdf'
                ;
  Inc (pdfFileCounter);
  fShowProgress (self, 1, 4);
  if Copy (aString, 1, 5) = '%PDF-' then
    SaveStringToFile(TempFileName, aString)
  else
    SaveStringToFile(TempFileName, DecodeStringBase64 (aString));
{}
  fShowProgress (self, 2, 4);
  try
    if not OpenDocument(TempFileName) then
      raise Exception.Create ('Could not open ' + TempFileName);
  finally
    fShowProgress (self, 0, 4);
  end;
end;

procedure TXmlUtil.presentAsDOCX(aCaption, aString: String);
var
  TempFileName: String;
begin
TempFileName :=
{$ifdef WINDOWS}
                GetEnvironmentVariable ('Temp')
{$else}
                '/tmp'
{$endif}
                + DirectorySeparator
                + 'temp'
                +  IntToStr (docxFileCounter mod 100)
                + '.docx'
                ;
  Inc (docxFileCounter);
  fShowProgress (self, 1, 4);
  SaveStringToFile(TempFileName, DecodeStringBase64 (aString));
  fShowProgress (self, 2, 4);
  try
    if not OpenDocument(PChar (TempFileName)) then
      raise Exception.Create ('Could not open ' + TempFileName);
  finally
    fShowProgress (self, 0, 4);
  end;
end;

procedure TXmlUtil.presentAsHTML(aCaption, aString: String);
var
  TempFileName: String;
begin
  TempFileName :=
{$ifdef WINDOWS}
                  GetEnvironmentVariable ('Temp')
{$else}
                  '/tmp'
{$endif}
                + DirectorySeparator
                + 'temp'
                +  IntToStr (htmlFileCounter mod 100)
                + '.html'
                ;
  Inc (htmlFileCounter);
  fShowProgress (self, 1, 4);
  SaveStringToFile(TempFileName, aString);
  fShowProgress (self, 2, 4);
  try
    if not OpenDocument(TempFileName) then
      raise Exception.Create ('Could not open ' + TempFileName);
  finally
    fShowProgress (self, 0, 4);
  end;
end;

procedure TXmlUtil.ZoomAsPDF(aBind: TCustomBindable);
begin
  if Assigned (aBind) then
    presentAsPdf (aBind.Name + ' as PDF', aBind.Value);
end;

function TXmlUtil.BindCaption(aBind: TCustomBindable): String;
begin
  result := aBind.Name;
  if aBind is TXmlAttribute then
    result := '@' + aBind.Name;
end;

function TXmlUtil.BooleanAsString(aBoolean: Boolean): String;
begin
  if aBoolean then
    result := 'true'
  else
    result := 'false';
end;

procedure TXmlUtil.CheckValidity(aBind: TCustomBindable);
var
  xMessage: String;
begin
  xMessage := ''; // avoid warning
  if aBind.Checked then
  begin
    if aBind.IsExpression then
      exit;
    if aBind is TXmlAttribute then
    begin
      if not (aBind as TXmlAttribute).IsValueValidAgainstXsd(xMessage) then
        ShowMessage (xMessage);
    end;
    if (aBind is TXml)
    or (aBind is TIpmItem) then
    begin
      if not aBind.IsValueValid then
        ShowMessage (aBind.ValidationMesssage);
    end;
  end;
end;

function TXmlUtil.isEditAllowed(aBind: TCustomBindable): Boolean;
begin
  Result := (    (aBind is TXmlAttribute)
{}{
             and (   (not Assigned ((aBind as TXmlAttribute).XsdAttr))
//TODO            or ((aBind as TXmlAttribute).XsdAttr.isReadOnly = False)
                 )
{}
            )
         or (    (aBind is TXml)
             and Assigned ((aBind as TXml).Xsd)
             and ((aBind as TXml).TypeDef.ElementDefs.Count = 0)
             and ((aBind as TXml).Xsd.isReadOnly = False)
            )
         or (    (aBind is TXml)
             and (not Assigned ((aBind as TXml).Xsd))
             and (aBind.Children.Count = 0)
            )
         or (    (aBind is TIpmItem)
             and ((aBind as TIpmItem).Items.Count = 0)
            )
          ;
end;

function TXmlUtil.isCheckAllowed(aBind: TCustomBindable): Boolean;
begin
  result := isEditAllowed (aBind)
         or (    (aBind is TXml)
             and ((aBind as TXml).TypeDef.ElementDefs.Count > 0)
             and ((aBind as TXml).Xsd.isReadOnly = False)
             and ((aBind as TXml).Xsd.isCheckboxDisabled = False)
            )
          ;
end;

procedure TXmlUtil.fShowProgress(aSender: TObject; aProgress,
  aProgressMax: Integer);
begin
  if Assigned (OnProgress) then
    OnProgress (aSender, aProgress, aProgressMax);
end;

function TXmlUtil.getAcquireLock: TProcedure;
begin
  if Assigned (fAcquireLock) then
    Result := fAcquireLock
  else
    Result := _ignoreNilProc;
end;

function TXmlUtil.getImageImdex(aBind: TCustomBindable;
  aOffset: Integer): Integer;
begin
  result := -1;
  if not Assigned (aBind) then Exit;
  if aBind is TIpmItem then Exit;
  if aBind is TXmlAttribute then Exit;
  if aBind is TXml then
  begin
    if isExtendAdviced(aBind) then
    begin
      result := aOffset + Ord (iiExtendLevel);
      exit;
    end;
    if isBoolean(aBind) then
    begin
      if (aBind.Value <> 'true')
      and (aBind.Value <> '1') then
        result := aOffset + Ord (iiUnchecked)
      else
        result := aOffset + Ord (iiChecked);
    end;
    if isDateTime(aBind) then
    begin
      result := aOffset + Ord (iiDateTime);
      exit;
    end;
    if isDate(aBind) then
    begin
      result := aOffset + Ord (iiDate);
      exit;
    end;
    if isTime(aBind) then
    begin
      result := aOffset + Ord (iiTime);
      exit;
    end;
    if isEnumeration(aBind) then
    begin
      result := aOffset + Ord (iiEnumeration);
      exit;
    end;
    if isHelpSelection(aBind) then
    begin
      result := aOffset + Ord (iiEnumeration);
      exit;
    end;
    if isGridAdviced(aBind) then
    begin
      result := aOffset + Ord (iiGrid);
      exit;
    end;
    if isTreeAdviced(aBind) then
    begin
      result := aOffset + Ord (iiTree);
      exit;
    end;
  end;
end;

function TXmlUtil.getReleaseLock: TProcedure;
begin
  if Assigned (fReleaseLock) then
    Result := fReleaseLock
  else
    Result := _ignoreNilProc;
end;

procedure TXmlUtil.EmbeddedXml(aXml: TXml);
var
  xXml: TXml;
  x: Integer;
begin
  xXml := TXml.Create;
  try
    xXml.LoadFromString(aXml.Value, nil);
  except
    xXml.Name := '';
  end;
  if xXml.Name <> '' then
  begin
    aXml.Value := '';
    aXml.AddXml (xXml);
  end
  else
    xXml.Free;    {}
  for x := 0 to aXml.Items.Count - 1 do
    EmbeddedXml (aXml.Items.XmlItems[x]);    {}
end;

procedure TXmlUtil.presentString(aCaption, aString: String);
  procedure _presentString;
  var
    xForm: TShowXmlForm;
    xXml: TXml;
    xXsdDescr: TXsdDescr;
  begin
    if AnsiStartsStr(base64PdfStartStr, aString) then {Base64 encoded PDF}
    begin
      presentAsPDF(aCaption, aString);
      exit;
    end;
    if AnsiStartsStr(base64DocxStartStr, aString) then {Base64 encoded DOCX}
    begin
      presentAsDOCX(aCaption, aString);
      exit;
    end;
    if AnsiStartsStr(base64RtfStartStr, aString) then {Base64 encoded RTF}
    begin
      presentAsRTF(aCaption, aString);
      exit;
    end;
    if (UpperCase(Copy(aString, 1, 7)) = 'HTTP://')
    or (UpperCase(Copy(aString, 1, 8)) = 'HTTPS://') then
    begin
      if not OpenDocument(PChar (aString)) then
        raise Exception.Create ('Could not execute ' + aString);
      exit;
    end;
    if (LowerCase(Copy (aString, 1,  5)) = '<html')
    or (LowerCase(Copy (aString, 1, 10)) = '<!doctype ')
    then
    begin
      presentAsHTML(aCaption, aString);
      exit;
    end;
    xXsdDescr := TXsdDescr.Create;
    xXml := TXml.Create;
    try
      try
        xXml.LoadFromString(aString, nil);
      except
        xXml.Name := '';
      end;
      if xXml.Name = '' then
      begin
        try
          xXml.LoadJsonFromString(aString, nil);
        except
          xXml.Name := '';
        end;
      end;
      if xXml.Name = '' then
      begin
        fShowProgress (Self, 2, 4);
        presentAsText (aCaption, aString);
        exit;
      end;
      xmlUtil.EmbeddedXml (xXml);
      CreateXsdFromXml(xXsdDescr, xXml, True);
      fShowProgress (Self, 3, 4);
      Application.CreateForm(TShowXmlForm, xForm);
      try
        xForm.Caption := aCaption;
        xForm.Bind := xXml;
        xForm.isReadOnly := True;
        xForm.doEnableCompare := doEnableCompare;
        xForm.ignoreDifferencesOn := ignoreDifferencesOn;
        fShowProgress (Self, 0, 4);
        xForm.ShowModal;
      finally
        FreeAndNil (xForm);
      end;
    finally
      FreeAndNil(xXml);
      FreeAndNil(xXsdDescr);
    end;
  end;
begin
  fShowProgress (Self, 1, 4);
  try
    _presentString;
  finally
    fShowProgress (Self, 0, 4);
  end;
end;

constructor TXmlUtil.Create;
begin
  IpmDescrs := TIpmDescrs.Create;
  IpmDescrs.Sorted := False;
  cursorStackIndex := 0;
end;

destructor TXmlUtil.Destroy;
begin
  IpmDescrs.Clear;
  IpmDescrs.Free;
end;

function TXmlUtil.IpmDescrsAsXml: TXml;
var
  x, y: Integer;
  xRecog: TRecog;
begin
  result := TXml.CreateAsString ('RecordDescriptors', '');
  for x := 0 to IpmDescrs.Count - 1 do
  begin
    with IpmDescrs.IpmDescrs [x] do
    begin
      with result.AddXml (TXml.CreateAsString ('RecordDescriptor', '')) do
      begin
        AddXml (TXml.CreateAsString ('DescriptionType', 'Cobol'));
        if CobolEnvironment = ceTandem then
          AddXml (TXml.CreateAsString ('CobolEnvironment', 'Tandem'));
        if CobolEnvironment = ceIbmZOs then
          AddXml (TXml.CreateAsString ('CobolEnvironment', 'IBM Zos'));
        AddXml (TXml.CreateAsString ('DescriptorFileName', FileName));
        for y := 0 to Recogs.Count - 1 do
        begin
          xRecog := Recogs.Objects [y] as TRecog;
          with AddXml (TXml.CreateAsString ('Recognition', '')) do
          begin
            AddXml (TXml.CreateAsInteger ('Start', xRecog.Start));
            AddXml (TXml.CreateAsInteger ('Length', xRecog.Length));
            AddXml (TXml.CreateAsString ('Value', xRecog.Value));
          end;
        end;
      end;
    end;
  end;
end;

procedure TXmlUtil.IpmDescrsFromXml(aFileName: String; aXml: TXml);
var
  x, y: Integer;
  xRecog: TRecog;
  yXml: TXml;
  xIpmDescr: TIpmDescr;
begin
  if (not Assigned (aXml))
  or (aXml.Name <> 'RecordDescriptors') then
    raise Exception.Create ('recDescrsFromXml: illegal argument');
  IpmDescrs.Clear;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    if aXml.Items.XmlItems [x].Name = 'RecordDescriptor' then
    begin
      xIpmdescr := TIpmDescr.Create;
      if aXml.Items.XmlItems [x].Items.XmlValueByTag ['CobolEnvironment'] = 'Tandem' then
        xIpmdescr.CobolEnvironment := ceTandem;
      if aXml.Items.XmlItems [x].Items.XmlValueByTag ['CobolEnvironment'] = 'IBM Zos' then
        xIpmdescr.CobolEnvironment := ceIbmZOs;
      xIpmdescr.FileName := ExpandRelativeFileName (aFileName, aXml.Items.XmlItems [x].Items.XmlValueByTag ['DescriptorFileName']);
      if FileExistsUTF8(xIpmdescr.FileName) { *Converted from FileExists* } then
        xIpmDescr.LoadFromFile(xIpmdescr.FileName, _OnParseErrorEvent);
      IpmDescrs.AddObject(xIpmdescr.FileName, xIpmDescr);
      for y := 0 to aXml.Items.XmlItems [x].Items.Count - 1 do
      begin
        yXml := aXml.Items.XmlItems [x].Items.xmlItems[y];
        if yXml.Name = 'Recognition' then
        begin
          xRecog := TRecog.Create;
          xIpmdescr.Recogs.AddObject ('', xRecog);
          xRecog.Start := yXml.Items.XmlIntegerByTagDef ['Start', 1];
          xRecog.Length := yXml.Items.XmlIntegerByTagDef ['Length', 1];
          xRecog.Value := yXml.Items.XmlValueByTagDef ['Value', ''];
        end;
      end;
    end;
  end;
end;

function TXmlUtil.FindIpmDescr (aString: String): TIpmDescr;
begin
  result := IpmDescrs.FindIpmDescr (aString);
end;

{ TRecDescr }

initialization
  XmlUtil := TXmlUtil.Create;

finalization
  XmlUtil.Free;

end.
