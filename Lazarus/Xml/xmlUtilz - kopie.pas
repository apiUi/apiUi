unit xmlUtilz;
interface

uses Classes, Forms, Controls, ComCtrls, StdCtrls, Bind, Xmlz, Xsdz;

type TXmlUtil = class(TObject)
public
  NewValue: String;
  newChecked: Boolean;
  function CreateXsdFromXml (aXml: TXml; aLinkXmlToXsd: Boolean): TXsd;
  procedure FreeXsd (aXsd: TXsd; aRecursive: Boolean);
  procedure ShowSoapBodyInGrid (aSoapMessageString: String);
  function isDeleteAllowed (aBind: TCustomBindable; doPrompt: Boolean): Boolean;
  function Delete (aXml: TXml): TXml;
  function isAddAllowed (aBind: TCustomBindable; doPrompt: Boolean): Boolean;
  function Add (aXml: TXml): TXml;
  function isEditSupported (aBind: TCustomBindable): Boolean;
  function editValue (aBind: TCustomBindable; aReadOnly: Boolean): Boolean;
  procedure CheckValueAgainstXsd(aBind: TCustomBindable);
  procedure CopyToClipboard (aBind: TCustomBindable);
  procedure PasteFromClipboard (aBind: TCustomBindable);
  procedure Populate (aBind: TCustomBindable; aViewType: TxvViewType);
  procedure Validate (aBind: TCustomBindable);
  procedure ZoomAsBase64 (aBind: TCustomBindable);
  procedure ZoomAsPDF (aBind: TCustomBindable);
  procedure ZoomAsText (aBind: TCustomBindable; aReadOnly: Boolean);
  procedure ZoomAsHtml (aBind: TCustomBindable);
  procedure ZoomAsXml (aBind: TCustomBindable; aReadOnly: Boolean);
  procedure ViewAsXml (aBind: TCustomBindable; aReadOnly: Boolean);
  procedure ListXsdProperties (aListView: TListView; aBind: TCustomBindable);
  procedure ListXsdEnumerations (aListView: TListView; aBind: TCustomBindable);
  procedure ListXsdDocumentation (aMemo: TMemo; aBind: TCustomBindable);
end;

var
  XmlUtil: TXmlUtil;

implementation

uses Dialogs
   , ShowXmlUnit
   , ShowHtmlUnit
   , ChooseStringUnit
   , xsdDateTimeFormUnit
   , ipmInfoUnit
   , igGlobals
   , StrUtils
   , SysUtils
   , ClipBrd
   , XmlGridUnit
   , ShowPdfFileUnit
   ;

{ TXmlUtil }

function TXmlUtil.Add(aXml: TXml): TXml;
var
  xXml: TXml;
begin
  result := TXml.Create (aXml.Xsd);
  try
    result.Parent := aXml.Parent;
    result.Checked := True;
    (aXml.Parent as TXml).Items.InsertObject( (aXml.Parent as TXml).Items.IndexOfObject (aXml) + 1
                                 , result.TagName
                                 , result
                                 );
  except
    result.Free;
    raise;
  end; {try}
end;

procedure TXmlUtil.FreeXsd (aXsd: TXsd; aRecursive: Boolean);
var
  x: Integer;
begin
  if aRecursive then
  begin
    for x:= 0 to aXsd.AttributeDefs.Count - 1 do
      aXsd.AttributeDefs.XsdAttrs[x].Free;
    aXsd.AttributeDefs.Clear;
    for x:= 0 to aXsd.ElementDefs.Count - 1 do
      FreeXsd (aXsd.ElementDefs.Xsds[x], aRecursive);
  end;
  aXsd.Free;
end;

procedure TXmlUtil.ShowSoapBodyInGrid (aSoapMessageString: String);
var
  x, d: Integer;
  xXml, dXml, sXml: TXml;
  xXsd: TXsd;
  xForm: TXmlGridForm;
  xXsdPropertiesVisible: Boolean;
begin
  sXml := TXml.Create;
  try
    sXml.LoadFromString(aSoapMessageString, nil);
    if NameWithoutPrefix(sXml.TagName) = 'Envelope' then
    begin
      for x := 0 to sXml.Items.Count - 1 do
      begin
        xXml := sXml.Items.XmlItems [x];
        if (NameWithoutPrefix(xXml.TagName) = 'Body')
        and (xXml.Items.Count = 1) then
        begin
          xXsd := CreateXsdFromXml(xXml.Items.XmlItems[0], False);
          try
            dXml := TXml.Create(xXsd);
            dXml.LoadValues(xXml.Items.XmlItems[0],False);
            try
              Application.CreateForm(TXmlGridForm, xForm);
              xXsdPropertiesVisible := xForm.XsdPropertiesVisible;
              try
                xForm.isReadOnly := True;
                xForm.Xml := dXml;
                xForm.XsdPropertiesVisible := False;
                xForm.ShowPropertiesAction.Enabled := False;
                xForm.ShowModal;
              finally
                xForm.XsdPropertiesVisible := xXsdPropertiesVisible;
                FreeAndNil (xForm);
              end;
            finally
              dXml.Free;
            end;
          finally
            FreeXsd(xXsd, True);
          end;
        end;
      end;
    end;
  finally
    sXml.Free;
  end;
end;

procedure TXmlUtil.CopyToClipboard(aBind: TCustomBindable);
begin
  if aBind is TXmlAttribute then
    raise Exception.Create('Not implemented for XML attributes')
  else
    if aBind is TXml then
      Clipboard.AsText := (aBind as TXml).AsText ( False
                                                 , 0
                                                 , _xmlProgName
                                                 , True
                                                 , False
                                                 );
end;

function TXmlUtil.Delete(aXml: TXml): TXml;
begin
  if Assigned (aXml.Parent) then
    result := (aXml.Parent as TXml).DeleteChild(aXml)
  else
  begin
    raise Exception.Create('Not allowed to delete root');
{
    aXml.Free;
    result := nil;
}
  end;
end;

function TXmlUtil.editValue(aBind: TCustomBindable; aReadOnly: Boolean): Boolean;
var
  xChooseStringForm: TChooseStringForm;
  xDateTimeForm: TxsdDateTimeForm;
  xInfoForm: TipmInfoForm;
begin
  result := False;
  if (aBind is TXml)
  and (Assigned ((aBind as TXml).Xsd))
  and (   ((aBind as TXml).Xsd.Enumerations.Count > 0)
       or ((aBind as TXml).Xsd.BaseDataTypeName = 'boolean')
      ) then
  begin
    Application.CreateForm(TChooseStringForm,xChooseStringForm);
    try
      xChooseStringForm.ListBox.Clear;
      if (aBind as TXml).Xsd.BaseDataTypeName = 'boolean' then
      begin
        xChooseStringForm.ListBox.Items.Add ('false');
        xChooseStringForm.ListBox.Items.Add ('true');
        xChooseStringForm.ListBox.Items.Add ('0');
        xChooseStringForm.ListBox.Items.Add ('1');
      end
      else
        xChooseStringForm.ListBox.Items.Text := (aBind as TXml).Xsd.Enumerations.Text;
      xChooseStringForm.ChoosenString := (aBind as TXml).Value;
      xChooseStringForm.Caption := 'Choose value for ' + (aBind as TXml).Tagname;
      xChooseStringForm.ShowModal;
      result := (xChooseStringForm.ModalResult = mrOk);
      NewValue := xChooseStringForm.ChoosenString;
    finally
      xChooseStringForm.Free;
    end;
  end;
  if (aBind is TXml)
  and (Assigned ((aBind as TXml).Xsd))
  and (   ((aBind as TXml).Xsd.BaseDataTypeName = 'date')
       or ((aBind as TXml).Xsd.BaseDataTypeName = 'dateTime')
      ) then
  begin
    Application.CreateForm(TxsdDateTimeForm, xDateTimeForm);
    try
      xDateTimeForm.Caption := 'Enter value for ' + (aBind as TXml).Tagname;
      if ((aBind as TXml).Xsd.BaseDataTypeName = 'dateTime') then
        xDateTimeForm.dtFormat := dtfDateTime;
      if ((aBind as TXml).Xsd.BaseDataTypeName = 'date') then
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
  and (   ((aBind as TXml).Xsd.BaseDataTypeName = 'base64Binary')
      ) then
  begin
    Application.CreateForm(TipmInfoForm, xInfoForm);
    try
      xInfoForm.Caption := 'Enter value for ' + (aBind as TXml).Tagname;
      xInfoForm.Memo.Lines.Text := B64Decode ((aBind as TXml).Value);
      xInfoForm.EditAllowed := not ReadOnly;
      xInfoForm.ShowModal;
      result := (xInfoForm.ModalResult = mrOK)
            and (not ReadOnly);
      NewValue := B64Encode (xInfoForm.Memo.Lines.Text);
    finally
      xInfoForm.Free;
    end;
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

function TXmlUtil.isDeleteAllowed(aBind: TCustomBindable;
  doPrompt: Boolean): Boolean;
var
  n : Integer;
begin
  result := False;
  if not (aBind is TXml) then Exit;
  if not Assigned ((aBind as TXml).Parent) then Exit;
  n := ((aBind as TXml).Parent as TXml).NumberOfSubItemsWithTag((aBind as TXml).TagName, False);
  result := ((aBind as TXml).Xsd.maxOccurs <> '1')
     and (n > StrToIntDef ((aBind as TXml).Xsd.minOccurs, 0))
     and (n > 1)
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

function TXmlUtil.isEditSupported(aBind: TCustomBindable): Boolean;
begin
  result := (aBind is TXml)
        and Assigned ((aBind as TXml).Xsd)
        and ((aBind as TXml).Xsd.ElementDefs.Count = 0)
        and (   ((aBind as TXml).Xsd.BaseDataTypeName = 'date')
             or ((aBind as TXml).Xsd.BaseDataTypeName = 'dateTime')
             or ((aBind as TXml).Xsd.BaseDataTypeName = 'boolean')
             or ((aBind as TXml).Xsd.BaseDataTypeName = 'base64Binary')
             or ((aBind as TXml).Xsd.Enumerations.Count > 0)
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
      (aBind as TXml).Reset;
      hXml.TagName := (aBind as TXml).TagName;
      (aBind as TXml).LoadValues(hXml, False);
      (aBind as TXml).CheckAllAttributes(True);
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
    raise Exception.Create('XML is Nil');
  (aBind as TXml).Populate (aViewType);
end;

procedure TXmlUtil.Validate(aBind: TCustomBindable);
var
  xMessage: String;
begin
  if aBind is TXmlAttribute then
    raise Exception.Create ('Not implemented for XML atributes');
  if not Assigned (aBind) then
    raise Exception.Create('XML should be not nil');
  if not (aBind as TXml).Xsd.IsValidXml((aBind as TXml), xMessage) then
    ShowMessage (xMessage)
  else
    ShowMessage ((aBind as TXml).TagName + ' validated OK');
end;

procedure TXmlUtil.ZoomAsText(aBind: TCustomBindable; aReadOnly: Boolean);
var
  xCaption: String;
  xForm: TipmInfoForm;
begin
  if not Assigned (aBind) then
    raise Exception.Create('not assigned');
  if aBind is TXml then
  begin
    NewValue := (aBind as TXml).Value;
    xCaption := (aBind as TXml).FullCaption;
    if Assigned ((aBind as TXml).Xsd)
    and ((aBind as TXml).Xsd.BaseDataTypeName = 'base64Binary') then
      NewValue := B64Decode(NewValue);
  end;
  if aBind is TXmlAttribute then
  begin
    NewValue := (aBind as TXmlAttribute).Value;
    xCaption := xCaption
              + '.'
              + (aBind as TXmlAttribute).Name;
  end;
  Application.CreateForm(TipmInfoForm, xForm);
  try
    xForm.Caption := xCaption;
    xForm.Memo.Lines.Text := NewValue;
    xForm.EditAllowed := not aReadOnly;
    xForm.ShowModal;
    if xForm.ModalResult = mrOK then
    begin
      if (aBind is TXml)
      and Assigned ((aBind as TXml).Xsd)
      and ((aBind as TXml).Xsd.BaseDataTypeName = 'base64Binary') then
        NewValue := B64Encode(xForm.Memo.Lines.Text)
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
begin
  if not Assigned (aBind) then
    raise Exception.Create('not assigned');
  if aBind is TXml then
  begin
    s := (aBind as TXml).Value;
    xCaption := (aBind as TXml).FullCaption;
    if Assigned ((aBind as TXml).Xsd)
    and ((aBind as TXml).Xsd.BaseDataTypeName = 'base64Binary') then
      s := B64Decode(s);
  end;
  if aBind is TXmlAttribute then
  begin
    s := (aBind as TXmlAttribute).Value;
    xCaption := xCaption
              + '.'
              + (aBind as TXmlAttribute).Name;
  end;
  hXml := TXml.Create;
  try
    hXml.LoadFromString(s, nil);
    xmlUtil.CreateXsdFromXml(hXml, True);
    try
      Application.CreateForm(TShowXmlForm, xForm);
      try
        xForm.Caption := xCaption;
        xForm.xml := hXml;
        xForm.isReadOnly := aReadOnly;
        xForm.ShowModal;
      finally
        xForm.Free;
      end;
    finally
      xmlUtil.FreeXsd(hXml.Xsd, True);
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
var
  xDataType: TXsdDataType;
begin
  aListView.Clear;
  xDataType := nil;
  if (aBind is TXml) then
  begin
    if Assigned ((aBind as TXml).Xsd) then
    begin
      AddProperty('NameSpace', (aBind as TXml).Xsd.NameSpace);
      AddProperty('ContentModel', (aBind as TXml).Xsd.ContentModel);
      AddProperty('DerivationMethod', (aBind as TXml).Xsd.DerivationMethod);
      AddProperty('minOccurs', (aBind as TXml).Xsd.minOccurs);
      AddProperty('maxOccurs', (aBind as TXml).Xsd.maxOccurs);
{      xDataType := Xml.Xsd; }
      xDataType := (aBind as TXml).DataType;
    end;
  end;
  if (aBind is TXmlAttribute) then
  begin
    if Assigned ((aBind as TXmlAttribute).XsdAttr) then
      AddProperty('Use', (aBind as TXmlAttribute).XsdAttr.Use);
    xDataType := (aBind as TXmlAttribute).XsdAttr;
  end;
  if Assigned (xDataType) then
  begin
    if xDataType._Error then
      AddProperty ('Error', 'Parse failure');
    AddProperty ('DataType', xDataType.DataTypeName);
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
  end;
end;

procedure TXmlUtil.ListXsdDocumentation(aMemo: TMemo; aBind: TCustomBindable);
var
  xDataType: TXsdDataType;
begin
  aMemo.Clear;
  xDataType := nil;
  if aBind is TXmlAttribute then
    xDataType := (aBind as TXmlAttribute).XsdAttr
  else
    if aBind is TXml then
      xDataType := (aBind as TXml).Xsd;
  if Assigned (xDataType)then
    aMemo.Lines.Text := xDataType.Documentation.Text;
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
      xDataType := (aBind as TXml).Xsd;
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

function TXmlUtil.CreateXsdFromXml(aXml: TXml; aLinkXmlToXsd: Boolean): TXsd;
  procedure _AddXsdAttribute (aXsd: TXsd; aAtt: TXmlAttribute);
  var
    x: Integer;
    xXsdAtt: TXsdAttr;
  begin
    xXsdAtt := nil;
    for x := 0 to aXsd.AttributeDefs.Count - 1 do
      if aXsd.AttributeDefs.XsdAttrs [x].Name = aAtt.Name then
        xXsdAtt := aXsd.AttributeDefs.XsdAttrs [x];
    if not Assigned (xXsdAtt) then
    begin
      xXsdAtt := TXsdAttr.Create;
      xXsdAtt.Name := aAtt.Name;
      aXsd.AttributeDefs.AddObject('', xXsdAtt);
    end;
    if aLinkXmlToXsd then
      aAtt.XsdAttr := xXsdAtt;
  end;
  procedure _ChildXml (aXsd: TXsd; aXml: TXml);
  var
    x: Integer;
    xXsd: TXsd;
    xName: String;
  begin
    xXsd := nil;
    xName := NameWithoutPrefix(aXml.Name);
    for x := 0 to aXsd.ElementDefs.Count - 1 do
      if aXsd.ElementDefs.Xsds[x].Name = xName then
        xXsd := aXsd.ElementDefs.Xsds [x];
    if not Assigned (xXsd) then
    begin
      xXsd := TXsd.Create;
      aXsd.ElementDefs.AddObject('', xXsd);
      xXsd.minOccurs := '0';
      xXsd.maxOccurs := '1';
      xXsd.Name := xName;
      xXsd.DataTypeName := 'Fictitious';
      xXsd.DoNotEncode := True;
    end
    else
    begin
      xXsd.maxOccurs := 'unbounded';
    end;
    if aLinkXmlToXsd then
      aXml.Xsd := xXsd;
    for x := 0 to aXml.Attributes.Count - 1 do
      _AddXsdAttribute (xXsd, aXml.Attributes.XmlAttributes[x]);
    for x := 0 to aXml.Items.Count - 1 do
      _ChildXml (xXsd, aXml.Items.XmlItems[x]);
  end;
var
  x: Integer;
begin
  if not Assigned (aXml) then Exit;
  try
    result := TXsd.Create;
    result.Name := NameWithoutPrefix(aXml.Name);
    result.maxOccurs := '1';
    result.DataTypeName := 'Fictitious';
    result.DoNotEncode := True;
    if aLinkXmlToXsd then
      aXml.Xsd := result;
    for x := 0 to aXml.Attributes.Count - 1 do
      _AddXsdAttribute (result, aXml.Attributes.XmlAttributes[x]);
    for x := 0 to aXml.Items.Count - 1 do
      _ChildXml (result, aXml.Items.XmlItems[x]);
  finally
  end;
end;

procedure TXmlUtil.ViewAsXml(aBind: TCustomBindable; aReadOnly: BOolean);
var
  xCaption: String;
  s: String;
  xForm: TShowXmlForm;
begin
  if not Assigned (aBind) then
    raise Exception.Create('Xml not Assigned');
  if not (aBind is TXml) then
    raise Exception.Create('only allowed on elements');
  Application.CreateForm(TShowXmlForm, xForm);
  try
    xForm.Caption := (aBind as TXml).FullIndexCaption;
    xForm.xml := aBind as TXml;
    xForm.IsReadOnly := aReadonly;
    xForm.ShowModal;
  finally
    xForm.Free;
  end;
end;

procedure TXmlUtil.ZoomAsHtml(aBind: TCustomBindable);
var
  xScreen: TShowHtmlForm;
begin
  if not Assigned (aBind) then Exit;
  Application.CreateForm(TShowHtmlForm, xScreen);
  try
    xScreen.Caption := aBind.Name + ' as HTML';
    xScreen.Html := aBind.Value;
    xScreen.ShowModal;
  finally
    FreeAndNil (xScreen);
  end;
end;

procedure TXmlUtil.ZoomAsBase64(aBind: TCustomBindable);
begin
  if not Assigned (aBind) then Exit;
  Application.CreateForm(TipmInfoForm, ipmInfoForm);
  try
    ipmInfoForm.Caption := aBind.Name + ' as Base64 Text';
    ipmInfoForm.Memo.Lines.Text := B64Decode(aBind.Value);
    ipmInfoForm.ShowModal;
  finally
    FreeAndNil (ipmInfoForm);
  end;
end;

procedure TXmlUtil.ZoomAsPDF(aBind: TCustomBindable);
var
  TempFileName: String;
begin
  if not Assigned (aBind) then Exit;
  TempFileName := GetEnvironmentVariable ('Temp')
                + '\temp.pdf'
                ;
  SaveStringToFile(TempFileName, B64Decode (aBind.Value));
  Application.CreateForm(TShowPfdFileForm, ShowPfdFileForm);
  try
    ShowPfdFileForm.Caption := aBind.Name + ' as PDF';
    ShowPfdFileForm.Url := TempFileName;
    ShowPfdFileForm.ShowModal;
  finally
    FreeAndNil (ShowPfdFileForm);
  end;
end;

procedure TXmlUtil.CheckValueAgainstXsd(aBind: TCustomBindable);
var
  xMessage: String;
begin
  if aBind is TXmlAttribute then
  begin
    if not (aBind as TXmlAttribute).IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
  end
  else
  begin
    if not (aBind as TXml).IsValueValidAgainstXsd(xMessage) then
      ShowMessage (xMessage);
  end;
end;

initialization
  XmlUtil := TXmlUtil.Create;

finalization
  XmlUtil.Free;

end.
