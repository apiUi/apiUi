unit OpenXsdXmlUnit;

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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, FormIniFilez, Dialogs, Bind, Xmlz, Xsdz, Wsdlz;

type
  TOpenXsdXmlForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    XsdFileNameEdit: TComboBox;
    XmlFileNameEdit: TComboBox;
    Label2: TLabel;
    XsdButton: TButton;
    OpenDialog: TOpenDialog;
    XmlButton: TButton;
    procedure XsdButtonClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure XmlButtonClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fXsdDescr: TXsdDescr;
    fXml: TXml;
    fWsdl: TWsdl;
    fOperation: TWsdlOperation;
    function getXmlFileName: String;
    function getXsdFileName: String;
    procedure setXmlFileName(const Value: String);
    procedure setXsdFileName(const Value: String);
  public
    property Wsdl: TWsdl read fWsdl;
    property Operation: TWsdlOperation read fOperation;
    property xsdDescr: TXsdDescr read fXsdDescr;
    property Xml: TXml read fXml;
    property xsdFileName: String read getXsdFileName write setXsdFileName;
    property xmlFileName: String read getXmlFileName write setXmlFileName;
  end;

var
  OpenXsdXmlForm: TOpenXsdXmlForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TOpenXsdXmlForm.FormShow(Sender: TObject);
begin
  XsdFileNameEdit.Items.Text := IniFile.StringByName['XsdFileNameHistory'];
  XmlFileNameEdit.Items.Text := IniFile.StringByName['XmlFileNameHistory'];
  XsdFileNameEdit.SelectAll;
  XsdFileNameEdit.SetFocus;
end;

function TOpenXsdXmlForm.getXmlFileName: String;
begin
  result := XmlFileNameEdit.Text;
end;

function TOpenXsdXmlForm.getXsdFileName: String;
begin
  result := XsdFileNameEdit.Text;
end;

procedure TOpenXsdXmlForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
end;

procedure TOpenXsdXmlForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TOpenXsdXmlForm.OKBtnClick(Sender: TObject);
  procedure _saveText (aCB: TComboBox);
  var
    x: Integer;
    n: Boolean;
  begin
    n := True;
    for x := 0 to aCB.Items.Count - 1 do
      if aCB.Items.Strings[x] = aCB.Text then
        n := False;
    if n then
    begin
      aCB.Items.Insert(0, aCB.Text);
      if aCB.Items.Count > 10 then
        aCB.Items.Delete(aCB.Items.Count - 1);
    end;
  end;
  function FindWsdlXml (aWsdl: TWsdl; var aOperation: TWsdlOperation; aXml: TXml): TXml;
  var
    x, s, o: Integer;
    xXml, oXml: TXml;
    eBind: TCustomBindable;
    xWsdl: TWsdl;
    xService: TWsdlService;
    xKey: String;
  begin
    result := nil;
    {
      <?xml version="1.0" encoding="UTF-8" ?>
      <!-- 2/21/2009 10:31:26 PM: Generated with IpmGun by Bouwman -->
      <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd1="http://soapinterop.org/" xmlns:typens="http://soapinterop.org/xsd" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">
        <SOAP-ENV:Body>
          <mns:echoStructResponse xmlns:mns="http://soapinterop.org/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
            <return xsi:type="typens:SOAPStruct">
              <varInt xsi:type="xsd:int">1</varInt>
              <varFloat xsi:type="xsd:float">12</varFloat>
              <varString xsi:type="xsd:string">Jan</varString>
            </return>
          </mns:echoStructResponse>
        </SOAP-ENV:Body>
      </SOAP-ENV:Envelope>
    }
    if NameWithoutPrefix(aXml.TagName) = 'Envelope' then
    begin
      for x := 0 to aXml.Items.Count - 1 do
      begin
        xXml := aXml.Items.XmlItems [x];
        if (NameWithoutPrefix(xXml.TagName) = 'Body')
        and (xXml.Items.Count > 0) then
        begin
          xKey := NameWithoutPrefix (xXml.Items.XmlItems[0].TagName);
          for s := 0 to aWsdl.Services.Count - 1 do
          begin
            xService := aWsdl.Services.Services[s];
            for o := 0 to xService.Operations.Count - 1 do
            begin
              aOperation := xService.Operations.Operations[o];
              oXml := (aOperation.reqBind as TXml);
              if (oXml.Items.Count > aOperation.InputHeaders.Count)
              and (oXml.Items.XmlItems[aOperation.InputHeaders.Count].Name = xKey) then
              begin
                result := oXml;
                result.Name := 'Request';
                result.Xsd.ElementName := result.Name;
                aOperation.StubAction := saRequest;
                Exit;
              end;
              oXml := (aOperation.rpyBind as TXml);
              if (oXml.Items.Count > aOperation.OutputHeaders.Count)
              and (oXml.Items.XmlItems[aOperation.OutputHeaders.Count].Name = xKey) then
              begin
                result := oXml;
                result.Name := 'Reply';
                result.Xsd.ElementName := result.Name;
                Exit;
              end;
            end;
          end;
          raise Exception.Create('Wsdl does not describe ' + xKey);
        end;
      end;
      raise Exception.Create('no SOAP:Body found');
    end
    else
      raise Exception.Create('No Soap Envelope found in file');
  end;
  {
  function WsdlOpenFile(aName: String): TWsdl;
  var
    wsdlItems: TWSDLItems;
  begin
    wsdlItems := TWSDLItems.Create (self);
    try
      wsdlItems.Active := True;
      TWsdlItemsLoader(wsdlItems).Load(aName);
      result := TWsdl.Create(1,1, False);
      try
        result.LoadFromWsdlItems(wsdlItems);
      except
        result.Free;
        result := nil;
        raise;
      end;
    finally
      wsdlItems.Active := False;
    end;
  end;
  }
var
  xXsd: TXsd;
  xXml: TXml;
  x: Integer;
  xName: String;
begin
  try
    xXml := TXml.Create;
    try
      xXml.LoadFromFile(XmlFileName, nil);
      if xXml.Name = '' then
        raise Exception.Create('Illegal XML file');
      if UpperCase(ExtractFileExt(XsdFileName)) = '.XSD' then
      begin
        fXsdDescr := TXsdDescr.Create(1);
        try
          fXsdDescr.LoadXsdFromFile(XsdFileName, nil);
          xXsd := fXsdDescr.TypeDef.ElementByName [xXml.Name];
          if not Assigned (xXsd) then
            raise Exception.Create('Xml Schema does not describe XML root element ' + xXml.Name);
          fXml := TXml.Create(0, xXsd);
          try
            fXml.LoadValues(xXml, False);
            _saveText (XsdFileNameEdit);
            _saveText (XmlFileNameEdit);
            IniFile.StringByName['XsdFileNameHistory'] := XsdFileNameEdit.Items.Text;
            IniFile.StringByName['XmlFileNameHistory'] := XmlFileNameEdit.Items.Text;
          except
            FreeAndNil (fXml);
            raise;
          end;
        except
          FreeAndNil (fXsdDescr);
          raise;
        end;
      end
      else
      begin
        {$ifdef XMLDOM}
        fWsdl := WsdlOpenFile (xsdFileName);
        try
          fXml := FindWsdlXml (fWsdl, fOperation, xXml);
          for x := 0 to xXml.Items.Count - 1 do
          begin
            xName := xXml.Items.XmlItems[x].Name;
            xXml.Items.XmlItems[x].Name := fXml.Name;
            fXml.LoadValues(xXml.Items.XmlItems[x], False, False);
            xXml.Items.XmlItems[x].Name := xName;
          end;
          _saveText (XsdFileNameEdit);
          _saveText (XmlFileNameEdit);
          IniFile.StringByName['XsdFileNameHistory'] := XsdFileNameEdit.Items.Text;
          IniFile.StringByName['XmlFileNameHistory'] := XmlFileNameEdit.Items.Text;
        except
          FreeAndNil (fWsdl);
          raise;
        end;
        {$else}
          raise Exception.Create('Opening of Wsdls not yet implemented');
        {$endif}
      end;
    finally
      xXml.Free;
    end;
  except
    modalResult := mrNone;
    raise;
  end;
end;

procedure TOpenXsdXmlForm.setXmlFileName(const Value: String);
begin
  XmlFileNameEdit.Text := Value;
end;

procedure TOpenXsdXmlForm.setXsdFileName(const Value: String);
begin
  XsdFileNameEdit.Text := Value;
end;

procedure TOpenXsdXmlForm.XmlButtonClick(Sender: TObject);
begin
  OpenDialog.FileName := XmlFileName;
  OpenDialog.DefaultExt := 'xml';
  OpenDialog.Filter := 'XML File (*.xml)|*.xml';
  if OpenDialog.Execute then
    XmlFileName := OpenDialog.FileName;
end;

procedure TOpenXsdXmlForm.XsdButtonClick(Sender: TObject);
begin
  OpenDialog.FileName := XsdFileName;
  OpenDialog.DefaultExt := 'xsd';
  OpenDialog.Filter := 'XML SchemaFile (*.xsd)|*.xsd|WebService Description Files (*.wsdl)|*.wsdl';
  if OpenDialog.Execute then
    XsdFileName := OpenDialog.FileName;
end;

end.
