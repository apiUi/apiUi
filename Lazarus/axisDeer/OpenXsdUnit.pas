unit OpenXsdUnit;

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
  Buttons, ExtCtrls, FormIniFilez, Dialogs, Bind, Xmlz, Xsdz, Wsdlz, xmlUtilz, ClipBrd;

type
  TOpenXsdForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    XsdFileNameEdit: TComboBox;
    ElementNameEdit: TComboBox;
    XsdButton: TButton;
    OpenDialog: TOpenDialog;
    SchemaTypeComboBox: TComboBox;
    ParseButton: TButton;
    procedure XsdButtonClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fWsdl: TWsdl;
    fXsdDescr: TXsdDescr;
    fXml: TXml;
    function getElementName: String;
    function getXsdFileName: String;
    procedure setElementName(const Value: String);
    procedure setXsdFileName(const Value: String);
    procedure EnableOk;
    function getSchemaType: TSchemaType;
    procedure setSchemaType(const Value: TSchemaType);
  public
    property xsdDescr: TXsdDescr read fXsdDescr;
    property SchemaType: TSchemaType read getSchemaType write setSchemaType;
    property Xml: TXml read fXml;
    property xsdFileName: String read getXsdFileName write setXsdFileName;
    property ElementName: String read getElementName write setElementName;
  end;

var
  OpenXsdForm: TOpenXsdForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TOpenXsdForm.FormShow(Sender: TObject);
begin
  XsdFileNameEdit.Items.Text := IniFile.StringByName['XsdFileNameHistory'];
  EnableOk;
end;

function TOpenXsdForm.getElementName: String;
begin
  result := ElementNameEdit.Text;
end;

function TOpenXsdForm.getSchemaType: TSchemaType;
begin
  result := TSchemaType (SchemaTypeComboBox.ItemIndex);
end;

function TOpenXsdForm.getXsdFileName: String;
begin
  result := XsdFileNameEdit.Text;
end;

procedure TOpenXsdForm.EnableOk;
begin
  OKBtn.Enabled := (ElementNameEdit.ItemIndex > -1);
end;

procedure TOpenXsdForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  try SchemaType := TSchemaType(IniFile.IntegerByName['SchemaType']) except end;
  try xsdFileName:= IniFile.StringByName['XsdFileName']; except end;
end;

procedure TOpenXsdForm.FormDestroy(Sender: TObject);
begin
  IniFile.IntegerByName['SchemaType'] := Ord (SchemaType);
  IniFile.StringByName['XsdFileName'] := xsdFileName;
  IniFile.Save;
  IniFile.Free;
end;

procedure TOpenXsdForm.OKBtnClick(Sender: TObject);
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
var
  xXsd: TXsd;
  sl: TStringList;
begin
  try
    fXsdDescr := TXsdDescr.Create(1);
    try
      if (SchemaType = stJsonType) then
        XmlUtil.CreateXsdFromJsonSchemaFile(fXsdDescr, xsdFileName);
      if (SchemaType = stXsdType) then
        fXsdDescr.LoadXsdFromFile(XsdFileName, nil);
      if (SchemaType = stWsdlType) then
      begin
        FreeAndNil(fWsdl);
        fWsdl := TWsdl.Create(nil, -1, 1, True);
        fXsdDescr := fWsdl.XsdDescr;
        fWsdl.LoadFromSchemaFile(xsdFileName, nil);
      end;
      xXsd := fXsdDescr.TypeDef.ElementByName [ElementName];
      if not Assigned (xXsd) then
        raise Exception.Create('Xml Schema does not describe XML root element ' + ElementName);
      sl := TStringList.Create;
      try
        try xXsd.GenerateReport(sl); except end;
        Clipboard.AsText:= sl.Text;
      finally
        sl.Free;
      end;
      fXml := TXml.Create(0, xXsd);
    except
      FreeAndNil (fXsdDescr);
      raise;
    end;
  except
    modalResult := mrNone;
    raise;
  end;
  _saveText(XsdFileNameEdit);
end;

procedure TOpenXsdForm.ParseButtonClick(Sender: TObject);
var
  xXsd: TXsd;
  xElementName: String;
  x: Integer;
begin
  try
    FreeAndNil(fWsdl);
    xElementName := ElementName;
    try
      if (SchemaType = stJsonType) then
      begin
        fXsdDescr := TXsdDescr.Create(1);
        XmlUtil.CreateXsdFromJsonSchemaFile(fXsdDescr, xsdFileName);
      end;
      if (SchemaType = stXsdType) then
      begin
        fXsdDescr := TXsdDescr.Create(1);
        fXsdDescr.LoadXsdFromFile(XsdFileName, nil);
      end;
      if (SchemaType = stWsdlType) then
      begin
        FreeAndNil(fWsdl);
        fWsdl := TWsdl.Create(nil, -1, 1, True);
        fXsdDescr := fWsdl.XsdDescr;
        fWsdl.LoadFromSchemaFile(xsdFileName, nil);
      end;
      ElementNameEdit.Clear;
      for x := 0 to fXsdDescr.TypeDef.ElementDefs.Count - 1 do
        ElementNameEdit.Items.Add(fXsdDescr.TypeDef.ElementDefs.Xsds[x].ElementName);
      xXsd := fXsdDescr.TypeDef.ElementByName [xElementName];
      if Assigned (xXsd) then
        ElementName := xElementName
      else
      begin
        if ElementNameEdit.Items.Count > 0 then
        begin
          ElementNameEdit.ItemIndex := 0;
          xXsd := fXsdDescr.TypeDef.ElementByName [ElementName];
        end;
      end;
      if Assigned (xXsd) then
        fXml := TXml.Create(0, xXsd)
      else
        raise Exception.Create('No root element found');
      EnableOk;
    except
      if not Assigned(fWsdl) then
        FreeAndNil (fXsdDescr);
      FreeAndNil(fWsdl);
      raise;
    end;
  except
    modalResult := mrNone;
    raise;
  end;
end;

procedure TOpenXsdForm.setElementName(const Value: String);
begin
  ElementNameEdit.Text := Value;
end;

procedure TOpenXsdForm.setSchemaType(const Value: TSchemaType);
begin
  SchemaTypeComboBox.ItemIndex := Ord (Value);
end;

procedure TOpenXsdForm.setXsdFileName(const Value: String);
begin
  XsdFileNameEdit.Text := Value;
end;

procedure TOpenXsdForm.XsdButtonClick(Sender: TObject);
var
  x: Integer;
  xXsdDescr: TXsdDescr;
  xJsonSchema: TXml;
begin
  OpenDialog.FileName := XsdFileName;
  OpenDialog.DefaultExt := 'xsd';
  OpenDialog.Filter := 'XML SchemaFile (*.xsd)|*.xsd|Json SchemaFile (*.json)|*.json';
  if OpenDialog.Execute then
  begin
    XsdFileName := OpenDialog.FileName;
    ElementNameEdit.Items.Clear;
    xXsdDescr := TXsdDescr.Create(1);
    try
      if SchemaType = stJsonType then
        XmlUtil.CreateXsdFromJsonSchemaFile(xXsdDescr, xsdFileName)
      else
        xXsdDescr.LoadXsdFromFile(XsdFileName, nil);
      for x := 0 to xXsdDescr.TypeDef.ElementDefs.Count - 1 do
        ElementNameEdit.Items.Add(xXsdDescr.TypeDef.ElementDefs.Xsds[x].ElementName);
      if ElementNameEdit.Items.Count > 0 then
        ElementNameEdit.ItemIndex := 0;
      EnableOk;
    finally
      xXsdDescr.Free;
    end;
  end;
end;

end.
