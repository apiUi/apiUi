unit EditConnectionStringUnit;

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
  SysUtils, Classes, sqldb, oracleconnection, odbcconn, Graphics, Forms,
  Controls, StdCtrls, Buttons, ComCtrls, ExtCtrls, Dialogs, ActnList, Menus;

type

  { TOpenSQLServerForm }

  TOpenSQLServerForm = class(TForm)
    DatabaseNameEdit: TLabeledEdit;
    HostEdit: TLabeledEdit;
    ParamsEdit: TLabeledEdit;
    Panel1: TPanel;
    PageControl1: TPageControl;
    dbc: TSQLConnector;
    UserNameEdit: TLabeledEdit;
    TabSheet1: TTabSheet;
    PasswordEdit: TLabeledEdit;
    TestConButton: TBitBtn;
    ConnectorTypeEdit: TLabeledEdit;
    ActionList1: TActionList;
    TestConnectionAction: TAction;
    SaveAction: TAction;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    OpenAction: TAction;
    SaveAsAction: TAction;
    CloseAction: TAction;
    Saveas1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    procedure FiledChanged(Sender: TObject);
    procedure TestConnectionActionExecute(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
    procedure TestConnectionActionUpdate(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    EncryptionSeed: String;
    fConnStringFileName: String;
    function SimpleEncrypt(const Source: String): String;
    function getConnectionString: String;
    procedure setConnectionString(const Value: String);
    procedure OpenFileNamed (aFileName: String);
    procedure setConnStringFileName(const Value: String);
  public
    function DecryptString (aString: String): String;
    function EncryptString (aString: String): String;
    property ConnStringFileName: String read fConnStringFileName write setConnStringFileName;
    property ConnectionString: String read getConnectionString write setConnectionString;
  end;

var
  OpenSQLServerForm: TOpenSQLServerForm;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses db
   , IniFiles
   , igGlobals
   , Xmlz
   , xmlUtilz
   ;

procedure TOpenSQLServerForm.TestConnectionActionUpdate(Sender: TObject);
begin
  TestConnectionAction.Enabled := (ConnectorTypeEdit.Text <> '')
                              and (PasswordEdit.Text <> '')
                                ;
end;

procedure TOpenSQLServerForm.SaveActionExecute(Sender: TObject);
var
  od: TOpenDialog;
  xml: TXml;
begin
  TestConnectionActionExecute(Sender);
  xml := TXml.CreateAsString('DataSource', '');
  try
    xml.AddXml(TXml.CreateAsString('ConnectorType', ConnectorTypeEdit.Text));
    xml.AddXml(TXml.CreateAsString('DatabaseName', DatabaseNameEdit.Text));
    xml.AddXml(TXml.CreateAsString('HostName', HostEdit.Text));
    xml.AddXml(TXml.CreateAsString('UserName', UserNameEdit.Text));
    xml.AddXml(TXml.CreateAsString('Params', ParamsEdit.Text));
    xml.AddXml(TXml.CreateAsString('Password', XmlUtil.SimpleEncrypt(PasswordEdit.Text)));
    SaveStringToFile(ConnStringFileName, xml.Text);
  finally
    xml.Free;
  end;
end;

procedure TOpenSQLServerForm.SaveActionUpdate(Sender: TObject);
begin
  SaveAction.Enabled := (ConnStringFileName <> '');
end;

procedure TOpenSQLServerForm.SaveAsActionExecute(Sender: TObject);
var
  od: TOpenDialog;
  xml: TXml;
begin
  TestConnectionActionExecute(Sender);
  od := TSaveDialog.Create(nil);
  try
    with od do
    begin
      FileName := ConnStringFileName;
      DefaultExt := 'ConnStrng';
      Filter := 'ConnStrng file (*.ConnStrng)|*.ConnStrng';
      Title := 'Open ConnectionString File';
      Options := Options + [ofOverwritePrompt, ofPathMustExist];
    end;
    if od.Execute then
    begin
      xml := TXml.CreateAsString('DataSource', '');
      try
        xml.AddXml(TXml.CreateAsString('ConnectorType', ConnectorTypeEdit.Text));
        xml.AddXml(TXml.CreateAsString('DatabaseName', DatabaseNameEdit.Text));
        xml.AddXml(TXml.CreateAsString('HostName', HostEdit.Text));
        xml.AddXml(TXml.CreateAsString('UserName', UserNameEdit.Text));
        xml.AddXml(TXml.CreateAsString('Params', ParamsEdit.Text));
        xml.AddXml(TXml.CreateAsString('Password', XmlUtil.SimpleEncrypt(PasswordEdit.Text)));
        SaveStringToFile(od.FileName, xml.Text);
        ConnStringFileName := od.FileName;
      finally
        xml.Free;
      end;
    end;
  finally
    od.Free;
  end;
end;

procedure TOpenSQLServerForm.setConnectionString(const Value: String);
begin
  ConnectorTypeEdit.Text := Value;
end;

procedure TOpenSQLServerForm.setConnStringFileName(const Value: String);
begin
  fConnStringFileName := Value;
  Caption := 'Datasource file ' + Value;
end;

procedure TOpenSQLServerForm.TestConnectionActionExecute(Sender: TObject);
var
  swapCursor: TCursor;
  s: String;
  dbs: TSQLConnector;
begin
  StatusBar.Panels [0].Text := 'Connecting...';
  StatusBar.Update;
  swapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  dbs := TSQLConnector.Create(nil);
  try
    dbs.ConnectorType:=ConnectorTypeEdit.Text;
    dbs.DatabaseName:=DatabaseNameEdit.Text;
    dbs.HostName:=HostEdit.Text;
    dbs.Password:=PasswordEdit.Text;
    dbs.UserName:=UserNameEdit.Text;
    s :=ReplaceStrings(ParamsEdit.Text, '%pwd%', PasswordEdit.Text, false, false);
    dbs.Params.Text := ReplaceStrings(s, ';', LineEnding, false, false);
    try
      try
        dbs.Connected:=True;
        dbs.Connected:=False;
        dbs.Connected:=True;
        StatusBar.Panels [0].Text := 'Connection successful';
      except
        StatusBar.Panels[0].Text := 'Connection failed';
        Raise;
      end;
    finally
      dbs.Connected:=False;
    end;
  finally
    dbs.Free;
    Screen.Cursor := swapCursor;
  end;
end;

procedure TOpenSQLServerForm.Close1Click(Sender: TObject);
begin
 Close;
end;

procedure TOpenSQLServerForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

function TOpenSQLServerForm.DecryptString(aString: String): String;
begin
  EncryptionSeed := 'fh^ruh54bdkjbkjb44458&*';
//  result := SimpleEncrypt(B64Decode (aString));
end;

function TOpenSQLServerForm.EncryptString(aString: String): String;
begin
  EncryptionSeed := 'fh^ruh54bdkjbkjb44458&*';
//  result := B64Encode (SimpleEncrypt(aString));
end;

procedure TOpenSQLServerForm.FiledChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := '';
end;

procedure TOpenSQLServerForm.FormShow(Sender: TObject);
begin
  if ParamCount > 0 then
    OpenFileNamed (ParamStr(1));
end;

function TOpenSQLServerForm.getConnectionString: String;
begin
  result := ConnectorTypeEdit.Text;
end;

procedure TOpenSQLServerForm.OpenActionExecute(Sender: TObject);
begin;
  with TOpenDialog.Create(nil) do
  try
    FileName := ConnStringFileName;
    DefaultExt := 'ConnStrng';
    Filter := 'ConnStrng file (*.ConnStrng)|*.ConnStrng';
    Title := 'Open ConnectionString File';
    Options := Options + [ofFileMustExist];
    if Execute then
      OpenFileNamed(FileName);
  finally
    Free;
  end;
end;

procedure TOpenSQLServerForm.OpenFileNamed(aFileName: String);
var
  xml: TXml;
begin
  xml := TXml.Create;
  try
    xml.LoadFromFile(aFileName, nil);
    if xml.TagName = '' then
      raise Exception.CreateFmt('%s does not contain valid Xml', [aFileName]);
    if xml.TagName <> 'DataSource' then
      raise Exception.CreateFmt('%s does not contain valid ConnectionString data', [aFileName]);
    ConnectorTypeEdit.Text:=xml.Items.XmlValueByTag['ConnectorType'];
    DatabaseNameEdit.Text:=xml.Items.XmlValueByTag['DatabaseName'];
    HostEdit.Text:=xml.Items.XmlValueByTag['HostName'];
    ParamsEdit.Text:=xml.Items.XmlValueByTag['Params'];
    PasswordEdit.Text := XmlUtil.SimpleEncrypt(xml.Items.XmlValueByTag['Password']);
    UserNameEdit.Text:=xml.Items.XmlValueByTag['UserName'];
    ConnStringFileName := aFileName;
  finally
    xml.Free;
  end;
end;

function TOpenSQLServerForm.SimpleEncrypt(const Source: String): String;
var
  Index: Integer;
begin
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := Chr((Ord(EncryptionSeed[Index mod Length(EncryptionSeed)]) xor Ord(Source[Index])));
end;

end.

