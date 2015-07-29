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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Dialogs, ActnList, Menus;

type
  TOpenSQLServerForm = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PasswordEdit: TLabeledEdit;
    TestConButton: TBitBtn;
    ServerNameEdit: TLabeledEdit;
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
    Label1: TLabel;
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
    function getPassword: String;
    procedure setPassword(const Value: String);
    procedure OpenFileNamed (aFileName: String);
    procedure setConnStringFileName(const Value: String);
  public
    function DecryptString (aString: String): String;
    function EncryptString (aString: String): String;
    property ConnStringFileName: String read fConnStringFileName write setConnStringFileName;
    property ConnectionString: String read getConnectionString write setConnectionString;
    property Password: String read getPassword write setPassword;
  end;

var
  OpenSQLServerForm: TOpenSQLServerForm;

implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
{$IFnDEF FPC}
  adodb,
{$ELSE}
  sqldb,
{$ENDIF}
  adoint
   , oledb
   , db
   , IniFiles
   , igGlobals
   , Xmlz
   , xmlUtilz
   ;

procedure TOpenSQLServerForm.TestConnectionActionUpdate(Sender: TObject);
begin
  TestConnectionAction.Enabled := (ServerNameEdit.Text <> '')
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
  xml.AddXml(TXml.CreateAsString('ConnectionString', ConnectionString));
  xml.AddXml(TXml.CreateAsString('Password', XmlUtil.SimpleEncrypt(Password)));
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
  od := TOpenDialog.Create(nil);
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
      xml.AddXml(TXml.CreateAsString('ConnectionString', ConnectionString));
      xml.AddXml(TXml.CreateAsString('Password', XmlUtil.SimpleEncrypt(Password)));
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
  ServerNameEdit.Text := Value;
end;

procedure TOpenSQLServerForm.setConnStringFileName(const Value: String);
begin
  fConnStringFileName := Value;
  Caption := 'Datasource file ' + Value;
end;

procedure TOpenSQLServerForm.setPassword(const Value: String);
begin
  PasswordEdit.Text := Value;
end;

procedure TOpenSQLServerForm.TestConnectionActionExecute(Sender: TObject);
var
  dbc : TAdoConnection;
  swapCursor: TCursor;
  s: String;
begin
  StatusBar.Panels [0].Text := 'Connecting...';
  swapCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    dbC := TAdoConnection.Create(nil);
    try
      s := ConnectionString;
      s := ReplaceStrings(s, '%pwd%', Password, false, false);
      dbc.ConnectionString := s;
      try
        dbc.Open;
        dbc.Close;
        StatusBar.Panels [0].Text := 'Connection successful';
      except
        StatusBar.Panels[0].Text := 'Connection failed';
        Raise;
      end;
    finally
      if dbc.Connected then
        dbc.Close;
      dbc.Free;
    end;
  finally
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
  result := ServerNameEdit.Text;
end;

function TOpenSQLServerForm.getPassword: String;
begin
  result := PasswordEdit.Text;
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
    ConnectionString := xml.Items.XmlValueByTag['ConnectionString'];
    Password := XmlUtil.SimpleEncrypt(xml.Items.XmlValueByTag['Password']);
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

