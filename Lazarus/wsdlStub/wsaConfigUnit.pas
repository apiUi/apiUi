unit wsaConfigUnit;

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
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez, ComCtrls, Xmlz;

type
  TwsaConfigForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    EnabledCheckBox: TCheckBox;
    mustUnderstandCheckBox: TCheckBox;
    MustUnderstandComboBox: TComboBox;
    AsynchDialogCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure wsaButtonClick(Sender: TObject);
    procedure EnableComponents(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fdoReadOnly: Boolean;
    procedure setwsaEnabled(const Value: boolean);
    procedure setdoReadOnly(const Value: Boolean);
    procedure setwsaMustUnderstand(const Value: Boolean);
    procedure setwsaSpecificMustUnderstand(const Value: Boolean);
    function getwsaMustUnderstand: Boolean;
    function getwsaSpecificMustUnderstand: Boolean;
    function getwsaEnabled: boolean;
    function getAsynchronousDialog: Boolean;
    procedure setAsynchronousDialog(const Value: Boolean);
  public
    wsaXml: TXml;
    property doReadOnly: Boolean read fdoReadOnly write setdoReadOnly;
    property wsaEnabled: boolean read getwsaEnabled write setwsaEnabled;
    property wsaSpecificMustUnderstand: Boolean read getwsaSpecificMustUnderstand write setwsaSpecificMustUnderstand;
    property wsaMustUnderstand: Boolean read getwsaMustUnderstand write setwsaMustUnderstand;
    property AsynchronousDialog: Boolean read getAsynchronousDialog write setAsynchronousDialog;
  end;

var
  wsaConfigForm: TwsaConfigForm;

implementation

uses ShowXmlUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TwsaConfigForm.wsaButtonClick(Sender: TObject);
begin
  if Assigned (wsaXml) then
  begin
    Application.CreateForm(TShowXmlForm, ShowXmlForm);
    try
      ShowXmlForm.Caption := 'WS-Addressing header data';
      ShowXmlForm.Bind := wsaXml;
      ShowXmlForm.isReadOnly := doReadOnly;
      ShowXmlForm.ShowModal;
    finally
      FreeAndNil (ShowXmlForm);
    end;
  end;
end;

procedure TwsaConfigForm.EnableComponents(Sender: TObject);
begin
  mustUnderstandCheckBox.Enabled := EnabledCheckBox.Checked;
  MustUnderstandComboBox.Enabled := mustUnderstandCheckBox.Enabled
                                and mustUnderstandCheckBox.Checked;
  AsynchDialogCheckBox.Enabled := EnabledCheckBox.Checked;
end;

procedure TwsaConfigForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  IniFile := TFormIniFile.Create (Self);
end;

procedure TwsaConfigForm.FormDestroy(Sender: TObject);
begin
  IniFile.Free;
end;

procedure TwsaConfigForm.FormShow(Sender: TObject);
begin
//wsaButton.Enabled := Assigned (wsaXml);
  EnableComponents(nil);
end;

function TwsaConfigForm.getAsynchronousDialog: Boolean;
begin
  result := AsynchDialogCheckBox.Checked;
end;

function TwsaConfigForm.getwsaEnabled: boolean;
begin
  result := EnabledCheckBox.Checked;
end;

function TwsaConfigForm.getwsaMustUnderstand: Boolean;
begin
  result := (MustUnderstandComboBox.ItemIndex = 0);
end;

function TwsaConfigForm.getwsaSpecificMustUnderstand: Boolean;
begin
  result := mustUnderstandCheckBox.Checked;
end;

procedure TwsaConfigForm.setAsynchronousDialog(const Value: Boolean);
begin
  AsynchDialogCheckBox.Checked := Value;
end;

procedure TwsaConfigForm.setdoReadOnly(const Value: Boolean);
begin
  fdoReadOnly := Value;
  EnabledCheckBox.Enabled := not doReadOnly;
  mustUnderstandCheckBox.Enabled := not doReadOnly;
  mustUnderstandComboBox.Enabled := not doReadOnly;
end;

procedure TwsaConfigForm.setwsaEnabled(const Value: boolean);
begin
  EnabledCheckBox.Checked := Value;
end;

procedure TwsaConfigForm.setwsaMustUnderstand(const Value: Boolean);
begin
  if Value then
    MustUnderstandComboBox.ItemIndex := 0
  else
    MustUnderstandComboBox.ItemIndex := 1;
end;

procedure TwsaConfigForm.setwsaSpecificMustUnderstand(const Value: Boolean);
begin
  mustUnderstandCheckBox.Checked := Value;
end;

end.

