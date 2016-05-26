unit PromptTacoUnit;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TPromptTacoForm = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    AuthEdit: TEdit;
    PortEdit: TEdit;
    HostEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure CheckOkEnabled(Sender: TObject);
  private
    function getAddress: String;
    function getAuthorisation: String;
    function getPort: Integer;
    procedure setAddress(const Value: String);
    procedure setAuthorisation(const Value: String);
    procedure setPort(const Value: Integer);
    { Private declarations }
  public
    property Address: String read getAddress write setAddress;
    property Port: Integer read getPort write setPort;
    property Authorisation: String read getAuthorisation write setAuthorisation;
  end;

var
  PromptTacoForm: TPromptTacoForm;

implementation

{$R *.lfm}

procedure TPromptTacoForm.FormShow(Sender: TObject);
begin
  AuthEdit.SetFocus;
  CheckOkEnabled(nil);
end;

function TPromptTacoForm.getAddress: String;
begin
  result := HostEdit.Text;
end;

function TPromptTacoForm.getAuthorisation: String;
begin
  result := AuthEdit.Text;
end;

function TPromptTacoForm.getPort: Integer;
begin
  result := StrToIntDef(PortEdit.Text, 0);
end;

procedure TPromptTacoForm.setAddress(const Value: String);
begin
  HostEdit.Text := Value;
end;

procedure TPromptTacoForm.setAuthorisation(const Value: String);
begin
  AuthEdit.Text := Value;
end;

procedure TPromptTacoForm.setPort(const Value: Integer);
begin
  PortEdit.Text := IntToStr(Value);
end;

procedure TPromptTacoForm.CheckOkEnabled(Sender: TObject);
var
  xFloat: Extended;
  DoEnable: Boolean;
begin
  DoEnable := True; // start optimistic
  if (PortEdit.Text <> '')
  then begin
    try
      xFloat := StrToFloat (PortEdit.Text);
    except
      DoEnable := False;
    end;
  end;
  OkButton.Enabled := DoEnable;
end;

end.
