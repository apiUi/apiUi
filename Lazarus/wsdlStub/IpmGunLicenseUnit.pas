unit IpmGunLicenseUnit;

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
  Buttons, ComCtrls, ExtCtrls, IpmGunLicense;

type

  { TIpmGunLicenseForm }

  TIpmGunLicenseForm = class(TForm)
    CancelButton : TBitBtn ;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel2: TPanel;
    CompanyEdit: TLabeledEdit;
    LicenseDateEdit: TLabeledEdit;
    BaseEdit: TLabeledEdit;
    LicenseEdit: TLabeledEdit;
    procedure OKBtnClick(Sender: TObject);
  private
    function getCompany: String;
    function getLicenseExpirationDate: String;
    function getLicenseString: String;
    procedure setBaseString(const Value: String);
    procedure setCompany(const Value: String);
    procedure setLicenseExpirationDate(const Value: String);
    procedure setLicenseString(const Value: String);
    { Private declarations }
  public
    property Company: String read getCompany write setCompany;
    property LicenseExpirationDate: String read getLicenseExpirationDate write setLicenseExpirationDate;
    property BaseString: String write setBaseString;
    property LicenseString: String read getLicenseString write setLicenseString;
  end;

var
  IpmGunLicenseForm: TIpmGunLicenseForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TIpmGunLicenseForm }

function TIpmGunLicenseForm.getCompany: String;
begin
  result := CompanyEdit.Text;
end;

function TIpmGunLicenseForm.getLicenseExpirationDate: String;
begin
  result := LicenseDateEdit.Text;
end;

function TIpmGunLicenseForm.getLicenseString: String;
begin
  result := LicenseEdit.Text;
end;

procedure TIpmGunLicenseForm.setBaseString(const Value: String);
begin
  BaseEdit.Text := Value;
end;

procedure TIpmGunLicenseForm.setCompany(const Value: String);
begin
  CompanyEdit.Text := Value;
end;

procedure TIpmGunLicenseForm.setLicenseExpirationDate(const Value: String);
begin
  LicenseDateEdit.Text := Value;
end;

procedure TIpmGunLicenseForm.setLicenseString(const Value: String);
begin
  LicenseEdit.Text := Value;
end;

procedure TIpmGunLicenseForm.OKBtnClick(Sender: TObject);
begin
  if not validateIpmLicense ( Company
                            + LicenseExpirationDate
                            + BaseEdit.Text
                            , LicenseString
                            ) then
  begin
    ModalResult := mrNone;
    raise Exception.Create('Invalid licensestring');
  end;
end;

end.

