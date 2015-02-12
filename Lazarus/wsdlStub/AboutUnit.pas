unit AboutUnit;

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
  Buttons, ExtCtrls, SysUtils;

type
  TAboutBox = class(TForm)
    OKButton: TButton;
    Label1: TLabel;
    Bevel1: TBevel;
    VersionLabel: TLabel;
    Label3: TLabel;
    LicensedLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure setProgName(const Value: String);
    procedure setLicensedTo(const Value: String);
    { Private declarations }
  public
    property ProgName: String write setProgName;
    property LicensedTo: String write setLicensedTo;
  end;

var
  AboutBox: TAboutBox;

implementation
function GetVersion: string;
begin
  Result := 'verzie';
end;


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version: ' + GetVersion;
end;

procedure TAboutBox.setLicensedTo(const Value: String);
begin
  LicensedLabel.Caption := 'Licensed to ' + Value;
end;

procedure TAboutBox.setProgName(const Value: String);
begin
  Caption := 'About ' + Value;
  Label1.Caption := Value;
end;

end.

