unit AboutUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BuildLabel: TLabel;
    LazarusLabel : TLabel ;
    FPCLabel : TLabel ;
    OKButton: TButton;
    ProgramLabel: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    LicensedLabel: TLabel;
    procedure BuildLabelMouseMove (Sender : TObject ; Shift : TShiftState ; X ,
      Y : Integer );
    procedure FormCreate(Sender: TObject);
    procedure FormShow (Sender : TObject );
  private
    procedure setLicensedTo(const Value: String);
    { Private declarations }
  public
    ProgName, VersionInfo: String;
    property LicensedTo: String write setLicensedTo;
  end;

var
  AboutBox: TAboutBox;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
end;

procedure TAboutBox .FormShow (Sender : TObject );
begin
  BuildLabel.Caption := 'Date: ' + {$I %date%};
  LazarusLabel.Caption := 'Lazarus version: ' + LCLVersion;
  FPCLabel.Caption := 'FPC version: ' + {$I %fpcversion%};
  Caption := 'About ' + ProgName;
  ProgramLabel.Caption := ProgName + ' ' + VersionInfo;
end;

procedure TAboutBox .BuildLabelMouseMove (Sender : TObject ;
  Shift : TShiftState ; X , Y : Integer );
begin

end;

procedure TAboutBox.setLicensedTo(const Value: String);
begin
  LicensedLabel.Caption := 'Licensed to ' + Value;
  {$ifdef TrialVersion}
  LicensedLabel.Caption := 'Trial';
  {$endif}
end;

end.

