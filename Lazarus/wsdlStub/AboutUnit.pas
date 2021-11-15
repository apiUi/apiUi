{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
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
    CopyRightLabel: TLabel;
    LicensedLabel: TLabel;
    procedure BuildLabelMouseMove (Sender : TObject ; Shift : TShiftState ; X ,
      Y : Integer );
    procedure FormCreate(Sender: TObject);
    procedure FormShow (Sender : TObject );
  private
    procedure setCopyRight (AValue : String );
    procedure setLicensedTo(const Value: String);
    { Private declarations }
  public
    ProgName, VersionInfo: String;
    property LicensedTo: String write setLicensedTo;
    property CopyRight: String write setCopyRight;
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
  if Progname = '' then
    ProgName := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '');
  BuildLabel.Caption := 'Date: ' + {$I %date%};
  LazarusLabel.Caption := 'Lazarus version: ' + LCLVersion;
  FPCLabel.Caption := 'FPC version: ' + {$I %fpcversion%};
  Caption := 'About ' + ProgName;
  ProgramLabel.Caption := ProgName + ' ' + VersionInfo;
end;

procedure TAboutBox .setCopyRight (AValue : String );
begin
  CopyRightLabel.Caption := AValue;
end;

procedure TAboutBox .BuildLabelMouseMove (Sender : TObject ;
  Shift : TShiftState ; X , Y : Integer );
begin

end;

procedure TAboutBox.setLicensedTo(const Value: String);
begin
  LicensedLabel.Caption := 'Licensed to ' + Value;
  if Value = '' then
    LicensedLabel.Caption := '';
  {$ifdef TrialVersion}
  LicensedLabel.Caption := 'Trial';
  {$endif}
end;

end.

