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
unit cbAbout;

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
  Buttons, ExtCtrls, Dialogs, VersionSupport;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    toolMemo: TMemo;
    OKButton: TButton;
    NameLabel: TLabel;
    Bevel1: TBevel;
    VersionLabel: TLabel;
    CopyRightLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
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
  function _GetVersion: string;
  var
    x: TVersionInfo;
  begin
    Result := '';
    x := TVersionInfo.Create;
    try
      if x.BuildInfoAvailable then
        result := x.FileVersion
      else
        result := '(not available)';
    finally
      x.Free;
    end;
  end;
begin
  VersionLabel.Caption := 'Version: ' + _GetVersion;
  with toolMemo.Lines do
  begin
    Add ('Built on date: ' + {$i %DATE%});
    Add ('with FreePascal: ' + {$i %FPCVERSION%});
    Add ('for CPU: '+ {$i %FPCTARGETCPU%} + ' OS: '+ {$i %FPCTARGETOS%});
  end;
end;

end.

