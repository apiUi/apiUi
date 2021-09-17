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
unit ErrorFound;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Dialogs, Buttons, ExtCtrls
  ;

type
  TErrorFoundDlg = class(TForm)
    OKBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    FileNameEdit: TEdit;
    LineNumberLabel: TLabel;
    LineNumberEdit: TEdit;
    ColumnNumberLabel: TLabel;
    ColumnNumberEdit: TEdit;
    TokenStringLabel: TLabel;
    TokenStringEdit: TEdit;
    EditButton: TButton;
    procedure EditButtonClick(Sender: TObject);
  private
  public
    Viewer: String;
  end;

var
  ErrorFoundDlg: TErrorFoundDlg;

implementation

{$R *.lfm}

procedure TErrorFoundDlg.EditButtonClick(Sender: TObject);
begin
  if Viewer <> '' then
//    WinExec(PAnsiChar(Viewer + ' ' + FileNameEdit.Text), 1)
  else
    ShowMessage ('No viewer specified');
end;

end.
