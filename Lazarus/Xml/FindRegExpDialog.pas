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
unit FindRegExpDialog;

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
  Buttons, ExtCtrls, FormIniFilez;

type

  { TFindDlg }

  TFindDlg = class(TForm)
    SearchEdit: TLabeledEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    SearchInRadioGroup: TRadioGroup;
    ScopeRadioGroup: TRadioGroup;
    RegularExpressionCheckBox: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    { Public declarations }
  end;

var
  FindDlg: TFindDlg;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFindDlg.FormShow(Sender: TObject);
begin
  SearchEdit.SelectAll;
  SearchEdit.SetFocus;
end;

procedure TFindDlg.OKBtnClick(Sender: TObject);
begin

end;

procedure TFindDlg.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create(self, True);
  IniFile.Restore;
  SearchEdit.Text := IniFile.StringByName['SearchText'];
  RegularExpressionCheckBox.Checked:=IniFile.BooleanByName['isRegExp'];
  SearchInRadioGroup.ItemIndex:=IniFile.IntegerByName['SearchIn'];
  ScopeRadioGroup.ItemIndex:=IniFile.IntegerByName['Scope'];
end;

procedure TFindDlg.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['SearchText']:=SearchEdit.Text;
  IniFile.BooleanByName['isRegExp']:=RegularExpressionCheckBox.Checked;
  IniFile.IntegerByName['SearchIn']:=SearchInRadioGroup.ItemIndex;
  IniFile.IntegerByName['Scope']:=ScopeRadioGroup.ItemIndex;
  IniFile.Save;
  IniFile.Free;
end;

end.
