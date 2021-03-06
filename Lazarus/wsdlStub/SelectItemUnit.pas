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
unit SelectItemUnit;

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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, FormIniFilez;

type
  TSelectItemForm = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    ListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetSelectedItem: String;
  public
    property SelectedItem: String read GetSelectedItem;
  end;

var
  SelectItemForm: TSelectItemForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TSelectItemForm.GetSelectedItem: String;
begin
  if ListBox.ItemIndex > -1 then
    result := ListBox.Items.Strings [ListBox.ItemIndex]
  else
    result := '';
end;

procedure TSelectItemForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TSelectItemForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TSelectItemForm.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
    ModalResult := mrOk;
end;

procedure TSelectItemForm.FormShow(Sender: TObject);
begin
  ListBox.SetFocus;
end;

end.
