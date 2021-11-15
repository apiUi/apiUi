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
unit SelectProjectFolderUnit;

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
  Buttons, ExtCtrls, Dialogs, FormIniFilez;

type

  { TSelectProjectFolderForm }

  TSelectProjectFolderForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel3: TPanel;
    Label8: TLabel;
    ProjectFolderNameEdit: TEdit;
    Button1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getFolderName : String ;
    procedure setFolderName (AValue : String );
  public
    property FolderName: String read getFolderName write setFolderName;
  end;

var
  SelectProjectFolderForm: TSelectProjectFolderForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses StrUtils;

procedure TSelectProjectFolderForm.Button1Click(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
  try
    Caption := self.Caption;
    FileName := ProjectFolderNameEdit.Text;
    if Execute then
      ProjectFolderNameEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TSelectProjectFolderForm.OKBtnClick(Sender: TObject);
begin

end;

function TSelectProjectFolderForm .getFolderName : String ;
begin
  result := ProjectFolderNameEdit.Text;
end;

procedure TSelectProjectFolderForm .setFolderName (AValue : String );
begin
  ProjectFolderNameEdit.Text := AValue;
end;

procedure TSelectProjectFolderForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  FolderName := IniFile.StringByName['WsdlLocation'];
end;

procedure TSelectProjectFolderForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['WsdlLocation'] := FolderName;
  IniFile.Save;
  IniFile.Free;
end;

end.

