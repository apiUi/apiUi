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
unit OpenTwoFoldersUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  ShellAPI, Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, EditBtn, FormIniFilez;

type typeOpenFileMode = (ofmRead, ofmWrite);
type

  { TOpenTwoFoldersForm }

  TOpenTwoFoldersForm = class(TForm)
    FolderName1Edit: TDirectoryEdit;
    FolderName2Edit: TDirectoryEdit;
    FolderName1Label: TLabel;
    FolderName2Label: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure enableOK(Sender: TObject);
    procedure FolderNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function getFolderLabel1: String;
    function getFolderName2Label: String;
    procedure setFolderLabel1(const Value: String);
    procedure setFolderName2Label(const Value: String);
    function getFolderName1: String;
    function getFolderName2Name: String;
    procedure setFolderName1(const Value: String);
    procedure setFolderName2Name(const Value: String);
    { Private declarations }
  public
    LastOpenedFolder1, LastOpenedFolder2: String;
    FileOpenMode1: typeOpenFileMode;
    FileOpenMode2: typeOpenFileMode;
    FileFilter1: String;
    FileFilter2: String;
    FileDefaultExt1: String;
    FileDefaultExt2: String;
    property FolderLabel1: String read getFolderLabel1 write setFolderLabel1;
    property FolderLabel2: String read getFolderName2Label write setFolderName2Label;
    property FolderName1: String read getFolderName1 write setFolderName1;
    property FolderName2: String read getFolderName2Name write setFolderName2Name;
  end;

var
  OpenTwoFoldersForm: TOpenTwoFoldersForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
function StripSlash(const path: string): string;
var
  len: integer;
begin
  result := path;
  len := length(path);
  if (len = 0) or (path[len] <> '\') then exit;
  setlength(result,len-1);
end;


function TOpenTwoFoldersForm.getFolderName1: String;
begin
  result := FolderName1Edit.Text;
end;

function TOpenTwoFoldersForm.getFolderName2Name: String;
begin
  result := FolderName2Edit.Text;
end;

procedure TOpenTwoFoldersForm.setFolderName1(const Value: String);
begin
  FolderName1Edit.Text := Value;
end;

procedure TOpenTwoFoldersForm.setFolderName2Name(const Value: String);
begin
  FolderName2Edit.Text := Value;
end;

procedure TOpenTwoFoldersForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
  enableOK(nil);
end;

procedure TOpenTwoFoldersForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create (Self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TOpenTwoFoldersForm.enableOK(Sender: TObject);
begin
 OKButton.Enabled := (FolderName1 <> '')
                 and (FolderName2 <> '')
                 and (FolderName1 <> FolderName2)
                 ;
end;

procedure TOpenTwoFoldersForm.FolderNameChange(Sender: TObject);
begin
  enableOK(nil);
end;

function TOpenTwoFoldersForm.getFolderLabel1: String;
begin
  result := FolderName1Label.Caption;
end;

function TOpenTwoFoldersForm.getFolderName2Label: String;
begin
  result := FolderName2Label.Caption;
end;

procedure TOpenTwoFoldersForm.setFolderLabel1(const Value: String);
begin
  FolderName1Label.Caption := Value;
end;

procedure TOpenTwoFoldersForm.setFolderName2Label(const Value: String);
begin
  FolderName2Label.Caption := Value;
end;

end.
