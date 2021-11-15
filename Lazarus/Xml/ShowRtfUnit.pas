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
unit ShowRtfUnit;

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
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  FormIniFilez;

type
  TShowRtfForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKButton: TButton;
    WrapTextCheckBox: TCheckBox;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure WrapTextCheckBoxClick(Sender: TObject);
  private
    fRtfString: String;
    procedure setRtfString(const Value: String);
    function getEditAllowed: Boolean;
    procedure setEditAllowed(const Value: Boolean);
    function getDoWrapText: Boolean;
    procedure setDoWrapText(const Value: Boolean);
  private
    IniFile: TFormIniFile;
    property DoWrapText: Boolean read getDoWrapText write setDoWrapText;
  public
    property EditAllowed: Boolean read getEditAllowed write setEditAllowed;
    property rtfString: String read fRtfString write setRtfString;
  end;

var
  ShowRtfForm: TShowRtfForm;

implementation

uses Registry;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TShowRtfForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  EditAllowed := False;
  DoWrapText := IniFile.ReadBool ('InfoMemo', 'WrapText', False);
end;

procedure TShowRtfForm.FormDestroy(Sender: TObject);
begin
  IniFile.WriteBool ('InfoMemo', 'WrapText', DoWrapText);
  IniFile.Save;
  IniFile.Free;
end;

procedure TShowRtfForm.OKButtonClick(Sender: TObject);
begin
  if not EditAllowed then
    Memo.Lines.Clear;
end;

procedure TShowRtfForm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = ord ('A'))
  and (Shift = [ssCtrl])
  then
  begin
    Memo.SelectAll;
  end;
{
  if (Key = ord ('M'))
  and (Shift = [ssCtrl])
  then
  begin
    ConsoleForm.ShowXmlStrings (Memo.Lines.Text);
  end;
}
end;

procedure TShowRtfForm.FormShow(Sender: TObject);
begin
  Memo.SetFocus;
end;

procedure TShowRtfForm.WrapTextCheckBoxClick(Sender: TObject);
begin
  if DoWrapText then
    Memo.ScrollBars := ssVertical
  else
    Memo.ScrollBars := ssBoth;
end;

function TShowRtfForm.getDoWrapText: Boolean;
begin
  result := WrapTextCheckBox.Checked;
end;

procedure TShowRtfForm.setDoWrapText(const Value: Boolean);
begin
  WrapTextCheckBox.Checked := Value;
end;

function TShowRtfForm.getEditAllowed: Boolean;
begin
  result := not Memo.ReadOnly;
end;

procedure TShowRtfForm.setEditAllowed(const Value: Boolean);
begin
  Memo.ReadOnly := not Value;
  if Value then
  begin
    Memo.Color := clWindow;
    OKButton.Caption := '&OK';
    OKButton.ModalResult := mrOk;
  end
  else
  begin
    Memo.ParentColor := True;
    OKButton.Caption := '&Close';
    OKButton.ModalResult := mrCancel;
  end;
end;

procedure TShowRtfForm.setRtfString(const Value: String);
var
  S: TMemoryStream;
begin
  fRtfString := Value;
  S := TMemoryStream.Create;
  try
    S.Size := Length (Value);
    S.Position := 0;
    S.WriteBuffer(Value[1], S.Size);
    S.Seek(0, soFromBeginning);
    Memo.Lines.LoadFromStream(S);
  finally
    S.Free;
  end;
end;

end.
