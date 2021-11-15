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
unit EditTextUnit;

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
  Buttons, ExtCtrls, Dialogs
  , ParserClasses
  , Express, Bind, ComCtrls
  , Wsdlz
  , Xmlz
  , Menus , ActnList
  , FormIniFilez
  , WsdlProjectz , SynHighlighterAny , SynEdit
  ;

type

  { TEditTexttForm }

  TEditTexttForm = class(TForm)
    CancelButton : TBitBtn ;
    FindAction : TAction ;
    FindNextAction : TAction ;
    CancelAction : TAction ;
    ImageList1 : TImageList ;
    OkAction : TAction ;
    ActionList1 : TActionList ;
    OkButton : TBitBtn ;
    SynAnySyn1 : TSynAnySyn ;
    ScriptEdit : TSynEdit ;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TopPanel: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    StatusBar: TStatusBar;
    procedure FindActionExecute (Sender : TObject );
    procedure FindNextActionExecute (Sender : TObject );
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ScriptEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Finds: String;
    useRegExp: Boolean;
    fScriptChanged: Boolean;
    LastCaption: String;
    procedure doFind (aNext: Boolean);
  end;

var
  EditTexttForm: TEditTexttForm;

implementation

uses
  {$IFnDEF FPC}
    ShellApi,
  {$ELSE}
  {$ENDIF}
  SelectXmlElement
   , SelectItemUnit
   , SelectDbNameUnit
   , FindRegExpDialog
   , xmlUtilz
   , RegExpr
   ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure MemoSetSelectedText (Memo: TCustomMemo; Line: Integer; Column: Integer; Width: Integer);
var
  x: Integer;
  Offset: Integer;
begin
  if not (Memo is TCustomMemo) then
    raise Exception.Create ('First arg is not a TCustomMemo');
  if Line > Memo.Lines.Count then
    raise Exception.Create ('Line out of index for memo');
  Offset := Column - 1;
  x := 0;
  while (x < Line - 1) do
  begin
    Offset := Offset + system.Length (Memo.Lines[x]) + 2;
    Inc (x);
  end;
  Memo.SetFocus;
  Memo.SelStart := Offset;
  Memo.SelLength := Width;
end;

procedure TEditTexttForm.FormShow(Sender: TObject);
begin
  ScriptEdit.ParentColor := ScriptEdit.ReadOnly;
  ScriptEdit.SetFocus;
  fScriptChanged := False;
  Screen.Cursor := crDefault;
end;

procedure TEditTexttForm.OKExecute(Sender: TObject);
begin
  StatusBar.SimpleText := '';
  fScriptChanged := False;
  if (ScriptEdit.Lines.Count = 1)
  and (ScriptEdit.Lines.Strings[0] = '') then
    ScriptEdit.Lines.Delete(0);
end;

procedure TEditTexttForm.CancelExecute(Sender:TObject);
begin
  StatusBar.SimpleText := '';
  ModalResult := mrCancel;
  Close;
end;

procedure TEditTexttForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    Finds := StringByName['FindString'];
    useRegExp := BooleanByName['useRegExp'];
  finally
    Free;
  end;
end;

procedure TEditTexttForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['FindString'] := Finds;
    BooleanByName['useRegExp'] := useRegExp;
    Save;
  finally
    Free;
  end;
end;


procedure TEditTexttForm .doFind (aNext: Boolean) ;
var
  FPos, IPos, FLen: Integer;
begin
  FPos := ScriptEdit.SelStart;
  FLen := Length(FindS);
  IPos := -1;
  if not useRegExp then
  begin
  {
    if frMatchcase in FindAction.Dialog.Options then
       IPos := Pos(FindS, Copy(ScriptEdit.Text,FPos+1,SLen-FPos))
    else
    }
     IPos := Pos(AnsiUpperCase(FindS),AnsiUpperCase( Copy (ScriptEdit.Text, FPos+1, Length (ScriptEdit.Text))));
  end
  else
  begin
    with TRegExpr.Create do
    try
      Expression := Finds;
      if Exec (Copy (ScriptEdit.Text, FPos+1, Length (ScriptEdit.Text))) then
        IPos := MatchPos[0];
    finally
      Free;
    end;
  end;

  if IPos > 0 then
  begin
    FPos := FPos + IPos;
    Self.ActiveControl := ScriptEdit;
    ScriptEdit.SelStart:= FPos;
    ScriptEdit.SelEnd := ScriptEdit.SelStart + FLen;
    FPos:=FPos+FLen-1;
  end
  else
  begin
    ShowMessageFmt('Text ''%s'' was not found', [FindS]);
  end;
end;

procedure TEditTexttForm.ScriptEditChange(Sender: TObject);
begin
  fScriptChanged := True;
end;

procedure TEditTexttForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;

procedure TEditTexttForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canClose := True;
  if fScriptChanged then
    if not (MessageDlg ( 'Discard changes to text?'
                      , mtConfirmation
                      , [mbYes, mbNo, mbCancel]
                      , 0
                      ) = mrYes) then
      CanClose := False;
end;

procedure TEditTexttForm .FindActionExecute (Sender : TObject );
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    FindDlg.Caption := 'Find Text';
    FindDlg.SearchInRadioGroup.Enabled := False;
    FindDlg.ScopeRadioGroup.Enabled := False;
    FindDlg.SearchEdit.Text := Finds;
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      Finds := FindDlg.SearchEdit.Text;
      useRegExp := Finddlg.RegularExpressionCheckBox.Checked;
      doFind(False);
    end;
  finally
    FreeAndNil(FindDlg);
  end;
end;

procedure TEditTexttForm .FindNextActionExecute (Sender : TObject );
begin
  doFind(True);
end;

end.
