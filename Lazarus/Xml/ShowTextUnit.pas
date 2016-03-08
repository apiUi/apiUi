unit ShowTextUnit;

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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  FormIniFilez;

type

  { TShowTextForm }

  TShowTextForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKButton: TButton;
    Memo: TMemo;
    WrapTextCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown (Sender : TObject ; var Key : Word ;
      Shift : TShiftState );
    procedure OKButtonClick(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure WrapTextCheckBoxClick(Sender: TObject);
  private
    function getEditAllowed: Boolean;
    procedure setEditAllowed(const Value: Boolean);
    function getDoWrapText: Boolean;
    procedure setDoWrapText(const Value: Boolean);
  private
    IniFile: TFormIniFile;
    property DoWrapText: Boolean read getDoWrapText write setDoWrapText;
  public
    property EditAllowed: Boolean read getEditAllowed write setEditAllowed;
  end;

var
  ShowTextForm: TShowTextForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TShowTextForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  EditAllowed := False;
  DoWrapText := IniFile.BooleanByName['WrapText'];
end;

procedure TShowTextForm.FormDestroy(Sender: TObject);
begin
  IniFile.BooleanByName['WrapText']:=DoWrapText;
  IniFile.Save;
  IniFile.Free;
end;

procedure TShowTextForm .FormKeyDown (Sender : TObject ; var Key : Word ;
  Shift : TShiftState );
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TShowTextForm.OKButtonClick(Sender: TObject);
begin
  if not EditAllowed then
    Memo.Lines.Clear;
end;

procedure TShowTextForm.MemoKeyDown(Sender: TObject; var Key: Word;
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

procedure TShowTextForm.FormShow(Sender: TObject);
begin
  Memo.SetFocus;
  Screen.Cursor:=Self.Cursor;
end;

procedure TShowTextForm.WrapTextCheckBoxClick(Sender: TObject);
begin
  if DoWrapText then
    Memo.ScrollBars := ssVertical
  else
    Memo.ScrollBars := ssBoth;
end;

function TShowTextForm.getDoWrapText: Boolean;
begin
  result := WrapTextCheckBox.Checked;
end;

procedure TShowTextForm.setDoWrapText(const Value: Boolean);
begin
  WrapTextCheckBox.Checked := Value;
end;

function TShowTextForm.getEditAllowed: Boolean;
begin
  result := not Memo.ReadOnly;
end;

procedure TShowTextForm.setEditAllowed(const Value: Boolean);
begin
  Memo.ReadOnly := not Value;
  if Value then
  begin
    Memo.Color := clWindow;
    OKButton.Caption := '&OK';
    OKButton.ModalResult := mrOk;
    OKButton.Cancel := False;
  end
  else
  begin
    Memo.ParentColor := True;
    OKButton.Caption := '&Close';
    OKButton.ModalResult := mrCancel;
    OKButton.Cancel := True;
  end;
end;

end.
