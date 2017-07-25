unit TestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, VirtualTrees, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, RegExpr, FormIniFilez;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    TstButton: TButton;
    FirstButton: TButton;
    Memo: TSynMemo;
    NextButton: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    rxEdit: TLabeledEdit;
    procedure Button1Click(Sender: TObject);
    procedure FirstButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure TstButtonClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    rx: TRegExpr;
    procedure Exec(aFirst: Boolean);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FirstButtonClick(Sender: TObject);
begin
  Exec(True);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  x: Integer;
  s: String;
begin
  s := IntToStr (rx.SubExprMatchCount) + LineEnding;
  for x := 0 to rx.SubExprMatchCount do
    s := s + IntToStr(x) + ': ' + rx.Match[x] + LineEnding;
  ShowMessage(s);
end;

procedure TMainForm.NextButtonClick(Sender: TObject);
begin
  Exec(False);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  rx := TRegExpr.Create;
  IniFile := TFormIniFile.Create(Self, True);
  IniFile.Restore;
  rxEdit.Text := IniFile.StringByName['RegularExpression'];
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['RegularExpression'] := rxEdit.Text;
  rx.Free;
  IniFile.Save;
  IniFile.Free;
end;

procedure TMainForm.DataChange(Sender: TObject);
begin
  NextButton.Enabled:=False;
end;

procedure TMainForm.TstButtonClick(Sender: TObject);
begin
//  Memo.Text:=IniFile.EncryptPassword(Memo.Text);
end;

procedure TMainForm.Exec(aFirst: Boolean);
var
  found: Boolean;
begin
  if aFirst then
  begin
    rx.Expression:=rxEdit.Text;
    found := rx.Exec (Memo.Text);
  end
  else
    found := rx.ExecNext;
  if found then
  begin
    memo.SelStart:=rx.MatchPos[0];
    memo.SelEnd:=memo.SelStart + rx.MatchLen[0];
    memo.SetFocus;
  end
  else
  begin
    memo.SelStart := 0;
    memo.SelEnd := 0;
    memo.SetFocus;
  end;
  NextButton.Enabled:=found;
end;

end.

