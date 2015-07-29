unit CommandDialog;

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
  Buttons, ExtCtrls, FormIniFilez, Dialogs;

type
  TCommandDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    FileNameEdit: TComboBox;
    ArgsEdit: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    OpenDialog: TOpenDialog;
    WaitCheckBox: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    { Public declarations }
  end;

var
  CommandDlg: TCommandDlg;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TCommandDlg.FormShow(Sender: TObject);
begin
  FileNameEdit.Items.Text := IniFile.StringByName['FileNameHistory'];
  ArgsEdit.Items.Text := IniFile.StringByName['ArgsHistory'];
  FileNameEdit.SelectAll;
  FileNameEdit.SetFocus;
end;

procedure TCommandDlg.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TCommandDlg.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TCommandDlg.OKBtnClick(Sender: TObject);
  procedure _saveText (aCB: TComboBox);
  var
    x: Integer;
    n: Boolean;
  begin
    n := True;
    for x := 0 to aCB.Items.Count - 1 do
      if aCB.Items.Strings[x] = aCB.Text then
        n := False;
    if n then
    begin
      aCB.Items.Insert(0, aCB.Text);
      if aCB.Items.Count > 10 then
        aCB.Items.Delete(aCB.Items.Count - 1);
    end;
  end;
begin
  _saveText (FileNameEdit);
  _saveText (ArgsEdit);
  IniFile.StringByName['FileNameHistory'] := FileNameEdit.Items.Text;
  IniFile.StringByName['ArgsHistory'] := ArgsEdit.Items.Text;
end;

procedure TCommandDlg.Button1Click(Sender: TObject);
begin
  OpenDialog.FileName := FileNameEdit.Text;
  if OpenDialog.Execute then
    FileNameEdit.Text := OpenDialog.FileName;
end;

end.
