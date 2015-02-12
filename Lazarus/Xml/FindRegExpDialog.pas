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
  TFindDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    SearchEdit: TEdit;
    SearchInRadioGroup: TRadioGroup;
    ScopeRadioGroup: TRadioGroup;
    RegularExpressionCheckBox: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TFindDlg.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create(self);
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
