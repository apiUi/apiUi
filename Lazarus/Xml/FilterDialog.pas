{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit FilterDialog;

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
  TFilterDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    FindEdit0: TComboBox;
    FindEdit1: TComboBox;
    FindEdit2: TComboBox;
    FindEdit3: TComboBox;
    HasNotCheckBox1: TCheckBox;
    HasNotCheckBox2: TCheckBox;
    HasNotCheckBox3: TCheckBox;
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
  FilterDlg: TFilterDlg;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFilterDlg.FormShow(Sender: TObject);
begin
  FindEdit0.Items.Text := IniFile.StringByName['Find0History'];
  FindEdit1.Items.Text := IniFile.StringByName['Find1History'];
  FindEdit2.Items.Text := IniFile.StringByName['Find2History'];
  FindEdit3.Items.Text := IniFile.StringByName['Find3History'];
  FindEdit0.SelectAll;
  FindEdit0.SetFocus;
end;

procedure TFilterDlg.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
end;

procedure TFilterDlg.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TFilterDlg.OKBtnClick(Sender: TObject);
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
  _saveText (FindEdit0);
  _saveText (FindEdit1);
  _saveText (FindEdit2);
  _saveText (FindEdit3);
  IniFile.StringByName['Find0History'] := FindEdit0.Items.Text;
  IniFile.StringByName['Find1History'] := FindEdit1.Items.Text;
  IniFile.StringByName['Find2History'] := FindEdit2.Items.Text;
  IniFile.StringByName['Find3History'] := FindEdit3.Items.Text;
end;

end.
