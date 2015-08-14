unit DbFilterDialog;

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
  TDbFilterDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    FileSearchButton: TButton;
    QueryEdit: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ParamEdit1: TComboBox;
    ParamEdit2: TComboBox;
    ParamEdit3: TComboBox;
    ParamEdit4: TComboBox;
    ConnStringEdit: TComboBox;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileSearchButtonClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    { Public declarations }
  end;

var
  DbFilterDlg: TDbFilterDlg;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TDbFilterDlg.FormShow(Sender: TObject);
begin
  ConnStringEdit.Items.Text := IniFile.StringByName['ConnStringHistory'];
  ParamEdit1.Items.Text := IniFile.StringByName['Param1History'];
  ParamEdit2.Items.Text := IniFile.StringByName['Param2History'];
  ParamEdit3.Items.Text := IniFile.StringByName['Param3History'];
  ParamEdit4.Items.Text := IniFile.StringByName['Param4History'];
  ParamEdit1.SelectAll;
  ParamEdit1.SetFocus;
end;

procedure TDbFilterDlg.FileSearchButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := ConnStringEdit.Text;
    DefaultExt := 'ConnStrng';
    Filter := 'ConnStrng file (*.ConnStrng)|*.ConnStrng';
    Title := 'Open ConnectionString File';
    Options := Options + [ofFileMustExist];
    if Execute then
    begin
      ConnStringEdit.Text := FileName;
    end;
  finally
    Free;
  end;
end;

procedure TDbFilterDlg.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  ConnStringEdit.Text := IniFile.StringByName['ConnParamsFilename'];
  QueryEdit.Lines.Text := IniFile.StringByNameDef['QueryText', QueryEdit.Lines.Text];
end;

procedure TDbFilterDlg.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['ConnParamsFilename'] := ConnStringEdit.Text;
  IniFile.StringByName['QueryText'] := QueryEdit.Lines.Text;
  IniFile.Save;
  IniFile.Free;
end;

procedure TDbFilterDlg.OKBtnClick(Sender: TObject);
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
  _saveText (ConnStringEdit);
  _saveText (ParamEdit1);
  _saveText (ParamEdit2);
  _saveText (ParamEdit3);
  _saveText (ParamEdit4);
  IniFile.StringByName['ConnStringHistory'] := ConnStringEdit.Items.Text;
  IniFile.StringByName['Param1History'] := ParamEdit1.Items.Text;
  IniFile.StringByName['Param2History'] := ParamEdit2.Items.Text;
  IniFile.StringByName['Param3History'] := ParamEdit3.Items.Text;
  IniFile.StringByName['Param4History'] := ParamEdit4.Items.Text;
end;

end.
