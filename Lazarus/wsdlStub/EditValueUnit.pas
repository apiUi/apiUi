unit EditValueUnit;

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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs
  , FormIniFilez
  , StdCtrls
  , Ipmz
  ;

type
  TEditValueForm = class(TForm)
    Label1: TLabel;
    KeyEdit: TEdit;
    Label2: TLabel;
    OkButton: TButton;
    Button2: TButton;
    Label3: TLabel;
    PictureEdit: TEdit;
    ComboEdit: TComboBox;
    ValueEdit: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ComboEditChange(Sender: TObject);
  public
    Ipm: TIpmItem;
    ReadOnly: Boolean;
  end;

var
  EditValueForm: TEditValueForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TEditValueForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TEditValueForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TEditValueForm.FormShow(Sender: TObject);
begin
  KeyEdit.Text := Ipm.FullCaption;
  PictureEdit.Text := Ipm.PictureCaption;
  if Ipm.Level88Values.Text <> '' then
  begin
    ComboEdit.Visible := True;
    ValueEdit.Visible := False;
    ComboEdit.Text := Ipm.Value;
    ComboEdit.MaxLength := Ipm.InputLength;
    ComboEdit.Enabled := not ReadOnly;
    ComboEdit.Items.Text := Ipm.Level88Values.Text;
    if ReadOnly = True then
      ComboEdit.Color := clBtnFace
    else
      ComboEdit.Color := clWhite;
    ComboEdit.SelectAll;
    ComboEdit.SetFocus;
  end
  else
  begin
    ComboEdit.Visible := False;
    ValueEdit.Visible := True;
    ValueEdit.Text := Ipm.Value;
    ValueEdit.MaxLength := Ipm.InputLength;
    ValueEdit.ReadOnly := ReadOnly;
    if ReadOnly = True then
      ValueEdit.Color := clBtnFace
    else
      ValueEdit.Color := clWhite;
    ValueEdit.SelectAll;
    ValueEdit.SetFocus;
  end;
end;

procedure TEditValueForm.FormResize(Sender: TObject);
begin
  KeyEdit.Width := Width - 2 * KeyEdit.Left - 15;
  PictureEdit.Width := Width - 2 * PictureEdit.Left - 15;
  ValueEdit.Width := Width - 2 * ValueEdit.Left - 15;
  ComboEdit.Width := Width - 2 * ComboEdit.Left - 15;
end;

procedure TEditValueForm.OkButtonClick(Sender: TObject);
var
  xValue: String;
begin
  if Ipm.Level88Values.Text <> '' then
    xValue := ComboEdit.Text
  else
    xValue := ValueEdit.Text;
  try
    Ipm.ValueToBuffer(xValue);
    Ipm.Value := xValue;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TEditValueForm.ValueEditChange(Sender: TObject);
var
  DoEnable: Boolean;
begin
  DoEnable := True; // start optimistic
  if (Ipm.Numeric)
  and (ValueEdit.Text <> '')
  then begin
    try
      StrToFloat (ValueEdit.Text);
    except
      DoEnable := False;
    end;
  end;
  OkButton.Enabled := DoEnable;
end;

procedure TEditValueForm.ComboEditChange(Sender: TObject);
var
  DoEnable: Boolean;
begin
  DoEnable := True; // start optimistic
  if (Ipm.Numeric)
  and (ComboEdit.Text <> '')
  then begin
    try
      StrToFloat (ComboEdit.Text);
    except
      DoEnable := False;
    end;
  end;
  OkButton.Enabled := DoEnable;
end;

end.
