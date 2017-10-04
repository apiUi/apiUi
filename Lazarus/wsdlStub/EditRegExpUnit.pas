unit EditRegExpUnit;

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
  Classes, Graphics, Forms, Controls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez, RegExpr;

type

  { TEditRegExpForm }

  TEditRegExpForm = class(TForm)
    CancelButton : TBitBtn ;
    SampleValueEdit: TLabeledEdit;
    RegularExpressionEdit: TLabeledEdit;
    OkButton : TBitBtn ;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure RegularExpressionEditChange(Sender: TObject);
    procedure SampleValueEditChange(Sender: TObject);
  private
    procedure CheckRegExp;
  public
    { Public declarations }
  end;

var
  EditRegExpForm: TEditRegExpForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TEditRegExpForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TEditRegExpForm.FormShow(Sender: TObject);
begin
  CheckRegExp;
end;

procedure TEditRegExpForm.OkButtonClick(Sender: TObject);
begin

end;

procedure TEditRegExpForm.RegularExpressionEditChange(Sender: TObject);
begin
  CheckRegExp;
end;

procedure TEditRegExpForm.SampleValueEditChange(Sender: TObject);
begin
  CheckRegExp;
end;

procedure TEditRegExpForm.CheckRegExp;
begin
  if RegularExpressionEdit.Text = '' then
  begin
    OkButton.Enabled := True;
    Exit;
  end;
  with TRegExpr.Create do
  try
    try
      Expression := '^(' + RegularExpressionEdit.Text + ')$';  // bol and eol: must match entire string
      OkButton.Enabled := Exec(SampleValueEdit.Text);
    except
      OkButton.Enabled := False;
    end;
  finally
    Free;
  end;
end;

procedure TEditRegExpForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

end.

