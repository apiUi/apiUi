unit EditPartExpUnit;

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
  Buttons, ExtCtrls, SysUtils, FormIniFilez, RegExpr, Dialogs;

type

  { TEditPartExpForm }

  TEditPartExpForm = class(TForm)
    CancelButton : TBitBtn ;
    SampleValueAEdit: TLabeledEdit;
    MaskExpressionEdit: TLabeledEdit;
    OkButton : TBitBtn ;
    SampleValueBEdit: TLabeledEdit;
    MaskedValueAEdit: TLabeledEdit;
    MaskedValueBEdit: TLabeledEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaskExpressionEditKeyPress(Sender: TObject; var Key: char);
    procedure MaskExpressionEditChange(Sender: TObject);
    procedure SampleValueAEditChange(Sender: TObject);
  private
    procedure CheckExpression;
  public
    { Public declarations }
  end;

var
  EditPartExpForm: TEditPartExpForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TEditPartExpForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TEditPartExpForm.FormShow(Sender: TObject);
begin
  CheckExpression;
  MaskExpressionEdit.SetFocus;
end;

procedure TEditPartExpForm.MaskExpressionEditKeyPress(Sender: TObject;
  var Key: char);
begin
  if not (Key in ['_', '?', #8, #9]) then
  begin
    Key := #0;
    if Ord (Key) <> VK_ESCAPE then
      ShowMessage('''_'' and ''?'' are the only acceptable characters');
  end
end;

procedure TEditPartExpForm.MaskExpressionEditChange(Sender: TObject);
begin
  CheckExpression;
end;

procedure TEditPartExpForm.SampleValueAEditChange(Sender: TObject);
begin
  CheckExpression;
end;

procedure TEditPartExpForm.CheckExpression;
var
  a, b, m, xa, xb: String;
  x: Integer;
begin
  a := SampleValueAEdit.Text;
  b := SampleValueBEdit.Text;
  m := MaskExpressionEdit.Text;

  SetLength(xa, Length(a));
  x := 1;
  while (x <= Length (a))
    and (x <= Length (m)) do
  begin
    if m[x] = '?' then
      xa[x] := m[x]
    else
      xa[x] := a[x];
    Inc (x);
  end;
  while (x <= Length (a)) do
  begin
    xa[x] := a[x];
    Inc (x);
  end;

  SetLength(xb, Length(b));
  x := 1;
  while (x <= Length (b))
    and (x <= Length (m)) do
  begin
    if m[x] = '?' then
      xb[x] := m[x]
    else
      xb[x] := b[x];
    Inc (x);
  end;
  while (x <= Length (b)) do
  begin
    xb[x] := b[x];
    Inc (x);
  end;

  MaskedValueAEdit.Text := xa;
  MaskedValueBEdit.Text := xb;

  OkButton.Enabled := (xa = xb);

end;

procedure TEditPartExpForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

end.

