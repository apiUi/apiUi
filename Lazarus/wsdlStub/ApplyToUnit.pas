unit ApplyToUnit;

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
  Buttons, ExtCtrls, SysUtils, FormIniFilez;

type

  { TApplyToForm }

  TApplyToForm = class(TForm)
    CancelButton : TBitBtn ;
    OkButton : TBitBtn ;
    RadioGroup: TRadioGroup;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  ApplyToForm: TApplyToForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TApplyToForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    RadioGroup.ItemIndex := IntegerByName['AllAcross'];
  finally
    Free;
  end;
end;

procedure TApplyToForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    IntegerByName['AllAcross'] := RadioGroup.ItemIndex;
    Save;
  finally
    Free;
  end;
end;

end.

