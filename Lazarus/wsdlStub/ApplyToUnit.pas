unit ApplyToUnit;

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
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez;

type
  TApplyToForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup: TRadioGroup;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    IniFile: TFormIniFile;
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
  IniFile := TFormIniFile.Create (Self);
  RadioGroup.ItemIndex := IniFile.IntegerByName['AllAcross'];
end;

procedure TApplyToForm.FormDestroy(Sender: TObject);
begin
  IniFile.IntegerByName['AllAcross'] := RadioGroup.ItemIndex;
  IniFile.Free;
end;

end.

