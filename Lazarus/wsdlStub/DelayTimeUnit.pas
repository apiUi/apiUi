{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelayTimeUnit;

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

  { TDelayTimeForm }

  TDelayTimeForm = class(TForm)
    CancelButton : TBitBtn ;
    DelayRadioGroup: TRadioGroup;
    DelayMinEdit: TLabeledEdit;
    DelayMaxEdit: TLabeledEdit;
    ApplyToRadioGroup: TRadioGroup;
    OkButton : TBitBtn ;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DelayRadioGroupClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function getDelayMsMax: Integer;
    function getDelayMsMin: Integer;
    procedure setDelayMsMax(const Value: Integer);
    procedure setDelayMsMin(const Value: Integer);
  public
    property DelayMsMin: Integer read getDelayMsMin write setDelayMsMin;
    property DelayMsMax: Integer read getDelayMsMax write setDelayMsMax;
    { Public declarations }
  end;

var
  DelayTimeForm: TDelayTimeForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TDelayTimeForm.Button1Click(Sender: TObject);
begin
  if DelayRadioGroup.ItemIndex = 0 then
  begin
    DelayMsMin := 0;
    DelayMsMax := 0;
  end;
  if DelayRadioGroup.ItemIndex = 1 then
  begin
    DelayMsMax := DelayMsMin;
  end;
end;

procedure TDelayTimeForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TDelayTimeForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TDelayTimeForm.FormShow(Sender: TObject);
begin
  if (DelayMsMin = 0)
  and (DelayMsMax = 0) then
    DelayRadioGroup.ItemIndex := 0
  else
    if DelayMsMin = DelayMsMax then
      DelayRadioGroup.ItemIndex := 1
    else
      DelayRadioGroup.ItemIndex := 2;
  DelayRadioGroupClick(nil);
  ApplyToRadioGroup.ItemIndex := 0;
end;

function TDelayTimeForm.getDelayMsMax: Integer;
begin
  result := StrToInt(DelayMaxEdit.Text);
end;

function TDelayTimeForm.getDelayMsMin: Integer;
begin
  result := StrToInt(DelayMinEdit.Text);
end;

procedure TDelayTimeForm.DelayRadioGroupClick(Sender: TObject);
begin
  DelayMinEdit.Visible := (DelayRadioGroup.ItemIndex > 0);
  DelayMaxEdit.Visible := (DelayRadioGroup.ItemIndex > 1);
  case DelayRadioGroup.ItemIndex of
    1: DelayMinEdit.EditLabel.Caption := 'Delay(ms):';
    2: DelayMinEdit.EditLabel.Caption := 'Between(ms):';
  end;
end;

procedure TDelayTimeForm.setDelayMsMax(const Value: Integer);
begin
  DelayMaxEdit.Text := IntToStr(Value);
end;

procedure TDelayTimeForm.setDelayMsMin(const Value: Integer);
begin
  DelayMinEdit.Text := IntToStr(Value);
end;

end.

