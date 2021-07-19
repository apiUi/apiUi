{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit DisclaimerUnit;

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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TDisclaimerForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    OKButton: TButton;
    Panel4: TPanel;
    Memo1: TMemo;
    DisclaimerAcceptedCheckBox: TCheckBox;
    Panel5: TPanel;
    ProgramIcon: TImage;
    procedure DisclaimerAcceptedCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DisclaimerForm: TDisclaimerForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TDisclaimerForm.DisclaimerAcceptedCheckBoxClick(Sender: TObject);
begin
  OKButton.Enabled := DisclaimerAcceptedCheckBox.Checked;
end;

end.

