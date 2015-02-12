unit DisclaimerUnit;

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

