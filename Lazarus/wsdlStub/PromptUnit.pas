unit PromptUnit;

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
  TPromptForm = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    PromptEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure PromptEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    Numeric: Boolean;
    ReadOnly: Boolean;
  end;

var
  PromptForm: TPromptForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TPromptForm.FormShow(Sender: TObject);
begin
  PromptEdit.ReadOnly := ReadOnly;
  if ReadOnly then
    PromptEdit.ParentColor := True
  else
  begin
    PromptEdit.Color := clWindow;
    PromptEdit.OnChange (self);
    PromptEdit.SetFocus;
    PromptEdit.SelectAll;
  end;
end;

procedure TPromptForm.PromptEditChange(Sender: TObject);
var
  xFloat: Extended;
  DoEnable: Boolean;
begin
  DoEnable := True; // start optimistic
  if (Numeric)
  and (PromptEdit.Text <> '')
  then begin
    try
      xFloat := StrToFloat (PromptEdit.Text);
    except
      DoEnable := False;
    end;
  end;
  OkButton.Enabled := DoEnable;
end;

end.
