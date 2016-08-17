unit PromptFolderUnit;

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
  Buttons, ExtCtrls, EditBtn;

type

  { TPromptFolderForm }

  TPromptFolderForm = class(TForm)
    CancelButton : TBitBtn ;
    PromptEdit: TDirectoryEdit;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Numeric: Boolean;
    ReadOnly: Boolean;
    Pattern: String;
  end;

var
  PromptFolderForm: TPromptFolderForm;

implementation

uses RegExpr;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TPromptFolderForm.FormShow(Sender: TObject);
begin
  PromptEdit.ReadOnly := ReadOnly;
  if ReadOnly then
    PromptEdit.ParentColor := True
  else
  begin
    PromptEdit.Color := clWindow;
    PromptEdit.SetFocus;
    PromptEdit.SelectAll;
  end;
end;

end.
