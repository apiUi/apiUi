unit mqBrowseUnit;

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
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez;

type
  TmqBrowseForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GetManagerEdit: TLabeledEdit;
    Label1: TLabel;
    GetQueueEdit: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure EnableEdits;
  end;

var
  mqBrowseForm: TmqBrowseForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TmqBrowseForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    GetManagerEdit.Text := StringByName['GetQueueManager'];
    GetQueueEdit.Text := StringByName['GetQueue'];
  finally
    Free;
  end;
end;

procedure TmqBrowseForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['GetQueueManager'] := GetManagerEdit.Text;
    StringByName['GetQueue'] := GetQueueEdit.Text;
    Save;
  finally
    Free;
  end;
end;

procedure TmqBrowseForm.EnableEdits;
begin
end;

procedure TmqBrowseForm.FormShow(Sender: TObject);
begin
  EnableEdits;
end;

end.

