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
  private
    IniFile: TFormIniFile;
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
  IniFile := TFormIniFile.Create (Self);
  GetManagerEdit.Text := IniFile.StringByName['GetQueueManager'];
  GetQueueEdit.Text := IniFile.StringByName['GetQueue'];
end;

procedure TmqBrowseForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['GetQueueManager'] := GetManagerEdit.Text;
  IniFile.StringByName['GetQueue'] := GetQueueEdit.Text;
  IniFile.Free;
end;

procedure TmqBrowseForm.EnableEdits;
begin
end;

procedure TmqBrowseForm.FormShow(Sender: TObject);
begin
  EnableEdits;
end;

end.

