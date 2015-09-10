unit messagesFromDiskUnit;

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
  Buttons, ExtCtrls, SysUtils, FormIniFilez, Xmlz;

type
  TmessagesFromDiskForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FileNamesEdit: TLabeledEdit;
    SeparatorEdit: TLabeledEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    mqHeaderXml: TXml;
  end;

var
  messagesFromDiskForm: TmessagesFromDiskForm;

implementation


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TmessagesFromDiskForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    FileNamesEdit.Text := StringByName['FileNames'];
    SeparatorEdit.Text := StringByName['Separator'];
  finally
    Free;
  end;
end;

procedure TmessagesFromDiskForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['FileNames'] := FileNamesEdit.Text;
    StringByName['Separator'] := SeparatorEdit.Text;
    Save;
  finally
    Free;
  end;
end;

procedure TmessagesFromDiskForm.FormShow(Sender: TObject);
begin
end;

end.

