unit messagesToDiskUnit;

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
  TmessagesToDiskForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DirectoryEdit: TLabeledEdit;
    ExtentionEdit: TLabeledEdit;
    SeparatorEdit: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    mqHeaderXml: TXml;
  end;

var
  messagesToDiskForm: TmessagesToDiskForm;

implementation


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TmessagesToDiskForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    DirectoryEdit.Text := StringByName['Directory'];
    ExtentionEdit.Text := StringByName['Extention'];
    SeparatorEdit.Text := StringByName['Separator'];
  finally
    Free;
  end;
end;

procedure TmessagesToDiskForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['Directory'] := DirectoryEdit.Text;
    StringByName['Extention'] := ExtentionEdit.Text;
    StringByName['Separator'] := SeparatorEdit.Text;
    Save;
  finally
    Free;
  end;
end;

procedure TmessagesToDiskForm.FormShow(Sender: TObject);
begin
end;

end.

