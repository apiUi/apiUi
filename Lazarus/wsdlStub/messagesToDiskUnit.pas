unit messagesToDiskUnit;

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
  Buttons, ExtCtrls, SysUtils, FormIniFilez, ComCtrls, Xmlz;

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
  private
    IniFile: TFormIniFile;
  public
    mqHeaderXml: TXml;
  end;

var
  messagesToDiskForm: TmessagesToDiskForm;

implementation

uses ShowXmlUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TmessagesToDiskForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  IniFile := TFormIniFile.Create (Self);
  DirectoryEdit.Text := IniFile.StringByName['Directory'];
  ExtentionEdit.Text := IniFile.StringByName['Extention'];
  SeparatorEdit.Text := IniFile.StringByName['Separator'];
end;

procedure TmessagesToDiskForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['Directory'] := DirectoryEdit.Text;
  IniFile.StringByName['Extention'] := ExtentionEdit.Text;
  IniFile.StringByName['Separator'] := SeparatorEdit.Text;
  IniFile.Free;
end;

procedure TmessagesToDiskForm.FormShow(Sender: TObject);
var
  x, y: Integer;
begin
end;

end.

