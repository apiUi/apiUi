unit messagesFromDiskUnit;

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
  TmessagesFromDiskForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FileNamesEdit: TLabeledEdit;
    SeparatorEdit: TLabeledEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    mqHeaderXml: TXml;
  end;

var
  messagesFromDiskForm: TmessagesFromDiskForm;

implementation

uses ShowXmlUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TmessagesFromDiskForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  IniFile := TFormIniFile.Create (Self);
  FileNamesEdit.Text := IniFile.StringByName['FileNames'];
  SeparatorEdit.Text := IniFile.StringByName['Separator'];
end;

procedure TmessagesFromDiskForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['FileNames'] := FileNamesEdit.Text;
  IniFile.StringByName['Separator'] := SeparatorEdit.Text;
  IniFile.Free;
end;

procedure TmessagesFromDiskForm.FormShow(Sender: TObject);
var
  x, y: Integer;
begin
end;

end.

