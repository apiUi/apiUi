unit OpenWsdlUnit;

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
  Buttons, ExtCtrls, Dialogs, FormIniFilez;

type

  { TOpenWsdlForm }

  TOpenWsdlForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel3: TPanel;
    Label8: TLabel;
    WsdlLocationEdit: TEdit;
    Button1: TButton;
    OpenFileDialog: TOpenDialog;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getWsdlLocation : String ;
    procedure setWsdlLocation (AValue : String );
  public
    property WsdlLocation: String read getWsdlLocation write setWsdlLocation;
  end;

var
  OpenWsdlForm: TOpenWsdlForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses StrUtils;

procedure TOpenWsdlForm.Button1Click(Sender: TObject);
begin
  if not AnsiStartsText ('http://', WsdlLocationEdit.Text) then
    OpenFileDialog.FileName := WsdlLocationEdit.Text;
  if OpenFileDialog.Execute = True then
    WsdlLocationEdit.Text := OpenFileDialog.FileName;
end;

function TOpenWsdlForm .getWsdlLocation : String ;
begin
  result := WsdlLocationEdit.Text;
end;

procedure TOpenWsdlForm .setWsdlLocation (AValue : String );
begin
  WsdlLocationEdit.Text := AValue;
end;

procedure TOpenWsdlForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  WsdlLocation := IniFile.StringByName['WsdlLocation'];
end;

procedure TOpenWsdlForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['WsdlLocation'] := WsdlLocation;
  IniFile.Save;
  IniFile.Free;
end;

end.

