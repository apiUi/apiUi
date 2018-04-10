unit SelectProjectFolderUnit;

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

  { TSelectProjectFolderForm }

  TSelectProjectFolderForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel3: TPanel;
    Label8: TLabel;
    ProjectFolderNameEdit: TEdit;
    Button1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getFolderName : String ;
    procedure setFolderName (AValue : String );
  public
    property FolderName: String read getFolderName write setFolderName;
  end;

var
  SelectProjectFolderForm: TSelectProjectFolderForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses StrUtils;

procedure TSelectProjectFolderForm.Button1Click(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
  try
    Caption := self.Caption;
    FileName := ProjectFolderNameEdit.Text;
    if Execute then
      ProjectFolderNameEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TSelectProjectFolderForm.OKBtnClick(Sender: TObject);
begin

end;

function TSelectProjectFolderForm .getFolderName : String ;
begin
  result := ProjectFolderNameEdit.Text;
end;

procedure TSelectProjectFolderForm .setFolderName (AValue : String );
begin
  ProjectFolderNameEdit.Text := AValue;
end;

procedure TSelectProjectFolderForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  FolderName := IniFile.StringByName['WsdlLocation'];
end;

procedure TSelectProjectFolderForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['WsdlLocation'] := FolderName;
  IniFile.Save;
  IniFile.Free;
end;

end.

