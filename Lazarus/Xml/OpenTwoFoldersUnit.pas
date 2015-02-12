unit OpenTwoFoldersUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  ShellAPI, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FormIniFilez, ShlObj;

type typeOpenFileMode = (ofmRead, ofmWrite);
type
  TOpenTwoFoldersForm = class(TForm)
    FolderName1Edit: TLabeledEdit;
    FolderName2Edit: TLabeledEdit;
    BrowseFolder1Button: TButton;
    BrowseFolder2Button: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure enableOK(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BrowseFolder2ButtonClick(Sender: TObject);
    procedure BrowseFolder1ButtonClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getFolderLabel1: String;
    function getFolderName2Label: String;
    procedure setFolderLabel1(const Value: String);
    procedure setFolderName2Label(const Value: String);
    function getFolderName1: String;
    function getFolderName2Name: String;
    procedure setFolderName1(const Value: String);
    procedure setFolderName2Name(const Value: String);
    { Private declarations }
  public
    LastOpenedFolder1, LastOpenedFolder2: String;
    FileOpenMode1: typeOpenFileMode;
    FileOpenMode2: typeOpenFileMode;
    FileFilter1: String;
    FileFilter2: String;
    FileDefaultExt1: String;
    FileDefaultExt2: String;
    property FolderLabel1: String read getFolderLabel1 write setFolderLabel1;
    property FolderLabel2: String read getFolderName2Label write setFolderName2Label;
    property FolderName1: String read getFolderName1 write setFolderName1;
    property FolderName2: String read getFolderName2Name write setFolderName2Name;
  end;

var
  OpenTwoFoldersForm: TOpenTwoFoldersForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
function StripSlash(const path: string): string;
var
  len: integer;
begin
  result := path;
  len := length(path);
  if (len = 0) or (path[len] <> '\') then exit;
  setlength(result,len-1);
end;


procedure TOpenTwoFoldersForm.BrowseFolder1ButtonClick(Sender: TObject);
var
  s: String;
begin
  {$IFnDEF FPC}
  s := FolderName1Edit.Text;
  if not GetFolder(self,'Open Folder 1',s) then exit;
  FolderName1Edit.Text := s;
  {$endif}
end;

procedure TOpenTwoFoldersForm.BrowseFolder2ButtonClick(Sender: TObject);
var
  s: String;
begin
  {$IFnDEF FPC}
  s := FolderName2Edit.Text;
  if not GetFolder(self,'Open Folder 2',s) then exit;
  FolderName2Edit.Text := s;
  {$endif}
end;

function TOpenTwoFoldersForm.getFolderName1: String;
begin
  result := FolderName1Edit.Text;
end;

function TOpenTwoFoldersForm.getFolderName2Name: String;
begin
  result := FolderName2Edit.Text;
end;

procedure TOpenTwoFoldersForm.setFolderName1(const Value: String);
begin
  FolderName1Edit.Text := Value;
end;

procedure TOpenTwoFoldersForm.setFolderName2Name(const Value: String);
begin
  FolderName2Edit.Text := Value;
end;

procedure TOpenTwoFoldersForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  FolderName1 := IniFile.StringByName['Folder1'];
  FolderName2 := IniFile.StringByName['Folder2'];
  enableOK(nil);
end;

procedure TOpenTwoFoldersForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['Folder1'] := FolderName1;
  IniFile.StringByName['Folder2'] := FolderName2;
  IniFile.Save;
  IniFile.Free;
end;

procedure TOpenTwoFoldersForm.enableOK(Sender: TObject);
begin
 OKButton.Enabled := (FolderName1 <> '')
                 and (FolderName2 <> '')
                 and (FolderName1 <> FolderName2)
                 ;
end;

function TOpenTwoFoldersForm.getFolderLabel1: String;
begin
  result := FolderName1Edit.EditLabel.Caption;
end;

function TOpenTwoFoldersForm.getFolderName2Label: String;
begin
  result := FolderName2Edit.EditLabel.Caption;
end;

procedure TOpenTwoFoldersForm.setFolderLabel1(const Value: String);
begin
  FolderName1Edit.EditLabel.Caption := Value;
end;

procedure TOpenTwoFoldersForm.setFolderName2Label(const Value: String);
begin
  FolderName2Edit.EditLabel.Caption := Value;
end;

end.
