unit SelectItemUnit;

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
  Buttons, ExtCtrls, FormIniFilez;

type
  TSelectItemForm = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    ListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function GetSelectedItem: String;
  public
    property SelectedItem: String read GetSelectedItem;
  end;

var
  SelectItemForm: TSelectItemForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TSelectItemForm.GetSelectedItem: String;
begin
  if ListBox.ItemIndex > -1 then
    result := ListBox.Items.Strings [ListBox.ItemIndex]
  else
    result := '';
end;

procedure TSelectItemForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TSelectItemForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TSelectItemForm.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
    ModalResult := mrOk;
end;

procedure TSelectItemForm.FormShow(Sender: TObject);
begin
  ListBox.SetFocus;
end;

end.
