unit EditListValuesUnit;

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
  Buttons, ExtCtrls, Dialogs
  , Menus
  , FormIniFilez, ToolWin, Grids, ValEdit, ComCtrls
  ;

type
  TEditListValuesForm = class(TForm)
    Panel3: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    StatusBar: TStatusBar;
    MemoPopUpMenu: TPopupMenu;
    IpmFieldMenuItem: TMenuItem;
    SelectFunctionMenuItem: TMenuItem;
    N1: TMenuItem;
    Grammar1: TMenuItem;
    N2: TMenuItem;
    DbNameMenuItem: TMenuItem;
    EmbeddedSQLMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    ValueListEditor: TValueListEditor;
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure MemoPopUpMenuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fisReadOnly: Boolean;
    procedure setisReadOnly(const Value: Boolean);
  public
    property isReadOnly: Boolean read fisReadOnly write setisReadOnly;
  end;

var
  EditListValuesForm: TEditListValuesForm;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  SelectXmlElement, SelectItemUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure MemoSetSelectedText (Memo: TCustomMemo; Line: Integer; Column: Integer; Width: Integer);
var
  x: Integer;
  Offset: Integer;
begin
  if not (Memo is TCustomMemo) then
    raise Exception.Create ('First arg is not a TCustomMemo');
  if Line > Memo.Lines.Count then
    raise Exception.Create ('Line out of index for memo');
  Offset := Column - 1;
  x := 0;
  while (x < Line - 1) do
  begin
    Offset := Offset + system.Length (Memo.Lines[x]) + 2;
    Inc (x);
  end;
  Memo.SetFocus;
  Memo.SelStart := Offset;
  Memo.SelLength := Width;
end;

procedure TEditListValuesForm.FormShow(Sender: TObject);
var
  x: Integer;
begin
  ValueListEditor.ParentColor := isReadOnly;
  ValueListEditor.SetFocus;
end;

procedure TEditListValuesForm.CancelBtnClick(Sender:TObject);
begin
  StatusBar.SimpleText := '';
end;

procedure TEditListValuesForm.MemoPopUpMenuPopup(Sender: TObject);
begin
{
  IpmFieldMenuItem.Enabled := (    (IpmdescrType = ipmDTCobol)
                               and (   ((Assigned (InIpmDescrs)) and (InIpmDescrs.Count > 0))
                                    or ((Assigned (OutIpmDescrs)) and (OutIpmDescrs.Count > 0))
                                    or ((Assigned (ReplyIpmDescrs)) and (ReplyIpmDescrs.Count > 0))
                                    or ((Assigned (RequestIpmDescrs)) and (RequestIpmDescrs.Count > 0))
                                   )
                              )
                            or (    (IpmDescrType = ipmDTXsd)
                                and (   (Assigned (inXsdXml) and (inXsdXml.Items.Count > 0))
                                     or (Assigned (outXsdXml) and (outXsdXml.Items.Count > 0))
                                    )
                              )
                            or (    (IpmDescrType = ipmDTWsdl)
                                and (   (Assigned (inWsdlXml) and (inWsdlXml.Items.Count > 0))
                                     or (Assigned (outWsdlXml) and (outWsdlXml.Items.Count > 0))
                                    )
                               );
  DbNameMenuItem.Enabled := YagButton.DataBase.Connected;
}
end;

procedure TEditListValuesForm.setisReadOnly(const Value: Boolean);
begin
  fisReadOnly := Value;
  OKBtn.Visible := not Value;
  if Value = True then
  begin
    ValueListEditor.Options := ValueListEditor.Options - [goEditing];
    CancelBtn.Caption := '&Close';
  end
  else
  begin
    ValueListEditor.Options := ValueListEditor.Options + [goEditing];
    CancelBtn.Caption := '&Cancel';
  end;
end;

procedure TEditListValuesForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TEditListValuesForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

end.
