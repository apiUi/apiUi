unit EditListValuesUnit;

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
  Buttons, ExtCtrls, Dialogs
  , Menus
  , FormIniFilez, Grids, ValEdit, ComCtrls
  ;

type

  { TEditListValuesForm }

  TEditListValuesForm = class(TForm)
    CancelBtn : TBitBtn ;
    OKBtn : TBitBtn ;
    Panel3: TPanel;
    Panel2: TPanel;
    StatusBar: TStatusBar;
    MemoPopUpMenu: TPopupMenu;
    IpmFieldMenuItem: TMenuItem;
    SelectFunctionMenuItem: TMenuItem;
    N1: TMenuItem;
    Grammar1: TMenuItem;
    N2: TMenuItem;
    DbNameMenuItem: TMenuItem;
    EmbeddedSQLMenuItem: TMenuItem;
    ValueListEditor: TValueListEditor;
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure MemoPopUpMenuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fisReadOnly: Boolean;
    procedure setisReadOnly(const Value: Boolean);
  public
    property isReadOnly: Boolean read fisReadOnly write setisReadOnly;
  end;

var
  EditListValuesForm: TEditListValuesForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TEditListValuesForm.FormShow(Sender: TObject);
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
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    ValueListEditor.DefaultColWidth := IntegerByNameDef['KeyColumnWidth', ValueListEditor.DefaultColWidth];
  finally
    Free;
  end;
end;

procedure TEditListValuesForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
    IntegerByName['KeyColumnWidth'] := ValueListEditor.ColWidths[0];
  finally
    Free;
  end;
end;

end.
