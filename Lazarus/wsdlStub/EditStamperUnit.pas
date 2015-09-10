unit EditStamperUnit;

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
  , ParserClasses
  , Express, Bind, ComCtrls
  , Wsdlz
  , Xmlz
  , Menus
  , FormIniFilez
  ;

type
  TEditStamperForm = class(TForm)
    TopPanel: TPanel;
    Label1: TLabel;
    ElementNameEdit: TEdit;
    Panel3: TPanel;
    ScriptMemo: TMemo;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    CheckButton: TButton;
    StatusBar: TStatusBar;
    MemoPopUpMenu: TPopupMenu;
    IpmFieldMenuItem: TMenuItem;
    SelectFunctionMenuItem: TMenuItem;
    N1: TMenuItem;
    Grammar1: TMenuItem;
    N2: TMenuItem;
    DbNameMenuItem: TMenuItem;
    EmbeddedSQLMenuItem: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ScriptMemoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CheckButtonClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure IpmFieldMenuItemClick(Sender: TObject);
    procedure MemoPopUpMenuPopup(Sender: TObject);
    procedure SelectFunctionMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Grammar1Click(Sender: TObject);
    procedure TopPanelResize(Sender: TObject);
    procedure EmbeddedSQLMenuItemClick(Sender: TObject);
  private
    fScriptChanged: Boolean;
    LastCaption: String;
    fWsdlOperation: TWsdlOperation;
    fBind: TCustomBindable;
    procedure setBind(const Value: TCustomBindable);
    procedure setWsdlOperation(const Value: TWsdlOperation);
    function getElementName: String;
    procedure setElementName(const Value: String);
    procedure ExpressError( Sender: TObject
                          ; LineNumber, ColumnNumber, Offset: Integer
                          ; TokenString, Data: String
                          );
  public
    property WsdlOperation: TWsdlOperation read fWsdlOperation write setWsdlOperation;
    property Bindable: TCustomBindable read fBind write setBind;
  end;

var
  EditStamperForm: TEditStamperForm;

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

procedure TEditStamperForm.FormShow(Sender: TObject);
begin
  ScriptMemo.ParentColor := ScriptMemo.ReadOnly;
  ScriptMemo.SetFocus;
  fScriptChanged := False;
end;

procedure TEditStamperForm.OKBtnClick(Sender: TObject);
begin
  StatusBar.SimpleText := '';
  Bindable.Value := ':=' + ScriptMemo.Text;
  fScriptChanged := False;
  WsdlOperation.PrepareRpyStamper (Bindable);
end;

procedure TEditStamperForm.ExpressError(Sender: TObject; LineNumber,
  ColumnNumber, Offset: Integer; TokenString, Data: String);
begin
  MemoSetSelectedText ( ScriptMemo
                      , LineNumber
                      , ColumnNumber - system.Length (ElementNameEdit.Text) + 2
                      , system.Length (TokenString)
                      );
  StatusBar.SimpleText := Data + ': ' + TokenString;
end;

procedure TEditStamperForm.CheckButtonClick(Sender: TObject);
var
  SwapOnError: TOnErrorEvent;
  swapScriptLines: String;
begin
  StatusBar.SimpleText := '';
  SwapOnError := WsdlOperation.OnError;
  try
    WsdlOperation.OnError := ExpressError;
    begin
      SwapScriptLines := Bindable.Value;
      Bindable.Value := ':=' + ScriptMemo.Lines.Text;
      try
        WsdlOperation.PrepareRpyStamper (Bindable);
      finally
        Bindable.Value := SwapScriptLines;
      end;
    end;
  finally
    WsdlOperation.OnError := SwapOnError;
  end;
end;

procedure TEditStamperForm.CancelBtnClick(Sender:TObject);
begin
  StatusBar.SimpleText := '';
end;

procedure TEditStamperForm.IpmFieldMenuItemClick(Sender: TObject);
begin
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  try
    SelectXmlElementForm.doShowReq := True;
    SelectXmlElementForm.doShowRpy := True;
    SelectXmlElementForm.WsdlOperation := WsdlOperation;
    SelectXmlElementForm.LastCaption := LastCaption;
    SelectXmlElementForm.IncludeRecurring := True;
    SelectXmlElementForm.ShowModal;
    if SelectXmlElementForm.ModalResult = mrOk then
    begin
      LastCaption := SelectXmlElementForm.SelectedCaption;
      ScriptMemo.SelText := LastCaption;
    end;
  finally
    FreeAndNil (SelectXmlElementForm);
  end;
end;

procedure TEditStamperForm.SelectFunctionMenuItemClick(Sender: TObject);
var
  x: Integer;
begin
  Application.CreateForm(TSelectItemForm, SelectItemForm);
  try
    SelectItemForm.Caption := 'Select function';
    SelectItemForm.ListBox.Clear;
    for x := 0 to WsdlOperation.StamperFunctionPrototypes.Count - 1 do
      SelectItemForm.ListBox.Items.Add (WsdlOperation.StamperFunctionPrototypes.Strings [x]);
    SelectItemForm.ShowModal;
    if SelectItemForm.ModalResult = mrOk then
    begin
      ScriptMemo.SelText := SelectItemForm.SelectedItem;
    end;
  finally
    FreeAndNil (SelectItemForm);
  end;
end;

procedure TEditStamperForm.MemoPopUpMenuPopup(Sender: TObject);
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

procedure TEditStamperForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TEditStamperForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TEditStamperForm.Grammar1Click(Sender: TObject);
begin
   OpenDocument(PChar ( ExtractFilePath (ParamStr(0))
                       + '\Documentation\Grammar.htm'
                       )
               ); { *Converted from ShellExecute* }
end;

procedure TEditStamperForm.TopPanelResize(Sender: TObject);
begin
  ElementNameEdit.Width := TopPanel.Width
                         - ElementNameEdit.Left
                         - 5
                         ;

end;

procedure TEditStamperForm.EmbeddedSQLMenuItemClick(Sender: TObject);
begin
   OpenDocument(PChar ( ExtractFilePath (ParamStr(0))
                       + '\Documentation\EmbeddedSQL.htm'
                       )
               ); { *Converted from ShellExecute* }
end;

function TEditStamperForm.getElementName: String;
begin
  result := ElementNameEdit.Text;
end;

procedure TEditStamperForm.setElementName(const Value: String);
begin
  ElementNameEdit.Text := Value;
end;

procedure TEditStamperForm.setWsdlOperation(const Value: TWsdlOperation);
begin
  fWsdlOperation := Value;
end;

procedure TEditStamperForm.ScriptMemoChange(Sender: TObject);
begin
  fScriptChanged := True;
end;

procedure TEditStamperForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canClose := True;
  if fScriptChanged then
    if not (MessageDlg ( 'Discard changes to assignment?'
                      , mtConfirmation
                      , [mbYes, mbNo, mbCancel]
                      , 0
                      ) = mrYes) then
      CanClose := False;
end;

procedure TEditStamperForm.setBind(const Value: TCustomBindable);
begin
  fBind := Value;
  if wsdlOperation.reqBind.IsAncestorOf (Bindable) then
    ElementNameEdit.Text := 'Req.' + fBind.FullIndexCaption + ' := '
  else
    ElementNameEdit.Text := 'Rpy.' + fBind.FullIndexCaption + ' := ';
  if Copy (fBind.Value, 1, 2) = ':=' then
    ScriptMemo.Lines.Text := Trim (Copy (fBind.Value, 3, 30000))
  else
    ScriptMemo.Lines.Text := fBind.Value;
end;

end.
