unit EditOperationScriptUnit;

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
  , ParserClasses
  , Express, Bind, ComCtrls
  , Wsdlz
  , Xmlz
  , Menus
  , FormIniFilez
  , WsdlProjectz , SynHighlighterAny , SynMemo
  ;

type

  { TEditOperationScriptForm }

  TEditOperationScriptForm = class(TForm)
    SynAnySyn1 : TSynAnySyn ;
    ScriptMemo : TSynMemo ;
    TopPanel: TPanel;
    Label1: TLabel;
    ScriptNameEdit: TEdit;
    Panel3: TPanel;
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
    BeforeOrAfterEdit: TEdit;
    EmbeddedSQLMenuItem: TMenuItem;
    Helponfunctions1: TMenuItem;
    Anoperationbetweenquotes1: TMenuItem;
    N3: TMenuItem;
    ShowTokens1: TMenuItem;
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
    procedure DbNameMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Helponfunctions1Click(Sender: TObject);
    procedure Anoperationbetweenquotes1Click(Sender: TObject);
    procedure ShowTokens1Click(Sender: TObject);
  private
    fScriptChanged: Boolean;
    IniFile: TFormIniFile;
    LastCaption: String;
    fWsdlOperation: TWsdlOperation;
    fAfter, wasConnected: Boolean;
    procedure setAfter(const Value: Boolean);
    procedure setWsdlOperation(const Value: TWsdlOperation);
    function getScriptName: String;
    procedure setScriptName(const Value: String);
    procedure ExpressError( Sender: TObject
                          ; LineNumber, ColumnNumber, Offset: Integer
                          ; TokenString, Data: String
                          );
  public
    property WsdlOperation: TWsdlOperation read fWsdlOperation write setWsdlOperation;
    property After: Boolean read fAfter write setAfter;
    property ScriptName: String read getScriptName write setScriptName;
  end;

var
  EditOperationScriptForm: TEditOperationScriptForm;

implementation

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  SelectXmlElement
   , SelectItemUnit
   , SelectDbNameUnit
   , xmlUtilz
   ;

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

procedure TEditOperationScriptForm.FormShow(Sender: TObject);
var
  x: Integer;
begin
  wasConnected := _WsdlDbsConnection.Connected;
  if _WsdlDbsEnabled then
  begin
    try
      _WsdlDbsConnection.Connected := True;
      StatusBar.SimpleText := 'Database connected';
    except
      on E: Exception do
      begin
        StatusBar.SimpleText := 'Exception connecting DBS: ' + e.Message;
      end;
    end;
  end;
  ScriptMemo.ParentColor := ScriptMemo.ReadOnly;
  ScriptMemo.SetFocus;
  fScriptChanged := False;
end;

procedure TEditOperationScriptForm.OKBtnClick(Sender: TObject);
var
  Ok: Boolean;
  x: Integer;
begin
  StatusBar.SimpleText := '';
  AcquireLock;
  try
    if After then
    begin
      WsdlOperation.AfterScriptLines.Text := ScriptMemo.Text;
      try
        WsdlOperation.PrepareAfter;
      except
      end;
    end
    else
    begin
      WsdlOperation.BeforeScriptLines.Text := ScriptMemo.Text;
      try
        WsdlOperation.PrepareBefore;
      except
      end;
    end;
  finally
    fScriptChanged := False;
    ReleaseLock;
  end;
end;

procedure TEditOperationScriptForm.ExpressError(Sender: TObject; LineNumber,
  ColumnNumber, Offset: Integer; TokenString, Data: String);
var
  st, x: Integer;
begin
  st := 0;
  for x := 0 to LineNumber - 2 do
    st := ST + Length (ScriptMemo.Lines.Strings[x]) + 2;
  ScriptMemo.SelStart := st + ColumnNumber;
  ScriptMemo.SelEnd := ScriptMemo.SelStart + Length (TokenString);
  StatusBar.SimpleText := Data;
end;

procedure TEditOperationScriptForm.CheckButtonClick(Sender: TObject);
var
  x: Integer;
  SwapOnError: TOnErrorEvent;
  swapScriptLines: String;
begin
  StatusBar.SimpleText := '';
  SwapOnError := WsdlOperation.OnError;
  AcquireLock;
  try
    WsdlOperation.OnError := ExpressError;
    if After then
    begin
      SwapScriptLines := WsdlOperation.AfterScriptLines.Text;
      WsdlOperation.AfterScriptLines.Text := ScriptMemo.Lines.Text;
      try
        WsdlOperation.PrepareAfter;
      finally
        WsdlOperation.AfterScriptLines.Text := SwapScriptLines;
      end;
    end
    else
    begin
      SwapScriptLines := WsdlOperation.BeforeScriptLines.Text;
      WsdlOperation.BeforeScriptLines.Text := ScriptMemo.Lines.Text;
      try
        WsdlOperation.PrepareBefore;
      finally
        WsdlOperation.BeforeScriptLines.Text := SwapScriptLines;
      end;
    end;
  finally
    WsdlOperation.OnError := SwapOnError;
    ReleaseLock;
  end;
end;

procedure TEditOperationScriptForm.DbNameMenuItemClick(Sender: TObject);
begin
  Application.CreateForm(TSelectDbNameForm,SelectDbNameForm);
  try
    SelectDbNameForm.DataBase := _WsdlDbsConnection;
    SelectDbNameForm.ShowModal;
    if SelectDbNameForm.ModalResult = mrOk then
      ScriptMemo.SelText := SelectDbNameForm.SelectedDbName;
  finally
    FreeAndNil (SelectDbNameForm);
  end;
end;

procedure TEditOperationScriptForm.Anoperationbetweenquotes1Click(
  Sender: TObject);
var
  x: Integer;
begin
  Application.CreateForm(TSelectItemForm, SelectItemForm);
  try
    SelectItemForm.Caption := 'Select operation';
    SelectItemForm.ListBox.Clear;
    for x := 0 to allOperations.Count - 1 do
      SelectItemForm.ListBox.Items.Add (allOperations.Strings [x]);
    SelectItemForm.ShowModal;
    if SelectItemForm.ModalResult = mrOk then
    begin
      ScriptMemo.SelText := '''' + SelectItemForm.SelectedItem + '''';
    end;
  finally
    FreeAndNil (SelectItemForm);
  end;
end;

procedure TEditOperationScriptForm.CancelBtnClick(Sender:TObject);
begin
  StatusBar.SimpleText := '';
end;

procedure TEditOperationScriptForm.IpmFieldMenuItemClick(Sender: TObject);
begin
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  try
    SelectXmlElementForm.doShowReq := True;
    SelectXmlElementForm.doShowRpy := True;
    SelectXmlElementForm.doShowMq := True;
    SelectXmlElementForm.doShowWsa := True;
    SelectXmlElementForm.doShowRti := True;
    SelectXmlElementForm.WsdlOperation := WsdlOperation;
    SelectXmlElementForm.LastCaption := LastCaption;
    SelectXmlElementForm.IncludeRecurring := True;
    SelectXmlElementForm.maxOccurrences := WsdlOperation.Wsdl.XsdDescr.xsdElementsWhenRepeatable;
    SelectXmlElementForm.ElementEnabled := True;
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

procedure TEditOperationScriptForm.SelectFunctionMenuItemClick(Sender: TObject);
var
  x: Integer;
begin
  Application.CreateForm(TSelectItemForm, SelectItemForm);
  try
    SelectItemForm.Caption := 'Select function';
    SelectItemForm.ListBox.Clear;
    for x := 0 to WsdlOperation.FunctionPrototypes(After).Count - 1 do
      SelectItemForm.ListBox.Items.Add (WsdlOperation.FunctionPrototypes(After).Strings [x]);
    SelectItemForm.ShowModal;
    if SelectItemForm.ModalResult = mrOk then
    begin
      ScriptMemo.SelText := SelectItemForm.SelectedItem;
    end;
  finally
    FreeAndNil (SelectItemForm);
  end;
end;

procedure TEditOperationScriptForm.MemoPopUpMenuPopup(Sender: TObject);
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
}
  IpmFieldMenuItem.Enabled := (not ScriptMemo.ReadOnly);
  SelectFunctionMenuItem.Enabled := (not ScriptMemo.ReadOnly);
  DbNameMenuItem.Enabled := _WsdlDbsConnection.Connected
                        and (not ScriptMemo.ReadOnly);

end;

procedure TEditOperationScriptForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  After := False;
end;

procedure TEditOperationScriptForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TEditOperationScriptForm.Grammar1Click(Sender: TObject);
begin
   OpenDocument(PChar ( ExtractFilePath (ParamStr(0))
                       + '\Documentation\Grammar.htm'
                       )
               ); { *Converted from ShellExecute* }
end;

procedure TEditOperationScriptForm.Helponfunctions1Click(Sender: TObject);
begin
   OpenDocument(PChar (ChangeFileExt ( ExtractFilePath (ParamStr(0))
                                      + '\Documentation\BuiltIn'
                                      + ExtractFileName (ParamStr(0))
                                      , '.htm'
                                      )
                       )
               ); { *Converted from ShellExecute* }
end;

procedure TEditOperationScriptForm.TopPanelResize(Sender: TObject);
begin
  BeforeOrAfterEdit.Width := TopPanel.Width
                            - BeforeOrAfterEdit.Left
                            - 5
                            ;

end;

procedure TEditOperationScriptForm.EmbeddedSQLMenuItemClick(Sender: TObject);
begin
   OpenDocument(PChar ( ExtractFilePath (ParamStr(0))
                       + '\Documentation\EmbeddedSQL.htm'
                       )
               ); { *Converted from ShellExecute* }
end;

function TEditOperationScriptForm.getScriptName: String;
begin
  result := ScriptNameEdit.Text;
end;

procedure TEditOperationScriptForm.setScriptName(const Value: String);
begin
  ScriptNameEdit.Text := Value;
end;

procedure TEditOperationScriptForm.setWsdlOperation(const Value: TWsdlOperation);
begin
  fWsdlOperation := Value;
  if After then
    ScriptMemo.Lines.Text := fWsdlOperation.AfterScriptLines.Text
  else
    ScriptMemo.Lines.Text := fWsdlOperation.BeforeScriptLines.Text;
end;

procedure TEditOperationScriptForm.ShowTokens1Click(Sender: TObject);
begin
  if After then
    ShowText ('Tokens', WsdlOperation.DebugTokenStringAfter)
  else
    ShowText ('Tokens', WsdlOperation.DebugTokenStringBefore);
end;

procedure TEditOperationScriptForm.setAfter(const Value: Boolean);
begin
  fAfter := Value;
  if fAfter then
  begin
    BeforeOrAfterEdit.Text := 'After';
    if Assigned (fWsdlOperation) then
      ScriptMemo.Lines.Text := fWsdlOperation.AfterScriptLines.Text;
  end
  else
  begin
    BeforeOrAfterEdit.Text := 'Before';
    if Assigned (fWsdlOperation) then
      ScriptMemo.Lines.Text := fWsdlOperation.BeforeScriptLines.Text;
  end;
end;

procedure TEditOperationScriptForm.ScriptMemoChange(Sender: TObject);
begin
  fScriptChanged := True;
end;

procedure TEditOperationScriptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    _WsdlDbsConnection.Connected := wasConnected;
  except
  end;
end;

procedure TEditOperationScriptForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canClose := True;
  if fScriptChanged then
    if not (MessageDlg ( 'Discard changes to script?'
                      , mtConfirmation
                      , [mbYes, mbNo, mbCancel]
                      , 0
                      ) = mrYes) then
      CanClose := False;
end;

end.
