{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit EditOperationScriptUnit;

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
  , apiuiconsts
  , ParserClasses
  , Express, Bind, ComCtrls
  , Wsdlz
  , Xmlz
  , xmlio
  , Menus , ActnList
  , FormIniFilez
  , WsdlProjectz , SynHighlighterAny , SynEdit
  ;

type

  { TEditOperationScriptForm }

  TEditOperationScriptForm = class(TForm)
    CancelButton : TBitBtn ;
    FindAction : TAction ;
    FindNextAction : TAction ;
    CancelAction : TAction ;
    ImageList1 : TImageList ;
    OkAction : TAction ;
    CheckAction : TAction ;
    ActionList1 : TActionList ;
    OkButton : TBitBtn ;
    SynAnySyn1 : TSynAnySyn ;
    ScriptEdit : TSynEdit ;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TopPanel: TPanel;
    ScriptNameEdit: TEdit;
    Panel3: TPanel;
    Panel2: TPanel;
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
    Helponfunctions1: TMenuItem;
    Anoperationbetweenquotes1: TMenuItem;
    N3: TMenuItem;
    ShowTokens1: TMenuItem;
    procedure FindActionExecute (Sender : TObject );
    procedure FindNextActionExecute (Sender : TObject );
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ScriptEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure CheckExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure IpmFieldMenuItemClick(Sender: TObject);
    procedure MemoPopUpMenuPopup(Sender: TObject);
    procedure SelectFunctionMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Grammar1Click(Sender: TObject);
    procedure EmbeddedSQLMenuItemClick(Sender: TObject);
    procedure DbNameMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Helponfunctions1Click(Sender: TObject);
    procedure Anoperationbetweenquotes1Click(Sender: TObject);
    procedure ShowTokens1Click(Sender: TObject);
  private
    Finds: String;
    useRegExp: Boolean;
    fScriptChanged: Boolean;
    LastCaption: String;
    fWsdlOperation: TWsdlOperation;
    fAfter, wasConnected: Boolean;
    procedure doFind (aNext: Boolean);
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
   , FindRegExpDialog
   , xmlUtilz
   , RegExpr
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
begin
  wasConnected := _WsdlDbsConnector.Connected;
  if _WsdlDbsEnabled then
  begin
    try
      _WsdlDbsConnector.Connected := True;
      StatusBar.SimpleText := 'Database connected';
    except
      on E: Exception do
      begin
        StatusBar.SimpleText := 'Exception connecting DBS: ' + e.Message;
      end;
    end;
  end;
  ScriptEdit.ParentColor := ScriptEdit.ReadOnly;
  ScriptEdit.SetFocus;
  fScriptChanged := False;
  Screen.Cursor := crDefault;
end;

procedure TEditOperationScriptForm.OKExecute(Sender: TObject);
begin
  StatusBar.SimpleText := '';
  fScriptChanged := False;
  if (ScriptEdit.Lines.Count = 1)
  and (ScriptEdit.Lines.Strings[0] = '') then
    ScriptEdit.Lines.Delete(0);
end;

procedure TEditOperationScriptForm.ExpressError(Sender: TObject; LineNumber,
  ColumnNumber, Offset: Integer; TokenString, Data: String);
var
  st, x: Integer;
begin
  st := 0;
  for x := 0 to LineNumber - 2 do
    st := ST + Length (ScriptEdit.Lines.Strings[x]) + Length(LineEnding);
  ScriptEdit.SelStart := st + ColumnNumber;
  ScriptEdit.SelEnd := ScriptEdit.SelStart + Length (TokenString);
  StatusBar.SimpleText := Data;
end;

procedure TEditOperationScriptForm.CheckExecute(Sender: TObject);
var
  sl: TJBStringList;
begin
  StatusBar.SimpleText := '';
  ScriptEdit.SetFocus;
  sl := TJBStringList.Create;
  try
    sl.Text := ScriptEdit.Lines.Text;
    WsdlOperation.CheckScript(sl, ExpressError);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TEditOperationScriptForm.DbNameMenuItemClick(Sender: TObject);
begin
  Application.CreateForm(TSelectDbNameForm,SelectDbNameForm);
  try
    SelectDbNameForm.DataBase := _WsdlDbsConnector;
    SelectDbNameForm.ShowModal;
    if SelectDbNameForm.ModalResult = mrOk then
      ScriptEdit.SelText := SelectDbNameForm.SelectedDbName;
  finally
    FreeAndNil (SelectDbNameForm);
  end;
end;

procedure TEditOperationScriptForm.Anoperationbetweenquotes1Click(
  Sender: TObject);
var
  x: Integer;
begin
  if WsdlOperation.invokeList.Count = 0 then
    ShowMessage('No operations invoked');
  Application.CreateForm(TSelectItemForm, SelectItemForm);
  try
    SelectItemForm.Caption := 'Select operation';
    SelectItemForm.ListBox.Clear;
    with WsdlOperation.invokeList do
      for x := 0 to Count - 1 do
        SelectItemForm.ListBox.Items.Add (Operations [x].Alias);
    SelectItemForm.ShowModal;
    if SelectItemForm.ModalResult = mrOk then
    begin
      ScriptEdit.SelText := '''' + SelectItemForm.SelectedItem + '''';
    end;
  finally
    FreeAndNil (SelectItemForm);
  end;
end;

procedure TEditOperationScriptForm.CancelExecute(Sender:TObject);
begin
  StatusBar.SimpleText := '';
  ModalResult := mrCancel;
  Close;
end;

procedure TEditOperationScriptForm.IpmFieldMenuItemClick(Sender: TObject);
begin
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  try
    SelectXmlElementForm.doShowReq := True;
    SelectXmlElementForm.doShowRpy := True;
    SelectXmlElementForm.doShowWsa := True;
    SelectXmlElementForm.doShowRti := True;
    SelectXmlElementForm.doShowReqRpyInfo := True;
    SelectXmlElementForm.WsdlOperation := WsdlOperation;
    SelectXmlElementForm.LastCaption := LastCaption;
    SelectXmlElementForm.IncludeRecurring := True;
    SelectXmlElementForm.IncludeInvoked := True;
    SelectXmlElementForm.maxOccurrences := 1;
    SelectXmlElementForm.ElementEnabled := True;
    SelectXmlElementForm.ShowModal;
    if SelectXmlElementForm.ModalResult = mrOk then
    begin
      LastCaption := SelectXmlElementForm.SelectedCaption;
      ScriptEdit.SelText := LastCaption;
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
      ScriptEdit.SelText := SelectItemForm.SelectedItem;
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
  IpmFieldMenuItem.Enabled := (not ScriptEdit.ReadOnly);
  SelectFunctionMenuItem.Enabled := (not ScriptEdit.ReadOnly);
  DbNameMenuItem.Enabled := _WsdlDbsConnector.Connected
                        and (not ScriptEdit.ReadOnly);

end;

procedure TEditOperationScriptForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    Finds := StringByName['FindString'];
    useRegExp := BooleanByName['useRegExp'];
  finally
    Free;
  end;
  After := False;
end;

procedure TEditOperationScriptForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['FindString'] := Finds;
    BooleanByName['useRegExp'] := useRegExp;
    Save;
  finally
    Free;
  end;
end;

procedure TEditOperationScriptForm.Grammar1Click(Sender: TObject);
begin
  OpenURL (apiuiconsts.apiuiScriptGrammar);
end;

procedure TEditOperationScriptForm.Helponfunctions1Click(Sender: TObject);
begin
  OpenURL(apiuiconsts.apiuiAllFunctions);
end;

procedure TEditOperationScriptForm.EmbeddedSQLMenuItemClick(Sender: TObject);
begin
  OpenURL(apiuiconsts.apiuiEmbeddedSQL);
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
    ScriptEdit.Lines.Text := fWsdlOperation.AfterScriptLines.Text
  else
    ScriptEdit.Lines.Text := fWsdlOperation.BeforeScriptLines.Text;
end;

procedure TEditOperationScriptForm.ShowTokens1Click(Sender: TObject);
begin
  ShowText ('Tokens', WsdlOperation.DebugTokenStringBefore);
end;

procedure TEditOperationScriptForm .doFind (aNext: Boolean) ;
var
  FPos, IPos, FLen: Integer;
begin
  FPos := ScriptEdit.SelStart;
  FLen := Length(FindS);
  IPos := -1;
  if not useRegExp then
  begin
  {
    if frMatchcase in FindAction.Dialog.Options then
       IPos := Pos(FindS, Copy(ScriptEdit.Text,FPos+1,SLen-FPos))
    else
    }
     IPos := Pos(AnsiUpperCase(FindS),AnsiUpperCase( Copy (ScriptEdit.Text, FPos+1, Length (ScriptEdit.Text))));
  end
  else
  begin
    with TRegExpr.Create do
    try
      Expression := Finds;
      if Exec (Copy (ScriptEdit.Text, FPos+1, Length (ScriptEdit.Text))) then
        IPos := MatchPos[0];
    finally
      Free;
    end;
  end;

  if IPos > 0 then
  begin
    FPos := FPos + IPos;
    Self.ActiveControl := ScriptEdit;
    ScriptEdit.SelStart:= FPos;
    ScriptEdit.SelEnd := ScriptEdit.SelStart + FLen;
    FPos:=FPos+FLen-1;
  end
  else
  begin
    ShowMessageFmt('Text ''%s'' was not found', [FindS]);
  end;
end;

procedure TEditOperationScriptForm.setAfter(const Value: Boolean);
begin
  fAfter := Value;
  if fAfter then
  begin
    if Assigned (fWsdlOperation) then
      ScriptEdit.Lines.Text := fWsdlOperation.AfterScriptLines.Text;
  end
  else
  begin
    if Assigned (fWsdlOperation) then
      ScriptEdit.Lines.Text := fWsdlOperation.BeforeScriptLines.Text;
  end;
end;

procedure TEditOperationScriptForm.ScriptEditChange(Sender: TObject);
begin
  fScriptChanged := True;
end;

procedure TEditOperationScriptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    _WsdlDbsConnector.Connected := wasConnected;
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

procedure TEditOperationScriptForm .FindActionExecute (Sender : TObject );
begin
  Application.CreateForm(TFindDlg, FindDlg);
  try
    FindDlg.Caption := 'Find Text';
    FindDlg.SearchInRadioGroup.Enabled := False;
    FindDlg.ScopeRadioGroup.Enabled := False;
    FindDlg.SearchEdit.Text := Finds;
    FindDlg.ShowModal;
    if FindDlg.ModalResult = mrOk then
    begin
      Finds := FindDlg.SearchEdit.Text;
      useRegExp := Finddlg.RegularExpressionCheckBox.Checked;
      doFind(False);
    end;
  finally
    FreeAndNil(FindDlg);
  end;
end;

procedure TEditOperationScriptForm .FindNextActionExecute (Sender : TObject );
begin
  doFind(True);
end;

end.
