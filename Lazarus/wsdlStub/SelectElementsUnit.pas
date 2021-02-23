unit SelectElementsUnit;

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
  Buttons, ExtCtrls, ComCtrls, ActnList
   , FormIniFilez
   , Dialogs
   , Wsdlz
   , Bind
   , Xmlz
   ;

type

  { TSelectElementsForm }

  TSelectElementsForm = class(TForm)
    Button5: TButton;
    Panel1: TPanel;
    OKBtn: TButton;
    Panel2: TPanel;
    ListView: TListView;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    EditAction: TAction;
    DeleteAction: TAction;
    AddAction: TAction;
    OKAction: TAction;
    OpenFileDialog: TOpenDialog;
    Button4: TButton;
    Button3: TButton;
    DeleteAllAction: TAction;
    RowUpAction: TAction;
    BitBtn1: TBitBtn;
    RowDownAction: TAction;
    BitBtn2: TBitBtn;
    procedure EditActionUpdate(Sender: TObject);
    procedure DeleteActionUpdate(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure OKActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure DeleteAllActionUpdate(Sender: TObject);
    procedure DeleteAllActionExecute(Sender: TObject);
    procedure RowUpActionUpdate(Sender: TObject);
    procedure RowUpActionExecute(Sender: TObject);
    procedure RowDownActionExecute(Sender: TObject);
    procedure RowDownActionUpdate(Sender: TObject);
  private
    fLastCaption: String;
    procedure UpdateListView;
  public
    doShowReq, doShowRpy, doShowMq, doShowWsa, doShowRti: Boolean;
    WsdlOperation: TWsdlOperation;
    SrceBind: TCustomBindable;
    ControlBinds: TBindableList;
    DuplicatesAllowed, GroupAllowed: Boolean;
    stubChanged: Boolean;
  end;

var
  SelectElementsForm: TSelectElementsForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses SelectXmlElement
   , igGlobals
   ;

procedure TSelectElementsForm.EditActionUpdate(Sender: TObject);
begin
  EditAction.Enabled := (ListView.Selected <> nil);
end;

procedure TSelectElementsForm.DeleteActionUpdate(Sender: TObject);
begin
  DeleteAction.Enabled := (ListView.Selected <> nil);
end;

procedure TSelectElementsForm.AddActionUpdate(Sender: TObject);
begin
  AddAction.Enabled := True;
end;

procedure TSelectElementsForm.DeleteActionExecute(Sender: TObject);
var
  xItemIndex: Integer;
begin
  if MessageDlg ( 'Delete item '
                + ListView.Selected.Caption
                + '?'
                , mtWarning
                , [mbYes, mbNo]
                , 0) = mrYes
  then
  begin
    xItemIndex := ListView.ItemIndex;
    ControlBinds.Delete(ListView.ItemIndex);
    UpdateListView;
    if xItemIndex = ListView.Items.Count then
      xItemIndex := xItemIndex - 1;
    if ListView.Items.Count > 0 then
      ListView.ItemIndex := xItemIndex;
    stubChanged := True;
  end;
end;

procedure TSelectElementsForm.AddActionExecute(Sender: TObject);
  function BooleanPromptDialog(aPrompt: String): Boolean;
  var
    mr: TModalResult;
  begin
    mr := MessageDlg(aPrompt, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if mr = mrCancel then
      raise Exception.Create('Cancelled by user');
    if (mr = mrNo)
    and (not GroupAllowed) then
      raise Exception.Create ('Group elements not allowed in context of: ' + Caption);
    result := (mr = mrYes);
  end;
var
  xAdd: Integer;
  xDoExpand: Boolean;
  procedure _AddBind (aBind: TCustomBindable; aPrefix: String);
  var
    x: Integer;
  begin
    if aBind = nil then
      exit;
    if (aBind.Children.Count > 0)
    and xDoExpand then
    begin
      for x := 0 to aBind.Children.Count - 1 do
        _AddBind (aBind.Children.Bindables [x], aPrefix);
    end
    else
    begin
      xAdd := ControlBinds.AddObject ( aPrefix + aBind.FullIndexCaption
                                     , aBind
                                     );
    end;
  end; {AddBind}

begin
  xAdd := -1;
  Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
  try
    SelectXmlElementForm.LastCaption := fLastCaption;
    SelectXmlElementForm.doShowReq := doShowReq;
    SelectXmlElementForm.doShowRpy := doShowRpy;
    SelectXmlElementForm.doShowMq := doShowMq;
    SelectXmlElementForm.doShowWsa := doShowWsa;
    SelectXmlElementForm.doShowRti := doShowRti;
    SelectXmlElementForm.WsdlOperation := WsdlOperation;
    SelectXmlElementForm.IncludeRecurring := True;
    SelectXmlElementForm.maxOccurrences := 1;
    SelectXmlElementForm.ShowModal;
    if SelectXmlElementForm.ModalResult = mrOk then
    begin
      fLastCaption := SelectXmlElementForm.SelectedCaption;
      if SelectXmlElementForm.SelectedBind.Children.Count > 0 then
        xDoExpand := BooleanPromptDialog('Expand group ' + fLastCaption);
      _AddBind (SelectXmlElementForm.SelectedBind, Copy (fLastCaption, 1, Pos('.', fLastCaption)));
      stubChanged := True;
    end;
  finally
    FreeAndNil (SelectXmlElementForm);
  end;
  UpdateListView;
  ListView.ItemIndex := xAdd;
end;

procedure TSelectElementsForm.EditActionExecute(Sender: TObject);
var
  x: Integer;
begin
  x := ListView.ItemIndex;
  if x > -1 then
  begin
    Application.CreateForm(TSelectXmlElementForm, SelectXmlElementForm);
    try
      SelectXmlElementForm.LastCaption := fLastCaption;
      SelectXmlElementForm.doShowReq := doShowReq;
      SelectXmlElementForm.doShowRpy := doShowRpy;
      SelectXmlElementForm.doShowMq := doShowMq;
      SelectXmlElementForm.doShowWsa := doShowWsa;
      SelectXmlElementForm.doShowRti := doShowRti;
      SelectXmlElementForm.WsdlOperation := WsdlOperation;
      SelectXmlElementForm.IncludeRecurring := True;
      SelectXmlElementForm.maxOccurrences := 1;
      SelectXmlElementForm.ShowModal;
      if SelectXmlElementForm.ModalResult = mrOk then
      begin
        fLastCaption := SelectXmlElementForm.SelectedCaption;
        ControlBinds.Strings [x] := SelectXmlElementForm.SelectedCaption;
        ControlBinds.Bindables [x] := SelectXmlElementForm.SelectedBind;
        stubChanged := True;
      end;
    finally
      FreeAndNil (SelectXmlElementForm);
    end;
    UpdateListView;
    ListView.ItemIndex := x;
  end;
end;

procedure TSelectElementsForm.OKActionUpdate(Sender: TObject);
begin
  OKAction.Enabled := True {(ListView.Selected <> nil)}
                 ;
end;

procedure TSelectElementsForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TSelectElementsForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TSelectElementsForm.FormShow(Sender: TObject);
begin
  UpdateListView;
  stubChanged := False;
end;

procedure TSelectElementsForm.UpdateListView;
  function _LastCaption (aCaption: String): String;
  var
    x, p: Integer;
  begin
    result := '';
    p := 1;
    for x := 1 to Length (aCaption) do
      if aCaption [x] = '.' then
        p := x + 1;
    result := Copy (aCaption, p, MaxInt);
  end;

var
  x: Integer;
  ListItem: TListItem;
begin
  ListView.Clear;
  for x := 0 to ControlBinds.Count - 1 do
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := _LastCaption (ControlBinds.Strings [x]);
    ListItem.SubItems.Add(ControlBinds.Strings [x]);
  end;
  if ListView.Items.Count > 0 then
    ListView.ItemIndex := 0;
  ListView.SetFocus;
end;

procedure TSelectElementsForm.OKBtnClick(Sender: TObject);
var
  xLow: Integer;
  xHigh: Integer;
begin
  if not DuplicatesAllowed then
  begin
    for xLow := 0 to ListView.Items.Count - 2 do
    begin
      for xHigh := xLow + 1 to ListView.Items.Count - 1 do
      begin
        if ListView.Items [xLow].SubItems [0]
         = ListView.Items [xHigh].SubItems [0] then
        begin
          ModalResult := mrNone;
          ListView.SetFocus;
          ListView.Selected := ListView.Items [xHigh];
          Raise Exception.Create ( 'Duplicates row: '
                                 + IntToStr (xLow + 1)
                                 );
        end; {if duplicate}
      end; {xHigh}
    end; {xLow}
  end; {Dups not allowed}
end;

procedure TSelectElementsForm.DeleteAllActionUpdate(Sender: TObject);
begin
  DeleteAllAction.Enabled := (ListView.Items.Count > 0);
end;

procedure TSelectElementsForm.DeleteAllActionExecute(Sender: TObject);
begin
  if MessageDlg ( 'Delete all items '
                + '?'
                , mtWarning
                , [mbYes, mbNo]
                , 0) = mrYes
  then
  begin
    ControlBinds.Clear;
    UpdateListView;
    stubChanged := True;
  end;
end;

procedure TSelectElementsForm.RowUpActionUpdate(Sender: TObject);
begin
  RowUpAction.Enabled := (ListView.ItemIndex > 0);
end;

procedure TSelectElementsForm.RowUpActionExecute(Sender: TObject);
var
  x: Integer;
  xString: String;
  xBind: TCustomBindable;
begin
  x := ListView.ItemIndex;
  xBind := ControlBinds.Bindables [x];
  ControlBinds.Bindables [x] := ControlBinds.Bindables [x - 1];
  ControlBinds.Bindables [x - 1] := xBind;
  xString := ControlBinds.Strings [x];
  ControlBinds.Strings [x] := ControlBinds.Strings [x - 1];
  ControlBinds.Strings [x - 1] := xString;
  UpdateListView;
  ListView.ItemIndex := x - 1;
  ListView.ItemFocused := ListView.Selected;
  stubChanged := True;
end;

procedure TSelectElementsForm.RowDownActionExecute(Sender: TObject);
var
  x: Integer;
  xString: String;
  xBind: TCustomBindable;
begin
  x := ListView.ItemIndex;
  xBind := ControlBinds.Bindables [x];
  ControlBinds.Bindables [x] := ControlBinds.Bindables [x + 1];
  ControlBinds.Bindables [x + 1] := xBind;
  xString := ControlBinds.Strings [x];
  ControlBinds.Strings [x] := ControlBinds.Strings [x + 1];
  ControlBinds.Strings [x + 1] := xString;
  UpdateListView;
  ListView.ItemIndex := x + 1;
  ListView.ItemFocused := ListView.Selected;
  stubChanged := True;
end;

procedure TSelectElementsForm.RowDownActionUpdate(Sender: TObject);
begin
  RowDownAction.Enabled := (ListView.ItemIndex > -1)
                     and (ListView.ItemIndex < ListView.Items.Count - 1);
end;

end.
