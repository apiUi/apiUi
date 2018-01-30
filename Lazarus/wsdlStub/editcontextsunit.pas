unit EditContextsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,Grids,Menus,
  StdCtrls,ActnList,ExtCtrls,Buttons, FormIniFilez;

type

  { TEditContextsForm }

  TEditContextsForm = class(TForm)
    CancelButton: TBitBtn;
    MenuItem4: TMenuItem;
    OkButton: TBitBtn;
    Panel2: TPanel;
    RemovePropertyAction: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    RemoveContextAction: TAction;
    AddPropertyAction: TAction;
    AddContextAction: TAction;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    AddRowBefore: TMenuItem;
    AddRowAfter: TMenuItem;
    PopupMenu1: TPopupMenu;
    StringGrid: TStringGrid;
    procedure AddContextActionExecute(Sender: TObject);
    procedure AddPropertyActionExecute(Sender: TObject);
    procedure AddRowBeforeClick(Sender: TObject);
    procedure AddRowAfterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure RemoveContextActionExecute(Sender: TObject);
    procedure RemoveContextActionUpdate(Sender: TObject);
    procedure RemovePropertyActionExecute(Sender: TObject);
    procedure RemovePropertyActionUpdate(Sender: TObject);
    procedure StringGridGetEditText(Sender: TObject; ACol,ARow: Integer;
      var Value: string);
    procedure StringGridSelectEditor(Sender: TObject; aCol,aRow: Integer;
      var Editor: TWinControl);
    procedure StringGridSetEditText(Sender: TObject; ACol,ARow: Integer;
      const Value: string);
  private
    function BooleanPromptDialog (aCaption: String): Boolean;
  public
    { public declarations }
  end;

var
  EditContextsForm: TEditContextsForm;

implementation

{$R *.lfm}

{ TEditContextsForm }
uses xmlio
   , PromptUnit
   ;

procedure TEditContextsForm.AddRowBeforeClick(Sender: TObject);
var
  r, c: Integer;
begin
  with StringGrid do
  begin
   RowCount := RowCount + 1;
   for r := RowCount - 2 downto Row do
     Rows [r + 1] := Rows [r];
   for c := 0 to ColCount - 1 do
   begin
     Cells[c, Row] := '';
     Objects[c, Row] := nil;
   end;
  end;
end;

procedure TEditContextsForm.AddContextActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
begin
  Application.CreateForm(TPromptForm, xForm);
  with xForm do
  try
    Caption := 'Name for new Context';
    PromptEdit.Text := '';
    Numeric := False;
    Pattern := '[A-Za-z0-9\.,]+';
    ShowModal;
    if ModalResult = mrOk then with StringGrid do
    begin
      RowCount := RowCount + 1;
      Row := RowCount - 1;
      Cells [0, RowCount - 1] := PromptEdit.Text;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.AddPropertyActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
begin
  Application.CreateForm(TPromptForm, xForm);
  with xForm do
  try
    Caption := 'Name for new Property';
    PromptEdit.Text := '';
    Numeric := False;
    Pattern := '[A-Za-z0-9\.,]+';
    ShowModal;
    if ModalResult = mrOk then with StringGrid do
    begin
      ColCount := ColCount + 1;
      Col := ColCount - 1;
      Cells [ColCount - 1, 0] := PromptEdit.Text;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.AddRowAfterClick(Sender: TObject);
var
  r, c: Integer;
begin
  with StringGrid do
  begin
   RowCount := RowCount + 1;
   Row := Row + 1;
   for r := RowCount - 2 downto Row do
     Rows [r + 1] := Rows [r];
   for c := 0 to ColCount - 1 do
   begin
     Cells[c, Row] := '';
     Objects[c, Row] := nil;
   end;
   Cells [0, Row] := IntToStr(RowCount);
  end;
end;

procedure TEditContextsForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.PopupMenu1Popup(Sender: TObject);
begin
  RemovePropertyActionUpdate(nil);
  RemoveContextActionUpdate(nil);
end;

procedure TEditContextsForm.RemoveContextActionExecute(Sender: TObject);
var
  r: Integer;
begin
  with StringGrid do
  begin
    if BooleanPromptDialog('Remove context ' + Cells [0, Row]) then
    begin
      for r := Row to RowCount - 2 do
        Rows[r] := Rows [r + 1];
    end;
    RowCount := RowCount - 1;
  end;
end;

procedure TEditContextsForm.RemoveContextActionUpdate(Sender: TObject);
begin
  RemoveContextAction.Enabled := (StringGrid.Row > 0);
end;

procedure TEditContextsForm.RemovePropertyActionExecute(Sender: TObject);
var
  c: Integer;
begin
  with StringGrid do
  begin
    if BooleanPromptDialog('Remove property ' + Cells [Col, 0]) then
    begin
      for c := Col to ColCount - 2 do
        Cols[c] := Cols [c + 1];
    end;
    ColCount := ColCount - 1;
  end;
end;

procedure TEditContextsForm.RemovePropertyActionUpdate(Sender: TObject);
begin
  RemovePropertyAction.Enabled := (StringGrid.Col > 0);
end;

procedure TEditContextsForm.StringGridGetEditText(Sender: TObject; ACol,ARow: Integer;
  var Value: string);
begin
  if false then Value := xmlio.DecryptPassword(StringGrid.Cells[ACol, ARow]);
end;

procedure TEditContextsForm.StringGridSelectEditor(Sender: TObject; aCol,aRow: Integer;
  var Editor: TWinControl);
begin
  if false then
  with (Editor as TCustomEdit) do
    if aCol = 1 then
      PasswordChar := '*'
    else
      PasswordChar := #0;
end;

procedure TEditContextsForm.StringGridSetEditText(Sender: TObject; ACol,ARow: Integer;
  const Value: string);
begin
  if false then StringGrid.Cells [ACol, ARow] := xmlio.EncryptPassword(Value);
end;

function TEditContextsForm.BooleanPromptDialog(aCaption: String): Boolean;
begin
  result := (MessageDlg(aCaption, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

end.

