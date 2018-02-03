unit EditContextsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,Grids,Menus,
  StdCtrls,ActnList,ExtCtrls,Buttons,ComCtrls, FormIniFilez;

type

  { TEditContextsForm }

  TEditContextsForm = class(TForm)
    AddRowAfter: TMenuItem;
    AddRowBefore: TMenuItem;
    CancelButton: TBitBtn;
    ContextComboBox: TComboBox;
    Label1: TLabel;
    mainImageList: TImageList;
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
    PopupMenu1: TPopupMenu;
    StringGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure AddContextActionExecute(Sender: TObject);
    procedure AddPropertyActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    ColWidths: TStringList;
    function isPassWordColumn (aColumn: Integer): Boolean;
    function BooleanPromptDialog (aCaption: String): Boolean;
    procedure PopulateContextComboBox;
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

procedure TEditContextsForm.AddContextActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
  r, c: Integer;
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
      r := Row;
      Row := RowCount - 1;
      Cells [0, RowCount - 1] := PromptEdit.Text;
      for c := 1 to ColCount - 1 do
        Cells [c, RowCount - 1] := Cells [c, r];
      PopulateContextComboBox;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.AddPropertyActionExecute(Sender: TObject);
var
  xForm: TPromptForm;
  r, c: Integer;
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
      if PromptEdit.Text = 'context' then
        raise Exception.Create('"context" not allowed as property name');
      ColCount := ColCount + 1;
      c := Col;
      Col := ColCount - 1;
      Cells [ColCount - 1, 0] := PromptEdit.Text;
    end;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormCreate(Sender: TObject);
begin
  ColWidths := TStringList.Create;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    ColWidths.Text := StringByName['ColWidths'];
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormDestroy(Sender: TObject);
var
  c: Integer;
begin
  with TFormIniFile.Create(self, False) do
  try
    for c := 1 to StringGrid.ColCount - 1 do
      ColWidths.Values[StringGrid.Cells[c, 0]] := IntToStr(StringGrid.ColWidths[c]);
    StringByName['ColWidths'] := ColWidths.Text;
    Save;
  finally
    Free;
  end;
end;

procedure TEditContextsForm.FormShow(Sender: TObject);
var
  c, x: Integer;
begin
  for c := 1 to StringGrid.ColCount - 1 do
  begin
    if ColWidths.IndexOfName(StringGrid.Cells[c, 0]) > -1 then
      StringGrid.ColWidths[c] := StrToInt(ColWidths.Values[StringGrid.Cells[c, 0]]);
  end;
  PopulateContextComboBox;
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
      DeleteRow(Row);
      PopulateContextComboBox;
    end;
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
      DeleteCol(Col);
  end;
end;

procedure TEditContextsForm.RemovePropertyActionUpdate(Sender: TObject);
begin
  RemovePropertyAction.Enabled := (StringGrid.Col > 0);
end;

procedure TEditContextsForm.StringGridGetEditText(Sender: TObject; ACol,ARow: Integer;
  var Value: string);
begin
  if isPassWordColumn(ACol) then Value := xmlio.DecryptPassword(StringGrid.Cells[ACol, ARow]);
end;

procedure TEditContextsForm.StringGridSelectEditor(Sender: TObject; aCol,aRow: Integer;
  var Editor: TWinControl);
begin
  with (Editor as TCustomEdit) do
    if isPassWordColumn(aCol) then
      PasswordChar := '*'
    else
      PasswordChar := #0;
end;

procedure TEditContextsForm.StringGridSetEditText(Sender: TObject; ACol,ARow: Integer;
  const Value: string);
begin
  if isPassWordColumn(aCol) then StringGrid.Cells [ACol, ARow] := xmlio.EncryptPassword(Value);
end;

function TEditContextsForm.isPassWordColumn(aColumn: Integer): Boolean;
var
  colName: String;
begin
  colName := UpperCase(StringGrid.Cells[aColumn, 0]);
  result := (RightStr(colName, 3) = 'PWD')
         or (RightStr(colName, 8) = 'PASSWORD')
          ;
end;

function TEditContextsForm.BooleanPromptDialog(aCaption: String): Boolean;
begin
  result := (MessageDlg(aCaption, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TEditContextsForm.PopulateContextComboBox;
var
  r: Integer;
begin
  ContextComboBox.Items.Clear;
  for r := 1 to StringGrid.RowCount - 1 do
    ContextComboBox.Items.Add (StringGrid.Cells[0, r]);
end;

end.

