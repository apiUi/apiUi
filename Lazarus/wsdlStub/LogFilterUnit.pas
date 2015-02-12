unit LogFilterUnit;

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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormIniFilez, StdCtrls, ExtCtrls
  , Logz, Wsdlz;

type
  TLogFilterForm = class(TForm)
    FilteringPanel: TPanel;
    ActionPanel: TPanel;
    ActionEnabledCheckBox: TCheckBox;
    ActionEqualsCombo: TComboBox;
    ActionsCombo: TComboBox;
    ServicePanel: TPanel;
    ServiceEnabledCheckBox: TCheckBox;
    ServiceEqualsCombo: TComboBox;
    ServiceEdit: TEdit;
    ServiceRegExpCheckBox: TCheckBox;
    OperationPanel: TPanel;
    OperationEnabledCheckBox: TCheckBox;
    OperationEqualsCombo: TComboBox;
    OperationEdit: TEdit;
    OperationRegExpCheckBox: TCheckBox;
    CorrelationPanel: TPanel;
    CorrelationEnabledCheckBox: TCheckBox;
    CorrelationEqualsCombo: TComboBox;
    CorrelationEdit: TEdit;
    CorrelationRegExpCheckBox: TCheckBox;
    ButtonPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    EnabledCheckBox: TCheckBox;
    ValidationPanel: TPanel;
    ValidationErrorEnabledCheckBox: TCheckBox;
    RequestValidationCheckBox: TCheckBox;
    ReplyValidationCheckBox: TCheckBox;
    ExceptionPanel: TPanel;
    ExceptionEqualsCombo: TComboBox;
    ExceptionEdit: TEdit;
    ExceptionRegExpCheckBox: TCheckBox;
    ExceptionEnabledCheckBox: TCheckBox;
    MiMPanel: TPanel;
    MiMEnabledCheckBox: TCheckBox;
    RequestMiMCheckBox: TCheckBox;
    ReplyMiMCheckBox: TCheckBox;
    ReqPanel: TPanel;
    RpyPanel: TPanel;
    RequestEnabledCheckBox: TCheckBox;
    RequestEqualsComboBox: TComboBox;
    CheckBox1: TCheckBox;
    ReplyEnabledCheckBox: TCheckBox;
    ReplyEqualsComboBox: TComboBox;
    RequestEdit: TEdit;
    ReplyEdit: TEdit;
    CheckBox2: TCheckBox;
    Panel1: TPanel;
    UnexpectedValuesCheckBox: TCheckBox;
    MatchStyleCombo: TComboBox;
    Panel2: TPanel;
    FilterStyleCombo: TComboBox;
    RemarksCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure SetEnabled (Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getUnexpectedValuesEnabled: Boolean;
    procedure setUnexpectedValuesEnabled(const Value: Boolean);
    function getReplyEnabled: Boolean;
    function getReplyEquals: Boolean;
    function getReplyValue: String;
    function getRequestEnabled: Boolean;
    function getRequestEquals: Boolean;
    function getRequestValue: String;
    procedure setReplyEnabled(const Value: Boolean);
    procedure setReplyEquals(const Value: Boolean);
    procedure setReplyValue(const Value: String);
    procedure setRequestEnabled(const Value: Boolean);
    procedure setRequestEquals(const Value: Boolean);
    procedure setRequestValue(const Value: String);
    function getMiMEnabled: Boolean;
    function getReplyMiMEnabled: Boolean;
    function getRequestMiMEnabled: Boolean;
    procedure setMiMEnabled(const Value: Boolean);
    procedure setReplyMiMEnabled(const Value: Boolean);
    procedure setRequestMiMEnabled(const Value: Boolean);
    function getMessageValidationEnabled: Boolean;
    function getReplyValidationEnabled: Boolean;
    function getRequestValidationEnabled: Boolean;
    procedure setMessageValidationEnabled(const Value: Boolean);
    procedure setReplyValidationEnabled(const Value: Boolean);
    procedure setRequestValidationEnabled(const Value: Boolean);
    function getExceptionEnabled: Boolean;
    procedure setExceptionEnabled(const Value: Boolean);
    function getExceptionEquals: Boolean;
    function getExceptionIsRegExpr: Boolean;
    function getExceptionValue: String;
    procedure setExceptionEquals(const Value: Boolean);
    procedure setExceptionIsRegExpr(const Value: Boolean);
    procedure setExceptionValue(const Value: String);
    function getServiceEnabled: Boolean;
    procedure setServiceEnabled(const Value: Boolean);
    function getServiceEquals: Boolean;
    function getServiceIsRegExpr: Boolean;
    function getServiceValue: String;
    procedure setServiceEquals(const Value: Boolean);
    procedure setServiceIsRegExpr(const Value: Boolean);
    procedure setServiceValue(const Value: String);
    function getOperationEquals: Boolean;
    function getOperationIsRegExpr: Boolean;
    function getOperationValue: String;
    procedure setOperationEquals(const Value: Boolean);
    procedure setOperationIsRegExpr(const Value: Boolean);
    procedure setOperationValue(const Value: String);
    function getCorrelationEquals: Boolean;
    function getCorrelationIsRegExpr: Boolean;
    function getCorrelationValue: String;
    procedure setCorrelationEquals(const Value: Boolean);
    procedure setCorrelationIsRegExpr(const Value: Boolean);
    procedure setCorrelationValue(const Value: String);
    function getActionValue: TStubAction;
    procedure setActionValue(const Value: TStubAction);
    function getActionEquals: Boolean;
    procedure setActionEquals(const Value: Boolean);
    function getCorrelationEnabled: Boolean;
    procedure setCorrelationEnabled(const Value: Boolean);
    function getOperationEnabled: Boolean;
    procedure setOperationEnabled(const Value: Boolean);
    function getActionEnabled: Boolean;
    function getFilterEnabled: Boolean;
    procedure setFilterEnabled(const Value: Boolean);
    procedure setActionEnabled(const Value: Boolean);
    function getFilterStyle: TLogFilterStyle;
    procedure setFilterStyle(const Value: TLogFilterStyle);
    function getMatchAny: Boolean;
    procedure setMatchAny(const Value: Boolean);
    function getRemarksEnabled: Boolean;
    procedure setRemarksEnabled(const Value: Boolean);
  public
    property FilterEnabled: Boolean read getFilterEnabled write setFilterEnabled;
    property FilterStyle: TLogFilterStyle read getFilterStyle write setFilterStyle;
    property MatchAny: Boolean read getMatchAny write setMatchAny;
    property ActionEnabled: Boolean read getActionEnabled write setActionEnabled;
    property ActionEquals: Boolean read getActionEquals write setActionEquals;
    property ActionValue: TStubAction read getActionValue write setActionValue;
    property MiMEnabled: Boolean read getMiMEnabled write setMiMEnabled;
    property RequestMiMEnabled: Boolean read getRequestMiMEnabled write setRequestMiMEnabled;
    property ReplyMiMEnabled: Boolean read getReplyMiMEnabled write setReplyMiMEnabled;
    property MessageValidationEnabled: Boolean read getMessageValidationEnabled write setMessageValidationEnabled;
    property RequestValidationEnabled: Boolean read getRequestValidationEnabled write setRequestValidationEnabled;
    property ReplyValidationEnabled: Boolean read getReplyValidationEnabled write setReplyValidationEnabled;
    property ExceptionEnabled: Boolean read getExceptionEnabled write setExceptionEnabled;
    property ExceptionEquals: Boolean read getExceptionEquals write setExceptionEquals;
    property ExceptionValue: String read getExceptionValue write setExceptionValue;
    property ExceptionIsRegExpr: Boolean read getExceptionIsRegExpr write setExceptionIsRegExpr;
    property ServiceEnabled: Boolean read getServiceEnabled write setServiceEnabled;
    property ServiceEquals: Boolean read getServiceEquals write setServiceEquals;
    property ServiceValue: String read getServiceValue write setServiceValue;
    property ServiceIsRegExpr: Boolean read getServiceIsRegExpr write setServiceIsRegExpr;
    property OperationEnabled: Boolean read getOperationEnabled write setOperationEnabled;
    property OperationEquals: Boolean read getOperationEquals write setOperationEquals;
    property OperationValue: String read getOperationValue write setOperationValue;
    property OperationIsRegExpr: Boolean read getOperationIsRegExpr write setOperationIsRegExpr;
    property CorrelationEnabled: Boolean read getCorrelationEnabled write setCorrelationEnabled;
    property CorrelationEquals: Boolean read getCorrelationEquals write setCorrelationEquals;
    property CorrelationValue: String read getCorrelationValue write setCorrelationValue;
    property CorrelationIsRegExpr: Boolean read getCorrelationIsRegExpr write setCorrelationIsRegExpr;
    property RequestEnabled: Boolean read getRequestEnabled write setRequestEnabled;
    property RequestEquals: Boolean read getRequestEquals write setRequestEquals;
    property RequestValue: String read getRequestValue write setRequestValue;
    property ReplyEnabled: Boolean read getReplyEnabled write setReplyEnabled;
    property ReplyEquals: Boolean read getReplyEquals write setReplyEquals;
    property ReplyValue: String read getReplyValue write setReplyValue;
    property UnexpectedValuesEnabled: Boolean read getUnexpectedValuesEnabled write setUnexpectedValuesEnabled;
    property RemarksEnabled: Boolean read getRemarksEnabled write setRemarksEnabled;
  end;

var
  LogFilterForm: TLogFilterForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TLogFilterForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TLogFilterForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TLogFilterForm.setActionEnabled(const Value: Boolean);
begin
  ActionEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setServiceEnabled(const Value: Boolean);
begin
  ServiceEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setFilterEnabled(const Value: Boolean);
begin
  EnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setFilterStyle(const Value: TLogFilterStyle);
begin
  FilterStyleCombo.ItemIndex := Ord (Value);
end;

function TLogFilterForm.getFilterEnabled: Boolean;
begin
  result := EnabledCheckBox.Checked;
end;

function TLogFilterForm.getFilterStyle: TLogFilterStyle;
begin
  result := TLogFilterStyle(FilterStyleCombo.ItemIndex);
end;

procedure TLogFilterForm.SetEnabled (Sender: TObject);
  function nEnabledConditions: Integer;
  begin
    result := 0;
    if ActionEnabled then Inc (result);
    if MiMEnabled then Inc (result);
    if MessageValidationEnabled then Inc (result);
    if ExceptionEnabled then Inc (result);
    if ServiceEnabled then Inc (result);
    if OperationEnabled then Inc (result);
    if CorrelationEnabled then Inc (result);
    if RequestEnabled then Inc (result);
    if ReplyEnabled then Inc (result);
    if UnexpectedValuesEnabled then Inc (result);
  end;
begin
  FilterStyleCombo.Enabled := FilterEnabled;
  MatchStyleCombo.Enabled := FilterEnabled and (nEnabledConditions > 1);
  ActionEnabledCheckBox.Enabled := FilterEnabled;
  ActionEqualsCombo.Enabled := FilterEnabled and ActionEnabled;
  ActionsCombo.Enabled := FilterEnabled and ActionEnabled;
  MiMEnabledCheckBox.Enabled := FilterEnabled;
  RequestMiMCheckBox.Enabled := FilterEnabled and MiMEnabled;
  ReplyMiMCheckBox.Enabled := FilterEnabled and MiMEnabled;
  ValidationErrorEnabledCheckBox.Enabled := FilterEnabled;
  RequestValidationCheckBox.Enabled := FilterEnabled and MessageValidationEnabled;
  ReplyValidationCheckBox.Enabled := FilterEnabled and MessageValidationEnabled;
  ExceptionEnabledCheckBox.Enabled := FilterEnabled;
  ExceptionEqualsCombo.Enabled := FilterEnabled and ExceptionEnabled;
  ExceptionEdit.Enabled := FilterEnabled and ExceptionEnabled;
  ExceptionRegExpCheckBox.Enabled := FilterEnabled and ExceptionEnabled;
  ServiceEnabledCheckBox.Enabled := FilterEnabled;
  ServiceEqualsCombo.Enabled := FilterEnabled and ServiceEnabled;
  ServiceEdit.Enabled := FilterEnabled and ServiceEnabled;
  ServiceRegExpCheckBox.Enabled := FilterEnabled and ServiceEnabled;
  OperationEnabledCheckBox.Enabled := FilterEnabled;
  OperationEqualsCombo.Enabled := FilterEnabled and OperationEnabled;
  OperationEdit.Enabled := FilterEnabled and OperationEnabled;
  OperationRegExpCheckBox.Enabled := FilterEnabled and OperationEnabled;
  CorrelationEnabledCheckBox.Enabled := FilterEnabled;
  CorrelationEqualsCombo.Enabled := FilterEnabled and CorrelationEnabled;
  CorrelationEdit.Enabled := FilterEnabled and CorrelationEnabled;
  CorrelationRegExpCheckBox.Enabled := FilterEnabled and CorrelationEnabled;
  RequestEnabledCheckBox.Enabled := FilterEnabled;
  RequestEqualsCombobox.Enabled := FilterEnabled and RequestEnabled;
  RequestEdit.Enabled := FilterEnabled and RequestEnabled;
  ReplyEnabledCheckBox.Enabled := FilterEnabled;
  ReplyEqualsCombobox.Enabled := FilterEnabled and ReplyEnabled;
  ReplyEdit.Enabled := FilterEnabled and ReplyEnabled;
  UnexpectedValuesCheckBox.Enabled := FilterEnabled;
  RemarksCheckBox.Enabled := FilterEnabled;
end;

function TLogFilterForm.getActionEnabled: Boolean;
begin
  result := ActionEnabledCheckBox.Checked;
end;

procedure TLogFilterForm.FormShow(Sender: TObject);
begin
  SetEnabled (nil);
end;

function TLogFilterForm.getOperationEnabled: Boolean;
begin
  result := OperationEnabledCheckBox.Checked;
end;

procedure TLogFilterForm.setOperationEnabled(const Value: Boolean);
begin
  OperationEnabledCheckBox.Checked := Value;
end;

function TLogFilterForm.getCorrelationEnabled: Boolean;
begin
  result := CorrelationEnabledCheckBox.Checked;
end;

procedure TLogFilterForm.setCorrelationEnabled(const Value: Boolean);
begin
  CorrelationEnabledCheckBox.Checked := Value;
end;

function TLogFilterForm.getActionEquals: Boolean;
begin
  result := (ActionEqualsCombo.ItemIndex = 0);
end;

procedure TLogFilterForm.setActionEquals(const Value: Boolean);
begin
  if Value then
    ActionEqualsCombo.ItemIndex := 0
  else
    ActionEqualsCombo.ItemIndex := 1;
end;

function TLogFilterForm.getActionValue: TStubAction;
begin
  result := TStubAction (ActionsCombo.ItemIndex);
end;

procedure TLogFilterForm.setActionValue(const Value: TStubAction);
begin
  ActionsCombo.ItemIndex := Ord (Value);
end;

//Exception
procedure TLogFilterForm.setExceptionEnabled(const Value: Boolean);
begin
  ExceptionEnabledCheckBox.Checked := Value;
end;

function TLogFilterForm.getExceptionEnabled: Boolean;
begin
  result := ExceptionEnabledCheckBox.Checked;
end;

function TLogFilterForm.getExceptionEquals: Boolean;
begin
  result := (ExceptionEqualsCombo.ItemIndex = 0);
end;

function TLogFilterForm.getExceptionIsRegExpr: Boolean;
begin
  result := ExceptionRegExpCheckBox.Checked;
end;

function TLogFilterForm.getExceptionValue: String;
begin
  result := ExceptionEdit.Text;
end;

procedure TLogFilterForm.setExceptionEquals(const Value: Boolean);
begin
  if Value then
    ExceptionEqualsCombo.ItemIndex := 0
  else
    ExceptionEqualsCombo.ItemIndex := 1;
end;

procedure TLogFilterForm.setExceptionIsRegExpr(const Value: Boolean);
begin
  ExceptionRegExpCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setExceptionValue(const Value: String);
begin
  ExceptionEdit.Text := Value;
end;

//service
function TLogFilterForm.getServiceEnabled: Boolean;
begin
  result := ServiceEnabledCheckBox.Checked;
end;

function TLogFilterForm.getServiceEquals: Boolean;
begin
  result := (ServiceEqualsCombo.ItemIndex = 0);
end;

function TLogFilterForm.getServiceIsRegExpr: Boolean;
begin
  result := ServiceRegExpCheckBox.Checked;
end;

function TLogFilterForm.getServiceValue: String;
begin
  result := ServiceEdit.Text;
end;

procedure TLogFilterForm.setServiceEquals(const Value: Boolean);
begin
  if Value then
    ServiceEqualsCombo.ItemIndex := 0
  else
    ServiceEqualsCombo.ItemIndex := 1;
end;

procedure TLogFilterForm.setServiceIsRegExpr(const Value: Boolean);
begin
  ServiceRegExpCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setServiceValue(const Value: String);
begin
  ServiceEdit.Text := Value;
end;

//operation
function TLogFilterForm.getOperationEquals: Boolean;
begin
  result := (OperationEqualsCombo.ItemIndex = 0);
end;

function TLogFilterForm.getOperationIsRegExpr: Boolean;
begin
  result := OperationRegExpCheckBox.Checked;
end;

function TLogFilterForm.getOperationValue: String;
begin
  result := OperationEdit.Text;
end;

procedure TLogFilterForm.setOperationEquals(const Value: Boolean);
begin
  if Value then
    OperationEqualsCombo.ItemIndex := 0
  else
    OperationEqualsCombo.ItemIndex := 1;
end;

procedure TLogFilterForm.setOperationIsRegExpr(const Value: Boolean);
begin
  OperationRegExpCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setOperationValue(const Value: String);
begin
  OperationEdit.Text := Value;
end;

//Correlation
function TLogFilterForm.getCorrelationEquals: Boolean;
begin
  result := (CorrelationEqualsCombo.ItemIndex = 0);
end;

function TLogFilterForm.getCorrelationIsRegExpr: Boolean;
begin
  result := CorrelationRegExpCheckBox.Checked;
end;

function TLogFilterForm.getCorrelationValue: String;
begin
  result := CorrelationEdit.Text;
end;

procedure TLogFilterForm.setCorrelationEquals(const Value: Boolean);
begin
  if Value then
    CorrelationEqualsCombo.ItemIndex := 0
  else
    CorrelationEqualsCombo.ItemIndex := 1;
end;

procedure TLogFilterForm.setCorrelationIsRegExpr(const Value: Boolean);
begin
  CorrelationRegExpCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setCorrelationValue(const Value: String);
begin
  CorrelationEdit.Text := Value;
end;



function TLogFilterForm.getMatchAny: Boolean;
begin
  result := (MatchStyleCombo.ItemIndex = 1);
end;

function TLogFilterForm.getMessageValidationEnabled: Boolean;
begin
  result := ValidationErrorEnabledCheckBox.Checked;
end;

function TLogFilterForm.getReplyValidationEnabled: Boolean;
begin
  result := ReplyValidationCheckBox.Checked;
end;

function TLogFilterForm.getRequestValidationEnabled: Boolean;
begin
  result := RequestValidationCheckBox.Checked;
end;

procedure TLogFilterForm.setMatchAny(const Value: Boolean);
begin
  if Value then
    MatchStyleCombo.ItemIndex := 1
  else
    MatchStyleCombo.ItemIndex := 0;
end;

procedure TLogFilterForm.setMessageValidationEnabled(const Value: Boolean);
begin
  ValidationErrorEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setReplyValidationEnabled(const Value: Boolean);
begin
  ReplyValidationCheckBox.Checked :=Value;
end;

procedure TLogFilterForm.setRequestValidationEnabled(const Value: Boolean);
begin
  RequestValidationCheckBox.Checked := Value;
end;

function TLogFilterForm.getMiMEnabled: Boolean;
begin
  result := MimEnabledCheckBox.Checked;
end;

function TLogFilterForm.getReplyMiMEnabled: Boolean;
begin
  result := ReplyMiMCheckBox.Checked;
end;

function TLogFilterForm.getRequestMiMEnabled: Boolean;
begin
  result := RequestMiMCheckBox.Checked;
end;

procedure TLogFilterForm.setMiMEnabled(const Value: Boolean);
begin
  MimEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setReplyMiMEnabled(const Value: Boolean);
begin
  ReplyMiMCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setRequestMiMEnabled(const Value: Boolean);
begin
  RequestMiMCheckBox.Checked := Value;
end;

function TLogFilterForm.getRemarksEnabled: Boolean;
begin
  result := RemarksCheckBox.Checked;
end;

function TLogFilterForm.getReplyEnabled: Boolean;
begin
  result := ReplyEnabledCheckBox.Checked;
end;

function TLogFilterForm.getReplyEquals: Boolean;
begin
  result := (ReplyEqualsCombobox.ItemIndex = 0);
end;

function TLogFilterForm.getReplyValue: String;
begin
  result := ReplyEdit.Text;
end;

function TLogFilterForm.getRequestEnabled: Boolean;
begin
  result := RequestEnabledCheckBox.Checked;
end;

function TLogFilterForm.getRequestEquals: Boolean;
begin
  result := (RequestEqualsCombobox.ItemIndex = 0);
end;

function TLogFilterForm.getRequestValue: String;
begin
  result := RequestEdit.Text;
end;

procedure TLogFilterForm.setRemarksEnabled(const Value: Boolean);
begin
  RemarksCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setReplyEnabled(const Value: Boolean);
begin
  ReplyEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setReplyEquals(const Value: Boolean);
begin
  if Value then
    ReplyEqualsCombobox.ItemIndex := 0
  else
    ReplyEqualsCombobox.ItemIndex := 1;
end;

procedure TLogFilterForm.setReplyValue(const Value: String);
begin
  ReplyEdit.Text := Value;
end;

procedure TLogFilterForm.setRequestEnabled(const Value: Boolean);
begin
  RequestEnabledCheckBox.Checked := Value;
end;

procedure TLogFilterForm.setRequestEquals(const Value: Boolean);
begin
  if Value then
    RequestEqualsCombobox.ItemIndex := 0
  else
    RequestEqualsCombobox.ItemIndex := 1;
end;

procedure TLogFilterForm.setRequestValue(const Value: String);
begin
  RequestEdit.Text := Value;
end;

function TLogFilterForm.getUnexpectedValuesEnabled: Boolean;
begin
  result := UnexpectedValuesCheckBox.Checked;
end;

procedure TLogFilterForm.setUnexpectedValuesEnabled(const Value: Boolean);
begin
  UnexpectedValuesCheckBox.Checked := Value;
end;

end.
