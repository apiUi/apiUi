unit MqTestUnit;

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
  Dialogs, mqInterface, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    MqInterface: TMqInterface;
    FireAndForgetButton: TButton;
    MqUseRadioGroup: TRadioGroup;
    RequestReplyButton: TButton;
    GetOnlyButton: TButton;
    Label2: TLabel;
    MQManagerEdit: TEdit;
    MQPutQueueLable: TLabel;
    MQPutEdit: TEdit;
    MQReplyToLabel: TLabel;
    MQReplyToEdit: TEdit;
    MQGetLabel: TLabel;
    MQGetEdit: TEdit;
    MqTimeOutLabel: TLabel;
    MqTimeOutUnitsLabel: TLabel;
    MQTimeOutEdit: TEdit;
    Label1: TLabel;
    RequestEdit: TEdit;
    Label3: TLabel;
    ReplyEdit: TEdit;
    procedure FireAndForgetButtonClick(Sender: TObject);
    procedure MqUseRadioGroupClick(Sender: TObject);
    procedure RequestReplyButtonClick(Sender: TObject);
    procedure GetOnlyButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitMqInterface;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FireAndForgetButtonClick(Sender: TObject);
begin
  InitMqInterface;
  ReplyEdit.Text := '';
  MqInterface.FireAndForget(RequestEdit.Text);
end;

procedure TForm1.MqUseRadioGroupClick(Sender: TObject);
begin
  if MqUseRadioGroup.ItemIndex = 0 then
    MqInterface.Use := mquServer
  else
    MqInterface.Use := mquClient;
end;

procedure TForm1.RequestReplyButtonClick(Sender: TObject);
begin
   InitMqInterface;
   ReplyEdit.Text := MqInterface.RequestReply (RequestEdit.Text);
end;

procedure TForm1.GetOnlyButtonClick(Sender: TObject);
begin
  InitMqInterface;
  ReplyEdit.Text := MqInterface.GetOnly;
end;

procedure TForm1.InitMqInterface;
begin
  MqInterface.Qmanager := MQManagerEdit.Text;
  MqInterface.PutQueue := MQPutEdit.Text;
  MqInterface.GetQueue := MQGetEdit.Text;
  MqInterface.ReplyToQueue := MQReplyToEdit.Text;
  MqInterface.TimeOut := MQTimeOutEdit.Text; 
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if MqInterface.Use = mquServer then
    MqUseRadioGroup.ItemIndex := 0;
  if MqInterface.Use = mquClient then
    MqUseRadioGroup.ItemIndex := 1;
end;

end.
