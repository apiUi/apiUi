unit KafkaTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  kafkaclient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    StatusLabel: TLabel;
    TopicEdit: TEdit;
    Button1: TButton;
    BrokerEdit: TEdit;
    Memo1: TMemo;
    procedure ButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonClick(Sender: TObject);
var
  sl: TStringList;
begin
  Cursor := crHourGlass;
  try
    StatusLabel.Caption := '...';
    with TKafkaProducer.Create do
    try
      sl := TStringList.Create;
      sl.Text := Memo1.Lines.Text;
      try
        ConfigureParamList(sl);
        StartBroker(BrokerEdit.Text);
    //  SetDeliveryReportCallback(..);
        CreateProducerTopic(TopicEdit.Text);
        if (Sender as TButton).Tag =1 then
          ProduceMessage('aKey', 'aMessage');
        StatusLabel.Caption := Format ('Version: %s', [VersionStr]);
      except
        on e: exception do StatusLabel.Caption := e.Message;
      end;
    finally
      FreeAndNil(sl);
      Free;
    end;
  finally
    Cursor := crDefault;
  end;
end;

end.

