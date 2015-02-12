unit MainFormClient;

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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SyncObjs,
  Dialogs, StdCtrls, ExtCtrls, StompClient, StompTypes, IdSync;

type
  TStompThread = class;
  TFrameProcedure = procedure (arg: IStompFrame) of Object;
  TForm5 = class(TForm)
    Edit1: TEdit;
    PutQEdit: TEdit;
    EnterButton: TButton;
    SenderEdit: TEdit;
    ReceivedMemo: TMemo;
    SendMemo: TMemo;
    SendButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    GetQEdit: TEdit;
    DisconnectButton: TButton;
    ExchangeButton: TButton;
    ClearButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ExchangeButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnterButtonClick(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Thread: TStompThread;
    stomp: IStompClient;
    getqname, putqname: string;
    procedure ReceivedFrame (aFrame: IStompFrame);
  public
    { Public declarations }
  end;

  TStompThread = class(TThread)
  private
    fStompClient: IStompClient;
    fFrame: IStompFrame;
    fFrameProcedure: TFrameProcedure;
    fOnFrame: TFrameProcedure;
    procedure fSynchronised;
  protected
    procedure Execute; override;
  public
    constructor Create (aStompClient: IStompClient; aFrameProcedure: TFrameProcedure);
  end;

var
  Form5: TForm5;

implementation


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
function generateCorreletation: String;
begin
  result := IntToStr (Trunc (10000000 * Random));
end;

{ TStompThread }

constructor TStompThread.Create(aStompClient: IStompClient; aFrameProcedure: TFrameProcedure);
begin
  inherited Create (False);
  fStompClient := aStompClient;
  fFrameProcedure := aFrameProcedure;
  FreeOnTerminate := True;
end;

procedure TStompThread.Execute;
begin
  while not terminated do
  begin
    fFrame := nil;
    fFrame := fStompClient.Receive(500);
    if Assigned (fFrame) then
      with TIdSync.Create do
        try
          SynchronizeMethod (fSynchronised);
        finally
          Free;
        end;
  end;
end;

procedure TStompThread.fSynchronised;
begin
  fFrameProcedure (fFrame);
end;

{ TForm5 }

procedure TForm5.EnterButtonClick(Sender: TObject);
var
  f: IStompFrame;
begin
  getqname := '/queue/' + GetQEdit.Text;
  stomp := TStompClient.Create;
  stomp.Connect(Edit1.Text);
  stomp.Subscribe(getqname, amClient);
  Thread := TStompThread.Create (stomp, ReceivedFrame);
  EnterButton.Enabled := False;
  DisconnectButton.Enabled := True;
  Edit1.Enabled := False;
  GetQEdit.Enabled := False;
  SenderEdit.Enabled := False;
  SendButton.Enabled := True;
  SendMemo.Enabled := True;
  ExchangeButton.Enabled := False;
end;

procedure TForm5.SendButtonClick(Sender: TObject);
begin
  putqname := '/queue/' + PutQEdit.Text;
  stomp.Send(putqname, SendMemo.Lines.Text,
    StompUtils.NewHeaders
      .Add('sender', SenderEdit.Text)
      .Add('tijdstip', formatdatetime('yyyy/mm/dd hh:nn:ss', now))
      .Add('correlation-id', generateCorreletation)
//    .Add('content-length', IntToStr(Length(SendMemo.Lines.Text)))
      .Add(TStompHeaders.NewReplyToHeader(getqname))
      .Add(TStompHeaders.NewPersistentHeader(false))
      );
  SendMemo.Lines.Clear;
end;

procedure TForm5.SendMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 13) and not (ssCtrl in Shift) then
  begin
    key := 0;
    SendButton.Click;
  end;
end;

procedure TForm5.tmrTimer(Sender: TObject);
var
  f: IStompFrame;
  {$IFnDEF FPC}
  fw: FLASHWINFO;
  {$endif}
begin
  f := stomp.Receive;
  if assigned(f) then
  begin
    ReceivedFrame (f);
    {$IFnDEF FPC}
    if (WindowState = wsMinimized) or (Application.ActiveFormHandle <> self.Handle) then
    begin
      fw.cbSize := SizeOf(FLASHWINFO);
      fw.hwnd := self.Handle;
      fw.dwFlags := FLASHW_ALL;
      fw.uCount := 5;
      fw.dwTimeout := 500;
      FlashWindowEx(fw);
    end;
    {$endif}
  end;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  if DisconnectButton.Enabled then
    DisconnectButton.Click;
end;

procedure TForm5.DisconnectButtonClick(Sender: TObject);
begin
  if Assigned (stomp)
  and (stomp.Connected) then
  begin
    Thread.Terminate;
    stomp.Unsubscribe(getqname);
    stomp.Disconnect;
  end;
  EnterButton.Enabled := True;
  DisconnectButton.Enabled := False;
  Edit1.Enabled := True;
  GetQEdit.Enabled := True;
  SenderEdit.Enabled := True;
  SendButton.Enabled := False;
  SendMemo.Enabled := False;
  ExchangeButton.Enabled := True;
end;

procedure TForm5.ExchangeButtonClick(Sender: TObject);
var
  s: String;
begin
  s := GetQEdit.Text;
  GetQEdit.Text := PutQEdit.Text;
  PutQEdit.Text := s;
end;

procedure TForm5.ReceivedFrame(aFrame: IStompFrame);
  function _headers: String;
  var
    x: Integer;
  begin
    result := '';
    for x := 0 to aFrame.GetHeaders.Count - 1 do
    begin
      result := result
              + aFrame.GetHeaders.GetAt(x).Key
              + '='
              + aFrame.GetHeaders.GetAt(x).Value
              + sLineBreak
              ;
    end;
  end;
var
  putqname: String;
begin
  ReceivedMemo.Lines.Add ( '[ ' + aFrame.GetCommand
                         + sLineBreak
                         + _headers
                         + ']'
                         + sLineBreak
                         + aFrame.GetBody
                         );
  putqname := aFrame.GetHeaders.Value('reply-to');
  if (aFrame.GetCommand = 'MESSAGE')
  and (putqname <> '') then
  begin
    stomp.Send(putqname, 'In reply on: ' + aFrame.GetBody,
      StompUtils.NewHeaders
        .Add('correlation-id', aFrame.GetHeaders.Value('correlation-id'))
        .Add(TStompHeaders.NewPersistentHeader(false))
        );
  end;
end;

procedure TForm5.ClearButtonClick(Sender: TObject);
begin
  ReceivedMemo.Clear;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Randomize;
end;

end.
