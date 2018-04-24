unit progressunit;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,
  ComCtrls, ProgressInterface;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    procedure setCurrentAction(AValue: String);
    procedure setProgressMax(AValue: Integer);
    procedure setProgressMin(AValue: Integer);
    procedure setProgressPos(AValue: Integer);
  public
    ProgressInterface: TProgressInterface;
    procedure Update;
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.Button1Click(Sender: TObject);
begin
  if Assigned (ProgressInterface)
  and Assigned(ProgressInterface.OnCancel) then
    ProgressInterface.OnCancel (self)
  else
    raise Exception.Create ('?no OnCancel assigned?');
end;

procedure TProgressForm.setProgressMax(AValue: Integer);
begin
  ProgressBar1.Max := AValue;
end;

procedure TProgressForm.setCurrentAction(AValue: String);
begin
  Panel1.Caption := AValue;
end;

procedure TProgressForm.setProgressMin(AValue: Integer);
begin
  ProgressBar1.Min := AValue;
end;

procedure TProgressForm.setProgressPos(AValue: Integer);
begin
  ProgressBar1.Position := AValue;
end;

procedure TProgressForm.Update;
begin
  if Assigned (ProgressInterface) then with ProgressInterface do
  begin
    self.Caption := Caption;
    setCurrentAction(CurrentAction);
    setProgressMax(ProgressMax);
    setProgressMin(ProgressMin);
    setProgressPos(ProgressPos);
    setCurrentAction(CurrentAction);
    setCurrentAction(CurrentAction);
  end;
end;

end.

