unit progressunit;
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,
  ComCtrls, ProgressInterface;

type
  TProcedure = Procedure of Object;
  { TProgressForm }

  TProgressForm = class(TForm)
    Button1: TButton;
    ActionEdit: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    lMax, lMin,lPos: Integer;
    lCaption, lAction:  String;
    lDoLog: Boolean;
  public
    ProgressInterface: TProgressInterface;
    AcquireLock, ReleaseLock: TProcedure;
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

procedure TProgressForm.Timer1Timer(Sender: TObject);
begin
  AcquireLock;
  try
    lMax := ProgressInterface.ProgressMax;
    lMin := ProgressInterface.ProgressMin;
    lPos := ProgressInterface.ProgressPos;
    lCaption := ProgressInterface.Caption;
    lAction := ProgressInterface.CurrentAction;
    lDoLog := ProgressInterface.doShowProgress;
  finally
    ReleaseLock;
  end;
  if not lDoLog then Close;
  Caption := lCaption;
  ProgressBar1.Max := lMax;
  ProgressBar1.Min := lMin;
  ProgressBar1.Position := lPos;
  ActionEdit.Text := lAction;
end;

end.

