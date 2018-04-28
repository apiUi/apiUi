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
    CancelButton: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
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

procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  if Assigned (ProgressInterface)
  and Assigned(ProgressInterface.OnCancel) then
    ProgressInterface.OnCancel (self)
  else
    raise Exception.Create ('?no OnCancel assigned?');
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not (    Assigned(ProgressInterface)
                   and ProgressInterface.doShowProgress
                  );
end;

procedure TProgressForm.FormShow(Sender: TObject);
begin
  CancelButton.Enabled := Assigned(ProgressInterface)
                      and Assigned(ProgressInterface.OnCancel)
                          ;
  Timer1Timer(nil); // otherwise first info would not be shown immediately ...
end;

procedure TProgressForm.Timer1Timer(Sender: TObject);
begin
  if Assigned (AcquireLock) then AcquireLock;
  try
    lMax := ProgressInterface.ProgressMax;
    lMin := ProgressInterface.ProgressMin;
    lPos := ProgressInterface.ProgressPos;
    lCaption := ProgressInterface.Caption;
    lAction := ProgressInterface.CurrentAction;
    lDoLog := ProgressInterface.doShowProgress;
  finally
    if Assigned (ReleaseLock) then ReleaseLock;
  end;
  if not lDoLog then Close;
  Caption := lCaption;
  ProgressBar1.Max := lMax;
  ProgressBar1.Min := lMin;
  ProgressBar1.Position := lPos;
  Panel1.Caption := lAction;
end;

end.

