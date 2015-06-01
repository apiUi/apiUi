unit StressTestUnit;

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
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez;

type

  { TStressTestForm }

  TStressTestForm = class(TForm)
    CancelButton : TBitBtn ;
    ConcurrentThreadsEdit : TLabeledEdit ;
    LoopsPerThreadEdit : TLabeledEdit ;
    DelayRadioGroup: TRadioGroup;
    DelayMinEdit: TLabeledEdit;
    DelayMaxEdit: TLabeledEdit;
    GroupBox1 : TGroupBox ;
    OkButton : TBitBtn ;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DelayRadioGroupClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getConcurrentThreads: Integer;
    function getLoopsPerThread: Integer;
    function getDelayMsMax: Integer;
    function getDelayMsMin: Integer;
    procedure setConcurrentThreads (const Value: Integer);
    procedure setLoopsPerThread (const Value: Integer);
    procedure setDelayMsMax(const Value: Integer);
    procedure setDelayMsMin(const Value: Integer);
  public
    property ConcurrentThreads: Integer read getConcurrentThreads write setConcurrentThreads;
    property LoopsPerThread: Integer read getLoopsPerThread write setLoopsPerThread;
    property DelayMsMin: Integer read getDelayMsMin write setDelayMsMin;
    property DelayMsMax: Integer read getDelayMsMax write setDelayMsMax;
    { Public declarations }
  end;

var
  StressTestForm: TStressTestForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses Dialogs
   ;

procedure TStressTestForm.Button1Click(Sender: TObject);
var
  x: Integer;
begin
  try
    x := ConcurrentThreads;
    x := LoopsPerThread;
    case DelayRadioGroup.ItemIndex of
      0:
      begin
        DelayMsMin := 0;
        DelayMsMax := 0;
      end;
      1:
      begin
        DelayMsMax := DelayMsMin;
      end;
      2:
      begin
        x := DelayMsMin;
        x := DelayMsMax;
      end;
    end;
    if DelayRadioGroup.ItemIndex = 0 then
    begin
    end;
    if DelayRadioGroup.ItemIndex = 1 then
    begin
      DelayMsMax := DelayMsMin;
    end;
  except
    on e: exception do
    begin
      ModalResult := mrNone;
      ShowMessage (e.Message);
    end;
  end;
end;

procedure TStressTestForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
  try
    ConcurrentThreads := IniFile.IntegerByNameDef['ConcurrentThreads', 5];
    LoopsPerThread := IniFile.IntegerByNameDef['LoopsPerThread', 10];
    DelayMsMin := IniFile.IntegerByNameDef['DelayMin', 100];
    DelayMsMax := IniFile.IntegerByNameDef['DelayMax', 1000];
    DelayRadioGroup.ItemIndex := Inifile.IntegerByNameDef['DelayRadioGroupItem', 2];
  except
  end;
end;

procedure TStressTestForm.FormDestroy(Sender: TObject);
begin
  try IniFile.IntegerByName['ConcurrentThreads'] := ConcurrentThreads; except end;
  try IniFile.IntegerByName['LoopsPerThread'] := LoopsPerThread; except end;
  try IniFile.IntegerByName['DelayMin'] := DelayMsMin; except end;
  try IniFile.IntegerByName['DelayMax'] := DelayMsMax; except end;
  try IniFile.IntegerByName['DelayRadioGroupItem'] := DelayRadioGroup.ItemIndex;  except end;
  IniFile.Save;
  IniFile.Free;
end;

procedure TStressTestForm.FormShow(Sender: TObject);
begin
  if (DelayMsMin = 0)
  and (DelayMsMax = 0) then
    DelayRadioGroup.ItemIndex := 0
  else
    if DelayMsMin = DelayMsMax then
      DelayRadioGroup.ItemIndex := 1
    else
      DelayRadioGroup.ItemIndex := 2;
  DelayRadioGroupClick(nil);
end;

function TStressTestForm .getConcurrentThreads : Integer ;
begin
  result := StrToInt(ConcurrentThreadsEdit.Text);
end;

function TStressTestForm .getLoopsPerThread : Integer ;
begin
  result := StrToInt(LoopsPerThreadEdit.Text);
end;

function TStressTestForm.getDelayMsMax: Integer;
begin
  result := StrToInt(DelayMaxEdit.Text);
end;

function TStressTestForm.getDelayMsMin: Integer;
begin
  result := StrToInt(DelayMinEdit.Text);
end;

procedure TStressTestForm .setConcurrentThreads (const Value : Integer );
begin
  ConcurrentThreadsEdit.Text := IntToStr(Value);
end;

procedure TStressTestForm .setLoopsPerThread (const Value : Integer );
begin
  LoopsPerThreadEdit.Text := IntToStr(Value);
end;

procedure TStressTestForm.DelayRadioGroupClick(Sender: TObject);
begin
  DelayMinEdit.Visible := (DelayRadioGroup.ItemIndex > 0);
  DelayMaxEdit.Visible := (DelayRadioGroup.ItemIndex > 1);
  case DelayRadioGroup.ItemIndex of
    1: DelayMinEdit.EditLabel.Caption := 'Delay(ms):';
    2: DelayMinEdit.EditLabel.Caption := 'Between(ms):';
  end;
end;

procedure TStressTestForm.setDelayMsMax(const Value: Integer);
begin
  DelayMaxEdit.Text := IntToStr(Value);
end;

procedure TStressTestForm.setDelayMsMin(const Value: Integer);
begin
  DelayMinEdit.Text := IntToStr(Value);
end;

end.

