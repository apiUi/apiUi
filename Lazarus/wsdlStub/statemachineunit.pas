unit statemachineunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, xmlio, Xmlz;

type

  { TStateMachineScenario }

  TStateMachineScenario = class (TObject)
  private
    fState: String;
    procedure SetRequiredState(AValue: String);
    procedure SetState(AValue: String);
  public
    Name: String;
    NextStates, RequiredStates: TJBStringList;
    property State: String read fState write SetState;
    property RequiredState: String write SetRequiredState;
    function thisScenario: TStateMachineScenario;
    constructor Create;
    destructor Destroy; Override;
  end;

  TStateMachine = class (TObject)
  private
    fStateMachineLock: TCriticalSection;
    function upsertScenarioByName(Index: String): TStateMachineScenario;
  public
    scenarios: TJBStringList;
    function AsXml: TXml;
    procedure CopyFrom (aSrc: TStateMachine);
    procedure AcquireLock;
    procedure ReleaseLock;
    procedure Reset;
    property ScenarioByName [Index: String]: TStateMachineScenario read upsertScenarioByName;
    procedure Clear;
    function thisStateMacine: TStateMachine;
    constructor Create;
    destructor Destroy; Override;
  end;

implementation

{ TStateMachine }

function TStateMachine.upsertScenarioByName(Index: String): TStateMachineScenario;
var
  f: Integer;
begin
  if scenarios.Find(Index, f) then
    result := scenarios.Objects[f] as TStateMachineScenario
  else
  begin
    Result := TStateMachineScenario.Create;
    Result.Name := Index;
    scenarios.AddObject(Result.Name, Result);
  end;
end;

function TStateMachine.AsXml: TXml;
var
  x, y: Integer;
begin
  result := TXml.CreateAsString('StateMachine', '');
  for x := 0 to scenarios.Count - 1 do with scenarios.Objects[x] as TStateMachineScenario do
  begin
    with result.AddXml(TXml.CreateAsString('scenario', '')) do
    begin
      AddXml(TXml.CreateAsString('name', thisScenario.Name));
      AddXml(TXml.CreateAsString('state', thisScenario.State));
      with AddXml(TXml.CreateAsString('possibleStates', '')) do
      begin
        for y := 0 to thisScenario.NextStates.Count - 1 do
        begin
          AddXml (TXml.CreateAsString('name', NextStates.Strings[y]));
        end;
      end;
    end;
  end;
end;

procedure TStateMachine.CopyFrom(aSrc: TStateMachine);
var
  x, y: Integer;
  s, d: TStateMachineScenario;
begin
  aSrc.AcquireLock;
  try
    Clear;
    for x := 0 to aSrc.scenarios.Count - 1 do
    begin
      s := aSrc.scenarios.Objects[x] as TStateMachineScenario;
      d := ScenarioByName[s.Name];
      for y := 0 to s.RequiredStates.Count - 1 do
        d.RequiredState := s.RequiredStates.Strings[y];
      for y := 0 to s.NextStates.Count - 1 do
        d.State := s.NextStates.Strings[y];
      d.State := s.State;
    end;
  finally
    aSrc.ReleaseLock;
  end;
end;

procedure TStateMachine.AcquireLock;
begin
  fStateMachineLock.Acquire;
end;

procedure TStateMachine.ReleaseLock;
begin
  fStateMachineLock.Release;
end;

procedure TStateMachine.Reset;
begin
  Clear;
end;

procedure TStateMachine.Clear;
var
  x: Integer;
begin
  for x := 0 to scenarios.Count - 1 do
    scenarios.Objects[x].Free;
  scenarios.Clear;
end;

function TStateMachine.thisStateMacine: TStateMachine;
begin
  result := self;
end;

constructor TStateMachine.Create;
begin
  fStateMachineLock := TCriticalSection.Create;
  scenarios := TJBStringList.Create;
  scenarios.Sorted:=True;
end;

destructor TStateMachine.Destroy;
begin
  Clear;
  fStateMachineLock.Free;
  scenarios.Free;
end;

{ TStateMachineScenario }

procedure TStateMachineScenario.SetState(AValue: String);
var
  f: Integer;
begin
  if fState = AValue then Exit;
  fState := AValue;
  NextStates.Add (AValue);
end;

procedure TStateMachineScenario.SetRequiredState(AValue: String);
begin
  RequiredStates.Add (AValue);
end;

function TStateMachineScenario.thisScenario: TStateMachineScenario;
begin
  result := self;
end;

constructor TStateMachineScenario.Create;
begin
  NextStates := TJBStringList.Create;
  NextStates.Sorted := True;
  NextStates.Duplicates := dupIgnore;
  RequiredStates := TJBStringList.Create;
  RequiredStates.Sorted := True;
  RequiredStates.Duplicates := dupIgnore;
  State := 'Started';
end;

destructor TStateMachineScenario.Destroy;
begin
  NextStates.Clear;
  NextStates.Free;
  RequiredStates.Clear;
  RequiredStates.Free;
  inherited Destroy;
end;

end.

