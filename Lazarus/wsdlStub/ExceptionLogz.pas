unit ExceptionLogz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   , SysUtils
   , StrUtils
   ;

type
  TExceptionLog = class(TOBject)
  private
  protected
  public
    Nr: Integer;
    TimeStamp: TDateTime;
    Text: String;
    constructor Create (aText: String);
  end;

  TExceptionLogList = class (TStringList)
  private
    procedure SeTExceptionLog(Index: integer; const Value: TExceptionLog);
  protected
    function GeTExceptionLog (Index: integer): TExceptionLog;
  public
    property EventItems [Index: integer]: TExceptionLog read GeTExceptionLog write SeTExceptionLog;
    function AddEvent (aEvent: TExceptionLog): TExceptionLog;
    procedure Clear; override;
  end;




implementation

{ TExceptionLogList }

function TExceptionLogList.AddEvent(aEvent: TExceptionLog): TExceptionLog;
begin
  AddObject('', aEvent);
  result := aEvent;
end;

procedure TExceptionLogList.Clear;
begin
  inherited;

end;

function TExceptionLogList.GeTExceptionLog(Index: integer): TExceptionLog;
begin
  result := TExceptionLog (Objects [Index]);
end;

procedure TExceptionLogList.SeTExceptionLog(Index: integer; const Value: TExceptionLog);
begin
  Objects [Index] := Value;
end;

{ TExceptionLog }

constructor TExceptionLog.Create(aText: String);
begin
  TimeStamp := now;
  Text := aText;
end;

end.
