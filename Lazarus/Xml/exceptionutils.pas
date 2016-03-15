unit exceptionUtils ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils ;

function ExceptionStackListString(E: Exception): String;

implementation

function ExceptionStackListString(E: Exception): String;
var
  I: Integer;
  Frames: PPointer;
begin
  result := LineEnding + 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    result := result + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  result := result + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    result := result + LineEnding + BackTraceStrFunc(Frames[I]);
end;

end.

