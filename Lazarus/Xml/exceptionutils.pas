{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
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

