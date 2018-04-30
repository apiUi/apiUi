unit ProgressInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TProgressInterface = class
public
  doShowProgress: Boolean;
  ProgressMax: Integer;
  ProgressMin: Integer;
  ProgressPos: Integer;
  CurrentAction: String;
  ExceptionRaised: Boolean;
  ExceptionMessage: String;
  ExceptionStackTrace: String;
  Caption: String;
  OnCancel: TNotifyEvent;
  OnCanceled: TNotifyEvent;
end;

implementation

end.

