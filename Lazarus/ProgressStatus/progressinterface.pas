{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit ProgressInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TProgressInterface = class
public
  doShowProgress: Boolean;
  doUpdateConsole: Boolean;
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

