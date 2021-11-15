{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit ExceptionLogz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   , SysUtils
   , StrUtils
   , ClaimListz
   ;

type
  TExceptionLog = class(TClaimableObject)
  private
  protected
  public
    TimeStamp: TDateTime;
    Text: String;
    constructor Create (aText: String);
  end;

  TExceptionLogList = class (TClaimableObjectList)
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
  inherited AddObject('', aEvent);
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
