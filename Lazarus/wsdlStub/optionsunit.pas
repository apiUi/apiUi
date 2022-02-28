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
unit optionsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  longOptsArrayType = array [0..9] of String;

const
  helpOpt = 'help';
  portOpt = 'port';
  projectOpt = 'project';
  lstLogOpt = 'lstLog';
  scriptOpt = 'script';
  openSslLocOpt = 'certsFolder';
  terminateOpt = 'terminate';
  trackIOOpt = 'trackIO';
  debugOpt = 'debug';
  contextOpt = 'context';
  longOpts: longOptsArrayType = ( helpOpt
                                , portOpt + ':'
                                , projectOpt + ':'
                                , contextOpt + ':'
                                , lstLogOpt + ':'
                                , scriptOpt + ':'
                                , terminateOpt
                                , openSslLocOpt + ':'
                                , trackIOOpt
                                , debugOpt
                                );
function hasOption (aOption: String): Boolean;
function getOption (aOption: String): String;

implementation
uses StrUtils
   ;

function indexOption (aOption: String): Integer;
var
  x: Integer;
begin
  result := -1;
  for x := 1 to argc - 1 do
    if AnsiStartsStr ('--' + aOption, argv [x]) then
      result := x;
end;

function hasOption (aOption: String): Boolean;
begin
  result := (indexOption(aOption) > 0);
end;

function getOption (aOption: String): String;
  function _extractValue (aString: String): String;  // "--option=value"
  begin
    result := '';
    if Pos ('=', aString) > 0 then
      result := Copy (aString, Pos ('=', aString) + 1, MaxInt);
  end;
begin
  result := '';
  if hasOption(aOption) then
    result := _extractValue(ParamStr (indexOption (aOption)));
end;

end.

