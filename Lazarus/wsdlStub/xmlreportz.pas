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
unit xmlreportz;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, ClaimListz, WsdlProjectz, snapshotz, xmlz;

function xmlRegressionReport (aProject: TWsdlProject; aList: TSnapshotList): TXml;

implementation

uses htmlXmlUtilz
   , logz
   , xmlio
   , wsdlz
   , xmlxsdparser
   ;

function xmlRegressionReport (aProject: TWsdlProject; aList: TSnapshotList): TXml;
  function _Report (aList: TSnapshotList): TXml;
  var
    x: Integer;
    s: String;
  begin
    result := TXml.CreateAsString('regressionReport', '');
    s := 'OK';
    for x := 0 to aList.Count - 1 do with aList.SnapshotItems[x] do
      if Status <> rsOk then
        s := 'NOK';
    result.AddXml (TXml.CreateAsString('result', s));
    with result.AddXml(TXml.CreateAsString('verdicts', '')) do
    begin
      for x := 0 to aList.Count - 1 do with aList.SnapshotItems[x] do
      begin
        with AddXml(TXml.CreateAsString('_', '')) do
        begin
          AddXml (TXml.CreateAsString('name', thisSnaphot.Name));
          AddXml (TXml.CreateAsString('verdict', thisSnaphot.Verdict));
        end;
      end;
    end;
  end;
var
  x: Integer;
begin
  with aList as TSnapshotList do
  begin
    for x := 0 to Count - 1 do with SnapshotItems[x] do
      if (Status = rsUndefined)
      and (not aProject.abortPressed) then
        doReport;
    if not aProject.abortPressed then
      result := _Report (aList as TSnapshotList);
  end;
end;

end.

