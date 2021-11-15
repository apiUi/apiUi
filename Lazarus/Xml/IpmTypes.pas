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
unit IpmTypes;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type TIpmHostConnectionType = (ipmHCTcpIp, ipmHCHttp);
type TIpmSendUsing = (ipmSUPathsend, ipmSUMq, ipmSUHttp, ipmSUFile);
type TIpmTandem = (ipmPWServer, ipmPWProcess);
type TIpmSendFormat = (ipmSFFlat, ipmSFXml);
type TIpmDescrType = (ipmDTFreeFormat, ipmDTCobol, ipmDTXml, ipmDTXsd, ipmDTWsdl, ipmDTEmailDepricated, ipmDTSwiftMTDepricated, ipmDTJson);
type TIpmOperatingMode = (ipmOMControlled, ipmOMAttended, ipmOMUnattended);
type TIpmEncoding = (ipmEncNone, ipmEncUrl, ipmEncBase64, ipmEncHex);
type TIpmMqDialog = (mqdlgRequestReply, mqdlgFireAndForget, mqdlgGetOnly);
type TIpmMqConnection = (mqconHosted, mqconLocalServer, mqconLocalClient);
type TIpmMqCharset = (mqcsAscii, mqcsEbcdic);
type TIpmInitReply = (ipmIRNone, ipmIRCopy, ipmIRCallBack);
type TIpmExecuteType = (ipmETCurrent, ipmETTestSet, ipmETOverview);
type TSoapVersion = (svUnspecified, svSOAP11, svSOAP12);



implementation

end.
