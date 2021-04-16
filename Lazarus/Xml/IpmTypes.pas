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
