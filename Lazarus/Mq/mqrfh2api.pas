unit mqrfh2api;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  mqapi
   , Xmlz
   ;
type
  MQRFH2 = record
    StrucId             : MQCHAR4;
    Version             : MQLONG;
    StrucLength         : MQLONG;
    Encoding            : MQLONG;
    CodedCharSetId      : MQLONG;
    Format              : MQCHAR8;
    Flags               : MQLONG;
    NameValueCCSID      : MQLONG;
  end;
  PMQRFH2 = ^MQRFH2;

const
  MQRFH2_DEFAULT : MQRFH2 = (StrucId: 'RFH ';
                             Version: 2;
                             StrucLength: MQRFH_STRUC_LENGTH_FIXED_2;
                             Encoding: 546;
                             CodedCharSetId: 437;
                             Format: '        ';
                             Flags: 0;
                             NameValueCCSID: 1208;
                          );
type
  MQRFH2VAR = record
    DataLength         : MQLONG;
    Data               : MQCHARSTRING; // placeholder
  end;
  PMQRFH2VAR = ^MQRFH2VAR;


{
const
  MQGMO_DEFAULT : MQGMO = (StrucId:MQGMO_STRUC_ID;
                           Version:MQGMO_VERSION_1;
                           Options:MQGMO_NO_WAIT;
                           WaitInterval:0;
                           Signal1:0;
                           Signal2:0;
                           ResolvedQName:#0;
                           MatchOptions:(MQMO_MATCH_MSG_ID+MQMO_MATCH_CORREL_ID);
                           GroupStatus:MQGS_NOT_IN_GROUP;
                           SegmentStatus: MQSS_NOT_A_SEGMENT;
                           Segmentation:MQSEG_INHIBITED;
                           Reserved1:' ';
                           MsgToken:($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0);
                           ReturnedLength:MQRL_UNDEFINED;
                          );

}

function mqRfh2Result (aBuffer: AnsiString; var aRfhHeader: AnsiString): String;
function mqRfh2HeaderAsTxt (aBuffer: AnsiString): String;
function mqRfh2HeaderAsXml (aBuffer: AnsiString): TXml;
function mqRfh2HeaderFromXml (aXml: TXml): AnsiString;
function mqRfh2ReverseInts (aBuffer: AnsiString; ReversedInteger: Boolean): AnsiString;

type TCompType = record
  case Integer of
   1: (mqLong: MQLONG); // 4 bytes
   2: (Bytes: array [1..4] of Byte);
end;

implementation

uses SysUtils
   , mqinterface
   , Dialogs
   ;

function longMQlong (aInteger: MQLONG; aReversedInteger: Boolean): MQLONG;
var
  x: Integer;
  sComp: TCompType;
  dComp: TCompType;
begin
  if not aReversedInteger then
  begin
    result := aInteger;
    exit;
  end;
  sComp.mqLong := aInteger;
  dComp.Bytes [1] := sComp.Bytes [4];
  dComp.Bytes [2] := sComp.Bytes [3];
  dComp.Bytes [3] := sComp.Bytes [2];
  dComp.Bytes [4] := sComp.Bytes [1];
  result := dComp.mqLong;
end;

function mqRfh2HeaderFromXml (aXml: TXml): AnsiString;
  function _s (sXml: TXml): AnsiString;
  var
    x: Integer;
  begin
    result := '';
    if not sXml.Checked then Exit;
    result := '<' + sXml.Name + '>' + sXml.Value;
    for x := 0 to sXml.Items.Count - 1 do
      result := result + _s(sXml.Items.XmlItems [x]);
    result := result + '</' + sXml.Name + '>';
  end;
var
  x: Integer;
  xXml: TXml;
  xDone: Boolean;
  p: PMQRFH2;
  pv: PMQRFH2VAR;
  l: Integer;
  s: AnsiString;
begin
  result := '';
  l := MQRFH_STRUC_LENGTH_FIXED_2;
  SetLength (result, l);
  p := PMQRFH2 (PMQCHAR (result));
  p^ := MQRFH2_DEFAULT;
  if not Assigned (aXml) then Exit;
  if not aXml.Checked then Exit;
  if aXml.Name <> 'Rfh' then raise Exception.Create('Ilegal XML for Rfh header');
  for x := 0 to aXml.Items.Count - 1 do
  begin
    xXml := aXml.Items.XmlItems [x];
    if xXml.Checked then
    begin
      xDone := False;
      if xXml.Name = 'StrucId' then
      begin
        CopyChars (p.StrucId, xXml.Value, SizeOf (p.StrucId));
        xDone := True;
      end;
      if xXml.Name = 'Version' then
      begin
        p.Version := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if xXml.Name = 'StrucLength' then
      begin
        p.StrucLength := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if xXml.Name = 'Encoding' then
      begin
        p.Encoding := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if xXml.Name = 'CodedCharSetId' then
      begin
        p.CodedCharSetId := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if xXml.Name = 'Format' then
      begin
        CopyChars (p.Format, xXml.Value, SizeOf (p.Format));
        xDone := True;
      end;
      if xXml.Name = 'Flags' then
      begin
        p.Flags := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if xXml.Name = 'NameValueCCSID' then
      begin
        p.NameValueCCSID := StrToIntDef (xXml.Value, 0);
        xDone := True;
      end;
      if (not xDone) then
      begin
        s := _s (xXml);
        if s <> '' then
        begin
          while (Length (s) mod 4 <> 0) do
            s := s + ' ';
          pv := nil; // to avoid compiler warning
          SetLength (result, l + Length (s) + SizeOf (pv.DataLength));
          p := PMQRFH2 (PMQCHAR (result));
          pv := PMQRFH2VAR (PMQCHAR (result) + l);
          pv.DataLength := Length (s);
//          move (s, pv.Data, pv.DataLength);
          CopyChars (pv.Data, s, pv.DataLength);
          l := l + SizeOf (pv.DataLength) + pv.DataLength;
        end;
      end;
    end;
  end;
  p.StrucLength := l;
end;

function mqRfh2HeaderAsXml (aBuffer: AnsiString): TXml;
var
  p: PMQRFH2;
  pv: PMQRFH2VAR;
  o: Integer;
begin
  p := PMQRFH2 (PMQCHAR (aBuffer));
  if p.StrucId <> 'RFH ' then
  begin
    result := TXml.CreateAsString('Rfh_IllegalStruct', Copy (aBuffer, 1, 40));
    exit;
  end;
  if (p.Version <> 1)
  and (p.Version <> 2) then
    raise Exception.Create ( 'mqRfh2HeaderAsXml Unsupported RFH Version: '
                           + IntToStr (p.Version)
                           + ' (Endian)reversed: '
                           + IntToStr (longMQlong(p.Version, True))
                           );

  result := TXml.CreateAsString('Rfh', '');
  try
    with result do
    begin
      AddXml (TXml.CreateAsString('StrucId', p.StrucId));
      AddXml (TXml.CreateAsString('Version', IntToStr(p.Version)));
      AddXml (TXml.CreateAsString('StrucLength', IntToStr(p.StrucLength)));
      AddXml (TXml.CreateAsString('Encoding', IntToStr(p.Encoding)));
      AddXml (TXml.CreateAsString('CodedCharSetId', IntToStr(p.CodedCharSetId)));
      AddXml (TXml.CreateAsString('Format', p.Format));
      AddXml (TXml.CreateAsString('Flags', IntToStr(p.Flags)));
      if (p.Version = 1) then
        o := MQRFH_STRUC_LENGTH_FIXED
      else
      begin
        o := MQRFH_STRUC_LENGTH_FIXED_2;
        AddXml (TXml.CreateAsString('NameValueCCSID', IntToStr(p.NameValueCCSID)));
      end;
      while o < p.StrucLength do
      begin
        pv := PMQRFH2VAR (PMQCHAR (aBuffer) + o);
        with AddXml (Txml.Create) do
        try
          LoadFromString (Copy (aBuffer, o + 5, pv.DataLength), nil);
        except
          TagName := 'parseerror';
          Value := Copy (aBuffer, o + 5, pv.DataLength);
        end;
        o := o + pv.DataLength + SizeOf (pv.DataLength);
      end;
    end;
    result.CheckDownline(True);
  except
    result.Free;
    result := nil;
    raise;
  end;
end;

function mqRfh2HeaderAsTxt (aBuffer: AnsiString): String;
var
  p: PMQRFH2;
  pv: PMQRFH2VAR;
  o: Integer;
begin
  if Copy (aBuffer, 1, 4) <> 'RFH ' then
    raise Exception.Create('Illegal RFH Buffer');
  p := PMQRFH2 (PMQCHAR (aBuffer));
  if p.StrucId <> 'RFH ' then
    raise Exception.Create('Illegal RFH StrucId');
  if (p.Version <> 1)
  and (p.Version <> 2) then
    raise Exception.Create ( 'mqRfh2HeaderAsXml Unsupported RFH Version: '
                           + IntToStr (p.Version)
                           + ' (Endian)reversed: '
                           + IntToStr (longMQlong(p.Version, True))
                           );

  result := 'RfhHeader: ' + #$D#$A
          + '  StrucId: ' + p.StrucId + #$D#$A
          + '  Version: ' + IntToStr (p.Version) + #$D#$A
          + '  StrucLength: ' + IntToStr (p.StrucLength) + #$D#$A
          + '  Encoding: ' + IntToStr (p.Encoding) + #$D#$A
          + '  CodedCharSetId: ' + IntToStr (p.CodedCharSetId) + #$D#$A
          + '  Format: ' + p.Format + #$D#$A
          + '  Flags: ' + IntToStr (p.Flags) + #$D#$A
          ;
  if (p.Version = 1) then
    o := MQRFH_STRUC_LENGTH_FIXED
  else
  begin
    o := MQRFH_STRUC_LENGTH_FIXED_2;
    result := result
            + '  NameValueCCSID: ' + IntToStr (p.NameValueCCSID) + #$D#$A;
  end;
  while o < p.StrucLength do
  begin
    pv := PMQRFH2VAR (PCHAR (aBuffer) + o);
    result := result
            + '  Rfh variable header'
            + #$D#$A
            + '    DataLength: '
            + IntToStr (pv.DataLength)
            + #$D#$A
            + '    '
            + Copy (aBuffer, o + 5, pv.DataLength)
            + #$D#$A
            ;
    o := o + pv.DataLength + 4;
  end;
end;

function mqRfh2Result (aBuffer: AnsiString; var aRfhHeader: AnsiString): String;
var
  p: PMQRFH2;
  pv: PMQRFH2VAR;
  o: Integer;
  xReversedInteger: Boolean;
begin
  aRfhHeader := '';
  xReversedInteger := False;
{}{
  if Copy (aBuffer, 1, 4) <> 'RFH ' then
    raise Exception.Create('Illegal RFH Buffer: ' + aBuffer);
{}
  p := PMQRFH2 (PMQCHAR (aBuffer));
  if p.StrucId <> 'RFH ' then
    raise Exception.Create('Illegal RFH StrucId');
  if (longMQLong (p.Version, True) = 1)
  or (longMQLong (p.Version, True) = 2) then
    xReversedInteger := True;
  if xReversedInteger then
  begin
    p.Version := longMQlong(p.Version, True);
    p.StrucLength := longMQlong(p.StrucLength, True);
    p.Encoding := longMQlong(p.Encoding, True);
    p.CodedCharSetId := longMQlong(p.CodedCharSetId, True);
    p.Flags := longMQlong(p.Flags, True);
    if p.Version = 2 then
      p.NameValueCCSID := longMQlong(p.NameValueCCSID, True);
  end;
  if (p.Version <> 1)
  and (p.Version <> 2) then
    raise Exception.Create('Illegal RFH Version: ' + IntToStr (p.Version));
  aRfhHeader := Copy (aBuffer, 1, p.StrucLength);
  p := PMQRFH2 (PMQCHAR (aRfhHeader));
  if (p.Version = 1) then
    o := MQRFH_STRUC_LENGTH_FIXED
  else
    o := MQRFH_STRUC_LENGTH_FIXED_2;
  while o < p.StrucLength do
  begin
    pv := PMQRFH2VAR (PMQCHAR (aRfhHeader) + o);
    pv.DataLength := longMQlong (pv.DataLength, xReversedInteger);
    o := o + pv.DataLength + SizeOf (pv.DataLength);
  end;
  result := Copy (aBuffer, 1 + (p.StrucLength div SizeOf(aBuffer[1])), Length (aBuffer));
end;

function mqRfh2ReverseInts (aBuffer: AnsiString; ReversedInteger: Boolean): AnsiString;
var
  ps, pd: PMQRFH2;
  pvd: PMQRFH2VAR;
  o: Integer;
begin
  if Copy (aBuffer, 1, 4) <> 'RFH ' then
  begin
    result := aBuffer;
    Exit;
  end;
  result := aBuffer + ' ';
  SetLength (Result, Length (aBuffer));
  if not ReversedInteger then Exit;
  ps := PMQRFH2 (PMQCHAR (aBuffer));
  pd := PMQRFH2 (PMQCHAR (result));
  if ps = pd then
    Raise Exception.Create ('mqRfh2ReverseInts: ps = pd');
  pd.Version := longMQlong(ps.Version, True);
  pd.StrucLength := longMQlong(ps.StrucLength, True);
  pd.Encoding := longMQlong(ps.Encoding, True);
  pd.CodedCharSetId := longMQlong(ps.CodedCharSetId, True);
  pd.Flags := longMQlong(ps.Flags, True);
  if pd.Version = 2 then
    pd.NameValueCCSID := longMQlong(ps.NameValueCCSID, True);
  if (ps.Version <> 1)
  and (ps.Version <> 2) then
    raise Exception.Create('Illegal RFH Version: ' + IntToStr (ps.Version));
  if (ps.Version = 1) then
    o := MQRFH_STRUC_LENGTH_FIXED
  else
    o := MQRFH_STRUC_LENGTH_FIXED_2;
  while o < ps.StrucLength do
  begin
    pvd := PMQRFH2VAR (PCHAR (result) + o);
    o := o + pvd.DataLength + SizeOf (pvd.DataLength);
    pvd.DataLength := longMQlong (pvd.DataLength, True);
  end;
end;

end.

