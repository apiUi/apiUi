unit SwiftUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   , SysUtils
   , Express
   , ParserClasses
   , Xsdz
   , Xmlz
   , Bind
   , Graphics
   , Dialogs
   ;

type

  TStwiftMtStreamer = class (TObject)
  private
    fXml: TXml;
    function getAsText: String;
    function crlf(aString: String): String;
  published
  public
    property AsText: String read getAsText;
    constructor Create (aXml: TXml); Overload;
    destructor Destroy; override;
  end;

  TSwiftMtProps = class (TObject)
  private
    fXsd: TXsd;
    fmaxLength: Integer;
    fmtTag: String;
    fmtSepSuffix: String;
    fmtSeparator: String;
    fmtFinFormat: String;
    fmtPattern: String;
    fminLength: Integer;
    fmtMatchContent: Boolean;
    fmtSepPrefix: String;
    fmaxOccurs: Boolean;
    fminOccurs: Integer;
    fmtMatchUntilPattern: String;
    fmtLookAheadPattern: String;
    fmtIncludeNext: Boolean;
    fmtIncludePrefix: Boolean;
  public
    expansionName: String;
    longName: String;
    property mtTag: String read fmtTag;
    property mtFinFormat: String read fmtFinFormat;
    property mtMatchContent: Boolean read fmtMatchContent;
    property mtMatchUntilPattern: String read fmtMatchUntilPattern;
    property mtLookAheadPattern: String read fmtLookAheadPattern;
    property mtIncludeNext: Boolean read fmtIncludeNext;
    property mtIncludePrefix: Boolean read fmtIncludePrefix;
    property mtSepPrefix: String read fmtSepPrefix;
    property mtSepSuffix: String read fmtSepSuffix;
    property mtSeparator: String read fmtSeparator;
    property mtPattern: String read fmtPattern;
    property minLength: Integer read fminLength;
    property maxLength: Integer read fmaxLength;
    property maxOccurs: Boolean read fmaxOccurs;
    property minOccurs: Integer read fminOccurs;
    function SetValue(aXml: TXml; aValue: String): String;
    constructor Create (aXsd: TXsd); Overload;
    destructor Destroy; override;
  end;

  TSwiftMT = class (TObject)
  private
    fBlock1, fBlock2, fBlock3, fBlock4, fBlock5: String;
    fMtXsd: TXsd;
    function getMtAsString: String;
    function getBlock1AsString: String;
    function getBlock2AsString: String;
    function getBlock3AsString: String;
    function getBlock4AsString: String;
    function getBlock5AsString: String;
    function getAsXml: TXml;
  public
    block1Xsd, block2Xsd, block3Xsd, block4Xsd, block5Xsd: TXsd;
    property AsXml: TXml read getAsXml;
    property MtAsString: String read getMtAsString;
    property Block1AsString: String read getBlock1AsString;
    property Block2AsString: String read getBlock2AsString;
    property Block3AsString: String read getBlock3AsString;
    property Block4AsString: String read getBlock4AsString;
    property Block5AsString: String read getBlock5AsString;
    procedure ParseMtMessage(aString: String);
    constructor Create (aString: String; aXsd: TXsd); Overload;
    destructor Destroy; override;
  end;

var
  _swiftMTXsd: TXsd;
  _swiftMTXsdFileName: String;

implementation

uses RegExpr
   , xmlUtilz
   ;

{ TSwiftMT }

constructor TSwiftMT.Create(aString: String; aXsd: TXsd);
begin
  if not Assigned (aXsd) then
    raise Exception.Create('TSwiftMT.Create(aMtMsgAsString: String; aXsd: TXsd): aXsd is null');
  inherited Create;
  fMtXsd := aXsd;
  ParseMtMessage(aString);
end;

destructor TSwiftMT.Destroy;
begin
  inherited;
end;

function TSwiftMT.getAsXml: TXml;
  function _CandidatesAsText(aXml: TXml): String;
  var
    x: Integer;
    sep: String;
  begin
    result := '';
    sep := '';
    for x := 0 to aXml.Items.Count - 1 do
    begin
      result := result + sep + aXml.Items.XmlItems[x].Name;
      sep := ', ';
    end;
  end;
  procedure _createNextOccurrence (aRoot, aXml: TXml);
  begin
    if (LowerCase (aXml.Xsd.maxOccurs) = 'unbounded')
    or (    (StrToInt (aXml.Xsd.maxOccurs) > 1)
        and (aXml.IndexOfRepeatableItem < StrToInt (aXml.Xsd.maxOccurs) - 1)
       ) then
    begin
      if aXml.Tag = 0 then
      begin
        aXml.Tag := 1;
        xmlUtil.Add (aXml).Checked := False;
      end;
    end
    else
    begin
      if (aRoot <> aXml)
      and Assigned (aXml.Parent) then
        _createNextOccurrence (aRoot, aXml.Parent as TXml);
    end;
  end;
  procedure _evaluateField (aXml: TXml; aValue: String);
  var
    xValue: String;
  begin
    if not Assigned (aXml.Xsd)
    or not Assigned (aXml.Xsd.Obj)
    or not (aXml.Xsd.Obj is TSwiftMtProps) then
      raise Exception.Create( '_evaluateField - parse error in: '
                            + aValue
                            + CRLF
                            + 'not properly prepared: '
                            + aXml.Name
                            );
    xValue := aValue;
    if (Length (xValue) > 1)
    and (Copy (xValue, 1 + Length (xValue) - 2, 2) = CRLF) then
      xValue := Copy (xValue, 1, Length (xValue) - 2);
    with aXml.Xsd.Obj as TSwiftMtProps do
    begin
      xValue :=  SetValue (aXml, xValue);
      if xValue <> '' then
        raise Exception.Create( '_evaluateField - parse error in: '
                              + aValue
                              + CRLF
                              + 'not parsed: '
                              + xValue
                              );
    end;
  end;
  function _removeTerminatingCrLf (aString: String): String;
  begin
    if (Length (aString) > 1)
    and (Copy (aString, Length (aString) - 1, 2) = CRLF) then
      result := Copy (aString, 1, Length (aString) - 2)
    else
      result := aString;
  end;
  function _findXmlTag (aXml: TXml; aTag, aValue: String; var xpctdXml: TXml): TXml;
  var
    xContent, xTagValue: String;
    x: Integer;
  begin
    result := nil;
    if aXml.Items.Count > 0 then
    begin
      x := 0;
      while (not Assigned (result))
      and (x < aXml.Items.Count) do
      begin
        if (not aXml.Items.XmlItems[x].isProcessed) then
        begin
          result := _findXmlTag (aXml.Items.XmlItems[x], aTag, _removeTerminatingCrLf(aValue), xpctdXml);
          if not Assigned (result) then
          begin
            aXml.Items.XmlItems[x].isProcessed := True;
            with aXml.Items.XmlItems[x].Xsd.Obj as TSwiftMtProps do
              if (aXml.Items.XmlItems[x].IndexOfRepeatableItem < fminOccurs)
              and not aXml.Items.XmlItems[x].Checked
              and (aXml.TypeDef.ContentModel <> 'Choice') then   // then take a look in next group
                Inc (x, 10000)
              else
                xpctdXml := nil;
          end;
        end;
        Inc (x);
      end;
    end;
    if not aXml.Checked then
    begin
      if Assigned (aXml.Xsd)
      and Assigned (aXml.Xsd.Obj) then with aXml.Xsd.Obj as TSwiftMtProps do
      begin
        if mtTag <> '' then
        begin
          xContent := '';
          if mtMatchContent
          or (aXml.TypeDef.Enumerations.Count = 1) then
          begin
            try
              xContent := aXml.TypeDef.Enumerations.Strings[0];
            except
              on e: Exception do
                raise Exception.Create('Expected to find an enumeration at ' + CRLF + e.Message);
            end;
          end;
          if (mtTag = aTag)
          and (   (xContent = '')
               or (xContent = aValue)
              ) then
            result := aXml
          else
            xpctdXml := aXml;
        end;
      end;
    end;
  end;
  function _unexpectedTagMessage (aTag, aValue: STring; aXpctdXml: TXml): String;
    function _enums: String;
    var
      x: Integer;
      s: String;
    begin
      result := '';
      with aXpctdXml.TypeDef.Enumerations do
      if Count > 0 then
      begin
        s := ' [';
        for x := 0 to Count - 1 do
        begin
          result := result + s + Strings[x];
          s:= ', ';
        end;
        result := result + ']';
      end;
    end;
  begin
    result := 'Unexpected tag in message: ' + aTag + ' ' + _removeTerminatingCrLf (aValue);
    if Assigned (aXpctdXml) then with aXpctdXml.Xsd.Obj as TSwiftMtProps do
      result := result + CRLF + 'Expected: ' + mtTag + _enums;
  end;
  procedure _evaluateBlock35(aRoot: TXml; aBlock: String);
  var
    Sccs: Boolean;
    xTag, xValue: String;
    p: Integer;
    fXml, xpctdXml: TXml;
  begin
    try
    //example: {3:{103:EBA}{108:1320929948356871}}
      with TRegExpr.Create do
      try
        Expression := '\{[^:\{]+:[^\}]*';
        Sccs := Exec(aBlock);
        while Sccs do
        begin
          //{103:EBA}
          p := Pos (':', Match[0]);
          xTag := Copy (Match[0], 2, p - 2);
          xValue := Copy (Match[0], p + 1, 30000);
          fXml := _FindXmlTag (aRoot, xTag, _removeTerminatingCrLf (xValue), xpctdXml);
          if not Assigned (fXml) then
            raise Exception.Create(_unexpectedTagMessage(xTag, xValue, xpctdXml));
          _evaluateField (fXml, xValue);
          sccs := ExecNext;
        end;
      finally
        free;
      end;
    finally
    end;
  end;
  procedure _evaluateBlock4(aRoot: TXml);
  var
    xTag, xValue: String;
    s, e: Integer;
    fXml, xpctdXml: TXml;
  begin
    try
      with TRegExpr.Create do
      try
        Expression := ':[0-9A-Z]{1,4}:';
        if Exec(fBlock4) then
        begin
          s := MatchPos[0];
          xTag := Copy (Match[0], 2, Length (Match[0]) - 2);
          while ExecNext do
          begin
            e := MatchPos[0];
            xValue := Copy (fBlock4, s + Length (xTag) + 2, e - s - Length (xTag) - 2);
            fXml := _FindXmlTag (aRoot, xTag, _removeTerminatingCrLf(xValue), xpctdXml);
            if not Assigned (fXml) then
              raise Exception.Create(_unexpectedTagMessage(xTag, xValue, xpctdXml));
            _createNextOccurrence(aRoot, fXml);
            _evaluateField (fXml, xValue);
            s := e;
            xTag := Copy (Match[0], 2, Length (Match[0]) - 2);
          end;
          xValue := Copy (fBlock4, s + Length (xTag) + 2, 100000);
          fXml := _FindXmlTag (aRoot, xTag, _removeTerminatingCrLf(xValue), xpctdXml);
          if not Assigned (fXml) then
            raise Exception.Create(_unexpectedTagMessage(xTag, xValue, xpctdXml));
          _evaluateField (fXml, xValue);
        end;
      finally
        free;
      end;
    except
      raise;
    end;
  end;
var
  x: Integer;
  xmlCursor, b3Xml, b4Xml, b5Xml: TXml;
begin
  result := TXml.Create (-1000, fMtXsd);
  try
    xmlCursor := nil;
    b3Xml := result.Items.XmlItemByTag ['Block3'];
    if not Assigned (b3Xml) then
      raise Exception.Create('TSwiftMT.getAsXml: No Block3 found');
    if not Assigned (b3Xml.Xsd.Obj)  then
      b3Xml.Xsd.Obj := TSwiftMtProps.Create (b3Xml.Xsd);
    b4Xml := result.Items.XmlItemByTag ['Block4'];
    if not Assigned (b4Xml) then
      raise Exception.Create('TSwiftMT.getAsXml: No Block4 found');
    b5Xml := result.Items.XmlItemByTag ['Block5'];
    if not Assigned (b5Xml) then
      raise Exception.Create('TSwiftMT.getAsXml: No Block5 found');
    if not Assigned (b5Xml.Xsd.Obj)  then
      b5Xml.Xsd.Obj := TSwiftMtProps.Create (b5Xml.Xsd);
    if fBlock1 <> '' then
    begin
      with result.Items.XmlItemByTag['Block1'] do
      begin
      // F01RABOBANKSUTR0000000000
      // ^^ ^           ^   ^     ^<
      // 123456789012345678901234567890
      // 000000000100000000020000000003
        with Items.XmlItemByTag['ApplicationIdentifier'] do
        begin
          Value := Copy (fBlock1, 1, 1);
          Checked := True;
        end;
        with Items.XmlItemByTag['ServiceIdentifier'] do
        begin
          Value := Copy (fBlock1, 2, 2);
          Checked := True;
        end;
        with Items.XmlItemByTag['LogicalTerminalAddress'] do
        begin
          Value := Copy (fBlock1, 4, 12);
          Checked := True;
        end;
        with Items.XmlItemByTag['SessionNumber'] do
        begin
          Value := Copy (fBlock1, 16, 4);
          Checked := True;
        end;
        with Items.XmlItemByTag['SequenceNumber'] do
        begin
          Value := Copy (fBlock1, 20, 6);
          Checked := True;
        end;
      end;
    end;
    with result.Items.XmlItemByTag['Block2'] do
    begin
    // I202CITIUS33XXXXN}
    // ^^  ^           ^   ^     ^<
    // 123456789012345678901234567890
    // 000000000100000000020000000003
      for x := 0 to b4Xml.Items.Count - 1 do
        if b4Xml.Items.XmlItems[x].Name = ('MT' + Copy (fBlock2, 2, 3)) then
          xmlCursor := b4Xml.Items.XmlItems[x];
      if not Assigned (xmlCursor) then
      begin
        raise Exception.CreateFmt( 'Exception while parsing SwiftMT message: %sMT%s not among [%s].%sAdjusting the Service Description File may solve this issue'
                                 , [CRLF, Copy (fBlock2, 2, 3), _CandidatesAsText(b4Xml), CRLF]
                                 );
      end;
      if Copy (fBlock2, 1, 1) = 'I' then
      begin
        with Items.XmlItemByTag['Input'] do
        begin
          with Items.XmlItemByTag['InputIdentifier'] do
          begin
            Value := Copy (fBlock2, 1, 1);
            Checked := True;
          end;
          with Items.XmlItemByTag['MessageType'] do
          begin
            Value := Copy (fBlock2, 2, 3);
            Checked := True;
          end;
          with Items.XmlItemByTag['DestinationAddress'] do
          begin
            Value := Copy (fBlock2, 5, 12);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['MessagePriority'] do
          begin
            Value := Copy (fBlock2, 17, 1);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['DeliveryMonitoring'] do
          begin
            Value := Copy (fBlock2, 18, 1);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['ObsolescencePeriod'] do
          begin
            Value := Copy (fBlock2, 19, 3);
            Checked := (Value <> '');
          end;
        end;
      end;
      if Copy (fBlock2, 1, 1) = 'O' then
      begin
        with Items.XmlItemByTag['Output'] do
        begin
          with Items.XmlItemByTag['OutputIdentifier'] do
          begin
            Value := Copy (fBlock2, 1, 1);
            Checked := True;
          end;
          with Items.XmlItemByTag['MessageType'] do
          begin
            Value := Copy (fBlock2, 2, 3);
            Checked := True;
          end;
          with Items.XmlItemByTag['InputTime'] do
          begin
            Value := Copy (fBlock2, 5, 4);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['MessageInputReference'] do
          begin
            with Items.XmlItemByTag['Date'] do
            begin
              Value := Copy (fBlock2, 9, 6);
              Checked := (Value <> '');
            end;
            with Items.XmlItemByTag['LTIdentifier'] do
            begin
              Value := Copy (fBlock2, 15, 9);
              Checked := (Value <> '');
            end;
            with Items.XmlItemByTag['BranchCode'] do
            begin
              Value := Copy (fBlock2, 24, 3);
              Checked := (Value <> '');
            end;
            with Items.XmlItemByTag['SessionNumber'] do
            begin
              Value := Copy (fBlock2, 27, 4);
              Checked := (Value <> '');
            end;
            with Items.XmlItemByTag['ISN'] do
            begin
              Value := Copy (fBlock2, 31, 6);
              Checked := (Value <> '');
            end;
          end;
          with Items.XmlItemByTag['Date'] do
          begin
            Value := Copy (fBlock2, 37, 6);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['Time'] do
          begin
            Value := Copy (fBlock2, 43, 4);
            Checked := (Value <> '');
          end;
          with Items.XmlItemByTag['MessagePriority'] do
          begin
            Value := Copy (fBlock2, 47, 1);
            Checked := (Value <> '');
          end;
        end;
      end;
    end;
    if fBlock3 <> '' then
      _evaluateBlock35(b3Xml, fBlock3);
    if fBlock4 <> '' then
      _evaluateBlock4 (xmlCursor);
    if fBlock5 <> '' then
      _evaluateBlock35(b5Xml, fBlock5);
  except
    on e: Exception do
    begin
      raise Exception.Create ( 'Exception: '
                             + e.Message
                             + CRLF
                             + 'parsed so far:'
                             + CRLF
                             + result.AsText(False,0,True,False)
                             );

    end;
  end;
end;

function TSwiftMT.getBlock1AsString: String;
begin
  result := fBlock1;
end;

function TSwiftMT.getBlock2AsString: String;
begin
  result := fBlock2;
end;

function TSwiftMT.getBlock3AsString: String;
begin
  result := fBlock3;
end;

function TSwiftMT.getBlock4AsString: String;
begin
  result := fBlock4;
end;

function TSwiftMT.getBlock5AsString: String;
begin
  result := fBlock5;
end;

function TSwiftMT.getMtAsString: String;
begin
  result := '{1:' + getBlock1AsString + '}'
          + '{2:' + getBlock2AsString + '}'
          + '{3:' + getBlock3AsString + '}'
          + '{4:' + getBlock4AsString + CRLF + '-}'
          + '{5:' + getBlock5AsString + '}'
          ;
end;

procedure TSwiftMT.ParseMtMessage(aString: String);
begin
  with TRegExpr.Create do
  try
    Expression := '\{1\:[^\}]*\}';
    if Exec (aString) then
      fBlock1 := Copy (Match[0], 4, Length (Match[0]) - 4);
    Expression := '\{2\:[^\}]*\}';
    if Exec (aString) then
      fBlock2 := Copy (Match[0], 4, Length (Match[0]) - 4);
    Expression := '\{3\:(\{[^\}]*\})*\}';
    if Exec (aString) then
      fBlock3 := Copy (Match[0], 4, Length (Match[0]) - 4);
    Expression := '\{4\:.*\n\-\}';
    if Exec (aString) then
      fBlock4 := Copy (Match[0], 4, Length (Match[0]) - 5);
    Expression := '\{5\:[^\}]*\}';
    if Exec (aString) then
      fBlock5 := Copy (Match[0], 4, Length (Match[0]) - 4);
  finally
    Free;
  end;
end;

{ TSwiftMtProps }

constructor TSwiftMtProps.Create(aXsd: TXsd);
  procedure _evaluateAppInfo (aAppInfo: String);
    function _getAttribValue (aXml: TXml; aKey, aDefault: String): String;
    var
      xXml: TXml;
    begin
      result := aDefault;
      xXml := aXml.Items.XmlItemByTag[aKey];
      if Assigned (xXml) then
        result := xXml.Attributes.ValueByTag['value'];
    end;
  var
    appinfXml: TXml;
  begin
    appinfXml := TXml.Create;
    try
      appinfXml.LoadFromString(aAppInfo, nil);
      if appinfXml.Name = '' then
        appinfXml.LoadFromString(aXsd.sType.Appinfo.Text, nil);
      if appinfXml.Name <> '' then
      begin
        fmtTag := _getAttribValue (appinfXml, 'Tag', fmtTag);
        fmtSepSuffix := _getAttribValue (appinfXml, 'SepSuffix', fmtSepSuffix);
        if (fmtSepSuffix = '&#xA;')
        or (fmtSepSuffix = #$A) then // ?? should have been '&#xD;&#xA;'
          fmtSepSuffix := CRLF;
        fmtSeparator := _getAttribValue (appinfXml, 'Separator', fmtSeparator);
        if (fmtSeparator = '&#xA;') // ?? should have been '&#xD;&#xA;'
        or (fmtSeparator = #$A) then // ?? should have been '&#xD;&#xA;'
          fmtSeparator := CRLF;
        fmtFinFormat := _getAttribValue (appinfXml, 'FinFormat', fmtFinFormat);
        fmtMatchContent := fmtMatchContent
                        or (_getAttribValue (appinfXml, 'MatchContent', '') = 'true');
        fmtSepPrefix := _getAttribValue (appinfXml, 'SepPrefix', fmtSepPrefix);
        if (fmtSepPrefix = '&#xA;') // ?? should have been '&#xD;&#xA;'
        or (fmtSepPrefix = #$A) then // ?? should have been '&#xD;&#xA;'
          fmtSepPrefix := CRLF;
        fmtMatchUntilPattern := _getAttribValue (appinfXml, 'MatchUntilPattern', fmtMatchUntilPattern);
        if (fmtMatchUntilPattern = '&#xA;') // ?? should have been '&#xD;&#xA;'
        or (fmtMatchUntilPattern = #$A) then // ?? should have been '&#xD;&#xA;'
          fmtMatchUntilPattern := CRLF;
        fmtLookAheadPattern := _getAttribValue (appinfXml, 'LookAheadPattern', fmtLookAheadPattern);
        if (fmtLookAheadPattern = '&#xA;') // ?? should have been '&#xD;&#xA;'
        or (fmtLookAheadPattern = #$A) then // ?? should have been '&#xD;&#xA;'
          fmtLookAheadPattern := CRLF;
        fmtIncludeNext := fmtIncludeNext
                      or (_getAttribValue (appinfXml, 'IncludeNext', '') = 'true');
        fmtIncludePrefix := fmtIncludePrefix
                      or (_getAttribValue (appinfXml, 'IncludePrefix', '') = 'true');
      end;
    finally
      appInfXml.Free;
    end;
  end;
var
  x: Integer;
begin
  if Assigned (aXsd.Obj) then
    raise Exception.Create('TSwiftMtProps.Create(aXsd: TXsd) : Recurisive???'); // recursive...
  fXsd := aXsd;
  aXsd.Obj := Self;
  fminLength := StrToIntDef (aXsd.sType.MinLength, 0);
  fmaxLength := StrToIntDef (aXsd.sType.MaxLength, 2000);
  fminOccurs := StrToIntDef (aXsd.minOccurs, 1);
  fMaxOccurs := (aXsd.minOccurs = 'unbounded')
             or (StrToIntDef (aXsd.maxOccurs, 1) > 1)
              ;
  aXsd.sType.Pattern := StringReplace(aXsd.sType.Pattern, #$A, #$D#$A, [rfReplaceAll]);
  aXsd.sType.Pattern := StringReplace(aXsd.sType.Pattern, #$D#$D#$A, #$D#$A, [rfReplaceAll]);
  fmtPattern:= aXsd.sType.Pattern;
  fmtTag := '';
  fmtSepSuffix := '';
  fmtSeparator:= '';
  fmtFinFormat:= '';
  fmtMatchContent := False;
  fmtSepPrefix:= '';
  _evaluateAppInfo (aXsd.Appinfo.Text);
  _evaluateAppInfo (aXsd.sType.Appinfo.Text);
{}{
  if (not aXsd.sType.isMaxLengthAdjusted)
  and (fmtFinFormat <> '')  then
  begin
    with TRegExpr.Create do
    try
      Expression := '^[0-9]+\*';
      if Exec(fmtFinFormat) then
      begin
        xMaxOccurs := StrToInt (Copy (Match[0], 1, Length (Match[0]) - 1));
        xMaxLength := StrToInt (aXsd.sType.MaxLength) - (xMaxOccurs - 1) * Length (CRLF);
        aXsd.sType.MaxLength := IntToStr (xMaxLength div xMaxOccurs);
        aXsd.sType.isMaxLengthAdjusted := True;
        aXsd.sType.MaxOccursAdjusted := xMaxOccurs
      end;
    finally
      Free;
    end;
  end;
  if aXsd.sType.isMaxLengthAdjusted then
  begin
    aXsd.maxOccurs := IntToStr (aXsd.sType.MaxOccursAdjusted);
    fmaxOccurs := True;
  end;
{}
  for x := 0 to aXsd.sType.ElementDefs.Count - 1 do
    if not Assigned (aXsd.sType.ElementDefs.Xsds[x].Obj) then
      aXsd.sType.ElementDefs.Xsds[x].Obj := TSwiftMtProps.Create (aXsd.sType.ElementDefs.Xsds[x]);
end;

destructor TSwiftMtProps.Destroy;
begin
  inherited;
end;

function TSwiftMtProps.setValue(aXml: TXml; aValue: String): String;
var
  x, len: Integer;
  xValue, s: String;
  xMessage: String;
  xOk: Boolean;
  xSeparatorLength: Integer;
  nXml: TXml;
begin
  result := aValue;
  xValue := aValue;
  if aXml.Items.Count = 0 then
  begin
    if (fmtSepPrefix <> '') then
    begin
      if (Pos(fmtSepPrefix, xValue) <> 1) then
        exit;
      xValue := Copy (xValue, 1 + Length (fmtSepPrefix), 1000);
    end;
    if (fmtSepSuffix <> '') then
    begin
      if (Pos(fmtSepSuffix, xValue) < 1) then
        exit;
      xValue := Copy (xValue, 1, Pos(fmtSepSuffix, xValue) - 1);
      if aXml.TypeDef.IsValidValue (aXml.Name, xValue, xMessage) then
      begin
        aXml.Value := xValue;
        aXml.Checked := True;
        result := Copy ( aValue
                       , 1
                       + Length (fmtSepPrefix)
                       + Length (xValue)
                       + Length (fmtSepSuffix)
                       , 1000
                       );
      end;
      exit;
    end;
    if (fmtSeparator <> '') then
    begin
      if (Pos(fmtSeparator, xValue) > 0) then
      begin
        xValue := Copy (xValue, 1, Pos(fmtSeparator, xValue) - 1);
        xSeparatorLength := Length (fmtSeparator);
      end
      else
        xSeparatorLength := 0;
      if aXml.TypeDef.IsValidValue (aXml.Name, xValue, xMessage) then
      begin
        aXml.Value := xValue;
        aXml.Checked := True;
        result := Copy ( aValue
                       , 1
                       + Length (fmtSepPrefix)
                       + Length (xValue)
                       + xSeparatorLength
                       , 1000
                       );
      end;
      exit;
    end;
{}{
    len := pos(CRLF, xValue) - 1; // fields sometimes have repeating freetext ...separated by CRLF
    if len < 0 then
{}
      len := Length (xValue);  // if not found at most length
    if fmaxLength < len then
      len := fmaxLength;  // from schema
{slow but safe}{
    xOk := aXml.TypeDef.IsValidValue (aXml.Name, Copy (xValue, 1, len), xMessage);
    while (not xOk)
      and (len > fminLength) do
    begin
      Dec (len);
      xOk := aXml.TypeDef.IsValidValue (aXml.Name, Copy (xValue, 1, len), xMessage);
    end;
{}
    if aXml.TypeDef.Pattern <> '' then
    begin
      with TRegExpr.Create do
      try
//        Expression :=  StringReplace(aXml.TypeDef.Pattern, '\n', '(@@)', [rfReplaceAll]);
//        s := StringReplace(Copy (xValue, 1, len), #$D#$A, '@@', [rfReplaceAll]);
        Expression :=  aXml.TypeDef.Pattern;
        s := Copy (xValue, 1, len);
        xOk := Exec (s)
           and (MatchPos[0] = 1)
           and (MatchLen[0] >= fMinLength);
        if xOk then
          Len := MatchLen[0];
      finally
        Free;
      end;
    end;
{}
    if (aXml.TypeDef.Pattern = '')
    or xOk then
    begin
      aXml.Value := Copy (xValue, 1, len);
      aXml.Checked := True;
      result := Copy ( xValue
                     , 1 + len
                     , 1000
                     );
      if fmaxOccurs
      and (result <> '') then
      begin
        nXml := xmlUtil.Add (aXml);
        nXml.Checked := False;
        if Copy (result, 1, 2) = CRLF then
          result := Copy (result, 3, 1000);
        result := SetValue (nXml, result);
      end;
    end;
    exit
  end;
  x := 0;
  while x < aXml.Items.Count do // do not change to 'for x :=' because items may be added in the meantime
  begin
    with aXml.Items.XmlItems[x].Xsd.Obj as TSwiftMtProps do
      result := SetValue (aXml.Items.XmlItems[x], result);
    Inc (x);
  end;
end;

{ TStwiftMtStreamer }

constructor TStwiftMtStreamer.Create(aXml: TXml);
begin
  inherited Create;
  fXml := aXml;
end;

function TStwiftMtStreamer.crlf(aString: String): String;
begin
  if (Copy (aString, Length (aString) - 1, 2) <> Bind.CRLF) then
    result := Bind.CRLF
  else
    result := '';
end;

destructor TStwiftMtStreamer.Destroy;
begin

  inherited;
end;

function TStwiftMtStreamer.getAsText: String;
  function _SBlock4 (aXml: TXml; var pXsd: TXsd): String;
  var
    x: Integer;
  begin
    result := '';
    if not aXml.Checked then
      Exit;
    with aXml.Xsd.Obj as TSwiftMtProps do
    begin
      if (mtTag <> '')
      and (   {}{(not aXml.TypeDef.isMaxLengthAdjusted)
           or {}(pXsd <> aXml.Xsd)
          ) then
        result := result + crlf(result) + ':' + mtTag + ':';
      if aXml.Items.Count = 0 then
      begin
        if pXsd = aXml.Xsd then
          result := result + crlf(result);
        result := result + fmtSepPrefix + aXml.Value + fmtSepSuffix + fmtSeparator;
      end;
    end;
    pXsd := aXml.Xsd;
    for x := 0 to aXml.Items.Count - 1 do
      result := result + _SBlock4 (aXml.Items.XmlItems[x], pXsd);
  end;
  function _StreamBlock4 (aXml: TXml): String;
  var
    pXsd: TXsd;
    x: Integer;
  begin
    result := '';
    if not Assigned (aXml) then
      exit;
    pXsd := nil;
    result := '{4:';
    for x := 0 to aXml.Items.Count - 1 do
      if aXml.Items.XmlItems[x].Checked then
        result := result + _SBlock4 (aXml.Items.XmlItems[x], pXsd);
    result := result + crlf(result) + '-}';
  end;
  function _StreamBlock1 (aXml: TXml): String;
  begin
    result := '';
    if not Assigned (aXml) then
      exit;
    result := '{1:'
            + aXml.Items.XmlValueByTag['ApplicationIdentifier']
            + aXml.Items.XmlValueByTag['ServiceIdentifier']
            + aXml.Items.XmlValueByTag['LogicalTerminalAddress']
            + aXml.Items.XmlValueByTag['SessionNumber']
            + aXml.Items.XmlValueByTag['SequenceNumber']
            + '}'
            ;
  end;
  function _StreamBlock2 (aXml: TXml): String;
  var
    iXml, oXml: TXml;
  begin
    result := '';
    if not Assigned (aXml) then
      exit;
    iXml := aXml.Items.XmlCheckedItemByTag['Input'];
    oXml := aXml.Items.XmlCheckedItemByTag['Output'];
    if (not Assigned (iXml))
    and (not Assigned (oXml)) then
      raise Exception.Create('Input/Output not clear:' + aXml.Text);
    result := '';
    if Assigned (iXml) then
      result := '{2:'
              + iXml.Items.XmlValueByTag['InputIdentifier']
              + iXml.Items.XmlValueByTag['MessageType']
              + iXml.Items.XmlValueByTag['DestinationAddress']
              + iXml.Items.XmlValueByTag['MessagePriority']
              + iXml.Items.XmlValueByTag['DeliveryMonitoring']
              + iXml.Items.XmlValueByTag['ObsolescencePeriod']
              + '}'
              ;
    if Assigned (oXml) then
    begin
      result := '{2:'
              + oXml.Items.XmlValueByTag['OutputIdentifier']
              + oXml.Items.XmlValueByTag['MessageType']
              + oXml.Items.XmlValueByTag['InputTime']
              ;
      with oXml.Items.XmlItemByTag['MessageInputReference'] do
        result := result
                + Items.XmlValueByTag['Date']
                + Items.XmlValueByTag['LTIdentifier']
                + Items.XmlValueByTag['BranchCode']
                + Items.XmlValueByTag['SessionNumber']
                + Items.XmlValueByTag['ISN']
              ;
      result := result
              + oXml.Items.XmlValueByTag['Date']
              + oXml.Items.XmlValueByTag['Time']
              + oXml.Items.XmlValueByTag['MessagePriority']
              + '}'
    end;
              ;
  end;
  function _sBlock35 (aXml: TXml): String;
  var
    x: Integer;
    bTag, eTag: String;
  begin
    result := '';
    if not Assigned (aXml)
    or not aXml.Checked then
      exit;
    bTag := '';
    eTag := '';
    if Assigned (aXml.Xsd.Obj) then
    with aXml.Xsd.Obj as TSwiftMtProps do
    begin
      if mtTag <> '' then
      begin
        bTag := '{' + mtTag + ':';
        eTag := '}';
      end;
    end;
    result := bTag + aXml.Value;
    for x := 0 to aXml.Items.Count - 1 do
      result := result + _sBlock35 (aXml.Items.XmlItems[x]);
    result := result + eTag;
  end;
  function _StreamBlock35 (aXml: TXml): String;
  begin
    result := '';
    if not Assigned (aXml)
    or not aXml.Checked then
      exit;
    if not Assigned (aXml.Xsd.Obj) then
      aXml.Xsd.Obj := TSwiftMtProps.Create(aXml.Xsd);
    result := _sBlock35 (aXml);
  end;
  function _parentBlock (aXml: TXml): TXml;
  begin
    result := aXml;
    while Assigned (result)
    and Assigned (result.Parent)
    and (result.Parent.Name <> 'FinMessage') do
      result := result.Parent as TXml;
  end;
var
  pXsd: TXsd;
begin
  result := '';
  if not fXml.Checked then Exit;
  if fXml.Name = 'FinMessage' then
  begin
    result := result + _StreamBlock1  (fXml.Items.XmlItemByTag['Block1']);
    result := result + _StreamBlock2  (fXml.Items.XmlItemByTag['Block2']);
    result := result + _StreamBlock35 (fXml.Items.XmlItemByTag['Block3']);
    result := result + _StreamBlock4  (fXml.Items.XmlItemByTag['Block4']);
    result := result + _StreamBlock35 (fXml.Items.XmlItemByTag['Block5']);
    exit;
  end;
  if fXml.Name = 'Block1' then
  begin
    result := result + _StreamBlock1  (fXml);
    exit;
  end;
  if fXml.Name = 'Block2' then
  begin
    result := result + _StreamBlock2  (fXml);
    exit;
  end;
  if fXml.Name = 'Block3' then
  begin
    result := result + _StreamBlock35  (fXml);
    exit;
  end;
  if fXml.Name = 'Block4' then
  begin
    result := result + _StreamBlock4  (fXml);
    exit;
  end;
  if fXml.Name = 'Block5' then
  begin
    result := result + _StreamBlock35  (fXml);
    exit;
  end;
  with _parentBlock(fXml) do
  begin
    if Name = 'Block1' then
      result := fXml.Value;
    if Name = 'Block2' then
      result := fXml.Value;
    if Name = 'Block3' then
      result := _sBlock35(fXml);
    if Name = 'Block4' then
    begin
      pXsd := nil;
      result := _sBlock4(fXml, pXsd);
    end;
    if Name = 'Block5' then
      result := _sBlock35(fXml);
  end;
end;

end.
