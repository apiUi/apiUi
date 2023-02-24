{
 This file is part of the apiUi project
 Copyright (c) 2009-2023 by Jan Bouwman

 See the file COPYING, included in this distribution,
 for details about the copyright.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit pegasimul8rmapping;

{$mode objfpc}{$H+}

interface
uses Classes
   ;

function generatePegaSimul8rOperationSimulations (aOperation: TObject): string;
function generatePegaSimul8rOperationSimulationsParams (aXml: TObject; aOperation: TObject): string;
procedure ScanSimul8rMessagesLog(se: TObject; aString: String; aLogList: TObject; aDoLogEventProcessing: Boolean);

implementation

uses StrUtils
   , SysUtils
   , Bind
   , Xmlz
   , Ipmz
   , Wsdlz
   , Xsdz
   , IdURI
   , RegExpr
   , WsdlProjectz
   , Logz
   , xmlio
   , xmlxsdparser
   , IpmTypes
   ;


function generatePegaSimul8rOperationSimulations(aOperation: TObject): string;
  function _hasResponseBody (aXml: TXml): TXml;
  var
    x: Integer;
  begin
    result := Nil;
    if aXml.Checked then
    begin
      for x := 0 to aXml.Items.Count - 1 do
      with aXml.Items.XmlItems[x] do
      begin
        if Checked
        and Assigned (Xsd)
        and (Xsd.ParametersType = oppBody) then
          result := thisXml;
      end;
    end;
  end;

  function _hasResponseHeaders (aXml: TXml): Boolean;
  var
    x: Integer;
  begin
    result := False;
    if aXml.Checked then
    begin
      for x := 0 to aXml.Items.Count - 1 do
      with aXml.Items.XmlItems[x] do
      begin
        if Checked
        and Assigned (Xsd)
        and (Xsd.ParametersType = oppHeader) then
          result := True;
      end;
    end;
  end;

  function _hasTransformer (aXml: TXml): Boolean;
  var
    x: Integer;
  begin
    if aXml.Checked
    and (pos ('{{', aXml.Value) > 0) then
      result := True
    else
    begin
      for x := 0 to aXml.Items.Count - 1 do
      begin
        result := _hasTransformer(aXml.Items.XmlItems[x]);
        if result then
          Exit;
      end;
    end;
  end;

  procedure _TranslateStampersToHandlebars (aOperation: TWsdlOperation; aXml: TXml);
  var
    x, f: Integer;
    xBind: TCustomBindable;
    hName: String;
  begin
    if not aXml.Checked then Exit;
    if StartsStr(':=Req.', aXml.Value) then
    begin
      with TRegExpr.Create('^\:\=Req\.[a-zA-Z](a-zA-Z0-9\-\.)*$') do
      try
//        if Exec(aXml.Value) then
        begin
          xBind := aOperation.FindBindOnScriptId(Copy (aXml.Value, 3, MaxInt));
          if Assigned (xBind)
          and (xBind is TXml) then with xBind as TXml do
          begin
            if Assigned (Xsd) then
            begin
              if Xsd.ParametersType = oppPath then
              begin
                hName := '{' + SeparatedStringN(nil, aXml.Value, '.', 3) + '}';
                with SeparatedStringList(nil, aOperation.Wsdl.ServerPathNames[0] + aOperation.WsdlService.openApiPath, '/') do
                try
                  if Find(hName, f) then
                    aXml.Value := '{{request.path.[' + IntToStr(f - 1) + ']}}';
                finally
                  Free;
                end;
              end
              else
              begin
                if Xsd.ParametersType = oppQuery then
                begin
                  aXml.Value := '{{request.query.' + xBind.Name + '}}';
                end
                else
                begin
                  if Xsd.ParametersType = oppHeader then
                  begin
                    aXml.Value := '{{request.headers.[' + xBind.Name + ']}}';
                  end
                  else
                  begin // body...
                    if aOperation.isSoapService then
                    begin
                      aXml.Value := '{{xPath request.body ''' + aOperation.FullXPath(xBind) + '''}}'
                    end
                    else
                    begin
                      aXml.Value := '{{jsonPath request.body ''' + (xbind as TXml).fullJsonBodyPath + '''}}'
          // xml
                    end;

                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        Free;
      end;
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _TranslateStampersToHandlebars(aOperation, aXml.Items.XmlItems[x]);
  end;

  function _comparator (aCorrelationValue: String): String;
  begin
    result := ifthen(aCorrelationValue = '.*', 'any', 'regex')
  end;

  function _isSoapBodyParam (aXml: TXml): Boolean;
  var
    xXml: TXml;
  begin
    result := true;
    xXml := aXml;
    while Assigned (xXml) do
    begin
      if Assigned (xXml.Xsd)
      and (xXml.Xsd.soapPartType = sptHeader) then
        result := False;
      xXml := TXml (xXml.Parent);
    end;
  end;

  function _fullXBodyPath (aXml: TXml): String;
    function _fxbp (aXml: TXml): String;
    begin
      result := '';
      if (aXml = nil)
      or (aXml.Parent = nil)
      then
        exit;
      if aXml.Parent = nil then
        result := '/' + aXml.GetIndexCaption
      else
      begin
        result := _fxbp (aXml.Parent as TXml)
                + '/'
                + aXml.GetIndexCaption;
      end;
    end;

  begin
    result := '/' + _fxbp (aXml);
  end;

  procedure _genMessageXml (aOperation: TWsdlOperation; aXml: TXml; aMessage: TWsdlMessage; aDefaultMessage: Boolean);
  var
    x, y, f: Integer;
    xPath, xPreparedReply, xReplyContentType, xCorrValue: String;
    corrXml, pathXml, arrayXml, rXml, sXml, mXml: TXml;
  begin
    with aXml do
    begin
      with aOperation.reqXml do
      begin
        ResetValues;
        LoadValues((aMessage.reqBind as TXml), False, True);
      end;
      with aOperation.rpyXml do
      begin
        ResetValues;
        LoadValues((aMessage.rpyBind as TXml), False, True);
        _TranslateStampersToHandlebars (aOperation, aOperation.rpyBind as TXml);
      end;
      xPreparedReply := aOperation.PrepareReply (_progName, True);
      aXml.AddXml (TXml.CreateAsString('title', aMessage.Name));
      aXml.AddXml (TXml.CreateAsString('defnDesc', aMessage.Name));
      sXml := aMessage.pegaSimul8rSimulationData.CheckedItemByTag['state'];
      if Assigned (sXml) then
        aXml.AddXml (TXml.CreateAsString(sXml.Name, '')).LoadValues(sXml, True, True);
      with aXml.AddXml (TXml.CreateAsString('request', '')) do
      begin
        if aOperation.isSoapService then
          AddXml (TXml.CreateAsString('requestType', 'application/xml'))
        else
          AddXml (TXml.CreateAsString('requestType', aOperation.Consumes));
        if aOperation.isOpenApiService then
        begin
          if aOperation.Wsdl.ServerPathNames.Count > 0 then
            xPath := aOperation.Wsdl.ServerPathNames[0] + aOperation.WsdlService.openApiPath
          else
            xPath := aOperation.WsdlService.openApiPath;
          if aOperation.hasPathParam then
          begin
            pathXml := AddXml (TXml.CreateAsString('pathParam', ''));
            pathXml.jsonType := jsonArray;
            for x := 0 to aMessage.reqXml.Items.Count - 1 do with aMessage.reqXml.Items.XmlItems[x] do
            begin
              if Xsd.ParametersType = oppPath then
              begin
                f := aMessage.CorrelationBindables.IndexOfObject(thisXml);
                if f < 0 then
                  xCorrValue := '.*'
                else
                  xCorrValue := aMessage.CorrelationBindables.Bindables[f].CorrelationValue;
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (xCorrValue)));
                arrayXml.AddXml (TXml.CreateAsString('mapValue', xCorrValue));
              end;
            end;
          end;
          if (not aDefaultMessage)
          and aOperation.hasHeaderCorrelation then
          begin
            pathXml := AddXml (TXml.CreateAsString('headerParam', ''));
            pathXml.jsonType := jsonArray;
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType = oppHeader) then
              begin
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('identifier', thisXml.Name));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('mapValue', thisXml.CorrelationValue));
              end;
            end;
          end;
          if (not aDefaultMessage)
          and aOperation.hasQueryCorrelation then
          begin
            pathXml := AddXml (TXml.CreateAsString('queryParam', ''));
            pathXml.jsonType := jsonArray;
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType = oppQuery) then
              begin
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('identifier', thisXml.Name));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('mapValue', thisXml.CorrelationValue));
              end;
            end;
          end;
        end;
        if (not aDefaultMessage)
        and aOperation.hasBodyCorrelation then
        begin
          pathXml := AddXml (TXml.CreateAsString('bodyParam', ''));
          pathXml.jsonType := jsonArray;
          for x := 0 to aMessage.CorrelationBindables.Count - 1 do
          begin
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType in [oppDefault, oppBody]) then
              begin
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('path', IfThen ( aOperation.isSoapService
                                                                    , _fullXBodyPath (thisXml) + '/text()'
                                                                    , thisXml.fullJsonBodyPath
                                                                    )));
                if aOperation.isSoapService then
                  arrayXml.AddXml (TXml.CreateAsString('Type', IfThen (_issoapbodyparam (thisXml), 'SOAPBody', 'SOAPHeader')));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('mapValue', thisXml.CorrelationValue));
              end;
            end;
          end;
        end;
      end;
      rXml := aXml.AddXml (TXml.CreateAsString('response', ''));
      rXml.AddXml(TXml.CreateAsInteger('responseStatus', aOperation.ResponseNo));
      if xPreparedReply <> '' then
      begin
        rXml.AddXml (TXml.CreateAsString ('contentType', aOperation.apiReplyMediaType));
        rXml.AddXml (TXml.CreateAsString('body', xPreparedReply));
      end;
      if rXml.Items.Count = 0 then
        rXml.AddXml(TXml.CreateAsInteger('responseStatus', 200)); // avoid PegaSimul8r complaining; same as default PegaSimul8r behaviour
      if Assigned (aMessage.pegaSimul8rSimulationData) then
      begin
        mXml := aMessage.pegaSimul8rSimulationData.Items.XmlCheckedItemByTag['response'];
        if Assigned (mXml) then
        begin
          mXml := mXml.items.XmlCheckedItemByTag['responseDataTransform'];
          if Assigned (mXml) then
            rXml.AddXml(TXml.CreateAsString(mXml.Name, mXml.Value));
        end;
        mXml := aMessage.pegaSimul8rSimulationData.Items.XmlCheckedItemByTag['advanced'];
        if Assigned (mXml) then
        with aXml.AddXml (TXml.CreateAsString(mXml.Name, '')) do
        begin
          CopyDownLine(mXml, True);
          SetJsonTypeForIntegers;
        end;
      end;
    end;
  end;
var
  xPath, xPreparedReply, xReplyContentType: String;
  xDefaultMessage: Boolean;
  m, x, y, xIndex: Integer;
  corrXml, rXml, mXml: TXml;
  xMessage: TWsdlMessage;
begin
  if not (aOperation is TWsdlOperation) then
    raise Exception.Create('generatePegaSimul8rMapping: Illegal argument (aOperation)');
  with aOperation as TWsdlOperation do
  begin
    if not Assigned (thisOperation.Cloned) then
      raise Exception.Create('generatePegaSimul8rMapping: only allowed on cloned operations');
    if thisOperation.isOpenApiService
    or thisOperation.isSoapService then
    with TXml.CreateAsString('', '') do
    try
      with AddXml (TXml.CreateAsString('RequestList', '')) do
      begin
        jsonType := jsonArray;
        if thisOperation.Messages.Count > 1 then
          for m := 1 to thisOperation.Messages.Count - 1 do
            _genMessageXml (thisOperation, AddXml (TXml.CreateAsString('_', '')), thisOperation.Messages.Messages[m], False);
        _genMessageXml (thisOperation, AddXml (TXml.CreateAsString('_', '')), thisOperation.Messages.Messages[0], True);
      end;
      result := StreamJSON(0, false);
    finally
      Free;
    end
    else
      raise Exception.Create ('only implemented for openapi and soap/xml');
  end;
end;

function generatePegaSimul8rOperationSimulationsParams (aXml: TObject; aOperation: TObject): string;
  function _ParamsAsHttpString (aXml: TXml): String;
  var
    x: Integer;
    xSep: String;
  begin
    result := '';
    xSep := '?';
    for x := 0 to aXml.Items.Count - 1 do with aXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        result := result + xSep + Name + '=' + xmlio.urlPercentEncode (xmlio.resolveAliasses (Value));
        xSep := '&';
      end;
    end;
  end;
var
  rXml: TXml;
begin
  result := '';
  if (not Assigned (aXml))
  or (not (aXml is TXml))then
    raise Exception.Create('function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string; not assigned Xml');
  if (not Assigned (aOperation))
  or (not (aOperation is TWsdlOperation)) then
    raise Exception.Create('function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string; not assigned operation');
  with aOperation as TWsdlOperation do
  begin
    if not Assigned (pegaSimul8rConnectorData) then
      raise Exception.Create('function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string; not Assigned (thisOperation.pegaSimul8rConnectorData)');
    with TXml.CreateAsString ((aXml as TXml).Name, '') do
    try
      CopyDownLine((aXml as TXml), True);
      Items.XmlValueByTag['Connector'] := pegaSimul8rConnectorData.items.XmlValueByTagDef['Connector', thisOperation.Alias];
      Items.XmlValueByTag['ClassName'] := pegaSimul8rConnectorData.items.XmlValueByTag['ClassName'];
      rXml := pegaSimul8rConnectorData.FindCheckedXml('PegaSimul8rConnectorData.Ruleset.RulesetName');
      if Assigned (rXml) then
      begin
        Items.XmlValueByTag['Ruleset'] := rXml.Value;
      end;
      Items.XmlValueByTag['Method'] := thisOperation.httpVerb;
      result := _ParamsAsHttpString(thisXml);
    finally
      thisXml.Free;
    end;
  end;
end;

procedure ScanSimul8rMessagesLog(se: TObject; aString: String; aLogList: TObject; aDoLogEventProcessing: Boolean);
  procedure _replCloseOpenWithoutComma;
  var
    x: Integer;
  begin
    x := Pos ('}  {', aString);
    while x > 0 do
    begin
       aString [x + 1] := ',';
       x := Pos ('}  {', aString);
    end;
  end;

  function _makePathFormat (aPathFormat, aPath: String): String;
  var
    formatSL, pathSL: TParserStringList;
    xf, xp: Integer;
  begin
    formatSl := nil;
    pathSL := nil;
    result := '';
    try
      Assert (Copy (aPathFormat, 1, 1) = '/', 'PathFormat should start with a ''/''');
      Assert (Copy (aPath, 1, 1) = '/', 'Path should start with a ''/''');
      formatSL := SeparatedStringList(nil, aPathFormat, '/');
      pathSL := SeparatedStringList(nil, aPath, '/');
      xf := formatSL.Count - 1;
      xp := pathSL.Count - 1;
      while (xf > 0) and (xp > 0) do
      begin
        if (formatSL.Strings[xf] <> '')
        and (formatSL.Strings[xf][1] = '{') then
          formatSL.Strings[xf] := '%s'
        else
          formatSL.Strings[xf] := pathSL.Strings[xp];
        Dec (xf);
        Dec (xp);
      end;
      for xf := 1 to formatSL.Count - 1 do
        result := result + '/' + formatSL.Strings[xf];
    finally
      FreeAndNil(formatSL);
      FreeAndNil(pathSL);
    end;
  end;

  function _makeDocument (aPathFormat, aPath: String): String;
  var
    formatSL, pathSL: TParserStringList;
    xf, xp: Integer;
  begin
    formatSl := nil;
    pathSL := nil;
    result := '';
    try
      formatSL := SeparatedStringList(nil, aPathFormat, '/');
      pathSL := SeparatedStringList(nil, aPath, '/');
      xf := formatSL.Count - 1;
      xp := pathSL.Count - 1;
      while (xf > 0) and (xp > 0) do
      begin
        if (formatSL.Strings[xf] = '%s') then
          formatSL.Strings[xf] := pathSL.Strings[xp]
        else
          Assert(formatSL.Strings[xf] = pathSL.Strings[xp], 'Unexpected difference found');
        Dec (xf);
        Dec (xp);
      end;
      for xf := 1 to formatSL.Count - 1 do
        result := result + '/' + formatSL.Strings[xf];
    finally
      FreeAndNil(formatSL);
      FreeAndNil(pathSL);
    end;
  end;

  function _convertTimeStamp (aValue: String): String;
  begin
    result := aValue;
    if (Length (aValue) > 19)
    and (aValue [5] <> '-') then
    begin
// 123456789012345678901234567890
// 20220926T155513.057 GMT
// 2022-09-26T15:55:13.057Z
      result := Copy (aValue, 1, 4)
              + '-'
              + Copy (aValue, 5, 2)
              + '-'
              + Copy (aValue, 7, 2)
              + 'T'
              + Copy (aValue, 10, 2)
              + ':'
              + Copy (aValue, 12, 2)
              + ':'
              + Copy (aValue, 14, 2)
              + '.'
              + Copy (aValue, 17, 3)
              + 'Z'
              ;
    end;
  end;

  function _workAroundSimul8rSoapRequestLoggingBug (aString: String): String;
  begin
    // workaround simul8r not logging soap stuf in request
    result := aString;
    with TXml.Create do
    try
      LoadFromString('<xml>' + aString + '</xml>', nil);
      if (Name = '')
      or (Items.Count = 0) then // invalid xml
        Exit;

      if (Items.Count > 0)
      and (NameWithoutPrefix (Items.XmlItems[0].Name) = 'Envelope') then
        Exit;
      SeparateNsPrefixes;
      ResolveNameSpaces;
      if Items.Count = 2 then // header + body
      begin
        result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"><soapenv:Header>'
                + Items.XmlItems[0].StreamXML (True, True, 2, True, True)
                + '</soapenv:Header><soapenv:Body>'
                + Items.XmlItems[1].StreamXML (True, True, 2, True, True)
                + '</soapenv:Body></soapenv:Envelope>'
      end
      else  // only body
        result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"><soapenv:Body>'
                + Items.XmlItems[0].StreamXML (True, True, 2, True, True)
                + '</soapenv:Body></soapenv:Envelope>'
                ;
    finally
      Free;
    end;
  end;

var
  x, y, d, f: Integer;
  xLog: TLog;
  xBodiesAsBase64: Boolean;
  xPegaAlias, xPath, xQuery, xSortkey: String;
  xIsSoap: Boolean;
  allConnectors: TJBStringList;
  xLogList: TLogList;
begin
  if (not Assigned (se))
  or (not (se is TWsdlProject)) then
    raise Exception.Create('ScanSimul8rMessagesLog: illegal argument for se');
  if (not Assigned (aLogList))
  or (not (aLogList is TLogList)) then
    raise Exception.Create('ScanSimul8rMessagesLog: illegal argument for LogList');
  xLogList := aLogList as TLogList;
  xLogList.Sorted := True;
  xLogList.Duplicates := dupAccept;
  with se as TWsdlProject do
  begin
    allConnectors := TJBStringList.Create;
    allConnectors.Sorted:=True;
    try
      for x := 0 to allAliasses.Count - 1 do with allAliasses.Operations[x] do
      begin
        if Assigned (pegaSimul8rConnectorData) then
        begin
          allConnectors.AddObject ( pegaSimul8rConnectorData.Items.XmlValueByTagDef['Connector', '']
                                  + ':'
                                  + UpperCase(httpVerb)
                                  , thisOperation
                                  );
        end;
      end;
      with TXml.Create do
      try
        if doWorkAroundSimul8rBug then
        begin
          _replCloseOpenWithoutComma;
        end;
        LoadJsonFromString(aString, nil);
        if Name = '' then
          raise Exception.Create ('Unabale to parse as JSON' + LineEnding + aString);
        with Items.XmlItemByTag['interactions'] do
        begin
          if not Assigned (thisXml) then
            raise Exception.Create('Does not contain saved Simul8r messages');
          for x := 0 to Items.Count - 1 do
          begin
            with Items.XmlItems [x] do
            begin
              if TagName <> '_' then
                raise Exception.Create('serveEvents array entry expected');
              xSortkey := '';
              xLog := TLog.Create;
              xLog.MessageId := Items.XmlValueByTagDef ['id', xLog.MessageId];
              xIsSoap:=False;
              with Items.XmlItemByTag['request'] do if Assigned (thisXml) then
              begin
                xLog.ServiceName := Items.XmlValueByTag['ServiceName'];
                with Items.XmlItemByTag['InboundTimeStamp'] do if Assigned (thisXml) then
                try
                  xLog.InboundTimeStamp := xsdParseDateTime (_convertTimeStamp (thisXml.Value));
                except
                  xLog.InboundTimeStamp := TDateTime (0);
                end;
                with Items.XmlItemByTag['RequestContent-Type'] do if Assigned (thisXml) then
                begin
                  xLog.RequestContentType := thisXml.Value;
                  if xLog.RequestContentType = 'JSON' then
                    xLog.RequestContentType := 'application/json';
                  if Copy (xLog.RequestContentType, 1, 3) = 'XML' then
                    xLog.RequestContentType := 'application/xml';
                end;
                xPath := items.XmlValueByTag['path'];
                xLog.httpParams := items.XmlValueByTag['queryString'];
                if Copy (xLog.httpParams, 1, 1) = '?' then
                  xLog.httpParams := Copy (xLog.httpParams, 2, MaxInt);
                xLog.httpCommand := Uppercase (items.XmlValueByTag['method']);
                xIsSoap := (xLog.httpCommand = 'SOAPMETHOD');
                if xIsSoap then
                  xLog.httpCommand := 'POST';
                xLog.OperationName := xLog.ServiceName + ':' + xLog.httpCommand;
                xLog.StubAction := saStub;
                with Items.XmlItemByTag['headers'] do
                if Assigned (thisXml) then
                begin
                  for d := 0 to Items.Count - 1 do with items.XmlItems[d] do
                  begin
                    xLog.RequestHeaders := xLog.RequestHeaders + Name + ': ' + Value + LineEnding;
                  end;
                end;
                xLog.httpDocument := items.XmlValueByTagDef['path', ''];
                xLog.httpParams := items.XmlValueByTagDef['queryString', ''];
                xLog.RequestBody := Items.XmlValueByTag['body'];
                xLog.InboundBody := xLog.RequestBody;
                if doWorkAroundSimul8rBug then
                begin
                  if xIsSoap then
                    xLog.RequestBody := _workAroundSimul8rSoapRequestLoggingBug (xLog.RequestBody);
                end;
              end;
              with Items.XmlItemByTag['response'] do if Assigned (thisXml) then
              begin
                with Items.XmlItemByTag['OutBoundTimeStamp'] do if Assigned (thisXml) then
                try
                  xSortkey := thisXml.Value;
                  xLog.OutBoundTimeStamp := xsdParseDateTime (_convertTimeStamp (thisXml.Value));
                except
                  xLog.OutBoundTimeStamp := TDateTime (0);
                end;
                with Items.XmlItemByTag['ResponseContent-Type'] do if Assigned (thisXml) then
                begin
                  xLog.ReplyContentType := thisXml.Value;
                  if xLog.ReplyContentType = 'JSON' then
                    xLog.ReplyContentType := 'application/json';
                  if Copy (xLog.ReplyContentType, 1, 3) = 'XML' then
                    xLog.ReplyContentType := 'application/xml';
                end;
                xLog.ReplyBody := Items.XmlValueByTag['body'];
                xLog.httpResponseCode := StrToInt(Items.XmlValueByTag['statusCode']);
                with Items.XmlItemByTag['headers'] do if Assigned (thisXml) then
                begin
                  for d := 0 to Items.Count - 1 do with items.XmlItems[d] do
                  begin
                    xLog.ReplyHeaders := xLog.ReplyHeaders + Name + ': ' + Value + LineEnding;
                  end;
                end;
              end;
              with Items.XmlItemByTag['timing'] do if Assigned (thisXml) then
                xLog.DelayTimeMs := items.XmlIntegerByTag['addedDelay'];
              if xIsSoap then with TXml.Create do
              try
                try
                  LoadFromString(xLog.RequestBody, nil);
                  if Name <> '' then
                  begin
                    SeparateNsPrefixes;
                    ResolveNameSpaces;
                    with FindUQXml('Envelope.Body') do if Assigned (thisXml) then
                      if Items.Count = 1 then with Items.XmlItems[0] do
                        if allOperations.Find(Name + ';' + NameSpace, f) then
                           xLog.Operation := allOperations.Operations[f];
                  end;
                except
                  xLog.Operation := nil;
                end;
              finally
                free;
              end
              else
              begin
                if allConnectors.Find(xlog.ServiceName + ':' + xLog.httpCommand, f) then
                  xLog.Operation := allConnectors.Objects[f] as TWsdlOperation;
              end;
              if Assigned (xLog.Operation) then
              begin
                xLog.ServiceName:= xlog.Operation.WsdlService.Name;
                xLog.OperationName:= xlog.Operation.Alias;
                if (xLog.Operation.WsdlService.DescriptionType <> ipmDTFreeFormat)
                and (   doValidateInboundRequests(xLog.Operation)
                     or doValidateOutboundReplies(xLog.Operation)
                    ) then
                begin
                  xlog.httpDocument := xLog.Operation.WsdlService.logPathFormat;
                  if xPath <> '' then
                  begin
                    xLog.PathFormat := _makePathFormat (xLog.Operation.WsdlService.logPathFormat, xPath);
                    xLog.httpDocument := _makeDocument(xLog.PathFormat, xPath);
                  end;
                  xLog.httpUri := xlog.httpDocument;
                  if xLog.httpParams <> '' then
                    xLog.httpUri := xLog.httpUri + '?' + xLog.httpParams;
                  xLog.Mssg := xLog.Operation.MessageBasedOnRequest;
                  with TWsdlOperation.Create(xLog.Operation) do
                  try
                    try
                      xLog.toBindables(thisOperation);
                      xLog.RequestValidateResult := '';
                      xLog.ReplyValidateResult := '';
                      if doValidateInboundRequests(thisOperation) then
                      begin
                        reqBind.IsValueValid;
                        xLog.RequestValidateResult := reqBind.AllValidationsMessage;
                        xLog.RequestValidated := True;
                      end;
                      if doValidateOutboundReplies(thisOperation) then
                      begin
                        rpyBind.IsValueValid;
                        xLog.ReplyValidateResult := rpyBind.AllValidationsMessage;
                        xLog.ReplyValidated := True;
                      end;
                    except
                    end;
                  finally
                    Free;
                  end;
                end;
                if aDoLogEventProcessing then
                begin
                  if xLog.Operation.onFetchLogFromRemoteServer <> '' then
                  begin
                    with TWsdlOperation.Create(xLog.Operation) do
                    try
                      try
                        xLog.toBindables(thisOperation);
                        OperationScriptExecuteLater(thisOperation, onFetchLogFromRemoteServer, 0);
                      except
                      end;
                    finally
                      // Freed in Thread;
                    end;
                  end;
                end;
              end;
              LogFilter.Execute (xLog);
              xLogList.SaveLog (xSortkey, xLog);
            end;
          end; // for each xml
        end;
      finally
        Free;
      end;
    finally
      allConnectors.Free;
    end;
  end;
end;

end.

