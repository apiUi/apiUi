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
   , StrUtils
   , SysUtils
   , Bind
   , Xmlz
   , Ipmz
   , Wsdlz
   , Xsdz
   , IdURI
   , RegExpr
   , xmlio
   ;

function generatePegaSimul8rOperationSimulations (aOperation: TWsdlOperation): string;
function generatePegaSimul8rOperationSimulationsParams (aXml: TXml; aOperation: TWsdlOperation): string;

implementation

function generatePegaSimul8rOperationSimulations(aOperation: TWsdlOperation): string;
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

  procedure _TranslateStampersToHandlebars (aXml: TXml);
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
      _TranslateStampersToHandlebars(aXml.Items.XmlItems[x]);
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

  procedure _genMessageXml (aXml: TXml; aMessage: TWsdlMessage; aDefaultMessage: Boolean);
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
        _TranslateStampersToHandlebars (aOperation.rpyBind as TXml);
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
  if not Assigned (aOperation.Cloned) then
    raise Exception.Create('generatePegaSimul8rMapping: only allowed on cloned operations');
  if aOperation.isOpenApiService
  or aOperation.isSoapService then
  with TXml.CreateAsString('', '') do
  try
    with AddXml (TXml.CreateAsString('RequestList', '')) do
    begin
      jsonType := jsonArray;
      if aOperation.Messages.Count > 1 then
        for m := 1 to aOperation.Messages.Count - 1 do
          _genMessageXml (AddXml (TXml.CreateAsString('_', '')), aOperation.Messages.Messages[m], False);
      _genMessageXml (AddXml (TXml.CreateAsString('_', '')), aOperation.Messages.Messages[0], True);
    end;
    result := StreamJSON(0, false);
  finally
    Free;
  end
  else
    raise Exception.Create ('only implemented for openapi and soap/xml');
end;

function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string;
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
  if not Assigned (aXml) then Exit;
  if not Assigned (aOperation) then
    raise Exception.Create('function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string; not assigned operation');
  if not Assigned (aOperation.pegaSimul8rConnectorData) then
    raise Exception.Create('function generatePegaSimul8rOperationSimulationsParams(aXml: TXml; aOperation: TWsdlOperation): string; not Assigned (aOperation.pegaSimul8rConnectorData)');
  with TXml.CreateAsString (aXml.Name, '') do
  try
    CopyDownLine(aXml, True);
    Items.XmlValueByTag['Connector'] := aOperation.pegaSimul8rConnectorData.items.XmlValueByTagDef['Connector', aOperation.Alias];
    Items.XmlValueByTag['ClassName'] := aOperation.pegaSimul8rConnectorData.items.XmlValueByTag['ClassName'];
    rXml := aOperation.pegaSimul8rConnectorData.FindCheckedXml('PegaSimul8rConnectorData.Ruleset.RulesetName');
    if Assigned (rXml) then
    begin
      Items.XmlValueByTag['Ruleset'] := rXml.Value;
    end;
    Items.XmlValueByTag['Method'] := aOperation.httpVerb;
    result := _ParamsAsHttpString(thisXml);
  finally
    thisXml.Free;
  end;
end;

end.

