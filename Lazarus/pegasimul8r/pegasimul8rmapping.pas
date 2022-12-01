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

  procedure _genMessageXml (aXml: TXml; aMessage: TWsdlMessage; aDefaultMessage: Boolean);
  var
    x, y: Integer;
    xPath, xPreparedReply, xReplyContentType: String;
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
      AddXml (TXml.CreateAsString('title', aMessage.Name));
      AddXml (TXml.CreateAsString('description', aMessage.Name));
      sXml := aMessage.pegaSimul8rSimulationData.ItemByTag['state'];
      if Assigned (sXml) then
        AddXml (TXml.CreateAsString(sXml.Name, '')).LoadValues(sXml, True, True);
      with AddXml (TXml.CreateAsString('request', '')) do
      begin
        AddXml (TXml.CreateAsString('contentType', aOperation.Consumes));
        if aOperation.isOpenApiService then
        begin
          if aOperation.Wsdl.ServerPathNames.Count > 0 then
            xPath := aOperation.Wsdl.ServerPathNames[0] + aOperation.WsdlService.openApiPath
          else
            xPath := aOperation.WsdlService.openApiPath;
          if aOperation.hasPathCorrelation then
          begin
            pathXml := AddXml (TXml.CreateAsString('pathParameters', ''));
            pathXml.jsonType := jsonArray;
            y := 0;
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType = oppPath) then
              begin
                Inc (y);
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('index', IntToStr (y))).jsonType := jsonNumber;
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('value', thisXml.CorrelationValue));
              end;
            end;
          end;
          if (not aDefaultMessage)
          and aOperation.hasHeaderCorrelation then
          begin
            pathXml := AddXml (TXml.CreateAsString('headerParameters', ''));
            pathXml.jsonType := jsonArray;
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType = oppHeader) then
              begin
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('identifier', thisXml.Name));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('value', thisXml.CorrelationValue));
              end;
            end;
          end;
          if (not aDefaultMessage)
          and aOperation.hasQueryCorrelation then
          begin
            pathXml := AddXml (TXml.CreateAsString('queryParameters', ''));
            pathXml.jsonType := jsonArray;
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] as TXml do
            begin
              if (Xsd.ParametersType = oppQuery) then
              begin
                arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
                arrayXml.AddXml (TXml.CreateAsString('identifier', thisXml.Name));
                arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
                arrayXml.AddXml (TXml.CreateAsString('value', thisXml.CorrelationValue));
              end;
            end;
          end;
        end;
        if aOperation.isSoapService then
        begin
          with TIdUri.Create(aOperation.SoapAddress) do
          try
            AddXml (TXml.CreateAsString('urlPath', Path));
          finally
            free;
          end;
        end;
        if (not aDefaultMessage)
        and aOperation.hasBodyCorrelation then
        begin
          pathXml := AddXml (TXml.CreateAsString('bodyParameters', ''));
          pathXml.jsonType := jsonArray;
          for x := 0 to aMessage.CorrelationBindables.Count - 1 do
          with aMessage.CorrelationBindables.Bindables[x] as TXml do
          begin
            if (Xsd.ParametersType in [oppDefault, oppBody]) then
            begin
              arrayXml := pathXml.AddXml (TXml.CreateAsString('_', ''));
              arrayXml.AddXml (TXml.CreateAsString('path', IfThen ( aOperation.ConsumesXmlOnly
                                                                  , aOperation.FullXPath(thisXml) + '/text()'
                                                                  , thisXml.fullJsonBodyPath
                                                                  )));
              arrayXml.AddXml (TXml.CreateAsString('comparator', _comparator (thisXml.CorrelationValue)));
              arrayXml.AddXml (TXml.CreateAsString('value', thisXml.CorrelationValue));
            end;
          end;
        end;
      end;
      rXml := AddXml (TXml.CreateAsString('response', ''));
      if aOperation.Produces <> '' then
        rXml.AddXml (TXml.CreateAsString('contentType', aOperation.Produces));
      if aOperation.isOpenApiService then
      begin
        for x := 0 to aOperation.rpyXml.Items.Count - 1 do
        with aOperation.rpyXml.Items.XmlItems[x] do
        begin
          if Checked then
          begin
            xPreparedReply := aOperation.PrepareReply (_progName, True);
            xReplyContentType := aOperation.apiReplyMediaType;
            if xReplyContentType = '' then
              try
                xReplyContentType := SeparatedStringN(nil, aOperation.Produces, LineEnding, 1);
              except
              end;
            rXml.AddXml(TXml.CreateAsInteger('status', aOperation.ResponseNo));
            if aOperation.hasApiReplyHeader then
            begin
              mXml := rXml.AddXml(TXml.CreateAsString ('headers', ''));
  //          mXml.AddXml(TXml.CreateAsString('Content-Type', xReplyContentType));
              for y := 0 to Items.Count - 1 do
              with Items.XmlItems[y] do
                if Checked
                and Assigned (Xsd)
                and (Xsd.ParametersType = oppHeader) then
                  mXml.AddXml(TXml.CreateAsString(Name, Value));
            end;
            if Assigned (_hasResponseBody (thisXml)) then
              rXml.AddXml (TXml.CreateAsString('body', xPreparedReply));
          end;
        end;
        if rXml.Items.Count = 0 then
          rXml.AddXml(TXml.CreateAsInteger('status', 200)); // avoid PegaSimul8r complaining; same as default PegaSimul8r behaviour
      end;
      if aOperation.isSoapService then
      begin
        rXml.AddXml(TXml.CreateAsInteger('status', 200));
        xPreparedReply := aOperation.PrepareReply (_progName, True);
        mXml := rXml.AddXml(TXml.CreateAsString ('headers', ''));
        mXml.AddXml(TXml.CreateAsString('Content-Type', 'application/xml'));
        rXml.AddXml (TXml.CreateAsString('body', xPreparedReply));
        if _hasTransformer (thisXml) then
        with rXml.AddXml(TXml.CreateAsString ('transformers', '')) do
        begin
          jsonType := jsonArray;
          AddXml (TXml.CreateAsString ('', 'response-template')).jsonType := jsonString;
        end;
      end;
      with aMessage.pegaSimul8rSimulationData.ItemByTag['response'] do if Assigned (thisXml) then
      begin
        with ItemByTag['responseDataTransform'] do if Assigned (thisXml) then
          rXml.AddXml(TXml.CreateAsString(thisXml.Name, thisXml.Value));
      end;
      rXml := aMessage.pegaSimul8rSimulationData.ItemByTag['advanced'];
      if Assigned (rXml) then
        AddXml (TXml.CreateAsString(rXml.Name, '')).CopyDownLine(rXml, True);
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
    jsonType := jsonArray;
    if aOperation.Messages.Count > 1 then
      for m := 1 to aOperation.Messages.Count - 1 do
        _genMessageXml (AddXml (TXml.CreateAsString('_', '')), aOperation.Messages.Messages[m], False);
    _genMessageXml (AddXml (TXml.CreateAsString('_', '')), aOperation.Messages.Messages[0], True);
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

