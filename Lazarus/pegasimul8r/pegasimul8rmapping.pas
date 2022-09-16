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
unit pegasimul8rmapping.pas;

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
   ;

function generatePegaSimul8rMappingMetaQuery (aOperation: TWsdlOperation): string;
function generatePegaSimul8rMapping (aOperation: TWsdlOperation; aMessage: TWsdlMessage): string;
function generatePegaSimul8rResetStateMachine: String;

implementation

function generatePegaSimul8rMappingMetaQuery(aOperation: TWsdlOperation): string;
begin
  result := '{"matches": ".*_apiUi.*' + aOperation.Alias + '.*"}';
end;

function generatePegaSimul8rMapping(aOperation: TWsdlOperation; aMessage: TWsdlMessage): string;
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

var
  xPath, xPreparedReply, xReplyContentType: String;
  xDefaultMessage: Boolean;
  x, y, xIndex: Integer;
  corrXml, rXml, mXml: TXml;
begin
  if not Assigned (aOperation.Cloned) then
    raise Exception.Create('generatePegaSimul8rMapping: only allowed on cloned operations');
  xIndex := aOperation.Messages.IndexOfObject(aMessage);
  if xIndex < 0 then
    raise Exception.Create('generatePegaSimul8rMapping: Message does not belong to operation');
  xDefaultMessage := (xIndex = 0);
  if aOperation.isOpenApiService
  or aOperation.isSoapService then
  with TXml.CreateAsString('', '') do
  try
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
    AddXml (TXml.CreateAsString('name', aMessage.Name));
    if aOperation.doUseStateMachine
    and (not xDefaultMessage) then
    begin
      AddXml (TXml.CreateAsString('scenarioName', aMessage.stateMachineScenarioName));
      AddXml (TXml.CreateAsString('requiredScenarioState', aMessage.stateMachineRequiredState));
      AddXml (TXml.CreateAsString('newScenarioState', aMessage.stateMachineNextState));
    end;
    with AddXml (TXml.CreateAsString('request', '')) do
    begin
      AddXml (TXml.CreateAsString('method', aOperation.httpVerb));
      if aOperation.isOpenApiService then
      begin
        if aOperation.Wsdl.ServerPathNames.Count > 0 then
          xPath := aOperation.Wsdl.ServerPathNames[0] + aOperation.WsdlService.openApiPath
        else
          xPath := aOperation.WsdlService.openApiPath;
        if aOperation.hasPathCorrelation then
        begin
          for x := 0 to aMessage.CorrelationBindables.Count - 1 do
          with aMessage.CorrelationBindables.Bindables[x] as TXml do
          begin
            if (Xsd.ParametersType = oppPath) then
            begin
              xPath := ReplaceStr(xPath, '{' + Name + '}', CorrelationValue);
            end;
          end;
          for x := 0 to aOperation.reqXml.Items.Count - 1 do
          with aOperation.reqXml.Items.XmlItems [x] as TXml do
          begin
            if (Xsd.ParametersType = oppPath) then
            begin
              xPath := ReplaceStr(xPath, '{' + Name + '}', '.*'); // for the path params that are not correl.item
            end;
          end;
          AddXml (TXml.CreateAsString('urlPathPattern', xPath));
        end
        else
          AddXml (TXml.CreateAsString('urlPath', xPath));
        if (not xDefaultMessage)
        and aOperation.hasHeaderCorrelation then
        begin
          with AddXml(TXml.CreateAsString('headers', '')) do
          begin
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] do
            begin
              corrXml := thisBind as TXml;
              if (corrXml.Xsd.ParametersType = oppHeader)
  //          and (corrXml.CorrelationValue <> '.*')
              then
              begin
                with AddXml (Txml.CreateAsString(corrXml.Name, '')) do
                  AddXml (TXml.CreateAsString('matches', corrXml.CorrelationValue));
              end;
            end;
          end;
        end;
        if (not xDefaultMessage)
        and aOperation.hasQueryCorrelation then
        begin
          with AddXml(TXml.CreateAsString('queryParameters', '')) do
          begin
            for x := 0 to aMessage.CorrelationBindables.Count - 1 do
            with aMessage.CorrelationBindables.Bindables[x] do
            begin
              corrXml := thisBind as TXml;
              if (corrXml.Xsd.ParametersType = oppQuery)
  //          and (corrXml.CorrelationValue <> '.*')
              then
              begin
                with AddXml (Txml.CreateAsString(corrXml.Name, '')) do
                  AddXml (TXml.CreateAsString('matches', corrXml.CorrelationValue));
              end;
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
      if (not xDefaultMessage)
      and aOperation.hasBodyCorrelation then
      begin
        with AddXml(TXml.CreateAsString('bodyPatterns', '')) do
        begin
          jsonType := jsonArray;
          for x := 0 to aMessage.CorrelationBindables.Count - 1 do
          with aMessage.CorrelationBindables.Bindables[x] do
          begin
            corrXml := thisBind as TXml;
            if Assigned (corrXml) then
            begin
              if aOperation.isOpenApiService then
              begin
                if (corrXml.Xsd.ParametersType in [oppDefault, oppBody])
      //          and (corrXml.CorrelationValue <> '.*')
                then
                begin
                  with AddXml (Txml.CreateAsString('_', '')) do
                  begin
                    if aOperation.ConsumesXmlOnly then
                    begin
                      with AddXml (Txml.CreateAsString ( 'matchesXPath', '')) do
                      begin
                        AddXml (Txml.CreateAsString ( 'expression'
                                                    , aOperation.FullXPath(corrXml) + '/text()'
                                                    ));
                        AddXml (Txml.CreateAsString ( 'matches'
                                                    , corrXml.CorrelationValue
                                                    ));
                      end;
                    end
                    else
                    begin
                      with AddXml (Txml.CreateAsString ( 'matchesJsonPath', '')) do
                      begin
                        AddXml (Txml.CreateAsString ( 'expression'
                                                    , corrXml.fullJsonBodyPath
                                                    ));
                        AddXml (Txml.CreateAsString ( 'matches'
                                                    , corrXml.CorrelationValue
                                                    ));
                      end;
                    end;
                  end;
                end;
              end;
              if aOperation.isSoapService then
              begin
                with AddXml (Txml.CreateAsString('_', '')) do
                begin
                  with AddXml (Txml.CreateAsString ( 'matchesXPath', '')) do
                  begin
                    AddXml (Txml.CreateAsString ( 'expression'
                                                , aOperation.FullXPath(corrXml) + '/text()'
                                                ));
                    AddXml (Txml.CreateAsString ( 'matches'
                                                , corrXml.CorrelationValue
                                                ));
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    rXml := AddXml (TXml.CreateAsString('response', ''));
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
          rXml.AddXml(TXml.CreateAsInteger('status', Xsd.ResponseNo));
          mXml := rXml.AddXml(TXml.CreateAsString ('headers', ''));
          mXml.AddXml(TXml.CreateAsString('Content-Type', xReplyContentType));
          for y := 0 to Items.Count - 1 do
          with Items.XmlItems[y] do
            if Checked
            and Assigned (Xsd)
            and (Xsd.ParametersType = oppHeader) then
              mXml.AddXml(TXml.CreateAsString(Name, Value));
          if Assigned (_hasResponseBody (thisXml)) then
            rXml.AddXml (TXml.CreateAsString('body', xPreparedReply));
          if _hasTransformer (thisXml) then
          with rXml.AddXml(TXml.CreateAsString ('transformers', '')) do
          begin
            jsonType := jsonArray;
            AddXml (TXml.CreateAsString ('', 'response-template')).jsonType := jsonString;
          end;
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
    if (aOperation.DelayTimeMsMin > 0)
    or (aOperation.DelayTimeMsMax > 0) then
    begin
      if (aOperation.DelayTimeMsMin = aOperation.DelayTimeMsMax) then
        rXml.AddXml(TXml.CreateAsInteger('fixedDelayMilliseconds', aOperation.DelayTimeMsMax))
      else
      with rXml.AddXml(TXml.CreateAsString('delayDistribution', '')) do
      begin
        AddXml (TXml.CreateAsString('type', 'uniform'));
        AddXml (TXml.CreateAsInteger('lower', aOperation.DelayTimeMsMin));
        AddXml (TXml.CreateAsInteger('upper', aOperation.DelayTimeMsMax));
      end;
    end;
    if xDefaultMessage then
      AddXml (TXml.CreateAsString('priority', '999999')).jsonType := jsonNumber
    else
      AddXml (TXml.CreateAsInteger('priority', xIndex)).jsonType := jsonNumber;
    with AddXml (TXml.CreateAsString('metadata', '')) do
      AddXml (TXml.CreateAsString('_apiUi', aOperation.Alias));
    result := StreamJSON(0, false);
  finally
    Free;
  end
  else
    raise Exception.Create ('only implemented for openapi and soap/xml');
end;

function generatePegaSimul8rResetStateMachine: String;
begin
  result := '/__admin/scenarios/reset';
end;

end.

