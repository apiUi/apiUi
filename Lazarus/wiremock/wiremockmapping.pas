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
unit wiremockmapping;

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
   ;

function generateWireMockMappingMetaQuery (aOperation: TWsdlOperation): string;
function generateWireMockMapping (aOperation: TWsdlOperation; aMessage: TWsdlMessage): string;

implementation

function generateWireMockMappingMetaQuery(aOperation: TWsdlOperation): string;
begin
  result := '{"matches": ".*_apiUi.*' + aOperation.Alias + '.*"}';
end;

function generateWireMockMapping(aOperation: TWsdlOperation; aMessage: TWsdlMessage): string;
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

var
  xPath: String;
  xDefaultMessage: Boolean;
  x, y, xIndex: Integer;
  corrXml, rXml, mXml: TXml;
begin
  if not Assigned (aOperation.Cloned) then
    raise Exception.Create('generateWireMockMapping: only allowed on cloned operations');
  xIndex := aOperation.Messages.IndexOfObject(aMessage);
  if xIndex < 0 then
    raise Exception.Create('generateWireMockMapping: Message does not belong to operation');
  xDefaultMessage := (xIndex = 0);
  if aOperation.isOpenApiService then
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
    end;
    AddXml (TXml.CreateAsString('name', aMessage.Name));
    with AddXml (TXml.CreateAsString('request', '')) do
    begin
      AddXml (TXml.CreateAsString('method', aOperation.httpVerb));
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
        AddXml (TXml.CreateAsString('urlPath', aOperation.WsdlService.openApiPath));
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
            if (corrXml.Xsd.ParametersType in [oppDefault, oppBody])
//          and (corrXml.CorrelationValue <> '.*')
            then
            begin
              with AddXml (Txml.CreateAsString('_', '')) do
                with AddXml (Txml.CreateAsString ( 'matchesJsonPath', '')) do
                begin
                        AddXml (Txml.CreateAsString ( 'expression'
                                                    , corrXml.fullJsonBodyPath
                                                    ));
                        AddXml (Txml.CreateAsString ( 'matches'
                                                    , corrXml.CorrelationValue
                                                    ));
//                                                    + '=~'''
//                                                    + corrXml.CorrelationValue
  //                                                  + ''''
    //                                                )
                end;
            end;
          end;
        end;
      end;
    end;
    rXml := AddXml (TXml.CreateAsString('response', ''));
    for x := 0 to aOperation.rpyXml.Items.Count - 1 do
    with aOperation.rpyXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        rXml.AddXml(TXml.CreateAsInteger('status', Xsd.ResponseNo));
        if _hasResponseHeaders (thisXml) then
        begin
          mXml := rXml.AddXml(TXml.CreateAsString ('headers', ''));
          for y := 0 to Items.Count - 1 do
          with Items.XmlItems[y] do
            if Checked
            and Assigned (Xsd)
            and (Xsd.ParametersType = oppHeader) then
              mXml.AddXml(TXml.CreateAsString(Name, Value));
        end;
        if Assigned (_hasResponseBody (thisXml)) then
          rXml.AddXml (TXml.CreateAsString('body', aOperation.PrepareReply (_progName, True)));
        if _hasTransformer (thisXml) then
        with rXml.AddXml(TXml.CreateAsString ('transformers', '')) do
        begin
          jsonType := jsonArray;
          AddXml (TXml.CreateAsString ('', 'response-template')).jsonType := jsonString;
        end;
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
    raise Exception.Create ('only implemented for openapi');
end;

end.

