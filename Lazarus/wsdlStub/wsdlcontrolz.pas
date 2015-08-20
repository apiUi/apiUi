unit wsdlcontrolz ;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface

uses Classes
   , Xmlz
   , igGlobals
   , IdCustomHTTPServer
   , Wsdlz
   , SysUtils
   , IdHTTPServer
   , IdContext
   , SyncObjs
   , WsdlProjectz
   ;


type

  { TWsdlControl }

  TWsdlControl = class
  private
    fActive : Boolean ;
    fPortNumber : Integer ;
    function getActive : Boolean ;
    procedure setActive (AValue : Boolean );
    procedure setPortNumber (AValue : Integer );
  public
    se: TWsdlProject;
    HttpWebPageServer: TIdHTTPServer;
    OnRestartEvent: TStringFunction;
    OnReactivateEvent: TStringFunction;
    OnActivateEvent: TProcedureB;
    OnOpenProjectEvent: TProcedureS;
    OnClearLogEvent: TStringFunctionBoolean;
    OnQuitEvent: TStringFunctionBoolean;
    OnReloadDesignEvent: TStringFunction;
    procedure HttpWebPageServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    constructor Create;
    property portNumber: Integer read fPortNumber write setPortNumber;
    property Active: Boolean read getActive write setActive;
  end;

implementation

uses strutils
   , wsdlStubHtmlUnit
   , GZIPUtils
   {$ifdef windows}
   , ActiveX
   {$endif}
   ;
{ TWsdlControl }

procedure TWsdlControl .setPortNumber (AValue : Integer );
begin
  if fPortNumber = AValue then Exit ;
  fPortNumber := AValue ;
  HttpWebPageServer.Active := False;
  HttpWebPageServer.DefaultPort := AValue;
  HttpWebPageServer.Bindings.Clear;
  with HttpWebPageServer.Bindings.Add do
  begin
    Port := AValue;
    IP := '0.0.0.0';
  end;
end;

procedure TWsdlControl .setActive (AValue : Boolean );
begin
  if AValue = Active then Exit;
  try
    if (AValue) then
    try
      HttpWebPageServer.SessionState := False;
      HttpWebPageServer.Active := true;
      Notify(format('Listening for HTTP commands on %s:%d.',[HttpWebPageServer.Bindings[0].IP, HttpWebPageServer.Bindings[0].Port]));
    except
      on e: exception do
      begin
        raise Exception.CreateFmt('Exception %s in Activate. Exception is:"%s".', [e.ClassName, e.Message]);
      end;
    end
    else
    begin
      HttpWebPageServer.Active := false;
      Notify('Stopped listening for commands on ' + IntToStr(portNumber));
    end;
  finally
    fActive := HttpWebPageServer.Active;
  end;
end;

function TWsdlControl .getActive : Boolean ;
begin
  result := HttpWebPageServer.Active;
end;

procedure TWsdlControl.HttpWebPageServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  function _prepWsdl(s: String):String;
    function _replPath(s:String):String;
    var
      x: Integer;
    begin
      result := '';
      for x := 1 to Length (s) do
        if s[x] = '\' then
          result := ''
        else
          result := result + s[x];
    end;
  var
    o,n : String;
  begin
    result := s;
    o := _replPath(ChangeFileExt(webserviceWsdlFileName, '.xsd'));
    n := 'wsdlStubWebService?XSD';
    result := ReplaceText(result, o, n);
    o := 'localhost:3738';
    n := _WsdlHostName + ':' + IntToStr(portNumber);
    result := ReplaceText(result, o, n);
  end;
  function _prepXsd(fn: String):String;
  const reqs = '--requestElementNames--';
  const scrpts = '--scriptNames--';
  var
    sl, dl: TStringList;
    x, o: Integer;
  begin
    result :='';
    sl := TStringlist.Create;
    dl := TStringList.Create;
    try
      sl.LoadFromFile(fn);
      for x := 0 to sl.Count - 1 do
      begin
        if Pos (reqs, sl.Strings[x]) > 0 then
          for o := 0 to allOperations.Count - 1 do
            dl.Add(ReplaceText(sl.Strings[x], reqs, allOperations.Strings[o]))
        else
        begin
          if Pos (scrpts, sl.Strings[x]) > 0 then
            for o := 0 to se.Scripts.Count - 1 do
              dl.Add(ReplaceText(sl.Strings[x], scrpts, se.Scripts.Strings[o]))
          else
            dl.Add(sl.Strings[x]);
        end;
      end;
      result := dl.Text;
    finally
      sl.Free;
      dl.Free;
    end;
  end;
  function _prepHttp(fn: String):String;
  const rops = '--operationNames--';
  var
    sl, dl: TStringList;
    x, o, s: Integer;
  begin
    result :='';
    sl := TStringlist.Create;
    dl := TStringList.Create;
    try
      sl.LoadFromFile(fn);
      for x := 0 to sl.Count - 1 do
      begin
        if Pos (rops, sl.Strings[x]) > 0 then
          with webserviceWsdl.Services do
            for s := 0 to Count - 1 do
              for o := 0 to Services[s].Operations.Count - 1 do
                dl.Add(ReplaceText(sl.Strings[x], rops, Services[s].Operations.Operations[o].Name))
        else
          dl.Add(sl.Strings[x]);
      end;
      result := ReplaceText(dl.Text, '.wsdl', '?WSDL');
    finally
      sl.Free;
      dl.Free;
    end;
  end;
var
  xOperId, xErrorMessage, dCorrelation, dSep, xParams: String;
  xXml, oXml, dXml, eXml: TXml;
  xOperation, oOperation, dOperation: TWsdlOperation;
  dRequest: TWsdlMessage;
  xStream: TMemoryStream;
  x, f: Integer;
begin
  {$ifdef windows}
  CoInitialize(nil);
  try
  {$endif}
  AResponseInfo.ContentEncoding := 'identity';
  try
    if (ARequestInfo.Document = '/' + _ProgName + 'WebService')
    or (ARequestInfo.Document = '/' + 'wsdlStubWebService')
    then begin
      try
        xXml := TXml.Create;
        try
          if ArequestInfo.QueryParams = 'WSDL' then
          begin
            AResponseInfo.ContentText := _prepWsdl(ReadStringFromFile(webserviceWsdlFileName));
            Exit;
          end;
          if ArequestInfo.QueryParams = 'XSD' then
          begin
            AResponseInfo.ContentText := _prepXsd(webserviceXsdFileName);
            Exit;
          end;
          xParams := se.httpRequestStreamToString(ARequestInfo, AResponseInfo);
          xXml.LoadFromString(xParams, nil);
          if xXml.Name = '' then
            raise Exception.Create('Could not parse: ' + xParams);
          xXml.SeparateNsPrefixes;
          xXml.ResolveNameSpaces;
          oXml := xXml.FindXml('Envelope.Body');
          if not Assigned (oXml) then
            raise Exception.Create('Not a soap message: ' + xParams);
          if oXml.Items.Count = 0 then
            raise Exception.Create('No operation in: ' + xParams);
          xOperId := oXml.Items.XmlItems[0].Name;
          xOperation := webserviceWsdl.OperationByRequest[xOperId];
          if not Assigned (xOperation) then
            raise Exception.Create(format ('Unknown Operation: %s (%s)', [xOperId, xParams]));
          oOperation := TWsdlOperation.Create(xOperation); //creates a private copy
          (oOperation.rpyBind as TXml).CheckDownline(True);
          try
            try
              oOperation.RequestStringToBindables(xParams);
              if xOperId = 'clearLogsReq' then
              begin
                if not Assigned (OnClearLogEvent) then
                  raise Exception.Create('clearLogsReq refused because ' + _progName + ' has no OnClearLogEvent procedure assigned');
                OnClearLogEvent(True);
              end;
              if xOperId = 'activateReq' then
              begin
                if not Assigned (OnActivateEvent) then
                  raise Exception.Create('activateReq refused because ' + _progName + ' has no OnActivateEvent procedure assigned');
                dXml := oXml.FindXml('Body.activateReq.Activate');
                if not Assigned (dXml) then
                  raise Exception.Create('Missing Activate/Deactivate argument (boolean) in request');
                OnActivateEvent (dXml.ValueAsBoolean);
              end;
              if xOperId = 'openProjectReq' then
              begin
                if not Assigned (OnOpenProjectEvent) then
                  raise Exception.Create('openProjectReq refused because ' + _progName + ' has no OnOpenProjectEvent procedure assigned');
                dXml := oXml.FindXml('Body.openProjectReq.projectFileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                OnOpenProjectEvent (dXml.Value);
              end;
              if xOperId = 'regressionReportReq' then
              begin
                dXml := oXml.FindXml('Body.regressionReportReq.referenceFileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                oXml := se.MessagesRegressionReportAsXml (ExpandRelativeFileName(se.projectFileName,dXml.Value), False);
                oXml.Name := (oOperation.rpyBind as TXml).Items.XmlItems[0].Name;
                try
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].ResetValues;
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].LoadValues(oXml, False, False);
                finally
                  oXml.Free;
                end;
              end;
              if xOperId = 'resetEnvVarReq' then
              begin
                dXml := oXml.FindXml('Body.resetEnvVarReq.Name');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find name to use in request');
                wsdlz.resetEnvVar(dXml.Value);
              end;
              if xOperId = 'resetEnvVarsReq' then
              begin
                dXml := oXml.FindXml('Body.resetEnvVarsReq.RegularExpression');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find regular expression to use in request');
                wsdlz.resetEnvVars(dXml.Value);
              end;
              if xOperId = 'saveLogsToFileReq' then
              begin
                dXml := oXml.FindXml('Body.saveLogsToFileReq.fileName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find filename to use in request');
                se.SaveMessagesLog(ExpandRelativeFileName(se.projectFileName, dXml.Value));
              end;
              if xOperId = 'sendAllRequestsReq' then
              begin
                dXml := oXml.FindXml('Body.sendAllRequestsReq.elementName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find elementname to use in request');
                if allOperations.Find(dXml.Value, f) then
                  dOperation := allOperations.Operations [f]
                else
                  raise Exception.Create('Cannot find operation based on: ' + dXml.Value);
                if dOperation.StubAction <> saRequest  then
                  raise Exception.Create('sendAllRequestsReq refused because requested operation is not configured as request');
                if not se.IsActive then
                  raise Exception.Create('sendAllRequestsReq refused because ' + _progName + ' is inactive');
                se.ExecuteAllOperationRequests (dOperation);
              end;
              if xOperId = 'sendRequestReq' then
              begin
                dXml := oXml.FindXml('Body.sendRequestReq.elementName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find elementname to use in request');
                if allOperations.Find(dXml.Value, f) then
                  dOperation := allOperations.Operations [f]
                else
                  raise Exception.Create('Cannot find operation based on: ' + dXml.Value);
                if dOperation.StubAction <> saRequest  then
                  raise Exception.Create('Refused because requested operation is not configured as request');
                dXml := oXml.FindXml('Body.sendRequestReq.correlationValues');
                if (not Assigned (dXml))
                and (dOperation.CorrelationBindables.Count > 0) then
                  raise Exception.Create('No correlationValues found in request');
                if Assigned (dXml)
                and (dXml.Items.Count <> dOperation.CorrelationBindables.Count)  then
                  raise Exception.Create ( 'Number of correlation items is '
                                         + IntToStr (dXml.Items.Count)
                                         + '; must be '
                                         + IntToStr (dOperation.CorrelationBindables.Count)
                                         );
                dSep := '';
                dCorrelation := '';
                if Assigned (dXml) then
                begin
                  for x := 0 to dXml.Items.Count - 1 do
                  begin
                    dOperation.CorrelationBindables.Bindables [x].Value := dXml.Items.XmlItems [x].Value;
                    dOperation.CorrelationBindables.Bindables [x].Checked := True;
                    dCorrelation := dCorrelation + dSep + dXml.Items.XmlItems [x].Value;
                    dSep := ';';
                  end;
                end;
                dRequest := dOperation.MessageBasedOnRequest;
                if not Assigned (dRequest) then
                  raise Exception.Create ('Could not find message based on correlation: ' + dXml.Text);
                if not se.IsActive then
                  raise Exception.Create('sendRequestReq refused because ' + _progName + ' is inactive');
                se.SendMessage (dOperation, dRequest, dCorrelation);
              end;
              if xOperId = 'setEnvVarReq' then
              begin
                dXml := oXml.FindXml('Body.setEnvVarReq.Name');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find name to use in request');
                eXml := oXml.FindXml('Body.setEnvVarReq.Value');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find value to use in request');
                wsdlz.setEnvVar(dXml.Value, eXml.Value);
              end;
              if xOperId = 'shutDownReq' then
              begin
                OnQuitEvent(True);
              end;
              if xOperId = 'unexpectedValuesReportReq' then
              begin
                oXml := se.displayedLogs.UnexpectedsAsXml;
                oXml.Name := (oOperation.rpyBind as TXml).Items.XmlItems[0].Name;
                try
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].ResetValues;
                  (oOperation.rpyBind as TXml).Items.XmlItems[0].LoadValues(oXml, False, False);
                finally
                  oXml.Free;
                end;
              end;
              if xOperId = 'executeScriptReq' then
              begin
                if not se.IsActive then
                  raise Exception.Create('executeScriptReq refused because ' + _progName + ' is inactive');
                dXml := oXml.FindXml('Body.executeScriptReq.scriptName');
                if not Assigned (dXml) then
                  raise Exception.Create('Cannot find scriptname in request');
                if se.Scripts.Find(dXml.Value, f) then
                  se.ScriptExecuteText((se.Scripts.Objects[f] as TStringList).Text)
                else
                  raise Exception.Create('Cannot find script based on: ' + dXml.Value);
              end;
              AResponseInfo.ContentText := oOperation.StreamReply(_progName, True);
            except
              on e: exception do
              begin
                xErrorMessage := e.Message;
                try
                  dXml := oOperation.fltBind as TXml;
                  dXml.CheckDownline(True);
                  dXml.FindXml('Faults.*.applicationFault.code').Value := xOperId;
                  dXml.FindXml('Faults.*.applicationFault.text').Value := xErrorMessage;
                  AResponseInfo.ContentText := oOperation.StreamFault(_progName, true);
                  AResponseInfo.ResponseNo := 500;
                except
                  AResponseInfo.ContentText := xErrorMessage;
                  AResponseInfo.ResponseNo := 500;
                end;
              end;
            end;
          finally
            oOperation.Free;
          end;
        finally
          FreeAndNil (xXml);
        end;
      except
        on e: exception do
        begin
          AResponseInfo.ContentText := e.Message + #10#13 + se.ExceptionStackListString(e);
          AResponseInfo.ResponseNo := 500;
        end;
      end;
    end
    else
      AResponseInfo.ContentText := createHtmlResponse (se, ARequestInfo);
  finally
    if AResponseInfo.ContentEncoding <> 'identity' then
    begin
      aResponseInfo.ContentStream := TMemoryStream.Create;
      xStream := TMemoryStream.Create;
      try
        se.WriteStringToStream(AResponseInfo.ContentText, xStream);
        if AResponseInfo.ContentEncoding = 'deflate' then
          GZIPUtils.deflate(xStream, aResponseInfo.ContentStream as TMemoryStream);
        if AResponseInfo.ContentEncoding = 'gzip' then
          GZIPUtils.GZip(xStream, aResponseInfo.ContentStream as TMemoryStream);
      finally
        xStream.Free;
      end;
      aResponseInfo.ContentText := '';
    end;
  end;
  {$ifdef windows}
  finally
  CoUninitialize;
  end;
  {$endif}
end;

constructor TWsdlControl .Create;
begin
  HttpWebPageServer := TIdHTTPServer.Create(nil);
  with HttpWebPageServer do
  begin
    OnCommandGet := HttpWebPageServerCommandGet;
    OnCommandOther := HttpWebPageServerCommandGet;
  end;
  PortNumber := 3738;
end;

end.

