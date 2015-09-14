unit Listenerz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils
   , Xmlz
   , mqInterface, MQAPI
   , StompInterface, StompClient, StompTypes
   , IdSSLOpenSSL
   , Wsdlz
   ;

type
  TListeners = class(TObject)
  public
    httpProxyPort, httpBmtpPort: Integer;
    sslVersion: TIdSSLVersion;
    sslCertificateFile, sslKeyFile, sslRootCertificateFile: String;
    httpPorts, httpsPorts: TStringList;
    mqInterfaces: TStringList;
    stompInterfaces: TStringList;
    smtpPort: Integer;
    smtpsPort: Integer;
    smtpTlsCertificateFile, smtpTlsKeyFile, smtpTlsRootCertificateFile: String;
    pop3Port: Integer;
    pop3UserName, pop3Password: String;
    function AsXml: TXml;
    procedure Clear;
    procedure FromXml (aXml: TXml; aOnHaveFrame: TOnHaveFrame);
    constructor Create;
    destructor Destroy; Override;
  end;



implementation

{ TListeners }

function TListeners.AsXml: TXml;
var
  x: Integer;
  xMq: TMqInterface;
begin
  result := TXml.CreateAsString('Listeners', '');
  with result do
  begin
    if httpPorts.Count > 0 then
      with AddXml(TXml.CreateAsString('Http', '')) do
        for x := 0 to httpPorts.Count - 1 do
          AddXml(TXml.CreateAsString('Port', httpPorts.Strings[x]));
    if httpsPorts.Count > 0 then
    begin
      with AddXml(TXml.CreateAsString('Https', '')) do
      begin
        for x := 0 to httpsPorts.Count - 1 do
          AddXml(TXml.CreateAsString('Port', httpsPorts.Strings[x]));
        with AddXml(TXml.CreateAsString('SSL', '')) do
        begin
          if sslVersion = sslvSSLv3 then
            AddXml(TXml.CreateAsString('Version', 'SSL Version 3'));
          if sslVersion = sslvTLSv1 then
            AddXml(TXml.CreateAsString('Version', 'TLS Version 1'));
          AddXml(TXml.CreateAsString('CertificateFile', sslCertificateFile));
          AddXml(TXml.CreateAsString('KeyFile', sslKeyFile));
          AddXml(TXml.CreateAsString('RootCertificateFile', sslRootCertificateFile));
        end;
      end;
    end;
    if httpProxyPort > 0 then
      with AddXml(TXml.CreateAsString('HttpProxy', '')) do
        AddXml(TXml.CreateAsInteger('Port', httpProxyPort));
    if httpBmtpPort > 0 then
      with AddXml(TXml.CreateAsString('Bmtp', '')) do
        AddXml(TXml.CreateAsInteger('Port', httpBmtpPort));
    if mqInterfaces.Count > 0 then
      with AddXml(TXml.CreateAsString('Mq', '')) do
        for x := 0 to mqInterfaces.Count - 1 do
          AddXml ((mqInterfaces.Objects [x] as TMqInterface).AsXml);
    if stompInterfaces.Count > 0 then
      with AddXml(TXml.CreateAsString('Stomp', '')) do
        for x := 0 to stompInterfaces.Count - 1 do
           AddXml((stompInterfaces.Objects[x] as TStompInterface).AsXml);
    if (smtpPort > 0)
    or (smtpsPort > 0)
    or (pop3Port > 0)
    then begin
      with AddXml(TXml.CreateAsString('Mail', '')) do
      begin
        if smtpPort > 0 then
          with AddXml(TXml.CreateAsString('Smtp', '')) do
            AddXml(TXml.CreateAsInteger('Port', smtpPort));
        if smtpsPort > 0 then
        begin
          with AddXml(TXml.CreateAsString('Smtps', '')) do
          begin
            AddXml(TXml.CreateAsInteger('Port', smtpsPort));
            with AddXml(TXml.CreateAsString('TLS', '')) do
            begin
              AddXml(TXml.CreateAsString('CertificateFile', smtpTlsCertificateFile));
              AddXml(TXml.CreateAsString('KeyFile', smtpTlsKeyFile));
              AddXml(TXml.CreateAsString('RootCertificateFile', smtpTlsRootCertificateFile));
            end;
          end;
        end;
        if pop3Port > 0 then
        begin
          with AddXml(TXml.CreateAsString('Pop3', '')) do
          begin
            AddXml(TXml.CreateAsInteger('Port', pop3Port));
            with AddXml(TXml.CreateAsString('User', '')) do
            begin
              AddXml(TXml.CreateAsString('Name', pop3UserName));
              AddXml(TXml.CreateAsString('Password', EncryptString(pop3Password)));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TListeners.FromXml(aXml: TXml; aOnHaveFrame: TOnHaveFrame);
var
  m, x, y: Integer;
  mXml, xXml, yXml: TXml;
begin
  if not Assigned (aXml) then raise Exception.Create('ListenersFromXml: No XML assigned');
  if aXml.Name <> 'Listeners' then raise Exception.Create('ListenersFromXml: Illegal XML assigned');
  Clear;
  if not aXml.Checked then Exit;
  for x := 0 to aXml.Items.Count - 1 do
  begin
    with aXml.Items.XmlItems[x] do
    begin
      if Checked then
      begin
        if Name = 'Http' then
        begin
          for y := 0 to Items.Count - 1 do
          begin
            if (Items.XmlItems[y].Checked)
            and (Items.XmlItems[y].Name = 'Port') then
            begin
              httpPorts.Add (Items.XmlItems[y].Value);
              _WsdlPortNumber := Items.XmlItems[y].Value;
            end;
          end;
        end;
        if Name = 'HttpProxy' then
        begin
          httpProxyPort := Items.XmlCheckedIntegerByTag['Port'];
        end;
        if Name = 'Bmtp' then
        begin
          httpBmtpPort := Items.XmlCheckedIntegerByTag['Port'];
        end;
        if Name = 'Https' then
        begin
          for y := 0 to Items.Count - 1 do
          begin
            if (Items.XmlItems[y].Checked)
            and (Items.XmlItems[y].Name = 'Port') then
            begin
              httpsPorts.Add (Items.XmlItems[y].Value);
              _WsdlPortNumber := Items.XmlItems[y].Value;
            end;
          end;
          xXml := Items.XmlCheckedItemByTag['SSL'];
          if Assigned (xXml) then with xXml do
          begin
            yXml := Items.XmlCheckedItemByTag['Version'];
            if Assigned (yXml) then
            begin
              if yXml.Value = 'SSL Version 3' then
                sslVersion := sslvSSLv3;
              if yXml.Value = 'TLS Version 1' then
                sslVersion := sslvTLSv1;
            end;
            sslCertificateFile := Items.XmlCheckedValueByTag['CertificateFile'];
            sslKeyFile := Items.XmlCheckedValueByTag['KeyFile'];
            sslRootCertificateFile := Items.XmlCheckedValueByTag['RootCertificateFile'];
          end;
        end;
        if Name = 'Mq' then
          for y := 0 to Items.Count - 1 do
            if Items.XmlItems[y].Checked then
              mqInterfaces.AddObject ('', TMqInterface.CreateFromXml (Items.XmlItems [y]));
        if Name = 'Stomp' then
          for y := 0 to Items.Count - 1 do
            if Items.XmlItems[y].Checked then
              stompInterfaces.AddObject ('', TStompInterface.CreateFromXml (Items.XmlItems [y], aOnHaveFrame));
        if Name = 'Mail' then
        begin
          for m := 0 to Items.Count - 1 do
          begin
            if Items.XmlItems[m].Checked then
            begin
              with Items.XmlItems[m] do
              begin
                if Name = 'Smtp' then
                begin
                  smtpPort := Items.XmlCheckedIntegerByTag['Port'];
                end;
                if Name = 'Smtps' then
                begin
                  smtpsPort := Items.XmlCheckedIntegerByTag['Port'];
                  xXml := Items.XmlCheckedItemByTag['TLS'];
                  if Assigned (xXml) then with xXml do
                  begin
                    smtpTlsCertificateFile := Items.XmlCheckedValueByTag['CertificateFile'];
                    smtpTlsKeyFile := Items.XmlCheckedValueByTag['KeyFile'];
                    smtpTlsRootCertificateFile := Items.XmlCheckedValueByTag['RootCertificateFile'];
                  end;
                end;
                if Name = 'Pop3' then
                begin
                  pop3Port := Items.XmlCheckedIntegerByTag['Port'];
                  xXml := Items.XmlCheckedItemByTag['User'];
                  if Assigned (xXml) then with xXml do
                  begin
                    pop3UserName := Items.XmlCheckedValueByTag['Name'];
                    pop3Password := DecryptString (Items.XmlCheckedValueByTag['Password']);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TListeners.Clear;
var
  x: Integer;
begin
  httpPorts.Clear;
  httpsPorts.Clear;
  httpProxyPort := 0;
  httpBmtpPort := 0;
  smtpPort := 0;
  smtpsPort := 0;
  smtpTlsCertificateFile := '';
  smtpTlsKeyFile := '';
  smtpTlsRootCertificateFile := '';
  pop3Port := 0;
  pop3Username := '';
  pop3Password := '';
  sslCertificateFile := '';
  sslKeyFile := '';
  sslRootCertificateFile := '';
  for x := 0 to mqInterfaces.Count - 1 do
    mqInterfaces.Objects [x].Free;
  mqInterfaces.Clear;
  for x := 0 to stompInterfaces.Count - 1 do
    stompInterfaces.Objects [x].Free;
  stompInterfaces.Clear;
end;

constructor TListeners.Create;
begin
  httpPorts:= TStringList.Create;
  httpsPorts := TStringList.Create;
  httpBmtpPort := 0;
  mqInterfaces := TStringList.Create;
  stompInterfaces := TStringList.Create;
  smtpPort := 0;
  pop3Port := 0;
end;

destructor TListeners.Destroy;
begin
  inherited;
  Clear;
  httpPorts.Free;
  httpsPorts.Free;
  mqInterfaces.Free;
  stompInterfaces.Free;
end;

end.
