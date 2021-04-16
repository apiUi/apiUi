unit Listenerz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils
   , Xmlz
   , mqinterface, mqapi
   , StompInterface
   , IdSSLOpenSSL
   , xmlio
   , Wsdlz
   ;

type

  { TListeners }

  TListeners = class(TObject)
  private
    fConnected : Boolean ;
    procedure setConnected (AValue : Boolean );
  public
    httpProxyPort, httpBmtpPort: Integer;
    sslVersion: TIdSSLVersion;
    sslCertificateFile, sslKeyFile, sslRootCertificateFile, sslPassword: String;
    httpPorts, httpsPorts: TJBStringList;
    mqInterfaces: TJBStringList;
    stompInterfaces: TJBStringList;
    SpecificationXml: TXml;
    property Connected: Boolean read fConnected write setConnected;
    procedure OnGetSslPassword (var aPassword: String);
    procedure Clear;
    procedure FromXml (aOnHaveFrame: TOnHaveFrame);
    constructor Create;
    destructor Destroy; Override;
  end;



implementation

uses xmlzConsts
   ;

{ TListeners }

{
function TListeners.AsXml: TXml;
var
  x: Integer;
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
          AddXml(TXml.CreateAsString('Version', sslVersionToString(sslVersion)));
          AddXml(TXml.CreateAsString('CertificateFile', sslCertificateFile));
          AddXml(TXml.CreateAsString('KeyFile', sslKeyFile));
          AddXml(TXml.CreateAsString('RootCertificateFile', sslRootCertificateFile));
          if sslPassword <> '' then
            AddXml (TXml.CreateAsString('Password', Xmlz.EncryptString(sslPassword)));
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
}

procedure TListeners.FromXml(aOnHaveFrame: TOnHaveFrame);
var
  m, x, y: Integer;
  xXml, yXml, hXml: TXml;
begin
  if not Assigned (SpecificationXml) then raise Exception.Create('ListenersFromXml: No SpecificationXml assigned');
  if SpecificationXml.Name <> 'Listeners' then raise Exception.Create('ListenersFromXml: Illegal XML assigned');
  Clear;
  if not SpecificationXml.Checked then Exit;
  hXml := TXml.Create;
  try
    hXml.CopyDownLine(SpecificationXml, True);
    hXml.ResolveAliasses;
    for x := 0 to hXml.Items.Count - 1 do
    begin
      with hXml.Items.XmlItems[x] do
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
              sslVersion := sslvTLSv1_2; // nice default
              yXml := Items.XmlCheckedItemByTag['Version'];
              if Assigned (yXml) then
                sslVersion := sslVersionFromString(yXml.Value);
              sslCertificateFile := Items.XmlCheckedValueByTag['CertificateFile'];
              sslKeyFile := Items.XmlCheckedValueByTag['KeyFile'];
              sslRootCertificateFile := Items.XmlCheckedValueByTag['RootCertificateFile'];
              yXml := Items.XmlCheckedItemByTag['Password'];
              if Assigned (yXml) then
              begin
                try
                  sslPassword :=  Xmlz.DecryptString(yXml.Value);
                except
                  sslPassword :=  '';
                end;
              end;
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
        end;
      end;
    end;
  finally
    hXml.Free;
  end;
end;

procedure TListeners .setConnected (AValue : Boolean );
begin
  if fConnected = AValue then Exit ;
  fConnected := AValue ;
end;

procedure TListeners.OnGetSslPassword(var aPassword: String);
begin
  aPassword := sslPassword;
end;

procedure TListeners.Clear;
var
  x: Integer;
begin
  httpPorts.Clear;
  httpsPorts.Clear;
  httpProxyPort := 0;
  httpBmtpPort := 0;
  sslCertificateFile := '';
  sslKeyFile := '';
  sslRootCertificateFile := '';
  sslPassword := '';
  for x := 0 to mqInterfaces.Count - 1 do
    mqInterfaces.Objects [x].Free;
  mqInterfaces.Clear;
  for x := 0 to stompInterfaces.Count - 1 do
    stompInterfaces.Objects [x].Free;
  stompInterfaces.Clear;
end;

constructor TListeners.Create;
begin
  SpecificationXml := TXml.CreateAsString('Listeners', '');
  httpPorts:= TJBStringList.Create;
  httpsPorts := TJBStringList.Create;
  httpBmtpPort := 0;
  mqInterfaces := TJBStringList.Create;
  stompInterfaces := TJBStringList.Create;
end;

destructor TListeners.Destroy;
begin
  inherited;
  Clear;
  httpPorts.Free;
  httpsPorts.Free;
  mqInterfaces.Free;
  stompInterfaces.Free;
  SpecificationXml.Free;
end;

end.
