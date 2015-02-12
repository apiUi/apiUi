unit IpmGunLicense;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses FormIniFilez , FileUtil
   ;

function generateIpmLicense (aBase: String): String;
function validateIpmLicense (aBase: String; aLicense: String): Boolean;
function ValidateLicense (IniFile: TFormIniFile; prgName, dbName: String): Boolean;
function UpdateLicense  (IniFile: TFormIniFile; prgName, dbName: String): Boolean;
function LogUsage (dbName: String): Boolean;

implementation

uses
{$IFnDEF FPC}
  ADOdb,
{$ELSE}
  sqldb,
{$ENDIF}
  SysUtils
   , HashUtilz
   , StrUtils
   , igGlobals
   , Dialogs
   , Forms
   , Controls
   , IpmGunLicenseUnit
   ;

var
  ErrorReadingLicenseInfo: Boolean;
  Db: {$ifndef fpc}TADOConnection{$else}TSQLConnection{$endif};
  Qry: {$ifndef fpc}TADOQuery{$else}TSQLQuery{$endif};


procedure initDb (var dbName: String);
const ConnStr = 'Provider=%s;Data Provider=%s;Data Source=%s';
const Provider = 'MSDataShape.1';
const DataProvider = 'Microsoft.Jet.OLEDB.4.0';
begin
  dbName := ExpandUNCFileNameUTF8(dbName); { *Converted from ExpandUNCFileName* }
  {$ifndef fpc}
  Db := TADOConnection.Create(nil);
  Qry := TADOQuery.Create(nil);
  Db.Connected := False;
  Db.ConnectionString
    := Format( ConnStr
             , [ Provider
               , DataProvider
               , dbName
               ]
             );
  {$else}
  Db := TSQLConnection.Create(nil);
  Qry := TSQLQuery.Create(nil);
  Db.Connected := False;
(*
  Db.ConnectionString
    := Format( ConnStr
             , [ Provider
               , DataProvider
               , dbName
               ]
             );
*)
  {$endif}
  try
    Db.Connected := True;
    {$ifndef fpc}
    Qry := TADOQuery.Create (nil);
    Qry.Connection := Db;
    {$else}
    Qry := TSQLQuery.Create (nil);
//  Qry.Connection := Db;
    {$endif}
  except
    on e: Exception do
    begin
      ShowMessage(format('Exception %s in initDb. Exception is:"%s".', [e.ClassName, e.Message]));
      raise;
    end;
  end;
end;

procedure termDb;
begin
  FreeAndNil (Qry);
  FreeAndNil (Db);
end;

function UpdateLicense  (IniFile: TFormIniFile; prgName, dbName: String): Boolean;
var
  xLicenseDate: TDateTime;
  xCompanyName: String;
  xLicenseExpirationDate: String;
  xLicenseString: String;
begin
  initDb (dbName);
  try
    if ErrorReadingLicenseInfo then
      raise Exception.Create
              ( prgName
              + ' could not read the license information.'
              + #$D#$A
              + #$D#$A
              + 'Please contact your '
              + prgName
              + ' provider for assistance.'
              );

    Application.CreateForm(TIpmGunLicenseForm, IpmGunLicenseForm);
    try
      IpmGunLicenseForm.Caption := prgName + ' - License information';
      IpmGunLicenseForm.Company := xCompanyName;
      IpmGunLicenseForm.LicenseExpirationDate := xLicenseExpirationDate;
      IpmGunLicenseForm.DbName := dbName;
      IpmGunLicenseForm.ShowModal;
      if IpmGunLicenseForm.ModalResult = mrOK then
      begin
        try
          Qry.SQL.Clear;
          Qry.SQL.Add('Update LicenseInformation');
          Qry.SQL.Add('set CompanyName = :CompanyName');
          Qry.SQL.Add('  , LicenseExpireDate = :LicenseExpireDate');
          Qry.SQL.Add('  , LicenseString = :LicenseString');
          {$ifndef fpc}
          Qry.Parameters.ParamValues ['CompanyName'] := IpmGunLicenseForm.Company;
          Qry.Parameters.ParamValues ['LicenseExpireDate'] := IpmGunLicenseForm.LicenseExpirationDate;
          Qry.Parameters.ParamValues ['LicenseString'] := IpmGunLicenseForm.LicenseString;
          {$else}
          {$endif}
          Qry.ExecSql;
        except
          on E: Exception do
          begin
            ShowMessage (E.Message);
          end;
        end;
      end;
    finally
      FreeAndNil (IpmGunLicenseForm);
    end;
  finally
    termDb;
  end;
end;


function ValidateLicenseExpirationDate(eDt: String): Boolean;
var
  xDt: TDateTime;
  xYear, xMonth, xDay: Word;
begin
// 2007-01-01
// 1234567890
  xYear := StrToInt (Copy (eDt, 1, 4));
  xMonth := StrToInt (Copy (eDt, 6, 2));
  xDay := StrToInt (Copy (eDt, 9, 2));
  xDt := EncodeDate(xYear, xMonth, xDay);
  result := (Now < xDt);
  if not result then
  begin
    ShowMessage ( 'Your license has expired on '
                + DateToStr (xDt)
                + '.'
                + #$D#$A
                + #$D#$A
                + 'Therefore you will have limited functionallity'
                + #$D#$A
                + #$D#$A
                + 'Please contact your provider.'
                );
  end
  else
  begin
    if ((xDt - Now) < 30) then
      ShowMessage ( 'Your license expires on '
                  + DateToStr (xDt)
                  + #$D#$A
                  + 'Please contact your provider'
                  );
  end;
end;

function ValidateLicense (IniFile: TFormIniFile; prgName, dbName: String): Boolean;
var
  y,m,d: Word;
  ymd: Integer;
  xLicenseDate: TDateTime;
  xCompanyName: String;
  xLicenseExpirationDate: String;
  xLicenseString: String;
begin
  result := False;
  initDb (dbName);
  result := False;
  try
    ErrorReadingLicenseInfo := False;
    try
      Qry.SQL.Clear;
      Qry.SQL.Add('Select CompanyName, LicenseExpireDate, LicenseString');
      Qry.SQL.Add('from LicenseInformation');
      Qry.Open;
      while not Qry.EOF do
      begin
        xCompanyName := Qry.FieldByName ('CompanyName').AsString;
        xLicenseExpirationDate := Qry.FieldByName ('LicenseExpireDate').AsString;
        xLicenseString := Qry.FieldByName ('LicenseString').AsString;
        Qry.Next;
      end;
      Qry.Close;
      result := (validateIpmLicense( xCompanyName
                                        + xLicenseExpirationDate
                                        + generateIpmLicense (dbName)
                                        , xLicenseString
                                        )
                     )
                 and (   (AnsiStartsStr('\\', dbName))
                      or (GetUsername = 'Jan')
                      or (GetUsername = 'Bouwman')
                      or (GetUsername = 'BouwmanJW')
                     )
                   ;
    except
      on e: Exception do
      begin
        ShowMessage(format('Exception %s in ValidateLicense. Exception is:"%s".', [e.ClassName, e.Message]));
        ErrorReadingLicenseInfo := True;
    {}
        result := True;
        xLicenseExpirationDate := '2010-08-01';
    {}
      end;
    end;

    if result
    and (not ErrorReadingLicenseInfo) then
    begin
      result := ValidateLicenseExpirationDate (xLicenseExpirationDate);
      if result then  // set a license for four days for stand alone pc usage
      begin
        DecodeDate (now + 14, y, m, d);
        IniFile.IntegerByName['LicenseExpirationDate'] := 10000 * y + 100 * m + d;
        IniFile.StringByName['LicenseKey'] := generateIpmLicense(IntToStr (10000 * y + 100 * m + d));
      end;
    end
    else
    begin
      if ErrorReadingLicenseInfo then
      begin
        // first try if we have a stand alone license
        ymd := IniFile.IntegerByName['LicenseExpirationDate'];
        d := ymd mod 100;
        ymd := ymd div 100;
        m := ymd mod 100;
        y := ymd div 100;
        xLicenseDate := EncodeDate(y,m,d);
        if (Now <= xLicenseDate)
        and ((Now + 16) > xLicenseDate) then
          result := validateIpmLicense(IntToStr (10000 * y + 100 * m + d), IniFile.StringByName['LicenseKey']);
        if result then
          ShowMessage ( prgName
                      + ' could not read license information from server.'
                      + #$D#$A
                      + #$D#$A
                      + 'Your offline license is valid until ' + DateToStr (xLicenseDate)
                      )
        else
          ShowMessage ( prgName
                      + ' could not read the license information.'
                      + #$D#$A
                      + #$D#$A
                      + 'Therefore you will have limited functionality.'
                      + #$D#$A
                      + 'Please contact your '
                      + prgName
                      + ' provider for assistance.'
                      );
      end
      else
      begin
  {
        ShowMessage ( 'wsdlStub did not find a valid licensestring.'
                    + #$D#$A
                    + #$D#$A
                    + 'Therefore wsdlStub will have limited functionallity.'
                    + #$D#$A
                    + #$D#$A
                    + 'Please contact your wsdlStub provider for'
                    + #$D#$A
                    + 'a valid licensestring or technical assistance.'
                    );
  }
      end;
    end;
  finally
    termDB;
  end;
end;

function generateIpmLicense (aBase: String): String;
var
  S: String;
begin
  S := Sha1('hdal%)j1asas90' + LowerCase (aBase) + 'cnfdj 3h*q!H');
  result := Copy (S, 4, 4)
          + '-'
          + Copy (S, 8, 4)
          + '-'
          + Copy (S, 12, 4)
          + '-'
          + Copy (S, 16, 4)
          + '-'
          + Copy (S, 18, 4)
          + '-'
          + Copy (S, 26, 4)
          + '-'
          + Copy (S, 30, 4)
          + '-'
          + Copy (S, 34, 4)
          ;
end;

function validateIpmLicense (aBase: String; aLicense: String): Boolean;
begin
  result := (aLicense = generateIpmLicense(aBase));
end;

function LogUsage(dbName: String): Boolean;
var
  xUpdated: TDateTime;
  xUsageDate: TDateTime;
begin
  result := False;
  initDb (dbName);
  try
    xUpdated := Now;
    xUsageDate := Date;
{
    if (getUserName <> 'Jan')
    and (getUserName <> 'Bouwman') then
}
    begin
      try
        try
          Qry.SQL.Clear;
          Qry.SQL.Add('Insert into UsageNames');
          Qry.SQL.Add('( UserName');
          Qry.SQL.Add(', nUsage');
          Qry.SQL.Add(', Updated');
          Qry.SQL.Add(') values');
          Qry.SQL.Add('( :UserName');
          Qry.SQL.Add(', 1');
          Qry.SQL.Add(', :Updated');
          Qry.SQL.Add(')');
          {$ifndef fpc}
          Qry.Parameters.ParamValues ['UserName'] := getUserName;
          Qry.Parameters.ParamValues ['Updated'] := xUpdated;
          {$else}
          Qry.Params.ParamValues ['UserName'] := getUserName;
          Qry.Params.ParamValues ['Updated'] := xUpdated;
          {$endif}
          Qry.ExecSql;
        except
          on E: Exception do
          begin
            try
              Qry.SQL.Clear;
              Qry.SQL.Add('Update UsageNames');
              Qry.SQL.Add('set nUsage = nUsage + 1');
              Qry.SQL.Add('  , Updated = :Updated');
              Qry.SQL.Add('where UserName = :UserName');
              {$ifndef fpc}
              Qry.Parameters.ParamValues ['Updated'] := xUpdated;
              Qry.Parameters.ParamValues ['UserName'] := getUserName;
              {$else}
              Qry.Params.ParamValues ['Updated'] := xUpdated;
              Qry.Params.ParamValues ['UserName'] := getUserName;
              {$endif}
              Qry.ExecSql;
            except
            end;
          end; {try to update UsageNames when insert failed}
        end; {try to insert UsageNames}

        try
          Qry.SQL.Clear;
          Qry.SQL.Add('Insert into UsageDates');
          Qry.SQL.Add('( UsageDate');
          Qry.SQL.Add(', nUsage');
          Qry.SQL.Add(', Updated');
          Qry.SQL.Add(') values');
          Qry.SQL.Add('( :UsageDate');
          Qry.SQL.Add(', 1');
          Qry.SQL.Add(', :Updated');
          Qry.SQL.Add(')');
          {$ifndef fpc}
          Qry.Parameters.ParamValues ['UsageDate'] := xUsageDate;
          Qry.Parameters.ParamValues ['Updated'] := xUpdated;
          {$else}
          Qry.Params.ParamValues ['UsageDate'] := xUsageDate;
          Qry.Params.ParamValues ['Updated'] := xUpdated;
          {$endif}
          Qry.ExecSql;
        except
          on E: Exception do
          begin
            try
              Qry.SQL.Clear;
              Qry.SQL.Add('Update UsageDates');
              Qry.SQL.Add('set nUsage = nUsage + 1');
              Qry.SQL.Add('  , Updated = :Updated');
              Qry.SQL.Add('where UsageDate = :UsageDate');
              {$ifndef fpc}
              Qry.Parameters.ParamValues ['Updated'] := xUpdated;
              Qry.Parameters.ParamValues ['UsageDate'] := xUsageDate;
              {$else}
              Qry.Params.ParamValues ['Updated'] := xUpdated;
              Qry.Params.ParamValues ['UsageDate'] := xUsageDate;
              {$endif}
              Qry.ExecSql;
            except
            end;
          end; {try to update UsageDates when insert failed}
        end; {try to insert UsageDates}
      except
      end; //try
    end; {if user <> JAN BOUWMAN}
    result := True;
  finally
    termDb;
  end;
end;

end.
