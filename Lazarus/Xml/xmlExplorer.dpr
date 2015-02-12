program xmlExplorer;

uses
  Forms,
  xmlExplorerUnit in 'xmlExplorerUnit.pas' {xmlExplorerForm},
  igGlobals in '..\IpmGun\igGlobals.pas',
  RegExpr in '..\RegExpr\RegExpr.pas',
  ShowHtmlUnit in '..\IpmGun\ShowHtmlUnit.pas' {ShowHtmlForm},
  WebBrowserWrapperUnit in '..\Library\WebBrowser\WebBrowserWrapperUnit.pas',
  ChooseStringUnit in '..\IpmGun\ChooseStringUnit.pas' {ChooseStringForm},
  xsdDateTimeFormUnit in 'xsdDateTimeFormUnit.pas' {xsdDateTimeForm},
  ShowXmlUnit in 'ShowXmlUnit.pas' {ShowXmlForm},
  ipmInfoUnit in '..\IpmGun\ipmInfoUnit.pas' {ipmInfoForm},
  ShowPdfFileUnit in '..\IpmGun\ShowPdfFileUnit.pas',
  Ipmz in '..\IpmGun\Ipmz.pas',
  ErrorFound in '..\IpmGun\ErrorFound.pas' {ErrorFoundDlg},
  IpmAnalyser in '..\IpmGun\IpmScanner\IpmAnalyser.pas',
  IPMPARSER in '..\IpmGun\IpmScanner\IPMPARSER.PAS',
  IPMSCANNER in '..\IpmGun\IpmScanner\IPMSCANNER.PAS',
  FindRegExpDialog in '..\IpmGun\FindRegExpDialog.pas' {FindDlg},
  Xmlz in 'Xmlz.pas',
  forceMsXml6 in 'forceMsXml6.pas',
  IpmGunLicense in '..\IpmGun\IpmGunLicense.pas',
  IpmGunLicenseUnit in '..\IpmGun\IpmGunLicenseUnit.pas' {IpmGunLicenseForm},
  ShowStringListUnit in 'ShowStringListUnit.pas' {ShowStringListForm},
  FilterDialog in 'FilterDialog.pas' {FilterDlg},
  ShowRtfUnit in 'ShowRtfUnit.pas' {ShowRtfForm},
  FormIniFilez in '..\IpmGun\FormIniFilez.pas',
  HashUtilz in '..\Library\Hash\HashUtilz.pas',
  A2BXmlz in 'A2BXmlz.pas',
  ShowA2BXmlUnit in 'ShowA2BXmlUnit.pas' {ShowA2BXmlForm},
  Diff_ONP in '..\Library\A2BStringList\Diff_ONP.pas',
  OpenTwoFoldersUnit in 'OpenTwoFoldersUnit.pas' {OpenTwoFoldersForm},
  ShowFolderDifferencesUnit in 'ShowFolderDifferencesUnit.pas' {ShowFolderDifferencesForm},
  dualListUnit in '..\IpmGun\dualListUnit.pas' {dualListForm},
  vstUtils in '..\Library\vstUtils\vstUtils.pas',
  jsonAnalyser in 'json\jsonAnalyser.pas',
  JSONPARSER in 'json\JSONPARSER.PAS',
  JSONSCANNER in 'json\JSONSCANNER.PAS';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TxmlExplorerForm, xmlExplorerForm);
  Application.CreateForm(TFilterDlg, FilterDlg);
  Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
  Application.CreateForm(TShowFolderDifferencesForm, ShowFolderDifferencesForm);
  Application.CreateForm(TdualListForm, dualListForm);
  Application.Run;
end.
