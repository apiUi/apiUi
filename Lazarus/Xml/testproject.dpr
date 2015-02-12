program testproject;

uses
  Forms,
  testunit in 'testunit.pas' {Form1},
  Xmlz in 'Xmlz.pas',
  Xsdz in 'Xsdz.pas',
  RegExpr in '..\RegExpr\RegExpr.pas',
  Ipmz in '..\IpmGun\Ipmz.pas',
  IpmAnalyser in '..\IpmGun\IpmScanner\IpmAnalyser.pas',
  IPMPARSER in '..\IpmGun\IpmScanner\IPMPARSER.PAS',
  IPMSCANNER in '..\IpmGun\IpmScanner\IPMSCANNER.PAS',
  IpmTypes in '..\IpmGun\IpmTypes.pas',
  ShowPdfFileUnit in '..\IpmGun\ShowPdfFileUnit.pas' {ShowPfdFileForm},
  FormIniFilez in '..\IpmGun\FormIniFilez.pas',
  ShowHtmlUnit in '..\IpmGun\ShowHtmlUnit.pas' {ShowHtmlForm},
  FindRegExpDialog in '..\IpmGun\FindRegExpDialog.pas' {FindDlg},
  ipmInfoUnit in '..\IpmGun\ipmInfoUnit.pas' {ipmInfoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TShowPfdFileForm, ShowPfdFileForm);
  Application.CreateForm(TShowHtmlForm, ShowHtmlForm);
  Application.CreateForm(TFindDlg, FindDlg);
  Application.CreateForm(TipmInfoForm, ipmInfoForm);
  Application.Run;
end.
