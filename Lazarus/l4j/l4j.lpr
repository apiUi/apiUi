program l4j;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, L4JMainUnit, abbrevia, lazrichedit, IdHTTP, IdStack, l4jTypes,
  FilterDialog, DbFilterDialog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TL4JMainForm, L4JMainForm);
  Application.CreateForm(TFilterDlg, FilterDlg);
  Application.CreateForm(TDbFilterDlg, DbFilterDlg);
  Application.Run;
end.

