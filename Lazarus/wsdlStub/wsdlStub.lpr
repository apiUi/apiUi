program wsdlStub;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, abbrevia, virtualtreeview_package,
  IdExceptionCore, IdStack, IdHTTP, WsdlStubMainUnit, snapshotz, exceptionUtils,
  htmlXmlUtilz, htmlreportz , junitunit, StringListListUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

