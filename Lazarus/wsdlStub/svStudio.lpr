program svStudio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , virtualtreeview_package , lazrichedit ,  wsdlStubMainUnit ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TMainForm , MainForm );
  Application.Run;
end.

