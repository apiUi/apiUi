program wsdlStub;

{$mode objfpc}{$H+}
{$define UseCThreads}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , virtualtreeview_package , lazrichedit, WsdlStubMainUnit ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TMainForm , MainForm );
  Application.Run;
end.
