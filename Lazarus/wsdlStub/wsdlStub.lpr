program wsdlStub;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , virtualtreeview_package , lazrichedit, WsdlStubMainUnit,
PromptFolderUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TMainForm , MainForm );
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

