program wsdlStub;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , virtualtreeview_package , lazrichedit, WsdlStubMainUnit, IdStack;

{$R *.res}

begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('c:\janbo\heap.trc') then
    DeleteFile('c:\janbo\heap.trc');
  SetHeapTraceOutput('c:\janbo\heap.trc');
  {$ENDIF DEBUG}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TMainForm , MainForm );
  Application.Run;
end.

