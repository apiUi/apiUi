program wsdlStub;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  {$ifdef heaptrace}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  heaptrc,
  {$endif heaptrace}
  Interfaces, // this includes the LCL widgetset
  Forms , virtualtreeview_package , lazrichedit , WsdlStubMainUnit , IdStack ,
  SysUtils ;

{$R *.res}

begin
  {$ifdef heaptrace}
  // Define heaptrace instead of -gh to redirect output for the Leakview package:
  if FileExists(ParamStr(0) + 'heap.trc') then
    DeleteFile(ParamStr(0) + 'heap.trc');
  SetHeapTraceOutput(ParamStr(0) + 'heap.trc');
  {$endif heaptrace}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TMainForm , MainForm );
  Application.Run;
end.

