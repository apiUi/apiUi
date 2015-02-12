program testMQ;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , MqTestUnit ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Run;
end.

