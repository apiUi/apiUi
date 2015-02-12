program stompTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , MainFormClient ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TForm5 , Form5 );
  Application.Run;
end.

