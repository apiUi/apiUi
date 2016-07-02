program nsSql;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , nsSqlMainUnit , QueryScanner , lazrichedit , virtualtreeview_package
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application .CreateForm (TmainUnit , mainUnit );
  Application.Run;
end.

