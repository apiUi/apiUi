program l4j;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms , l4jExplorerUnit , abbrevia , virtualtreeview_package , lazrichedit ,
  IpmGunLicense ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Run;
end.

