program l4jConnectionStrings;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, EditConnectionStringUnit, lazrichedit, virtualtreeview_package;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TOpenSQLServerForm, OpenSQLServerForm);
  Application.Run;
end.

