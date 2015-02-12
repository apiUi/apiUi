program testproject;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  testunit in 'testunit.pas' {Form1},
  StompClient in 'StompClient.pas',
  StompTypes in 'StompTypes.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
