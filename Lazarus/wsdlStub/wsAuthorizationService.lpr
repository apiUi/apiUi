Program wsAuthorizationService;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp , lazdaemonapp , wsAuthDaemonUnit , wsDaemonUnit , HashUtilz ,
  lazrichedit
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
