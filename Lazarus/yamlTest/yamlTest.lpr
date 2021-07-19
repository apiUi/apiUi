{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
program yamlTest ;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads ,
  {$ENDIF}{$ENDIF}
  Interfaces , // this includes the LCL widgetset
  Forms, FrameViewer09 , virtualtreeview_package , yamlTestUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True ;
  Application .Initialize ;
  Application .CreateForm (TForm1 , Form1 );
  Application .Run ;
end.

