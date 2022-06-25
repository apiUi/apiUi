{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
program yamlTest ;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads ,
  {$ENDIF}{$ENDIF}
  Interfaces , // this includes the LCL widgetset
  Forms, FrameViewer09 , virtualtreeview_package , TestUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True ;
  Application .Initialize ;
  Application .CreateForm (TForm1 , Form1 );
  Application .Run ;
end.

