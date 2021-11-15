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
unit SynEditTestUnit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , IDEWindowIntf , SynEdit, SynEditKeyCmds , SynHighlighterPas ,
  SynHighlighterHTML , SynHighlighterCpp , SynHighlighterAny ,
  SynHighlighterMulti , Forms , Controls , Graphics , Dialogs , EditBtn ,
  ButtonPanel , ComCtrls ;

type

  { TForm1 }

  TForm1 = class(TForm )
    FileNameEdit1 : TFileNameEdit ;
    FindDialog1 : TFindDialog ;
    ImageList : TImageList ;
    SynAnySyn1 : TSynAnySyn ;
    SynCppSyn1 : TSynCppSyn ;
    SynEdit1 : TSynEdit ;
    SynPasSyn1 : TSynPasSyn ;
    ToolBar1 : TToolBar ;
    ToolButton1 : TToolButton ;
    procedure FindDialog1Find (Sender : TObject );
    procedure ToolButton1Click (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1 .ToolButton1Click (Sender : TObject );
begin
  FindDialog1.Execute;
end;

procedure TForm1 .FindDialog1Find (Sender : TObject );
var
  FindS: String;
  FPos, IPos, FLen, SLen: Integer; {Internpos, Lengde søkestreng, lengde memotekst}
  Res : integer;
  Found: Boolean;
begin
  {FPos is global}
  Found:= False;
  FPos := SynEdit1.SelStart;
  FLen := Length(findDialog1.FindText);
  SLen := Length(SynEdit1.Text);
  FindS := findDialog1.FindText;

 //following 'if' added by mike
  if frMatchcase in findDialog1.Options then
     IPos := Pos(FindS, Copy(SynEdit1.Text,FPos+1,SLen-FPos))
  else
     IPos := Pos(AnsiUpperCase(FindS),AnsiUpperCase( Copy(SynEdit1.Text,FPos+1,SLen-FPos)));

  If IPos > 0 then begin
    FPos := FPos + IPos;
 //   Hoved.BringToFront;       {Edit control must have focus in }
    SynEdit1.SetFocus;
    Self.ActiveControl := SynEdit1;
    SynEdit1.SelStart:= FPos;  // -1;   mike   {Select the string found by POS}
    SynEdit1.SelEnd := SynEdit1.SelStart + FLen;
    Found := True;
    FPos:=FPos+FLen-1;   //mike - move just past end of found item
  end
  Else
  begin
    ShowMessage('Text was not found');
    FPos := 0;
  end;             //   - also do it before exec of dialog.
end;

end.

