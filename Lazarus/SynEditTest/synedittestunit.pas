unit SynEditTestUnit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , IDEWindowIntf , SynEdit , SynHighlighterPas ,
  SynHighlighterHTML , SynHighlighterCpp , SynHighlighterAny ,
  SynHighlighterMulti , Forms , Controls , Graphics , Dialogs , EditBtn ,
  ButtonPanel ;

type

  { TForm1 }

  TForm1 = class(TForm )
    FileNameEdit1 : TFileNameEdit ;
    SynAnySyn1 : TSynAnySyn ;
    SynCppSyn1 : TSynCppSyn ;
    SynEdit1 : TSynEdit ;
    SynPasSyn1 : TSynPasSyn ;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}

end.

