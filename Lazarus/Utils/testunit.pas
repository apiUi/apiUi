unit testunit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , Forms , Controls , Graphics , Dialogs ,
  StdCtrls ;

type

  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    Memo1 : TMemo ;
    procedure Button1Click (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation
uses wrdFunctionz
   ;
{$R *.lfm}

{ TForm1 }

procedure TForm1 .Button1Click (Sender : TObject );
begin
  wrdStringToPdfFile(Memo1.Text, 'c:\t.pdf');
end;

end.

