{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
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

