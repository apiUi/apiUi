unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses variants
   , OleServer
   , ComObj
   ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  wordApp, wordDoc: OleVariant;
  s: WideString;
begin
  Screen.Cursor := crHourGlass;
  try
    wordApp :=  CreateOleObject('Word.Application');
    try
      s := UTF8Decode('c:\data\Janbo.docx');
      wordDoc := wordApp.Documents.Open (s);
      s := UTF8Decode('c:\data\Janbo2.docx');
      wordDoc.SaveAs(s);
      wordApp.Quit;
    finally
      wordApp := null;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;

end;

end.

