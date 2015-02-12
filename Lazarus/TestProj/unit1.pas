unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , Forms , Controls , Graphics , Dialogs ,
  StdCtrls , EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FileNameEdit1 : TFileNameEdit ;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses xmlz
   , xsdz
   ;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  xXml: TXml;
begin
  with OpenDialog1 do
  begin
    if Execute then
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromFile(FileName, nil);
        ShowMessage(xXml.Text);
      finally
        xXml.Free;
      end;
    end;
  end;
end;

end.

