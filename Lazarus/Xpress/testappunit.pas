unit TestAppUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VTHeaderPopup, VirtualTrees, Forms, Controls,
  Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    VirtualStringTree1: TVirtualStringTree;
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

uses Xmlz;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(ApplicationName);
end;

end.

