unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, activexcontainer, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Word_8_5_TLB, SHDocVw_1_1_TLB, windows,
  xmlio, sqldb, oracleconnection ;

type

  { TForm1 }

  TForm1 = class(TForm)
    AxcApplication1: TAxcApplication;
    AxcDocument1: TAxcDocument;
    Button1: TButton;
    Edit1: TEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  browser: olevariant;

implementation

uses variants
   , OleServer
   , ComObj
   , Word_Tlb
   , wrdFunctionz
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
      s := UTF8Decode('c:\data\Janbo2.pdf');
      wordDoc.SaveAs(s, wdFormatPDF);
      wordApp.Quit;
    finally
      wordApp := null;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
var
   doc: _Document;
begin
  wrdStringToPdfFile ('JanBo was here to pfd', 'c:\data\Janbo2.pdf');
end;

procedure TForm1.Button3Click(Sender: TObject);
var browser: olevariant;
begin
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  aText: OleVariant;
begin
  if Key = VK_RETURN then
  begin
    xmlio.SaveStringToFile('c:/data/t.txt', edit1.Text);
    Browser.Navigate ('file://c:/data/t.txt');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  browser := Null;
end;

end.

