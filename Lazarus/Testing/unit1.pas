unit Unit1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , SynEdit , Forms , Controls , Graphics ,
  Dialogs , StdCtrls , EditBtn , PairSplitter , FormIniFilez , strutils ,
  wrdFunctionz , GZIPUtils ;

type

  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    Edit1 : TEdit ;
    FileNameEdit : TFileNameEdit ;
    PairSplitter1 : TPairSplitter ;
    PairSplitterSide1 : TPairSplitterSide ;
    PairSplitterSide2 : TPairSplitterSide ;
    SynEdit1 : TSynEdit ;
    procedure Button1Click (Sender : TObject );
    procedure FileNameEditAcceptFileName (Sender : TObject ;
      var Value : String );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
    procedure SynEdit1MouseMove (Sender : TObject ; Shift : TShiftState ; X ,
      Y : Integer );
  private
    IniFile: TFormIniFile;
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}
uses xmlio, LConvEncoding;

{ TForm1 }

procedure TForm1 .FileNameEditAcceptFileName (Sender : TObject ;
  var Value : String );
begin
  SynEdit1.Text := ReadStringFromFile(Value);
  Caption := GuessEncoding(SynEdit1.Text);
end;

procedure TForm1 .Button1Click (Sender : TObject );
begin
  Button1.Enabled := False;
  Button1.Caption := IntToStr (wrdFunctionz.wrdFileDiffencesCount('c:\temp\janbo1.docx', 'c:\temp\janbo2.docx'));
  Button1.Enabled := True;
end;

procedure TForm1 .FormCreate (Sender : TObject );
begin
  IniFile := TFormIniFile.Create(self);
  IniFile.Restore;
  FileNameEdit.Text := IniFile.StringByName['FileName'];
end;

procedure TForm1 .FormDestroy (Sender : TObject );
begin
  IniFile.StringByName['FileName']:=FileNameEdit.Text;
  IniFile.Save;
  IniFile.Free;
end;

procedure TForm1 .SynEdit1MouseMove (Sender : TObject ; Shift : TShiftState ;
  X , Y : Integer );
var
  P, L: TPoint;
  s: String;
begin
  P.x := X;
  P.y := Y;
  L := SynEdit1.PixelsToRowColumn(P);
  s := SynEdit1.GetWordAtRowCol(L);
  Edit1.Text := Format ('%s [%d:%d]', [s, L.x, L.y]);
end;

end.

