unit Unit1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , SynEdit , Forms , Controls , Graphics ,
  Dialogs , StdCtrls , EditBtn, FormIniFilez, strutils ;

type

  { TForm1 }

  TForm1 = class(TForm )
    FileNameEdit : TFileNameEdit ;
    SynEdit1 : TSynEdit ;
    procedure FileNameEditAcceptFileName (Sender : TObject ;
      var Value : String );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
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

end.

