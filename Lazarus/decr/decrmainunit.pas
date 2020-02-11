unit decrmainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    TextEdit: TEdit;
    SeedEdit: TEdit;
    DecrEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    function SimpleEncrypt(const Source, aSeed: AnsiString): AnsiString;
  public
    function DecryptPassword(aPassword, aSeed: AnsiString): AnsiString;
    function EncryptPassword(aPassword: AnsiString): AnsiString;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses base64;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DecrEdit.Text := DecryptPassword(TextEdit.Text, SeedEdit.Text);
end;

function TForm1.SimpleEncrypt(const Source, aSeed: AnsiString): AnsiString;
var
  Index: Integer;
begin
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := AnsiChar((Ord(aSeed[Index mod Length(aSeed)]) xor Ord(Source[Index])));
end;

function TForm1.DecryptPassword(aPassword, aSeed: AnsiString): AnsiString;
begin
  if aPassword <> '' then
    try result :=  SimpleEncrypt(DecodeStringBase64(aPassword), aSeed); except result := '' end
  else
    result := '';
end;

function TForm1.EncryptPassword(aPassword: AnsiString): AnsiString;
begin
  if aPassword <> '' then
    result := EncodeStringBase64 (SimpleEncrypt(aPassword, SeedEdit.Text))
  else
    result := '';
end;

end.

