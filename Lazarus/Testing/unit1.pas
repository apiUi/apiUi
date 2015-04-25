unit Unit1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , FileUtil , Forms , Controls , Graphics , Dialogs ,
  StdCtrls , Xmlz , Xsdz , Wsdlz , xmlUtilz, StompInterface, StompTypes, heaptrc;

type

  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    procedure Button1Click (Sender : TObject );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
  private
    { private declarations }
  public
    StrompInterface: TStompInterface;
    procedure HaveStompFrame (aStompInterface: TStompInterface; aQueue: String; aFrame: IStompFrame);
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1 .FormCreate (Sender : TObject );
var
  xName: String;
BEGIN
  xName := ParamStr(0) + 'heap.trc';
  if FileExists(xName) then
      DeleteFile(xName);
  SetHeapTraceOutput(xName);
{
  StrompInterface := TStompInterface.Create(self, @HaveStompFrame);
  with StrompInterface do
  begin
    Host := 'Localhost';
    Port := 61613;
    ClientId := 'JanBo';
    Connect;
  end;
}
end;

procedure TForm1 .FormDestroy (Sender : TObject );
begin
{
  with StrompInterface do
  begin
    Disconnect;
    Free;
  end;
}
end;

procedure TForm1 .HaveStompFrame (aStompInterface : TStompInterface ;
  aQueue : String ; aFrame : IStompFrame );
begin

end;

procedure TForm1 .Button1Click (Sender : TObject );
var
  xWsdl: TWsdl;

begin
  xWsdl := TWsdl.Create(-1, 1, False);
  try
    xWsdl.LoadFromSchemaFile('C:\Data\systemTesting\CRMi\MoveArchiveDocumentType\1\MoveArchiveDocumentType_1_contract.wsdl', nil);
    Caption := xWsdl.Name;
    with TWsdlOperation.Create(xWsdl.Services.Services[0].Operations.Operations[0]) do
    try
      Caption := reqTagName;
    finally
      Free;
    end;
  finally
    xWsdl.Free;
  end;
end;

end.

