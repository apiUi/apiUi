unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, oracleconnection, odbcconn, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    goButton: TButton;
    OracleConnection1: TOracleConnection;
    rsltMemo: TMemo;
    qryMemo: TMemo;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure goButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.goButtonClick(Sender: TObject);
begin
  rsltMemo.Clear;
  SQLQuery1.sql.Text:=qryMemo.Lines.Text;
  SQLQuery1.Open;
  SQLQuery1.First;
  while not SQLQuery1.EOF do
  begin
    rsltMemo.Lines.Add(IntToStr(Length ((SQLQuery1.Fields[0].AsString))));
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
end;

end.

