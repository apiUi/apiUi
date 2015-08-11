unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, oracleconnection, odbcconn, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    cancelButton: TButton;
    goButton: TButton;
    OracleConnection1: TOracleConnection;
    rsltMemo: TMemo;
    qryMemo: TMemo;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure cancelButtonClick(Sender: TObject);
    procedure goButtonClick(Sender: TObject);
  private
    fCancelled: Boolean;
    procedure setCancelled(AValue: Boolean);
    { private declarations }
  public
    property Cancelled: Boolean read fCancelled write setCancelled;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.goButtonClick(Sender: TObject);
var
  swapColor: TColor;
begin
  swapColor := goButton.Color;
  Cancelled := False;
  try
    rsltMemo.Clear;
    SQLQuery1.Active:=False;
    SQLQuery1.sql.Text:=qryMemo.Lines.Text;
    if not Cancelled then SQLQuery1.Open;
    if not Cancelled then SQLQuery1.First;
    while (not SQLQuery1.EOF)
    and (not Cancelled) do
    begin
      rsltMemo.Lines.Add(SQLQuery1.Fields[0].AsString);
      SQLQuery1.Next;
    end;
    SQLQuery1.Close;
  finally
    Cancelled := False;
  end;
end;

procedure TForm1.cancelButtonClick(Sender: TObject);
begin
  Cancelled:=True;
end;

procedure TForm1.setCancelled(AValue: Boolean);
begin
  if fCancelled=AValue then Exit;
  fCancelled:=AValue;
  cancelButton.Enabled:=(not AValue);
end;

end.

