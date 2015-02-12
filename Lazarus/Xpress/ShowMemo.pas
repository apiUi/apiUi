unit ShowMemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TShowMemoDlg = class(TForm)
    Panel1: TPanel;
    Memo: TMemo;
    OkButton: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Strings: TStrings;
  end;

var
  ShowMemoDlg: TShowMemoDlg;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TShowMemoDlg.FormShow(Sender: TObject);
begin
  Memo.Lines := Strings;
end;

end.
