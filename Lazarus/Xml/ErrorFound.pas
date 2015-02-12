unit ErrorFound;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Dialogs, Buttons, ExtCtrls
  ;

type
  TErrorFoundDlg = class(TForm)
    OKBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    FileNameEdit: TEdit;
    LineNumberLabel: TLabel;
    LineNumberEdit: TEdit;
    ColumnNumberLabel: TLabel;
    ColumnNumberEdit: TEdit;
    TokenStringLabel: TLabel;
    TokenStringEdit: TEdit;
    EditButton: TButton;
    procedure EditButtonClick(Sender: TObject);
  private
  public
    Viewer: String;
  end;

var
  ErrorFoundDlg: TErrorFoundDlg;

implementation

{$R *.lfm}

procedure TErrorFoundDlg.EditButtonClick(Sender: TObject);
begin
  if Viewer <> '' then
//    WinExec(PAnsiChar(Viewer + ' ' + FileNameEdit.Text), 1)
  else
    ShowMessage ('No viewer specified');
end;

end.
