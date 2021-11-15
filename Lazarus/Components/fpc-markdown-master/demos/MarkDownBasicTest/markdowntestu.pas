unit MarkDownTestU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HtmlView, MarkdownUtils, MarkdownProcessor, HtmlGlobals, LCLIntf;

type

  { TMainForm }

  TMainForm = class(TForm)
    B_Convert: TButton;
    HtmlViewer1: THtmlViewer;
    Memo1: TMemo;
    procedure B_ConvertClick(Sender: TObject);
    procedure HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.B_ConvertClick(Sender: TObject);
var
  md:TMarkdownProcessor;
begin
  md := TMarkdownProcessor.createDialect(mdDaringFireball);
  md.UnSafe := true;
  HtmlViewer1.LoadFromString(md.process(Memo1.Text));
  md.free;
end;

procedure TMainForm.HtmlViewer1HotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Handled := OpenURL(SRC);
end;

end.

