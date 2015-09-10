unit wsdlPropertiesUnit;

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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, FormIniFilez;

type
  TwsdlPropertiesForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    PageControl1: TPageControl;
    WsdlTabSheet: TTabSheet;
    ElementsWhenRepeatableEdit: TLabeledEdit;
    Label1: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    IniFile: TFormIniFile;
  public
    { Public declarations }
  end;

var
  wsdlPropertiesForm: TwsdlPropertiesForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TwsdlPropertiesForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TwsdlPropertiesForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create(self, True);
  IniFile.Restore;
end;

procedure TwsdlPropertiesForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

end.
