unit testerUnit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , Forms , Controls , Graphics , Dialogs ,
  StdCtrls, Wsdlz, WsdlProjectz, wsdlStubMainUnit ;

type

  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    procedure Button1Click (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}
uses OpenWsdlUnit, EditOperationScriptUnit
   ;

{ TForm1 }

procedure TForm1 .Button1Click (Sender : TObject );
var
  xForm: TEditOperationScriptForm;
begin
  Application.CreateForm(TEditOperationScriptForm, xForm);
  xForm.ShowModal;
  FreeAndNil(xForm);
end;

end.

