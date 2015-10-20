unit ChooseStringUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls , Buttons, FormIniFilez;

type

  { TChooseStringForm }

  TChooseStringForm = class(TForm)
    CancelButton : TBitBtn ;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox: TListBox;
    procedure ChooseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSelectionChange (Sender : TObject ; User : boolean );
  private
    fChoosenString: String;
    function getChoosenIndex: Integer;
    function GetChoosenString: String;
    procedure SetChoosenString (aString: String);
  public
    property ChoosenString: String read GetChoosenString write SetChoosenString;
    property ChoosenIndex: Integer read getChoosenIndex;
  end;

var
  ChooseStringForm: TChooseStringForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TChooseStringForm.GetChoosenString: String;
begin
  result := fChoosenString;
end;

procedure TChooseStringForm.SetChoosenString (aString: String);
begin
  fChoosenString := aString;
  ListBox.ItemIndex := ListBox.Items.IndexOf(aString);
end;

procedure TChooseStringForm.ChooseClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
  begin
    fChoosenString := ListBox.Items.Strings [ListBox.ItemIndex];
    ModalResult := mrOk;
  end;
end;

procedure TChooseStringForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    OkButton.Enabled := (ListBox.ItemIndex > -1);
  finally
    Free;
  end;
end;

procedure TChooseStringForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TChooseStringForm.FormShow(Sender: TObject);
begin
  ListBox.SetFocus;
end;

procedure TChooseStringForm .ListBoxSelectionChange (Sender : TObject ;
  User : boolean );
begin
  OkButton.Enabled := (ListBox.ItemIndex > -1);
end;

function TChooseStringForm.getChoosenIndex: Integer;
begin
  result := ListBox.ItemIndex;
end;

end.
