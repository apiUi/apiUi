unit AddFavouritesUnit;

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
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FormIniFilez;

type
  TAddFavouritesForm = class(TForm)
    Label1: TLabel;
    NameEdit: TEdit;
    OkButton: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getFavName: String;
    procedure setFavName(const Value: String);
    { Private declarations }
  public
    property FavouriteName: String read getFavName write setFavName;
  end;

var
  AddFavouritesForm: TAddFavouritesForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TAddFavouritesForm.getFavName: String;
begin
  result := NameEdit.Text;
end;

procedure TAddFavouritesForm.NameEditChange(Sender: TObject);
begin
  OkButton.Enabled := (FavouriteName <> '');
end;

procedure TAddFavouritesForm.setFavName(const Value: String);
begin
  NameEdit.Text := Value;
end;

procedure TAddFavouritesForm.FormShow(Sender: TObject);
begin
  OkButton.Enabled := (FavouriteName <> '');
end;

procedure TAddFavouritesForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
end;

procedure TAddFavouritesForm.FormDestroy(Sender: TObject);
begin
  IniFile.Free;
end;

end.
