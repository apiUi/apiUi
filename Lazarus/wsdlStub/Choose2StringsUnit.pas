unit Choose2StringsUnit;

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

  { TChoose2StringsForm }

  TChoose2StringsForm = class(TForm)
    CancelButton : TBitBtn ;
    RightListBox : TListBox ;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel2: TPanel;
    LeftListBox: TListBox;
    Splitter1 : TSplitter ;
    procedure ChooseLeftClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LeftListBoxSelectionChange (Sender : TObject ; User : boolean );
    procedure ChooseRightClick (Sender : TObject );
    procedure RightListBoxSelectionChange (Sender : TObject ; User : boolean );
  private
    fChoosenLeftString: String;
    fChoosenRightString: String;
    fListOfLists : TStringList ;
    function getChoosenIndex : Integer ;
    function GetChoosenLeftString : String ;
    function GetChoosenRightString : String ;
    procedure SetChoosenLeftString (AValue : String );
    procedure SetChoosenRightString (AValue : String );
  public
    property ListOfLists: TStringList read fListOfLists write fListOfLists;
    property ChoosenLeftString: String read GetChoosenLeftString write SetChoosenLeftString;
    property ChoosenRightString: String read GetChoosenRightString write SetChoosenRightString;
    property ChoosenIndex: Integer read getChoosenIndex;
  end;

var
  Choose2StringsForm: TChoose2StringsForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TChoose2StringsForm .GetChoosenLeftString : String ;
begin
  if LeftListBox.ItemIndex > -1 then
    fChoosenRightString := LeftListBox.Items[LeftListBox.ItemIndex];
  result := fChoosenLeftString;
end;

function TChoose2StringsForm .GetChoosenRightString : String ;
begin
  if RightListBox.ItemIndex > -1 then
    fChoosenRightString := RightListBox.Items[RightListBox.ItemIndex];
  result := fChoosenRightString;
end;

procedure TChoose2StringsForm .SetChoosenLeftString (AValue : String );
begin
  fChoosenLeftString := AValue;
  LeftListBox.ItemIndex := LeftListBox.Items.IndexOf(AValue);
end;

procedure TChoose2StringsForm .SetChoosenRightString (AValue : String );
begin
  fChoosenRightString := AValue;
  RightListBox.ItemIndex := RightListBox.Items.IndexOf(AValue);
end;

procedure TChoose2StringsForm.ChooseLeftClick(Sender: TObject);
begin
  if LeftListBox.ItemIndex > -1 then
  begin
    fChoosenLeftString := LeftListBox.Items.Strings [LeftListBox.ItemIndex];
    if RightListBox.ItemIndex > -1 then
    begin
      fChoosenRightString := LeftListBox.Items.Strings [LeftListBox.ItemIndex];
      ModalResult := mrOk;
    end;
  end;
end;

procedure TChoose2StringsForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
    OkButton.Enabled := (LeftListBox.ItemIndex > -1)
                    and (RightListBox.ItemIndex > -1)
                    ;
  finally
    Free;
  end;
end;

procedure TChoose2StringsForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TChoose2StringsForm.FormShow(Sender: TObject);
begin
  if not Assigned (fListOfLists) then
    raise Exception.Create ('TChoose2StringsForm.FormShow(Sender: TObject): No stringlist supplied');
  LeftListBox.Items.Text := fListOfLists.Text;
  if fListOfLists.Count > 0 then
    LeftListBox.ItemIndex := 0;
  LeftListBox.SetFocus;
end;

procedure TChoose2StringsForm .LeftListBoxSelectionChange (Sender : TObject ;
  User : boolean );
begin
  if LeftListBox.ItemIndex > -1 then
    RightListBox.Items.Text := (fListOfLists.Objects[LeftListBox.ItemIndex] as TStringList).Text;
  OkButton.Enabled := (RightListBox.ItemIndex > -1);
end;

procedure TChoose2StringsForm .ChooseRightClick (Sender : TObject );
begin

end;

procedure TChoose2StringsForm .RightListBoxSelectionChange (Sender : TObject ;
  User : boolean );
begin
  OkButton.Enabled := (RightListBox.ItemIndex > -1);
end;

function TChoose2StringsForm.getChoosenIndex: Integer;
begin
  result := LeftListBox.ItemIndex;
end;

end.
