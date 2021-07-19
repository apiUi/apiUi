{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit ChooseEnumUnit;

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
  StdCtrls, ExtCtrls, FormIniFilez, Xsdz, ComCtrls;

type
  TChooseEnumForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListView: TListView;
    procedure ChooseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fChoosenString: String;
    IniFile: TFormIniFile;
    function getChoosenIndex: Integer;
    function GetChoosenString: String;
    procedure SetChoosenString (aString: String);
  public
    Enums: TStringList;
    property ChoosenString: String read GetChoosenString write SetChoosenString;
    property ChoosenIndex: Integer read getChoosenIndex;
  end;

var
  ChooseEnumForm: TChooseEnumForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TChooseEnumForm.GetChoosenString: String;
begin
  result := fChoosenString;
end;

procedure TChooseEnumForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
end;

procedure TChooseEnumForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TChooseEnumForm.FormShow(Sender: TObject);
var
  x: Integer;
  xListItem: TListItem;
begin
  if not Assigned (Enums) then
    raise Exception.Create('no enumerations provided');
  for x := 0 to Enums.Count - 1 do
  begin
    xListItem := ListView.Items.Add;
    xListItem.Caption := Enums.Strings [x];
    if xListItem.Caption = ChoosenString then
      ListView.Selected := xListItem;
    xListItem.SubItems.Add((Enums.Objects[x] as TXsdEnumeration).Annotation);
  end;
  ListView.SetFocus;
end;

function TChooseEnumForm.getChoosenIndex: Integer;
begin
  result := ListView.ItemIndex;
end;

procedure TChooseEnumForm.SetChoosenString (aString: String);
begin
  fChoosenString := aString;
end;

procedure TChooseEnumForm.ChooseClick(Sender: TObject);
begin
  if Assigned (ListView.Selected) then
  begin
    ChoosenString := ListView.Selected.Caption;
    ModalResult := mrOk;
  end;
end;

end.
