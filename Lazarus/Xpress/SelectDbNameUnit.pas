unit SelectDbNameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls
  , Ipmz
  , Registry
  ;

type
  TSelectIpmItemForm = class(TForm)
    TreeViewPanel: TPanel;
    TreeView: TTreeView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    IniFile: TRegIniFile;
    procedure ViewIpmItem (aTreeView: TTreeView; aNode: TTreeNode; aIpmItem: TIpmItem);
  public
    RootIpmItem: TIpmItem;
    SelectedIpmItem: TIpmItem;
  end;

var
  SelectIpmItemForm: TSelectIpmItemForm;

implementation

{$R *.DFM}

procedure TSelectIpmItemForm.ViewIpmItem (aTreeView: TTreeView; aNode: TTreeNode; aIpmItem: TIpmItem);
var
  x: Integer;
  xChild: TTreeNode;
  xIpmItem: TIpmItem;
  xSubItems: TIpmItemList;
begin
  if aIpmItem = nil then
    exit;
  if aIpmItem.Occurs > 1 then
    exit;
  if uppercase (aIpmItem.Caption) = 'FILLER' then
    exit;
  xChild := aTreeView.Items.AddChildObject (aNode, aIpmItem.Caption, aIpmItem);
  xSubItems := aIpmItem.Items as TIpmItemList;
  for x := 0 to xSubItems.Count - 1 do
  begin
    xIpmItem := xSubItems.IpmItems [x];
    ViewIpmItem (aTreeView, xChild, xIpmItem);
  end;
end;

procedure TSelectIpmItemForm.FormShow(Sender: TObject);
begin
  TreeView.Items.Clear;
  ViewIpmItem (TreeView, nil, RootIpmItem);
end;

procedure TSelectIpmItemForm.Button1Click(Sender: TObject);
begin
  if TreeView.Selected <> nil then
    SelectedIpmItem := TIpmItem (TreeView.Selected.Data);
end;

procedure TSelectIpmItemForm.FormCreate(Sender: TObject);
begin
  IniFile := TRegIniFile.Create ('IpmGun.ini');
  Top := IniFile.ReadInteger ('SelectIpmItemScreen', 'Top', Top);
  Left := IniFile.ReadInteger ('SelectIpmItemScreen', 'Left', Left);
  Height := IniFile.ReadInteger ('SelectIpmItemScreen', 'Height', Height);
  Width := IniFile.ReadInteger ('SelectIpmItemScreen', 'Width', Width);
end;

procedure TSelectIpmItemForm.FormDestroy(Sender: TObject);
begin
  IniFile.WriteInteger ('SelectIpmItemScreen', 'Top', Top);
  IniFile.WriteInteger ('SelectIpmItemScreen', 'Left', Left);
  IniFile.WriteInteger ('SelectIpmItemScreen', 'Height', Height);
  IniFile.WriteInteger ('SelectIpmItemScreen', 'Width', Width);
  IniFile.Free;
end;

end.
