unit SelectDbNameUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Adodb, Windows,
{$ELSE}
  sqldb, LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls
  , Ipmz
  , FormIniFilez
  ;

type
  TSelectDbNameForm = class(TForm)
    TreeViewPanel: TPanel;
    TreeView: TTreeView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    IniFile: TFormIniFile;
    {$ifndef fpc}
    fDataBase: TADOConnection;
    function GetDataBase: TADOConnection;
    procedure SetDataBase (aDataBase: TADOConnection);
    {$else}
    fDataBase: TSQLConnection;
    function GetDataBase: TSQLConnection;
    procedure SetDataBase (aDataBase: TSQLConnection);
    {$endif}
    procedure ViewIpmItem (aTreeView: TTreeView; aNode: TTreeNode; aIpmItem: TIpmItem);
  public
    SelectedDbName: String;
    {$ifndef fpc}
    property DataBase: TADOConnection read GetDataBase write SetDataBase;
    {$else}
    property DataBase: TSQLConnection read GetDataBase write SetDataBase;
    {$endif}
  end;

var
  SelectDbNameForm: TSelectDbNameForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSelectDbNameForm.ViewIpmItem (aTreeView: TTreeView; aNode: TTreeNode; aIpmItem: TIpmItem);
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

procedure TSelectDbNameForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  ColumnList: TStringList;
  y: Integer;
begin
  if Node.Count = 0 then
  begin
    ColumnList := TStringList.Create;
    try
      DataBase.GetFieldNames(Node.Text, ColumnList);
      for y := 0 to ColumnList.Count - 1 do
      begin
        TreeView.Items.AddChild (Node, ColumnList.Strings [y]);
        Node.Expand(False);
      end;
    finally
      ColumnList.Free;
    end;
  end;
end;

procedure TSelectDbNameForm.FormShow(Sender: TObject);
var
  x, y: Integer;
  Root: TTreeNode;
  xChild: TTreeNode;
  TableList: TStrings;
begin
  TableList := TStringList.Create;
  try
    TreeView.Items.Clear;
    DataBase.GetTableNames(TableList, False);
    for x := 0 to TableList.Count - 1 do
    begin
      xChild := TreeView.Items.AddChild (nil, TableList.Strings [x]);
    end;
  finally
    TableList.Clear;
    TableList.Free;
  end;
  TreeView.SetFocus;
end;

procedure TSelectDbNameForm.Button1Click(Sender: TObject);
begin
  if TreeView.Selected <> nil then
    SelectedDbName := TreeView.Selected.Text;
end;

procedure TSelectDbNameForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TSelectDbNameForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

function TSelectDbNameForm.GetDataBase: {$ifndef fpc}TADOConnection{$else}TSQLConnection{$endif};
begin
  result := fDataBase;
end;

procedure TSelectDbNameForm.SetDataBase(aDataBase:  {$ifndef fpc}TADOConnection{$else}TSQLConnection{$endif});
begin
  fDataBase := aDataBase;
end;

end.
