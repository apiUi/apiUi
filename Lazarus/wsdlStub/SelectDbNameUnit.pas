{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit SelectDbNameUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Adodb, Windows,
{$ELSE}
  sqldb, LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls
  , xmlio
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
    {$ifndef fpc}
    fDataBase: TADOConnection;
    function GetDataBase: TADOConnection;
    procedure SetDataBase (aDataBase: TADOConnection);
    {$else}
    fDataBase: TSQLConnector;
    function GetDataBase: TSQLConnector;
    procedure SetDataBase (aDataBase: TSQLConnector);
    {$endif}
    procedure ViewIpmItem (aTreeView: TTreeView; aNode: TTreeNode; aIpmItem: TIpmItem);
  public
    SelectedDbName: String;
    {$ifndef fpc}
    property DataBase: TADOConnection read GetDataBase write SetDataBase;
    {$else}
    property DataBase: TSQLConnector read GetDataBase write SetDataBase;
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
  ColumnList: TJBStringList;
  y: Integer;
begin
  if Node.Count = 0 then
  begin
    ColumnList := TJBStringList.Create;
    try
      DataBase.Transaction.StartTransaction;
      try
        DataBase.GetFieldNames(Node.Text, ColumnList);
      finally
        DataBase.Transaction.EndTransaction;
      end;
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
  x: Integer;
  TableList: TStrings;
begin
  TableList := TJBStringList.Create;
  TreeView.BeginUpdate;
  try
    TreeView.Items.Clear;
    DataBase.Transaction.Rollback;
    DataBase.Transaction.StartTransaction;
    try
      DataBase.GetTableNames(TableList, False);
    finally
      DataBase.Transaction.EndTransaction;
    end;
    for x := 0 to TableList.Count - 1 do
    begin
      TreeView.Items.AddChild (nil, TableList.Strings [x]);
    end;
  finally
    TreeView.EndUpdate;
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
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TSelectDbNameForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

function TSelectDbNameForm.GetDataBase: TSQLConnector;
begin
  result := fDataBase;
end;

procedure TSelectDbNameForm.SetDataBase(aDataBase: TSQLConnector);
begin
  fDataBase := aDataBase;
end;

end.
