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
unit OpenWsdlUnit;

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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, ActnList, FormIniFilez, Xmlz, xmlio;

type

  { TOpenWsdlForm }

  TOpenWsdlForm = class(TForm)
    BrowseCloudAction: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel3: TPanel;
    Label8: TLabel;
    SpeedButton1: TSpeedButton;
    WsdlLocationEdit: TEdit;
    Button1: TButton;
    OpenFileDialog: TOpenDialog;
    procedure BrowseCloudActionExecute(Sender: TObject);
    procedure BrowseCloudActionUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getWsdlLocation : String ;
    procedure setWsdlLocation (AValue : String );
  public
    remoteServerConnectionXml: TXml;
    property WsdlLocation: String read getWsdlLocation write setWsdlLocation;
  end;

var
  OpenWsdlForm: TOpenWsdlForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses StrUtils, SelectItemUnit;

procedure TOpenWsdlForm.Button1Click(Sender: TObject);
begin
  if not AnsiStartsText ('http://', WsdlLocationEdit.Text) then
    OpenFileDialog.FileName := WsdlLocationEdit.Text;
  if OpenFileDialog.Execute = True then
    WsdlLocationEdit.Text := OpenFileDialog.FileName;
end;

function TOpenWsdlForm .getWsdlLocation : String ;
begin
  result := WsdlLocationEdit.Text;
end;

procedure TOpenWsdlForm .setWsdlLocation (AValue : String );
begin
  WsdlLocationEdit.Text := AValue;
end;

procedure TOpenWsdlForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
  WsdlLocation := IniFile.StringByName['WsdlLocation'];
end;

procedure TOpenWsdlForm.FormDestroy(Sender: TObject);
begin
  IniFile.StringByName['WsdlLocation'] := WsdlLocation;
  IniFile.Save;
  IniFile.Free;
end;

procedure TOpenWsdlForm.BrowseCloudActionExecute(Sender: TObject);
var
  x: Integer;
  s, n: String;
begin
  s := xmlio.apiUiServerDialog ( remoteServerConnectionXml
                             , '/apiUi/api/project/descriptors'
                             , ''
                             , 'GET'
                             , 'application/json'
                             );
  with TXml.Create do
  try
    LoadJsonFromString(s, nil);
    Application.CreateForm(TSelectItemForm, SelectItemForm);
    try
      SelectItemForm.Caption := 'Select describtor';
      SelectItemForm.ListBox.Clear;
      for x := 0 to Items.Count - 1 do
      begin
        if (AnsiStartsText ('http://', Items.XmlItems[x].Value))
        or (AnsiStartsText ('https://', Items.XmlItems[x].Value))
        or (AnsiStartsText ('apiary://', Items.XmlItems[x].Value))
        then
          SelectItemForm.ListBox.Items.Add (Items.XmlItems[x].Value)
        else
          SelectItemForm.ListBox.Items.Add ( 'apiui://project/descriptors/'
                                           + Items.XmlItems[x].Value
                                           )
      end;
      SelectItemForm.ShowModal;
      if SelectItemForm.ModalResult = mrOk then
      begin
        WsdlLocationEdit.Text := SelectItemForm.SelectedItem;
      end;
    finally
      FreeAndNil (SelectItemForm);
    end;
  finally
    Free;
  end;
end;

procedure TOpenWsdlForm.BrowseCloudActionUpdate(Sender: TObject);
begin
  BrowseCloudAction.Enabled := Assigned(remoteServerConnectionXml)
                           and (remoteServerConnectionXml.Items.XmlBooleanByTagDef['Enabled', False])
                             ;
end;

end.

