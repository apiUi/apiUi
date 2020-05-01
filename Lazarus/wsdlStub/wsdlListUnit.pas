unit wsdlListUnit;

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
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ActnList
   , FormIniFilez
   , Dialogs
   , Wsdlz
   , Xsdz
   , Xmlz
   , Bind
   , Menus
   ;

type

  { TwsdlListForm }

  TwsdlListForm = class(TForm)
    CancelButton: TButton;
    Panel1: TPanel;
    OKBtn: TButton;
    Panel2: TPanel;
    ListView: TListView;
    AddButton: TButton;
    ActionList1: TActionList;
    DeleteAction: TAction;
    AddAction: TAction;
    OKAction: TAction;
    DeleteButton: TButton;
    CopyFileNameAction: TAction;
    ListViewPopupMenu: TPopupMenu;
    CopyFileNameAction1: TMenuItem;
    procedure DeleteActionUpdate(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure OKActionExecute(Sender: TObject);
    procedure OKActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopyFileNameActionExecute(Sender: TObject);
    procedure CopyFileNameActionUpdate(Sender: TObject);
  private
    fStubChanged: Boolean;
    fReloadRequired: Boolean;
    procedure UpdateListView;
  public
    wsdlFileName: String;
    Wsdls: TStringList;
    EnvVars: TStringList;
    IsBetaTestMode: Boolean;
    ShowOperationsWithEndpointOnly: Boolean;
    SaveRelativeFilenames: Boolean;
    property stubChanged: Boolean read fStubChanged;
    property ReloadRequired: Boolean read fReloadRequired;
  end;

var
  wsdlListForm: TwsdlListForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
uses ErrorFound
   , OpenWsdlUnit
   , ShowXmlUnit
   , xmlUtilz
   , ClipBrd
   ;

procedure TwsdlListForm.DeleteActionUpdate(Sender: TObject);
begin
  DeleteAction.Enabled := (ListView.Selected <> nil);
end;

procedure TwsdlListForm.AddActionUpdate(Sender: TObject);
begin
  AddAction.Enabled := True;
end;

procedure TwsdlListForm.CopyFileNameActionExecute(Sender: TObject);
begin
  if Assigned (ListView.Selected) then
    Clipboard.AsText :=ListView.Selected.Caption;
end;

procedure TwsdlListForm.CopyFileNameActionUpdate(Sender: TObject);
begin
  CopyFileNameAction.Enabled := Assigned (ListView.Selected);
end;

procedure TwsdlListForm.DeleteActionExecute(Sender: TObject);
begin
  Wsdls.Delete(ListView.ItemIndex);
  UpdateListView;
  fStubChanged := True;
end;

procedure TwsdlListForm.AddActionExecute(Sender: TObject);
var
  s, o, f: Integer;
  ListItem: TListItem;
  Wsdl: TWsdl;
  xExt: String;
  swapCursor: TCursor;
begin
  Application.CreateForm(TOpenWsdlForm, OpenWsdlForm);
  try
    OpenWsdlForm.ShowModal;
    if OpenWsdlForm.ModalResult = mrOK then
    begin
      if Wsdls.Find(OpenWsdlForm.WsdlLocationEdit.Text, f) then
        raise Exception.Create(OpenWsdlForm.WsdlLocationEdit.Text + ': already in the list');
      SwapCursor := Screen.Cursor;
      try
        Screen.Cursor := crHourGlass;
        Wsdl := TWsdl.Create(EnvVars, ShowOperationsWithEndpointOnly);
        try
          xExt := UpperCase (ExtractFileExt (OpenWsdlForm.WsdlLocationEdit.Text));
          if (xExt = '.JSON')
          or (xExt = '.YAML') then
            Wsdl.LoadFromJsonYamlFile (OpenWsdlForm.WsdlLocationEdit.Text, nil, nil)
          else
            wsdl.LoadFromSchemaFile(OpenWsdlForm.WsdlLocationEdit.Text, nil, nil);
          Wsdl.XsdDescr.Finalise;
        except
          Wsdl.Free;
          raise;
        end;
        Wsdls.AddObject(OpenWsdlForm.WsdlLocationEdit.Text, Wsdl);
        UpdateListView;
        fStubChanged := True;
      finally
        Screen.Cursor := SwapCursor;
      end;
    end;
  finally
    FreeAndNil (OpenWsdlForm);
  end;
end;

procedure TwsdlListForm.OKActionExecute(Sender: TObject);
begin

end;

procedure TwsdlListForm.OKActionUpdate(Sender: TObject);
begin
  OKAction.Enabled := True
                 ;
end;

procedure TwsdlListForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create(self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TwsdlListForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TwsdlListForm.FormShow(Sender: TObject);
begin
  UpdateListView;
  fStubChanged := False;
end;

procedure TwsdlListForm.UpdateListView;
var
  x: Integer;
  ListItem: TListItem;
begin
  ListView.Clear;
  for x := 0 to Wsdls.Count - 1 do
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := Wsdls.Strings [x];
//    ListItem.SubItems.Add(Wsdl.FileName);
  end;
  if ListView.Items.Count > 0 then
    ListView.ItemIndex := 0;
  ListView.SetFocus;
end;

end.
