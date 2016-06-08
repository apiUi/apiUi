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
  TwsdlListForm = class(TForm)
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
    EditSdfButton: TButton;
    EditAction: TAction;
    NewAction: TAction;
    NewSdfButton: TButton;
    SaveDialog: TSaveDialog;
    ExtraXSDAction: TAction;
    ExtraXsdsButton: TButton;
    PropertiesAction: TAction;
    PropertiesButton: TButton;
    CopyFileNameAction: TAction;
    ListViewPopupMenu: TPopupMenu;
    CopyFileNameAction1: TMenuItem;
    procedure ExtraXSDActionExecute(Sender: TObject);
    procedure ExtraXSDActionUpdate(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure EditActionUpdate(Sender: TObject);
    procedure DeleteActionUpdate(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure OKActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PropertiesActionUpdate(Sender: TObject);
    procedure PropertiesActionExecute(Sender: TObject);
    procedure CopyFileNameActionExecute(Sender: TObject);
    procedure CopyFileNameActionUpdate(Sender: TObject);
  private
    fStubChanged: Boolean;
    fReloadRequired: Boolean;
    procedure UpdateListView;
  public
    wsdlFileName: String;
    Wsdls: TStringList;
    IsBetaTestMode: Boolean;
    xsdElementsWhenRepeatable: Integer;
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
   , wsdlPropertiesUnit
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
  if MessageDlg ( 'Remove Wsdl from list, loosing data and scripts: '
                + ListView.Selected.Caption
                + '?'
                , mtWarning
                , [mbYes, mbNo]
                , 0) = mrYes
  then
  begin
    Wsdls.Objects [ListView.ItemIndex].Free;
    Wsdls.Delete(ListView.ItemIndex);
    UpdateListView;
    fStubChanged := True;
  end;
end;

procedure TwsdlListForm.AddActionExecute(Sender: TObject);
var
  s, o, f: Integer;
  ListItem: TListItem;
  Wsdl: TWsdl;
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
        Wsdl := TWsdl.Create(-1, xsdElementsWhenRepeatable, ShowOperationsWithEndpointOnly);
        try
          if UpperCase (ExtractFileExt (OpenWsdlForm.WsdlLocationEdit.Text)) = '.SDF' then
            Wsdl.LoadFromSdfFile (OpenWsdlForm.WsdlLocationEdit.Text)
          else
          begin
            wsdl.LoadFromSchemaFile(OpenWsdlForm.WsdlLocationEdit.Text, nil);
{
            for s := 0 to Wsdl.Services.Count - 1 do
              for o := 0 to Wsdl.Services.Services [s].Operations.Count - 1 do
                Wsdl.Services.Services [s].Operations.Operations [o].OutputXml.Populate(xvAll);
}
          end;
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

procedure TwsdlListForm.OKActionUpdate(Sender: TObject);
begin
  OKAction.Enabled := True
                 ;
end;

procedure TwsdlListForm.PropertiesActionExecute(Sender: TObject);
var
  xXml, yXml: TXml;
  xForm: TwsdlPropertiesForm;
  xWsdl: TWsdl;
  x: Integer;
  xTag: String;
begin
  xWsdl := Wsdls.Objects [ListView.ItemIndex] as TWsdl;
  Application.CreateForm(TwsdlPropertiesForm, xForm);
  try
    xForm.Caption := 'Properties for : ' + ListView.Selected.Caption;
    xForm.ElementsWhenRepeatableEdit.Text := IntToStr(xWsdl.xsdElementsWhenRepeatable);
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
    begin
      xWsdl.xsdElementsWhenRepeatable := StrToIntDef (xForm.ElementsWhenRepeatableEdit.Text, -1);
      fStubChanged := True;
      ShowMessage ('Please reload your project to see the changes');
    end;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TwsdlListForm.PropertiesActionUpdate(Sender: TObject);
begin
  PropertiesAction.Enabled := (ListView.ItemIndex > -1)
                          and (UpperCase (ExtractFileExt (ListView.Selected.Caption)) <> '.SDF')
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
  ExtraXsdsButton.Visible := Assigned (_WsdlListOfFilesXsd);
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

procedure TwsdlListForm.EditActionUpdate(Sender: TObject);
begin
  EditAction.Enabled := (ListView.ItemIndex > -1)
                    and (UpperCase (ExtractFileExt (ListView.Selected.Caption)) = '.SDF');
end;

procedure TwsdlListForm.EditActionExecute(Sender: TObject);
var
  xXml: TXml;
  sl: TStringList;
begin
  xXml := TXml.Create;
  try
    xXml.LoadFromFile(ListView.Selected.Caption, nil);
    wsdlConvertSdfFrom36 (xXml);
    if EditXmlXsdBased( 'ServiceDefinition file: ' + ListView.Selected.Caption
                      , ''
                      , 'FreeFormatOperations.Operation.Name'
                      , 'FreeFormatOperations.Operation.Name'
                      , False
                      , _WsdlServiceDefinitionXsd
                      , xXml
                      ) then
    begin
      if (MessageDlg ('Save changes to ' + #$D#$A + ListView.Selected.Caption, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        sl := TStringList.Create;
        try
          if SaveRelativeFilenames then
            xXml.SetFileNamesRelative(ListView.Selected.Caption);
          sl.Text := xXml.AsText(False,0,True,False);
          sl.SaveToFile(ListView.Selected.Caption);
          ShowMessage ('Please reload your project to see the changes');
        finally
          sl.free;
        end;
      end;
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TwsdlListForm.NewActionExecute(Sender: TObject);
  function _inquire(aXml: TXml): Boolean;
  begin
    result := EditXmlXsdBased( 'New serviceDefinition file'
                             , ''
                             , ''
                             , ''
                             , False
                             , _WsdlServiceDefinitionXsd
                             , aXml
                             );
  end;
var
  xXml: TXml;
  sl: TStringList;
  xApprove: Boolean;
  xRpy: Integer;
  xWsdl: TWsdl;
begin
  xXml := TXml.Create (-10000, _WsdlServiceDefinitionXsd);
  try
    xApprove := _inquire(xXml);
    while xApprove do
    begin
      xRpy := MessageDlg ('Save service description', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if xRpy = mrNo then
        xApprove := False;
      if xRpy = mrCancel then
        xApprove := _inquire(xXml);
      if xRpy = mrYes then
      begin
        if SaveDialog.Execute then
        begin
          with TStringList.Create do
          try
            if SaveRelativeFilenames then
              xXml.SetFileNamesRelative(SaveDialog.FileName);
            Text := xXml.AsText(False,0,True,False);
            SaveToFile(SaveDialog.FileName);
          finally
            free;
          end;
          xWsdl := TWsdl.Create (-1, xsdElementsWhenRepeatable, ShowOperationsWithEndpointOnly);
          try
            xWsdl.LoadFromSdfFile (SaveDialog.FileName);
          except
            xWsdl.Free;
            raise;
          end;
          Wsdls.AddObject(SaveDialog.FileName, xWsdl);
          UpdateListView;
          fStubChanged := True;
          xApprove := False;
        end
        else
        begin
          _inquire(xXml);
        end;
      end;
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

procedure TwsdlListForm.ExtraXSDActionUpdate(Sender: TObject);
begin
  ExtraXSDAction.Enabled := (ListView.ItemIndex > -1)
//                  and (UpperCase (ExtractFileExt (ListView.Selected.Caption)) = '.WSDL')
                    and Assigned (_WsdlListOfFilesXsd);
end;

procedure TwsdlListForm.ExtraXSDActionExecute(Sender: TObject);
var
  xXml: TXml;
  xWsdl: TWsdl;
begin
  xWsdl := Wsdls.Objects [ListView.ItemIndex] as TWsdl;
  xXml := xWsdl.ExtraXsdsAsXml(False, '');
  try
    if EditXmlXsdBased ( 'Extra XML-Schema files for : ' + ListView.Selected.Caption
                       , ''
                       , ''
                       , ''
                       , False
                       , _WsdlListOfFilesXsd
                       , xXml
                       ) then
    begin
      fstubChanged := True;
      xWsdl.ExtraXsdsFromXml (xXml);
      fReloadRequired := True;
    end;
  finally
    FreeAndNil (xXml);
  end;
end;

end.
