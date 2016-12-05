unit Main;

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
  SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, Bind, Xmlz, Xsdz, Wsdlz, FormIniFilez;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileNew1: TAction;
    FileSave1: TAction;
    FileExit1: TAction;
    FileOpen1: TAction;
    FileSaveAsAction: TAction;
    HelpAbout1: TAction;
    WindowTileItem2: TMenuItem;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ImageList1: TImageList;
    SaveDialog: TSaveDialog;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FileSave1Update(Sender: TObject);
    procedure FileSaveAsActionUpdate(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EditCopy1Update(Sender: TObject);
    procedure EditPaste1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    IniFile: TFormIniFile;
    procedure CreateJsnChild ( aJsnSchemaFileName
                             , aJsnFileName: String
                             ; aXsdDescr: TXsdDescr
                             ; aXml: TXml
                             ; ExistingFile: Boolean
                             );
    procedure CreateXsdChild ( aXsdFileName
                             , aXmlFileName: String
                             ; aXsdDescr: TXsdDescr
                             ; aXml: TXml
                             ; ExistingFile: Boolean
                             );
    procedure CreateWsdlChild ( aWsdlFileName
                              , aXmlFileName: String
                              ; aWsdl: TWsdl
                              ; aOperation: TWsdlOperation
                              ; aXml: TXml
                              ; ExistingFile: Boolean
                              );
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses CHILDWIN
   , cbAbout
   , OpenXsdXmlUnit
   , OpenXsdUnit
   , xmlUtilz
   ;

procedure TMainForm.CreateWsdlChild ( aWsdlFileName
                                    , aXmlFileName: String
                                    ; aWsdl: TWsdl
                                    ; aOperation: TWsdlOperation
                                    ; aXml: TXml
                                    ; ExistingFile: Boolean
                                    );
var
  Child: TAxisDeerCF;
begin
  Child := TAxisDeerCF.Create(Application);
  Child.descrFileName := aWsdlFileName;
  Child.XmlFileName := aXmlFileName;
  Child.Wsdl := aWsdl;
  Child.Operation := aOperation;
  Child.Xml := aXml;
  Child.descrType := dtWsdl;
  Child.stubChanged := False;
  Child.ExistingFile := ExistingFile;
end;

procedure TMainForm.CreateXsdChild(aXsdFileName, aXmlFileName: String; aXsdDescr: TXsdDescr; aXml: TXml; ExistingFile: Boolean);
var
  Child: TAxisDeerCF;
begin
  Child := TAxisDeerCF.Create(Application);
  Child.descrFileName := aXsdFileName;
  Child.XmlFileName := aXmlFileName;
  Child.XsdDescr := aXsdDescr;
  Child.Xml := aXml;
  Child.descrType := dtXsd;
  Child.stubChanged := False;
  Child.ExistingFile := ExistingFile;
end;

procedure TMainForm.CreateJsnChild(aJsnSchemaFileName, aJsnFileName: String;
  aXsdDescr: TXsdDescr; aXml: TXml; ExistingFile: Boolean);
var
  Child: TAxisDeerCF;
begin
  Child := TAxisDeerCF.Create(Application);
  Child.descrFileName := aJsnSchemaFileName;
  Child.XmlFileName := aJsnFileName;
  Child.XsdDescr := aXsdDescr;
  Child.Xml := aXml;
  Child.descrType := dtJson;
  Child.stubChanged := False;
  Child.ExistingFile := ExistingFile;
end;

procedure TMainForm.FileNew1Execute(Sender: TObject);
var
  xForm: TOpenXsdForm;
begin
  Application.CreateForm(TOpenXsdForm, xForm);
  try
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
    begin
      try
//      if LowerCase(ExtractFileExt (xForm.xsdFileName)) = '.json' then
        if xForm.SchemaType = stJsonType then
          CreateJsnChild( xForm.xsdFileName
                        , 'Document' + IntToStr(MDIChildCount + 1) + '.json'
                        , xForm.xsdDescr
                        , xForm.Xml
                        , False
                        )
        else
          CreateXsdChild( xForm.xsdFileName
                        , 'Document' + IntToStr(MDIChildCount + 1) + '.xml'
                        , xForm.xsdDescr
                        , xForm.Xml
                        , False
                        );
      except
        xForm.Xml.Free;
        xForm.xsdDescr.Free;
        raise;
      end;
    end;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TMainForm.FileOpen1Execute(Sender: TObject);
var
  xForm: TOpenXsdXmlForm;
begin
  Application.CreateForm(TOpenXsdXmlForm, xForm);
  try
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
    begin
      try
        if UpperCase(ExtractFileExt(xForm.xsdFileName)) = '.XSD' then
          CreateXsdChild(xForm.xsdFileName, xForm.xmlFileName, xForm.xsdDescr, xForm.Xml, True)
        else
          CreateWsdlChild ( xForm.xsdFileName
                          , xForm.xmlFileName
                          , xForm.Wsdl
                          , xForm.Operation
                          , xForm.Xml
                          , True);
      except
        xForm.Xml.Free;
        xForm.xsdDescr.Free;
        raise;
      end;
    end;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TMainForm.FileSave1Execute(Sender: TObject);
begin
  if Assigned (ActiveMDIChild) then
    with ActiveMDIChild as TAxisDeerCF do
    begin
      SaveToFile (XmlFileName);
      ExistingFile := True;
      stubChanged := False;
    end;
end;

procedure TMainForm.FileSave1Update(Sender: TObject);
begin
  FileSave1.Enabled := Assigned (ActiveMDIChild);
end;

procedure TMainForm.FileSaveAsActionExecute(Sender: TObject);
begin
  if Assigned (ActiveMDIChild) then
  begin
    with ActiveMDIChild as TAxisDeerCF do
    begin
      SaveDialog.FileName := XmlFileName;
      SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
      if SaveDialog.Execute then
        SaveToFile (SaveDialog.FileName);
    end;
  end;
end;

procedure TMainForm.FileSaveAsActionUpdate(Sender: TObject);
begin
  FileSaveAsAction.Enabled := Assigned (ActiveMDIChild);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Xsdz.xsdMaxDepthXmlGen := 9999;
  Xsdz.xsdMaxDepthBillOfMaterials := 2;
  IniFile := TFormIniFile.Create(Self, True);
  IniFile.Restore;
end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
var
  xForm: TAboutBox;
begin
  Application.CreateForm(TAboutBox, xForm);
  try
    xForm.NameLabel.Caption := 'axisDeer - Schema based XML editor';
    xForm.CopyRightLabel.Caption := 'Copyright © 2011 - 215 Jan Bouwman';
    xForm.ShowModal;
  finally
    FreeAndNil (xForm);
  end;
end;

procedure TMainForm.EditCopy1Execute(Sender: TObject);
begin
  if Assigned (ActiveMDIChild) then
    (ActiveMDIChild as TAxisDeerCF).Copytoclipboard1Click(nil);
end;

procedure TMainForm.EditCopy1Update(Sender: TObject);
begin
  EditCopy1.Enabled := Assigned (ActiveMDIChild)
                   and Assigned ((ActiveMDIChild as TAxisDeerCF).InWsdlTreeView.FocusedNode)
                     ;
end;

procedure TMainForm.EditPaste1Execute(Sender: TObject);
begin
  if Assigned (ActiveMDIChild) then
    (ActiveMDIChild as TAxisDeerCF).WsdlPasteFromClipboardMenuItemClick(nil);
end;

procedure TMainForm.EditPaste1Update(Sender: TObject);
begin
  EditPaste1.Enabled := Assigned (ActiveMDIChild)
                    and Assigned ((ActiveMDIChild as TAxisDeerCF).InWsdlTreeView.FocusedNode)
                      ;
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

end.
