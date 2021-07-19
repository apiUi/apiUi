{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit QueryNewElementUnit;

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
  Classes, Graphics, Forms, Controls,
  Buttons, ExtCtrls, SysUtils, FormIniFilez, RegExpr, Dialogs,EditBtn,StdCtrls
  , Xsdz
  ;

type

  { TQueryNewElementForm }

  TQueryNewElementForm = class(TForm)
    CancelButton : TBitBtn ;
    NameSpaceEdit: TComboBox;
    NameEdit: TComboBox;
    FileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TagNameEdit: TLabeledEdit;
    OkButton : TBitBtn ;
    ElemDataTypeGroupBox: TRadioGroup;
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure PopulateNamelist(Sender: TObject);
    procedure EnableOkButton;
    function NameIsOk: Boolean;
    procedure TagNameEditChange(Sender: TObject);
  private
    xsddesc: TXsdDescr;
    function getElementOrTypedef: TElementOrTypeDefRef;
    function getFileName: String;
    function getName: String;
    function getNameSpace: String;
    function getTagName: String;
  public
    { Public declarations }
    property FileName: String read getFileName;
    property Namespace: String read getNameSpace;
    property ElementOrTypeDefRef: TElementOrTypeDefRef read getElementOrTypedef;
    property Name: String read getName;
    property TagName: String read getTagName;
  end;

var
  QueryNewElementForm: TQueryNewElementForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TQueryNewElementForm.FormCreate(Sender: TObject);
begin
  xsddesc := TXsdDescr.Create;
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TQueryNewElementForm.FormShow(Sender: TObject);
begin
  EnableOkButton;
end;

procedure TQueryNewElementForm.NameEditChange(Sender: TObject);
begin
  EnableOkButton;
end;

procedure TQueryNewElementForm.PopulateNamelist(Sender: TObject);
var
  x: Integer;
begin
  NameEdit.Items.Clear;
  with NameSpaceEdit do
  begin
    if (ItemIndex > -1)
    and (ItemIndex < Items.Count) then
    begin
      if ElemDataTypeGroupBox.ItemIndex = 0 then
      begin
        for x := 0 to xsddesc.TypeDef.ElementDefs.Count - 1 do
        begin
          if xsddesc.TypeDef.ElementDefs.Xsds[x].ElementNameSpace = Items [ItemIndex] then
            NameEdit.Items.Add (xsddesc.TypeDef.ElementDefs.Xsds[x].ElementName);
        end;
      end
      else
      begin
        for x := 0 to xsddesc.TypeDefs.Count - 1 do
        begin
          if xsddesc.TypeDefs.XsdDataTypes[x].NameSpace = Items [ItemIndex] then
            NameEdit.Items.Add (xsddesc.TypeDefs.XsdDataTypes[x].Name);
        end;
      end;
    end;
  end;
  EnableOkButton;
end;

procedure TQueryNewElementForm.EnableOkButton;
begin
  OkButton.Enabled := (ElemDataTypeGroupBox.ItemIndex > -1)
                  and (NameEdit.ItemIndex > -1)
                  and NameIsOk
                    ;
end;

function TQueryNewElementForm.NameIsOk: Boolean;
begin
  result := false;
  if (TagNameEdit.Text <> '') then
  with TRegExpr.Create('^[a-zA-Z0-9\-_]+$') do
  try
    result := Exec(TagNameEdit.Text);
  finally
    free;
  end;
end;

procedure TQueryNewElementForm.TagNameEditChange(Sender: TObject);
begin
  EnableOkButton;
end;

function TQueryNewElementForm.getElementOrTypedef: TElementOrTypeDefRef;
begin
  result := TElementOrTypeDefRef (ElemDataTypeGroupBox.ItemIndex);
end;

function TQueryNewElementForm.getFileName: String;
begin
  Result := FileNameEdit.Text;
end;

function TQueryNewElementForm.getName: String;
begin
  result := NameEdit.Text;
end;

function TQueryNewElementForm.getNameSpace: String;
begin
  Result := NameSpaceEdit.Text;
end;

function TQueryNewElementForm.getTagName: String;
begin
  result := TagNameEdit.Text;
end;

procedure TQueryNewElementForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
  xsddesc.Free;
end;

procedure TQueryNewElementForm.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
var
  x: Integer;
begin
  NameSpaceEdit.Items.Clear;
  NameEdit.Items.Clear;
  xsddesc.Clear;
  xsddesc.LoadXsdFromFile(Value, nil, nil);
  NameSpaceEdit.Items.Text := xsddesc.NameSpaceList.Text;
  EnableOkButton;
end;

end.

