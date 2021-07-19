{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit SaveProjectAsUnit;

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

  { TSaveProjectAsForm }

  TSaveProjectAsForm = class(TForm)
    CancelButton : TBitBtn ;
    DirectoryEdit1: TDirectoryEdit;
    FileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    OkButton : TBitBtn ;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EnableOkButton;
    procedure TagNameEditChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  SaveProjectAsForm: TSaveProjectAsForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSaveProjectAsForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
end;

procedure TSaveProjectAsForm.FormShow(Sender: TObject);
begin
  EnableOkButton;
end;

procedure TSaveProjectAsForm.EnableOkButton;
begin
  OkButton.Enabled := True
                    ;
end;

procedure TSaveProjectAsForm.TagNameEditChange(Sender: TObject);
begin
  EnableOkButton;
end;

procedure TSaveProjectAsForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

end.

