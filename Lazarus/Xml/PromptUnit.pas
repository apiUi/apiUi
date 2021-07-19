{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit PromptUnit;

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
  Buttons, FormIniFilez, ExtCtrls;

type

  { TPromptForm }

  TPromptForm = class(TForm)
    CancelButton : TBitBtn ;
    OkButton : TBitBtn ;
    Panel1: TPanel;
    Panel2: TPanel;
    PromptEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PromptEditChange(Sender: TObject);
  private
    { Private declarations }
    IniFile: TFormIniFile;
    function GetValue: String;
    procedure SetValue(AValue: String);
  public
    Numeric: Boolean;
    ReadOnly: Boolean;
    Pattern: String;
    property Value: String read GetValue write SetValue;
  end;

var
  PromptForm: TPromptForm;

implementation

uses RegExpr;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TPromptForm.FormShow(Sender: TObject);
begin
  PromptEdit.ReadOnly := ReadOnly;
  if ReadOnly then
    PromptEdit.ParentColor := True
  else
  begin
    PromptEdit.Color := clWindow;
    PromptEdit.OnChange (self);
    PromptEdit.SetFocus;
    PromptEdit.SelectAll;
  end;
end;

procedure TPromptForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self, True);
  IniFile.Restore;
end;

procedure TPromptForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

procedure TPromptForm.PromptEditChange(Sender: TObject);
var
  xFloat: Extended;
  DoEnable: Boolean;
begin
  DoEnable := True; // start optimistic
  if (PromptEdit.Text <> '') then
  begin
    if (Numeric)
    then begin
      try
        xFloat := StrToFloat (PromptEdit.Text);
      except
        DoEnable := False;
      end;
    end;
    if (Pattern <> '')
    and (PromptEdit.Text <> '')
    then begin
      with TRegExpr.Create do
      try
        Expression := '^' + Pattern + '$';
        DoEnable := Exec(PromptEdit.Text);
      finally
        Free;
      end;
    end;
  end;
  OkButton.Enabled := DoEnable;
end;

function TPromptForm.GetValue: String;
begin
  result := PromptEdit.Text;
end;

procedure TPromptForm.SetValue(AValue: String);
begin
  PromptEdit.Text := AValue;
end;

end.
