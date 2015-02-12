unit ccbGenerationParamsUnit;

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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FormIniFilez;

type
  TccbGenerationParamsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    NamePrefixEdit: TEdit;
    NameSuffixEdit: TEdit;
    ConditionNamePrefixEdit: TEdit;
    ConditionNameSuffixEdit: TEdit;
    StartingLevelEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    IniFile: TFormIniFile;
    function getConditionNamePrefix: String;
    function getConditionNameSuffix: String;
    function getNamePrefix: String;
    function getNameSuffix: String;
    function getStartingLevel: String;
    procedure setConditionNamePrefix(const Value: String);
    procedure setConditionNameSuffix(const Value: String);
    procedure setNamePrefix(const Value: String);
    procedure setNameSuffix(const Value: String);
    procedure setStartingLevel(const Value: String);
    { Private declarations }
  public
    property StartingLevel: String read getStartingLevel write setStartingLevel;
    property NamePrefix: String read getNamePrefix write setNamePrefix;
    property NameSuffix: String read getNameSuffix write setNameSuffix;
    property ConditionNamePrefix: String read getConditionNamePrefix write setConditionNamePrefix;
    property ConditionNameSuffix: String read getConditionNameSuffix write setConditionNameSuffix;
  end;

var
  ccbGenerationParamsForm: TccbGenerationParamsForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TccbGenerationParamsForm.Button1Click(Sender: TObject);
var
  x: Integer;
begin
  try
    x:= StrToInt (StartingLevel);
  except
    raise Exception.Create('Starting level must be a valid integer');
  end;
end;

function TccbGenerationParamsForm.getConditionNamePrefix: String;
begin
  result := ConditionNamePrefixEdit.Text;
end;

function TccbGenerationParamsForm.getConditionNameSuffix: String;
begin
  result := ConditionNameSuffixEdit.Text;
end;

function TccbGenerationParamsForm.getNamePrefix: String;
begin
  result := NamePrefixEdit.Text;
end;

function TccbGenerationParamsForm.getNameSuffix: String;
begin
  result := NameSuffixEdit.Text;
end;

function TccbGenerationParamsForm.getStartingLevel: String;
begin
  result := StartingLevelEdit.Text;
end;

procedure TccbGenerationParamsForm.setConditionNamePrefix(const Value: String);
begin
  ConditionNamePrefixEdit.Text := Value;
end;

procedure TccbGenerationParamsForm.setConditionNameSuffix(const Value: String);
begin
  ConditionNameSuffixEdit.Text := Value;
end;

procedure TccbGenerationParamsForm.setNamePrefix(const Value: String);
begin
  NamePrefixEdit.Text := Value;
end;

procedure TccbGenerationParamsForm.setNameSuffix(const Value: String);
begin
  NameSuffixEdit.Text := Value;
end;

procedure TccbGenerationParamsForm.setStartingLevel(const Value: String);
begin
  StartingLevelEdit.Text := Value;
end;

procedure TccbGenerationParamsForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
  IniFile.Restore;
end;

procedure TccbGenerationParamsForm.FormDestroy(Sender: TObject);
begin
  IniFile.Save;
  IniFile.Free;
end;

end.
