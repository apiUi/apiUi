unit BitWiseUnit;

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
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormIniFilez, VirtualTrees, ExtCtrls, StdCtrls, Ipmz;

type
  TBitWiseForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    IntegerValueEdit: TEdit;
    OKButton: TButton;
    Button2: TButton;
    VST1: TVirtualStringTree;
    Splitter1: TSplitter;
    VST2: TVirtualStringTree;
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VST1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure VST2NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure VST2Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VST2Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VST2GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VST2InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VST2Checked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST1Checked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST1Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VST1Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VST1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fIntegerValue: Int64;
    fIpmItem: TIpmItem;
    CaptionList: TStringList;
    procedure setIpmItem(const Value: TIpmItem);
    function getReadOnly: Boolean;
    procedure setReadOnly(const Value: Boolean);
    procedure setIntegerValue(const Value: Int64);
    property IntegerValue: Int64 read fIntegerValue write setIntegerValue;
  public
    { Public declarations }
    property ReadOnly: Boolean read getReadOnly write setReadOnly;
    property Ipm: TIpmItem read fIpmItem write setIpmItem;
  end;

  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
  end;


var
  BitWiseForm: TBitWiseForm;

implementation


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TBitWiseForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
  CaptionList := TstringList.Create;
  for x := 0 to 63 do
    CaptionList.Add(''); 
  VST1.NodeDataSize := SizeOf(TMyRec);
  VST1.RootNodeCount := 32;
  VST2.NodeDataSize := SizeOf(TMyRec);
  VST2.RootNodeCount := 32;
end;

procedure TBitWiseForm.FormDestroy(Sender: TObject);
begin
  CaptionList.Free;
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

procedure TBitWiseForm.VST1InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;
begin
  Data:=Sender.GetNodeData(Node);
  Data.Caption := 'Caption ' + IntToStr (Node.Index);
  Node.CheckType:= ctCheckBox;
end;

procedure TBitWiseForm.VST1Checked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if (Node.CheckState = csCheckedNormal) then
    IntegerValue := fIntegerValue + (1 shl Node.Index)
  else
    IntegerValue := fIntegerValue - (1 shl Node.Index);
end;

procedure TBitWiseForm.VST2Checked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  I64: Int64;
begin
  I64 := 1;
  I64 := I64 shl (Node.Index + 32);
  if (Node.CheckState = csCheckedNormal) then
    IntegerValue := fIntegerValue + I64
  else
    IntegerValue := fIntegerValue - I64;
end;

procedure TBitWiseForm.VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  CheckValue: Int64;
begin
  case column of
    0: begin
         CheckValue := 1 shl Node.Index;
         if ((fIntegerValue and CheckValue) > 0) then
           Node.CheckState := csCheckedNormal
         else
           Node.CheckState := csUnCheckedNormal;
         CellText := 'Bit ' + IntToStr (Node.Index);
       end;
    1: begin
         CellText := CaptionList.Strings[Node.Index];
       end;
  end
end;

procedure TBitWiseForm.VST2GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  I64: Int64;
begin
  case column of
    0: begin
        I64 := 1;
        I64 := I64 shl (Node.Index + 32);
        if ((fIntegerValue and I64) <> 0) then
          Node.CheckState := csCheckedNormal
        else
          Node.CheckState := csUnCheckedNormal;
        CellText := 'Bit ' + IntToStr (Node.Index + 32);
       end;
    1: CellText := CaptionList.Strings[Node.Index+32];
  end
end;

procedure TBitWiseForm.setIntegerValue(const Value: Int64);
begin
  fIntegerValue := Value;
  VST1.InvalidateColumn(0);
  IntegerValueEdit.Text := IntToStr (fIntegerValue);
end;

procedure TBitWiseForm.VST1Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := not ReadOnly;
end;

procedure TBitWiseForm.VST1Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column = 1);
end;

procedure TBitWiseForm.VST2InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;
begin
  Data:=Sender.GetNodeData(Node);
  Data.Caption := 'Caption ' + IntToStr (Node.Index + 32);
  Node.CheckType:= ctCheckBox;
end;

function TBitWiseForm.getReadOnly: Boolean;
begin
  result := VST1.ParentColor;
end;

procedure TBitWiseForm.setReadOnly(const Value: Boolean);
begin
  VST1.ParentColor := Value;
  VST2.ParentColor := Value;
end;

procedure TBitWiseForm.FormShow(Sender: TObject);
begin
  IntegerValue := StrToInt64Def(Ipm.Value, 0);
  if Ipm.Comp then
  begin
    case Ipm.Bytes of
      2: Begin
           VST1.RootNodeCount := 16;
           VST2.RootNodeCount := 0;
         end;
      4: Begin
           VST1.RootNodeCount := 32;
           VST2.RootNodeCount := 0;
         end;
      else
         begin
           VST1.RootNodeCount := 32;
           VST2.RootNodeCount := 32;
         end;
    end;
  end;
end;

procedure TBitWiseForm.OKButtonClick(Sender: TObject);
var
  x: Integer;
begin
  if Assigned (Ipm) then
  begin
    if (not ReadOnly) then
      Ipm.Value := IntToStr (IntegerValue);
    with TFormIniFile.Create(self, False) do
    try
      for x := 0 to 63 do
      begin
        StringByName['BitWiseCaption_' + IntToStr (x) + Ipm.Caption]:= CaptionList.Strings [x];
      end;
    finally
      Free;
    end;
  end;
end;

procedure TBitWiseForm.VST2Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := not ReadOnly;
end;

procedure TBitWiseForm.VST2Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column = 1);
end;

procedure TBitWiseForm.VST2NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: WideString);
begin
  if(Column = 1) then
    CaptionList.Strings [Node.Index + 32] := NewText;
end;

procedure TBitWiseForm.VST1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: WideString);
begin
  if (Column = 1) then
    CaptionList.Strings [Node.Index] := NewText;
end;

procedure TBitWiseForm.VSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TVirtualStringTree).FocusedNode
    := (Sender as TVirtualStringTree).GetNodeAt(X, Y);
end;

procedure TBitWiseForm.VSTColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TBitWiseForm.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.Selected [Sender.FocusedNode] := True;
end;

procedure TBitWiseForm.setIpmItem(const Value: TIpmItem);
var
  x: Integer;
begin
  fIpmItem := Value;
  with TFormIniFile.Create(self, False) do
  try
    if Assigned (Value) then
      for x := 0 to 63 do
        CaptionList.Strings [x] := StringByName['BitWiseCaption_' + IntToStr (x) + Value.Caption];
  finally
    Free;
  end;
end;

end.
