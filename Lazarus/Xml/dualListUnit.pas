unit dualListUnit;

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
  Buttons, ImgList, FormIniFilez,ExtCtrls;

type
  TdualListForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    ActionImageList: TImageList;
    Panel1: TPanel;
    mPanel: TPanel;
    rlstPanel: TPanel;
    rbPanel: TPanel;
    DstLabel: TPanel;
    lPanel: TPanel;
    rPanel: TPanel;
    SrcLabel: TPanel;
    llstPanel: TPanel;
    lbPanel: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mPanelResize(Sender: TObject);
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
    procedure FormShow(Sender: TObject);
    procedure DstListClick(Sender: TObject);
    procedure SrcListClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fEmptySelectionAllowed: Boolean;
    function getDstCaption: String;
    function getSrcCaption: String;
    procedure setDstCaption(const Value: String);
    procedure setSrcCaption(const Value: String);
    procedure DstExchange (Dir: Integer);
  public
    property EmptySelectionAllowed: Boolean read fEmptySelectionAllowed write fEmptySelectionAllowed;
    property SrcCaption: String read getSrcCaption write setSrcCaption;
    property DstCaption: String read getDstCaption write setDstCaption;
  end;

var
  dualListForm: TdualListForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TdualListForm.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
  SetButtons;
end;

procedure TdualListForm.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
  SetButtons;
end;

procedure TdualListForm.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I],
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
  SetButtons;
end;

procedure TdualListForm.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
  SetButtons;
end;

procedure TdualListForm.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TdualListForm.SetButtons;
var
  SrcEmpty, DstEmpty, SrcItemsSelected, DstItemsSelected: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  SrcItemsSelected := SrcList.SelCount > 0;
  DstEmpty := DstList.Items.Count = 0;
  DstItemsSelected := DstList.SelCount > 0;
  IncludeBtn.Enabled := not SrcEmpty and SrcItemsSelected;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty and DstItemsSelected;
  ExAllBtn.Enabled := not DstEmpty;
  OKBtn.Enabled := EmptySelectionAllowed or (not DstEmpty);
  UpBtn.Enabled := False;
  DownBtn.Enabled := False;
  if (DstList.Items.Count > 1)
  and (DstList.SelCount = 1)
  then
  begin
    if not DstList.Selected [0] then
      UpBtn.Enabled := True;
    if not DstList.Selected [DstList.Items.Count - 1] then
      DownBtn.Enabled := True;
  end;
end;

function TdualListForm.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := -1;
end;

procedure TdualListForm.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = -1 then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

procedure TdualListForm.FormShow(Sender: TObject);
begin
  SetButtons;
end;

procedure TdualListForm.DstListClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TdualListForm.SrcListClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TdualListForm.DstExchange (Dir: Integer);
var
  x: Integer;
  Selected: Integer;
begin
  for x := 0 to DstList.Items.Count - 1 do
    if DstList.Selected [x] then
      Selected := x;
  DstList.Items.Exchange (Selected + Dir, Selected);
  DstList.Selected [Selected + Dir] := True;
  SetButtons;
end;

procedure TdualListForm.UpBtnClick(Sender: TObject);
begin
  DstExchange (-1);
end;

procedure TdualListForm.DownBtnClick(Sender: TObject);
begin
  DstExchange (1);
end;

function TdualListForm.getDstCaption: String;
begin
  result := DstLabel.Caption;
end;

function TdualListForm.getSrcCaption: String;
begin
  result := SrcLabel.Caption;
end;

procedure TdualListForm.setDstCaption(const Value: String);
begin
  DstLabel.Caption := Value;
end;

procedure TdualListForm.setSrcCaption(const Value: String);
begin
  SrcLabel.Caption := Value;
end;

procedure TdualListForm.mPanelResize(Sender: TObject);
begin
  rPanel.Width := mPanel.Width div 2;
end;

procedure TdualListForm.FormCreate(Sender: TObject);
begin
  IniFile := TFormIniFile.Create (Self);
end;

procedure TdualListForm.FormDestroy(Sender: TObject);
begin
  IniFile.Free;
end;

end.
