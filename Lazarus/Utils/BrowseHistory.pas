unit BrowseHistory;

{$MODE Delphi}

interface
uses Classes
   , Menus
   , Dialogs
   ;

type
  TBrowseHistory = class(TObject)
private
  StringUsed :Boolean;
  ObjectUsed :Boolean;
  Items: TStringList;
  LastItem: TObject;
  LastString: String;
  HistoryIndex: Integer;
  procedure BackwardMenuItemClick (Sender: TObject);
  procedure BackwardPopUpMenuPopup (Sender: TObject);
  procedure ForwardMenuItemClick (Sender: TObject);
  procedure ForwardPopUpMenuPopup (Sender: TObject);
public
  Name: String;
  BackwardPopUpMenu: TPopupMenu;
  ForwardPopUpMenu: TPopupMenu;
  OnMenuItemClick: TNotifyEvent;
  function OkToEnableBackward: Boolean;
  function OkToEnableForward: Boolean;
  procedure Clear;
  procedure Add (Caption: String);
  function GetBackward: String;
  function GetForward: String;
  procedure AddObject (Caption: String; Item: TObject);
  function GetBackwardObject: TObject;
  function GetForwardObject: TObject;
  constructor Create;
  destructor Destroy;
end;

type TBrowseHistoryList = class (TStringList)
protected
  function GetBrowseHistory (Index: integer): TBrowseHistory;
public
  property BrowseHistorys [Index: integer]: TBrowseHistory read GetBrowseHistory;
  procedure Clear; override;
end;

implementation

uses
  SysUtils
;

function TBrowseHistory.OkToEnableBackward: Boolean;
begin
  if Items.Count > 0 then
    result := (HistoryIndex > 0)
  else
    result := False;
end;

function TBrowseHistory.OkToEnableForward: Boolean;
begin
  if Items.Count > 0 then
    result := (HistoryIndex < (Items.Count - 1))
  else
    result := False;
end;

procedure TBrowseHistory.Clear;
begin
  Items.Clear;
  HistoryIndex := -1;
  LastItem := nil;
  LastString := '';
  StringUsed := False;
  ObjectUsed := False;
end;

procedure TBrowseHistory.Add (Caption: String);
begin
  if ObjectUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  StringUsed := True;
  if Caption <> LastString then
  begin
    Items.Add (Caption);
    HistoryIndex := Items.Count - 1;
    LastString := Caption;
  end;
end;

function TBrowseHistory.GetBackward: String;
begin
  if ObjectUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  StringUsed := True;
  if OkToEnableBackward then
  begin
    Dec (HistoryIndex);
    result := Items.Strings [HistoryIndex];
    LastString := result;
  end
  else
    raise Exception.Create ('Index out of range');
end;

function TBrowseHistory.GetForward: String;
begin
  if ObjectUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  StringUsed := True;
  if OkToEnableForward then
  begin
    Inc (HistoryIndex);
    result := Items.Strings [HistoryIndex];
    LastString := result;
  end
  else
    raise Exception.Create ('Index out of range');
end;


procedure TBrowseHistory.AddObject (Caption: String; Item: TObject);
begin
  if StringUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  ObjectUsed := True;
  if (Item <> LastItem)
  and (Caption <> LastString) then
  begin
    Items.AddObject (Caption, Item);
    HistoryIndex := Items.Count - 1;
    LastItem := Item;
    LastString := Caption;
  end;
end;

function TBrowseHistory.GetBackwardObject: TObject;
begin
  if StringUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  ObjectUsed := True;
  if OkToEnableBackward then
  begin
    Dec (HistoryIndex);
    result := Items.Objects [HistoryIndex];
    LastItem := result;
    LastString := Items.Strings [HistoryIndex];
  end
  else
    raise Exception.Create ('Index out of range');
end;

function TBrowseHistory.GetForwardObject: TObject;
begin
  if StringUsed then
    raise Exception.Create ('Can not mix use of objects and strings');
  ObjectUsed := True;
  if OkToEnableForward then
  begin
    Inc (HistoryIndex);
    result := Items.Objects [HistoryIndex];
    LastItem := result;
    LastString := Items.Strings [HistoryIndex];
  end
  else
    raise Exception.Create ('Index out of range');
end;

procedure TBrowseHistory.BackwardMenuItemClick (Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem (Sender);
  HistoryIndex := HistoryIndex - 1 - MenuItem.MenuIndex;
  LastItem := Items.Objects [HistoryIndex];
  LastString := Items.Strings [HistoryIndex];
  if Assigned (OnMenuItemClick) then
  begin
    if ObjectUsed then
      OnMenuItemClick (LastItem)
    else
      OnMenuItemClick (TObject (LastString));
  end
  else
    Raise Exception.Create ('No onclick event for BrowseHistory');
end;

procedure TBrowseHistory.BackwardPopUpMenuPopup (Sender: TObject);
var
  x: Integer;
  PopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  PopupMenu := Sender as TPopupMenu;
  PopupMenu.Items.Clear;
  if OktoEnableBackward then
  begin
    x := HistoryIndex - 1;
    while (x >= 0)
    and (PopupMenu.Items.Count < 61)
    do begin
      MenuItem := TMenuItem.Create (nil);
      PopUpMenu.Items.Add (MenuItem);
      MenuItem.OnClick := BackwardMenuItemClick;
      MenuItem.Caption := Items.Strings [x];
      Dec (x);
    end;
  end;
end;

procedure TBrowseHistory.ForwardMenuItemClick (Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem (Sender);
  HistoryIndex := HistoryIndex + 1 + MenuItem.MenuIndex;
  LastItem := Items.Objects [HistoryIndex];
  LastString := Items.Strings [HistoryIndex];
  if Assigned (OnMenuItemClick) then
  begin
    if ObjectUsed then
      OnMenuItemClick (LastItem)
    else
      OnMenuItemClick (TObject (LastString));
  end
  else
    Raise Exception.Create ('No onclick event for BrowseHistory');
end;

procedure TBrowseHistory.ForwardPopUpMenuPopup (Sender: TObject);
var
  x: Integer;
  PopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  PopupMenu := Sender as TPopupMenu;
  PopupMenu.Items.Clear;
  if OktoEnableForward then
  begin
    x := HistoryIndex + 1;
    while (x < Items.Count)
    and (PopupMenu.Items.Count < 61)
    do begin
      MenuItem := TMenuItem.Create (nil);
      PopUpMenu.Items.Add (MenuItem);
      MenuItem.OnClick := ForwardMenuItemClick;
      MenuItem.Caption := Items.Strings [x];
      Inc (x);
    end;
  end;
end;

constructor TBrowseHistory.Create;
begin
  inherited Create;
  Items := TStringList.Create;
  HistoryIndex := -1;
  LastItem := nil;
  LastString := '';
  StringUsed := False;
  ObjectUsed := False;
  BackwardPopupMenu := TPopupMenu.Create (nil);
  BackwardPopupMenu.OnPopup := BackwardPopupMenuPopup;
  ForwardPopupMenu := TPopupMenu.Create (nil);
  ForwardPopupMenu.OnPopup := ForwardPopupMenuPopup;
end;

destructor TBrowseHistory.Destroy;
begin
  Items.Clear;
  Items.Free;
  BackwardPopupMenu.Items.Clear;
  BackwardPopupMenu.Free;
  ForwardPopupMenu.Items.Clear;
  ForwardPopupMenu.Free;
  inherited Destroy;
end;

function TBrowseHistoryList.GetBrowseHistory (Index: integer): TBrowseHistory;
begin
  result := TBrowseHistory (Objects [index]);
end;

procedure TBrowseHistoryList.Clear;
begin
  inherited Clear;
end;

end.
