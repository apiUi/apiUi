{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit FormIniFilez;

{$mode delphi}{$H+}

interface

uses SysUtils
   , Types
   , Classes
   , Controls
   , StdCtrls
   , ExtCtrls
   , Forms
   , ComCtrls
   , Registry
   , Dialogs
   , base64
   ;

type TOnYNACClickEvent = procedure ( Sender:TObject
                                   ; ClickResult: TModalResult
                                   ; var MoreData: Boolean
                                   ) of Object;
type
  THackControl = class(TWinControl);
  THackEdit = class(TCustomEdit);

type

{ TFormIniFile }

 TFormIniFile = class (TRegistry)
private
  fKey: String;
  fForm: TForm;
  fName: String;
    function getIntegerByName(Index: String): Integer;
    function getIntegerByNameDef(Index: String; Default: Integer): Integer;
    procedure setIntegerByName(Index: String; const Value: Integer);
    function getBooleanByNameDef(Index: String; Default: Boolean): Boolean;
    function getStringByNameDef(Index, Default: String): String;
    function getBooleanByName(Index: String): Boolean; Overload;
    procedure setBooleanByName(Index: String; const Value: Boolean);
  function getStringByName(Index: String): String;
  procedure setStringByName(Index: String; const Value: String);
  procedure Initialize(initScreenPos: Boolean);
  procedure fOpenKeys;
  procedure fCloseKeys;
public
  function getBooleanByName(Index: String; DefValue: Boolean): Boolean; Overload;
  procedure Restore;
  procedure Save;
  property BooleanByName [Index: String]: Boolean read getBooleanByName write setBooleanByName;
  property BooleanByNameDef [Index: String; Default: Boolean]: Boolean read getBooleanByNameDef;
  property StringByName [Index: String]: String read getStringByName write setStringByName;
  property StringByNameDef [Index, Default: String]: String read getStringByNameDef;
  property IntegerByName [Index: String]: Integer read getIntegerByName write setIntegerByName;
  property IntegerByNameDef [Index: String; Default: Integer]: Integer read getIntegerByNameDef;
  constructor Create (aForm: TForm; initScreenPos: Boolean); Overload;
  destructor Destroy; override;
end;

type
  THackSplitter = class (TSplitter)
public
  procedure Save (aIniFile: TFormIniFile);
  procedure Restore (aIniFile: TFormIniFile);
end;


implementation

uses Math
   , VirtualTrees
   ;

{ TFormIniFile }


constructor TFormIniFile.Create(aForm: TForm; initScreenPos: Boolean);
begin
  inherited Create;
  fName := aForm.Name;
  fForm := aForm;
  fKey := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), 'Ini');
  Initialize(initScreenPos);
end;

destructor TFormIniFile.Destroy;
begin
  if Assigned (fForm) then
  begin
    IntegerByName['FormTop'] := fForm.Top;
    IntegerByName['FormLeft'] := fForm.Left;
    IntegerByName['FormHeight'] := fForm.Height;
    IntegerByName['FormWidth'] := fForm.Width;
    BooleanByName['FormMaximized'] := (fForm.WindowState = wsMaximized);
  end;
  inherited;
end;

procedure TFormIniFile.Save;

  procedure _SaveListView (aControl: TListView);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Columns.Count - 1 do
      IntegerByName[aControl.Name + ':Width0' + IntToStr (x)] := aControl.Columns [x].Width;
  end;

  procedure _SaveTreeView (aControl: TVirtualStringTree);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Header.Columns.Count - 1 do
      IntegerByName[aControl.Name + ':Width0' + IntToStr (x)] := aControl.Header.Columns[x].Width;
  end;

  procedure _Save (aCntrl: THackControl);
  var
    i: Integer;
    WinControl: TWinControl;
  begin
    if aCntrl is TWinControl then
    begin
      WinControl := aCntrl as TWinControl;
      if (WinControl is TCustomListView) then
        _SaveListView (WinControl as TListView);
      if WinControl is TSplitter then
        THackSplitter(WinControl).Save (self);
      if (WinControl is TCustomVirtualStringTree) then
        _SaveTreeView (WinControl as TVirtualStringTree);

      for i := 0 to aCntrl.ControlCount  - 1 do
      begin
        if aCntrl.Controls [i] is TWinControl then
          _Save (THackControl(aCntrl.Controls [i]));
      end;
    end;
  end;
var
  x: Integer;
begin
  for x := 0 to fForm.ControlCount - 1 do
    _Save (THackControl (fForm.Controls[x]));
end;


procedure TFormIniFile.Restore;

  procedure _RestoreListView (aControl: TListView);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Columns.Count - 1 do
      aControl.Columns [x].Width := IntegerByNameDef[aControl.Name + ':Width0' + IntToStr (x),aControl.Columns [x].Width];
  end;

  procedure _RestoreTreeView (aControl: TVirtualStringTree);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Header.Columns.Count - 1 do
      aControl.Header.Columns [x].Width := IntegerByNameDef[aControl.Name + ':Width0' + IntToStr (x), aControl.Header.Columns [x].Width];
  end;

  procedure _Restore (aCntrl: THackControl);
  var
    i: Integer;
    WinControl: TWinControl;
  begin
    if aCntrl is TWinControl then
    begin
      WinControl := aCntrl as TWinControl;
      if (WinControl is TCustomListView) then
        _RestoreListView (WinControl as TListView);
      if WinControl is TSplitter then
        THackSplitter(WinControl).Restore (self);

      if (WinControl is TCustomVirtualStringTree) then
        _RestoreTreeView (WinControl as TVirtualStringTree);

      for i := 0 to aCntrl.ControlCount  - 1 do
      begin
        if aCntrl.Controls [i] is TWinControl then
          _Restore (THackControl(aCntrl.Controls [i]));
      end;
    end;
  end;
var
  x: Integer;
begin
  for x := 0 to fForm.ControlCount - 1 do
    _Restore (THackControl (fForm.Controls[x]));
end;

procedure THackSplitter.Restore(aIniFile: TFormIniFile);
var
  xControl: TControl;
begin
  xControl := FindAlignControl;
  if Assigned (xControl) then
  begin
    if Align in [alTop, alBottom] then
    begin
      try
        xControl.Height :=
          Max ( 1   // in case zero, it is not possible to succesfully enlarge it with the sibling splitter
              , aIniFile.IntegerByNameDef[xControl.Name + 'Height',xControl.Height]
              );
      except
      end;
    end;
    if Align in [alLeft] then
    begin
      try
        xControl.Width := aIniFile.IntegerByNameDef[xControl.Name + 'Width',xControl.Width];
      except
      end;
      xControl.Width :=
        min ( xControl.Width, xControl.Parent.Width - 20); //just in case current screen is samller then last one used
    end;
    if Align in [alRight] then
    begin
      try
        xControl.Width := aIniFile.IntegerByNameDef[xControl.Name + 'Width', xControl.Width];
      except
      end;
    end;
  end; // Assigned (xControl)
end;

procedure THackSplitter.Save(aIniFile: TFormIniFile);
var
  xControl: TControl;
begin
  xControl := FindAlignControl;
  if Assigned (xControl) then
  begin
    if Align in [alTop, alBottom] then
    begin
      aIniFile.IntegerByName[xControl.Name + 'Height'] := xControl.Height;
    end;
    if Align in [alLeft, alRight] then
    begin
      aIniFile.IntegerByName[xControl.Name + 'Width'] := xControl.Width;
    end;
  end; // if Assigned (xControl)
end;

function TFormIniFile.getStringByName(Index: String): String;
begin
  result := '';
  fOpenKeys;
  try
    OpenKey('Strings', True);
    try
      try result := ReadString(Index); except end;
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

procedure TFormIniFile.setStringByName(Index: String; const Value: String);
begin
  fOpenKeys;
  try
    OpenKey('Strings', True);
    try
      WriteString(Index, Value);
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

function TFormIniFile.getBooleanByName(Index: String; DefValue: Boolean): Boolean;
begin
  fOpenKeys;
  try
    result := DefValue;
    OpenKey('Booleans', True);
    try
      try result := ReadBool(Index); except end;
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

function TFormIniFile.getBooleanByName(Index: String): Boolean;
begin
  result := getBooleanByName(Index, False);
end;

procedure TFormIniFile.setBooleanByName(Index: String; const Value: Boolean);
begin
  fOpenKeys;
  try
    OpenKey('Booleans', True);
    try
      WriteBool(Index, Value);
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

function TFormIniFile.getStringByNameDef(Index, Default: String): String;
begin
  result := Default;
  fOpenKeys;
  try
    OpenKey('Strings', True);
    try
      try result := ReadString(Index); except end;
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
  if result = '' then
    result := Default;
end;

procedure TFormIniFile.Initialize(initScreenPos: Boolean);
var
  x: Integer;
begin
  if Assigned (fForm) then
  begin
    if initScreenPos then
    begin
      fForm.Position := poDesigned;
      try
        x := Min(IntegerByName['FormTop'], Screen.Height - 100);
      except
        x := fForm.Top;
      end;
      fForm.Top := x;
      try
        x := Min(IntegerByName['FormLeft'], Screen.Width - 100);
      except
        x := fForm.Left;
      end;
      fForm.Left := x;
      fForm.Height := IntegerByNameDef['FormHeight', fForm.Height];
      fForm.Width := IntegerByNameDef['FormWidth', fForm.Width];
      fForm.MakeFullyVisible();
      try
        if BooleanByName['FormMaximized'] then
          fForm.WindowState := wsMaximized;
      except
      end;
    end;
  end;
end;

procedure TFormIniFile.fOpenKeys;
begin
  OpenKey (fKey, True);
  OpenKey (fName, True);
end;

procedure TFormIniFile.fCloseKeys;
begin
  CloseKey; // fName
  CloseKey; // fKey
end;

function TFormIniFile.getBooleanByNameDef(Index: String;
  Default: Boolean): Boolean;
begin
  result := Default;
  fOpenKeys;
  try
    OpenKey('Booleans', True);
    try
      try result := ReadBool(Index); except end;
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

function TFormIniFile.getIntegerByName(Index: String): Integer;
begin
  result := getIntegerByNameDef(Index, 0);
end;

function TFormIniFile.getIntegerByNameDef(Index: String; Default: Integer): Integer;
begin
  result := Default;
  fOpenKeys;
  try
    OpenKey('Integers', True);
    try
      try result := ReadInteger(Index); except end;
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

procedure TFormIniFile.setIntegerByName(Index: String; const Value: Integer);
begin
  fOpenKeys;
  try
    OpenKey ('Integers', True);
    try
      WriteInteger(Index, Value);
    finally
      CloseKey;
    end;
  finally
    fCloseKeys;
  end;
end;

end.
