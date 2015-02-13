unit FormIniFilez;

{$mode objfpc}{$H+}

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
    EncryptionSeed: String;
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
  function SimpleEncrypt(const Source: AnsiString): AnsiString;
  function EncryptPassword (aPassword: AnsiString): AnsiString;
  function DecryptPassword(aPassword: AnsiString): AnsiString;
  procedure Restore;
  procedure Save;
  property BooleanByName [Index: String]: Boolean read getBooleanByName write setBooleanByName;
  property BooleanByNameDef [Index: String; Default: Boolean]: Boolean read getBooleanByNameDef;
  property StringByName [Index: String]: String read getStringByName write setStringByName;
  property StringByNameDef [Index, Default: String]: String read getStringByNameDef;
  property IntegerByName [Index: String]: Integer read getIntegerByName write setIntegerByName;
  property IntegerByNameDef [Index: String; Default: Integer]: Integer read getIntegerByNameDef;
  constructor Create (aForm: TForm); Overload;
  constructor Create (aForm: TForm; aName: String; initScreenPos: Boolean); Overload;
  destructor Destroy; override;
end;

type
  THackSplitter = class (TSplitter)
private
  function FindControl: TControl;
public
  procedure Save (aIniFile: TFormIniFile);
  procedure Restore (aIniFile: TFormIniFile);
end;

implementation

uses Math
   , VirtualTrees
   ;

{ TFormIniFile }


constructor TFormIniFile.Create(aForm: TForm);
begin
  inherited Create;
  fForm := aForm;
  fKey := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '.ini');
  if Assigned (aForm) then fName := aForm.Name;
  Initialize (True);
end;

constructor TFormIniFile.Create(aForm: TForm; aName: String; initScreenPos: Boolean);
begin
  inherited Create;
  fName := aName;
  fForm := aForm;
  fKey := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(ParamStr(0)), '.ini');
  Initialize(initScreenPos);
end;

destructor TFormIniFile.Destroy;
begin
  if Assigned (fForm) then
  begin
    fOpenKeys;
    try
      WriteInteger ('Top', fForm.Top);
      WriteInteger ('Left', fForm.Left);
      WriteInteger ('Height', fForm.Height);
      WriteInteger ('Width', fForm.Width);
      WriteBool ('wsMaximized', (fForm.WindowState = wsMaximized));
    finally
      fCloseKeys;
    end;
  end;
  inherited;
end;

procedure TFormIniFile.Save;

  procedure _SaveListView (aControl: TListView);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Columns.Count - 1 do
      WriteInteger ( aControl.Name + ':Width0' + IntToStr (x)
                   , aControl.Columns [x].Width
                   );
  end;

  procedure _SaveTreeView (aControl: TVirtualStringTree);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Header.Columns.Count - 1 do
      WriteInteger ( aControl.Name + ':Width0' + IntToStr (x)
                   , aControl.Header.Columns [x].Width
                   );
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

      if (WinControl is TCustomVirtualStringTree) then
        _SaveTreeView (WinControl as TVirtualStringTree);

      for i := 0 to aCntrl.ControlCount  - 1 do
      begin
        if aCntrl.Controls [i] is TWinControl then
          _Save (THackControl(aCntrl.Controls [i]))
        else
          if aCntrl.Controls [i] is TSplitter then
            THackSplitter(aCntrl.Controls [i]).Save (self);
      end;
    end;
  end;
var
  x: Integer;
begin
  fOpenKeys;
  try
    for x := 0 to fForm.ControlCount - 1 do
      _Save (THackControl (fForm.Controls[x]));
  finally
    fCloseKeys;
  end;
end;


procedure TFormIniFile.Restore;

  procedure _RestoreListView (aControl: TListView);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Columns.Count - 1 do
      try aControl.Columns [x].Width := ReadInteger (aControl.Name + ':Width0' + IntToStr (x)); except end;
  end;

  procedure _RestoreTreeView (aControl: TVirtualStringTree);
  var
    x: Integer;
  begin
    for x := 0 to aControl.Header.Columns.Count - 1 do
      try aControl.Header.Columns [x].Width := ReadInteger (aControl.Name + ':Width0' + IntToStr (x)); except end;
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

      if (WinControl is TCustomVirtualStringTree) then
        _RestoreTreeView (WinControl as TVirtualStringTree);

      for i := 0 to aCntrl.ControlCount  - 1 do
      begin
        if aCntrl.Controls [i] is TWinControl then
          _Restore (THackControl(aCntrl.Controls [i]))
        else
          if aCntrl.Controls [i] is TSplitter then
            THackSplitter(aCntrl.Controls [i]).Restore (self);
      end;
    end;
  end;
var
  x: Integer;
begin
  fOpenKeys;
  try
    for x := 0 to fForm.ControlCount - 1 do
      _Restore (THackControl (fForm.Controls[x]));
  finally
    fCloseKeys;
  end;
end;

function THackSplitter.FindControl: TControl;
// Copied from TSplitter; should be a better way to reach private fies
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure THackSplitter.Restore(aIniFile: TFormIniFile);
var
  xControl: TControl;
begin
  xControl := FindControl;
  if Assigned (xControl) then
  begin
    aIniFile.OpenKey(aIniFile.fName + ':' + xControl.Name, True);
    try
      if Align in [alTop, alBottom] then
      begin
        try
          xControl.Height :=
            Max ( 1   // in case zero, it is not possible to succesfully enlarge it with the sibling splitter
                , aIniFile.ReadInteger ('Height')
                );
        except
        end;
      end;
      if Align in [alLeft] then
      begin
        try
          xControl.Width := aIniFile.ReadInteger ('Width');
        except
        end;
        xControl.Width :=
          min ( xControl.Width, xControl.Parent.Width - 20); //just in case current screen is samller then last one used
      end;
      if Align in [alRight] then
      begin
        try
          xControl.Width := aIniFile.ReadInteger ('Width');
        except
        end;
      end;
    finally
      aIniFile.CloseKey;
    end;
  end; // Assigned (xControl)
end;

procedure THackSplitter.Save(aIniFile: TFormIniFile);
var
  xControl: TControl;
begin
  xControl := FindControl;
  if Assigned (xControl) then
  begin
    aIniFile.OpenKey(aIniFile.fName + ':' + xControl.Name, True);
    try
      if Align in [alTop, alBottom] then
      begin
        aIniFile.WriteInteger ( 'Height'
                              , xControl.Height
                              );
      end;
      if Align in [alLeft, alRight] then
      begin
        aIniFile.WriteInteger ( 'Width'
                              , xControl.Width
                              );
      end;
    finally
      aIniFile.CloseKey;
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

function TFormIniFile.SimpleEncrypt(const Source: AnsiString): AnsiString;
var
  Index: Integer;
begin
  EncryptionSeed := 'th^ruh54bdkjbkjb4k458&*';
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := AnsiChar((Ord(EncryptionSeed[Index mod Length(EncryptionSeed)]) xor Ord(Source[Index])));
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
      CloseKey;;
    end;
  finally
    fCloseKeys;
  end;
end;

procedure TFormIniFile.Initialize(initScreenPos: Boolean);
var
  x: Integer;
begin
  if Assigned (fForm) then
  begin
    fOpenKeys;
    try
      if initScreenPos then
      begin
        fForm.Position := poDesigned;
        try
          x := Min(ReadInteger ('Top'), Screen.Height - 100);
        except
          x := fForm.Top;
        end;
        fForm.Top := x;
        try
          x := Min(ReadInteger ('Left'), Screen.Width - 100);
        except
          x := fForm.Left;
        end;
        fForm.Left := x;
      end;
      try fForm.Height := ReadInteger ('Height'); except end;
      try fForm.Width := ReadInteger ('Width'); except end;
      try
        if ReadBool ('wsMaximized') = True then
          fForm.WindowState := wsMaximized;
      except
      end;
    finally
      fCloseKeys;
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

function TFormIniFile.DecryptPassword(aPassword: AnsiString): AnsiString;
begin
  if aPassword <> '' then
    result :=  SimpleEncrypt(DecodeStringBase64(aPassword))
  else
    result := '';
end;

function TFormIniFile.EncryptPassword(aPassword: AnsiString): AnsiString;
begin
  if aPassword = '' then
    result := EncodeStringBase64 (SimpleEncrypt(aPassword))
  else
    result := '';
end;

end.
