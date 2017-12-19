unit fedUnit;

interface

uses Classes
   , Bind
   , SysUtils
   ;

type TFed = class (TObject)
private
  fCursor: Integer;
  function getEof: Boolean;
    function getData: TCustomBindable;
public
  Name, Alias: String;
  FirstBind: TCustomBindable;
  List: TStringList;
  Parent: TFed;
  isSubElement: Boolean;
  isCursor: Boolean;
  property Eof: Boolean read getEof;
  property Data: TCustomBindable read getData;
  function BindableByName (aName: String): TCustomBindable;
  function Ancestor (aAlias: String): TFed;
  function PrepareBindableOnAliasField (aName: String): TCustomBindable;
  function FindBindableOnAliasField (aName: String): TCustomBindable;
  procedure New;
  procedure Open;
  procedure Close;
  procedure First;
  procedure Next;
  constructor Create;
  destructor Destroy; override;
end;


implementation

uses Xmlz
   , Xsdz
   ;
{ TFed }

function TFed.Ancestor(aAlias: String): TFed;
  function _Ancestor(aFed: TFed): TFed;
  begin
    result := aFed;
    if Assigned (aFed)
    and (aFed.Alias <> aAlias) then
      result := _Ancestor (aFed.Parent);
  end;
begin
  result := _Ancestor (Self);
end;

function TFed.PrepareBindableOnAliasField(aName: String): TCustomBindable;
var
  p: Integer;
  xFed: TFed;
begin
  result := nil;
  xFed := Self;
  p := Pos('.', aName);
  if p > 1 then
    xFed := Ancestor(Copy(aName, 1, p - 1));
  if Assigned (xFed) then
    with xFed.FirstBind do
      result := FindUQ(Name + Copy(aName, p, 30000));
end;

function TFed.FindBindableOnAliasField (aName: String): TCustomBindable;
var
  p: Integer;
  xFed: TFed;
begin
  result := nil;
  xFed := Self;
  p := Pos('.', aName);
  if p > 1 then
    xFed := Ancestor(Copy(aName, 1, p - 1));
  if Assigned (xFed) then
    with xFed.Data do
      result := FindUQ(Name + Copy(aName, p, 30000));
end;

function TFed.BindableByName(aName: String): TCustomBindable;
var
  x: Integer;
begin
  with (Data as TXml) do
    result := FindUQ(Name + aName);
  if not Assigned (result) then
    raise Exception.CreateFmt('Error: %s not found in %s', [aName, Data.FullIndexCaption]);
end;

procedure TFed.Close;
begin
  List.Clear;
end;

constructor TFed.Create;
begin
  inherited;
  List := TStringList.Create;
end;

destructor TFed.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TFed.First;
begin
  fCursor := 0;
end;

function TFed.getData: TCustomBindable;
begin
  if isCursor then
  begin
    if Eof then raise Exception.CreateFmt('TFed.GetData at EOF (%s)', [Name]);
    result := List.Objects[fCursor] as TCustomBindable;
  end
  else
    result := FirstBind;
end;

function TFed.getEof: Boolean;
begin
  result := not (fCursor < List.Count);
end;

procedure TFed.New;
var
  x, y: Integer;
  xXml, cXml: TXml;
begin
  xXml := nil;
  y := -1;
  cXml := FirstBind.Parent as TXml;
  begin
    for x := 0 to cXml.Items.Count - 1 do
    begin
      if cXml.Items.XmlItems[x].Name = FirstBind.Name then
      begin
        y := x;
        if not cXml.Items.XmlItems[x].Checked then
        begin
          cXml.Items.XmlItems[x].Checked := True;
          FirstBind := cXml.Items.XmlItems[x];
          Exit;
        end;
      end;
    end;
    xXml := TXml.Create((FirstBind as TXml).Xsd);
    cXml.InsertXml(y+1, xXml);
    xXml.Checked := True;
    FirstBind := xXml;
  end;
end;

procedure TFed.Next;
begin
  Inc (fCursor);
end;

procedure TFed.Open;
  function _split (s: String): TStringList;
  var
    x: Integer;
    ss: String;
  begin
    result := TStringList.Create;
    x := 1;
    ss := '';
    while x <= Length (s) do
    begin
      if s[x] = '.' then
      begin
        result.Add(ss);
        ss := '';
      end
      else
        ss := ss + s[x];
      Inc (x);
    end;
    result.Add(ss)
  end;
  procedure _CreateList (rXml: TXml; aList, Sl: TStringList; i: Integer);
  var
    x: Integer;
  begin
    if rXml.Checked
    and (rXml.Name = Sl.Strings[i]) then
    begin
      if i < (Sl.Count - 1) then
      begin
        for x := 0 to rXml.Items.Count - 1 do
          _CreateList (rXml.Items.XmlItems[x], aList, Sl, i + 1);
      end
      else
        aList.AddObject('', rXml);
    end;
  end;
var
  x: Integer;
  pXml: TXml;
  s: String;
  sl: TStringList;
begin
  List.Clear;
  if isSubElement then
  begin
    if not Assigned (Parent) then raise Exception.CreateFmt('TFed.Open: No parent for %s', [Name]);
    pXml := Parent.Data as TXml;
    s := pXml.Name + Name;
  end
  else
  begin
    if not Assigned (FirstBind) then raise Exception.Create('TFed.Open: No FirstBind assigned');
    if not (FirstBind is TXml) then raise Exception.Create('TFed.Open: Only for XML');
    if not Assigned (FirstBind.Parent) then raise Exception.Create('TFed.Open: Not for root XMLs');
    s := (FirstBind as TXml).FullCaption;
    pXml := (FirstBind as TXml).Root;
  end;
  sl := _Split (s);
  try
    _CreateList (pXml, List, sl, 0);
  finally
    sl.Free;
  end;
end;

end.
