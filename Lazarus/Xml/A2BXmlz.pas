unit A2BXmlz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes
   , SysUtils
   , Xmlz
   , Bind
   , a2bStringListUnit
   ;

type

{ TA2BXml }

 TA2BXml = class (TXml)
  private
    fDiffers: Boolean;
    fThisOneDiffers: Boolean;
    fIgnoredDifference: Boolean;
    function getNumberOfDiffs: Integer;
    procedure setDiffers(const Value: Boolean);
    procedure setIgnoredDifference(const Value: Boolean);
  public
    bValue: String;
    ChangeKind: TChangeKind;
    property Differs: Boolean read fDiffers write setDiffers;
    property IgnoredDifference: Boolean read fIgnoredDifference write setIgnoredDifference;
    property NumberOfDiffs: Integer read getNumberOfDiffs;
    property ThisOneDiffers: Boolean read fThisOneDiffers;
    function Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TStringList): Boolean;
    constructor CreateA (aXml: TXml); overload;
    constructor CreateB (bXml: TXml); overload;
    constructor CreateA2B (aXml, bXml: TXml; ignoreOrder: Boolean); overload;
    constructor CreateA (aXml: TXmlAttribute); overload;
    constructor CreateB (bXml: TXmlAttribute); overload;
    constructor CreateA2B (aXml, bXml: TXmlAttribute); overload;
  end;

procedure a2bInitialize;
procedure a2bUninitialize;

implementation

uses xmlUtilz, wrdFunctionz, StrUtils, igGlobals, base64;

procedure a2bInitialize;
begin
  wrdInitialize;
end;

procedure a2bUninitialize;
begin
  wrdUninitialize;
end;

function valuesDiffer (aValue, bValue: String): Boolean;
var
  aFileName, bFileName: String;
begin
  result := (aValue <> bValue);
  if result then
  begin
    if wrdInstalled
    and (   AnsiStartsStr(base64DocxStartStr, aValue)
         or AnsiStartsStr(base64RtfStartStr, aValue)
        )
    and (   AnsiStartsStr(base64DocxStartStr, bValue)
         or AnsiStartsStr(base64RtfStartStr, bValue)
        )
    then
    begin
      if AnsiStartsStr(base64DocxStartStr, aValue) then
        aFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileA.docx'
      else
        aFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileA.rtf';
      if AnsiStartsStr(base64DocxStartStr, bValue) then
        bFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileB.docx'
      else
        bFileName := GetEnvironmentVariable ('Temp') + '\A2BCompareFileB.rtf';
      SaveStringToFile ( aFileName , DecodeStringBase64 (aValue));
      SaveStringToFile ( bFileName , DecodeStringBase64 (bValue));
      result := (wrdFileDiffencesCount(bFileName, aFileName) > 0);
    end;
  end;
end;


function rmPrefix (aName: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aName) do
  begin
    if aName [x] = ':' then
      result := ''
    else
      result := result + aName [x];
  end;
end;

{ TA2BXml }

constructor TA2BXml.CreateA(aXml: TXml);
var
  x: Integer;
begin
  inherited Create;
  ChangeKind := ckDelete;
  TagName := aXml.TagName;
  Value := aXml.Value;
  fThisOneDiffers := True;
  for x := 0 to aXml.Attributes.Count - 1 do
    AddXml (TA2BXml.CreateA(aXml.Attributes.XmlAttributes[x]));
  for x := 0 to aXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateA(aXml.Items.XmlItems[x]));
end;

constructor TA2BXml.CreateB(bXml: TXml);
var
  x: Integer;
begin
  inherited Create;
  ChangeKind := ckAdd;
  TagName := bXml.TagName;
  bValue := bXml.Value;
  fThisOneDiffers := True;
  for x := 0 to bXml.Attributes.Count - 1 do
    AddXml (TA2BXml.CreateB(bXml.Attributes.XmlAttributes[x]));
  for x := 0 to bXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateB(bXml.Items.XmlItems[x]));
end;

constructor TA2BXml.CreateA2B(aXml, bXml: TXml; ignoreOrder: Boolean);
var
  x, a, b, c, f, i: Integer;
  Diffs: TA2BStringList;
  childXml: TA2BXml;
begin
  inherited Create;
  TagName := aXml.TagName;
  Value := aXml.Value;
  bValue := bXml.Value;
  ChangeKind := ckCopy;
  if valuesDiffer(Value, bValue) then
  begin
    ChangeKind := ckModify;
    fThisOneDiffers := True;
  end;
  Diffs := TA2BStringList.Create;

  for x := aXml.Attributes.Count - 1 downto 0 do
  begin
    if AnsiStartsStr('xmlns:', aXml.Attributes.XmlAttributes [x].Name)
    or (aXml.Attributes.XmlAttributes [x].Name = 'xmlns') then
    begin
      aXml.Attributes.XmlAttributes[x].Free;
      aXml.Attributes.Delete(x);
    end
    else
      aXml.Attributes.Strings [x] := rmPrefix (aXml.Attributes.XmlAttributes [x].Name);
  end;
  for x := bXml.Attributes.Count - 1 downto 0 do
  begin
    if AnsiStartsStr('xmlns:', bXml.Attributes.XmlAttributes [x].Name)
    or (bXml.Attributes.XmlAttributes [x].Name = 'xmlns') then
    begin
      bXml.Attributes.XmlAttributes[x].Free;
      bXml.Attributes.Delete(x);
    end
    else
      bXml.Attributes.Strings [x] := rmPrefix (bXml.Attributes.XmlAttributes [x].Name);
  end;
  aXml.Attributes.Sort;
  bXml.Attributes.Sort;
  with Diffs do
  begin
    Execute(aXml.Attributes, bXml.Attributes);
    a := 0; b := 0;
    for c := 0 to ChangeCount - 1 do
    begin
      while a < Changes[c].x do
      begin
        AddXml(TA2BXml.CreateA2B( aXml.Attributes.XmlAttributes[a]
                                , bXml.Attributes.XmlAttributes[b]
                                )
              );
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        for i := b to b + Changes[c].Range - 1 do
        begin
          AddXml (TA2BXml.CreateB(bXml.Attributes.XmlAttributes[b]));
          inc(b);
        end;
      end
      else
      begin
        if Changes[c].Kind = ckDelete then
        begin
          for i := a to a + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateA(aXml.Attributes.XmlAttributes[a]));
            inc(a);
          end;
        end
        else
        begin
          for i := a to a + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateA(aXml.Attributes.XmlAttributes[a]));
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateB(bXml.Attributes.XmlAttributes[b]));
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Attributes.Count) and (b < bXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateA2B ( aXml.Attributes.XmlAttributes[a]
                               , bXml.Attributes.XmlAttributes[b]
                               )
            );
      inc(a); inc(b);
    end;
    while (a < aXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateA(aXml.Attributes.XmlAttributes[a]));
      inc(a);
    end;
    while (b < bXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateB(bXml.Attributes.XmlAttributes[b]));
      inc(b);
    end;
  end;

  if ignoreOrder then
  begin
    for x := 0 to aXml.Items.Count - 1 do
      aXml.Items.Strings [x] := rmPrefix (aXml.Items.XmlItems [x].TagName) + ';' + aXml.Items.XmlItems[x].Value;
    aXml.Items.Sort;
    for x := 0 to bXml.Items.Count - 1 do
      bXml.Items.Strings [x] := rmPrefix (bXml.Items.XmlItems [x].TagName) + ';' + bXml.Items.XmlItems[x].Value;
    bXml.Items.Sort;
  end;
  for x := 0 to aXml.Items.Count - 1 do
    aXml.Items.Strings [x] := rmPrefix (aXml.Items.XmlItems [x].TagName);
  for x := 0 to bXml.Items.Count - 1 do
    bXml.Items.Strings [x] := rmPrefix (bXml.Items.XmlItems [x].TagName);
  with Diffs do
  begin
    Execute(aXml.Items, bXml.Items);
    a := 0; b := 0;
    for c := 0 to ChangeCount - 1 do
    begin
      while a < Changes[c].x do
      begin
        childXml := AddXml (TA2BXml.CreateA2B(aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrder)) as TA2BXml;
        Differs := Differs or childXml.Differs;
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        Differs := True;
        for i := b to b + Changes[c].Range - 1 do
        begin
          childXml := AddXml (TA2BXml.CreateB(bXml.Items.XmlItems[b])) as TA2BXml;
          Differs := Differs or childXml.Differs;
          inc(b);
        end;
      end
      else
      begin
        if Changes[c].Kind = ckDelete then
        begin
          Differs := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateA(aXml.Items.XmlItems[a])) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(a);
          end;
        end
        else
        begin
          Differs := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateA(aXml.Items.XmlItems[a])) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateB(bXml.Items.XmlItems[b])) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Items.Count) and (b < bXml.Items.Count) do
    begin
      childXml := AddXml (TA2BXml.CreateA2B(aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrder)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a); inc(b);
    end;
    while (a < aXml.Items.Count) do
    begin
      Differs := True;
      childXml := AddXml (TA2BXml.CreateA(aXml.Items.XmlItems[a])) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a);
    end;
    while (b < bXml.Items.Count) do
    begin
      Differs := True;
      childXml := AddXml (TA2BXml.CreateA(bXml.Items.XmlItems[b])) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(b);
    end;
  end;
  FreeAndNil (Diffs);
end;

constructor TA2BXml .CreateA (aXml : TXmlAttribute );
begin
  inherited Create;
  ChangeKind := ckDelete;
  TagName := '@' + aXml.Name;
  Value := aXml.Value;
  fThisOneDiffers := True;
end;

constructor TA2BXml .CreateB (bXml : TXmlAttribute );
begin
  inherited Create;
  ChangeKind := ckAdd;
  TagName := '@' + bXml.Name;
  bValue := bXml.Value;
  fThisOneDiffers := True;
end;

constructor TA2BXml .CreateA2B (aXml , bXml : TXmlAttribute);
begin
  inherited Create;
  TagName := '@' + aXml.Name;
  Value := aXml.Value;
  bValue := bXml.Value;
  ChangeKind := ckCopy;
  if valuesDiffer(Value, bValue) then
  begin
    ChangeKind := ckModify;
    fThisOneDiffers := True;
  end;
end;

procedure TA2BXml.setDiffers(const Value: Boolean);
begin
  fDiffers := Value;
  if Value then
    if Assigned (Parent) then
      (Parent as TA2BXml).Differs := True;
end;

procedure TA2BXml.setIgnoredDifference(const Value: Boolean);
begin
  fIgnoredDifference := Value;
{
  if Value then
    if Assigned (Parent) then
      (Parent as TA2BXml).IgnoredDifference := True;
}
end;

function TA2BXml.getNumberOfDiffs: Integer;
var
  x: Integer;
begin
  result := 0;
  if fThisOneDiffers then
    result := 1
  else
    for x := 0 to Items.Count - 1 do
      result := result + (Items.XmlItems[x] as TA2BXml).NumberOfDiffs;
end;

function TA2BXml.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TStringList): Boolean;
  procedure _downIgnored(aXml: TA2BXml);
  var
    x: Integer;
  begin
    aXml.fIgnoredDifference := True;
    for x := 0 to aXml.Items.Count - 1 do
      _downIgnored(aXml.Items.XmlItems[x] as TA2BXml);
  end;
  procedure _reset(aXml: TA2BXml);
  var
    x: Integer;
  begin
    aXml.fDiffers := False;
    for x := 0 to aXml.Items.Count - 1 do
      _reset (aXml.Items.XmlItems[x] as TA2BXml);
  end;
  procedure _set (aXml: TA2BXml);
  var
    x, f: Integer;
  begin
    if aXml.fIgnoredDifference then Exit;  // because of the _downIgnored...
    if aXml.ChangeKind = ckModify then
    begin
      aXml.Differs := (not Assigned (ignoreDifferencesOn))
                  or (    (not ignoreDifferencesOn.Find(rmPrefix(aXml.TagName) , f))
                      and (not ignoreDifferencesOn.Find(aXml.FullUQCaption , f))
                     );
      aXml.IgnoredDifference := not aXml.Differs;
      if not aXml.IgnoredDifference then
        Ignore := False;
    end;
    if aXml.ChangeKind = ckDelete then
    begin
      aXml.Differs := (not Assigned (ignoreAddingOn))
                  or (    (not ignoreAddingOn.Find(rmPrefix(aXml.TagName) , f))
                      and (not ignoreAddingOn.Find(aXml.FullUQCaption , f))
                     );
      aXml.IgnoredDifference := not aXml.Differs;
      if aXml.IgnoredDifference then
        _downIgnored(aXml)
      else
        Ignore := False;
    end;
    if aXml.ChangeKind = ckAdd then
    begin
      aXml.Differs := (not Assigned (ignoreRemovingOn))
                   or (    (not ignoreRemovingOn.Find(rmPrefix(aXml.TagName) , f))
                       and (not ignoreRemovingOn.Find(aXml.FullUQCaption , f))
                      );
      aXml.IgnoredDifference := not aXml.Differs;
      if aXml.IgnoredDifference then
        _downIgnored(aXml)
      else
        Ignore := False;
    end;
    for x := 0 to aXml.Items.Count - 1 do
      _set (aXml.Items.XmlItems[x] as TA2BXml);
  end;
begin
//_reset(Self);
  Ignore := Differs;
  _set(Self);
end;

end.
