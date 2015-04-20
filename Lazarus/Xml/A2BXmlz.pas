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
    fIgnored: Boolean;
    function getNumberOfDiffs: Integer;
    procedure setDiffers(const Value: Boolean);
    procedure setIgnored(const Value: Boolean);
    procedure setThisOneDiffers (AValue : Boolean );
  public
    bValue: String;
    Prefix: String;
    ChangeKind: TChangeKind;
    property ThisOneDiffers: Boolean read fThisOneDiffers write setThisOneDiffers;
    property Differs: Boolean read fDiffers write setDiffers;
    property Ignored: Boolean read fIgnored write setIgnored;
    property NumberOfDiffs: Integer read getNumberOfDiffs;
    procedure Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TStringList);
    constructor CreateA (aPrefix: String; aXml: TXml; aThisOneDiffers: Boolean); overload;
    constructor CreateB (aPrefix: String; bXml: TXml; aThisOneDiffers: Boolean); overload;
    constructor CreateA2B (aPrefix: String; aXml, bXml: TXml; ignoreOrder: Boolean); overload;
    constructor CreateA (aPrefix: String; aXml: TXmlAttribute; aThisOneDiffers: Boolean); overload;
    constructor CreateB (aPrefix: String; bXml: TXmlAttribute; aThisOneDiffers: Boolean); overload;
    constructor CreateA2B (aPrefix: String; aXml, bXml: TXmlAttribute); overload;
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

constructor TA2BXml.CreateA(aPrefix: String; aXml: TXml; aThisOneDiffers: Boolean);
var
  x: Integer;
begin
  inherited Create;
  ChangeKind := ckDelete;
  TagName := aXml.TagName;
  Value := aXml.Value;
  Prefix:=aPrefix;
  ThisOneDiffers := aThisOneDiffers;
  for x := 0 to aXml.Attributes.Count - 1 do
    AddXml (TA2BXml.CreateA(aPrefix,aXml.Attributes.XmlAttributes[x], False));
  for x := 0 to aXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[x], False));
end;

constructor TA2BXml.CreateB(aPrefix: String; bXml: TXml; aThisOneDiffers: Boolean);
var
  x: Integer;
begin
  inherited Create;
  ChangeKind := ckAdd;
  TagName := bXml.TagName;
  bValue := bXml.Value;
  Prefix:=aPrefix;
  ThisOneDiffers := aThisOneDiffers;
  for x := 0 to bXml.Attributes.Count - 1 do
    AddXml (TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[x], False));
  for x := 0 to bXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateB(aPrefix, bXml.Items.XmlItems[x], False));
end;

constructor TA2BXml.CreateA2B(aPrefix: String; aXml, bXml: TXml; ignoreOrder: Boolean);
var
  x, a, b, c, f, i: Integer;
  Diffs: TA2BStringList;
  childXml: TA2BXml;
begin
  inherited Create;
  TagName := aXml.TagName;
  Value := aXml.Value;
  bValue := bXml.Value;
  Prefix:=aPrefix;
  ChangeKind := ckCopy;
  if valuesDiffer(Value, bValue) then
  begin
    ChangeKind := ckModify;
    ThisOneDiffers := True;
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
        AddXml(TA2BXml.CreateA2B( aPrefix
                                , aXml.Attributes.XmlAttributes[a]
                                , bXml.Attributes.XmlAttributes[b]
                                )
              );
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        for i := b to b + Changes[c].Range - 1 do
        begin
          AddXml (TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
          inc(b);
        end;
      end
      else
      begin
        if Changes[c].Kind = ckDelete then
        begin
          for i := a to a + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateA(aPrefix, aXml.Attributes.XmlAttributes[a], True));
            inc(a);
          end;
        end
        else
        begin
          for i := a to a + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateA(aPrefix, aXml.Attributes.XmlAttributes[a], True));
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Attributes.Count) and (b < bXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateA2B ( aPrefix
                               , aXml.Attributes.XmlAttributes[a]
                               , bXml.Attributes.XmlAttributes[b]
                               )
            );
      inc(a); inc(b);
    end;
    while (a < aXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateA(aPrefix, aXml.Attributes.XmlAttributes[a], True));
      inc(a);
    end;
    while (b < bXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
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
        childXml := AddXml (TA2BXml.CreateA2B(aPrefix, aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrder)) as TA2BXml;
        Differs := Differs or childXml.Differs;
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        ThisOneDiffers := True;
        for i := b to b + Changes[c].Range - 1 do
        begin
          childXml := AddXml (TA2BXml.CreateB(aPrefix, bXml.Items.XmlItems[b], True)) as TA2BXml;
          Differs := Differs or childXml.Differs;
          inc(b);
        end;
      end
      else
      begin
        if Changes[c].Kind = ckDelete then
        begin
          ThisOneDiffers := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[a], True)) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(a);
          end;
        end
        else
        begin
          ThisOneDiffers := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[a], True)) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateB(aPrefix, bXml.Items.XmlItems[b], True)) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Items.Count) and (b < bXml.Items.Count) do
    begin
      childXml := AddXml (TA2BXml.CreateA2B(aPrefix, aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrder)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a); inc(b);
    end;
    while (a < aXml.Items.Count) do
    begin
      ThisOneDiffers := True;
      childXml := AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[a], True)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a);
    end;
    while (b < bXml.Items.Count) do
    begin
      ThisOneDiffers := True;
      childXml := AddXml (TA2BXml.CreateA(aPrefix, bXml.Items.XmlItems[b], True)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(b);
    end;
  end;
  FreeAndNil (Diffs);
end;

constructor TA2BXml .CreateA (aPrefix: String; aXml : TXmlAttribute; aThisOneDiffers: Boolean);
begin
  inherited Create;
  ChangeKind := ckDelete;
  TagName := '@' + aXml.Name;
  Value := aXml.Value;
  Prefix:= aPrefix;
  ThisOneDiffers := aThisOneDiffers;
end;

constructor TA2BXml .CreateB (aPrefix: String; bXml : TXmlAttribute; aThisOneDiffers: Boolean); overload;
begin
  inherited Create;
  ChangeKind := ckAdd;
  TagName := '@' + bXml.Name;
  bValue := bXml.Value;
  Prefix:=aPrefix;
  ThisOneDiffers := aThisOneDiffers;
end;

constructor TA2BXml .CreateA2B (aPrefix: String; aXml , bXml : TXmlAttribute);
begin
  inherited Create;
  TagName := '@' + aXml.Name;
  Value := aXml.Value;
  bValue := bXml.Value;
  Prefix:=aPrefix;
  ChangeKind := ckCopy;
  if valuesDiffer(Value, bValue) then
  begin
    ChangeKind := ckModify;
    fThisOneDiffers := True;
  end;
end;

procedure TA2BXml.setDiffers(const Value: Boolean);
begin
  if Value = fDiffers then Exit;
  fDiffers := Value;
  if Value then
    if Assigned (Parent) then
      (Parent as TA2BXml).Differs := True;
end;

procedure TA2BXml.setIgnored(const Value: Boolean);
var
  x: Integer;
begin
  if Value = fIgnored then Exit;
  fIgnored := Value;
  if Value then
  begin
    for x := 0 to Items.Count - 1 do
      (Items.XmlItems[x] as TA2BXml).Ignored := Value;
  end
  else
  begin
    if Assigned (Parent) then
      (Parent as TA2BXml).Ignored := Value;
  end;
end;

procedure TA2BXml .setThisOneDiffers (AValue : Boolean );
begin
  if fThisOneDiffers = AValue then Exit ;
  fThisOneDiffers := AValue ;
  if AValue then
    Differs := True;
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

procedure TA2BXml.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TStringList);
  procedure _set (aXml: TA2BXml);
  var
    x, f: Integer;
  begin
    case aXml.ChangeKind of
      ckDelete:
        aXml.Ignored := (Assigned (ignoreAddingOn))
                    and (   ignoreAddingOn.Find(rmPrefix(aXml.TagName) , f)
                         or ignoreAddingOn.Find(aXml.FullUQCaption , f)
                         or ignoreAddingOn.Find(aXml.Prefix + '.' + aXml.FullUQCaption , f)
                        );
      ckAdd:
        aXml.Ignored := (Assigned (ignoreRemovingOn))
                    and (   ignoreRemovingOn.Find(rmPrefix(aXml.TagName) , f)
                         or ignoreRemovingOn.Find(aXml.FullUQCaption , f)
                         or ignoreRemovingOn.Find(aXml.Prefix + '.' + aXml.FullUQCaption , f)
                        );
      else
      begin
        if aXml.ChangeKind = ckModify then
          aXml.Ignored := (Assigned (ignoreDifferencesOn))
                      and (   ignoreDifferencesOn.Find(rmPrefix(aXml.TagName) , f)
                           or ignoreDifferencesOn.Find(aXml.FullUQCaption , f)
                           or ignoreDifferencesOn.Find(aXml.Prefix + '.' + aXml.FullUQCaption , f)
                          );
        for x := 0 to aXml.Items.Count - 1 do
          _set (aXml.Items.XmlItems[x] as TA2BXml);
      end;
    end;
  end;
begin
  Ignored := True;
  _set(Self);
end;

end.