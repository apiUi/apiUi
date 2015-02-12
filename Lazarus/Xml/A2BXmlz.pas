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

type TA2BXmlAttribute = class (TXmlAttribute)
  private
    fDiffers: Boolean;
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
    constructor CreateA (aAtt: TXmlAttribute);
    constructor CreateB (bAtt: TXmlAttribute);
    constructor CreateA2B (aAtt, bAtt: TXmlAttribute; aPrefix: String; ignoreDifferencesOn: TStringList);
  end;

type TA2BXml = class (TXml)
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
    constructor CreateA (aXml: TXml);
    constructor CreateB (bXml: TXml);
    constructor CreateA2B (aXml, bXml: TXml; ignoreDifferencesOn: TStringList; ignoreOrder: Boolean);
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
    AddAttribute(TA2BXmlAttribute.CreateA(aXml.Attributes.XmlAttributes[x]));
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
    AddAttribute(TA2BXmlAttribute.CreateB(bXml.Attributes.XmlAttributes[x]));
  for x := 0 to bXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateB(bXml.Items.XmlItems[x]));
end;

constructor TA2BXml.CreateA2B(aXml, bXml: TXml; ignoreDifferencesOn: TStringList; ignoreOrder: Boolean);
var
  x, a, b, c, f, i: Integer;
  Diffs: TA2BStringList;
  childXml: TA2BXml;
  childAtt: TA2BXmlAttribute;
begin
  inherited Create;
  TagName := aXml.TagName;
  Value := aXml.Value;
  bValue := bXml.Value;
  ChangeKind := ckCopy;
  if valuesDiffer(Value, bValue) then
  begin
    if (not Assigned (ignoreDifferencesOn))
    or (    (not ignoreDifferencesOn.Find(rmPrefix(TagName) , f))
        and (not ignoreDifferencesOn.Find(aXml.FullUQCaption , f))
       )
    then
    begin
      ChangeKind := ckModify;
      Differs := True;
      fThisOneDiffers := True;
    end
    else
      fIgnoredDifference := True;
  end;
  Diffs := TA2BStringList.Create;

  for x := 0 to aXml.Attributes.Count - 1 do
    aXml.Attributes.Strings [x] := rmPrefix (aXml.Attributes.XmlAttributes [x].Name);
  for x := 0 to bXml.Attributes.Count - 1 do
    bXml.Attributes.Strings [x] := rmPrefix (bXml.Attributes.XmlAttributes [x].Name);
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
        childAtt := TA2BXmlAttribute.CreateA2B( aXml.Attributes.XmlAttributes[a]
                                              , bXml.Attributes.XmlAttributes[b]
                                              , aXml.FullUQCaption
                                              , ignoreDifferencesOn
                                              );
        Differs := Differs or childAtt.Differs;
        AddAttribute(childAtt);
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        Differs := True;
        for i := b to b + Changes[c].Range - 1 do
        begin
          childAtt := TA2BXmlAttribute.CreateB(bXml.Attributes.XmlAttributes[b]);
          Differs := True;
          AddAttribute(childAtt);
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
            childAtt := TA2BXmlAttribute.CreateA(aXml.Attributes.XmlAttributes[a]);
            Differs := True;
            AddAttribute(childAtt);
            inc(a);
          end;
        end
        else
        begin
          Differs := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childAtt := TA2BXmlAttribute.CreateA(aXml.Attributes.XmlAttributes[a]);
            Differs := True;
            AddAttribute(childAtt);
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            childAtt := TA2BXmlAttribute.CreateB(bXml.Attributes.XmlAttributes[b]);
            Differs := True;
            AddAttribute(childAtt);
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Attributes.Count) and (b < bXml.Attributes.Count) do
    begin
      childAtt := TA2BXmlAttribute.CreateA2B ( aXml.Attributes.XmlAttributes[a]
                                             , bXml.Attributes.XmlAttributes[b]
                                             , aXml.FullUQCaption
                                             , ignoreDifferencesOn
                                             );
      Differs := Differs or childAtt.Differs;
      AddAttribute(childAtt);
      inc(a); inc(b);
    end;
    while (a < aXml.Attributes.Count) do
    begin
      Differs := True;
      childAtt := TA2BXmlAttribute.CreateA(aXml.Attributes.XmlAttributes[a]);
      AddAttribute(childAtt);
      inc(a);
    end;
    while (b < bXml.Attributes.Count) do
    begin
      Differs := True;
      childAtt := TA2BXmlAttribute.CreateB(bXml.Attributes.XmlAttributes[b]);
      AddAttribute(childAtt);
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
        childXml := AddXml (TA2BXml.CreateA2B(aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreDifferencesOn, ignoreOrder)) as TA2BXml;
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
      childXml := AddXml (TA2BXml.CreateA2B(aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreDifferencesOn, ignoreOrder)) as TA2BXml;
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
  if Value then
    if Assigned (Parent) then
      (Parent as TA2BXml).IgnoredDifference := True;
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

{ TA2BXmlAttribute }

constructor TA2BXmlAttribute.CreateA(aAtt: TXmlAttribute);
begin
  inherited Create;
  ChangeKind := ckDelete;
  Name := aAtt.Name;
  Value := aAtt.Value;
  Differs := True;
end;

constructor TA2BXmlAttribute.CreateA2B(aAtt, bAtt: TXmlAttribute; aPrefix: String;
  ignoreDifferencesOn: TStringList);
var
  f: Integer;
begin
  inherited Create;
  Name := aAtt.Name;
  Value := aAtt.Value;
  bValue := bAtt.Value;
  if Value = bValue then
  begin
    Differs := False;
    ChangeKind := ckCopy;
  end
  else
  begin
    if (not Assigned (ignoreDifferencesOn))
    or (    (not ignoreDifferencesOn.Find(rmPrefix(aAtt.Name) , f))
        and (not ignoreDifferencesOn.Find(aPrefix + '.' + aAtt.Name , f))
       )
    then
    begin
      ChangeKind := ckModify;
      Differs := True;
    end
    else
      fIgnoredDifference := True;
  end;
end;

constructor TA2BXmlAttribute.CreateB(bAtt: TXmlAttribute);
begin
  inherited Create;
  ChangeKind := ckAdd;
  Name := bAtt.Name;
  bValue := bAtt.Value;
  Differs := True;
end;

function TA2BXmlAttribute.getNumberOfDiffs: Integer;
begin

end;

procedure TA2BXmlAttribute.setDiffers(const Value: Boolean);
begin
  fDiffers := Value;
end;

procedure TA2BXmlAttribute.setIgnoredDifference(const Value: Boolean);
begin
  fIgnoredDifference := Value;
end;

end.
