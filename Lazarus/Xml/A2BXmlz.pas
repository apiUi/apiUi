{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit A2BXmlz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
uses Classes
   , SysUtils
   , xmlio
   , Xmlz
   , Bind
   , a2bStringListUnit
   ;

const
  checkAgainstRegExpPrefix = 'regexp:';
  checkAgainstPartialPrefix = 'partialexp:';

type
  TCheckType = (checktypeNormal, checktypeRegExp, checktypePartial);
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
    bValue, bNameSpace: String;
    Prefix, checkExp: String;
    ChangeKind: TChangeKind;
    CheckType: TCheckType;
    function valuesDiffer (aValue, bValue: String): Boolean;
    function AsTabSeparatedValues: String;
    property ThisOneDiffers: Boolean read fThisOneDiffers write setThisOneDiffers;
    property Differs: Boolean read fDiffers write setDiffers;
    property Ignored: Boolean read fIgnored write setIgnored;
    property NumberOfDiffs: Integer read getNumberOfDiffs;
    procedure Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TJBStringList);
    constructor CreateA (aPrefix: String; aXml: TXml; aThisOneDiffers: Boolean); overload;
    constructor CreateB (aPrefix: String; bXml: TXml; aThisOneDiffers: Boolean); overload;
    constructor CreateA2B (aPrefix, aFullCaption: String; aXml, bXml: TXml; ignoreOrderOn, checkValueAgainst: TJBStringList); overload;
    constructor CreateA (aPrefix: String; aXml: TXmlAttribute; aThisOneDiffers: Boolean); overload;
    constructor CreateB (aPrefix: String; bXml: TXmlAttribute; aThisOneDiffers: Boolean); overload;
    constructor CreateA2B (aPrefix: String; aXml, bXml: TXmlAttribute); overload;
  end;

procedure a2bInitialize;
procedure a2bUninitialize;
procedure a2bExpandWhenValueIsJsonOrYaml (aXml: TXml);

implementation

uses wrdFunctionz, StrUtils, igGlobals, base64, RegExpr;

procedure a2bInitialize;
begin
  wrdInitialize;
end;

procedure a2bUninitialize;
begin
  wrdUninitialize;
end;

procedure a2bExpandWhenValueIsJsonOrYaml(aXml: TXml);
  procedure _expandWhenJsonOrYaml;
    function _itMightBeJson: Boolean;
    var
       x: Integer;
    begin
      result := False;
      x := 1;
      while (x <= Length (aXml.Value))
      and (aXml.Value [x] = ' ') do
        Inc (x);
      result := (x <= Length (aXml.Value))
            and (   (aXml.Value[x] = '{')
                 or (aXml.Value[x] = '[')
                );
    end;
    function _itMightBeYaml: Boolean;
    var
       ls: TJBStringList;
    begin
      result := False;
      ls := TJBStringList.Create;
      try
        ls.Text := aXml.Value;
        if (ls.Count > 0)
        and (ls.Strings[0] <> '') then
          with TRegExpr.Create('^[A-Za-z][A-Za-z0-9]+\:$') do
          try
            result := Exec(ls.Strings[0]);
          finally
            Free;
          end;
      finally
        ls.Free;
      end;
    end;
  var
     x: Integer;
     xXml: TXml;
  begin
     if _itMightBeJson then
     begin
       xXml := TXml.Create;
       try
         xXml.LoadJsonFromString(aXml.Value, nil);
         aXml.AddXml(xXml);
         aXml.Value := '';
       except
         xXml.Free;
       end;
       Exit;
     end;
     if _itMightBeYaml then
     begin
       xXml := TXml.Create;
       try
         xXml.LoadYamlFromString(aXml.Value, nil);
         aXml.AddXml(xXml);
         aXml.Value := '';
       except
         xXml.Free;
       end;
       Exit;
     end;
  end;
var
  x: Integer;
begin
  // Exit; // uncomment to test performance degradation
  if (aXml.Items.Count > 0) then
  begin
    for x := 0 to aXml.Items.Count - 1 do
      a2bExpandWhenValueIsJsonOrYaml(aXml.Items.XmlItems[x]);
    Exit;
  end;
  _expandWhenJsonOrYaml;
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

function TA2BXml.valuesDiffer (aValue, bValue: String): Boolean;
  function _checkPartial: Boolean;
  var
    xA, xB: String;
    x: Integer;
  begin
    result := false;
    if Length (aValue) > Length (checkExp) then
      SetLength(xA, Length (aValue))
    else
      SetLength(xA, Length (checkExp));
    x := 1;
    while (x <= Length (aValue))
      and (x <= Length (checkExp)) do
    begin
      if checkExp [x] = '?' then
        xA [x] := checkExp [x]
      else
        xA [x] := aValue [x];
      Inc (x);
    end;
    while (x <= Length (aValue)) do
    begin
      xA [x] := aValue [x];
      Inc (x);
    end;
    if Length (bValue) > Length (checkExp) then
      SetLength(xB, Length (bValue))
    else
      SetLength(xB, Length (checkExp));
    x := 1;
    while (x <= Length (bValue))
      and (x <= Length (checkExp)) do
    begin
      if checkExp [x] = '?' then
        xB [x] := checkExp [x]
      else
        xB [x] := bValue [x];
      Inc (x);
    end;
    while (x <= Length (bValue)) do
    begin
      xB [x] := bValue [x];
      Inc (x);
    end;
    result := (xA <> xB);
  end;

var
  aFileName, bFileName: String;
begin
  if CheckType = checktypeRegExp then
  begin
    if (checkExp <> '')
    and (aValue <> '') then
    begin
      with TRegExpr.Create('^(' + checkExp + ')$') do
      try
        result := not Exec(aValue);
      finally
        Free;
      end;
      exit;
    end;
  end;
  if CheckType = checktypePartial then
  begin
    if checkExp <> '' then
    begin
      result := _checkPartial;
      exit;
    end;
  end;
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


constructor TA2BXml.CreateA(aPrefix: String; aXml: TXml; aThisOneDiffers: Boolean);
var
  x: Integer;
begin
  inherited Create;
  Checked := True;
  ChangeKind := ckDelete;
  TagName := aXml.TagName;
  Value := aXml.Value;
  Prefix:=aPrefix;
  NameSpace := aXml.NameSpace;
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
  Checked := True;
  ChangeKind := ckAdd;
  TagName := bXml.TagName;
  bValue := bXml.Value;
  bNameSpace := bXml.NameSpace;
  Prefix:=aPrefix;
  ThisOneDiffers := aThisOneDiffers;
  for x := 0 to bXml.Attributes.Count - 1 do
    AddXml (TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[x], False));
  for x := 0 to bXml.Items.Count - 1 do
    AddXml (TA2BXml.CreateB(aPrefix, bXml.Items.XmlItems[x], False));
end;

constructor TA2BXml.CreateA2B(aPrefix, aFullCaption: String; aXml, bXml: TXml; ignoreOrderOn, checkValueAgainst: TJBStringList);
var
  x, a, b, c, f, i, y: Integer;
  Diffs: TA2BStringList;
  childXml: TA2BXml;
  xXml: TXml;
  xCaption: String;
  xCheckValueAgainst: String;
begin
  inherited Create;
  CheckType := checktypeNormal;
  xCaption := IfThen(aFullCaption = '', aXml.UQCaption, aFullCaption + '.' + aXml.UQCaption);
  Checked := True;
  TagName := aXml.TagName;
  Value := aXml.Value;
  NameSpace := aXml.NameSpace;
  bValue := bXml.Value;
  bNameSpace := bXml.NameSpace;
  if Assigned (checkValueAgainst) then
  begin
    xCheckValueAgainst := checkValueAgainst.Values[xCaption];
    if AnsiStartsStr(checkAgainstRegExpPrefix, xCheckValueAgainst) then
    begin
      checkExp := Copy (xCheckValueAgainst, Length (checkAgainstRegExpPrefix) + 1, MaxInt);
      CheckType := checktypeRegExp;
    end;
    if AnsiStartsStr(checkAgainstPartialPrefix, xCheckValueAgainst) then
    begin
      checkExp := Copy (xCheckValueAgainst, Length (checkAgainstPartialPrefix) + 1, MaxInt);
      CheckType := checktypePartial;
    end;
  end;
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
  if (Namespace <> bNameSpace)
  and (NameSpace <> '')
  and (bNameSpace <> '')
  then
  begin
    aXml.AddAttribute(TXmlAttribute.CreateAsString('xmlns', NameSpace));
    bXml.AddAttribute(TXmlAttribute.CreateAsString('xmlns', bNameSpace));
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
        childXml := AddXml(TA2BXml.CreateA2B( aPrefix
                                            , aXml.Attributes.XmlAttributes[a]
                                            , bXml.Attributes.XmlAttributes[b]
                                            )
                          ) as TA2BXml;
        Differs := Differs or childXml.Differs;
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
        for i := b to b + Changes[c].Range - 1 do
        begin
          AddXml (TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
          Differs := True;
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
            Differs := True;
            inc(a);
          end;
        end
        else
        begin
          for i := a to a + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateA(aPrefix, aXml.Attributes.XmlAttributes[a], True));
            Differs := True;
            inc(a);
          end;
          for i := b to b + Changes[c].Range - 1 do
          begin
            AddXml(TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
            Differs := True;
            inc(b);
          end;
        end;
      end;
    end;
    while (a < aXml.Attributes.Count) and (b < bXml.Attributes.Count) do
    begin
      childXml := AddXml(TA2BXml.CreateA2B ( aPrefix
                                           , aXml.Attributes.XmlAttributes[a]
                                           , bXml.Attributes.XmlAttributes[b]
                                           )
                        ) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a); inc(b);
    end;
    while (a < aXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateA(aPrefix, aXml.Attributes.XmlAttributes[a], True));
      Differs := True;
      inc(a);
    end;
    while (b < bXml.Attributes.Count) do
    begin
      AddXml(TA2BXml.CreateB(aPrefix, bXml.Attributes.XmlAttributes[b], True));
      Differs := True;
      inc(b);
    end;
  end;

  for x := 0 to aXml.Items.Count - 1 do
    aXml.Items.Strings [x] := aXml.Items.XmlItems [x].TagName;
  for x := 0 to bXml.Items.Count - 1 do
    bXml.Items.Strings [x] := bXml.Items.XmlItems [x].TagName;
  if Assigned (ignoreOrderOn)
  and ignoreOrderOn.Find(aPrefix + '.' + aXml.FullUQCaption , f) then
  begin
    for x := 0 to aXml.Items.Count - 1 do
    begin
      with ignoreOrderOn.Objects[f] as TJBStringList do
      begin
        for y := 0 to Count - 1 do
        begin
          xXml := aXml.Items.XmlItems [x].FindUQXml(Strings[y]);
          if Assigned (xXml) then
            aXml.Items.Strings[x] := aXml.Items.Strings[x] + ';' + xXml.Name + ';' + xXml.Value;
        end;
      end;
    end;
    aXml.Items.Sort;
    for x := 0 to bXml.Items.Count - 1 do
    begin
      with ignoreOrderOn.Objects[f] as TJBStringList do
      begin
        for y := 0 to Count - 1 do
        begin
          xXml := bXml.Items.XmlItems [x].FindUQXml(Strings[y]);
          if Assigned (xXml) then
            bXml.Items.Strings[x] := bXml.Items.Strings[x] + ';' + xXml.Name + ';' + xXml.Value;
        end;
      end;
    end;
    bXml.Items.Sort;
  end;
  with Diffs do
  begin
    Execute(aXml.Items, bXml.Items);
    a := 0; b := 0;
    for c := 0 to ChangeCount - 1 do
    begin
      while a < Changes[c].x do
      begin
        childXml := AddXml (TA2BXml.CreateA2B(aPrefix, xCaption, aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrderOn, checkValueAgainst)) as TA2BXml;
        Differs := Differs or childXml.Differs;
        inc(a); inc(b);
      end;
      if Changes[c].Kind = ckAdd then
      begin
//        ThisOneDiffers := True;
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
//          ThisOneDiffers := True;
          for i := a to a + Changes[c].Range - 1 do
          begin
            childXml := AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[a], True)) as TA2BXml;
            Differs := Differs or childXml.Differs;
            inc(a);
          end;
        end
        else
        begin
//          ThisOneDiffers := True;
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
      childXml := AddXml (TA2BXml.CreateA2B(aPrefix, xCaption, aXml.Items.XmlItems[a], bXml.Items.XmlItems[b], ignoreOrderOn, checkValueAgainst)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a); inc(b);
    end;
    while (a < aXml.Items.Count) do
    begin
//      ThisOneDiffers := True;
      childXml := AddXml (TA2BXml.CreateA(aPrefix, aXml.Items.XmlItems[a], True)) as TA2BXml;
      Differs := Differs or childXml.Differs;
      inc(a);
    end;
    while (b < bXml.Items.Count) do
    begin
//      ThisOneDiffers := True;
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
  Checked := True;
  ChangeKind := ckDelete;
  TagName := '@' + aXml.Name;
  Value := aXml.Value;
  Prefix:= aPrefix;
  ThisOneDiffers := aThisOneDiffers;
end;

constructor TA2BXml .CreateB (aPrefix: String; bXml : TXmlAttribute; aThisOneDiffers: Boolean); overload;
begin
  inherited Create;
  Checked := True;
  ChangeKind := ckAdd;
  TagName := '@' + bXml.Name;
  bValue := bXml.Value;
  Prefix:=aPrefix;
  ThisOneDiffers := aThisOneDiffers;
end;

constructor TA2BXml .CreateA2B (aPrefix: String; aXml , bXml : TXmlAttribute);
begin
  inherited Create;
  Checked := True;
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

function TA2BXml .AsTabSeparatedValues : String ;
const
  Tab = Chr(9);
  function _Action (aCK: TChangeKind): String;
  begin
    result := '';
    case aCk of
      ckAdd: result := 'Add';
      ckDelete: result := 'Delete';
      ckModify: result := 'Modify';
      ckCopy: result := 'Copy';
    end;
  end;
  function _tsv (aXml: TA2BXml; aIndent: Integer): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 0 to aIndent - 1 do
      result := result + ' ';
    result := result + aXml.Name
            + Tab + _Action (aXml.ChangeKind)
            + Tab + IfThen(aXml.Ignored and not (aXml.ChangeKind = ckCopy), 'true')
            + Tab + '"'+ aXml.Value + '"'
            + Tab + '"'+ aXml.bValue + '"'
            + LineEnding
            ;
    for x := 0 to aXml.Items.Count - 1 do
      result := result + _tsv (aXml.Items.XmlItems[x] as TA2BXml, aIndent + 2);
  end;

begin
  result := 'Tag'
          + Tab + 'Action'
          + Tab + 'Ignored'
          + Tab + 'Value'
          + Tab + 'ReferenceValue'
          + LineEnding
          + _tsv(self, 0);
end;

function TA2BXml.getNumberOfDiffs: Integer;
var
  x: Integer;
begin
  result := 0;
  if fThisOneDiffers and not fIgnored then
    result := 1
  else
    for x := 0 to Items.Count - 1 do
      result := result + (Items.XmlItems[x] as TA2BXml).NumberOfDiffs;
end;

procedure TA2BXml.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn: TJBStringList);
  procedure _set (aXml: TA2BXml);
  var
    x, f: Integer;
  begin
    if Assigned (ignoreDifferencesOn)
    and (   ignoreDifferencesOn.Find(aXml.TagName + '.*', f)
         or ignoreDifferencesOn.Find(aXml.FullCaption + '.*' , f)
        ) then
      Exit;
    case aXml.ChangeKind of
      ckDelete:
        aXml.Ignored := (Assigned (ignoreAddingOn))
                    and (   ignoreAddingOn.Find(aXml.TagName, f)
                         or ignoreAddingOn.Find(aXml.FullCaption , f)
                         or ignoreAddingOn.Find(aXml.Prefix + '.' + aXml.FullCaption , f)
                        );
      ckAdd:
        aXml.Ignored := (Assigned (ignoreRemovingOn))
                    and (   ignoreRemovingOn.Find(aXml.TagName, f)
                         or ignoreRemovingOn.Find(aXml.FullCaption , f)
                         or ignoreRemovingOn.Find(aXml.Prefix + '.' + aXml.FullCaption , f)
                        );
      else
      begin
        if aXml.ChangeKind = ckModify then
          aXml.Ignored := (Assigned (ignoreDifferencesOn))
                      and (   ignoreDifferencesOn.Find(aXml.TagName , f)
                           or ignoreDifferencesOn.Find(aXml.FullCaption , f)
                           or ignoreDifferencesOn.Find(aXml.Prefix + '.' + aXml.FullCaption , f)
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
