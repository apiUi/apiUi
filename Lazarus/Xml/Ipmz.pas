unit Ipmz;
{$define noXMLDOM}
{$MODE Delphi}

interface

uses Classes
   , Express
   , Bind
   , Xmlz
   , ParserClasses
   {$ifndef NoGUI}
   , Dialogs
   , Graphics
   {$endif}
   , FileUtil
   ;

type
  TCobolEnvironmentType = (ceTandem, ceIbmZOs);
  TEndiannessType = (etLittleEndian, etBigEndian);
  TOnFoundError = procedure ( ErrorString: String
                            ; aObject: TObject
                            ) of Object;

  TAnsiString = class (TObject)
  public
    s: AnsiString;
    b: Boolean;
  end;
  TStringListList = class (TStringList)
  private
    fColCount: Integer;
    fRowCount: Integer;
    procedure SetStringList(Index: integer; const Value: TStringList);
    procedure setColCount(const Value: Integer);
    procedure setRowCount(const Value: Integer);
    function getRowText(Index: integer): String;
    function getCellValue(aCol, aRow: Integer): String;
    procedure setCellValue(aCol, aRow: Integer; const Value: String);
  protected
    function GetStringList (Index: integer): TStringList;
  public
    property RowCount: Integer read fRowCount write setRowCount;
    property ColCount: Integer read fColCount write setColCount;
    property CellValue [aCol, aRow: Integer]: String read getCellValue write setCellValue;
    property RowText [Index: integer]: String read getRowText;
    property StringLists [Index: integer]: TStringList read GetStringList write SetStringList;
  end;

  TIpmItemList = class;

  TIpmItem = class(TCustomBindable)
  private
    function GetFullCaption: String;
    function getIsAlignedData: boolean;
    function getRootIpmItem: TIpmItem;
private
  sl: TStringList;
  TypeDefs: TStringList;
    fCobolEnvironment: TCobolEnvironmentType;
  procedure HaveString (aString: String);
  procedure Int64ToValue (aInteger: Int64);
  function Int64FromValue (aValue: String): Int64;
  procedure DoExtendPictureClause;
  function GetPictureCaption: String;
  function TagToXml (aValue: String): String;
  function IndentString (x: Integer): String;
  procedure _BuildXSDLocal (OnHaveString: TOnHaveString; aIndent: Integer);
  procedure _BuildXSDGlobalSimpleTypedefs (OnHaveString: TOnHaveString; aIndent: Integer; aTypeDefs: TStringList);
  procedure _BuildXSDGlobalComplexTypedefs (OnHaveString: TOnHaveString; aIndent: Integer; aTypeDefs: TStringList);
  procedure _BuildXML (OnHaveString: TOnHaveString; OnlyWhenFilled: Boolean);
  function GetSoapBodyItem: TIpmItem;
  function GetSoapHeaderItem: TIpmItem;
  function GetXsdOccurs: String;
  function GetMaxInclusive: String;
  function GetMinInclusive: String;
  function getCobolValue: AnsiString;
    procedure setCobolEnvironment(const Value: TCobolEnvironmentType);
  property IsAlignedData: boolean read getIsAlignedData;
public
  Level: Integer;
{ Group: Boolean; now in TCustomBindable}
  Occurs: Integer;
  Occurrence: Integer;
  Redefines: Boolean;
  minOccurs: Integer;
  Numeric: Boolean;
  HasComp: Boolean;
  Comp: Boolean;
  Display: Boolean;
  ExtendedPictureClause: String; // pic s9(02)v9(02) => s99v99
  FloatFormat: String;
  InputLength: Integer;
  Bytes: Integer;
  Offset: Integer;
  Precision: Integer;
  PictureClause: String;
  Signed: Boolean;
  SignLeading: Boolean;
  SignSeparate: Boolean;
{ Value: String; } {now in CustomBindable}
{  ExpectedValue: String; } {now in CustomBindable}
{  Parent: TIpmItem; } {now in custombindable}
  Items: TIpmItemList;
  Loaded: Boolean;
  Filled: Boolean;
  XmlFileName: String;
  Level88Values: TStringList;
  SkipOnXmlBuild: Boolean;
  {$ifndef NoGUI}
  function bgColor (aReadOnly: Boolean; aColumn: Integer): TColor; Override;
  {$endif}
  procedure Populate(aViewType: TxvViewType); Override;
  function Children: TBindableList; Override;
  function GetFullIndexCaption: String; Override;
  function GetIndexCaption: String; Override;
  function GetCaption: String; Override;
  function Buffer (aRecord: AnsiString): AnsiString; // ansi because of bnary data in Cobol records
  property RootIpmItem: TIpmItem read getRootIpmItem;
  property CobolEnvironment: TCobolEnvironmentType read fCobolEnvironment write setCobolEnvironment;
  property Caption: String read GetCaption;
  property SoapHeaderItem: TIpmItem read GetSoapHeaderItem;
  property SoapBodyItem: TIpmItem read GetSoapBodyItem;
  property FullCaption: String read GetFullCaption;
  property PictureCaption: String read GetPictureCaption;
  property minInclusive: String read GetMinInclusive;
  property maxInclusive: String read GetMaxInclusive;
  property CobolValue: AnsiString read getCobolValue;
  procedure LoadValues (aXml: TXml); overload;
  procedure LoadValues (aIpm: TIpmItem); overload;
  procedure GroupCopy (aIpm: TIpmItem; aSkipAssignments: Boolean);
  procedure ResetLoaded (aKeepValues: Boolean);
  function SetFilled: Boolean;
  procedure LinkToParent (Previous: TIpmItem);
  function ParentWithLevel (level: Integer): TIpmItem;
  function CreatePhysicalIpm (aCobolEnvironment: TCobolEnvironmentType): TIpmItem;
  procedure CalcIpmOffsets;
  procedure BuildXSDLocal (OnHaveString: TOnHaveString; aIndent: Integer);
  procedure BuildXSDGlobal (OnHaveString: TOnHaveString; aIndent: Integer);
  procedure BuildXML (OnHaveString: TOnHaveString; aIndent: Integer; OnlyWhenFilled: Boolean);
  procedure BuildCobolWS (OnHaveString: TOnHaveString);
  procedure Bind (aRoot: String; aExpress: TObject; aMaxOccurs: Integer); Override;
  function AsXml: TXml;
  function IsValueValid (var aMessage: String): Boolean; Override;
  function ValueToBuffer (aValue: AnsiString): AnsiString; // ansi because of bnary data in Cobol records
  function ValuesToBuffer (OnFoundError: TOnFoundError): AnsiString;
  procedure BufferToValues (OnFoundError: TOnFoundError; aBuffer: AnsiString); // ansi because of bnary data in Cobol records
  function FindIpmItem (aName: String): TIpmItem;
  function FindUQ (aName: String): TCustomBindable; Override;
  procedure PutGroupData (aObject: TObject); override;
  procedure PutStringData (aString: String); override;
  function DuplicateSelf (aParent: TIpmItem): TIpmItem;
  constructor Create; Overload;
  constructor Create (aIpm: TIpmItem); Overload;
  destructor Destroy; override;
end;

TIpmItemList = class (TBindableList)
  private
    procedure SetIpmItem(Index: integer; const Value: TIpmItem);
protected
  function GetIpmItem (Index: integer): TIpmItem;
public
  procedure AddAliasFieldIpmItems(aAlias: String; aIpmItem: TIpmItem);
  property IpmItems [Index: integer]: TIpmItem read GetIpmItem write SetIpmItem;
end;


type CompType = record
  case Integer of
   1: (Bytes: array [1..8] of Byte);
   2: (Short : Smallint); // 2 bytes Signed
   3: (UShort : Word); // 2 bytes Unsigned
   4: (Int: Integer); // 4 bytes Signed
   5: (UInt: LongWord); // 4 bytes Unsigned
   6: (Long: Int64); // 8 bytes Signed
   7: (ULong: UInt64); // 8 bytes Unsigned
end;

type TRecog = class (TObject)
public
  Start, Length: Integer;
  Value: String;
end;

type TIpmDescr = class (TObject)
private
  LineNumber: Integer;
    fCobolEnvironment: TCobolEnvironmentType;
  procedure AnalyserNeedData( Sender: TObject
                            ; var MoreData: Boolean
                            ; var Data: String
                            );
  procedure DoReplacing;
    function GetReplacingAsQString: String;
    procedure SetReplacingAsQString(const Value: String);
    procedure setCobolEnvironment(const Value: TCobolEnvironmentType);
public
  Alias: String;
  FileName: String;
  IpmItem: TIpmItem;
  FileContents: TStringList;
  Replacing: TStringList;
  Recogs: TStringList;
  function DuplicateSelf: TIpmDescr;
  property CobolEnvironment: TCobolEnvironmentType read fCobolEnvironment write setCobolEnvironment;
  property ReplacingAsString: String read GetReplacingAsQString write SetReplacingAsQString;
  function LoadFromFile (aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
  {$ifdef XMLDOM}
  function LoadFromXMLSchema (aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
  {$endif}
  constructor Create;
  destructor Destroy; override;
end;

type TIpmDescrs = class (TStringList)
  private
    fIpmIndex: Integer;
    fRecordDescrTypeDefaultAlias: String;
    fRecordDescrTypeBinds: TBindableList;
    function getSelectedIpmItem: TIpmItem;
    procedure setIpmIndex(const Value: Integer);
    procedure setRecordDescrTypeDefaultAlias(const Value: String);
    procedure setRecordDescrTypeBinds(const Value: TBindableList);
protected
  function GetIpmDescr (Index: integer): TIpmDescr;
public
  RecordDescrTypeValues: TStringListList;
  RecordDescrTypeAliasses: TStringList;
  property RecordDescrTypeBinds: TBindableList read fRecordDescrTypeBinds write setRecordDescrTypeBinds;
  property RecordDescrTypeDefaultAlias: String read fRecordDescrTypeDefaultAlias write setRecordDescrTypeDefaultAlias;
  property IpmIndex: Integer read fIpmIndex write setIpmIndex;
  property SelectedIpmItem: TIpmItem read getSelectedIpmItem{ write setIpmItem};
  property IpmDescrs [Index: integer]: TIpmDescr read GetIpmDescr;
  function GetRecordDescrTypeIndex (aRecord: String): Integer;
  function FindIpmItem (aName: String): TIpmItem;
  function FindIpmDescr (aString: String): TIpmDescr;
  procedure Clear; override;
  constructor Create;
  destructor Destroy; override;
end;

function ExtendPictureClause (arg: String): String;
function PictureClauseToLength (arg: String): Integer;

var
  ourEndianness: TEndiannessType;
  theirEndianness: TEndiannessType;
  BaseIpmItem: TIpmItem;
  PreviousIpmItem: TIpmItem;
  CurrentIpmItem: TIpmItem;
  Indent: Integer;
  IpmDescrs: TIpmDescrs;
  igLength: Integer;
  UserWorkingArea: String;
  UWASeparator: String;
  PictureClause: String;
  Occurs: Integer;
  OffsetColumns: Integer;
  Redefines: Boolean;

implementation

uses SysUtils
   , StrUtils
   , Math
   , ErrorFound
   , IpmAnalyser
   , xmlio
   ;

function ExtendPictureClause (arg: String): String;
var
  s: Integer;
  d: Integer;
  l: Integer;
  IntStr: String;
  i: Integer;
  RepeatChar: String;
  RepeatCount: Integer;
begin
  result := '';
  l := system.Length (arg);
  s := 1;
  while s <= l do
  begin
    case arg [s] of
      '(':
      begin
        RepeatChar := result [system.Length (result)];
        Inc (s);
        IntStr := '';
        while (s <= l) and (arg [s] <> ')') do
        begin
          IntStr := IntStr + arg [s];
          Inc (s);
        end;
        RepeatCount := StrToInt (IntStr) - 1; {eentje hebben we al}
        for i := 1 to RepeatCount do
          result := result + RepeatChar;
        Inc (s);
      end;
      'V': Inc (s);
      'v': Inc (s);
      else
      begin
        result := result + arg [s];
        Inc (s);
      end;
    end;
  end;
end;

function PictureClauseToLength (arg: String): Integer;
begin
  result := system.Length (ExtendPictureClause (arg));
end;

procedure TIpmItem.DoExtendPictureClause;
var
  s: Integer;
  l: Integer;
  IntStr: String;
  i: Integer;
  RepeatChar: String;
  RepeatCount: Integer;
  function _CompBytes (Bytes: Integer): Integer;
  begin
    result := 8;
    if Bytes < 10 then
      result := 4;
    if Bytes < 5 then
      result := 2;
  end;
begin
  ExtendedPictureClause := '';
  FloatFormat := '';
  InputLength := 0;
  Bytes := 0;
  Precision := 0;
  Numeric := False;
  l := Length (PictureClause);
  s := 1;
  while s <= l do
  begin
    case PictureClause [s] of
      '(':
      begin
        RepeatChar := ExtendedPictureClause [Length (ExtendedPictureClause)];
        Inc (s);
        IntStr := '';
        while (s <= l) and (PictureClause [s] <> ')') do
        begin
          IntStr := IntStr + PictureClause [s];
          Inc (s);
        end;
        RepeatCount := StrToInt (IntStr) - 1; {eentje hebben we al}
        for i := 1 to RepeatCount do
        begin
          ExtendedPictureClause := ExtendedPictureClause + RepeatChar;
          if RepeatChar = '9' then
            FloatFormat := FloatFormat + '0';
        end;
        InputLength := InputLength + RepeatCount;
        Bytes := Bytes + RepeatCount;
        if RepeatChar = '9' then
          Precision := Precision + RepeatCount;
        Inc (s);
      end;
      '9':
      begin
        Numeric := True;
        ExtendedPictureClause := ExtendedPictureClause + PictureClause [s];
        FloatFormat := FloatFormat + '0';
        Inc (InputLength);
        Inc (Bytes);
        Inc (Precision);
        Inc (s);
      end;
      'V', 'v':
      begin
        ExtendedPictureClause := ExtendedPictureClause + PictureClause [s];
        Inc (InputLength);
        Precision := 0;
        Inc (s);
      end;
      'S', 's':
      begin
        self.Signed := True;
        ExtendedPictureClause := ExtendedPictureClause + PictureClause [s];
        Inc (InputLength);
        Inc (s);
      end;
      '+', '-':
      begin
        ExtendedPictureClause := ExtendedPictureClause + PictureClause [s];
        SignSeparate := True;
        if s = 1 then
          SignLeading := True;
        Inc (InputLength);
        Inc (s);
      end;
      else
      begin
        ExtendedPictureClause := ExtendedPictureClause + PictureClause [s];
        Inc (InputLength);
        Inc (Bytes);
        Inc (s);
      end;
    end;
  end;
  if Numeric then
  begin
    if Precision = Bytes then
      Precision := 0;
    if Comp then
    begin
      Bytes := _CompBytes (Bytes);
      (Parent as TIpmItem).HasComp := True;
    end
    else
    begin
      if SignSeparate then
        Inc (Bytes);
    end;
  end;
end;

function TIpmItem.GetPictureCaption: String;
begin
  result := PictureClause;
  if Comp = True then
    result := result + ' comp';
end;

procedure TIpmItem.LinkToParent (Previous: TIpmItem);
begin
  if Level = Previous.Level then
    Parent := Previous.Parent
  else
  begin
    if Level > Previous.Level then
      Parent := Previous
    else
    begin
      Parent := Previous.ParentWithLevel (Level).Parent;
      if Parent = nil then
        raise Exception.Create ('Illegal leveling in IPM');
    end;
  end;
  ((Parent as TIpmItem).Items as TIpmItemList).AddObject (Name, Self);
  (Parent as TIpmItem).Group := True;
end;

function TIpmItem.ParentWithLevel (level: Integer): TIpmItem;
begin
  result := (Parent as TIpmItem);
  while (result <> nil)
  and (result.Level > Level)
  do
    result := result.Parent as TIpmItem;
end;

function TipmItem.DuplicateSelf (aParent: TIpmItem): TIpmItem;
var
  x: Integer;
begin
  result := TIpmItem.Create;

  result.Level := Level;
  result.Name := Name;
  result.Group := Group;
  result.Redefines := Redefines;
  result.minOccurs := minOccurs;
  result.Occurs := Occurs;
  result.Occurrence := Occurrence;
  result.Numeric := Numeric;
  result.HasComp := HasComp;
  result.Comp := Comp;
  result.Display := Display;
  result.ExtendedPictureClause := ExtendedPictureClause;
  result.FloatFormat := FloatFormat;
  result.InputLength := InputLength;
  result.Bytes := Bytes;
  result.Offset := Offset;
  result.Precision := Precision;
  result.PictureClause := PictureClause;
  result.Signed := self.Signed;
  result.SignLeading := SignLeading;
  result.SignSeparate := SignSeparate;
  result.Value := Value;
  result.ExpectedValue := ExpectedValue;
  result.Parent := aParent;
  for x := 0 to Items.Count - 1 do
    result.Items.AddObject ( Items.Strings [x]
                           , Items.IpmItems [x].DuplicateSelf (result)
                           );
  result.Loaded := Loaded;
  result.Filled := Filled;
  result.XmlFileName := XmlFileName;
  result.Level88Values.Text := Level88Values.Text;
  result.SkipOnXmlBuild := SkipOnXmlBuild;
end;

procedure TIpmItem.CalcIpmOffsets;
var
  xOffset: Integer;
  procedure _CalcIpmOffsets (aIpm: TIpmItem);
  var
    x: Integer;
  begin
    if aIpm.IsAlignedData then
      xOffset := xOffset + xOffset mod 2;
    aIpm.Offset := xOffset;
    if aIpm.Group then
    begin
      for x := 0 to aIpm.Items.Count - 1 do
        _CalcIpmOffsets(aIpm.Items.IpmItems[x]);
      // on Tandem a repeating group with a comp field must have an even length
      if (CobolEnvironment = ceTandem)
      and (aIpm.HasComp)
      and (aIpm.Occurs > 1) then
          xOffset := xOffset + ((xOffset - aIpm.Offset) mod 2);
      aIpm.Bytes := xOffset - aIpm.Offset;
    end
    else
      xOffset := xOffset + aIpm.Bytes;
  end;
begin
  xOffset := 0;
  _CalcIpmOffsets(self);
end;

function TIpmItem.CreatePhysicalIpm (aCobolEnvironment: TCobolEnvironmentType): TIpmItem;
  function _CreatePhysicalIpm (aParent: TIpmItem; aIpmItem: TIpmItem): TIpmItem;
  var
    x: Integer;
    xOccurs: Integer;
    xIpmItem: TIpmItem;
    rIpmItem: TIpmItem;
  begin
    result := TIpmItem.Create;
    result.Level := aIpmItem.Level;
    result.Name := aIpmItem.Name;
    result.Group := aIpmItem.Group;
    result.Redefines := aIpmItem.Redefines;
    result.minOccurs := aIpmItem.minOccurs;
    result.Occurs := aIpmItem.Occurs;
    result.Bytes := aIpmItem.Bytes;
    result.Comp := aIpmItem.Comp;
    result.HasComp := aIpmItem.HasComp;
    result.Display := aIpmItem.Display;
    result.ExtendedPictureClause := aIpmItem.ExtendedPictureClause;
    result.FloatFormat := aIpmItem.FloatFormat;
    result.Value := aIpmItem.Value;
    result.NullValue := aIpmItem.NullValue;
    result.PictureClause := aIpmItem.PictureClause;
    result.Numeric := aIpmItem.Numeric;
    result.Precision := aIpmItem.Precision;
    result.InputLength := aIpmItem.InputLength;
    result.Signed := aIpmItem.Signed;
    result.SignLeading := aIpmItem.SignLeading;
    result.SignSeparate := aIpmItem.SignSeparate;
    result.Level88Values.Text := aIpmItem.Level88Values.Text;
    if aParent <> nil then
    begin
      (aParent.Items as TIpmItemList).AddObject (result.Name, result);
      result.Parent := aParent;
    end;
    for x := 0 to (aIpmItem.Items as TIpmItemList).Count - 1 do
    begin
      xIpmItem := (aIpmItem.Items as TIpmItemList).IpmItems [x];
      if xIpmItem.Redefines = False then
      begin
        for xOccurs := 1 to xIpmItem.Occurs do
        begin
          rIpmItem := _CreatePhysicalIpm (result, xIpmItem);
          if xIpmItem.Occurs > 1 then
            rIpmItem.Occurrence := xOccurs;
        end;
      end;
    end;
  end;
  procedure _DetermineFieldSizes (aItem: TIpmItem);
  var
    x: Integer;
    xIpmItem: TIpmItem;
  begin
    if aItem.Group = False then
      aItem.DoExtendPictureClause
    else
    begin
      for x := 0 to (aItem.Items as TIpmItemList).Count - 1 do
      begin
        xIpmItem := (aItem.Items as TIpmItemList).IpmItems [x];
        if xIpmItem.Redefines = False then
          _DetermineFieldSizes (xIpmItem);
      end;
      { If this is a group and there is a
        comp field in it, we must tell our parent
      }
      if (aItem.HasComp) then
        (aItem.Parent as TIpmItem).HasComp := True;
    end;
  end;
begin
{
  if Level <> 1 then
    raise Exception.Create ('Illegal level for function');
}
  if Self = nil then Exit;
  Parent := TIpmItem.Create; // dummy parent for highest level
  _DetermineFieldSizes (self);
  result := _CreatePhysicalIpm (nil, self);
  Parent.Free; // free dummy parent
  Parent := nil;
  result.CobolEnvironment := aCobolEnvironment;
end;

constructor TIpmItem.Create(aIpm: TIpmItem);
var
  x: Integer;
  xIpm: TIpmItem;
begin
  inherited Create;
  Level88Values := TStringList.Create;
  Level88Values.Sorted := True;
  Level88Values.Duplicates := dupIgnore;
  Level := aIpm.Level;
  Name := aIpm.Name;
  Group := aIpm.Group;
  Redefines := aIpm.Redefines;
  minOccurs := aIpm.minOccurs;
  Occurs := aIpm.Occurs;
  Occurrence := aIpm.Occurrence;
  Bytes := aIpm.Bytes;
  Offset := aIpm.Offset;
  Comp := aIpm.Comp;
  HasComp := aIpm.HasComp;
  Display := aIpm.Display;
  ExtendedPictureClause := aIpm.ExtendedPictureClause;
  FloatFormat := aIpm.FloatFormat;
  Value := aIpm.Value;
  NullValue := aIpm.NullValue;
  PictureClause := aIpm.PictureClause;
  Numeric := aIpm.Numeric;
  Precision := aIpm.Precision;
  InputLength := aIpm.InputLength;
  Signed := aIpm.Signed;
  SignLeading := aIpm.SignLeading;
  SignSeparate := aIpm.SignSeparate;
  Level88Values.Text := aIpm.Level88Values.Text;
  Items := TIpmItemList.Create;
  for x := 0 to (aIpm.Items as TIpmItemList).Count - 1 do
  begin
    xIpm := TIpmItem.Create ((aIpm.Items as TIpmItemList).IpmItems [x]);
    xIpm.Parent := self;
    Items.AddObject('', xIpm);
  end;
end;

procedure TIpmItem.BufferToValues (OnFoundError: TOnFoundError; aBuffer: AnsiString);
  function _BufferToString (aItem: TIpmItem; BaseOffset: Integer): AnsiString;
  begin
    try
      // no need to check for out of bounds, because Copy does that already
      result := TrimRight (Copy ( aBuffer
                                , aItem.Offset - BaseOffset + 1
                                , aItem.Bytes
                                )
                          );
    except
      on E: Exception do
      begin
        if Assigned (OnFoundError) then
          OnFoundError (E.Message, aItem)
        else
          Raise;
      end;
    end;
  end;
  procedure _BufferToValues (aItem: TIpmItem; BaseOffset: Integer);
  var
    x: Integer;
    xItemList: TIpmItemList;
    xString: AnsiString;
    xInteger: Int64;
    xFloat: Extended;
    xExponent: Double;
    xByte: Byte;
    xComp: CompType;
    xSignByteIndex: Integer;
    procedure _setDoExpectValue (aItem: TIpmItem);
    begin
      if aItem = nil then
        exit;
      aItem.DoExpectValue := True;
      _setDoExpectValue (aItem.Parent as TIpmItem);
    end;
{
    procedure _setHasUnexpectedValue (aItem: TIpmItem);
    begin
      if aItem = nil then
        exit;
      aItem.HasUnExpectedValue := True;
      _setHasUnexpectedValue (aItem.Parent as TIpmItem);
    end;
}
  begin
    aItem.HasUnExpectedValue := False;
    if not aItem.Group then
    begin
      try
        if aItem.Numeric then {if numeric}
        begin
          if not (aItem.Offset - BaseOffset + aItem.Bytes > Length (aBuffer)) then {not behind end of buffer}
          begin
            if aItem.Comp then {if computational}
            begin
              xComp.Long := 0;
              if theirEndianness <> ourEndianness then
              begin
                for x := 1 to aItem.Bytes do {swap byte order}
                begin
                  xComp.Bytes [x] := Byte (aBuffer [aItem.Offset - BaseOffset + aItem.Bytes - x + 1]);
                end;
              end
              else
              begin
                for x := 1 to aItem.Bytes do {well, ...}
                begin
                  xComp.Bytes [x] := Byte (aBuffer [aItem.Offset - BaseOffset + x]);
                end;
              end;
              xInteger := 0; {just to avoid 'might not been initialized warning'}
              if (not aItem.Signed) and (CobolEnvironment = ceIbmZOs) then
              begin
                if aItem.Bytes = 2 then
                  xInteger := xComp.UShort;
                if aItem.Bytes = 4 then
                  xInteger := xComp.UInt;
                if aItem.Bytes = 8 then
                  xInteger := xComp.ULong;
              end
              else
              begin
                if aItem.Bytes = 2 then
                  xInteger := xComp.Short;
                if aItem.Bytes = 4 then
                  xInteger := xComp.Int;
                if aItem.Bytes = 8 then
                  xInteger := xComp.Long;
              end;
              aItem.Int64ToValue (xInteger);
            end {if computational}
            else
            begin {numeric display}
              xString := _BufferToString (aItem, BaseOffset);
              if aItem.SignSeparate then
              begin
                if not aItem.SignLeading then
                begin
                  // move trailing sign to leading position
                  xString := xString [Length (xString)]
                           + Copy (xString, 1, Length (xString) - 1)
                           ;
                end;
              end {if SignSeparate}
              else
              begin {Sign not separate}
                if aItem.SignLeading then
                  xSignByteIndex := 1
                else
                  xSignByteIndex := Length (xString);
                if xSignByteIndex > 0 then
                begin
                  xByte := Byte (xString [xSignByteIndex]);
                  if CobolEnvironment = ceTandem then
                  begin
                    if xByte > 127 then
                    begin
                      xByte := xByte - 128;
                      xString [xSignByteIndex] := AnsiChar (xByte);
                      xString := '-' + xString;
                    end;
                  end;
                  if (CobolEnvironment = ceIbmZOs) then
                  begin
                    if (xByte > Ord ('9'))
                    or (xByte < Ord ('0')) then
                    begin
                      if (xByte = Ord ('{'))
                      or (xByte = Ord ('}'))
                      then begin
                        if (xByte = Ord ('}')) then // negative multiple of 10
                        begin
                          xByte := Ord ('0');
                          xString [xSignByteIndex] := AnsiChar (xByte);
                          xString := '-' + xString;
                        end
                        else
                        begin
                          xByte := Ord ('0');
                          xString [xSignByteIndex] := AnsiChar (xByte);
                        end;
                      end
                      else
                      begin
                        if (xByte < Ord ('J')) then // positive; 'A' => '1'
                        begin
                          xByte := xByte - Ord ('A') + Ord ('1');
                          xString [xSignByteIndex] := AnsiChar (xByte);
                        end
                        else
                        begin // negative; 'J' => '1'
                          xByte := xByte - Ord ('J') + Ord ('1');
                          xString [xSignByteIndex] := AnsiChar (xByte);
                          xString := '-' + xString;
                        end;
                      end;
                    end;
                  end;
                end
                else
                  xString := '0';
              end; {end Sign not Separate}
              try
                xInteger := StrToInt64Def (xString, 0);
                aItem.Int64ToValue (xInteger);
              except
                on E: Exception do OnFoundError (E.Message, aItem);
              end;
            end; {numeric display}
            aItem.NullValue := False;
          end {if not behind end of buffer}
          else
          begin
            aItem.Value := '';
            aItem.NullValue := True;
          end;
        end {if numeric}
        else
        begin {alpha}
          aItem.Value := _BufferToString (aItem, BaseOffset);
        end;
        if aItem.DoExpectValue then
        begin
          _setDoExpectValue (aItem);
          if aItem.Value <> aItem.ExpectedValue then
            aItem.HasUnExpectedValue := True;
        end;
      except
        on E: Exception do OnFoundError (E.Message, aItem);
      end; {try}
    end {if not a group}
    else
    begin {if a group}
      aItem.DoExpectValue := False; {may be set from an elementairy item later}
      xItemList := aItem.Items as TIpmItemList;
      for x := 0 to xItemList.Count - 1 do
      begin
        _BufferToValues (xItemList.IpmItems [x], BaseOffset);
      end;
    end; {if a group}
  end;

begin
  if Self = nil then Exit;
  _BufferToValues (self, Offset);
end;

function TIpmItem.Buffer (aRecord: AnsiString): AnsiString;
begin
  result := Copy (aRecord, Offset + 1, Bytes);
end;

function TIpmItem.IsValueValid (var aMessage: String): Boolean;
var
  x: Integer;
begin
  result := True;
  aMessage := '';
  try
    if Group then
    begin
      x := 0;
      while result and (x < Items.Count) do
      begin
        result := Items.IpmItems [x].IsValueValid (aMessage);
        Inc (x);
      end;
    end
    else
      ValueToBuffer (Value);
  except
    on E: Exception do
    begin
      aMessage := e.Message;
      result := False;
    end;
  end;
end;

function TIpmItem.ValueToBuffer (aValue: AnsiString): AnsiString;
var
  x: Integer;
  aBuffer: AnsiString;
  xByte: Byte;
  xNegative: Boolean;
  xComp: CompType;
  xSignByteIndex: Integer;
  xInteger: Int64;
begin
  if Group then
    raise Exception.Create('ValueToBuffer not allowed on groups');
  if Numeric then
  begin
    xInteger := Int64FromValue (aValue);
    if Comp then
    begin
      if (not Signed) and (CobolEnvironment = ceIbmZOs) then
      begin
        if Bytes = 2 then
        begin
          xComp.UShort := xInteger;
          if xComp.UShort <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
        if Bytes = 4 then
        begin
          xComp.UInt :=   xInteger;
          if xComp.UInt <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
        if Bytes = 8 then
        begin
          xComp.ULong :=   xInteger;
          if xComp.ULong <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
      end
      else
      begin
        if Bytes = 2 then
        begin
          xComp.Short := xInteger;
          if xComp.Short <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
        if Bytes = 4 then
        begin
          xComp.Int :=   xInteger;
          if xComp.Int <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
        if Bytes = 8 then
        begin
          xComp.Long :=   xInteger;
          if xComp.Long <> xInteger then
            raise Exception.Create ('Value does not fit');
        end;
      end;
      SetLength (aBuffer, Bytes);
      if theirEndianness <> ourEndianness then // change byte order
      begin
        for x := 1 to Bytes do
        begin
          aBuffer [x] := AnsiChar (xComp.Bytes [Bytes - x + 1]);
        end;
      end
      else
      begin
        for x := 1 to Bytes do
        begin
          aBuffer [x] := AnsiChar (xComp.Bytes [x]);
        end;
      end;
    end // Comp
    else
    begin // Display
      if xInteger < 0 then
      begin
        xNegative := True;
        xInteger := -1 * xInteger;
      end
      else
        xNegative := False;
      aBuffer := IntToStr (xInteger);
      if Length (aBuffer) > Length (FloatFormat) then
        raise Exception.Create('Value does not fit');
      if Length (FloatFormat) > Length (aBuffer) then
        aBuffer := Copy ( FloatFormat
                        , 1
                        , Length (FloatFormat) - Length (aBuffer)
                        )
                 + aBuffer;
      if (SignSeparate) then
      begin
        if xNegative then
          if SignLeading then
            aBuffer := '-' + aBuffer
          else
            aBuffer := aBuffer + '-'
        else
          if SignLeading then
            aBuffer := '+' + aBuffer
          else
            aBuffer := aBuffer + '+';
      end // sign separate
      else
      begin // sign not separate; adjust first or last byte
        if CobolEnvironment = ceTandem then
        begin
          if (xNegative) then // turn of first bit of ...
          begin
            if SignLeading then
              xSignByteIndex := 1 // first byte
            else
              xSignByteIndex := Length (aBuffer);// last byte
            xByte := Byte (aBuffer [xSignByteIndex]);
            xByte := xByte + 128;
            aBuffer [xSignByteIndex] := AnsiChar (xByte);
          end;
        end; // Tandem
        if CobolEnvironment = ceIbmZOs then
        begin
          if self.Signed or xNegative then
          begin
            if SignLeading then
              xSignByteIndex := 1 // first byte
            else
              xSignByteIndex := Length (aBuffer);// last byte
            xByte := Byte (aBuffer [xSignByteIndex]);
            if xByte = Ord ('0') then
            begin
              if xNegative then
                xByte := Ord ('}')
              else
                xByte := Ord ('{');
            end
            else
            begin
              if xNegative then
                xByte := xByte - Ord ('1') + Ord ('J')
              else
                xByte := xByte - Ord ('1') + Ord ('A');
            end;
            aBuffer [xSignByteIndex] := AnsiChar (xByte);
          end; // Signed or Negative
        end; // ibm zOs
      end; // sign not separate
    end; // Numeric Display
  end // Numeric
  else
  begin // String
    if Length (aValue) > Bytes then
    begin
      raise Exception.Create ( CRLF + 'Value to long for buffer'
                             + CRLF + 'Buffer size: ' + IntToStr (Bytes)
                             + CRLF + 'Value length: ' + IntToStr (Length (aValue))
                             + CRLF + 'Value: ' + aValue
                             );
    end;
    SetLength (aBuffer, Bytes);
    x := 1;
    while (x <= Bytes)
    and (x <= Length (aValue))
    do begin
      aBuffer [x] := aValue [x];
      Inc (x);
    end;
    while (x <= Bytes) do
    begin
      aBuffer [x] := ' ';
      Inc (x);
    end;
  end; // string

  result := aBuffer;
end;

function TIpmItem.ValuesToBuffer (OnFoundError: TOnFoundError): AnsiString;
  procedure _ValuesToBuffer (aItem: TIpmItem; BaseOffset: Integer; var aBuffer: AnsiString);
  var
    x: Integer;
    xItemList: TIpmItemList;
    s: AnsiString;
  begin
    if not aItem.Group then
    begin
      try
        s := aItem.ValueToBuffer(aItem.Value);
        x := 1;
        while (x <= aItem.Bytes)
        and (x <= Length (s))
        do begin
          aBuffer [aItem.Offset - BaseOffset + x] := s [x];
          Inc (x);
        end;
      except
        on E: Exception do
        begin
          if Assigned (OnFoundError) then
            OnFoundError (E.Message, aItem)
          else
            raise Exception.Create(aItem.FullCaption + ': ' + e.Message);
        end;
      end;
    end
    else
    begin
      xItemList := aItem.Items as TIpmItemList;
      for x := 0 to xItemList.Count - 1 do
      begin
        _ValuesToBuffer (xItemList.IpmItems [x], BaseOffset, aBuffer);
      end;
    end;
  end;
var
  x: Integer;
  xBuffer: AnsiString;
begin
  if Self = nil then Exit;
  SetLength (xBuffer, Bytes);
  for x := 1 to Bytes do
    xBuffer [x] := ' ';
  _ValuesToBuffer (self, Offset, xBuffer);
  result := xBuffer;
end;

procedure TIpmItem._BuildXML (OnHaveString: TOnHaveString; OnlyWhenFilled: Boolean);
var
  x: Integer;
  xSubItems: TIpmItemList;
  TagName: String;
  function _ValueToXml (aValue: String): String;
  var
    x: Integer;
  begin
    result := '';
    for x := 1 to Length (aValue) do
    begin
      case aValue [x] of
        '<': result := result + '&lt;';
        '>': result := result + '&gt;';
        '&': result := result + '&amp;';
        '''': result := result + '&apos;';
        '"': result := result + '&quot;';
      {
        'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', ' ':
          result := result + aValue [x];
      }
      else
        if aValue [x] < ' ' then
          result := result + '&%' + SysUtils.IntToHex(Ord(aValue [x]), 2) + ';'
        else
          result := result + aValue [x];
      end;
    end;
  end;
begin
  if SkipOnXmlBuild then
    exit;
  if (OnlyWhenFilled)
  and (not Filled) then
    exit;
  if Group = True then
  begin
    xSubItems := Items as TIpmItemList;
    if (Uppercase (XmlConvention) = 'RNIBS')
    and (xSubItems.Count = 1)
    and (not xSubItems.IpmItems [0].Group)
    and (UpperCase (RightStr (xSubItems.IpmItems [0].Name, 4)) = '-VAL')
    then
    begin
      TagName := xSubItems.IpmItems [0].Name;
      xSubItems.IpmItems [0].Name := Name;
      xSubItems.IpmItems [0]._BuildXML (OnHaveString, OnlyWhenFilled);
      xSubItems.IpmItems [0].Name := TagName;
    end
    else
    begin
      OnHaveString ( IndentString (Indent)
                   + '<'
                   + TagToXml (Name)
                   + '>'
                   );
      Indent := Indent + 2;
      for x := 0 to xSubItems.Count - 1 do
      begin
        xSubItems.IpmItems [x]._BuildXML (OnHaveString, OnlyWhenFilled);
      end;
      Indent := Indent - 2;
      OnHaveString ( IndentString (Indent)
                   + '</'
                   + TagToXml (Name)
                   + '>'
                   );
    end;
  end
  else
  begin
    OnHaveString ( IndentString (Indent)
                 + '<'
                 + TagToXml (Name)
                 + '>'
                 + _ValueToXml (Value)
                 + '</'
                 + TagToXml (Name)
                 + '>'
                 );
  end;
end;

procedure TIpmItem.Bind (aRoot: String; aExpress: TObject; aMaxOccurs: Integer);
var
  x: Integer;
  xSubItems: TIpmItemList;
  xExpress: TExpress;
begin
{
  if Occurs > 1 then  // no reecurring items in script
    exit;
}
  if Occurrence > aMaxOccurs then
    exit;
  xExpress := aExpress as TExpress;
  if Group = True then
  begin
    xExpress.BindGroupObject (aRoot + '.' + FullCaption, Self);
    xSubItems := Items as TIpmItemList;
    for x := 0 to xSubItems.Count - 1 do
    begin
      xSubItems.IpmItems [x].Bind (aRoot, xExpress, aMaxOccurs);
    end;
  end
  else
  begin
    if UpperCase (Caption) <> 'FILLER' then
    begin
      if Numeric then
        xExpress.BindExtendedObject (aRoot + '.' + FullCaption, Self)
      else
        xExpress.BindStringObject (aRoot + '.' + FullCaption, Self);
    end;
  end;
end;

procedure TIpmItem.BuildXML (OnHaveString: TOnHaveString; aIndent: Integer; OnlyWhenFilled: Boolean);
begin
  Indent := aIndent;
  SetFilled;
  Filled := True;
  _BuildXML (OnHaveString, OnlyWhenFilled);
end;

procedure TIpmItem.BuildCobolWS(OnHaveString: TOnHaveString);
var
  ExpectedOffset: Integer;
  function _Occurs (aIpm: TIpmItem): String;
  begin
    if (aIpm.Occurs > 1) then
      result := IntToStr (aIpm.Occurrence)
    else
      result := '';
  end;
  procedure _BuildCobolWS ( aIpm: TIpmItem
                          ; aIndent: Integer
                          );
  var
    x: Integer;
  begin
    if aIpm.Offset > ExpectedOffset then
    begin
      OnHaveString ( IndentString (aIndent)
                   + '0'
                   + IntToStr (aIpm.Level)
                   + ' filler pic x('
                   + IntToStr (aIpm.Offset - ExpectedOffset)
                   + ').'
                   );
      ExpectedOffset := aIpm.Offset;
    end;
    if aIpm.Group then
    begin
      OnHaveString ( IndentString (aIndent)
                   + '0'
                   + IntToStr (aIpm.Level)
                   + ' '
                   + aIpm.Name
                   + _Occurs (aIpm)
                   + '.'
                   );
      for x := 0 to aIpm.Items.Count - 1 do
        _BuildCobolWS ( aIpm.Items.IpmItems [x]
                      , aIndent + 3
                      );
    end
    else
    begin
      OnHaveString ( IndentString (aIndent)
                   + '0'
                   + IntToStr (aIpm.Level)
                   + ' '
                   + aIpm.Name
                   + _Occurs (aIpm)
                   + ' '
                   + 'pic '
                   + aIpm.PictureCaption
                   + ' value '
                   + aIpm.CobolValue
                   + '.'
                   );
      ExpectedOffset := aIpm.Offset + aIpm.Bytes;
    end;
  end;
begin
  ExpectedOffset := Offset;
  _BuildCobolWS (Self, 1);
end;

constructor TIpmItem.Create;
begin
  inherited Create;
  Level88Values := TStringList.Create;
  Level88Values.Sorted := True;
  Level88Values.Duplicates := dupIgnore;
  SkipOnXmlBuild := False;
  Items := TIpmItemList.Create;
end;

destructor TIpmItem.Destroy;
var
  x: Integer;
  xIpmItemList: TIpmItemList;
begin
  xIpmItemList := Items as TIpmItemList;
  for x := 0 to xIpmItemList.Count - 1 do
  begin
    xIpmItemList.IpmItems [x].Free;
  end;
  xIpmItemList.Clear;
  xIpmItemList.Free;
  Level88Values.Clear;
  Level88Values.Free;
  inherited Destroy;
end;

procedure TIpmItemList.AddAliasFieldIpmItems(aAlias: String; aIpmItem: TIpmItem);
var
  x: Integer;
begin
  if aIpmItem.Group then
  begin
    for x := 0 to aIpmItem.Items.Count - 1 do
      AddAliasFieldIpmItems (aAlias, aIpmItem.Items.IpmItems [x]);
  end
  else
    AddObject ( aAlias
              + '.'
              + aIpmItem.FullCaption
              , aIpmItem
              );
end;

function TIpmItemList.GetIpmItem (Index: integer): TIpmItem;
begin
  result := TIpmItem (Objects [index]);
end;

procedure TIpmItemList.SetIpmItem(Index: integer; const Value: TIpmItem);
begin
  Objects [index] := Value;
end;

function TIpmItem.IndentString(x: Integer): String;
begin
  result := '';
  while x > 0 do
  begin
    result := result + ' ';
    Dec (x);
  end;
end;

function TIpmItem.TagToXml (aValue: String): String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (aValue) do
  begin
    case aValue [x] of
      '_':
         begin
           if XmlTagsHyphen = xmlTHHyphen then
             result := result + '-'
           else
             result := result + '_';
         end;
      '-':
         begin
           if XmlTagsHyphen = xmlTHReplaceBy then
             result := result + XmlReplaceHyphenBy
           else
             result := result + '-';
         end;
    else
      result := result + aValue [x];
    end;
  end;
  case XmlTagsCase of
    xmlTCLowerCase: result := LowerCase(result);
    xmlTCUpperCase: result := UpperCase(result);
  end;
end;

procedure TIpmItem._BuildXSDLocal (OnHaveString: TOnHaveString; aIndent: Integer);
var
  Str: String;
  SaveName: String;
  x: Integer;
  xSubItems: TIpmItemList;

begin
  if Uppercase (Name) = 'FILLER' then
    exit;
  if Group then
  begin
    xSubItems := Items as TIpmItemList;
    if (xSubItems.Count = 1)
    and (not xSubItems.IpmItems [0].Group)
    and (UpperCase (RightStr (xSubItems.IpmItems [0].Name, 4)) = '-VAL')
    then
    begin
      SaveName := xSubItems.IpmItems [0].Name;
      xSubItems.IpmItems [0].Name := Name;
      xSubItems.IpmItems [0]._BuildXSDLocal (OnHaveString, aIndent);
      xSubItems.IpmItems [0].Name := SaveName;
    end
    else
    begin
      OnHaveString ( IndentString (aIndent)
                   + '<xs:element name="'
                   + TagToXml (Name)
                   + '"'
                   + GetXsdOccurs
                   + '>'
                   );
      OnHaveString ( IndentString (aIndent)
                   + '  <xs:complexType>'
                   );
      OnHaveString ( IndentString (aIndent)
                   + '    <xs:sequence>'
                   );
      for x := 0 to xSubItems.Count - 1 do
      begin
        xSubItems.IpmItems [x]._BuildXSDLocal (OnHaveString, aIndent + 6);
      end;
      OnHaveString ( IndentString (aIndent)
                   + '    </xs:sequence>'
                   );
      OnHaveString ( IndentString (aIndent)
                   + '  </xs:complexType>'
                   );
      OnHaveString ( IndentString (aIndent)
                   + '</xs:element>'
                   );
    end
  end
  else
  begin
    DoExtendPictureClause;
    OnHaveString ( IndentString (aIndent)
                 + '<xs:element name="'
                 + TagToXml (Name)
                 + '"'
                 + GetXsdOccurs
                 + '>'
                 );
    OnHaveString (IndentString (aIndent + 2) + '<xs:simpleType>');
    if Numeric then
    begin
      if Precision = 0 then
      begin
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:restriction base="xs:integer">'
                     );
        OnHaveString ( IndentString (aIndent + 6)
                     + '<xs:minInclusive value="'
                     + minInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 6)
                     + '<xs:maxInclusive value="'
                     + maxInclusive
                     + '"/>'
                     );
      end
      else
      begin
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:restriction base="xs:decimal">'
                     );
        OnHaveString ( IndentString (aIndent + 6)
                     + '<xs:minInclusive value="'
                     + minInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 6)
                     + '<xs:maxInclusive value="'
                     + maxInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 6)
                     + '<xs:fractionDigits value="'
                     + IntToStr (Precision)
                     + '"/>'
                     );
      end;
    end
    else
    begin
      OnHaveString ( IndentString (aIndent + 4)
                   + '<xs:restriction base="xs:string">'
                   );
      OnHaveString ( IndentString (aIndent + 6)
                   + '<xs:maxLength value="'
                   + IntToStr (Bytes)
                   + '"/>'
                   );
    end;
    OnHaveString ( IndentString (aIndent + 4)
                 + '</xs:restriction>'
                 );
    OnHaveString (IndentString (aIndent + 2) + '</xs:simpleType>');
    OnHaveString (IndentString (aIndent) + '</xs:element>');
  end;
end;

procedure TIpmItem.BuildXSDLocal (OnHaveString: TOnHaveString; aIndent: Integer);
begin
  OnHaveString ( IndentString (Indent)
               + '<?xml version="1.0" encoding="UTF-8" ?>'
               );
  OnHaveString ( IndentString (Indent)
               + '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">'
               );
  _BuildXSDLocal (OnHaveString, aIndent + 2);
  OnHaveString ( IndentString (Indent)
               + '</xs:schema>'
               );
end;

{ TIpmDescrList }

procedure TIpmDescrs.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    IpmDescrs [x].Free;
  RecordDescrTypeValues.RowCount := 0;
  RecordDescrTypeValues.ColCount := 0;
  RecordDescrTypeAliasses.Clear;
  RecordDescrTypeBinds.Clear;
  IpmIndex := -1;
  inherited;
end;

constructor TIpmDescrs.Create;
begin
  inherited;
  IpmIndex := -1;
  Sorted := True;
  Duplicates := dupError;
  fRecordDescrTypeBinds := TBindableList.Create;
  RecordDescrTypeValues := TStringListList.Create;
  RecordDescrTypeAliasses := TStringList.Create;
end;

destructor TIpmDescrs.Destroy;
begin
  fRecordDescrTypeBinds.Clear;
  fRecordDescrTypeBinds.Free;
  RecordDescrTypeValues.Free;
  RecordDescrTypeAliasses.Free;
  inherited;
end;

function TIpmDescrs.FindIpmItem(aName: String): TIpmItem;
var
  x: Integer;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    result := IpmDescrs [x].IpmItem.FindIpmItem (aName);
    if result <> nil then
      exit;
  end;
end;

function TIpmDescrs.GetIpmDescr(Index: integer): TIpmDescr;
begin
  result := TIpmDescr (Objects [index]);
end;

{ TIpmDescr }

constructor TIpmDescr.Create;
begin
  inherited;
  FileContents := TStringList.Create;
  Replacing := TStringList.Create;
  IpmItem := TIpmItem.Create;
  Recogs := TStringList.Create;
end;

destructor TIpmDescr.Destroy;
var
  x: Integer;
begin
  FileContents.Free;
  FreeAndNil (IpmItem);
  Replacing.Free;
  for x := 0 to Recogs.Count - 1 do
    (Recogs.Objects[x] as TRecog).Free;
  Recogs.Clear;
  inherited;
end;

procedure TIpmItem.ResetLoaded(aKeepValues: Boolean);
var
  x: Integer;
begin
  Loaded := False;
  HasUnExpectedValue := False;
  if not aKeepValues then
    Value := '';
  for x := 0 to (Items as TIpmItemList).Count - 1 do
  begin
    (Items as TIpmItemList).IpmItems [x].ResetLoaded (aKeepValues);
  end;
end;

procedure TIpmItem.setCobolEnvironment(const Value: TCobolEnvironmentType);
  procedure _set (aIpm: TIpmItem);
  var
    x: Integer;
  begin
    aIpm.fCobolEnvironment := Value;
    for x := 0 to aIpm.Items.Count - 1 do
      _set(aIpm.Items.IpmItems[x]); // applies to whole downline
  end;
begin
  _set (self);
  CalcIpmOffsets;
end;

function TIpmItem.SetFilled: Boolean;
var
  x: Integer;
begin
  if Group then
  begin
    Filled := False;
    for x := 0 to (Items as TIpmItemList).Count - 1 do
    begin
      if (Items as TIpmItemList).IpmItems [x].SetFilled then
        Filled := True;
    end;
  end
  else
  begin
    if Numeric then
      Filled := (StrToFloatDef(Value, 0) <> 0)
    else
      Filled := (Value <> '');
  end;
  result := Filled;
end;

procedure TIpmItem.LoadValues(aXml: TXml);
var
  x: Integer;
  y: Integer;
  xXml: TXml;
  xIpmItem: TIpmItem;
  function _PrepStr (aString: String): String;
  var
    x: Integer;
  begin
    case XmlTagsHyphen of
    xmlTHDontChange: result := aString;
    xmlTHHyphen:
      begin
        result := '';
        for x := 1 to system.Length (aString) do
        begin
          case aString [x] of
            '_': result := result + '-';
          else
            result := result + aString [x];
          end;
        end;
      end; {case Hyphen}
    xmlTHReplaceBy:
      begin
        result := '';
        for x := 1 to system.Length (aString) do
        begin
          case aString [x] of
            '-': result := result + XmlReplaceHyphenBy;
          else
            result := result + aString [x];
          end;
        end;
      end; {case Hyphen}
    end;
    result := LowerCase(result);
  end;
begin
  if aXml = nil then
    raise Exception.Create ( 'Not valid XML data '
                           );
  if (_PrepStr (aXml.TagName) <> _PrepStr (Name)) then
    raise Exception.Create ( 'Load mismatch: '
                           + 'IpmItem= '
                           + Name
                           + ' XmlTag= '
                           + aXml.TagName
                           );
  if (aXml.Group <> Group) then
  begin
    if (Uppercase (XmlConvention) = 'RNIBS')
    and (aXml.Group = False)
    and ((Items as TIpmItemList).Count = 1)
    and (not (Items as TIpmItemList).IpmItems [0].Group)
    and (UpperCase (RightStr ((Items as TIpmItemList).IpmItems [0].Name, 4)) = '-VAL')
    then
    begin
      (Items as TIpmItemList).IpmItems [0].Value := aXml.Value;
      if (Items as TIpmItemList).IpmItems [0].DoExpectValue then
        if (Items as TIpmItemList).IpmItems [0].ExpectedValue
        <> (Items as TIpmItemList).IpmItems [0].Value
        then
          (Items as TIpmItemList).IpmItems [0].HasUnExpectedValue := True;
      (Items as TIpmItemList).IpmItems [0].Loaded := True;
    end
    else
{}{
      raise Exception.Create ( 'Mismatch on grouping for item: '
                             + 'IpmItem= '
                             + Name
                             + ' and XML Data Tag= '
                             + aXml.TagName
                             );
{}
  end {if aXml.Group <> aIpmItem.Group}
  else
  begin
    if aXml.Group = False then
    begin
      Value := aXml.Value;
      if DoExpectValue then
        if Value <> ExpectedValue then
          HasUnExpectedValue := True;
    end
    else
    begin
      for x := 0 to (aXml.Items as TXmlList).Count - 1 do
      begin
        xXml := (aXml.Items as TXmlList).XmlItems [x];
        y := 0;
        xIpmItem := nil;
        while (y < (Items as TIpmItemList).Count)
        and (xIpmItem = nil)
        do begin
          if (_PrepStr ((Items as TIpmItemList).IpmItems [y].Name) = _PrepStr (xXml.TagName))
          and ((Items as TIpmItemList).IpmItems [y].Loaded = False)
          then
            xIpmItem := (Items as TIpmItemList).IpmItems [y];
          Inc (y);
        end;
        if xIpmItem <> nil then
          xIpmItem.LoadValues (xXml);
      end; {for every xml.item}
    end; {else aXml.Group = False}
  end; {else aXml.Group <> aIpmItem.Group}
  Loaded := True;
end;

function TIpmDescr.LoadFromFile(aFileName: String; ErrorFound: TOnErrorEvent): Boolean;
var
  IpmAnalyser: TIpmAnalyser;
  saveParseFileName: String;
begin
  result := False;
  IpmAnalyser := TIpmAnalyser.Create (nil);
  IpmAnalyser.StartState := InitState;
  IpmAnalyser.OnNeedData := AnalyserNeedData;
  IpmAnalyser.OnError := ErrorFound;
  IpmItem.Free;
  IpmItem := nil;
  saveParseFileName := _ParseFileName;
  _ParseFileName := uncFilename(aFileName);
  try
    FileName := aFileName;
    FileContents.LoadFromFile (aFileName);
    DoReplacing;
    LineNumber := 0;
    IpmAnalyser.Prepare;
    LineNumber := 0;
    IpmAnalyser.Execute;
    if BaseIpmItem <> nil then
    begin
      IpmItem := BaseIpmItem.CreatePhysicalIpm (cobolEnvironment);
      Alias := IpmItem.Name;
      result := True;
    end;
  finally
    IpmAnalyser.Free;
    BaseIpmItem.Free;
    _ParseFileName := saveParseFileName;
  end;
end;

procedure TIpmDescr.AnalyserNeedData( Sender: TObject
                          ; var MoreData: Boolean
                          ; var Data: String
                          );
begin
  if LineNumber = FileContents.Count then
    MoreData := False
  else
  begin
    Data := FileContents.Strings [LineNumber];
    Inc (LineNumber);
  end;
end;



procedure TIpmDescr.DoReplacing;
var
  ReplaceOld: String;
  ReplaceNew: String;
  NewString: String;
  x: Integer;
  p: Integer;
begin
  for x := 0 to Replacing.Count - 1 do
  begin
    p := Pos ('=', Replacing.Strings [x]);
    ReplaceOld := LeftStr (Replacing.Strings [x], p - 1);
    ReplaceNew := RightStr (Replacing.Strings [x], Length (Replacing.Strings [x]) - p);
    NewString :=
      StringReplace ( FileContents.Text
                    , ReplaceOld
                    , ReplaceNew
                    , [rfReplaceAll, rfIgnoreCase]
                    );
    FileContents.Text := NewString;
  end;
end;

function TIpmItem.GetSoapHeaderItem: TIpmItem;
var
  xIpmList: TIpmItemList;
begin
  xIpmList := Items as TIpmItemList;
  if xIpmList.Count = 0 then
    result := nil
  else
    result := xIpmList.IpmItems [0];
end;

function TIpmItem.GetSoapBodyItem: TIpmItem;
var
  xIpmList: TIpmItemList;
begin
  xIpmList := Items as TIpmItemList;
  if xIpmList.Count < 2 then
    result := nil
  else
    result := xIpmList.IpmItems [1];
end;

function TIpmDescr.GetReplacingAsQString: String;
var
  x: Integer;
begin
  result := '';
  for x := 0 to Replacing.Count - 1 do
    result := result + Replacing.Strings [x] + ';';
end;

procedure TIpmDescr.SetReplacingAsQString(const Value: String);
var
  x: Integer;
  xString: String;
begin
  Replacing.Clear;
  xString := '';
  for x := 1 to length (Value) do
  begin
    if Value [x] = ';' then
    begin
      Replacing.Add(xString);
      xString := '';
    end
    else
      xString := xString + Value [x];
  end;
end;

{$ifdef XMLDOM}
function TIpmDescr.LoadFromXMLSchema(aFileName: String;
  ErrorFound: TOnErrorEvent): Boolean;
var
  SchemaDef: IXMLSchemaDef;
  x: Integer;
begin
  result := False;
  IpmItem.Free;
  IpmItem := nil;
  try
    FileName := aFileName;
    FileContents.LoadFromFile (aFileName);
    SchemaDef := LoadXMLSchema(aFileName).SchemaDef;
    BaseIpmItem := TIpmItem.Create;
{
    for x := 0 to SchemaDef.ElementDefs.Count - 1 do
      AddElement (nil, SchemaDef.ElementDefs.Items [x]);
}
    if BaseIpmItem <> nil then
    begin
      IpmItem := BaseIpmItem.CreatePhysicalIpm(cobolEnvironment);
      Alias := IpmItem.Name;
      result := True;
    end;
  finally
  end;
end;
{$endif}

procedure TIpmDescr.setCobolEnvironment(const Value: TCobolEnvironmentType);
var
  x: Integer;
begin
  fCobolEnvironment := Value;
  IpmItem.CobolEnvironment := Value;
end;

procedure TIpmItem.BuildXSDGlobal(OnHaveString: TOnHaveString;
  aIndent: Integer);
begin
  TypeDefs := TStringList.Create;
  try
    TypeDefs.Sorted := True;
    OnHaveString ( IndentString (Indent)
                 + '<?xml version="1.0" encoding="UTF-8" ?>'
                 );
    OnHaveString ( IndentString (Indent)
                 + '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">'
                 );
    _BuildXSDGlobalSimpleTypedefs (OnHaveString, aIndent + 2, TypeDefs);
    _BuildXSDGlobalComplexTypedefs (OnHaveString, aIndent + 2, TypeDefs);
    OnHaveString ( IndentString (Indent + 2)
                 + '<xs:element name="'
                 + TagToXml (Name)
                 + '" type="'
                 + TagToXml (Name)
                 + '"/>'
                 );
    OnHaveString ( IndentString (Indent)
                 + '</xs:schema>'
                 );
  finally
    TypeDefs.Clear;
    TypeDefs.Free;
  end;
end;

procedure TIpmItem._BuildXSDGlobalSimpleTypedefs(OnHaveString: TOnHaveString;
  aIndent: Integer; aTypeDefs: TStringList);
var
  Str: String;
  SaveName: String;
  x: Integer;
  xSubItems: TIpmItemList;
  xTypeDefIndex: Integer;
begin
  if Uppercase (Name) = 'FILLER' then
    exit; {no typedefs for cobol fillers}
  if (aTypeDefs.Find(TagToXml (Name), xTypeDefIndex)) then
    exit; {already defined}
  if Group then
  begin
    xSubItems := Items as TIpmItemList;
    if (xSubItems.Count = 1)
    and (not xSubItems.IpmItems [0].Group)
    and (UpperCase (RightStr (xSubItems.IpmItems [0].Name, 4)) = '-VAL')
    then
    begin
      SaveName := xSubItems.IpmItems [0].Name;
      xSubItems.IpmItems [0].Name := Name;
      xSubItems.IpmItems [0]._BuildXSDGlobalSimpleTypedefs (OnHaveString, aIndent, aTypeDefs);
      xSubItems.IpmItems [0].Name := SaveName;
    end
    else
    begin
      for x := 0 to xSubItems.Count - 1 do
      begin
        xSubItems.IpmItems [x]._BuildXSDGlobalSimpleTypedefs (OnHaveString, aIndent, aTypeDefs);
      end;
    end
  end
  else
  begin
    DoExtendPictureClause;
    OnHaveString ( IndentString (aIndent)
                 + '<xs:simpleType name="'
                 + TagToXml (Name)
                 + '"'
                 + '>'
                 );
    if Numeric then
    begin
      if Precision = 0 then
      begin
        OnHaveString ( IndentString (aIndent + 2)
                     + '<xs:restriction base="xs:integer">'
                     );
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:minInclusive value="'
                     + minInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:maxInclusive value="'
                     + maxInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 2)
                     + '</xs:restriction>'
                     );
      end
      else
      begin
        OnHaveString ( IndentString (aIndent + 2)
                     + '<xs:restriction base="xs:decimal">'
                     );
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:minInclusive value="'
                     + minInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:maxInclusive value="'
                     + maxInclusive
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 4)
                     + '<xs:fractionDigits value="'
                     + IntToStr (Precision)
                     + '"/>'
                     );
        OnHaveString ( IndentString (aIndent + 2)
                     + '</xs:restriction>'
                     );
      end;
    end
    else
    begin
      OnHaveString ( IndentString (aIndent + 2)
                   + '<xs:restriction base="xs:string">'
                   );
      OnHaveString ( IndentString (aIndent + 4)
                   + '<xs:maxLength value="'
                   + IntToStr (Bytes)
                   + '"/>'
                   );
      OnHaveString ( IndentString (aIndent + 2)
                   + '</xs:restriction>'
                   );

    end;
    OnHaveString ( IndentString (aIndent)
                 + '</xs:simpleType>'
                 );
    aTypeDefs.Add(TagToXml (Name));
  end;
end;

procedure TIpmItem._BuildXSDGlobalComplexTypedefs(OnHaveString: TOnHaveString;
  aIndent: Integer; aTypeDefs: TStringList);
var
  Str: String;
  SaveName: String;
  x: Integer;
  xSubItem: TIpmItem;
  xTypeDefIndex: Integer;
begin
  if not Group then
    exit; {already typedefed, test not really nessecary, already defined}
  if Uppercase (Name) = 'FILLER' then
    exit; {no typedefs for cobol fillers}
  if (aTypeDefs.Find(TagToXml (Name), xTypeDefIndex)) then
    exit; {already defined}
  aTypeDefs.Add(TagToXml (Name));
  for x := 0 to Items.Count - 1 do
  begin
    Items.IpmItems [x]._BuildXSDGlobalComplexTypedefs (OnHaveString, aIndent, aTypeDefs);
  end;
  OnHaveString ( IndentString (aIndent)
               + '<xs:complexType name="'
               + TagToXml (Name)
               + '"'
               + '>'
               );
  OnHaveString ( IndentString (aIndent + 2)
               + '<xs:sequence>'
               );
  for x := 0 to Items.Count - 1 do
  begin
    if Uppercase (Items.IpmItems [x].Name) <> 'FILLER' then
    begin
      OnHaveString ( IndentString (aIndent + 4)
                   + '<xs:element name="'
                   + TagToXml (Items.IpmItems [x].Name)
                   + '" type="'
                   + TagToXml (Items.IpmItems [x].Name)
                   + '"'
                   + Items.IpmItems [x].GetXsdOccurs
                   + '/>'
                   );
    end;
  end;
  OnHaveString ( IndentString (aIndent + 2)
               + '</xs:sequence>'
               );
  OnHaveString ( IndentString (aIndent)
               + '</xs:complexType>'
               );
end;

function TIpmItem.GetXsdOccurs: String;
begin
  result := '';
  if minOccurs <> 1 then
    Result := Result + ' minOccurs="' + IntToStr (minOccurs) + '"';
  if Occurs > 1 then
    Result := Result + ' maxOccurs="' + IntToStr (Occurs) + '"';
end;

function TIpmItem.GetMaxInclusive: String;
var
  x: Integer;
begin
  result := '';
  for x := 1 to Length (ExtendedPictureClause) do
  begin
    case ExtendedPictureClause [x] of
      'S', 's': ;
      'V', 'v': result := result + '.';
      else result := result + ExtendedPictureClause [x];
    end;
  end;
end;

function TIpmItem.GetMinInclusive: String;
var
  x: Integer;
begin
  result := '';
  if ExtendedPictureClause [1] = '9' then
    result := '0'
  else
  begin
    for x := 1 to Length (ExtendedPictureClause) do
    begin
      case ExtendedPictureClause [x] of
        'S', 's': result := result + '-';
        'V', 'v': result := result + '.';
        else result := result + ExtendedPictureClause [x];
      end;
    end;
  end;
end;

function TIpmItem.FindIpmItem(aName: String): TIpmItem;
var
  x: Integer;
  y: Integer;
  xName: String;
  newName: String;
begin
  result := nil;
  x := Pos ('.', aName);
  if x = 0 then
    xName := aName
  else
    xName := Copy (aName, 1, x - 1);
  if xName = Caption then
  begin
    if x = 0 then
      result := self
    else
    begin
      newName := Copy(aName, x + 1, Length (aName));
      for y := 0 to Items.Count - 1 do
      begin
        result := Items.IpmItems [y].FindIpmItem(newName);
        if result <> nil then
          exit;
      end;
    end;
  end;
end;

procedure TIpmItem.Int64ToValue(aInteger: Int64);
var
  SignString: String;
  xInteger: Int64;
  rs: String;
begin
  if Precision = 0 then //simple in this case
    Value := IntToStr (aInteger)
  else
  begin
    if aInteger < 0 then
    begin
      xInteger := -1 * aInteger;
      SignString := '-';
    end
    else
    begin
      xInteger := aInteger;
      SignString := '';
    end;
    rs := IntToStr (xInteger);
    if Length (rs) <= Precision then
      rs := LeftStr ( '00000000000000000000000000000000000000000000000'
                    , Precision - Length (rs) + 1
                    )
          + rs;
    Value := SignString
           + LeftStr (rs, Length (rs) - Precision)
           + DecimalSeparator
           + RightStr (rs, Precision)
           ;
  end;
end;

function TIpmItem.Int64FromValue (aValue: String): Int64;
var
  x: Integer;
  xv: String;
  xPrec: Integer;
  xSepUsed: Boolean;
begin
  if aValue = '' then
  begin
    result := 0;
    exit;
  end;
  if Precision = 0 then // simple
  begin
    result := StrToInt64 (aValue);
    exit;
  end;
  {
    a string like 1.2 will become
    case Precision = 1: 12
    case Precision = 2: 120
    ..
    a string like 1.23 will become
    case Precision = 1: 12 - an error is raised
    case Precision = 2: 123

  }
  xv := '';
  xSepUsed := False;
  xPrec := 100000;
  for x := 1 to Length (aValue) do
  begin
    if aValue [x] = DecimalSeparator then
    begin
      xSepUsed := True;
      xPrec := Precision;
    end
    else
    begin
      if xPrec > 0 then
        xv := xv + aValue [x] // truncate to many digits behind dec.sep
      else
        raise Exception.Create ('To many digits behind decimal separator');
      if aValue [x] <> '-' then // in case trailing sign used, possible???
        Dec (xPrec);
    end;
  end;
  if not xSepUsed then
    xPrec := Precision;
  if xPrec > 0 then
    xv := xv + LeftStr ('000000000000000000000000000000000000', xPrec);
  result := StrToInt64 (xv);
end;

function TIpmDescrs.getRecordDescrTypeIndex (aRecord: String): Integer;
  function _equalValues (aRow: Integer): Boolean;
  var
    c: Integer;
    xItem: TIpmItem;
  begin
    result := False;
    for c := 0 to RecordDescrTypeValues.ColCount - 1 do
    begin
      xItem := RecordDescrTypeBinds.Bindables [c] as TIpmItem;
      if xItem.Buffer(aRecord)
      <> xItem.ValueToBuffer(RecordDescrTypeValues.CellValue [c, aRow])
      then
        exit;
    end;
    result := True;
  end;
var
  x, a: Integer;
  xAlias: String;
begin
  result := 0;
  xAlias := RecordDescrTypeDefaultAlias;
  for x := 0 to RecordDescrTypeValues.RowCount - 1 do
  begin
{
    if RecordDescrTypeValues.RowText [x] = (BindValues + #$D#$A) then
}
    if _equalValues(x) then
    begin
      xAlias := RecordDescrTypeAliasses.Strings [x];
      break;
    end;
  end;
  for a := 0 to Count - 1 do
  begin
    if xAlias = Strings [a] then
      result := a;
  end;
end;

function TIpmDescrs.getSelectedIpmItem: TIpmItem;
begin
  if IpmIndex < 0 then
    result := nil
  else
    result := IpmDescrs [IpmIndex].IpmItem;
end;

procedure TIpmDescrs.setRecordDescrTypeDefaultAlias(const Value: String);
begin
  fRecordDescrTypeDefaultAlias := Value;
end;

procedure TIpmDescrs.setIpmIndex(const Value: Integer);
begin
  fIpmIndex := Value;
end;

function TIpmItem.getCobolValue: AnsiString;
  function _NumVal: AnsiString;
  begin
    if Value = '' then
      result := '0'
    else
      result := Value;
  end;
  function _StrVal: AnsiString;
  var
    x: Integer;
  begin
    if Value = '' then
      result := ' '
    else
    begin
      result := '';
      for x := 1 to Length (Value) do
        if Value [x] = '"' then
          result := result + '""'
        else
          result := result + Value [x];
    end;
  end;
begin
  if Numeric = False then
    result := '"' + _StrVal + '"'
  else
    result := _NumVal;
end;

function TIpmItem.getIsAlignedData: boolean;
begin
  if not Group then
    result := (Comp and (CobolEnvironment = ceTandem))
  else
    result := Items.IpmItems [0].IsAlignedData;
end;

procedure TIpmItem.GroupCopy (aIpm: TIpmItem; aSkipAssignments: Boolean);
var
  x: Integer;
  y: Integer;
  xIpm: TIpmItem; {for each item in a source group}
  yIpm: TIpmItem; {for item in destination group with corr. name and unloaded}
begin
  if self = nil then exit;
  if aIpm = nil then exit;
  if Group <> aIpm.Group then exit;
  if (Group = False) then
  begin
    if (   (not aSkipAssignments)
        or (Copy (Value, 1, 2) <> ':=')
       ) then
      Value := aIpm.Value;
  end
  else
  begin
    for x := 0 to aIpm.Items.Count - 1 do
    begin
      xIpm := aIpm.Items.IpmItems [x];
      y := 0;
      yIpm := nil;
      while (y < Items.Count)
      and (yIpm = nil)
      do begin
        if (Items.IpmItems [y].Name = xIpm.Name)
        and (Items.IpmItems [y].Loaded = False)
        then begin
          yIpm := Items.IpmItems [y];
        end;
        Inc (y);
      end;
      if yIpm <> nil then
        yIpm.GroupCopy (xIpm, aSkipAssignments)
    end; {for every Ipm.item}
  end; {else aIpm.Group = False}
  Loaded := True;
end;


procedure TIpmItem.PutGroupData (aObject: TObject);
var
  sIpm: TIpmItem;
begin
  ResetLoaded(True);
  GroupCopy (TIpmItem (aObject), True);
end;

procedure TIpmItem.PutStringData (aString: String);
var
  xBuffer: String;
  xOffset: Integer;
begin
  ValueToBuffer(aString);
  inherited;
end;

function TIpmItem.getRootIpmItem: TIpmItem;
begin
  if not Assigned (Parent) then
    result := self
  else
    result := (Parent as TipmItem).getRootIpmItem;
end;

procedure TIpmDescrs.setRecordDescrTypeBinds(const Value: TBindableList);
var
  x: Integer;
begin
  fRecordDescrTypeBinds.Text := Value.Text;
  for x := 0 to Value.Count - 1 do
    fRecordDescrTypeBinds.Bindables [x] := Value.Bindables [x];
end;

function TIpmDescr.DuplicateSelf: TIpmDescr;
begin
  result := TIpmdescr.Create;
  result.Alias := Alias;
  result.FileName := FileName;
  result.IpmItem := IpmItem.DuplicateSelf(nil);
  result.FileContents.Text := FileContents.Text;
  result.Replacing.Text := Replacing.Text;
end;

{ TStringListList }

function TStringListList.getCellValue(aCol, aRow: Integer): String;
begin
  result := StringLists [aRow].Strings [aCol];
end;

function TStringListList.getRowText(Index: integer): String;
begin
  result := StringLists[Index].Text;
end;

function TStringListList.GetStringList(Index: integer): TStringList;
begin
  try
    result := Objects [Index] as TStringList;
  except
    result := nil;
  end;
end;

procedure TStringListList.setCellValue(aCol, aRow: Integer;
  const Value: String);
begin
  StringLists [aRow].Strings [aCol] := Value;
end;

procedure TStringListList.setColCount(const Value: Integer);
var
  r, c: Integer;
  sl: TStringList;
begin
  for r := 0 to fRowCount - 1 do
  begin
    sl := StringLists [r];
    c := min (Value, fColCount);
    while (c < fColCount) do
    begin
      sl.Delete (Value);
      Inc (c);
    end;
    while (c < Value) do
    begin
      sl.Add ('');
      Inc (c);
    end;
  end;
  fColCount := Value;
end;

procedure TStringListList.setRowCount(const Value: Integer);
var
  r, c: Integer;
  sl: TStringList;
begin
  r := min (Value, fRowCount);
  while (r < fRowCount) do
  begin
    StringLists [Value].Free;
    Delete(Value);
    Inc (r);
  end;
  while (r < Value) do
  begin
    sl := TStringList.Create;
    for c := 0 to fColCount do
      sl.Add ('');
    AddObject('', sl);
    Inc (r);
  end;
  fRowCount := Value;
end;

procedure TStringListList.SetStringList(Index: integer;
  const Value: TStringList);
begin

end;

function TIpmItem.GetCaption: String;
begin
  result := Name;
  if Occurs > 1 then
    result := result + '[' + IntToStr (Occurrence) + ']';
end;

function TIpmItem.Children: TBindableList;
begin
  result := Items;
end;

function TIpmItem.GetFullCaption: String;
begin
  result := '';
  if Self = nil then
    exit;
  if Parent = nil then
    result := GetCaption
  else
    result := Parent.GetFullCaption
            + '.'
            + GetCaption
            ;
end;

function TIpmItem.GetFullIndexCaption: String;
begin
  result := GetFullCaption;
end;

function TIpmItem.GetIndexCaption: String;
begin
  result := GetCaption;
end;

function TIpmItem.AsXml: TXml;
var
  xsl: TStringList;
begin
  result := TXml.Create;
  xsl := sl;
  sl := TStringList.Create;
  try
    BuildXml  (HaveString, 0, True);
    result.LoadFromString (sl.Text, nil);
  finally
    sl.Free;
    sl := xsl;
  end;
end;

procedure TIpmItem.HaveString(aString: String);
begin
  sl.Add (aString);
end;

function TIpmItem.FindUQ(aName: String): TCustomBindable;
begin
  result := FindIpmItem (aName);
end;

procedure TIpmItem.Populate(aViewType: TxvViewType);
  procedure _populateValue (aIpm: TIpmItem);
  begin
    if (not aIpm.Group) then
    begin
      if (not aIpm.Numeric) then
        aIpm.Value := Copy (aIpm.Name, 1, aIpm.Bytes)
      else
      begin
        try
          aIpm.BufferToValues (nil, Copy ('12345678', 1, aIpm.Bytes));
          if aIpm.Value = '' then
            aIpm.value := '2';
        except
          aIpm.Value := '1';
        end;
      end;
    end;
  end;
  procedure _Populate (aIpm: TIpmItem);
  var
    x: Integer;
  begin
    if (aIpm.Value = '') then
    begin
      for x := 0 to aIpm.Items.Count - 1 do
        _Populate (aIpm.Items.IpmItems [x]);
      _populateValue (aIpm);
      aIpm.Checked := True;
    end;
  end;
var
  x: Integer;
begin
{
  for x := 0 to Attributes.Count - 1 do
    Attributes.XmlAttributes [x].Checked := False;
}
  if (Value = '') then
  begin
    for x := 0 to Items.Count - 1 do
      _Populate (Items.IpmItems [x]);
    _populateValue (Self);
  end;
end;

{$ifndef NoGUI}
function TIpmItem.bgColor (aReadOnly: Boolean; aColumn: Integer): TColor;
begin
  if aReadOnly or Group then
    result := clBtnFace
  else
    result := clWhite;
end;
{$endif}

procedure TIpmItem.LoadValues(aIpm: TIpmItem);
var
  x: Integer;
begin
  if not Assigned (Self) then Exit;
  Value := aIpm.Value;
  x := 0;
  while (x < Items.Count)
  and (x < aIpm.Items.Count) do
  begin
    Items.IpmItems [x].LoadValues (aIpm.Items.IpmItems [x]);
    Inc (x);
  end;
end;

function TIpmDescrs.FindIpmDescr(aString: String): TIpmDescr;
var
  x, y: Integer;
  xMatch: Boolean;
  xIpmDescr: TIpmDescr;
  yRecog: TRecog;
begin
  result := nil;
  for x := 0 to Count - 1 do
  begin
    xIpmDescr := IpmDescrs[x];
    xMatch := True;
    for y := 0 to xIpmDescr.Recogs.Count - 1 do
    begin
      yRecog := xIpmDescr.Recogs.Objects [y] as TRecog;
      xMatch := (    xMatch
                 and (Trim (Copy ( aString
                                 , yRecog.Start
                                 , yRecog.Length
                                 )
                           ) = yRecog.Value
                     )
                );
    end;
    if xMatch then
    begin
      result := xIpmDescr;
      exit; // leave on first match
    end;
  end;
end;

initialization
  ourEndianness := etLittleEndian;
  theirEndianness := etBigEndian;

end.
