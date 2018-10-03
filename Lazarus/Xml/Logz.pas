unit Logz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   , RegExpr
   , Bind
   , Xmlz
   , Ipmz
   , Wsdlz
   , Xsdz
   , igGlobals
   , ClaimListz
   , base64
   ;

type
  TCompareLogOrderBy = (clTimeStamp, clOperation, clCorrelation);
  TShowLogCobolStyle = (slCobol, slXml);
  TLogFilterStyle = (fsShowMatch, fsShowMismatch, fsHighlightMatch, fsHighlightMismatch);
  PDisplayRef = ^TObject;
  TOnEvent = procedure of Object;


type

  TLog = class;
  TLogList = class;

  { TLog }

  TLog = class(TClaimableObject)
  protected
  public
    doSuppressLog,doSuppressAsyncReply: Boolean;
    onSnapshot: Boolean;
    displayRef: PDisplayRef;
    DisplayedColumnsValid: Boolean;
    DisplayedColumns: TStringList;
    Nr: Integer;
    MessageId: String;
    InboundTimeStamp, OutBoundTimeStamp: TDateTime;
    TransportType: TTransportType;
    httpUri: String;
    httpResponseCode: Integer;
    httpCommand: String;
    httpDocument: String;
    httpParams: String;
    RequestContentType, ReplyContentType: String;
    DestinationIp: String;
    Stubbed: Boolean;
    CorrelationId: String;
    Operation: TWsdlOperation;
    Mssg: TWsdlMessage;
    Exception: String;
    Remarks: String;
    Notifications: String;
    BeforeScript: String;
    AfterScript: String;
    RequestHeaders: String;
    RequestBody: String;
    RequestBodyMiM: String;
    ReplyHeaders: String;
    ReplyBodyMiM: String;
    ReplyBody: String;
    InboundBody, OutboundBody: String;
    designSuspect: Boolean;
    StubAction: TStubAction;
    PassesFilter, ShowHighLighted: Boolean;
    RequestValidated: Boolean;
    RequestValidateResult: String;
    ReplyValidated: Boolean;
    ReplyValidateResult: String;
    ExpectedValuesChecked: Boolean;
    HasUnexpectedValue: Boolean;
    wsaCorrelated: Boolean;
    isAsynchronousRequest: Boolean;
    isAsynchronousReply: Boolean;
    Stream: TStream;
    markDeleted: Boolean;
    relatesTo: TLog;
    ServiceName, OperationName: String;
    DelayTimeMs, OperationCount: Integer;
    procedure AddRemark (aRemark: String);
    function CompareKey (aCompareBy: TCompareLogOrderBy; aSortColumns: TStringList): String;
    function DurationAsString: String;
    function StubActionAsString: String;
    function AsXml: TXml;
    function reqBodyAsXml: TXml;
    function rpyBodyAsXml: TXml;
    procedure FoundErrorInBuffer(ErrorString: String; aObject: TObject);
    procedure OpenApiRequestToBindables (aOperation: TWsdlOperation);
    procedure OpenApiReplyToBindables (aOperation: TWsdlOperation);
    procedure toBindables (aOperation: TWsdlOperation);
    procedure InitDisplayedColumns(aOperation: TWsdlOperation; aDisplayedLogColumns: TStringList);
    constructor Create;
    destructor Destroy; override;
  end;

  { TLogList }

  TLogList = class (TClaimableObjectList)
  private
    fNumber: Integer;
    procedure SetLog(Index: integer; const Value: TLog);
  protected
    function GetLog (Index: integer): TLog;
  public
    designSuspect: Boolean;
    property LogItems [Index: integer]: TLog read GetLog write SetLog;
    property Number: Integer read fNumber;
    function SaveLog (aString: String; aLog: TLog): TLog;
    function LogsAsString (aStubFileName: String): String;
    function UnexpectedsAsXml: TXml;
    function PrepareCoverageReportAsXml (aOperations: TWsdlOperations; ignoreCoverageOn: TStringList): TXmlCvrg;
    procedure InvalidateDisplayedColumns; overload;
    procedure InvalidateDisplayedColumns(aOperation: TWsdlOperation); overload;
    constructor Create; overload;
  end;

  TLogFilter = class(TOBject)
  private
    rx: TRegExpr;
  protected
  public
    Enabled: Boolean;
    FilterStyle: TLogFilterStyle;
    MatchAny: Boolean;
    StubActionEnabled: Boolean;
    StubActionEquals: Boolean;
    StubAction: TStubAction;
    MiMEnabled: Boolean;
    RequestMiMEnabled: Boolean;
    ReplyMiMEnabled: Boolean;
    MessageValidationEnabled: Boolean;
    RequestValidationEnabled: Boolean;
    ReplyValidationEnabled: Boolean;
    ExceptionEnabled: Boolean;
    ExceptionEquals: Boolean;
    ExceptionRegExp: Boolean;
    Exception: String;
    ServiceEnabled: Boolean;
    ServiceEquals: Boolean;
    ServiceRegExp: Boolean;
    Service: String;
    OperationEnabled: Boolean;
    OperationEquals: Boolean;
    OperationRegExp: Boolean;
    Operation: String;
    CorrelationEnabled: Boolean;
    CorrelationEquals: Boolean;
    CorrelationRegExp: Boolean;
    Correlation: String;
    RequestEnabled: Boolean;
    RequestEquals: Boolean;
    Request: String;
    ReplyEnabled: Boolean;
    ReplyEquals: Boolean;
    Reply: String;
    UnexpectedValuesEnabled: Boolean;
    RemarksEnabled: Boolean;
    procedure Execute (aLog: TLog);
    constructor Create;
    destructor Destroy; override;
  end;

function logDifferencesAsXml( aLogs, bLogs: TLogList
                            ; aReferenceFileName: String
                            ; aOrderBy: TCompareLogOrderBy
                            ; ignoreDifferencesOn, checkValueAgainst, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, sortColumns: TStringList
                            ): TXml;
function doOrder (List: TStringList; Index1, Index2: Integer): Integer;

implementation

uses SysUtils
   , a2bStringListUnit
   , A2BXmlz
   , IpmTypes
   , SwiftUnit
   , xmlxsdparser
   ;

function ifthen(val:boolean;const iftrue:String; const iffalse:String='') :String;
begin
  if val then result:=iftrue else result:=iffalse;
end;

function doOrder(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := 0;
  with (List as TLogList) do
  begin
    if Strings[Index1] > Strings[Index2] then
    begin
      result := 1;
      exit;
    end;
    if Strings[Index1] < Strings[Index2] then
    begin
      result := -1;
      exit;
    end;
    if LogItems[Index1].InboundTimeStamp > LogItems[Index2].InboundTimeStamp then
    begin
      result := 1;
      exit;
    end;
    if LogItems[Index1].InboundTimeStamp < LogItems[Index2].InboundTimeStamp then
    begin
      result := -1;
      exit;
    end;
  end;
end;

function logDifferencesAsXml( aLogs, bLogs: TLogList
                            ; aReferenceFileName: String
                            ; aOrderBy: TCompareLogOrderBy
                            ; ignoreDifferencesOn, checkValueAgainst, ignoreAddingOn, ignoreRemovingOn, ignoreOrderOn, sortColumns: TStringList
                            ): TXml;
  function _DetailXml (xLog: TLog): TXml;
  begin
    result := TXml.CreateAsString('Detail', '');
    result.AddXml (TXml.CreateAsString('messageTimestamp', xsdDateTime(xLog.InboundTimeStamp)));
    if Assigned (xLog.Operation) then
    begin
      result.AddXml (TXml.CreateAsString('Service', xLog.Operation.WsdlService.Name));
      result.AddXml (TXml.CreateAsString('Operation', xLog.Operation.Name));
    end;
    if Assigned (xLog.Mssg) then
      result.AddXml (TXml.CreateAsString('Message', xLog.Mssg.Name));
    result.AddXml (TXml.CreateAsString('Correlation', xLog.CorrelationId));
  end;
  function _aDetailXml (aLog: TLog): TXml;
  begin
    result := _DetailXml (aLog);
    result.AddXml (TXml.CreateAsString('Type', 'delete'));
  end;
  function _bDetailXml (bLog: TLog): TXml;
  begin
    result := _DetailXml (bLog);
    result.AddXml (TXml.CreateAsString('Type', 'add'));
  end;
  function _abDetailXml (aLog, bLog: TLog): TXml;
  begin
    result := _DetailXml (bLog);
    result.AddXml (TXml.CreateAsString('Type', 'change'));
  end;
  procedure _CompareAB(aLog, bLog: TLog; changesXml: TXml);
    procedure _addChanges(s: String; a2bXml: TA2bXml);
    var
      x: Integer;
    begin
      if (a2bXml.ChangeKind <> ckCopy)
      and (not a2bXml.Ignored) then
      begin
        with changesXml.AddXml(Txml.CreateAsString('Item', '')) do
        begin
          AddXml (TXml.CreateAsString('Tag', s + NameWithoutPrefix(a2bXml.Name)));
          case a2bXml.ChangeKind of
            ckAdd: AddXml (TXml.CreateAsString('Type', 'Add'));
            ckDelete: AddXml (TXml.CreateAsString('Type', 'Delete'));
            ckModify: AddXml (TXml.CreateAsString('Type', 'Modify'));
            ckCopy: raise Exception.Create ('?statement should not be reached?');
          end;
          AddXml (TXml.CreateAsString('currentValue', a2bXml.Value));
          AddXml (TXml.CreateAsString('referenceValue', a2bXml.bValue));
        end;
      end;
      for x := 0 to a2bXml.Items.Count - 1 do
        _addChanges (s + NameWithoutPrefix(a2bXml.Name) + '.', (a2bXml.Items.XmlItems[x] as TA2bXml));
    end;
  var
    aXml, bXml: TXml;
    a2bXml: TA2bXml;
  begin
    aXml := aLog.reqBodyAsXml;
    aXml.SeparateNsPrefixes;
    aXml.ResolveNameSpaces;
    a2bExpandWhenValueIsJsonOrYaml(aXml);
    bXml := bLog.reqBodyAsXml;
    bXml.SeparateNsPrefixes;
    bxml.ResolveNameSpaces;
    a2bExpandWhenValueIsJsonOrYaml(bXml);
    a2bXml := TA2BXml.CreateA2B(aLog.OperationName, '', aXml, bXml, ignoreOrderOn, checkValueAgainst);
    a2bXml.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
    _addChanges ('req.', a2bXml);
    FreeAndNil(a2bXml);
    FreeAndNil (aXml);
    FreeAndNil (bXml);
    aXml := aLog.rpyBodyAsXml;
    aXml.SeparateNsPrefixes;
    aXml.ResolveNameSpaces;
    a2bExpandWhenValueIsJsonOrYaml(aXml);
    bXml := bLog.rpyBodyAsXml;
    bXml.SeparateNsPrefixes;
    bxml.ResolveNameSpaces;
    a2bExpandWhenValueIsJsonOrYaml(bXml);
    a2bXml := TA2BXml.CreateA2B(aLog.OperationName, '', aXml, bXml, ignoreOrderOn, checkValueAgainst);
    a2bXml.Ignore(ignoreDifferencesOn, ignoreAddingOn, ignoreRemovingOn);
    _addChanges ('rpy.', a2bXml);
    FreeAndNil(a2bXml);
    FreeAndNil (aXml);
    FreeAndNil (bXml);
  end;
var
  x, a, b, c, i: Integer;
  LA, LB: TStringList;
  s: String;
  aSortedLogs, bSortedLogs: TLogList;
  headerXml, bodyXml, itemsXml: TXml;
  Diffs: TA2BStringList;
begin
  a2bInitialize;
  try
    aSortedLogs := TLogList.Create;
    aSortedLogs.Duplicates :=  dupAccept;
    bSortedLogs := TLogList.Create;
    bSortedLogs.Duplicates :=  dupAccept;
    result := TXml.CreateAsString('logDifferences', '');
    headerXml := result.AddXml(TXml.CreateAsString('Header', ''));
    try
      for x := 0 to aLogs.Count - 1 do
        if aLogs.LogItems [x].PassesFilter then
          aSortedLogs.AddObject (aLogs.LogItems[x].CompareKey(aOrderBy, sortColumns), aLogs.LogItems[x]);
      aSortedLogs.CustomSort(logz.doOrder);
      for x := 0 to bLogs.Count - 1 do
        if bLogs.LogItems [x].PassesFilter then
          bSortedLogs.AddObject (bLogs.LogItems[x].CompareKey(aOrderBy, sortColumns), bLogs.LogItems[x]);
      bSortedLogs.CustomSort(logz.doOrder);

      LA := TStringList.Create;
      LB := TStringList.Create;
      Diffs := TA2BStringList.Create;
      try
        for x := 0 to aSortedLogs.Count - 1 do
        begin
          with aSortedLogs.LogItems[x] do
          begin
            s := '';
            if Assigned (Operation) then
              s := s + Operation.WsdlService.Name + ';' + Operation.Name + ';'
            else
              s := s + ';;';
            if Assigned (Mssg) then
              s := s + Mssg.Name;
            s := s + ';' + CorrelationId;
          end;
          LA.Add(s);
        end;
        for x := 0 to bSortedLogs.Count - 1 do
        begin
          with bSortedLogs.LogItems[x] do
          begin
            s := '';
            if Assigned (Operation) then
              s := s + Operation.WsdlService.Name + ';' + Operation.Name + ';'
            else
              s := s + ';;';
            if Assigned (Mssg) then
              s := s + Mssg.Name;
            s := s + ';' + CorrelationId;
          end;
          LB.Add(s);
        end;
        Diffs.Execute(LA, LB);
        bodyXml := result.AddXml(TXml.CreateAsString('Body', ''));
        itemsXml := TXml.CreateAsString('Items', ''); // create in advance
        a := 0; b := 0;
        for c := 0 to Diffs.ChangeCount - 1 do
        begin
          while a < Diffs.Changes[c].x do
          begin
            _CompareAB(aSortedLogs.LogItems[a], bSortedLogs.LogItems[b], itemsXml);
            if itemsXml.Items.Count > 0 then
            begin
              with bodyXml.AddXml(_abDetailXml (aSortedLogs.LogItems[a], bSortedLogs.LogItems[b])) do
                AddXml (itemsXml);
              itemsXml := TXml.CreateAsString('Items', ''); // create in advance
            end;
            inc(a); inc(b);
          end;
          if Diffs.Changes[c].Kind = ckAdd then
          begin
            for i := b to b + Diffs.Changes[c].Range - 1 do
            begin
              bodyXml.AddXml(_bDetailXml (bSortedLogs.LogItems[b]));
              inc(b);
            end;
          end
          else
          begin
            if Diffs.Changes[c].Kind = ckDelete then
            begin
              for i := a to a + Diffs.Changes[c].Range - 1 do
              begin
                bodyXml.AddXml(_aDetailXml (aSortedLogs.LogItems[a]));
                inc(a);
              end;
            end
            else
            begin
              for i := a to a + Diffs.Changes[c].Range - 1 do
              begin
                bodyXml.AddXml(_aDetailXml (aSortedLogs.LogItems[a]));
                inc(a);
              end;
              for i := b to b + Diffs.Changes[c].Range - 1 do
              begin
                bodyXml.AddXml(_bDetailXml (bSortedLogs.LogItems[b]));
                inc(b);
              end;
            end;
          end;
        end;
        while (a < aSortedLogs.Count) and (b < bSortedLogs.Count) do
        begin
          _CompareAB(aSortedLogs.LogItems[a], bSortedLogs.LogItems[b], itemsXml);
          if itemsXml.Items.Count > 0 then
          begin
            with bodyXml.AddXml(_abDetailXml (aSortedLogs.LogItems[a], bSortedLogs.LogItems[b])) do
              AddXml (itemsXml);
            itemsXml := TXml.CreateAsString('Items', ''); // create in advance
          end;
          inc(a); inc(b);
        end;
        while (a < aSortedLogs.Count) do
        begin
          bodyXml.AddXml(_aDetailXml (aSortedLogs.LogItems[a]));
          inc(a);
        end;
        while (b < bSortedLogs.Count) do
        begin
          bodyXml.AddXml(_bDetailXml (bSortedLogs.LogItems[b]));
          inc(b);
        end;
        headerXml.AddXml(TXml.CreateAsBoolean('differencesFound', bodyXml.Items.Count > 0));
        headerXml.AddXml(TXml.CreateAsString('referenceLogFileName', aReferenceFileName));
        headerXml.AddXml(TXml.CreateAsString('Created', xsdNowAsDateTime));
      finally
        FreeAndNil (LA);
        FreeAndNil (LB);
        FreeAndNil (Diffs);
      end;
    finally
      aSortedLogs.Free;
      bSortedLogs.Free;
    end;
  finally
    a2bUninitialize;
    result.CheckDownline(True);
  end;
end;

{ TLogList }

function TLogList.SaveLog(aString: String; aLog: TLog): TLog;
begin
  result := aLog;
  inherited SaveObject (aString, aLog);
  Inc(fNumber);
end;

constructor TLogList.Create;
begin
  inherited Create;
  fNumber := 0;
end;

function TLogList.PrepareCoverageReportAsXml(aOperations: TWsdlOperations; ignoreCoverageOn: TStringList): TXmlCvrg;
var
  o, lg, p, e, d, x: Integer;
  xXml, faultXml, detailXml: TXml;
  oXml, mXml, xXmlCvrg, faultCoverageXml: TXmlCvrg;
  xLog: TLog;
begin
  result := TXmlCvrg.CreateAsString('coverageReport', '');
  // first setup a hyerarchy to count elements
  with result do
  begin
    Tag := 1;
    for o := 0 to aOperations.Count - 1 do with aOperations.Operations[o] do
    begin
      if (not isFreeFormat)
      and (not HiddenFromUI) then
      begin
        if isOpenApiService then
        begin
          with AddXml (TXmlCvrg.CreateAsString(Alias, '')) do
          begin
            AddXml (TXmlCvrg.CreateFromXsd ('Req', (ReqBind as TXml).Xsd));
            AddXml (TXmlCvrg.CreateFromXsd ('Rpy', (RpyBind as TXml).Xsd));
          end;
        end
        else
        begin
          with AddXml (TXmlCvrg.CreateAsString(Alias, '')) do
          begin
  {}{
            if Assigned (fltBind)
            and (fltBind is TXml)
            and Assigned ((fltBind as TXml).TypeDef) then
              AddXml (_typeAsXml ('Fault', (fltBind as TXml).Xsd));
  {}
            if Assigned (ReqBind)
            and (ReqBind is TXml)
            and Assigned ((ReqBind as TXml).TypeDef) then
              AddXml (TXmlCvrg.CreateFromXsd (reqTagName, (ReqBind as TXml).Xsd));
            if Assigned (ReqBind)
            and (ReqBind is TIpmItem) then
              AddXml (TXmlCvrg.CreateFromIpm (ReqBind.Name, (ReqBind as TIpmItem)));
            if Assigned (RpyBind)
            and (RpyBind is TXml)
            and Assigned ((RpyBind as TXml).TypeDef) then
              AddXml (TXmlCvrg.CreateFromXsd (rpyTagName, (RpyBind as TXml).Xsd));
            if Assigned (RpyBind)
            and (RpyBind is TIpmItem) then
              AddXml (TXmlCvrg.CreateFromIpm (RpyBind.Name, (RpyBind as TIpmItem)));
            if Assigned (FltBind)
            and (fltBind is TXml)
            and Assigned ((FltBind as TXml).TypeDef) then
              with AddXml (TXmlCvrg.CreateFromXsd ((FltBind as TXml).Name, (FltBind as TXml).Xsd)) do
              begin
                Name := 'Fault';
                for x := 0 to Items.Count - 1 do
                  if Items.XmlItems[x].Name = '' then
                    Items.XmlItems[x].Name := 'detail';
              end;
            if Assigned (FltBind)
            and (FltBind is TIpmItem) then
              AddXml (TXmlCvrg.CreateFromIpm (FltBind.Name, (FltBind as TIpmItem)));
          end;
        end;
      end;
    end;
  end;

  // and now do the counting
  for lg := 0 to Count - 1 do
  begin
    xLog := LogItems[lg];
    if Assigned (xLog.Operation)
    and (not xLog.Operation.HiddenFromUI)
    and Assigned(xLog.Operation.WsdlService) then
    begin
      if xLog.Operation.isOpenApiService then
      begin
        Inc (result.Counter);
        oXml := result.Items.XmlItemByTag[xLog.Operation.Alias] as TXmlCvrg;
        if not Assigned (oXml) then
          raise Exception.CreateFmt('Lookup for %s failed', [xLog.Operation.reqTagName]);
        Inc (oXml.Counter);
        if Assigned (xLog.Operation.reqBind) then
        begin
          mXml := oXml.Items.XmlItemByTag['Req'] as TXmlCvrg;
          if not Assigned (mXml) then
            raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.reqTagName);
          xXml := xLog.reqBodyAsXml;
          try
            xXml.Name := mXml.Name;
            mXml.CountUsage(xXml, False);
          finally
            xXml.Free;
          end;
        end;
        if Assigned (xLog.Operation.rpyBind) then
        begin
          mXml := oXml.Items.XmlItemByTag['Rpy'] as TXmlCvrg;
          if not Assigned (mXml) then
            raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.rpyTagName);
          xXml := xLog.rpyBodyAsXml;
          try
            if (xXml.Name = xLog.Operation.Alias)
            and (xXml.Items.Count = 1)
            and (xXml.Items.XmlItems[0].Name = 'Rpy') then
              mXml.CountUsage(xXml.Items.XmlItems[0], False);
          finally
            xXml.Free;
          end;
        end;
      end
      else
      begin
        case xLog.Operation.WsdlService.DescriptionType of
          ipmDTCobol:
          begin
            Inc (result.Counter);
            oXml := result.Items.XmlItemByTag[xLog.Operation.Alias] as TXmlCvrg;
            if not Assigned (oXml) then
              raise Exception.CreateFmt('Lookup for %s failed', [xLog.Operation.reqTagName]);
            Inc (oXml.Counter);
            if Assigned(xLog.Operation.reqBind) then
            begin
              mXml := oXml.Items.XmlItemByTag[xLog.Operation.reqBind.Name] as TXmlCvrg;
              if not Assigned (mXml) then
                raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.reqBind.Name);
              xXml := xLog.reqBodyAsXml;
              try
                mXml.CountUsage(xXml, false);
              finally
                xXml.Free;
              end;
            end;
            if Assigned(xLog.Operation.rpyBind) then
            begin
              mXml := oXml.Items.XmlItemByTag[xLog.Operation.rpyBind.Name] as TXmlCvrg;
              if not Assigned (mXml) then
                raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.rpyBind.Name);
              xXml := xLog.rpyBodyAsXml;
              try
                mXml.CountUsage(xXml, false);
              finally
                xXml.Free;
              end;
            end;
  {
            if Assigned(xLog.Operation.fltBind) then
            begin
              mXml := oXml.Items.XmlItemByTag[xLog.Operation.fltBind.Name] as TXmlCvrg;
              if not Assigned (mXml) then
                raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.fltBind.Name);
              xXml := xLog.fltBodyAsXml;
              try
                mXml.CountUsage(xXml, false);
              finally
                xXml.Free;
              end;
            end;
  }
          end;
          ipmDTXsd, ipmDTSwiftMT:
          begin
            Inc (result.Counter);
            oXml := result.Items.XmlItemByTag[xLog.Operation.Alias] as TXmlCvrg;
            if not Assigned (oXml) then
              raise Exception.CreateFmt('Lookup for %s failed', [xLog.Operation.reqTagName]);
            Inc (oXml.Counter);
            if Assigned (xLog.Operation.reqBind)
            and (xLog.RequestBody <> '') then
            begin
              mXml := oXml.Items.XmlItemByTag[xLog.Operation.reqTagName] as TXmlCvrg;
              if not Assigned (mXml) then
                raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.reqTagName);
              xXml := TXml.CreateAsString(mXml.Name, '');
              try
                xXml.AddXml (xLog.reqBodyAsXml);
                mXml.CountUsage(xXml, xLog.Operation.WsdlService.DescriptionType in [ipmDTSwiftMT]);
              finally
                xXml.Free;
              end;
            end;
            if Assigned (xLog.Operation.rpyBind)
            and (xLog.ReplyBody <> '') then
            begin
              mXml := oXml.Items.XmlItemByTag[xLog.Operation.rpyTagName] as TXmlCvrg;
              if not Assigned (mXml) then
                raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.rpyTagName);
              xXml := TXml.CreateAsString(mXml.Name, '');
              try
                xXml.AddXml (xLog.rpyBodyAsXml);
                mXml.CountUsage(xXml, xLog.Operation.WsdlService.DescriptionType in [ipmDTSwiftMT]);
              finally
                xXml.Free;
              end;
            end;
          end;
          ipmDTWsdl:
          begin
            Inc (result.Counter);
            oXml := result.Items.XmlItemByTag[xLog.Operation.Alias] as TXmlCvrg;
            if not Assigned (oXml) then
              raise Exception.Create('Operation Lookup failed for ' + xLog.Operation.reqTagName);
            Inc (oXml.Counter);
            xXml := xLog.reqBodyAsXml;
            try
              if NameWithoutPrefix(xXml.Name) = 'Envelope' then
              begin
                mXml := oXml.Items.XmlItemByTag[xLog.Operation.reqTagName] as TXmlCvrg;
                if not Assigned (mXml) then
                  raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.reqBind.Name);
                Inc (mXml.Counter);
                for p := 0 to xXml.Items.Count - 1 do //header, body
                  for e := 0 to xXml.Items.XmlItems[p].Items.Count - 1 do
                     for d := 0 to mXml.Items.Count - 1 do
                       mXml.XmlItems[d].CountUsage(xXml.Items.XmlItems[p].Items.XmlItems[e], False);
              end;
            finally
              xXml.Free;
            end;
            xXml := xLog.rpyBodyAsXml;
            try
              if NameWithoutPrefix(xXml.Name) = 'Envelope' then
              begin
                faultXml := xXml.FindUQXml('Envelope.Body.Fault');
                if Assigned (faultXml) then
                begin
                  detailXml := faultXml.ItemByTag['detail'];
                  if Assigned (detailXml) then
                    detailXml.Name := 'SOAPFault';
                  faultCoverageXml := TXmlCvrg(oXml.FindUQXml(oXml.Name + '.Fault'));
                  if Assigned (faultCoverageXml) then
                    faultCoverageXml.CountUsage(faultXml, False);
                end
                else
                begin
                  mXml := oXml.Items.XmlItemByTag[xLog.Operation.rpyTagName] as TXmlCvrg;
                  if not Assigned (mXml) then
                    raise Exception.Create('Operation Bind Lookup failed for ' + xLog.Operation.rpyBind.Name);
                  Inc (mXml.Counter);
                  for p := 0 to xXml.Items.Count - 1 do //header, body
                    for e := 0 to xXml.Items.XmlItems[p].Items.Count - 1 do
                       for d := 0 to mXml.Items.Count - 1 do
                         mXml.XmlItems[d].CountUsage(xXml.Items.XmlItems[p].Items.XmlItems[e], False);
                end;
              end;
            finally
              xXml.Free;
            end;
          end;
        end;
      end;
    end;
  end;
  for x := 0 to ignoreCoverageOn.Count - 1 do
  begin
    xXmlCvrg := TXmlCvrg(result.FindUQXml(ignoreCoverageOn.Strings[x]));
    if Assigned (xXmlCvrg) then
      xXmlCvrg.Ignore := True;
  end;
end;

function TLogList.GetLog(Index: integer): TLog;
begin
  result := TLog (Objects [Index]);
end;

procedure TLogList.InvalidateDisplayedColumns(aOperation: TWsdlOperation);
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    if LogItems[x].Operation = aOperation then
      LogItems[x].DisplayedColumnsValid := False;
end;

procedure TLogList.InvalidateDisplayedColumns;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    LogItems[x].DisplayedColumnsValid := False;
end;

function TLogList.LogsAsString (aStubFileName: String): String;
var
  x: Integer;
  n: Integer;
begin
  n := Count;
  with TXml.CreateAsString ('WsdlStubCaseMessages', '') do
  try
    AddXml (TXml.CreateAsString('wsdlStub', aStubFileName));
    with AddXml (TXml.CreateAsString ('refreshInfo', '')) do
    begin
      AddXml (TXml.CreateAsInteger ('Index', n - 1));
      if n > 0 then
        AddXml (TXml.CreateAsString ('MessageId', LogItems [n - 1].MessageId));
    end;
    for x := 0 to n - 1 do
      AddXml (LogItems [x].AsXml);
    result := AsText(False,0,False,False);
  finally
    Free;
  end;
end;

function TLogList.UnexpectedsAsXml: TXml;
var
  x, d: Integer;
  headerXml, bodyXml: TXml;
  xLog: TLog;
  xBind: TCustomBindable;
begin
  result := TXml.CreateAsString('logUnexpecteds', '');
  headerXml := result.AddXml(TXml.CreateAsString('Header', ''));
  bodyXml := result.AddXml(TXml.CreateAsString('Body', ''));
  for x := 0 to Count - 1 do
  begin
    xLog := LogItems[x];
    if Assigned (xLog.Operation)
    and (xLog.Operation.ExpectationBindables.Count > 0)
    and Assigned (xLog.Mssg)
    and (xLog.Exception = '') then
    begin
      xLog.toBindables (xLog.Operation);
      xLog.HasUnexpectedValue := xLog.Mssg.CheckValues(xLog.Operation);
      xLog.ExpectedValuesChecked := True;
      if xLog.HasUnexpectedValue then
      begin
        with bodyXml.AddXml(TXml.CreateAsString('Detail','')) do
        begin
          AddXml (TXml.CreateAsString('messageTimestamp', xsdDateTime(xLog.InboundTimeStamp)));
          AddXml (TXml.CreateAsString('Service', xLog.Operation.WsdlService.Name));
          AddXml (TXml.CreateAsString('Operation', xLog.Operation.Name));
          AddXml (TXml.CreateAsString('Message', xLog.Mssg.Name));
          AddXml (TXml.CreateAsString('Correlation', xLog.CorrelationId));
          with AddXml (TXml.CreateAsString('Items','')) do
          begin
            for d := 0 to xLog.Operation.ExpectationBindables.Count - 1 do
            begin
              xBind := xLog.Operation.ExpectationBindables.Bindables[d];
              if xBind.HasUnExpectedValue then
              begin
                with AddXml (TXml.CreateAsString('Item','')) do
                begin
                  AddXml (TXml.CreateAsString('Tag', xBind.FullIndexCaption));
                  AddXml (TXml.CreateAsString('currentValue', xBind.Value));
                  AddXml (TXml.CreateAsString('referenceValue', xBind.ExpectedValue));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  headerXml.AddXml(Txml.CreateAsBoolean('unexpectedValuesFound', (bodyXml.Items.Count > 0)));
  headerXml.AddXml(Txml.CreateAsString('Created', xsdNowAsDateTime));
  result.CheckDownline(True);
end;

procedure TLogList.SetLog(Index: integer; const Value: TLog);
begin
  Objects [Index] := Value;
end;


{ TLogFilter }

constructor TLogFilter.Create;
begin
  rx := TRegExpr.Create;
  FilterStyle := fsShowMatch;
end;

destructor TLogFilter.Destroy;
begin
  FreeAndNil (rx);
  inherited;
end;

procedure TLogFilter.Execute(aLog: TLog);
  function nEnabledConditions: Integer;
  begin
    result := 0;
    if StubActionEnabled then Inc (result);
    if MiMEnabled then Inc (result);
    if MessageValidationEnabled then Inc (result);
    if ExceptionEnabled then Inc (result);
    if ServiceEnabled then Inc (result);
    if OperationEnabled then Inc (result);
    if CorrelationEnabled then Inc (result);
    if RequestEnabled then Inc (result);
    if ReplyEnabled then Inc (result);
    if UnexpectedValuesEnabled then Inc (result);
    if RemarksEnabled then Inc (result);
  end;
  function _StringMatchesRegExpr (aString, aExpr: String; Contains: Boolean): Boolean;
  begin
    try
      if Contains then
        rx.Expression := aExpr
      else
        rx.Expression := '^(' + aExpr + ')$';  // bol and eol: must match entire string
      result := rx.Exec(aString);
    finally
    end;
  end;
  function _Matches (aString, aCompStr: String; aEquals, aRegExp, aContains: Boolean): Boolean;
  begin
    if aRegExp then
      result := _StringMatchesRegExpr(aString, aCompStr, aContains)
    else
      result := (aString = aCompStr);
    if not aEquals then
      result := not result;
  end;
var
  xMatches, xMatchAny: Boolean;
begin
  if not Enabled then
  begin
    aLog.PassesFilter := True;
    aLog.ShowHighLighted := False;
    exit;
  end;
  xMatchAny := MatchAny and (nEnabledConditions > 1);
  xMatches := not xMatchAny;
  try
    if (xMatches or xMatchAny)
    and StubActionEnabled then
      xMatches := (   ((    StubActionEquals) and (aLog.StubAction = StubAction))
                 or ((not StubActionEquals) and (aLog.StubAction <> StubAction))
                );
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and MiMEnabled then
      xMatches := (   (    (RequestMiMEnabled)
                     and (aLog.RequestBodyMiM <> '')
                     and (aLog.RequestBodyMiM <> aLog.RequestBody)
                    )
                 or (    (ReplyMiMEnabled)
                     and (aLog.ReplyBodyMiM <> '')
                     and (aLog.ReplyBodyMiM <> aLog.ReplyBody)
                    )
                );
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and MessageValidationEnabled then
      xMatches := (   ((RequestValidationEnabled) and (aLog.RequestValidateResult <> ''))
                 or ((ReplyValidationEnabled) and (aLog.ReplyValidateResult <> ''))
                );
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and ExceptionEnabled
    then
      xMatches := _Matches (aLog.Exception, Exception, ExceptionEquals, ExceptionRegExp, False);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and ServiceEnabled
    and Assigned (aLog.Operation)
    and Assigned (aLog.Operation.WsdlService)
    then
      xMatches := _Matches (aLog.Operation.WsdlService.Name, Service ,ServiceEquals, ServiceRegExp, False);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and OperationEnabled
    and Assigned (aLog.Operation)
    then
      xMatches := _Matches (aLog.Operation.Name, Operation, OperationEquals, OperationRegExp, False);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and CorrelationEnabled
    then
      xMatches := _Matches (aLog.CorrelationId, Correlation, CorrelationEquals, CorrelationRegExp, False);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and RequestEnabled
    then
      xMatches := _Matches (aLog.RequestBody, Request, RequestEquals, True, True);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and ReplyEnabled
    then
      xMatches := _Matches (aLog.ReplyBody, Reply, ReplyEquals, True, True);
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and UnexpectedValuesEnabled
    then
      xMatches := aLog.HasUnexpectedValue;
    if xMatches and xMatchAny then Exit;

    if (xMatches or xMatchAny)
    and RemarksEnabled
    then
      xMatches := (aLog.Remarks <> '');
    if xMatches and xMatchAny then Exit;

  finally
    case FilterStyle of
      fsShowMatch:
      begin
        aLog.PassesFilter := xMatches;
        aLog.ShowHighLighted := False;
      end;
      fsShowMismatch:
      begin
        aLog.PassesFilter := not xMatches;
        aLog.ShowHighLighted := False;
      end;
      fsHighlightMatch:
      begin
        aLog.PassesFilter := True;
        aLog.ShowHighLighted := xMatches;
      end;
      fsHighlightMismatch:
      begin
        aLog.PassesFilter := True;
        aLog.ShowHighLighted := not xMatches;
      end;
    end;
  end;
end;

{ TLog }

function TLog.AsXml: TXml;
var
  xBodiesAsBase64: Boolean;
begin
  xBodiesAsBase64 := False;
  result := TXml.CreateAsString('RequestReply', '');
  with result do
  begin
    AddXml (TXml.CreateAsString('InboundTimeStamp', xsdDateTime (Self.InboundTimeStamp)));
    AddXml (TXml.CreateAsString('OutboundTimeStamp', xsdDateTime (Self.OutboundTimeStamp)));
    AddXml (Txml.CreateAsInteger('DelayTimeMs', Ord (Self.DelayTimeMs)));
    AddXml (Txml.CreateAsInteger('OperationCount', Ord (Self.OperationCount)));
    AddXml (Txml.CreateAsInteger('TransportType', Ord (Self.TransportType)));
    AddXml (TXml.CreateAsString('StubAction', IntToStr(Ord(Self.StubAction))));
    AddXml (Txml.CreateAsString('CorrelationId', Self.CorrelationId));
    if Assigned (Self.Operation) then
    begin
      xBodiesAsBase64 := (Self.Operation.WsdlService.DescriptionType in [ipmDTCobol, ipmDTBmtp]);
      AddXml (TXml.CreateAsString('Service', Self.Operation.WsdlService.Name));
      AddXml (TXml.CreateAsString('Operation', Self.Operation.Name));
      if Assigned (Self.Mssg) then
        AddXml (TXml.CreateAsString('Reply', Self.Mssg.Name))
      else
        AddXml (TXml.CreateAsString('Reply', ''));
    end
    else
    begin
      AddXml (TXml.CreateAsString('Service', ''));
      AddXml (TXml.CreateAsString('Operation', ''));
      AddXml (TXml.CreateAsString('Reply', ''));
    end;
    AddXml (TXml.CreateAsString('Error', Self.Exception));
    AddXml (TXml.CreateAsString('Remarks', Self.Remarks));
    AddXml (TXml.CreateAsString('Notifications', Self.Notifications));
    AddXml (Txml.CreateAsString('httpUri', Self.httpUri));
    AddXml (Txml.CreateAsInteger('httpResponseCode', Self.httpResponseCode));
    AddXml (Txml.CreateAsString('httpCommand', Self.httpCommand));
    AddXml (Txml.CreateAsString('httpDocument', Self.httpDocument));
    AddXml (Txml.CreateAsString('httpParams', Self.httpParams));
    AddXml (Txml.CreateAsString('RequestContentType', Self.RequestContentType));
    AddXml (Txml.CreateAsString('ReplyContentType', Self.ReplyContentType));
    AddXml (Txml.CreateAsString('HttpRequestHeaders', Self.RequestHeaders));
    AddXml (Txml.CreateAsString('HttpReplyHeaders', Self.ReplyHeaders));
    if xBodiesAsBase64 then
    begin
      AddXml (TXml.CreateAsBoolean('BodiesAsBase64', xBodiesAsBase64));
      AddXml (Txml.CreateAsString('HttpRequestBody', base64.EncodeStringBase64(Self.RequestBody)));
      AddXml (Txml.CreateAsString('HttpRequestBodyMiM', base64.EncodeStringBase64(Self.RequestBodyMiM)));
      AddXml (Txml.CreateAsString('HttpReplyBody', base64.EncodeStringBase64(Self.ReplyBody)));
      AddXml (Txml.CreateAsString('HttpReplyBodyMiM', base64.EncodeStringBase64(Self.ReplyBodyMiM)));
    end
    else
    begin
      AddXml (Txml.CreateAsString('HttpRequestBody', Self.RequestBody));
      AddXml (Txml.CreateAsString('HttpRequestBodyMiM', Self.RequestBodyMiM));
      AddXml (Txml.CreateAsString('HttpReplyBody', Self.ReplyBody));
      AddXml (Txml.CreateAsString('HttpReplyBodyMiM', Self.ReplyBodyMiM));
    end;
    AddXml (Txml.CreateAsBoolean('RequestValidated', Self.RequestValidated));
    AddXml (Txml.CreateAsString('RequestValidateResult', Self.RequestValidateResult));
    AddXml (Txml.CreateAsBoolean('ReplyValidated', Self.ReplyValidated));
    AddXml (Txml.CreateAsString('ReplyValidateResult', Self.ReplyValidateResult));
    AddXml (Txml.CreateAsString('Nr', IntToStr (Self.Nr)));
    AddXml (Txml.CreateAsString('MessageId', Self.MessageId));
    AddXml (Txml.CreateAsBoolean('Stubbed', Self.Stubbed));
    AddXml (Txml.CreateAsBoolean('wsaCorrelated', Self.wsaCorrelated));
    AddXml (Txml.CreateAsString('ServiceName', Self.ServiceName));
    AddXml (Txml.CreateAsString('OperationName', Self.OperationName));
  end;
end;

constructor TLog.Create;
begin
  MessageId := 'uuid:' + generateRandomId;
  DisplayedColumns := TStringList.Create;
end;

destructor TLog.Destroy;
begin
  DisplayedColumns.Clear;
  FreeAndNil (DisplayedColumns);
  FreeAndNil (Stream);
end;

procedure TLog.AddRemark(aRemark: String);
begin
  if Remarks = '' then
    Remarks := aRemark
  else
    Remarks := Remarks + LineEnding + aRemark;
end;

function TLog.CompareKey (aCompareBy : TCompareLogOrderBy; aSortColumns: TStringList): String ;
var
  xXml, xReqXml, xRpyXml: TXml;
  x: Integer;
begin
  result := '';
  if Assigned (Operation) then
  begin
    case aCompareBy of
      clCorrelation:
      begin
        result := Operation.WsdlService.Name
                + ';'
                + Operation.Name
                + ';'
                + CorrelationId
                ;
      end;
      clOperation:
        result := Operation.WsdlService.Name
                + ';'
                + Operation.Name
                ;
    end;
  end
  else
  begin
    case aCompareBy of
      clOperation: result := '';
      clCorrelation: result := CorrelationId;
    end;
  end;
  if (aSortColumns.Count > 0)
//and (aCompareBy <> clTimeStamp)
  then
  begin
    xReqXml := reqBodyAsXml;
    xRpyXml := rpyBodyAsXml;
    try
      for x := 0 to aSortColumns.Count - 1 do
      begin
        result := result + ';';
        xXml := xReqXml.FindUQXml(aSortColumns.Strings[x]);
        if Assigned (xXml) then
          Result := Result + xXml.Value
        else
        begin
          xXml := xRpyXml.FindUQXml(aSortColumns.Strings[x]);
          if Assigned (xXml) then
            Result := Result + xXml.Value
        end;
      end;
    finally
      FreeAndNil(xReqXml);
      FreeAndNil(xRpyXml);
    end;
  end;
end;

function TLog.DurationAsString: String;
var
  Duration: TDateTime;
  xDuration: Extended;
begin
  result := '';
  if (InboundTimeStamp = 0)
  or (OutboundTimeStamp = 0) then
    raise sysUtils.Exception.Create('Duration can not be computed because not all required timestamps ar known');
  if StubAction = saRequest then
    Duration := InboundTimeStamp - OutBoundTimeStamp
  else
    Duration := OutBoundTimeStamp - InboundTimeStamp;
  xDuration := Duration * 24 * 60 * 60;
  result := Format ('%.3f', [xDuration]);
end;

function TLog.StubActionAsString : String ;
begin
  result := '';
  case StubAction of
    saStub: result := 'Stub';
    saForward: result := 'Forward';
    saRedirect: result := 'Redirect';
    saRequest: result := 'Request';
  end;
end;

function TLog.reqBodyAsXml: TXml;
begin
  if Assigned (Operation)
  and (Operation.reqBind is TIpmItem)
  then begin
    (Operation.reqBind as TIpmItem).BufferToValues (nil, RequestBody);
    try result := (Operation.reqBind as TIpmItem).AsXml; except end;
    Exit;
  end;

  if Assigned (Operation)
  and (Operation.WsdlService.DescriptionType = ipmDTSwiftMT) then
  begin
    with TSwiftMT.Create(RequestBody, Operation.reqXsd) do
    try
      try
        result := AsXml;
        Exit;
      except
        on e: sysUtils.Exception do
        begin
          result := TXml.Create;
          result.Checked := True;
          result.Name := 'Exception';
          result.Value := e.Message;
        end;
      end;
    finally
      Free;
    end;
  end;

  if Assigned (Operation)
  and (Operation.isOpenApiService) then
  begin
    OpenApiRequestToBindables(Operation);
    result := TXml.Create;
    result.CopyDownLine(Operation.reqXml, True);
    Exit;
  end;

  result := TXml.Create;
  result.LoadFromString(RequestBody, nil);
  if result.Name = '' then
  begin
    try
      result.LoadJsonFromString(RequestBody, nil);
    except
      on e: sysUtils.Exception do
      begin
        result.Checked := True;
        if Assigned (Operation) then
          result.Name := Operation.Name
        else
          result.Name := 'UnknownOperation';
        result.Value := 'unable to present request as Xml';
      end;
    end;
  end;
end;

function TLog.rpyBodyAsXml: TXml;
var
  x, y, f: Integer;
  hXml: TXml;
begin
  if Assigned (Operation)
  and (Operation.rpyBind is TIpmItem)
  then
  begin
    try
      (Operation.rpyBind as TIpmItem).BufferToValues (nil, ReplyBody);
      try result := (Operation.rpyBind as TIpmItem).AsXml; except end;
    except
      result := TXml.CreateAsString('UnableToPresentReplyAsXml', '');
      with TStringList.Create do
      try
        Text := ReplyBody;
        for x := 0 to Count - 1 do
          result.AddXml(TXml.CreateAsString('Line', Strings[x]));
      finally
        Free;
      end;
    end;
    Exit;
  end;

  if Assigned (Operation)
  and (Operation.WsdlService.DescriptionType = ipmDTSwiftMT) then
  begin
    with TSwiftMT.Create(ReplyBody, Operation.rpyXsd) do
    try
      try
        result := AsXml;
      except
        on e: sysUtils.Exception do
        begin
          result := TXml.Create;
          result.Checked := True;
          result.Name := 'Exception';
          result.Value := e.Message
        end;
      end;
    finally
      Free;
    end;
    Exit;
  end;

  if Assigned (Operation)
  and (Operation.isOpenApiService) then
  begin
    result := TXml.CreateAsString(Operation.Alias, '');
    with result.AddXml(TXml.CreateAsString('Rpy', '')) do
    begin
      with AddXml (TXml.CreateAsString('rspns' + IntToStr(httpResponseCode), '')) do
      begin
        with AddXml (TXml.Create) do
        begin
          if Pos ('xml', ReplyContentType) > 0 then
          begin
            LoadFromString(ReplyBody, nil);
            exit;
          end;
          if Pos ('json', ReplyContentType) > 0 then
          begin
            LoadJsonFromString(ReplyBody, nil);
            Name := 'body';
            exit;
          end;
        end;
      end;
    end;
  end;

  result := TXml.Create;
  result.LoadFromString(ReplyBody, nil);
  if result.Name = '' then
  begin
    try
      result.LoadJsonFromString(ReplyBody, nil);
    except
      on e: sysUtils.Exception do
      begin
        result.Checked := True;
        if Assigned (Operation) then
          result.Name := Operation.Name
        else
          result.Name := 'UnknownOperation';
        result.Value := 'unable to present reply as Xml';
      end;
    end;
  end;
end;

procedure TLog.FoundErrorInBuffer(ErrorString: String; aObject: TObject);
begin
  (aObject as TIpmItem).Value := '?wsdlStub Error found: ' + ErrorString;
end;

procedure TLog.OpenApiRequestToBindables (aOperation: TWsdlOperation);
var
  x, y, k, f: Integer;
  pathParams, pathMask, qryParams, hdrParams: TStringList;
  xXml: TXml;
  xValue, xSeparator: String;
begin
  if not Assigned (aOperation) then
    raise SysUtils.Exception.Create('procedure TLog.OpenApiRequestToBindables (aOperation: TWsdlOperation); nil arg');
  if not aOperation.isOpenApiService then
    raise SysUtils.Exception.Create('procedure TLog.OpenApiRequestToBindables (aOperation: TWsdlOperation); not an openApi operation');
  aOperation.reqXml.ResetValues;
  aOperation.reqXml.Checked := True;
  pathParams := TStringList.Create;
  pathMask := TStringList.Create;
  qryParams := TStringList.Create;
  hdrParams := TStringList.Create;
  hdrParams.NameValueSeparator := ':';
  try
    ExplodeStr (self.httpDocument, '/', pathParams);
    ExplodeStr (aOperation.WsdlService.openApiPathMask , '/', pathMask);
    if pathParams.Count <> pathMask.Count then
      raise sysutils.Exception.CreateFmt ( '%s document %s and service path %s do not match'
                                         , [ aOperation.Name
                                           ,             self.httpDocument
                                           ,                                 aOperation.WsdlService.openApiPathMask
                                           ]
                                         );
    k := pathParams.Count - 1;
    while (k > -1)
    and (pathMask.Strings[k] <> '%s') do
      k := k - 1;
    ExplodeStr (urlDecode(self.httpParams), '&', qryParams);
    hdrParams.Text := self.RequestHeaders;
    with aOperation.reqXml.Items do for x := Count - 1 downto 0 do
    begin
      case XmlItems[x].Xsd.ParametersType of
        oppBody:
          begin
            xXml := TXml.Create;
            try
              if Pos ('json', self.RequestContentType) > 0 then
                xXml.LoadJsonFromString(self.RequestBody, nil)
              else
                xXml.LoadFromString(self.RequestBody, nil);
              xXml.Name := XmlItems[x].Name;
              XmlItems[x].LoadValues (xXml, false, False, True, False);
            finally
              xXml.Free;
            end;
          end;
        oppPath:
          begin
            XmlItems[x].ValueToJsonArray(pathParams.Strings[k]);
            k := k - 1;
            while (k > -1)
            and (pathMask.Strings[k] <> '%s') do
              k := k - 1;
          end;
        oppQuery:
          begin
            xValue := '';
            xSeparator := '';
            for y := 0 to qryParams.Count - 1 do
            begin
              if qryParams.Names[y] = XmlItems[x].Name then
              begin
                xValue := xValue + xSeparator + qryParams.ValueFromIndex[y];
                xSeparator := '&' + XmlItems[x].Name + '=';
              end;
            end;
            if xSeparator <> '' then
              XmlItems[x].ValueToJsonArray(xValue);
          end;
        oppHeader:
          begin
            if hdrParams.IndexOfName(XmlItems[x].Name) > -1 then
              XmlItems[x].ValueToJsonArray(Copy(hdrParams.Values[XmlItems[x].Name], 2, MaxInt));
          end;
        oppForm: SjowMessage ('oppForm: not suported');
      end;
    end;
    if hdrParams.IndexOfName('Accept') > -1 then
    begin
      if Pos ('xml', hdrParams.Values['Accept']) > 0 then
        aOperation.ProduceType := ptXml;
      if Pos ('json', hdrParams.Values['Accept']) > 0 then   // preference
        aOperation.ProduceType := ptJson;
    end;
  finally
    FreeAndNil(pathParams);
    FreeAndNil(pathMask);
    FreeAndNil(qryParams);
    FreeAndNil(hdrParams);
  end;
end;

procedure TLog.OpenApiReplyToBindables (aOperation: TWsdlOperation);
var
  x, y, k, f: Integer;
  hdrParams: TStringList;
  xXml, dXml: TXml;
  xValue, xSeparator: String;
begin
  if not Assigned (aOperation) then
    raise SysUtils.Exception.Create('procedure TLog.OpenApiReplyToBindables (aOperation: TWsdlOperation); nil arg');
  if not aOperation.isOpenApiService then
    raise SysUtils.Exception.Create('procedure TLog.OpenApiReplyToBindables (aOperation: TWsdlOperation); not an openApi operation');
  aOperation.rpyXml.ResetValues;
  xXml := TXml.Create;
  try
    if pos('xml', LowerCase(self.ReplyContentType)) > 0 then
      xXml.LoadFromString(self.ReplyBody, nil);
    if pos('json', LowerCase(self.ReplyContentType)) > 0 then
      xXml.LoadJsonFromString(self.ReplyBody, nil);
    if xXml.Name = '' then
      xXml.Name := 'unknown';
    dXml := nil;
    try
      dXml := aOperation.rpyXml.ItemByTag['rspns' + IntToStr(self.httpResponseCode)];
    except
    end;
    if Assigned(dXml) then
    begin
      dXml.Checked := True;
      if (dXml.Items.Count > 0) then
      begin
        dXml := dXml.Items.XmlItems[0];
        xXml.Name := dXml.Name;
        dXml.LoadValues(xXml, false, true);
      end;
    end
    else
    begin
      dXml := aOperation.rpyXml.FindXml('*.undefined.responseCode');
      if Assigned (dXml) then
      begin
        dXml.Value := IntToStr(self.httpResponseCode);
        dXml.Checked := True;
      end;
    end;
  finally
    xXml.Free;
  end;
end;

{ InitDisplayedColumns
  requires that the Operation.Bindables are filled with the correct values
  use .toBindables in case not
}
procedure TLog.InitDisplayedColumns(aOperation: TWsdlOperation; aDisplayedLogColumns: TStringList);
var
  x, c: Integer;
begin
  DisplayedColumns.Clear;
  c := aDisplayedLogColumns.Count;
  for x := 0 to c - 1 do
    DisplayedColumns.Add('?');
  if Assigned (aOperation) then
  begin
    while aOperation.LogColumns.Count < c do
      aOperation.LogColumns.Add('');
    for x := 0 to c - 1 do
    begin
      try
        if Assigned(aOperation.LogColumns.Bindables[x]) then
        begin
          if aOperation.LogColumns.Bindables[x].Checked then
            DisplayedColumns.Strings[x] := aOperation.LogColumns.Bindables[x].Value
          else
            DisplayedColumns.Strings[x] := '&nil';
        end
        else
          DisplayedColumns.Strings[x] := '?';
      except
        on e: SysUtils.Exception do
          DisplayedColumns.Strings[x] := e.Message;
      end;
    end;
  end;
  DisplayedColumnsValid := True;
end;

procedure TLog.toBindables (aOperation: TWsdlOperation);
begin
  if not Assigned (aOperation) then
    raise sysUtils.Exception.Create('procedure TLog.toBindables (aOperation: TWsdlOperation); nil arg');
  if aOperation.isOpenApiService then
  begin
    self.OpenApiRequestToBindables (aOperation);
    self.OpenApiReplyToBindables (aOperation);
  end
  else
  begin
    aOperation.RequestStringToBindables(RequestBody);
    aOperation.ReplyStringToBindables(ReplyBody);
  end;
end;

end.
