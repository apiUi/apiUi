unit snapshotz ;

{$mode objfpc}{$H+}

interface

uses Classes
   , SysUtils
   , Xmlz
   , Logz
   , ClaimListz
   , xmlxsdparser
   ;

type
  TSnapshotStatus = (rsUndefined, rsOk, rsNok, rsException);
  TSnapshot = class;
  TRegressionSnapshot = class;
  TCoverageReport = class;
  TSnapshotList = class;
  TSnapshotEvent = procedure (aReport: TSnapshot) of Object;

  { TSnapshot }

  TSnapshot = class(TClaimableObject)
  private
    fOnReport: TSnapshotEvent;
    function getAsXml : TXml ; virtual; abstract;
    function getStatusAsText : String ;
    function getTypeAsText: string; virtual; abstract;
    public
      Status: TSnapshotStatus;
      Name, FileName, RefFileName, Message: String;
      timeStamp: TDateTime;
      procedure doReport; virtual abstract;
      function Verdict: String;
      function VerdictColorAsString: String;
      property typeAsText: string read getTypeAsText;
      property statusAsText: String read getStatusAsText;
      procedure FromXml (aXml: TXml);
      property AsXml: TXml read getAsXml;
      property OnReport: TSnapshotEvent read fOnReport write fOnReport;
      constructor Create;
  end;

  { TRegressionSnapshot }

  TRegressionSnapshot = class(TSnapshot)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
      constructor Create (aName, aFileName, aRefFileName: String);
  end;
  { TRegressionReport }

  { TCoverageReport }

  TCoverageReport = class(TSnapshot)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
  end;

  { TSnapshotList }

  TSnapshotList = class (TClaimableObjectList)
  private
    function getAsXml : TXml ;
    procedure SeTSnapshot(Index: integer; const Value: TSnapshot);
    function GeTSnapshot (Index: integer): TSnapshot;
  public
    property SnapshotItems [Index: integer]: TSnapshot read GeTSnapshot write SeTSnapshot;
    property AsXml: TXml read getAsXml;
    constructor Create; overload;
  end;


implementation

{ TCoverageReport }

function TCoverageReport.getAsXml: TXml;
begin
  result := TXml.CreateAsString('coverageReportDetails','');
  with result do
  begin
    AddXml (TXml.CreateAsString('name', self.name));
    AddXml (TXml.CreateAsString('status', statusAsText));
    AddXml (TXml.CreateAsString('message', self.Message));
    AddXml (TXml.CreateAsTimeStamp('timeStamp', self.timeStamp));
  end;
end;

function TCoverageReport.getTypeAsText: string ;
begin
  result := 'coverage';
end;

procedure TCoverageReport.doReport ;
var
  xXml: TXml;
begin
  Status := rsUndefined;
  Message := '';
  if Assigned (fOnReport) then
    fOnReport(self)
  else
  begin
    Status := rsException;
    Message := 'Exception: no OnReportEvent for Coverage assigned';
  end;
end;

{ TSnapshot }

function TSnapshot.getStatusAsText: String ;
begin
  case Status of
    rsUndefined: result := 'undefined';
    rsOk: result := 'ok';
    rsNok: result := 'nok';
    rsException: result := 'exception';
  end;
end;

function TSnapshot .Verdict : String ;
begin
  result := '?';
  case Status of
    rsUndefined: result := 'undefined';
    rsOk: result := 'passed';
    rsNok: result := 'failed';
    rsException: result := 'exception';
  end;
end;

function TSnapshot .VerdictColorAsString : String ;
begin
  result := 'white';
  case Status of
    rsUndefined: result := 'white';
    rsOk: result := 'green';
    rsNok: result := 'red';
    rsException: result := 'orange';
  end;
end;

procedure TSnapshot.FromXml (aXml : TXml );
var
  s: String;
  dXml: TXml;
begin
  if not Assigned (aXml)
  or (aXml.Name <> 'reportDetails') then
    raise Exception.Create ('TSnapshot.FromXml (aXml : TXml ); //illegal argument');
  with aXml do
  begin
    Name := Items.XmlValueByTagDef['name', Name];
    s := Items.XmlValueByTag['status'];
    if s = 'undefined' then Status := rsUndefined;
    if s = 'ok' then Status := rsOk;
    if s = 'nok' then Status := rsNok;
    if s = 'exception' then Status := rsException;
    FileName := Items.XmlValueByTagDef['fileName', FileName];
    RefFileName := Items.XmlValueByTagDef['refFileName', RefFileName];
    Message := Items.XmlValueByTagDef['message', Message];
    dXml := ItemByTag['timeStamp'];
    if Assigned (dXml) then
      timeStamp := xsdParseDateTime(dXml.Value);
  end;
end;

constructor TSnapshot .Create ;
begin
  inherited Create ;
  timeStamp := Now;
end;

{ TSnapshot }

{ TRegressionSnapshot }

function TRegressionSnapshot.getAsXml: TXml ;
begin
  result := TXml.CreateAsString('SnapshotDetails','');
  with result do
  begin
    AddXml (TXml.CreateAsString('name', self.name));
    AddXml (TXml.CreateAsString('status', statusAsText));
    AddXml (TXml.CreateAsString('fileName', self.FileName));
    AddXml (TXml.CreateAsString('refFileName', self.RefFileName));
    AddXml (TXml.CreateAsString('message', self.Message));
    AddXml (TXml.CreateAsTimeStamp('timeStamp', self.timeStamp));
  end;
end;

function TRegressionSnapshot .getTypeAsText : string ;
begin
  result := 'regression';
end;

procedure TRegressionSnapshot.doReport;
var
  xXml: TXml;
begin
  Status := rsUndefined;
  Message := '';
  if Assigned (fOnReport) then
    fOnReport(self)
  else
  begin
    Status := rsException;
    Message := 'Exception: no OnReportEvent for Regression assigned';
  end;
end;

constructor TRegressionSnapshot.Create (aName, aFileName, aRefFileName: String);
begin
  inherited Create;
  Name := aName;
  FileName := aFileName;
  RefFileName := aRefFileName;
end;

{ TSnapshotList }

function TSnapshotList.getAsXml : TXml ;
var
  x: Integer;
begin
  result := TXml.CreateAsString('wsdlStubSnapshotList', '');
  with result do
  begin
    AddXml (TXml.CreateAsTimeStamp('created', Now));
    for x := 0 to Count - 1 do
      AddXml (SnapshotItems[x].AsXml);
  end;
end;

procedure TSnapshotList.SeTSnapshot (Index : integer; const Value: TSnapshot);
begin
  Objects [Index] := Value;
end;

function TSnapshotList.GeTSnapshot (Index: integer): TSnapshot;
begin
  result := TSnapshot (Objects [Index]);
end;

constructor TSnapshotList .Create ;
begin
  inherited Create;
end;

end.

