unit savepointz ;

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
  TSavepointStatus = (rsUndefined, rsOk, rsNok, rsException);
  TSavepoint = class;
  TRegressionSavepoint = class;
  TCoverageReport = class;
  TSavepointList = class;
  TSavepointEvent = procedure (aReport: TSavepoint) of Object;

  { TSavepoint }

  TSavepoint = class(TClaimableObject)
  private
    fOnReport: TSavepointEvent;
    function getAsXml : TXml ; virtual; abstract;
    function getStatusAsText : String ;
    function getTypeAsText: string; virtual; abstract;
    public
      Status: TSavepointStatus;
      Name, FileName, RefFileName, Message: String;
      timeStamp: TDateTime;
      procedure doReport; virtual abstract;
      property typeAsText: string read getTypeAsText;
      property statusAsText: String read getStatusAsText;
      procedure FromXml (aXml: TXml);
      property AsXml: TXml read getAsXml;
      property OnReport: TSavepointEvent read fOnReport write fOnReport;
      constructor Create;
  end;

  { TRegressionSavepoint }

  TRegressionSavepoint = class(TSavepoint)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
      constructor Create (aName, aFileName, aRefFileName: String);
  end;
  { TRegressionReport }

  { TCoverageReport }

  TCoverageReport = class(TSavepoint)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
  end;

  { TSavepointList }

  TSavepointList = class (TClaimableObjectList)
  private
    function getAsXml : TXml ;
    procedure SeTSavepoint(Index: integer; const Value: TSavepoint);
    function GeTSavepoint (Index: integer): TSavepoint;
  public
    property ReportItems [Index: integer]: TSavepoint read GeTSavepoint write SeTSavepoint;
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

{ TSavepoint }

function TSavepoint.getStatusAsText: String ;
begin
  case Status of
    rsUndefined: result := 'undefined';
    rsOk: result := 'ok';
    rsNok: result := 'nok';
    rsException: result := 'exception';
  end;
end;

procedure TSavepoint.FromXml (aXml : TXml );
var
  s: String;
  dXml: TXml;
begin
  if not Assigned (aXml)
  or (aXml.Name <> 'reportDetails') then
    raise Exception.Create ('TSavepoint.FromXml (aXml : TXml ); //illegal argument');
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

constructor TSavepoint .Create ;
begin
  inherited Create ;
  timeStamp := Now;
end;

{ TSavepoint }

{ TRegressionSavepoint }

function TRegressionSavepoint.getAsXml: TXml ;
begin
  result := TXml.CreateAsString('savepointDetails','');
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

function TRegressionSavepoint .getTypeAsText : string ;
begin
  result := 'regression';
end;

procedure TRegressionSavepoint.doReport;
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

constructor TRegressionSavepoint.Create (aName, aFileName, aRefFileName: String);
begin
  inherited Create;
  Name := aName;
  FileName := aFileName;
  RefFileName := aRefFileName;
end;

{ TSavepointList }

function TSavepointList.getAsXml : TXml ;
var
  x: Integer;
begin
  result := TXml.CreateAsString('savepointList', '');
  with result do
  begin
    AddXml (TXml.CreateAsTimeStamp('created', Now));
    for x := 0 to Count - 1 do
      AddXml (ReportItems[x].AsXml);
  end;
end;

procedure TSavepointList.SeTSavepoint (Index : integer; const Value: TSavepoint);
begin
  Objects [Index] := Value;
end;

function TSavepointList.GeTSavepoint (Index: integer): TSavepoint;
begin
  result := TSavepoint (Objects [Index]);
end;

constructor TSavepointList .Create ;
begin
  inherited Create;
end;

end.

