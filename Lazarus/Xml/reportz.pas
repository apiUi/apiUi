unit Reportz ;

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
  TReportStatus = (rsUndefined, rsOk, rsNok, rsException);
  TReport = class;
  TRegressionReport = class;
  TCoverageReport = class;
  TReportList = class;
  TReportEvent = procedure (aReport: TReport) of Object;

  { TReport }

  TReport = class(TClaimableObject)
  private
    fOnReport: TReportEvent;
    function getAsXml : TXml ; virtual; abstract;
    function getStatusAsText : String ;
    function getTypeAsText: string; virtual; abstract;
    public
      Status: TReportStatus;
      Name, FileName, RefFileName, Message: String;
      timeStamp: TDateTime;
      procedure doReport; virtual abstract;
      property typeAsText: string read getTypeAsText;
      property statusAsText: String read getStatusAsText;
      procedure FromXml (aXml: TXml);
      property AsXml: TXml read getAsXml;
      property OnReport: TReportEvent read fOnReport write fOnReport;
  end;

  { TRegressionReport }

  TRegressionReport = class(TReport)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
      constructor Create (aName, aFileName, aRefFileName: String);
  end;
  { TRegressionReport }

  { TCoverageReport }

  TCoverageReport = class(TReport)
    private
      function getAsXml : TXml ; override;
      function getTypeAsText: string; override;
    public
      procedure doReport; override;
  end;

  { TReportList }

  TReportList = class (TClaimableObjectList)
  private
    function getAsXml : TXml ;
    procedure SetReport(Index: integer; const Value: TReport);
    function GetReport (Index: integer): TReport;
  public
    property ReportItems [Index: integer]: TReport read GetReport write SetReport;
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

{ TReport }

function TReport.getStatusAsText: String ;
begin
  case Status of
    rsUndefined: result := 'undefined';
    rsOk: result := 'ok';
    rsNok: result := 'nok';
    rsException: result := 'exception';
  end;
end;

procedure TReport.FromXml (aXml : TXml );
var
  s: String;
  dXml: TXml;
begin
  if not Assigned (aXml)
  or (aXml.Name <> 'reportDetails') then
    raise Exception.Create ('TReport.FromXml (aXml : TXml ); //illegal argument');
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

{ TReport }

{ TRegressionReport }

function TRegressionReport.getAsXml: TXml ;
begin
  result := TXml.CreateAsString('regressionReportDetails','');
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

function TRegressionReport .getTypeAsText : string ;
begin
  result := 'regression';
end;

procedure TRegressionReport.doReport;
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

constructor TRegressionReport.Create (aName, aFileName, aRefFileName: String);
begin
  inherited Create;
  timeStamp := Now;
  Name := aName;
  FileName := aFileName;
  RefFileName := aRefFileName;
end;

{ TReportList }

function TReportList.getAsXml : TXml ;
var
  x: Integer;
begin
  result := TXml.CreateAsString('reportList', '');
  with result do
  begin
    AddXml (TXml.CreateAsTimeStamp('created', Now));
    for x := 0 to Count - 1 do
      AddXml (ReportItems[x].AsXml);
  end;
end;

procedure TReportList.SetReport (Index : integer; const Value: TReport);
begin
  Objects [Index] := Value;
end;

function TReportList.GetReport (Index: integer): TReport;
begin
  result := TReport (Objects [Index]);
end;

constructor TReportList .Create ;
begin
  inherited Create;
end;

end.

