unit Reportz ;

{$mode objfpc}{$H+}

interface

uses Classes
   , SysUtils
   , Xmlz
   , Logz
   , ClaimListz
   ;

type
  TReportStatus = (rsUndefined, rsOk, rsNok);
  TReport = class;
  TRegressionReport = class;
  TReportList = class;
  TReportRegressionEvent = procedure (aReport: TRegressionReport) of Object;

  { TReport }

  TReport = class(TClaimableObject)
    public
      Status: TReportStatus;
      Name, FileName, RefFileName, Messsage: String;
      timeStamp: TDateTime;
      procedure doReport; virtual abstract;
  end;

  { TRegressionReport }

  TRegressionReport = class(TReport)
    private
      fContext: TObject;
      fOnReport: TReportRegressionEvent;
    public
      procedure doReport; override;
      property OnReport: TReportRegressionEvent read fOnReport write fOnReport;
      constructor Create (aName, aFileName, aRefFileName: String);
  end;

  { TReportList }

  TReportList = class (TClaimableObjectList)
  private
    procedure SetReport(Index: integer; const Value: TReport);
    function GetReport (Index: integer): TReport;
  public
    property ReportItems [Index: integer]: TReport read GetReport write SetReport;
    constructor Create; overload;
  end;


implementation

{ TReport }

{ TRegressionReport }

procedure TRegressionReport.doReport;
var
  xXml: TXml;
  df: String;
begin
  Status := rsUndefined;
  Messsage := '';
  if Assigned (fOnReport) then
    fOnReport(self)
  else
    Messsage := 'Exception: no OnReportEvent assigned';
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

