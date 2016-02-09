unit Reportz ;

{$mode objfpc}{$H+}

interface

uses Classes
   , SysUtils
   , Logz
   , ClaimListz
   ;

type
  TReportStatus = (rfUndefined, rfOk, rfNok);
  TReport = class;
  TReportList = class;

  TReport = class(TClaimableObject)
    private
    public
      Status: TReportStatus;
      Name, FileName, RefFileName: String;
      timeStamp: TDateTime;
      function AsHtml: String; Virtual; Abstract;
  end;

  { TRegressionReport }

  TRegressionReport = class(TReport)
    private
    public
      constructor Create (aFileName, aRefFileName: String);
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

{ TRegressionReport }

constructor TRegressionReport .Create (aFileName , aRefFileName : String );
begin
  inherited Create;
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

