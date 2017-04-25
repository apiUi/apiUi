unit junitunit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, ClaimListz, WsdlProjectz, snapshotz;

function JUnitSummary (aProject: TWsdlProject; aList: TSnapshotList): String;

implementation

uses xmlz
   , logz
   , xmlio
   , xmlUtilz
   , wsdlz
   , xmlxsdparser
   ;

function JUnitSummary (aProject: TWsdlProject; aList: TSnapshotList): String;
var
  x: Integer;
  ss: TSnapshot;
  nTests, nFailures, nErrors: Integer;
begin
  result := '';
  nTests := 0;
  nFailures := 0;
  nErrors := 0;;
  for x := 0 to (aList as TSnapshotList).Count - 1 do
  begin
    if (not aProject.abortPressed) then
    begin
      ss := (aList as TSnapshotList).SnapshotItems[x];
      if (ss.Status = rsUndefined) then
        ss.doReport;
      Inc (nTests);
      case ss.Status of
        rsOk: ;
        rsNok: Inc (nFailures);
        rsException: Inc(nErrors);
      end;
    end;
  end;
  if (not aProject.abortPressed) then
  begin
    with TXml.CreateAsString('testsuites', '') do
    try
      with AddXml(TXml.CreateAsString('testsuite', '')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('name', ExtractFileName(aProject.projectFileName)));
        AddAttribute(TXmlAttribute.CreateAsTimeStamp('timestamp', ss.timeStamp));
        AddAttribute(TXmlAttribute.CreateAsString('hostname', GetHostName));
        AddAttribute(TXmlAttribute.CreateAsInteger('tests', nTests));
        AddAttribute(TXmlAttribute.CreateAsInteger('failures', nFailures));
        AddAttribute(TXmlAttribute.CreateAsInteger('errors', nErrors));
        AddAttribute(TXmlAttribute.CreateAsString('package', aProject.projectFileName));
        for x := 0 to (aList as TSnapshotList).Count - 1 do
        begin
          ss := (aList as TSnapshotList).SnapshotItems[x];
          with AddXml(TXml.CreateAsString('testcase','')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('name', ss.Name));
            AddAttribute(TXmlAttribute.CreateAsString('classname', ss.FileName));
            AddAttribute(TXmlAttribute.CreateAsString('time', '0.0'));
            case ss.Status of
              rsOk: ;
              rsNok:
              begin
                with AddXml(TXml.CreateAsString('failure', '')) do
                begin
                  AddAttribute(TXmlAttribute.CreateAsString('message', 'differences found with reference; see corresponding logs for details'));
                  AddAttribute(TXmlAttribute.CreateAsString('type', 'regression'));
                end;
              end;
              rsException:
              begin
                with AddXml(TXml.CreateAsString('error', '')) do
                begin
                  AddAttribute(TXmlAttribute.CreateAsString('message', ss.Message));
                  AddAttribute(TXmlAttribute.CreateAsString('type', 'notDefined'));
                end;
              end;
            end;
          end;
        end;
      end;
      result := Text;
    finally
      Free;
    end;
  end;
end;

end.

