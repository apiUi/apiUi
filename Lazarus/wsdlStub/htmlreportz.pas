unit htmlreportz;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, ClaimListz, WsdlProjectz, snapshotz;

function htmlReportTestSummary (aProject: TWsdlProject; aList: TSnapshotList): String;

implementation

uses htmlXmlUtilz
   , xmlz
   , logz
   , xmlio
   , xmlUtilz
   , wsdlz
   , xmlxsdparser
   ;

function htmlReportTestSummary (aProject: TWsdlProject; aList: TSnapshotList): String;
  function nbsp (aText: String): String;
  begin
    result := htmlNbsp(aText);
  end;

  procedure _coverageReport (tableXml: THtmlTableXml; aList: TSnapshotList);
  var
    xLogList: TLoglist;
    xCvrg, yCvrg: TXmlCvrg;
    x: Integer;
  begin
    if aProject.abortPressed then Exit;
    xLogList := TLogList.Create;
    try
      for x := 0 to aList.Count - 1 do
        if (aList.SnapshotItems[x].FileName <> '')
        and (not aProject.abortPressed) then
          aProject.OpenMessagesLog (aList.SnapshotItems[x].FileName, True, False, xLogList);
      if aProject.abortPressed then Exit;
      xCvrg := xLogList.PrepareCoverageReportAsXml ( allAliasses
                                                   , aProject.ignoreCoverageOn
                                                   );
      try
        if aProject.abortPressed then Exit;
        if Assigned (xCvrg) then
        begin
          xCvrg.CalculateCoverage;
          with tableXml do
          begin
            with AddTr.hleft.vtop do
            begin
              AddTd.ColSpan(1).AddB(nbsp ( 'Coverage'));
              AddTd.ColSpan(1).AddXml ( htmlHorBarChartAsXml ( xCvrg.GreenCounter
                                                             , 0
                                                             , xCvrg.RedCounter
                                                             )
                                      );
              AddTd.ColSpan(1).AddB (nbsp ( xCvrg.DisplayPercentage(False)
                                          + ' % ('
                                          + IntToStr (xCvrg.GreenCounter)
                                          + '/'
                                          + IntToStr (xCvrg.GreenCounter + xCvrg.RedCounter)
                                          + ')'
                                          )
                                    );
            end;
            for x := 0 to xCvrg.Items.Count - 1 do
            begin
              yCvrg := xCvrg.Items.XmlItems[x] as TXmlCvrg;
              if not yCvrg.isIgnored then
              begin
                with AddTr.hleft.vtop do
                begin
                  AddTd.ColSpan(1).AddB(nbsp ( htmlIndent(2) + yCvrg.Name));
                  AddTd.ColSpan(1).AddXml ( htmlHorBarChartAsXml ( yCvrg.GreenCounter
                                                                 , 0
                                                                 , yCvrg.RedCounter
                                                                 )
                                          );
                  AddTd.ColSpan(1).AddB (nbsp ( yCvrg.DisplayPercentage(False)
                                              + ' % ('
                                              + IntToStr (yCvrg.GreenCounter)
                                              + '/'
                                              + IntToStr (yCvrg.GreenCounter + yCvrg.RedCounter)
                                              + ')'
                                              )
                                        );
                end;
              end;
            end;
            AddTr.AddTd.AddP(nbsp(''));
          end;
        end;
      finally
        xCvrg.Free;
      end;
    finally
      xLogList.Free;
    end;
  end;

  procedure _regressionReport (tableXml: THtmlTableXml; aList: TSnapshotList);
  var
    xSnapshot: TSnapshot;
    x, xRed, xOrange, xGreen: Integer;
    gPerc: String;
    xXml: TXml;
  begin
    xRed := 0;
    xOrange := 0;
    xGreen := 0;
    for x := 0 to aList.Count - 1 do with aList.SnapshotItems[x] do
    case Status of
      rsUndefined: Inc(xOrange);
      rsOk: Inc (xGreen);
      rsNok: Inc(xRed);
      rsException: Inc (xOrange);
    end;
    gPerc := '';
    if (aList.Count > 0) then
      gPerc := IntToStr (Round (100 * xGreen / (xGreen + xOrange + xRed)));
    with tableXml do
    begin
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Success rate: ' + gPerc + ' %'));
        AddTd.ColSpan(1).AddXml (htmlHorBarChartAsXml(xGreen, xOrange, xRed));
      end;
      AddTr;
      with AddTr.vtop.hleft do
      begin
        AddTh.AddB ('Date and Time');
        AddTh.AddB ('Name');
        AddTh.AddB ('Verdict');
        AddTh.AddB ('Remark');
      end;
      for x := 0 to aList.Count - 1 do
      begin
        xSnapshot := aList.SnapshotItems[x];
        with AddTr.vtop.hleft do
        begin
          AddTd.AddB (nbsp (xsdFormatDateTime(xSnapshot.TimeStamp, @TIMEZONE_UTC)));
          AddTd.AddB (nbsp (xSnapshot.Name));
          AddTd.bgcolor(xSnapshot.VerdictColorAsString).AddB (nbsp(xSnapshot.Verdict));
          AddTd.AddB (nbsp(xSnapshot.Message));
        end;
      end;
      AddTr.AddTd.AddP(nbsp(''));
    end;
  end;

  procedure _reportProperties (tableXml: THtmlTableXml);
  begin
    with tableXml do
    begin
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Regular expression checks:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.checkRegExpOn.Text));
      end;
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Ignored differences:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.ignoreDifferencesOn.Text));
      end;
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Ignored additions:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.ignoreAddingOn.Text));
      end;
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Ignored removals:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.ignoreRemovingOn.Text));
      end;
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Ignored ordering on:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.ignoreOrderOn.Text));
      end;
      with AddTr.hleft.vtop do
      begin
        AddTd.ColSpan(1).AddB(nbsp ('Ignored coverage on:'));
        AddTd.ColSpan(3).AddB(nbsp (aProject.ignoreCoverageOn.Text));
      end;
      AddTr.AddTd.AddP(nbsp(''));
    end;
  end;

  function _Report (aList: TSnapshotList): String;
  var
    xXml: THtmlXml;
    tblXml: THtmlTableXml;
  begin
    result := '';
    xXml := htmlCreateXml(_ProgName, 'Test summary report');
    with xXml do
    try
      with htmlFindContentXml (xXml) do
      begin
        AddP;
        tblXml := AddTable.Border(0).WidthPerc(100);
        _regressionReport(tblXml, aList);
        _coverageReport(tblXml, aList);
        _reportProperties (tblXml);
      end;
      if not (aProject.abortPressed) then
        result := htmlXmlAsString (xXml, _wsdlStubStylesheet);
    finally
      xXml.Free;
    end;
  end;
var
  x: Integer;
begin
  with aList as TSnapshotList do
  begin
    for x := 0 to Count - 1 do with SnapshotItems[x] do
      if (Status = rsUndefined)
      and (not aProject.abortPressed) then
        doReport;
    if not aProject.abortPressed then
      result := _Report (aList as TSnapshotList);
  end;
end;

end.

