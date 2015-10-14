unit vstUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses SysUtils
   , Classes
   , ParserClasses
   , Controls
   , ActnList
   , Dialogs
   , Grids
   , Graphics
   , VirtualTrees
   ;

function vstToGrid (aVst: TVirtualStringTree; OnGetText: TVSTGetTextEvent): String;
procedure vstFromGrid (aVst: TVirtualStringTree; aGrid: String; OnNewText: TVSTNewTextEvent);

implementation

procedure vstFromGrid (aVst: TVirtualStringTree; aGrid: String; OnNewText: TVSTNewTextEvent);
  function TabSepLineToStringGrid (aLine: String): TStringList;
  var
    c: Integer;
    col: String;
  begin
    result := TStringList.Create;
    col := '';
    for c := 1 to length (aLine) do
    begin
      if aLine [c] = #9 then
      begin
        result.Add(col);
        col := '';
      end
      else
        col := col + aLine[c];
    end;
    if Length (aLine) > 0 then
      result.Add(col);
  end;
var
  xNode: PVirtualNode;
  copyLines, copyColumns: TStringList;
  l, c: Integer;
begin
  if not Assigned(OnNewText) then
    raise Exception.Create('vstToGrid: OnNewText needed for vst: ' + aVst.Name);
  copyLines := TStringList.Create;
  try
    copyLines.Text := aGrid;
    xNode := aVST.GetFirst;
    // first check if first line is columnheader line
    copyColumns := TabSepLineToStringGrid (copyLines.Strings[0]);
    c := 0;
    while (c < copyColumns.Count)
    and (c < aVst.Header.Columns.Count) do
    begin
      if (copyColumns.Strings[c] <> aVst.Header.Columns.Items [c].Text) then
        raise Exception.Create('Columnheaders do not match, Operation aborted');
      Inc (c);
    end;

    l := 1; // line zero coints columnheaders
    while (l < copyLines.Count) do
    begin
      copyColumns := TabSepLineToStringGrid (copyLines.Strings[l]);
      try
        c := 0;
        while (c < copyColumns.Count)
        and (c < aVst.Header.Columns.Count) do
        begin
          OnNewText (aVst, xNode, c, copyColumns.Strings[c]);
          Inc (c);
        end;
      finally
        FreeAndNil (copyColumns);
      end;
      Inc (l);
      if Assigned (xNode) then
        xNode := aVst.GetNext(xNode);
    end;
  finally
    FreeAndNil (copyLines);
  end;
end;

function vstToGrid (aVst: TVirtualStringTree; OnGetText: TVSTGetTextEvent): String;
var
  xNode: PVirtualNode;
  xCol: Integer;
  xNLSep, xTabSep: String;
  xText: String;
begin
  if not Assigned(OnGetText) then
    raise Exception.Create('vstToGrid: OnGetText needed for vst: ' + aVst.Name);
  xNode := aVST.GetFirst;
  result := '';
  xTabSep := '';
  for xCol := 0 to aVST.Header.Columns.Count - 1 do
  begin
    if (coVisible in aVst.Header.Columns.Items[xCol].Options) then
    begin
      result := result + xTabSep + aVST.Header.Columns.Items[xCol].Text;
      xTabSep := #9;
    end;
  end;
  xNLSep := LineEnding;
  while Assigned (xNode) do
  begin
    if aVST.IsVisible [xNode] then
    begin
      result := result + xNLSep;
      xTabSep := '';
      for xCol := 0 to aVST.Header.Columns.Count - 1 do
      begin
        if (coVisible in aVst.Header.Columns.Items[xCol].Options) then
        begin
          OnGetText (aVST, xNode, xCol, ttStatic, xText);
          result := result + xTabSep + '"' + xText + '"';
          xTabSep := #9;
        end;
      end;
      xNLSep := LineEnding;
    end;
    xNode := aVST.GetNext(xNode);
  end;
end;


end.
