unit nsSqlMainUnit ;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes , SysUtils , FileUtil , SynEdit , SynHighlighterSQL , Forms ,
  Controls , Graphics , Dialogs , Menus , ExtCtrls , ComCtrls , StdCtrls ,
  ActnList , Grids , TacoInterface , Definez , FormIniFilez , types ,
  QueryScanner , Xmlz
  ;

type

  { TmainUnit }

  TmainUnit = class(TForm )
    DataGrid : TStringGrid ;
    MachineEdit : TLabeledEdit ;
    VolumeEdit : TLabeledEdit ;
    UseEnvironmentCheckBox : TCheckBox ;
    ColumnListbox : TListBox ;
    DefineListBox : TListBox ;
    ExecuteAction : TAction ;
    ActionList1 : TActionList ;
    MaxRowsEdit : TLabeledEdit ;
    mainImageList : TImageList ;
    MainMenu1 : TMainMenu ;
    MenuItem1 : TMenuItem ;
    MenuItem2 : TMenuItem ;
    browseMenuItem : TMenuItem ;
    MenuItem3 : TMenuItem ;
    MenuItem4 : TMenuItem ;
    MenuItem5 : TMenuItem ;
    DefinesPanel : TPanel ;
    ColumnsPanel : TPanel ;
    editPanel : TPanel ;
    Panel4 : TPanel ;
    Panel5 : TPanel ;
    definesPopUpMenu : TPopupMenu ;
    Panel6 : TPanel ;
    Panel7 : TPanel ;
    Panel8 : TPanel ;
    queryEdit : TSynEdit ;
    resultPanel : TPanel ;
    queryPanel : TPanel ;
    Splitter1 : TSplitter ;
    Splitter2 : TSplitter ;
    Splitter3 : TSplitter ;
    StatusBar : TStatusBar ;
    SynSQLSyn1 : TSynSQLSyn ;
    ToolBar1 : TToolBar ;
    ToolButton1 : TToolButton ;
    EnvironmentEdit : TLabeledEdit ;
    procedure browseMenuItemClick (Sender : TObject );
    procedure DefineListBoxClick (Sender : TObject );
    procedure ExecuteActionExecute (Sender : TObject );
    procedure ListBoxDblClick (Sender : TObject );
    procedure ListBoxMouseDown (Sender : TObject ; Button : TMouseButton ;
      Shift : TShiftState ; X , Y : Integer );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
    procedure FormShow (Sender : TObject );
    procedure MenuItem2Click (Sender : TObject );
    procedure MenuItem3Click (Sender : TObject );
    procedure MenuItem5Click (Sender : TObject );
    procedure UseEnvironmentCheckBoxClick (Sender : TObject );
  private
    procedure setUseEnvVars (AValue : Boolean );
  private
    fUseEnvVars : Boolean ;
    fTacoInterface: TTacoInterface;
    SqlResultsXml: TXml;
    SqlNullPresentation: String;
    QueryScanner: TQueryScanner;
    sqlQueries, SqlQueryStrings, ColumnWidths: TStringList;
    sqlQuery: TQuery;
    InvokeDefine: TDefine;
    SqlBrowseDefine: TDefine;
    LineNumber: Integer;
    property UseEnvVars: Boolean read fUseEnvVars write setUseEnvVars;
    procedure SaveGridColumnWidths(aGrid: TStringGrid);
    procedure ProcessSqlResult(aResult: String);
    procedure DoInvokeDefine(aDefine: TDefine);
    function UseEnvironment(aFileName: String): String;
    function DefineNameFound(aQuery: TQuery; DefineName: String): Boolean;
    procedure AnalyserScannerError(Sender: TObject; Data: String);
    procedure OnQueryToken(Sender: TObject);
    procedure ScannerNeedsData(Sender: TObject; var MoreData: Boolean; var Data: String);
    procedure CreateHostSqlQueryStrings(aQuery: String);
    procedure DoSqlQuery(aString: String);
  public
    tacoHost: String;
    tacoPort: Integer;
    procedure NeedTacoHostData (Sender: TTacoInterface);
    procedure QueryDefines;
  end;

var
  mainUnit : TmainUnit ;

implementation

{$R *.lfm}

uses xmlio
   , PromptTacoUnit
   ;

{ TmainUnit }

procedure TmainUnit .MenuItem2Click (Sender : TObject );
begin
  Close;
end;

procedure TmainUnit .MenuItem3Click (Sender : TObject );
begin
  if DefineListBox.ItemIndex > -1 then
    queryEdit.Lines.Text := 'Select count(*)'
                          + LineEnding
                          + 'from '
                          + DefineListBox.Items[DefineListBox.ItemIndex]
                          + LineEnding
                          + 'browse access'
                          ;
end;

procedure TmainUnit .MenuItem5Click (Sender : TObject );
begin
  ShowMessage ('notyetimplemented');
end;

procedure TmainUnit .UseEnvironmentCheckBoxClick (Sender : TObject );
begin
  UseEnvVars := UseEnvironmentCheckBox.Checked;
end;

procedure TmainUnit .setUseEnvVars (AValue : Boolean );
begin
  fUseEnvVars := AValue ;
  UseEnvironmentCheckBox.Checked := AValue;
  MachineEdit.Visible := AValue;
  VolumeEdit.Visible := AValue;
  EnvironmentEdit.Visible := AValue;
end;

procedure TmainUnit .SaveGridColumnWidths (aGrid : TStringGrid );
  function _ColHeaderStr(aString: String): String;
  var
    c: Integer;
  begin
    result := '';
    if aString <> '' then
      for c := 1 to Length(aString) do
        if aString[c] = '-' then
          result := result + '_'
        else
          result := result + aString[c];
  end;
var
  X: Integer;
begin
  if (aGrid.RowCount > 0) then
  begin
    for X := 0 to aGrid.ColCount - 1 do
      try
        ColumnWidths.Values[_ColHeaderStr(aGrid.Cells[X, 0])] := IntToStr
          (aGrid.ColWidths[X]);
      except
      end;
  end;
end;

procedure TmainUnit .ProcessSqlResult (aResult : String );
var
  sList: TStringList;
  X: Integer;
  RowNo: Integer;
  ColNo: Integer;
  Cols: Integer;
  Row: String;
  Value: String;
  InString: Boolean;
  isQuotedString: Boolean;
begin
  sqlQuery.ResultTimestamp := Now;
  SaveGridColumnWidths(DataGrid);
  sList := TStringList.Create;
  try
    {
      <SQLRESULT>
      12345678901
      }
    DataGrid.FixedCols := 0;
    try
      sList.Text := Copy(aResult, 12, Length(aResult) - 11);
    except
      raise Exception.Create('Could not assign host response to sList');
    end;
    Row := sList.Strings[0];
    X := 1;
    Cols := 1; { Col 1: Empty }
    for X := 1 to Length(Row) do
    begin
      if Row[X] = ';' then
        Inc(Cols);
    end;
    SqlBrowseDefine := sqlQuery.LastDefine;
    if (sqlQuery.SingleFullTableQuery) and (SqlBrowseDefine.ColClassKnown) then
      Cols := Cols + 2;
    DataGrid.ColCount := Cols;
    DataGrid.RowCount := sList.Count - 1;
    if (sqlQuery.SingleFullTableQuery) and (SqlBrowseDefine.ColClassKnown) then
      DataGrid.FixedCols := 2;
    StatusBar.Panels[0].Text := IntToStr(sList.Count - 2)
      + ' Rows';
    for RowNo := 0 to sList.Count - 2 do { ignore <END-OF-DATA> line }
    begin
      for ColNo := 0 to DataGrid.FixedCols - 1 do
      begin
        DataGrid.Cells[ColNo, RowNo] := '';
        DataGrid.Objects[ColNo, RowNo] := nil; { not null }
      end;
      ColNo := DataGrid.FixedCols;
      Row := sList.Strings[RowNo];
      Value := '';
      isQuotedString := False;
      X := 1;
      while X <= Length(Row) do
      begin
        if Row[X] = '"' then
        begin
          isQuotedString := True;
          InString := True;
          Inc(X);
          while (X <= Length(Row)) and (InString) do
          begin
            if (Row[X] = '"') and ((X = Length(Row)) or (Row[X + 1] = ';'))
              then
              InString := False
            else
              Value := Value + Row[X];
            Inc(X);
          end;
        end
        else
        begin
          if Row[X] <> ';' then
          begin
            if Row[X] = '.' then
              Value := Value + { Row [x] } DecimalSeparator
            else
              Value := Value + Row[X];
          end
          else
          begin { not in string and on a separator }
            if (Value = 'NULL') and (not isQuotedString) then
            begin
              DataGrid.Objects[ColNo, RowNo] := Pointer(1); { null }
              DataGrid.Cells[ColNo, RowNo] := SqlNullPresentation;
            end
            else
            begin
              DataGrid.Objects[ColNo, RowNo] := nil;
              DataGrid.Cells[ColNo, RowNo] := Value;
            end;
            Inc(ColNo);
            Value := '';
            isQuotedString := False;
          end;
          Inc(X);
        end;
      end; { for x := 0 to lenght }
      if (Value = 'NULL') and (not isQuotedString) then
      begin
        DataGrid.Objects[ColNo, RowNo] := Pointer(1); { null }
        DataGrid.Cells[ColNo, RowNo] := SqlNullPresentation;
      end
      else
      begin
        DataGrid.Objects[ColNo, RowNo] := nil;
        DataGrid.Cells[ColNo, RowNo] := Value; { show last value }
      end;
    end; { for RowNo := 0 to ... }
    DataGrid.Col := DataGrid.FixedCols;
    SetGridColumnWidths(DataGrid);
    if DataGrid.RowCount > 1 then
    begin
      DataGrid.Row := 1;
      DataGrid.FixedRows := 1;
      DataGrid.LeftCol := DataGrid.FixedCols;
    end;
    RowNo := sList.Count - 1;
    Row := sList.Strings[RowNo];
    X := 1;
    if Copy(Row, 1, Length('<TRUNCATED>')) = '<TRUNCATED>' then
    begin
      DataSheetStatusBar.Panels[1].Text := 'Max data returned';
      X := X + Length('<TRUNCATED>');
    end
    else
      DataSheetStatusBar.Panels[1].Text := '';

    if Copy(Row, X, Length('<RESPONSETIME>')) = '<RESPONSETIME>' then
    begin
      X := X + Length('<RESPONSETIME>') + 1;
      try
        ResponseTime := StrToFloat(GetString(Row, X));
        StatusBar.Panels.Items[1].Text := Format
          ('Responsetime: %.6f seconds', [ResponseTime / 1000000]);
      except
        StatusBar.Panels.Items[1].Text := 'Error formatting responsetime';
      end;
    end
    else
    begin
      StatusBar.Panels.Items[1].Text :=
        'Responsetime not delivered by server';
    end;

    DataTabSheet.TabVisible := True;
    MainPageControl.ActivePage := DataTabSheet;
    DataGrid.SetFocus;
//      BrowseHistory.Add(SqlQueryStrings.Text);
    SqlSourceQryString := SqlQueryStrings.Text;
    AddSqlQueryResult(sqlQuery, DataGrid);
  finally
    FreeAndNil(sList);
  end;
end;

procedure TmainUnit .DoInvokeDefine (aDefine : TDefine );
  procedure _evalResp (aDefine: TDefine; aResponse: String);
  var
    Offset: Integer;
    EndPos: Integer;
    xDefine: TDefine;
    sEnd: Integer;
    Column: TColumn;
    tList: TStringList;
    ColumnName: String;
    Col: Integer;
    xString: String;
    function _NextString(aString: String; var aOffset: Integer): String;
    var
      xStart: Integer;
    begin
      xStart := aOffset;
      while (aOffset <= Length(aString)) and (aString[Offset] <> ';') do
        Inc(aOffset);
      result := Copy(aString, xStart, aOffset - xStart);
      Inc(aOffset);
    end;

  begin
    Offset := 1;
    EndPos := Pos('<OK>', aResponse);
    if EndPos <= 0 then
      raise Exception.Create('<SQLCOLNAMES> without <OK>');
    tList := TStringList.Create;
    try
      tList.Text := aResponse;
      for Col := 0 to tList.Count - 2 do
      begin
        xString := tList.Strings[Col];
        Offset := 1;
        Column := TColumn.Create;
        if (xString[Offset] = '<') then
        begin
          while (Offset < Length(xString)) and (xString[Offset] <> '>') do
            Inc(Offset);
          Inc(Offset);
        end;
        Column.ColName := LowerCase(_NextString(xString, Offset));
        Column.ColSize := StrToInt(_NextString(xString, Offset));
        Column.NullAllowed := (_NextString(xString, Offset) = 'Y');
        Column.DataType := StrToInt(_NextString(xString, Offset));
        Column.Precision := StrToInt(_NextString(xString, Offset));
        Column.Scale := StrToInt(_NextString(xString, Offset));
        Column.DataTimeStartField := StrToInt(_NextString(xString, Offset));
        Column.DateTimeEndField := StrToInt(_NextString(xString, Offset));
        Column.KeySeqNumber := StrToInt(_NextString(xString, Offset));
        Column.DateTimeQualifier := LowerCase(_NextString(xString, Offset));
        if Offset <= Length(xString) then
          Column.DefaultClass := LowerCase(_NextString(xString, Offset));
        if Offset <= Length(xString) then
          Column.DefaultValue := LowerCase(_NextString(xString, Offset));
        if Offset <= Length(xString) then
        begin
          Column.ColClass := LowerCase(_NextString(xString, Offset));
          aDefine.ColClassKnown := True;
        end;
        if Offset <= Length(xString) then
          Column.PictureClause := LowerCase(_NextString(xString, Offset));
        Column.UseDefault := False;
        aDefine.Columns.AddObject(Column.ColName, Column);
      end; { for every COLUMN }
    except
      FreeAndNil(tList);
    end;
  end;
begin
  if aDefine.Columns.Count = 0 then
  begin
    _evalResp ( aDefine
              , fTacoInterface.tacoCommand ( '<SQLINVOKE>'
                                           + fTacoInterface.tacoString(aDefine.FileName)
                                           )
              );
  end; { Columns not yet read }
end;

function TmainUnit .UseEnvironment (aFileName : String ): String ;
begin
  result := aFileName;
end;

function TmainUnit .DefineNameFound (aQuery : TQuery ; DefineName : String
  ): Boolean ;
begin
  aQuery.LastDefine := Defines.FindDefine(LowerCase(DefineName));
  if aQuery.LastDefine = nil then
    result := False
  else
  begin
    aQuery.Text := aQuery.Text + UseEnvironment(aQuery.LastDefine.FileName);
    result := True;
  end;
end;

procedure TmainUnit .AnalyserScannerError (Sender : TObject ; Data : String );
begin
  ShowMessage('Scanner: ' + Data);
end;

procedure TmainUnit .OnQueryToken (Sender : TObject );
var
  Scanner: TQueryScanner;
begin
  Scanner := Sender as TQueryScanner;
  try
    case Scanner.Token of
      _ANY:
        begin
          sqlQuery.Text := sqlQuery.Text + Scanner.TokenAsString;
        end;
      _SELECTSTAR:
        begin
          sqlQuery.SqlSelectStarFrom := True;
        end;
      _DEFINE_NAME:
        begin
          if sqlQuery.DefineName <> '' then
            sqlQuery.DefineName :=  sqlQuery.DefineName + ',';
          sqlQuery.DefineName := sqlQuery.DefineName + Copy (Scanner.TokenAsString, 2, 100);
          if not(DefineNameFound(sqlQuery, Scanner.TokenAsString)) then
          begin
            queryEdit.SelStart := 1;
            queryEdit.SelEnd := 2;
            queryEdit.SelText := 'impl seltext';
{
            MemoSetSelectedText(queryEdit, Scanner.LineNumber,
              Scanner.ColumnNumber, system.Length(Scanner.TokenAsString));
}
            raise Exception.Create('DefineName ' + Scanner.TokenAsString +
                ' not known (Line: ' + IntToStr(Scanner.LineNumber)
                + ' Column: ' + IntToStr(Scanner.ColumnNumber) + ')');
          end
          else
          begin
            Inc(sqlQuery.NumberOfViewsInQuery);
            DoInvokeDefine(sqlQuery.LastDefine);
          end;
        end;
      _WHITESPACE:
        begin
          sqlQuery.Text := sqlQuery.Text + ' ';
        end;
      _STRING:
        begin
          sqlQuery.Text := sqlQuery.Text + '''' + Copy
            (Scanner.TokenAsString, 2,
            Length(Scanner.TokenAsString) - 2) + '''';
        end;
      _SEPARATOR:
        begin
          sqlQuery := TQuery.Create;
          sqlQueries.AddObject('', sqlQuery);
        end;
    end;
  except
    raise ;
  end;
end;

procedure TmainUnit .ScannerNeedsData (Sender : TObject ;
  var MoreData : Boolean ; var Data : String );
begin
  if LineNumber >= SqlQueryStrings.Count then
    MoreData := False
  else
  begin
    Data := SqlQueryStrings[LineNumber];
    Inc(LineNumber);
  end;
end;

procedure TmainUnit .CreateHostSqlQueryStrings (aQuery : String );
var
  X: Integer;
begin
  for X := 0 to sqlQueries.Count - 1 do
    sqlQueries.Objects[X].Free;
  sqlQueries.Clear;
  sqlQuery := TQuery.Create;
  sqlQueries.AddObject('', sqlQuery);
  SqlQueryStrings.Text := aQuery;
  LineNumber := 0;
  QueryScanner := TQueryScanner.Create;
  QueryScanner.OnNeedData := ScannerNeedsData;
  QueryScanner.OnError := AnalyserScannerError;
  QueryScanner.OnToken := OnQueryToken;
  try
    QueryScanner.Execute;
  finally
    FreeAndNil(QueryScanner);
  end;
  for x := sqlQueries.Count - 1 downto 0 do
  begin
    with sqlQueries.Objects[x] as TQuery do
    begin
      Text := Trim (Text);
      if Text = '' then
      begin
        Free;
        sqlQueries.Delete(x);
      end;
    end;
  end;
  if sqlQueries.Count = 0 then
    raise Exception.Create('No SQL queries available');
//BrowseHistory.Add(aQuery);
  for X := 0 to sqlQueries.Count - 1 do
  begin
    with sqlQueries.Objects[X] as TQuery do
    begin
      if (Uppercase(Copy(Text, 1, Length('Select'))) = 'SELECT')
      or (SqlSelectStarFrom) then
      begin
        SingleFullTableQuery := (SqlSelectStarFrom) and (NumberOfViewsInQuery = 1);
        if SqlSelectStarFrom then
        begin
          if SingleFullTableQuery then
            Text := 'select ' + LastDefine.SelectStarColums +
              ' from' + Text
          else
            Text := 'select * from' + Text;
        end;
      end
    end;
  end;
  sqlQuery := sqlQueries.Objects[0] as TQuery;
end;

procedure TmainUnit.DoSqlQuery (aString : String );
var
  X: Integer;
  SqlCommand, SendString: String;
begin
  SqlResultsXml.Items.Clear;
  CreateHostSqlQueryStrings(aString);
  for X := 0 to sqlQueries.Count - 1 do
  begin
    sqlQuery := sqlQueries.Objects[X] as TQuery;
    if (Uppercase(Copy(sqlQuery.Text, 1, Length('Select'))) = 'SELECT') then
      SqlCommand := '<SQLSELECT>'
    else
    begin
      if (Uppercase(Copy(sqlQuery.Text, 1, Length('Control'))) = 'CONTROL') then
        SqlCommand := '<SQLCONTROL>'
      else
        SqlCommand := '<SQLEXEC>';
    end;
    if SqlCommand = '<SQLSELECT>' then
    begin
      SendString := SqlCommand
                  + fTacoInterface.tacoString(MaxRowsEdit.Text)
                  + fTacoInterface.tacoString('999999999')
                  + fTacoInterface.tacoString(sqlQuery.Text)
                  + '<END-OF-DATA>'
                  ;
    end
    else
    begin
      SendString := SqlCommand
                  + fTacoInterface.tacoString(sqlQuery.Text)
                  + '<END-OF-DATA>'
                  ;
    end;
    ShowMessage (fTacoInterface.tacoCommand(SendString));
{
    if sqlQueries.Count > 1 then
      ShowXml('View Sql results as Xml', SqlResultsXml);
}
  end;
end;

procedure TmainUnit .NeedTacoHostData (Sender : TTacoInterface );
var
  xForm: TPromptTacoForm;
begin
  Application.CreateForm(TPromptTacoForm, xForm);
  try
    xForm.Address := tacoHost;
    xForm.Port := tacoPort;
    xForm.ShowModal;
    if xForm.ModalResult = mrOk then
    begin
      tacoHost := xForm.Address;
      Sender.Host := tacoHost;
      tacoPort := xForm.Port;
      Sender.Port := tacoPort;
      Sender.Authorisation := xForm.Authorisation;
      Sender.UserName := xmlio.GetUserName;
    end;
  finally
    FreeAndNil(xForm);
  end;
end;

procedure TmainUnit .QueryDefines ;
  procedure _ProcessDefines (aString: String);
  var
    Offset: Integer;
    EndPos: Integer;
    xDefine: TDefine;
    sStart: Integer;
  begin
    Offset := 1;
    EndPos := Pos('<OK><END-OF-DATA>', aString);
    if EndPos <= 0 then
      raise Exception.Create('<DEFINES> without <OK><END-OF-DATA>');
    // <Int>=define;TandemFileName<Int>...
    while Offset < EndPos do
    begin
      xDefine := TDefine.Create;
      while (Offset < EndPos) and (aString[Offset] <> '=') do
        Inc(Offset);
      sStart := Offset;
      while (Offset < EndPos) and (aString[Offset] <> ';') do
        Inc(Offset);
      xDefine.DefineName := LowerCase(Copy(aString, sStart, Offset - sStart));
      Inc(Offset);
      sStart := Offset;
      while (Offset < EndPos) and (aString[Offset] <> '<') do
        Inc(Offset);
      xDefine.FileName := Copy(aString, sStart, Offset - sStart);
      Defines.AddObject(xDefine.DefineName, xDefine);
    end;
    DefineListBox.Items.Text := Defines.Text;
  end;
begin
  Defines.Clear;
  _ProcessDefines (fTacoInterface.tacoCommand('<DEFINES>'));
end;

procedure TmainUnit .FormShow (Sender : TObject );
begin
  QueryDefines;
end;

procedure TmainUnit .FormCreate (Sender : TObject );
begin
  sqlQueries := TStringList.Create;
  SqlQueryStrings := TStringList.Create;
  ColumnWidths := TStringList.Create;
  SqlResultsXml := TXml.Create;
  fTacoInterface := TTacoInterface.Create(nil, nil);
  fTacoInterface.NeedHostData := NeedTacoHostData;
  with TFormIniFile.Create(self, True) do
  try
    tacoHost := StringByName['tacoHost'];
    tacoPort := IntegerByName['tacoPort'];
    MaxRowsEdit.Text := StringByName['queryMaxRows'];
    queryEdit.Text := StringByName['queryText'];
    UseEnvVars := BooleanByNameDef['useEnvVars', False];
    MachineEdit.Text := StringByName['MachineName'];
    VolumeEdit.Text := StringByName['VolumeName'];
    EnvironmentEdit.Text := StringByName['EnvironmentName'];
    ColumnWidths.Text := StringByName['ResultColumnWidths'];
    SqlNullPresentation := StringByNameDef['SqlNullPresentation', 'NULL'];
    Restore;
  finally
    Free;
  end;
end;

procedure TmainUnit .DefineListBoxClick (Sender : TObject );
var
  xDefine: TDefine;
begin
  if DefineListBox.ItemIndex > -1 then
  begin
    xDefine := Defines.Defines[DefineListBox.ItemIndex];
    DoInvokeDefine(xDefine);
    ColumnListBox.Items.Text := xDefine.Columns.Text;
  end;
end;

procedure TmainUnit .ExecuteActionExecute (Sender : TObject );
begin
  if (queryEdit.SelEnd - queryEdit.SelStart) > 0 then
    DoSqlQuery(queryEdit.SelText)
  else
    DoSqlQuery(queryEdit.Text);
end;

procedure TmainUnit .ListBoxDblClick (Sender : TObject );
var
  ListBox: TListBox;
  xString: String;
  X: Integer;
  xSeparator: String;
begin
  ListBox := Sender as TListBox;
  xString := '';
  xSeparator := '';
  for X := 0 to ListBox.Count - 1 do
  begin
    if (ListBox.Selected[X] = True) or (X = ListBox.ItemIndex) then
    begin
      xString := xString + xSeparator + ListBox.Items.Strings[X];
      xSeparator := ', ';
    end;
  end;
  queryEdit.SelText := xString;
  queryEdit.SetFocus;
end;

procedure TmainUnit .ListBoxMouseDown (Sender : TObject ;
  Button : TMouseButton ; Shift : TShiftState ; X , Y : Integer );
var
  APoint: TPoint;
  Index: integer;
  ListBox: TListBox;
begin
  if Sender is TListBox then
  begin
    if Button = mbRight then
    begin
      ListBox := Sender as TListBox;
      APoint.X := X;
      APoint.Y := Y;
      Index := ListBox.ItemAtPos(APoint, True);
      if Index > -1 then
      begin
        ListBox.Selected[Index] := True;
        if Assigned(ListBox.OnClick) then
          ListBox.OnClick(ListBox);
      end;
    end;
  end;
end;

procedure TmainUnit .browseMenuItemClick (Sender : TObject );
begin
  if DefineListBox.ItemIndex > -1 then
    queryEdit.Lines.Text := 'Select *' + LineEnding
                          + 'from '
                          + DefineListBox.Items[DefineListBox.ItemIndex]
                          + LineEnding
                          + 'browse access'
                          ;
end;

procedure TmainUnit .FormDestroy (Sender : TObject );
begin
  with TFormIniFile.Create(self, False) do
  try
    StringByName['tacoHost'] := tacoHost;
    IntegerByName['tacoPort'] := tacoPort;
    StringByName['queryText'] := queryEdit.Text;
    StringByName['queryMaxRows'] := MaxRowsEdit.Text;
    BooleanByName['useEnvVars'] := UseEnvVars;
    StringByName['MachineName'] := MachineEdit.Text;
    StringByName['VolumeName'] := VolumeEdit.Text;
    StringByName['EnvironmentName'] := EnvironmentEdit.Text;
    StringByName['ResultColumnWidths'] := ColumnWidths.Text;
    StringByNameDef['SqlNullPresentation', 'NULL'] := SqlNullPresentation;
    Save;
  finally
    Free;
  end;
  FreeAndNil(fTacoInterface);
  FreeAndNil(sqlQueries);
  FreeAndNil(SqlQueryStrings);
  FreeAndNil(SqlResultsXml);
  FreeAndNil(ColumnWidths);
end;

end.

