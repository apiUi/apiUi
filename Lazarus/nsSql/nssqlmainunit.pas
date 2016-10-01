unit nsSqlMainUnit ;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes , SysUtils , FileUtil , SynEdit , SynHighlighterSQL , Forms ,
  Controls , Graphics , Dialogs , Menus , ExtCtrls , ComCtrls , StdCtrls ,
  ActnList , Grids , TacoInterface , Definez , FormIniFilez , types
  , QueryScanner
  , Xmlz
  , Xsdz
  , xmlxsdparser
  , xmlUtilz
  , BrowseHistory
  ;

type

  { TMainForm }

  TNsSqlVerb = (nsvUnknown, nsvInsert, nsvDelete, nsvUpdate);

  TProcedureThread = class;
  TMainForm = class(TForm )
    MaintainIgnoreListSqlAction : TAction ;
    CompareXmlAction : TAction ;
    MenuItem10 : TMenuItem ;
    MenuItem11 : TMenuItem ;
    MenuItem12 : TMenuItem ;
    MenuItem13 : TMenuItem ;
    MenuItem14 : TMenuItem ;
    MenuItem15 : TMenuItem ;
    MenuItem16 : TMenuItem ;
    MenuItem17 : TMenuItem ;
    MenuItem6 : TMenuItem ;
    MenuItem7 : TMenuItem ;
    MenuItem8 : TMenuItem ;
    MenuItem9 : TMenuItem ;
    ToolButton17 : TToolButton ;
    ToolButton18 : TToolButton ;
    WriteXmlAction : TAction ;
    ReadXmlAction : TAction ;
    SaveSqlAction : TAction ;
    ReadSqlAction : TAction ;
    ToolButton10 : TToolButton ;
    ToolButton11 : TToolButton ;
    ToolButton12 : TToolButton ;
    ToolButton13 : TToolButton ;
    ToolButton14 : TToolButton ;
    ToolButton15 : TToolButton ;
    ToolButton16 : TToolButton ;
    ViewSqlResultsAction : TAction ;
    ForwardAction : TAction ;
    BackwardAction : TAction ;
    QueryBackButton : TToolButton ;
    QueryForwardButton : TToolButton ;
    ToolButton4 : TToolButton ;
    ToolButton5 : TToolButton ;
    ToolButton6 : TToolButton ;
    ToolButton7 : TToolButton ;
    ToolButton8 : TToolButton ;
    ToolButton9 : TToolButton ;
    ViewAction : TAction ;
    UpdateAction : TAction ;
    DeleteAction : TAction ;
    InsertAction : TAction ;
    MaxRowsEdit : TLabeledEdit ;
    Panel1 : TPanel ;
    ShowHostSqlAction : TAction ;
    DataGrid : TStringGrid ;
    ToolBar2 : TToolBar ;
    ToolButton2 : TToolButton ;
    ColumnListbox : TListBox ;
    DefineListBox : TListBox ;
    ExecuteAction : TAction ;
    ActionList1 : TActionList ;
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
    procedure BackwardActionUpdate (Sender : TObject );
    procedure CompareXmlActionExecute (Sender : TObject );
    procedure ForwardActionUpdate (Sender : TObject );
    procedure HistoryListMenuItemClick(Sender: TObject);
    procedure BackwardActionExecute (Sender : TObject );
    procedure browseMenuItemClick (Sender : TObject );
    procedure DefineListBoxClick (Sender : TObject );
    procedure DeleteActionExecute (Sender : TObject );
    procedure DeleteActionUpdate (Sender : TObject );
    procedure ExecuteActionExecute (Sender : TObject );
    procedure ExecuteActionUpdate (Sender : TObject );
    procedure ForwardActionExecute (Sender : TObject );
    procedure InsertActionExecute (Sender : TObject );
    procedure InsertActionUpdate (Sender : TObject );
    procedure ListBoxDblClick (Sender : TObject );
    procedure ListBoxMouseDown (Sender : TObject ; Button : TMouseButton ;
      Shift : TShiftState ; X , Y : Integer );
    procedure FormCreate (Sender : TObject );
    procedure FormDestroy (Sender : TObject );
    procedure FormShow (Sender : TObject );
    procedure MaintainIgnoreListSqlActionExecute (Sender : TObject );
    procedure MenuItem2Click (Sender : TObject );
    procedure MenuItem3Click (Sender : TObject );
    procedure MenuItem5Click (Sender : TObject );
    procedure ReadSqlActionExecute (Sender : TObject );
    procedure ReadXmlActionExecute (Sender : TObject );
    procedure SaveSqlActionExecute (Sender : TObject );
    procedure WriteXmlActionExecute (Sender : TObject );
    procedure ShowHostSqlActionExecute (Sender : TObject );
    procedure UpdateActionExecute (Sender : TObject );
    procedure UpdateActionUpdate (Sender : TObject );
    procedure ViewActionExecute (Sender : TObject );
    procedure ViewActionUpdate (Sender : TObject );
    procedure ViewSqlResultsActionExecute (Sender : TObject );
  private
    fActive: Boolean;
    fProcedureThread: TProcedureThread;
    fTacoInterface: TTacoInterface;
    SqlResultsXml: TXml;
    SqlNullPresentation, SqlSourceQryString: String;
    QueryScanner: TQueryScanner;
    SqlFileName, XmlFileName: String;
    sqlQueries, SqlQueryStrings, ColumnWidths: TStringList;
    sqlQuery: TQuery;
    ignoreDifferencesOnSql: TStringList;
    ResponseTime: Extended;
    SqlBrowseDefine: TDefine;
    LineNumber: Integer;
    BrowseHistory: TBrowseHistory;
    procedure NewGridRow(aGrid: TStringGrid);
    procedure InsertDataGridRow;
    procedure UpdateDatagridRow (aRow: Integer);
    procedure DeleteRowFromDatagrid (aRow: Integer);
    procedure OnStartBlockingThread;
    procedure OnEndBlockingThread;
    function BooleanPromptDialog(aPrompt: String): Boolean;
    procedure ShowSqlDeleteScreen(ARow: Integer);
    procedure ExecuteSQL (aString: String; aRow: Integer; aVerb: TNsSqlVerb);
    function doAuthorize: Boolean;
    procedure SetGridColumnWidths(aGrid: TStringGrid);
    procedure SaveGridColumnWidths(aGrid: TStringGrid);
    procedure AddSqlQueryResult(aQuery: TQuery; aGrid: TStringGrid);
    procedure ProcessSqlResult(aResult: String);
    procedure ProcessSqlStats (aResult: String; aRow: Integer; aVerb: TNsSqlVerb);
    procedure ProcessSqlResponse(aResult: String; aRow: Integer; aVerb: TNsSqlVerb);
    procedure DoInvokeDefine(aDefine: TDefine);
    function DefineNameFound(aQuery: TQuery; DefineName: String): Boolean;
    procedure AnalyserScannerError(Sender: TObject; Data: String);
    procedure OnQueryToken(Sender: TObject);
    procedure ScannerNeedsData(Sender: TObject; var MoreData: Boolean; var Data: String);
    function HostSqlQueryStrings: String;
    procedure CreateHostSqlQueryStrings(aQuery: String);
    procedure DoSqlQuery(aString: String);
  public
    tacoHost: String;
    tacoPort: Integer;
    procedure NeedTacoHostData (Sender: TTacoInterface);
    procedure QueryDefines;
  end;

  { TProcedureThread }

  TProcedureThread = class(TThread)
  private
    fRow: Integer;
    fVerb: TNsSqlVerb;
    fString, fResponse: String;
    fForm: TMainForm;
    fTacoInterface: TTacoInterface;
  protected
    procedure Execute; override;
  public
    property Verb: TNsSqlVerb read fVerb;
    property Response: String read fResponse;
    property Row: Integer read fRow;
    constructor Create ( aForm: TMainForm
                       ; aTacoInterface: TTacoInterface
                       ; aString: String
                       ; aRow: Integer
                       ; aVerb: TNsSqlVerb
                       ); overload;
  end;

var
  MainForm : TMainForm ;

implementation

{$R *.lfm}

uses xmlio
   , PromptTacoUnit
   , ViewSqlRowUnit
   , InsertSqlUnit
   , UpdateSqlUnit
   , InvokeSqlUnit
   , ShowA2BXmlUnit
   , dualListUnit
   , strutils
   , A2BXmlz
   ;

{ TProcedureThread }

procedure TProcedureThread .Execute ;
begin
  Synchronize(fForm.OnStartBlockingThread);
  try
    fResponse := fTacoInterface.tacoCommand(fString);
  finally
    Synchronize(fForm.OnEndBlockingThread);
  end;
end;

constructor TProcedureThread.Create ( aForm: TMainForm
                                    ; aTacoInterface: TTacoInterface
                                    ; aString : String
                                    ; aRow : Integer
                                    ; aVerb : TNsSqlVerb
                                    );
begin
  Inherited Create(False);
  FreeOnTerminate := True;
  fForm := aForm;
  fTacoInterface := aTacoInterface;
  fString := aString;
  frow := aRow;
  fVerb := aVerb;
end;

{ TMainForm }

procedure TMainForm .MenuItem2Click (Sender : TObject );
begin
  Close;
end;

procedure TMainForm .MenuItem3Click (Sender : TObject );
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

procedure TMainForm .MenuItem5Click (Sender : TObject );
begin
  Application.CreateForm(TInvokeSqlForm, InvokeSqlForm);
  try
    InvokeSqlForm.Define := Defines.Defines[DefineListBox.ItemIndex];
    InvokeSqlForm.ShowModal;
  finally
    FreeAndNil(InvokeSqlForm);
  end;
end;

procedure TMainForm .ReadSqlActionExecute (Sender : TObject );
begin
  with TOpenDialog.Create(Nil) do
  try
    FileName := SqlFileName;
    DefaultExt := 'SQL';
    Filter := 'SQL File (*.SQL)|*.SQL';
    Title := 'Read SQL from file';
    if Execute then
    begin
      SqlFileName := FileName;
      queryEdit.Lines.LoadFromFile(SqlFileName);
      CreateHostSqlQueryStrings(queryEdit.Lines.Text);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .ReadXmlActionExecute (Sender : TObject );
begin
  with TOpenDialog.Create(Nil) do
  try
    FileName := XmlFileName;
    DefaultExt := 'xml';
    Filter := 'XML File (*.xml)|*.xml';
    Title := 'Read XML from file';
    if Execute then
    begin
      XmlFileName := FileName;
      SqlResultsXml.LoadFromFile(XmlFileName, nil);
      if SqlResultsXml.Name <> 'sqlSelects' then
      begin
        SqlResultsXml.Items.Clear;
        raise Exception.Create(XmlFileName +
            ' does not contain Sql results as XML data');
      end;
      ViewSqlResultsActionExecute (nil)
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .SaveSqlActionExecute (Sender : TObject );
begin
  with TSaveDialog.Create (nil) do
  try
    FileName := SqlFileName;
    DefaultExt := 'SQL';
    Filter := 'SQL File (*.SQL)|*.SQL';
    Title := 'Save SQL to file';
    if Execute then
    begin
      SqlFileName := FileName;
      queryEdit.Lines.SaveToFile(SqlFileName);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .WriteXmlActionExecute (Sender : TObject );
begin
  with TSaveDialog.Create (nil) do
  try
    FileName := XmlFileName;
    DefaultExt := 'xml';
    Filter := 'XML File (*.xml)|*.xml';
    Title := 'Write query results as XML to file';
    if Execute then
    begin
      XmlFileName := FileName;
      SaveStringToFile(XmlFileName, SqlResultsXml.Text);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .ShowHostSqlActionExecute (Sender : TObject );
begin
  if (queryEdit.SelEnd - queryEdit.SelStart) > 0 then
    CreateHostSqlQueryStrings(queryEdit.SelText)
  else
    CreateHostSqlQueryStrings(queryEdit.Text);
  xmlUtilz.ShowText('nsSql - View query with resolved filenames', HostSqlQueryStrings);
end;

procedure TMainForm .UpdateActionExecute (Sender : TObject );
var
  xCol, xRow: Integer;
begin
  xRow := DataGrid.Row;
  if xRow < DataGrid.FixedRows then
    Exit;
  for xCol := DataGrid.FixedCols to DataGrid.ColCount - 1 do
  begin
    SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].Value :=
      DataGrid.Cells[xCol, xRow];
    SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols]
      .OriginalValue := DataGrid.Cells[xCol, xRow];
    SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].UseNull :=
      Assigned(DataGrid.Objects[xCol, xRow])
      and SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols]
      .NullAllowed;
    SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols]
      .OriginalUseNull := SqlBrowseDefine.Columns.Columns
      [xCol - DataGrid.FixedCols].UseNull;
  end;
  Application.CreateForm(TUpdateSqlForm, UpdateSqlForm);
  try
    UpdateSqlForm.Caption := 'Update ' + SqlBrowseDefine.DefineName;
    UpdateSqlForm.Define := SqlBrowseDefine;
    UpdateSqlForm.ShowModal;
    if UpdateSqlForm.ModalResult = mrOK then
      ExecuteSQL ( '<SQLEXEC>'
                 + fTacoInterface.tacoString(SqlBrowseDefine.UpdateQuery[' '])
                 , xRow
                 , nsvUpdate
                 );
  finally
    FreeAndNil(UpdateSqlForm);
  end;
end;

procedure TMainForm .UpdateActionUpdate (Sender : TObject );
begin
  UpdateAction.Enabled := (not fActive);
end;

procedure TMainForm .ViewActionExecute (Sender : TObject );
begin
  if DataGrid.Row >= DataGrid.FixedRows then
  begin
    Application.CreateForm(TViewSqlRowForm, ViewSqlRowForm);
    try
      ViewSqlRowForm.Headers := DataGrid.Rows[0];
      ViewSqlRowForm.Values := DataGrid.Rows[DataGrid.Row];
      ViewSqlRowForm.StartWithColumn := DataGrid.FixedCols;
      ViewSqlRowForm.Caption := 'Viewing row ' + IntToStr
        (DataGrid.Row + 1 - DataGrid.FixedRows);
      ViewSqlRowForm.ShowModal;
    finally
      FreeAndNil(ViewSqlRowForm);
    end;
  end;
end;

procedure TMainForm .ViewActionUpdate (Sender : TObject );
begin
  ViewAction.Enabled := (not fActive)
                    and (DataGrid.Row >= DataGrid.FixedRows)
                      ;
end;

procedure TMainForm .ViewSqlResultsActionExecute (Sender : TObject );
var
  xXml: TXml;
  xXsdDescr: TXsdDescr;
  xCursor: TCursor;
begin
  xXml := TXml.Create;
  xXsdDescr := TXsdDescr.Create(1);
  try
    xXml.Name := SqlResultsXml.Name;
    xXml.LoadValues(SqlResultsXml, True);
    xXml.CheckDownline(True);
    xmlUtil.CreateXsdFromXml(xXsdDescr, xXml, True);
    XmlUtilz.ShowXml('View Sql results as Xml', xXml);
  finally
    xXml.Free;
    xXsdDescr.Free;
  end;
end;

procedure TMainForm.NewGridRow(aGrid: TStringGrid);
var
  c: Integer;
  r: Integer;
begin
  if aGrid.RowCount > 1 then { if already detailrows visible }
  begin { Scroll all details from current row down 1 row }
    aGrid.RowCount := aGrid.RowCount + 1;
    for c := 0 to aGrid.ColCount - 1 do
    begin
      for r := aGrid.RowCount - 2 downto aGrid.Row do
      begin
        aGrid.Cells[c, r + 1] := aGrid.Cells[c, r];
        aGrid.Objects[c, r + 1] := aGrid.Objects[c, r];
      end;
    end;
  end
  else
  begin { first row to display }
    aGrid.RowCount := 2;
    aGrid.FixedRows := 1;
    aGrid.Row := aGrid.RowCount - 1; { Update last row as new row }
  end;
end;

procedure TMainForm.InsertDataGridRow;
var
  c: Integer;
  r: Integer;
begin
  NewGridRow(DataGrid);
  for c := DataGrid.FixedCols to DataGrid.ColCount - 1 do
  begin
    if SqlBrowseDefine.Columns.Columns[c - DataGrid.FixedCols]
      .UseDefault then
    begin
      DataGrid.Objects[c, DataGrid.Row] := nil; { ?? }
      DataGrid.Cells[c, DataGrid.Row] := SqlBrowseDefine.Columns.Columns
        [c - DataGrid.FixedCols].DefaultValue;
    end
    else
    begin
      if SqlBrowseDefine.Columns.Columns[c - DataGrid.FixedCols]
        .UseNull then
      begin
        DataGrid.Objects[c, DataGrid.Row] := Pointer(1);
        DataGrid.Cells[c, DataGrid.Row] := { 'Null' } SqlNullPresentation;
      end
      else
      begin
        DataGrid.Objects[c, DataGrid.Row] := nil;
        DataGrid.Cells[c, DataGrid.Row] := SqlBrowseDefine.Columns.Columns
          [c - DataGrid.FixedCols].Value;
      end;
    end;
  end;
end;

procedure TMainForm.UpdateDatagridRow (aRow : Integer );
var
  xCol: Integer;
begin
  for xCol := DataGrid.FixedCols to DataGrid.ColCount - 1 do
  begin
    if SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].DoUpdate then
    begin
      if SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].UseNull then
      begin
        DataGrid.Objects[xCol, ARow] := Pointer(1);
        DataGrid.Cells[xCol, ARow] := SqlNullPresentation;
      end { if UseNul }
      else
      begin
        DataGrid.Objects[xCol, ARow] := nil;
        DataGrid.Cells[xCol, ARow] := SqlBrowseDefine.Columns.Columns [xCol - DataGrid.FixedCols].Value;
      end; { not UseNull }
    end; { if DoUpdate }
  end; { for each column }
end;

procedure TMainForm .DeleteRowFromDatagrid (aRow : Integer );
var
  c: Integer;
  r: Integer;
begin
  for c := 0 to DataGrid.ColCount - 1 do
  begin
    for r := aRow + 1 to DataGrid.RowCount - 1 do
    begin
      DataGrid.Cells[c, r - 1] := DataGrid.Cells[c, r];
      DataGrid.Objects[c, r - 1] := DataGrid.Objects[c, r];
    end;
  end;
  DataGrid.RowCount := DataGrid.RowCount - 1;
end;

procedure TMainForm .OnStartBlockingThread ;
begin
  fActive := True;
  StatusBar.Panels[0].Text := '...';
  sqlQuery.SubmitTimestamp := Now;
end;

procedure TMainForm .OnEndBlockingThread ;
begin
  ProcessSqlResponse(fProcedureThread.Response, fProcedureThread.Row, fProcedureThread.Verb);
  fActive := False;
end;

function TMainForm .BooleanPromptDialog (aPrompt : String ): Boolean ;
begin
  result := (MessageDlg(aPrompt, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
end;

procedure TMainForm .ShowSqlDeleteScreen (ARow : Integer );
var
  xCol: Integer;
begin
  for xCol := DataGrid.FixedCols to DataGrid.ColCount - 1 do
  with SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols] do
  begin
    OriginalValue := DataGrid.Cells[xCol, ARow];
    OriginalUseNull := Assigned(DataGrid.Objects[xCol, ARow]);
  end;
  if BooleanPromptDialog(SqlBrowseDefine.DeleteQuery[LineEnding]) then
  begin
    ExecuteSQL ( '<SQLEXEC>'
               + fTacoInterface.tacoString(SqlBrowseDefine.DeleteQuery[' '])
               , aRow
               , nsvDelete
               );
  end;
end;

procedure TMainForm .ExecuteSQL (aString: String; aRow: Integer; aVerb: TNsSqlVerb);
var
  resultString: String;
begin
  fProcedureThread := TProcedureThread.Create(Self, fTacoInterface, aString, aRow, aVerb);
end;

function TMainForm .doAuthorize : Boolean ;
begin
  if not fTacoInterface.Authorized then
    QueryDefines;
  result := fTacoInterface.Authorized;
end;

procedure TMainForm .SetGridColumnWidths (aGrid : TStringGrid );
  function _ColHeaderStr(aString: String): String;
  var
    c: Integer;
  begin
    result := '';
    for c := 1 to Length(aString) do
      if aString[c] = '-' then
        result := result + '_'
      else
        result := result + aString[c];
  end;
var
  X: Integer;
begin
  if aGrid.RowCount > 0 then
  begin
    for X := 0 to aGrid.FixedCols - 1 do
      aGrid.ColWidths[X] := 16;
    for X := aGrid.FixedCols to aGrid.ColCount - 1 do
      aGrid.ColWidths[X] := StrToIntDef
        (ColumnWidths.Values[_ColHeaderStr(aGrid.Cells[X, 0])],
        aGrid.DefaultColWidth);
  end;
end;

procedure TMainForm .SaveGridColumnWidths (aGrid : TStringGrid );
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

procedure TMainForm .AddSqlQueryResult (aQuery : TQuery ; aGrid : TStringGrid );
var
  r, c: Integer;
begin
  SqlResultsXml.Name := 'sqlSelects';
  with SqlResultsXml.AddXml(TXml.CreateAsString('query', '')) do
  begin
    AddXml(TXml.CreateAsString('text', aQuery.Text));
    AddXml(TXml.CreateAsString('submitTimestamp',
        xsdFormatDateTime(sqlQuery.SubmitTimestamp, @TIMEZONE_UTC)));
    AddXml(TXml.CreateAsString('resultTimestamp',
        xsdFormatDateTime(sqlQuery.ResultTimestamp, @TIMEZONE_UTC)));
    AddXml(TXml.CreateAsString('define', aQuery.DefineName));
    with AddXml(TXml.CreateAsString('result', '')) do
    begin
      for r := 1 to aGrid.RowCount - 1 do
      begin
        with AddXml(TXml.CreateAsString('row', '')) do
        begin
          for c := aGrid.FixedCols to aGrid.ColCount - 1 do
          begin
            AddXml(TXml.CreateAsString(aGrid.Cells[c, 0],
                aGrid.Cells[c, r]));
          end;
        end;
      end;
    end;
  end;
end;

procedure TMainForm .ProcessSqlResult (aResult : String );
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
    Cols := 1;
    for X := 1 to Length(Row) do
    begin
      if Row[X] = ';' then
        Inc(Cols);
    end;
    SqlBrowseDefine := sqlQuery.LastDefine;
{
    if (sqlQuery.SingleFullTableQuery) and (SqlBrowseDefine.ColClassKnown) then
      Cols := Cols + 2;
}
    DataGrid.ColCount := Cols;
    DataGrid.RowCount := sList.Count - 1;
{
    if (sqlQuery.SingleFullTableQuery) and (SqlBrowseDefine.ColClassKnown) then
      DataGrid.FixedCols := 2;
}
    StatusBar.Panels[0].Text := IntToStr(sList.Count - 2) + ' Rows';
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
      StatusBar.Panels[1].Text := 'Max data returned';
      X := X + Length('<TRUNCATED>');
    end
    else
      StatusBar.Panels[1].Text := '';

    if Copy(Row, X, Length('<RESPONSETIME>')) = '<RESPONSETIME>' then
    begin
      X := X + Length('<RESPONSETIME>') + 1;
      try
        ResponseTime := StrToFloat(fTacoInterface.decodeTacoString(Row, X));
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

    DataGrid.SetFocus;
    BrowseHistory.Add(SqlQueryStrings.Text);
    SqlSourceQryString := SqlQueryStrings.Text;
    AddSqlQueryResult(sqlQuery, DataGrid);
  finally
    FreeAndNil(sList);
  end;
end;

procedure TMainForm.ProcessSqlStats (aResult: String; aRow: Integer; aVerb: TNsSqlVerb);
{
  <SQLSTATS>
  1234567890
  <OK>
  <END-OF-DATA>
  1234567890123
}
  function _prep(s: String): String;
  begin
    result := Copy (s, 11, Length (s) - 10);
    if AnsiEndsStr('<END-OF-DATA>', result) then
      result := Copy (result, 1 , Length (result) - 13);
    if AnsiEndsStr('<OK>', result) then
      result := Copy (result, 1 , Length (result) - 4);
  end;

begin
  StatusBar.Panels[0].Text := 'SqlStats';
  StatusBar.Panels[1].Text := _prep(aResult);
  if (aRow > 0)
  and (aVerb = nsvDelete) then
  begin
    DeleteRowFromDatagrid (aRow);
  end;
  if (aRow > 0)
  and (aVerb = nsvInsert) then
  begin
    InsertDataGridRow;
  end;
  if (aRow > 0)
  and (aVerb = nsvUpdate) then
  begin
    UpdateDataGridRow(aRow);
  end;
end;

procedure TMainForm.ProcessSqlResponse(aResult: String; aRow: Integer; aVerb: TNsSqlVerb);
var
  isProcessed: Boolean;
begin
  isProcessed := False;
  sqlQuery.ResultTimestamp := Now;
  if (not isProcessed)
  and AnsiStartsText('<SQLRESULT>', aResult) then
  begin
    ProcessSqlResult(aResult);
    isProcessed := True;
  end;
  if (not isProcessed)
  and AnsiStartsText('<SQLSTATS>', aResult) then
  begin
    ProcessSqlStats(aResult, aRow, aVerb);
    isProcessed := True;
  end;
end;

procedure TMainForm .DoInvokeDefine (aDefine : TDefine );
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

function TMainForm .DefineNameFound (aQuery : TQuery ; DefineName : String
  ): Boolean ;
begin
  aQuery.LastDefine := Defines.FindDefine(LowerCase(DefineName));
  if aQuery.LastDefine = nil then
    result := False
  else
  begin
    aQuery.Text := aQuery.Text + aQuery.LastDefine.FileName;
    result := True;
  end;
end;

procedure TMainForm .AnalyserScannerError (Sender : TObject ; Data : String );
begin
  ShowMessage('Scanner: ' + Data);
end;

procedure TMainForm .OnQueryToken (Sender : TObject );
  procedure SelectTokenInEditor (aEdit: TSynEdit; aScanner: TQueryScanner);
  var
    xStart, x: Integer;
  begin
    xStart := 0;
    for x := 0 to aScanner.LineNumber - 2 do // 2 because: without current line and scanner starts with 1...
    begin
      xStart := xStart + Length (aEdit.Lines.Strings[x]) + Length (LineEnding);
    end;
    xStart := xStart + aScanner.ColumnNumber;
    aEdit.SelStart := xStart;
    aEdit.SelEnd := xStart + Length(aScanner.TokenAsString);
  end;

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
            SelectTokenInEditor (queryEdit, Scanner);
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

procedure TMainForm .ScannerNeedsData (Sender : TObject ;
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

function TMainForm .HostSqlQueryStrings : String ;
var
  x: Integer;
  xQuery: TQuery;
begin
  result := '';
  with TStringList.Create do
  try
    for x := 0 to sqlQueries.Count - 1 do
    begin
      xQuery := sqlQueries.Objects[x] as TQuery;
      if Trim(xQuery.Text) <> '' then
        Add (Trim (xQuery.Text));
    end;
    result := Text;
  finally
    Free;
  end;
end;

procedure TMainForm .CreateHostSqlQueryStrings (aQuery : String );
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
  BrowseHistory.Add(aQuery);
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

procedure TMainForm.DoSqlQuery (aString : String );
var
  X: Integer;
  SqlCommand, SendString, resultString: String;
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
                  ;
    end
    else
    begin
      SendString := SqlCommand
                  + fTacoInterface.tacoString(sqlQuery.Text)
                  ;
    end;
    fActive := True;
    ExecuteSQL(SendString, -1, nsvUnknown);
    while fActive do
    begin
      Sleep(100);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TMainForm .NeedTacoHostData (Sender : TTacoInterface );
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

procedure TMainForm .QueryDefines ;
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

procedure TMainForm .FormShow (Sender : TObject );
begin
  Invalidate;
  Application.ProcessMessages;
  QueryDefines;
end;

procedure TMainForm .MaintainIgnoreListSqlActionExecute (Sender : TObject );
var
  Srcs, Dsts: TStringList;
begin
  Srcs := TStringList.Create;
  Srcs.Sorted := True;
  Srcs.Duplicates := dupIgnore;
  Dsts := TStringList.Create;
  Dsts.Sorted := True;
  Dsts.Duplicates := dupIgnore;
  try
    Dsts.Text := ignoreDifferencesOnSql.Text;
    Application.CreateForm(TdualListForm, dualListForm);
    try
      dualListForm.Caption := 'List of columns to be ignored';
      dualListForm.DstList.Items.Text := Dsts.Text;
      dualListForm.SrcList.Items.Text := '';
      dualListForm.DstCaption := 'Ignored columns';
      dualListForm.SrcCaption := '';
      dualListForm.EmptySelectionAllowed := True;
      dualListForm.ShowModal;
      if dualListForm.ModalResult = mrOk then
      begin
        ignoreDifferencesOnSql.Text := dualListForm.DstList.Items.Text;
      end;
    finally
      FreeAndNil(dualListForm);
    end;
  finally
    Srcs.Clear;
    Srcs.Free;
    Dsts.Clear;
    Dsts.Free;
  end;
end;

procedure TMainForm.FormCreate (Sender : TObject );
begin
  sqlQueries := TStringList.Create;
  SqlQueryStrings := TStringList.Create;
  ColumnWidths := TStringList.Create;
  ignoreDifferencesOnSql := TStringList.Create;
  ignoreDifferencesOnSql.Sorted := True;
  ignoreDifferencesOnSql.Duplicates := dupIgnore;
  SqlResultsXml := TXml.Create;
  fTacoInterface := TTacoInterface.Create(nil, nil);
  fTacoInterface.NeedHostData := NeedTacoHostData;
  BrowseHistory := TBrowseHistory.Create;
//QueryBackButton.DropdownMenu := BrowseHistory.BackwardPopUpMenu;
//QueryForwardButton.DropdownMenu := BrowseHistory.ForwardPopUpMenu;
  BrowseHistory.OnMenuItemClick := HistoryListMenuItemClick;
  with TFormIniFile.Create(self, True) do
  try
    tacoHost := StringByName['tacoHost'];
    tacoPort := IntegerByName['tacoPort'];
    MaxRowsEdit.Text := StringByNameDef['queryMaxRows', '9999'];
    queryEdit.Text := StringByName['queryText'];
    SqlNullPresentation := StringByNameDef['SqlNullPresentation', 'NULL'];
    ColumnWidths.Text := StringByName['ResultColumnWidths'];
    SqlFileName := StringByName['SqlFileName'];
    XmlFileName := StringByName['XmlFileName'];
    ignoreDifferencesOnSql.Text := StringByNameDef['ignoreDifferencesOnSql', ignoreDifferencesOnSql.Text];
    Restore;
    SetGridColumnWidths(DataGrid);
  finally
    Free;
  end;
end;

procedure TMainForm .DefineListBoxClick (Sender : TObject );
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

procedure TMainForm .DeleteActionExecute (Sender : TObject );
begin
  ShowSqlDeleteScreen (DataGrid.Row);
end;

procedure TMainForm .DeleteActionUpdate (Sender : TObject );
begin
  DeleteAction.Enabled := (not fActive)
                      and (DataGrid.RowCount > DataGrid.FixedRows)
                      and (sqlQuery.SingleFullTableQuery)
                      and (SqlBrowseDefine.ColClassKnown)
                        ;
end;

procedure TMainForm .ExecuteActionExecute (Sender : TObject );
begin
  if not doAuthorize then Exit;
  if (queryEdit.SelEnd - queryEdit.SelStart) > 0 then
    DoSqlQuery(queryEdit.SelText)
  else
    DoSqlQuery(queryEdit.Text);
end;

procedure TMainForm .ExecuteActionUpdate (Sender : TObject );
begin
  ExecuteAction.Enabled := not fActive;
end;

procedure TMainForm .ForwardActionExecute (Sender : TObject );
begin
  queryEdit.Lines.Text := BrowseHistory.GetForward;
end;

procedure TMainForm .InsertActionExecute (Sender : TObject );
var
  xCol: Integer;
begin
  for xCol := DataGrid.FixedCols to DataGrid.ColCount - 1 do
  begin
    if DataGrid.Row < DataGrid.FixedRows then
    begin
      SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].Value := '';
      SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].UseNull := False;
    end
    else
    begin
      SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].Value := DataGrid.Cells[xCol, DataGrid.Row];
      SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].UseNull
        := Assigned(DataGrid.Objects[xCol, DataGrid.Row])
       and SqlBrowseDefine.Columns.Columns[xCol - DataGrid.FixedCols].NullAllowed;
    end;
  end;
  Application.CreateForm(TInsertSqlForm, InsertSqlForm);
  try
    InsertSqlForm.Caption := 'Insert into ' + SqlBrowseDefine.DefineName;
    InsertSqlForm.Define := SqlBrowseDefine;
    InsertSqlForm.ShowModal;
    if InsertSqlForm.ModalResult = mrOK then
      ExecuteSQL ( '<SQLEXEC>'
                 + fTacoInterface.tacoString(SqlBrowseDefine.InsertQuery[' '])
                 , DataGrid.Row
                 , nsvInsert
                 );
  finally
    FreeAndNil(InsertSqlForm);
  end;
end;

procedure TMainForm .InsertActionUpdate (Sender : TObject );
begin
  InsertAction.Enabled := (not fActive)
                      and Assigned (sqlQuery)
                      and sqlQuery.SingleFullTableQuery
                      and Assigned(SqlBrowseDefine)
                      and SqlBrowseDefine.ColClassKnown
                      and (DataGrid.Row >= DataGrid.FixedRows)
                        ;
end;

procedure TMainForm .ListBoxDblClick (Sender : TObject );
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

procedure TMainForm .ListBoxMouseDown (Sender : TObject ;
  Button : TMouseButton ; Shift : TShiftState ; X , Y : Integer );
var
  APoint: TPoint;
  Index: integer;
  ListBox: TListBox;
begin
end;

procedure TMainForm .browseMenuItemClick (Sender : TObject );
begin
  if DefineListBox.ItemIndex > -1 then
    queryEdit.Lines.Text := 'Select *' + LineEnding
                          + 'from '
                          + DefineListBox.Items[DefineListBox.ItemIndex]
                          + LineEnding
                          + 'browse access'
                          ;
end;

procedure TMainForm.HistoryListMenuItemClick (Sender : TObject );
begin
  queryEdit.Lines.Text := String(Sender);
end;

procedure TMainForm .ForwardActionUpdate (Sender : TObject );
begin
  ForwardAction.Enabled := BrowseHistory.OkToEnableForward;
end;

procedure TMainForm .BackwardActionUpdate (Sender : TObject );
begin
  BackwardAction.Enabled := BrowseHistory.OkToEnableBackward;
end;

procedure TMainForm .CompareXmlActionExecute (Sender : TObject );
var
  xXml: TXml;
  xA2BXml: TA2BXml;
  ShowA2BXmlForm: TShowA2BXmlForm;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := 'Read Sql results as XML';
    FileName := XmlFileName;
    DefaultExt := 'xml';
    Filter := 'XML File (*.xml)|*.xml';
    if Execute = True then
    begin
      xXml := TXml.Create;
      try
        xXml.LoadFromFile(FileName, nil);
        if xXml.Name <> 'sqlSelects' then
          raise Exception.Create(FileName +
              ' does not contain Sql results as XML data');
        if SqlResultsXml.Items.Count <> xXml.Items.Count then
          if not BooleanPromptDialog('Number of queries differ, Continue') then
            exit;
        Application.CreateForm(TShowA2BXmlForm, ShowA2BXmlForm);
        try
          xA2BXml := TA2BXml.CreateA2B('', SqlResultsXml, xXml, nil);
          try
            xA2BXml.Ignore(ignoreDifferencesOnSql, nil, nil);
            ShowA2BXmlForm.Caption := 'Differences in replies';
            ShowA2BXmlForm.ignoreDifferencesOn := ignoreDifferencesOnSql;
            ShowA2BXmlForm.Xml := xA2BXml;
            ShowA2BXmlForm.ShowModal;
          finally
            xA2BXml.Free;
          end;
        finally
          FreeAndNil(ShowA2BXmlForm);
        end;
      finally
        xXml.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm .BackwardActionExecute (Sender : TObject );
begin
  queryEdit.Lines.Text := BrowseHistory.GetBackward;
end;

procedure TMainForm .FormDestroy (Sender : TObject );
begin
  SaveGridColumnWidths(DataGrid);
  with TFormIniFile.Create(self, False) do
  try
    StringByName['tacoHost'] := tacoHost;
    IntegerByName['tacoPort'] := tacoPort;
    StringByName['queryText'] := queryEdit.Text;
    StringByName['queryMaxRows'] := MaxRowsEdit.Text;
    StringByName['SqlNullPresentation'] := SqlNullPresentation;
    StringByName['ResultColumnWidths'] := ColumnWidths.Text;
    StringByName['SqlFileName'] := SqlFileName;
    StringByName['XmlFileName'] := XmlFileName;
    StringByName['ignoreDifferencesOnSql'] := ignoreDifferencesOnSql.Text;
    Save;
  finally
    Free;
  end;
  FreeAndNil(fTacoInterface);
  FreeAndNil(sqlQueries);
  FreeAndNil(SqlQueryStrings);
  FreeAndNil(SqlResultsXml);
  FreeAndNil(ColumnWidths);
  FreeAndNil(ignoreDifferencesOnSql);
end;

end.

