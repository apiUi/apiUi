program applogToLog4J;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, StrUtils, CustApp, abZipper, abUnzper, abZipTyp, abbrevia, l4jTypes, Xmlz, xmlUtilz, igGlobals,
  lazrichedit, IdStream, sqldb, oracleconnection, odbcconn, db;
var
  Param1, Param2, Param3, Param4: String;
  zipFileName, zipFileExt: String;
  xSize, maxSize, xFile: Integer;

procedure StringToArchive(var Str: String);
var
  MS: TStringStream;
  zipper: TAbZipper;
begin
  WriteLn ('Writing: ' + zipFileName + ' obpmMessages' + IntToStr (xFile));
  zipper := TAbZipper.Create(nil);
  zipper.FileName:=zipFileName {+ ifthen(xFile > 0, '_' + IntToStr (xFile), '') + zipFileExt};
  zipper.AutoSave:=True;
  try
    MS := TStringStream.Create(Str);
    try
      MS.Position:=0;
      zipper.AddFromStream('obpmMessages' + IntToStr (xFile), MS);
      Inc (xFile);
    finally
      MS.Free;
    end;
  finally
    zipper.Free;
  end;
end;

function ReadStringFromFile(aFileName: String): String;
begin
  with TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';  // Deallocates memory
      Free;
      raise;
    end;
    Free;
  end;
end;

procedure sqlLoop(aQry, xQry: TSqlQuery);
var
  Msg: l4jTypes.TMsg;
  x, f, EventDataLength: Integer;
  s, sx, nm, RowId, xEventData: String;
  field: TField;
begin
  TimeStamp:='';
  MessageId:='';
  ServiceRequestorId:='';
  ServiceId:='';
  EventType:='';
  EventDataLength:=-1;
  EventData:='';
  sx := '';

  for f := 0 to aQry.Fields.Count - 1 do
  begin
    field := aQry.Fields.Fields[f];
    nm := UpperCase(field.DisplayName);
    if (nm = 'TIMESTAMP') then
      TimeStamp:=field.AsString
    else
    begin
      if (nm = 'MESSAGEID') then
        MessageId:=field.AsString
      else
      begin
        if (nm = 'SERVICEREQUESTERID')
        or (nm = 'SERVICEREQUESTORID') then
          ServiceRequestorId:=field.AsString
        else
        begin
          if (nm = 'SERVICEID') then
            ServiceId:=field.AsString
          else
          begin
            if (nm = 'EVENTTYPE') then
              EventType:=field.AsString
            else
            begin
              if (nm = 'TUPLEID') then
                RowId:=field.AsString
              else
              begin
                if AnsiStartsStr('EVENTDATA', nm) then
                  EventData:=EventData+field.AsString
                else
                begin
                  if AnsiStartsStr('LENGTHEVENTDATA', nm) then
                    EventDataLength:=field.AsInteger
                  else
                  begin
                    sx := sx
                        + '<' + field.DisplayName + '>'
                        + field.AsString
                        + '</' + field.DisplayName + '>'
                        ;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  if Msgs.Find(MessageId, f) then
    Msg := Msgs.Msg[f]
  else
  begin
    if xSize > maxSize then
    begin
      s := Msgs.AsText;
      StringToArchive(s);
      s := ''; // free memory, just to be sure
      Msgs.Clear;
      xSize := 0;
    end;
    Msg := l4jTypes.TMsg.Create;
    Msgs.AddObject(MessageId, Msg);
  end;
  if (TimeStamp < Msg.FirstTimeStamp) then
    Msg.FirstTimeStamp := TimeStamp;
  s := '<' + EventType + 'Info>'
     + '<TimeStamp>' + TimeStamp + '</TimeStamp>'
     + '<MessageId>' + MessageId + '</MessageId>'
     + '<ServiceRequestorId>' + ServiceRequestorId + '</ServiceRequestorId>'
     + '<ServiceId>' + ServiceId + '</ServiceId>'
     + sx
     ;

  if EventDataLength > Length (EventData) then
  begin
    x := 0;
    while Length (EventData) < EventDataLength do
    begin
      xqry.SQL.Clear;
      xqry.SQL.Add ( Format ( 'select dbms_lob.substr (event_data, %d, %d) as EventData'
                            , [                                     SizeOfDataPart
                              ,                                         1 + x * SizeOfDataPart
                              ]
                            )
                   );
      xqry.SQL.Add ( 'from app_log_data');
      xqry.SQL.Add ( 'where RowId = ''' + RowId + '''');
      xqry.Open;
      xqry.First;
      if xqry.EOF then
        raise Exception.Create('EOF at selecting on RowId: ' + RowId);

      xEventData:=xqry.Fields[0].AsString;
      xqry.Close;
      if xEventData = '' then
        raise Exception.Create('Read empty string on RowId: ' + RowId);
      EventData:=EventData+xEventData;
      Inc (x);
    end;
  end;
  s := s
     + '<EventType>' + EventType + '</EventType>'
     + '</' + EventType + 'Info>'
     + LineEnding
     + '<' + EventType + '>' + EventData + '</' + EventType + '>'
     + LineEnding
     ;
  Msg.events := Msg.events + s;
  xSize := xSize + Length(s);
end;


procedure main;
var
  csFileName, sqlFileName, s, qryText: String;
  dbs: TSQLConnector;
  qry, xqry: TSQLQuery;
  x: Integer;
begin
  csFileName:=ParamStr(1);
  sqlFileName:=ParamStr(2);
  zipFileName:=ParamStr(3);
{
  zipFileExt:=ExtractFileExt(zipFileName);
  zipFileName:=Copy (zipFileName, 1, Length(zipFileName) - length (zipFileExt));
}
  if Paramcount > 3 then Param1 := ParamStr(4);
  if Paramcount > 4 then Param2 := ParamStr(5);
  if Paramcount > 5 then Param3 := ParamStr(6);
  if Paramcount > 6 then Param4 := ParamStr(7);
  Dbs := TSQLConnector.Create(nil);
  try
    with TXml.Create do
    try
      LoadFromFile(csFileName, nil);
      if TagName <> 'DataSource' then
        raise Exception.CreateFmt('%s does not contain valid ConnectionString data', [csFileName]);
      dbs.ConnectorType:=Items.XmlValueByTagDef['ConnectorType', 'Oracle'];
      dbs.DatabaseName:=Items.XmlValueByTagDef['DatabaseName', 'XE'];
      dbs.HostName:=Items.XmlValueByTag['HostName'];
      dbs.Params.Text:=ReplaceStrings( Items.XmlValueByTag['Params']
                                    , ';'
                                    , LineEnding
                                    , false
                                    , false
                                    );
      dbs.Password:=XmlUtil.SimpleEncrypt(Items.XmlValueByTag['Password']);
      dbs.Params.Text:=ReplaceStrings( dbs.Params.Text
                                    , '%pwd%'
                                    , dbs.Password
                                    , false
                                    , false
                                    );
      dbs.UserName:=Items.XmlValueByTag['UserName'];
    finally
      Free;
    end;
    dbs.Transaction := TSQLTransaction.Create(nil);
    dbs.Transaction.Action:=caNone;
    qry := TSQLQuery.Create(nil);
    qry.DataBase:=dbs;
    qry.Transaction:=dbs.Transaction;
    qry.ParseSql := False;
    qry.ReadOnly:=True;
    qry.UsePrimaryKeyAsKey:=False;
    qryText := ReplaceStrings( ReadStringFromFile(sqlFileName)
                             , '$EventData'
                             , getEventDataQuery (False)
                             , false
                             , false
                             );
    qry.SQL.Text := qryText;
    for x := 0 to Qry.Params.Count - 1 do
    begin
      if qry.Params.Items[x].Name = 'Param1' then qry.Params.Items[x].AsString:=Param1;
      if qry.Params.Items[x].Name = 'Param2' then qry.Params.Items[x].AsString:=Param2;
      if qry.Params.Items[x].Name = 'Param3' then qry.Params.Items[x].AsString:=Param3;
      if qry.Params.Items[x].Name = 'Param4' then qry.Params.Items[x].AsString:=Param4;
    end;

    xqry := TSQLQuery.Create(nil);
    xqry.DataBase:=dbs;
    xqry.Transaction:=dbs.Transaction;
    xqry.ParseSql := False;
    xqry.ReadOnly:=True;
    xqry.UsePrimaryKeyAsKey:=False;

    Msgs := TMsgList.Create;
    try
      Msgs.Sorted := True;
      qry.Open;
      qry.First;
      while not qry.EOF do
      begin
        SqlLoop (qry, xqry);
        qry.Next;
      end;
      s := Msgs.AsText;
      StringToArchive(s);
      s := ''; // free memory, just to be sure
    finally
      Msgs.Clear;
      xSize := 0;
      Msgs.Free;
    end;
  finally
    if Assigned (Dbs) then
    begin
      if Assigned (Dbs.Transaction) then
        Dbs.Transaction.Free;
      Dbs.Free;
    end;
    FreeAndNil(Qry);
  end;
end;



type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg, iniFileName: String;
begin
  ErrorMsg:='';
  // quick check parameters
  //ErrorMsg:=CheckOptions('o','');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help')
  or (ParamCount < 4) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  { add your program here }
  try
    iniFileName:=ParamStr(0) + 'ini.Xml';
    if not FileExists(iniFileName) then
      raise Exception.Create('IniFile does not exist: ' + iniFileName);
    if not FileExists(ParamStr(1)) then
      raise Exception.Create('ConnectionParamfile does not exist: ' + ParamStr(1));
    if not FileExists(ParamStr(2)) then
      raise Exception.Create('SqlQueryFile does not exist: ' + ParamStr(2));
    if FileExists(ParamStr(3)) then
      raise Exception.Create('Output zipfile already exists: ' + ParamStr(3));
    with TXml.Create do
    try
      LoadFromFile(iniFileName, nil);
      if Name <> 'appLogToLog4J' then
        raise Exception.Create ('Inifile not valid');
      maxSize := Items.XmlIntegerByTag['maxSize'];
    finally
      Free;
    end;
    Main;
  except
    on e: exception do
    begin
      WriteLn ( 'Exception: '
              +  e.Message
              );
      Terminate;
      Exit;
    end;
  end;
  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  WriteLn (ExeName);
  WriteLn ('');
  WriteLn ('appLog2Log4j sqlConnectionParams sqlFileName outputZipFile Params{0,4}');
  WriteLn ('');
  WriteLn ('');
  WriteLn ('Example:');
  WriteLn ('appLog2Log4j sqlConnectionParams.txt sqlQuery.txt applog.zip "DCN" "2014-01-15"');
  WriteLn ('');
  WriteLn ('This command will connect to the datasource according the connection params');
  WriteLn ('given in the first file and execute te sql query given in the second file');
  WriteLn ('The query result is written to a zip file which can be read by L4J');
  WriteLn ('The zipFileName may be followed by up to 4 arguments for the sql query');
  WriteLn ('');
  WriteLn ('Resultcolumns of the sql query must be mapped to program working storage elements.');
  WriteLn ('These working storage elements are:');
  WriteLn ('  :ws.TimeStamp');
  WriteLn ('  :ws.MessageId');
  WriteLn ('  :ws.ServiceRequestorId');
  WriteLn ('  :ws.ServiceId');
  WriteLn ('  :ws.EventType');
  WriteLn ('  :ws.EventData');
  WriteLn ('  :ws.Dummy');
  WriteLn ('Note:');
  WriteLn ('  Assign resultcolumns you want to add to :ws.Dummy.');
  WriteLn ('  They will show up in the result as is.');
  WriteLn ('');
  WriteLn ('Up to four arguments for the sql where-clause can be passed on the command line.');
  WriteLn ('In the query you can refere to these parameters by:');
  WriteLn ('  :Param1');
  WriteLn ('  :Param2');
  WriteLn ('  :Param3');
  WriteLn ('  :Param4');
  WriteLn ('');
  WriteLn ('Example sql query');
  WriteLn ('  select to_char(log_time,''YYYY-MM-DD"T"HH24:MM:SS"."FF'') as TimeStamp');
  WriteLn ('       , process_group_instance_id || '';'' || service_MessageId as MessageId');
  WriteLn ('       , requester_id as ServiceRequestorId');
  WriteLn ('       , service_id as ServiceId');
  WriteLn ('       , event_type as EventType');
  WriteLn ('       , $EventData');
  WriteLn ('  from APP_LOG_DATA');
  WriteLn ('  where component_name = :ws.Param1');
  WriteLn ('  and to_char(log_time,''YYYY-MM-DD'') = :ws.Param2');
  WriteLn ('');
  WriteLn ('Do not terminate the sql query with a semicolon!!');
  WriteLn ('The $EventData sprcial columnname will be expanded by the program to get more than 4000 bytes for that column.');
end;

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

