unit l4jTypes;

{$mode objfpc}{$H+}
interface
uses Classes
   , Xmlz
   , Express
   , sysutils
   ;

type
  TLogType = class (TObject)
    public
      Name: AnsiString;
      sTag: AnsiString;
      eTag: AnsiString;
      eyeCatchers: TXml;
  end;
  TLogTypes = class (TStringList)
  private
    function GetLogType(Index: integer): TLogType;
  published
    public
    property Types [Index: integer]: TLogType read GetLogType;
  end;

type
  TColumnType = (ctAttribute, ctElement, ctEscXmlElement, ctIndex, ctElapsed, ctSize, ctHasString, ctHasXmlValue);
  TDisplayedColumn = class (TObject)
    public
      Header, Key: String;
      ColumnType: TColumnType;
  end;

type
  TOnHaveString = procedure (aString: String
                            ) of Object;
  TOnUpdateStatus = procedure (aNumber, aTotal: Integer
                              ) of Object;


 TMsg = class (TObject)
  private
    function getAsText: String;
public
  FirstTimeStamp, LastTimeStamp: String;
  Count: Integer;
  headers: String;
  events: String;
  property AsText: String read getAsText;
  constructor Create;
  destructor Destroy; override;
end;

 TMsgList = class (TStringList)
  private
    function getMsg(Index: Integer): TMsg;
    procedure setMsg(Index: Integer; const Value: TMsg);
    function getAsText: String;
  published
public
  property Msg [Index: Integer]: TMsg read getMsg write setMsg;
  property AsText: String read getAsText;
  procedure Clear; override;
  constructor Create;
end;

 { TSl }

 function getEventData: String;
 function getEventDataQuery(aIncEventData: Boolean): String;


const
  NrOfDataParts = 4;
  SizeOfDataPart = 3990;
var
  xpMoreData, xpFetched: Boolean;
  xpScript: String;
  Msgs: TMsgList;
  TimeStamp, MessageId, UserTaskId, ServiceRequestorId, ServiceId, EventType, EventData, Dummy: String;
  fParam1, fParam2, fParam3, fParam4: String;
  EventDataParts: array [0..NrOfDataParts-1] of string;


implementation

function getEventData: String;
var
  x: Integer;
begin
  if EventData <> '' then
    result := EventData
  else
  begin
    Result := '';
    for x := 0 to NrOfDataParts - 1 do
      result := result + EventDataParts[x];
  end;
end;

function getEventDataQuery(aIncEventData: Boolean): String;
var
  x: Integer;
  xEventDataColumn: String;
begin
{
  if aIncEventData then
    xEventDataColumn := Format (', substr (event_data,1 , %d) as EventData', [SizeOfDataPart])
  else
    xEventDataColumn := '';
  result := 'substr(RowId, 1, 255) as TupleId, Length (event_data) as LengthEventData' + xEventDataColumn;
}
  if aIncEventData then
    xEventDataColumn := Format (', dbms_lob.substr (event_data, %d, 1) as EventData', [SizeOfDataPart])
  else
    xEventDataColumn := '';
  result := 'substr(RowId, 1, 255) as TupleId, dbms_lob.GetLength (event_data) as LengthEventData' + xEventDataColumn;
end;

{ TLogTypes }

function TLogTypes.getLogType(Index: integer): TLogType;
begin
  result := TLogType (Objects [index]);
end;

{ TMsg }

constructor TMsg.Create;
begin
  FirstTimeStamp := 'Z';
  LastTimeStamp:='0'; //zero
end;

destructor TMsg.Destroy;
begin
  inherited;
end;

function TMsg.getAsText: String;
var
  x: Integer;
begin
  result := '<log4j_event timestamp="' + FirstTimeStamp + '" lasttimestamp="' + LastTimeStamp + '">'
          + '<count>'+ IntToStr (Count) + '</count>'
          + '<headers>'+ headers + '</headers>'
          + '<events>'+ events + '</events>'
          + '</log4j_event>'
          + LineEnding;
          ;
end;

{ TMsgList }

procedure TMsgList.Clear;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Msg[x].Free;
  inherited;
end;

constructor TMsgList.Create;
begin
  Sorted := True;
  Duplicates := dupError;
end;

function TMsgList.getAsText: String;
var
  x: Integer;
begin
  result := '<log4j_events>';
  for x := 0 to Count - 1 do
    result := result + Msg[x].AsText;
  result := result + '</log4j_events>';
end;

function TMsgList.getMsg(Index: Integer): TMsg;
begin
  result := (Objects[Index] as TMsg);
end;

procedure TMsgList.setMsg(Index: Integer; const Value: TMsg);
begin
  Objects[Index] := Value as TMsg;
end;

end.

