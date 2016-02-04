unit SchemaLocationz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   ;

type
  TSchemaLocations = class;
  TSchemaLocation = class;
  TSchemaLocations = class(TStringList)
  private
    n: Integer;
    fFileNames: TStringList;
    function GetSchemaLocation(Index: String): TSchemaLocation;
    function GetContent(Index: String): String;
  public
    property SchemaLocations[Index: String]
      : TSchemaLocation read GetSchemaLocation;
    property Contents[Index: String]: String read GetContent;
    function UrlToHost(aUrlName: String): String;
    function AddSchemaLocation(aDocumentName, aFileName: String): String;
    procedure Clear; override;
    constructor Create; Overload;
    destructor Destroy; override;
  end;

  TSchemaLocation = class(TObject)
  public
    DocumentName: String;
    FileName: String;
    Uri: String;
  end;

var
  SchemaLocations: TSchemaLocations;

implementation

uses sysUtils
   , igGlobals
   ;

{ TSchemaLocations }

function TSchemaLocations.AddSchemaLocation(aDocumentName, aFileName: String): String;
var
  f: Integer;
  xLoc: TSchemaLocation;
begin
  if fFileNames.Find (aFileName, f) then with fFileNames.Objects [f] as TSchemaLocation do
    result := Uri
  else
  begin
    result := aDocumentName + '?XSD=Xsd' + IntToStr (n);
    xLoc := TSchemaLocation.Create;
    xLoc.DocumentName := aDocumentName;
    xLoc.FileName := aFileName;
    xLoc.Uri := result;
    AddObject(result, xLoc);
    fFileNames.AddObject(aFileName, xLoc);
    Inc (n);
  end;
end;

procedure TSchemaLocations.Clear;
begin
  fFileNames.Clear;
  while Count > 0 do
  begin
    Objects[0].Free;
    Delete(0);
  end;
  n := 0;
end;

constructor TSchemaLocations.Create;
begin
  Sorted := True;
  Duplicates := dupError;
  fFileNames := TStringList.Create;
  with fFileNames do
  begin
    Sorted := True;
    Duplicates := dupError;
  end;
end;

destructor TSchemaLocations.Destroy;
begin
  Clear;
  fFileNames.Free;
  inherited;
end;

function TSchemaLocations.GetContent(Index: String): String;
begin
  result := ReadStringFromFile(GetSchemaLocation(Index).FileName);
end;

function TSchemaLocations.GetSchemaLocation(Index: String)
  : TSchemaLocation;
var
  f: integer;
begin
  result := nil;
  if Find(Index, f) then
    result := Objects[f] as TSchemaLocation;
end;

function TSchemaLocations.UrlToHost(aUrlName: String): String;
begin
  result := GetSchemaLocation(aUrlName).FileName;
end;

initialization

SchemaLocations := TSchemaLocations.Create;

finalization

if Assigned(SchemaLocations) then
begin
  SchemaLocations.Clear;
  SchemaLocations.Free;
end;

end.
