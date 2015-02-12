unit a2bStringListUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
  uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  DiffUnit
     , HashUnit
     , Classes
     , SysUtils
     , StrUtils
     ;
type
  TChangeKind = (ckAdd, ckDelete, ckModify, ckCopy);

  PChangeRec =^TChangeRec;
  TChangeRec = record
    Kind     : TChangeKind; //(ckAdd, ckDelete, ckModify)
    x        : integer;     //Array1 offset (where to add, delete, modify)
    y        : integer;     //Array2 offset (what to add, modify)
    Range    : integer;     //range :-)
  end;

  TA2BStringList = class(TObject)
  private
    Diff: TDiff;
    function GetChangeCount: integer;
    function GetChanges(index: integer): TChangeRec;
  protected
  public
    property ChangeCount: integer read GetChangeCount;
    property Changes[index: integer]: TChangeRec read GetChanges; default;
    procedure Execute (aList, bList: TStringList);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TA2BStringList }

constructor TA2BStringList.Create;
begin
  Diff := TDiff.Create(nil);
end;

destructor TA2BStringList.Destroy;
begin
  Diff.Free;
  inherited;
end;

procedure TA2BStringList.Execute(aList, bList: TStringList);
var
  x, i, a, p: Integer;
  HashList1,HashList2: TList;
begin
  HashList1 := TList.create;
  HashList2 := TList.create;
  try
    HashList1.Capacity := aList.Count;
    HashList2.Capacity := bList.Count;
    for i := 0 to aList.Count - 1 do
      HashList1.add(HashLine(aList[i],False, False));
    for i := 0 to bList.Count - 1 do
      HashList2.add(HashLine(bList[i],False, False));
    Diff.Execute ( PIntArray (HashList1.List)
                 , PIntArray (HashList2.List)
                 , HashList1.Count
                 , HashList2.Count
                 );
  finally
    HashList1.Free;
    HashList2.Free;
  end;
end;

function TA2BStringList.GetChangeCount: integer;
begin
  result := Diff.ChangeCount;
end;

function TA2BStringList.GetChanges(index: integer): TChangeRec;
begin
  result := TChangeRec (Diff.Changes [index]);
end;

end.
