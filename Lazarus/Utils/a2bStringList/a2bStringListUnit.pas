{
 This file is part of the apiUi project
 Copyright (c) 2009-2021 by Jan Bouwman

 See the file COPYING, included in this distribution,
 for details about the copyright.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/>.
}
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
     , crc32hashunit
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

  { TA2BStringList }

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
  HashList1,HashList2: PIntArray;
begin
  GetMem (HashList1, sizeof(integer)*(aList.Count));
  GetMem (HashList2, sizeof(integer)*(bList.Count));
  try
    for i := 0 to aList.Count - 1 do
      HashList1 [i + 1] := Integer(crc32HashString(aList[i],False, False));
    for i := 0 to bList.Count - 1 do
      HashList2 [i + 1] := Integer(crc32HashString(bList[i],False, False));
    Diff.Execute ( HashList1
                 , HashList2
                 , aList.Count
                 , bList.Count
                 );
  finally
    FreeMem (HashList1);
    FreeMem (HashList2);
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
