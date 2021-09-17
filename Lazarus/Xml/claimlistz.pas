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
unit ClaimListz ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, xmlio ;


type
{ TClaimableObject }

TClaimableObject = class (TObject)
private
  fClaimCount: Integer;
  function getClaimed: Boolean;
protected
public
  property Claimed: Boolean read getClaimed;
  procedure Claim;
  procedure Disclaim;
end;

{ TClaimableObjectList }

TClaimableObjectList = class (TJBStringList)
public
  function SaveObject (aString: String; aObject: TClaimableObject): TClaimableObject;
  function AddObject(const S: string; AObject: TObject): TClaimableObject;
  procedure Delete(Index: Integer); override;
  procedure Clear; override;
end;


implementation

{ TClaimableObjectList }

function TClaimableObjectList .SaveObject (aString : String ; aObject : TClaimableObject
  ): TClaimableObject ;
begin
  result := aObject;
  inherited AddObject(aString, aObject);
  aObject.Claim;
end;

function TClaimableObjectList .AddObject (const S : string ; AObject : TObject
  ): TClaimableObject ;
begin
  if not (AObject is TClaimableObject) then
    raise Exception.Create('Only decendents of type TClaimableObject allowed');
  result := SaveObject(S, AObject as TClaimableObject);
end;

procedure TClaimableObjectList .Delete (Index : Integer );
begin
  (Objects[Index] as TClaimableObject).Disclaim;
  inherited;
end;

procedure TClaimableObjectList .Clear ;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    (Objects[x] as TClaimableObject).Disclaim;
  Inherited;
end;

{ TClaimableObject }

function TClaimableObject .getClaimed : Boolean ;
begin
  result := (fClaimCount > 0);
end;

procedure TClaimableObject .Claim ;
begin
  Inc (fClaimCount);
end;

procedure TClaimableObject .Disclaim ;
begin
  if Assigned (Self) then
  begin
    Dec (fClaimCount);
    if fClaimCount < 1 then
      Free;
  end;
end;


end.

