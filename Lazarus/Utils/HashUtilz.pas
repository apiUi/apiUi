unit HashUtilz;

interface

function Sha1 (aString: String): String;
function Sha1Bin (aString: String): String;
function Md5 (aString: String): String;

implementation

uses DCPsha1
   , DCPmd5
   , Sysutils
   ;

function Sha1 (aString: String): String;
var
  s: AnsiString;
  x: Integer;
  hashOut: array[0..19] of byte;
begin
  result := '';
  s := aString;
  with TDCP_sha1.Create(nil) do
  try
    Init;
    UpdateStr(s);
    Final(hashOut);
    for x  := 0 to 20 - 1 do
      result := result + IntToHex(hashOut[x], 2);
  finally
    Free;
  end;
  result := LowerCase (result);
end;

function Sha1Bin (aString: String): String;
var
  s, d: AnsiString;
  x: Integer;
  hashOut: array[0..19] of byte;
  PD, PR: ^Byte;
begin
  result := '';
  s := aString;
  with TDCP_sha1.Create(nil) do
  try
    Init;
    UpdateStr(s);
    Final(hashOut);
    d := '';
    SetLength(d, 20);
    PD := Addr(hashOut[0]);
    PR := Addr(d[1]);
    for x  := 0 to 20 - 1 do
    begin
      PR^ := PD^;
      Inc(PR);
      Inc(PD);
    end;
    result := d;
  finally
    Free;
  end;
end;

function Md5 (aString: String): String;
var
  s: AnsiString;
  x: Integer;
  hashOut: array[0..19] of byte;
begin
  result := '';
  s := aString;
  with TDCP_md5.Create(nil) do
  try
    Init;
    UpdateStr(s);
    Final(hashOut);
    for x  := 0 to 20 - 1 do
      result := result + IntToHex(hashOut[x], 2);
  finally
    Free;
  end;
  result := LowerCase (result);
end;

end.
