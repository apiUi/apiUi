{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit CustScanner;

interface
uses Classes, SysUtils, Bind;
const nl = #10;  (* newline character *)
const max_chars = 2*1048576;
const max_matches = 4096;
const max_rules   = 1024;

type TOnNeedDataEvent = procedure ( Sender:TObject
                                   ; var MoreData: Boolean
                                   ; var Data: String
                                   ) of Object;
type EScannerException = class (Exception);
type TOnScannerErrorEvent = procedure ( Sender:TObject
                                      ; Data: String
                                      ) of Object;

type TCustScanner = class (TObject)
protected
  FOnToken: TNotifyEvent;
  FOnOutput: TNotifyEvent;
  FOnEcho: TNotifyEvent;
  FOnDefaultAction: TNotifyEvent;
  FOnNeedData: TOnNeedDataEvent;
  FOnError: TOnScannerErrorEvent;
  FTokenString: String;      (* matched text *)
  FData: String;      (* current input line *)
  FLineNumber, FColumnNumber, FOffset : Integer;     (* current input position *)
  yyOffset: Integer;
  yyColumnNumber: Integer;
  yystate    : Integer; (* current state of lexical analyzer *)
  yyactchar  : Char;    (* current character *)
  yylastchar : Char;    (* last matched character (#0 if none) *)
  yyrule     : Integer; (* matched rule *)
  yyreject   : Boolean; (* current match rejected? *)
  yydone     : Boolean; (* yylex return value set? *)
  FToken     : Integer; (* yylex return value *)
  yystext            : String;
  yysstate, yylstate : Integer;
  yymatches          : Integer;
  yystack            : array [1..max_matches] of Integer;
  yypos              : array [1..max_rules] of Integer;
  yysleng            : Integer;
  bufptr : Integer;
  buf    : array [1..max_chars] of Char;
  procedure RaiseException ( msg : String );
  function Getyyleng: Integer;
  procedure Setyyleng (leng: Integer);
  function get_char : Char;
  procedure unget_char ( c : Char );
  procedure Output (s: String);
  procedure Echo;
  procedure yymore;
  procedure yyless ( n : Integer );
  procedure reject;
  procedure return ( n : Integer );
  procedure returnc ( c : Char );
  function yywrap : Boolean;
  procedure yynew;
  procedure yyscan;
  procedure yymark ( n : Integer );
  procedure yymatch ( n : Integer );
  function yyfind ( var n : Integer ) : Boolean;
  function yydefault : Boolean;
  function GetTokenAsInteger: Integer;
  function GetTokenAsFloat: Extended;
  procedure yyclear;

public
  property OnNeedData: TOnNeedDataEvent read FOnNeedData write FOnNeedData;
  property OnToken: TNotifyEvent read FOnToken write FOnToken;
  property OnOutput: TNotifyEvent read FOnOutput write FOnOutput;
  property OnEcho: TNotifyEvent read FOnEcho write FOnEcho;
  property OnDefaultAction: TNotifyEvent read FOnDefaultAction write FOnDefaultAction;
  property OnError: TOnScannerErrorEvent read FOnError write FOnError;
  property Token: Integer read FToken;
  property TokenAsString: String read FTokenString;
  property TokenAsInteger: Integer read GetTokenAsInteger;
  property TokenAsFloat: Extended read GetTokenAsFloat;
  property yyleng: Integer read Getyyleng write Setyyleng ;
  property LineNumber: Integer read FLineNumber;
  property ColumnNumber: Integer read FColumnNumber;
  property Offset: Integer read FOffset;
  procedure start ( state : Integer );
  function CurrentState: Integer;
  function yylex: Integer; virtual ;
  procedure DefaultAction ( c : Char );
  procedure Execute;
  constructor Create;
end;

implementation

constructor TCustScanner.Create;
begin
  yyclear;
end;

function TCustScanner.GetTokenAsInteger: Integer;
begin
  try
    result := StrToInt (FTokenString);
  except
    RaiseException ( 'could not convert <'
                   + FTokenString
                   + '> to integer'
                   );
  end;
end;

function TCustScanner.GetTokenAsFloat: Extended;
begin
  try
    result := StrToFloat (FTokenString);
  except
    RaiseException ( 'could not convert <'
                   + FTokenString
                   + '> to integer'
                   );
  end;
end;

function TCustScanner.Getyyleng: Integer;
begin
  result := Length (FTokenString);
end;

procedure TCustScanner.Setyyleng (leng: Integer);
begin
  SetLength (FTokenString, leng);
end;

procedure TCustScanner.RaiseException ( msg : String );
begin
  if Assigned (FOnError) then
    FOnError (Self, msg)
  else
    Raise EScannerException.Create (msg)
  ;
end(*RaiseException*);

function TCustScanner.get_char : Char;
var
  i : Integer;
  MoreData: Boolean;
begin
  if (bufptr=0) then
  begin
    MoreData := True;
    FOnNeedData (Self, MoreData, FData);
    if MoreData then
    begin
      if length(FData) >= max_chars then
        raise Exception.CreateFmt( 'Line %d, Length %d exceeds max lenght of %d: %s...'
                                 , [ FLineNumber + 1
                                   , length(FData)
                                   , max_chars
                                   , Copy (FData, 1, 100)
                                   ]
                                 );
      inc(FLineNumber);
      inc (yyOffset);
      yyColumnNumber := 1;
      buf[1] := nl;
      for i := 1 to length(FData) do
        buf[i+1] := FData[length(FData)-i+1];
      inc(bufptr, length(FData)+1);
    end;
  end;
  if bufptr>0 then
  begin
    get_char := buf[bufptr];
    dec(bufptr);
    inc (yyOffset);
    inc(yyColumnNumber);
  end
  else
    get_char := #0;
end(*get_char*);

procedure TCustScanner.unget_char ( c : Char );
begin
  if bufptr=max_chars then
    RaiseException('input buffer overflow');
  inc(bufptr);
  dec (yyOffset);
  dec(yyColumnNumber);
  buf[bufptr] := c;
end(*unget_char*);

procedure TCustScanner.DefaultAction ( c : Char );
begin
  if Assigned (FOnDefaultAction) then
  begin
    yyleng := 1;
    FTokenString [1] := c;
    FOnDefaultAction (Self);
  end;
end(*DefaultAction*);


function TCustScanner.yylex : Integer;
begin
  result := 1;
end(*yylex*);

procedure TCustScanner.Execute;
var
  loc_yysstate, loc_yylstate: Integer;
begin
  if not Assigned (FOnNeedData) then
    RaiseException ('No procedure assigned to OnNeedData')
  else
    loc_yysstate := yysstate;
    loc_yylstate := yylstate;
    yyclear;
    yysstate := loc_yysstate;
    yylstate := loc_yylstate;
    while yylex > 0 do
      if Assigned (FOnToken) then
      begin
        FOffset := yyOffset - yyleng;
        FColumnNumber := yyColumnNumber - yyleng;
        FOnToken (Self);
      end;
end;

procedure TCustScanner.Output (s: String);
begin
  FTokenString := s;
  if Assigned(FOnOutput) then FOnOutput(Self);
end;

procedure TCustScanner.Echo;
begin
  if Assigned(FOnEcho) then FOnEcho(Self);
end(*Echo*);

procedure TCustScanner.yymore;
  begin
    yystext := FTokenString;
  end(*yymore*);

procedure TCustScanner.yyless ( n : Integer );
  var i : Integer;
  begin
    for i := yyleng downto n+1 do
      unget_char(FTokenString[i]);
    yyleng := n;
  end(*yyless*);

procedure TCustScanner.reject;
  var i : Integer;
  begin
    yyreject := true;
    for i := yyleng+1 to yysleng do
      FTokenString := FTokenString+get_char;
    dec(yymatches);
  end(*reject*);

procedure TCustScanner.return ( n : Integer );
  begin
    FToken := n;
    yydone := true;
  end(*return*);

procedure TCustScanner.returnc ( c : Char );
  begin
    FToken := ord(c);
    yydone := true;
  end(*returnc*);

function TCustScanner.CurrentState: Integer;
begin
  result := yysstate;
end;

procedure TCustScanner.start ( state : Integer );
  begin
    yysstate := state;
  end(*start*);

(* yywrap: *)

function TCustScanner.yywrap : Boolean;
  begin
    yywrap := true;
  end(*yywrap*);

(* Internal routines: *)

procedure TCustScanner.yynew;
  begin
    if yylastchar<>#0 then
      if yylastchar=nl then
        yylstate := 1
      else
        yylstate := 0;
    yystate := yysstate+yylstate;
    FTokenString  := yystext;
    yystext := '';
    yymatches := 0;
    yydone := false;
  end(*yynew*);

procedure TCustScanner.yyscan;
  begin
{    if yyleng=255 then RaiseException('FTokenString overflow');}
    yyactchar := get_char;
    yyleng := yyleng + 1;
    FTokenString[yyleng] := yyactchar;
  end(*yyscan*);

procedure TCustScanner.yymark ( n : Integer );
  begin
    if n>max_rules then RaiseException('too many rules');
    yypos[n] := yyleng;
  end(*yymark*);

procedure TCustScanner.yymatch ( n : Integer );
  begin
    inc(yymatches);
    if yymatches>max_matches then RaiseException('match stack overflow');
    yystack[yymatches] := n;
  end(*yymatch*);

function TCustScanner.yyfind ( var n : Integer ) : Boolean;
  begin
    yyreject := false;
    while (yymatches>0) and (yypos[yystack[yymatches]]=0) do
      dec(yymatches);
    if yymatches>0 then
      begin
        yysleng := yyleng;
        n       := yystack[yymatches];
        yyless(yypos[n]);
        yypos[n] := 0;
        if yyleng>0 then
          yylastchar := FTokenString[yyleng]
        else
          yylastchar := #0;
        yyfind := true;
      end
    else
      begin
        yyless(0);
        yylastchar := #0;
        yyfind := false;
      end
  end(*yyfind*);

function TCustScanner.yydefault : Boolean;
  begin
    yyreject := false;
    yyactchar := get_char;
    if yyactchar<>#0 then
      begin
        DefaultAction(yyactchar);
        yydefault := true;
      end
    else
      begin
        yylstate := 1;
        yydefault := false;
      end;
    yylastchar := yyactchar;
  end(*yydefault*);

procedure TCustScanner.yyclear;
begin
  bufptr := 0;
  yysstate := 0;
  yylstate := 1;
  yylastchar := #0;
  FTokenString := '';
  yystext := '';
  FLineNumber := 0;
  yyOffset := -1;
end(*yyclear*);

end.
