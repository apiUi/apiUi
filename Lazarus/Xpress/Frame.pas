unit Frame;

interface
uses
  CustParser, ParserClasses
;
type TOnHaveDataEvent = procedure ( Sender: TObject
                                  ; aString: String
                                  ) of Object;
type TFrame = class (TObject)
protected
  FOnHaveData: TOnHaveDataEvent;
public
  LayoutList: YYStype;
  property OnHaveData: TOnHaveDataEvent read FOnHaveData write FOnHaveData;
  procedure Expand;
private
  LayoutItem: YYSType;
end;

{$INCLUDE Parser.def}

implementation
uses
  SysUtils, Bind
;
procedure TFrame.Expand;
var
  locString: String;
  Bind: TBind;
begin
  locString := '';
  LayoutItem := LayOutList;
  while (LayoutItem <> Nil) do
  begin
    case LayoutItem.Token of
      _LAYOUT_TOKEN: locString := locString + LayoutItem.TokenString;
      _LAYOUT_FIELD:
      begin
        Bind := LayoutItem.yy.yyObject as TBind;
        locString := locString + Bind.AsString;
      end;
      _NEWLINE:
      begin
        FOnHaveData (Self, locString);
        locString := '';
      end;
    end;
    LayoutItem := LayoutItem.NextToken;
  end;
  if (locString <> '') then
  begin
    FOnHaveData (Self, locString);
  end;
end;

end.
