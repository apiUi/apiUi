unit htmlXmlUtilz ;

{$mode objfpc}{$H+}

{
  mainly utilities providing xml structures that can be saved as html
}


interface

uses Classes
   , SysUtils
   , Xmlz
   , xmlio
   ;

function htmlIndent (x: Integer): String;
function htmlNbsp (aText: String): String;
function htmlVTop (aXml: TXml): TXml;
function htmlHLeft (aXml: TXml): TXml;
function htmlCreateXml (aName, aCaption: String): TXml;
function htmlXmlAsString (aXml: TXml; aStylesheet: String): String;
function htmlHorBarChartAsXml(aGreen, aOrange, aRed: Integer): TXml;
function htmlFindContentXml (aXml: TXml): TXml;

type
  THtmlTableXml = class;
  THtmlThXml = class;
  THtmlTdXml = class;
  THtmlTrXml = class;

  { THtmlXml }

  THtmlXml = class(TXml)
  protected
    function hleft: THtmlXml;
    function vtop: THtmlXml;
    function bgcolor (aColorAsString: String): THtmlXml;
  public
    function AddTable: THtmlTableXml;
    function AddB (aString: String): THtmlXml;
    function AddP: THtmlXml;
    function AddDiv: THtmlXml;
  end;

  { THtmlTableXml }

  THtmlTableXml = class (THtmlXml)
  public
    function Border (n: Integer): THtmlTableXml;
    function WidthPerc (n: Integer): THtmlTableXml;
    function AddTr: THtmlTrXml;
  end;

  { THtmlTrXml }

  THtmlTrXml = class (THtmlXml)
  public
    function hleft: THtmlTrXml;
    function vtop: THtmlTrXml;
    function bgcolor (aColorAsString: String): THtmlXml;
    function AddTd: THtmlTdXml;
    function AddTh: THtmlThXml;
  end;

  { THtmlTdXml }

  THtmlTdXml = class (THtmlXml)
  public
    function bgcolor (aColorAsString: String): THtmlXml;
    function WidthPerc (n: Integer): THtmlTdXml;
    function ColSpan (n: Integer): THtmlTdXml;
  end;

  THtmlThXml = class (THtmlTdXml)
  public
  end;


implementation

function htmlNbsp (aText: String): String;
begin
  result := {'_' + }aText + '_';
end;

function htmlHorBarChartAsXml(aGreen, aOrange, aRed: Integer): TXml;
var
  xGreen, xOrange, xRed: Integer;
begin
  result := TXml.CreateAsString('td', '');
  if (aGreen + aOrange + aRed) = 0 then
    Exit;
  xGreen := Round( 100
                 * (aGreen / (aGreen + aOrange + aRed))
                 );
  xOrange := Round( 100
                  * (aOrange / (aGreen + aOrange + aRed))
                  );
  xRed := 100 - xGreen - xOrange;
  with result do
  begin
    Name := 'table';
    AddAttribute(TXmlAttribute.CreateAsString('border', '0'));
    AddAttribute(TXmlAttribute.CreateAsString('width', '100%'));
    with AddXml (TXml.CreateAsString('tr', '')) do
    begin
      if xGreen > 0 then
        with AddXml (TXml.CreateAsString('td', '_')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('width', IntToStr(xGreen) + '%'));
          AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'green'));
        end;
      if xOrange > 0 then
        with AddXml (TXml.CreateAsString('td', '_')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('width', IntToStr(xOrange) + '%'));
          AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'orange'));
        end;
      if xRed > 0 then
        with AddXml (TXml.CreateAsString('td', '_')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('width', IntToStr(xRed) + '%'));
          AddAttribute(TXmlAttribute.CreateAsString('bgcolor', 'red'));
        end;
    end;
  end;
end;

function htmlIndent (x: Integer): String;
begin
  result := '';
  while x > 0 do
  begin
    result := result + '_';
    Dec (x);
  end;
end;

function htmlFindContentXml (aXml : TXml ): TXml ;
begin
  result := aXml.FindXml('html.body.div');
end;

function htmlVTop (aXml: TXml ): TXml ;
begin
  result := aXml;
  result.AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
end;

function htmlHLeft (aXml : TXml ): TXml ;
begin
  result := aXml;
  result.AddAttribute(TXmlAttribute.CreateAsString('align', 'left'));
end;

function htmlCreateXml (aName , aCaption : String ): TXml ;
begin
  result := TXml.CreateAsString('html', '');
  with result do
  begin
    with AddXml (TXml.CreateAsString('head', '')) do
    begin
      with AddXml (TXml.CreateAsString('style', '--stylesheet--')) do
        AddAttribute(TXmlAttribute.CreateAsString('type', 'text/css'));
      AddXml (TXml.CreateAsString('title', aName + ' - ' + aCaption));
    end;
    with AddXml (TXml.CreateAsString('body', '')) do
    begin
      with AddXml (TXml.CreateAsString('div', '')) do
      begin
        AddAttribute(TXmlAttribute.CreateAsString('id', 'content'));
        with AddXml (TXml.CreateAsString('p', '')) do
        begin
          AddAttribute(TXmlAttribute.CreateAsString('style', 'margin-left: -10px'));
          with AddXml (TXml.CreateAsString('table', '')) do
          begin
            AddAttribute(TXmlAttribute.CreateAsString('width', '100%'));
            AddAttribute(TXmlAttribute.CreateAsString('border', '0'));
            with AddXml (TXml.CreateAsString('tr', '')) do
            begin
              with AddXml (TXml.CreateAsString('td', '')) do
              begin
                with AddXml (TXml.CreateAsString('p', htmlNbsp (aName + ' - ' + aCaption))) do
                begin
                  AddAttribute(TXmlAttribute.CreateAsString('class', 'heading1'));
                end;
              end;
              with AddXml (TXml.CreateAsString('td', '')) do
              begin
                AddAttribute(TXmlAttribute.CreateAsString('align', 'right'));
                with AddXml (TXml.CreateAsString('p', htmlNbsp (DateTimeToStr(Now)))) do
                begin
                  AddAttribute(TXmlAttribute.CreateAsString('class', 'heading1'));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function htmlXmlAsString (aXml : TXml ; aStylesheet : String ): String ;
begin
  result := StringReplace
            ( aXml.asHtmlString
            , '--stylesheet--'
            , xmlio.ReadStringFromFile(aStylesheet)
            , [rfReplaceAll]
            );
end;

{ THtmlTrXml }

function THtmlTrXml .hleft : THtmlTrXml ;
begin
  result := inherited as THtmlTrXml;
end;

function THtmlTrXml .vtop : THtmlTrXml ;
begin
  result := inherited as THtmlTrXml;
end;

function THtmlTrXml .bgcolor (aColorAsString : String ): THtmlXml ;
begin
  result := inherited as THtmlTrXml;
end;

function THtmlTrXml .AddTd : THtmlTdXml ;
begin
  result := AddXml (THtmlTdXml.CreateAsString('td', '') as TXml) as THtmlTdXml;
end;

function THtmlTrXml .AddTh : THtmlThXml ;
begin
  result := AddXml (THtmlThXml.CreateAsString('th', '') as TXml) as THtmlThXml;
end;

{ THtmlTdXml }

function THtmlTdXml .bgcolor (aColorAsString : String ): THtmlXml ;
begin
  result := inherited as THtmlTdXml;
end;

function THtmlTdXml .WidthPerc (n : Integer ): THtmlTdXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('width', IntToStr (n) + '%'));
end;

function THtmlTdXml .ColSpan (n : Integer ): THtmlTdXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('colspan', IntToStr (n)));
end;

{ THtmlTableXml }

function THtmlTableXml .Border (n : Integer ): THtmlTableXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('border', IntToStr (n)));
end;

function THtmlTableXml .WidthPerc (n : Integer ): THtmlTableXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('width', IntToStr (n) + '%'));
end;

function THtmlTableXml .AddTr : THtmlTrXml ;
begin
  result := AddXml (THtmlTrXml.CreateAsString('tr', '') as TXml) as THtmlTrXml;
end;

{ THtmlXml }

function THtmlXml .hleft : THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('align', 'left'));
end;

function THtmlXml .vtop : THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
end;

function THtmlXml .bgcolor (aColorAsString : String ): THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('bgcolor', aColorAsString));
end;

function THtmlXml .AddTable : THtmlTableXml ;
begin
  result := AddXml (THtmlTableXml.CreateAsString('table', '') as TXml) as THtmlTableXml;
end;

function THtmlXml .AddB (aString: String): THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('b', aString) as TXml) as THtmlXml;
end;

function THtmlXml .AddP : THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('p', '') as TXml) as THtmlXml;
end;

function THtmlXml .AddDiv : THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('div', '') as TXml) as THtmlXml;
end;


end.

