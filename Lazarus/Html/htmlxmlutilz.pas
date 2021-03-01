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

type
  THtmlTableXml = class;
  THtmlThXml = class;
  THtmlTdXml = class;
  THtmlTrXml = class;
  THtmlDivXml = class;
  THtmlAXml = class;

  { THtmlXml }

  THtmlXml = class(TXml)
  protected
    function hleft: THtmlXml;
    function hright: THtmlXml;
    function vtop: THtmlXml;
    function id (aId: String): THtmlXml;
    function bgcolor (aColorAsString: String): THtmlXml;
    function style (aStyleString: String): THtmlXml;
    function name_ (aName: String): THtmlXml;
    function href (aName: String): THtmlXml;
    function clasz (aClassString: String): THtmlXml;
  public
    function AddTable: THtmlTableXml;
    function AddHtml (aName: String): THtmlXml;
    function AddHtml (aName: String; aValue: String): THtmlXml;
    function AddA (aString: String): THtmlAXml;
    function AddB (aString: String): THtmlXml;
    function AddP: THtmlXml;
    function AddP(aString: String): THtmlXml;
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
    function hright: THtmlTrXml;
    function vtop: THtmlTrXml;
    function bgcolor (aColorAsString: String): THtmlXml;
    function AddTd: THtmlTdXml;
    function AddTh: THtmlThXml;
  end;

  { THtmlTdXml }

  THtmlTdXml = class (THtmlXml)
  public
    function bgcolor (aColorAsString: String): THtmlTdXml;
    function WidthPerc (n: Integer): THtmlTdXml;
    function ColSpan (n: Integer): THtmlTdXml;
    function RowSpan (n: Integer): THtmlTdXml;
  end;

  THtmlThXml = class (THtmlTdXml)
  public
  end;

  { THtmlTrXml }

  { THtmlDivXml }

  THtmlDivXml = class (THtmlXml)
  public
    function id (aId: String): THtmlDivXml;
  end;

  { THtmlAXml }

  THtmlAXml = class (THtmlXml)
  public
    function name_ (aName: String): THtmlAXml;
    function href (aName: String): THtmlAXml;
  end;


function htmlIndent (x: Integer): String;
function htmlNbsp (aText: String): String;
function htmlVTop (aXml: TXml): TXml;
function htmlHLeft (aXml: TXml): TXml;
function htmlCreateXml (aName, aCaption: String): THtmlXml;
function htmlXmlAsString (aXml: TXml; aStylesheet: String): String;
function htmlHorBarChartAsXml(aGreen, aOrange, aRed: Integer): THtmlTableXml;
function htmlFindContentXml (aXml: TXml): THtmlXml;


implementation

function htmlNbsp (aText: String): String;
begin
  result := {'_' + }aText + '_';
end;

function htmlHorBarChartAsXml(aGreen, aOrange, aRed: Integer): THtmlTableXml;
var
  xGreen, xOrange, xRed: Integer;
begin
  result := THtmlTableXml.CreateAsString('table', '').Border(0).WidthPerc(100);
  if (aGreen + aOrange + aRed) > 0 then
  begin
    xGreen := Round( 100
                   * (aGreen / (aGreen + aOrange + aRed))
                   );
    xOrange := Round( 100
                    * (aOrange / (aGreen + aOrange + aRed))
                    );
    xRed := 100 - xGreen - xOrange;
    with result.AddTr do
    begin
      if xGreen > 0 then
        AddTd.bgcolor('green').WidthPerc(xGreen).Value := '_';
      if xOrange > 0 then
        AddTd.bgcolor('orange').WidthPerc(xOrange).Value := '_';
      if xRed > 0 then
        AddTd.bgcolor('red').WidthPerc(xRed).Value := '_';
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

function htmlFindContentXml (aXml : TXml ): THtmlXml ;
begin
  result := aXml.FindXml('html.body.div') as THtmlXml;
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

function htmlCreateXml (aName , aCaption : String ): THtmlXml ;
begin
  result := THtmlXml.CreateAsString('html', '');
  with result do
  begin
    with AddHtml('head') do
    begin
      AddHtml('style', '--stylesheet--').AddAttribute(TXmlAttribute.CreateAsString('type','text/css'));
      AddHtml('title', aName + ' - ' + aCaption);
    end;
    with AddHtml ('body')
          .AddHtml('div').id('content') do
    begin
      with AddP.style('margin-left: -10px').AddTable.WidthPerc(100).Border(0).AddTr do
      begin
        AddTd.hleft.AddP(htmlNbsp (aName + ' - ' + aCaption)).clasz('heading1');
        AddTd.hright.AddP(htmlNbsp (DateTimeToStr(Now))).clasz('heading1');
      end;
    end;
  end;
end;

function htmlXmlAsString (aXml : TXml ; aStylesheet : String ): String ;
begin
  result := StringReplace
            ( aXml.asHtmlString
            , '--stylesheet--'
            , xmlio.ReadStringFromFile(aStylesheet, nil)
            , [rfReplaceAll]
            );
end;

{ THtmlAXml }

function THtmlAXml .name_ (aName : String ): THtmlAXml ;
begin
  result := inherited as THtmlAXml;
end;

function THtmlAXml .href (aName : String ): THtmlAXml ;
begin
  result := inherited as THtmlAXml;
end;

{ THtmlDivXml }

function THtmlDivXml .id (aId : String ): THtmlDivXml ;
begin
  result := inherited as THtmlDivXml;
end;

{ THtmlTrXml }

function THtmlTrXml .hleft : THtmlTrXml ;
begin
  result := inherited as THtmlTrXml;
end;

function THtmlTrXml .hright : THtmlTrXml ;
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

function THtmlTdXml .bgcolor (aColorAsString : String ): THtmlTdXml ;
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

function THtmlTdXml .RowSpan (n : Integer ): THtmlTdXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('rowspan', IntToStr (n)));
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

function THtmlXml .hright : THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('align', 'right'));
end;

function THtmlXml .vtop : THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('valign', 'top'));
end;

function THtmlXml .id (aId : String ): THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('id', aId));
end;

function THtmlXml .bgcolor (aColorAsString : String ): THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('bgcolor', aColorAsString));
end;

function THtmlXml.style(aStyleString: String): THtmlXml;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('style', aStyleString));
end;

function THtmlXml .name_ (aName : String ): THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('name', aName));
end;

function THtmlXml .href (aName : String ): THtmlXml ;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('href', aName));
end;

function THtmlXml.clasz(aClassString: String): THtmlXml;
begin
  result := self;
  AddAttribute(TXmlAttribute.CreateAsString('class', aClassString));
end;

function THtmlXml .AddTable : THtmlTableXml ;
begin
  result := AddXml (THtmlTableXml.CreateAsString('table', '') as TXml) as THtmlTableXml;
end;

function THtmlXml.AddHtml(aName: String): THtmlXml;
begin
  result := AddXml (THtmlXml.CreateAsString(aName, '') as TXml) as THtmlXml;
end;

function THtmlXml.AddHtml(aName: String; aValue: String): THtmlXml;
begin
  result := AddXml (THtmlXml.CreateAsString(aName, aValue) as TXml) as THtmlXml;
end;

function THtmlXml .AddA (aString : String ): THtmlAXml ;
begin
  result := AddXml (THtmlAXml.CreateAsString('a', aString) as TXml) as THtmlAXml;
end;

function THtmlXml .AddB (aString: String): THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('b', aString) as TXml) as THtmlXml;
end;

function THtmlXml .AddP : THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('p', '') as TXml) as THtmlXml;
end;

function THtmlXml.AddP(aString: String): THtmlXml;
begin
  result := AddXml (THtmlXml.CreateAsString('p', aString) as TXml) as THtmlXml;
end;

function THtmlXml .AddDiv : THtmlXml ;
begin
  result := AddXml (THtmlXml.CreateAsString('div', '') as TXml) as THtmlXml;
end;


end.

