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


end.

