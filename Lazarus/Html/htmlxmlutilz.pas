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

function htmlNbsp (aText: String): String;
function htmlCreateXml (aName, aCaption: String): TXml;
function htmlXmlAsString (aXml: TXml; aStylesheet: String): String;
function htmlHorBarChartAsXml(aWidth, aGreen, aOrange, aRed: Integer): TXml;
function htmlFindContentXml (aXml: TXml): TXml;

implementation

function htmlNbsp (aText: String): String;
begin
  result := '_' + aText + '_';
end;

function htmlHorBarChartAsXml(aWidth, aGreen, aOrange, aRed: Integer): TXml;
// aOrange not yet implemented !!!!
var
  xGreen, xOrange, xRed: Integer;
begin
  xGreen := Round( aWidth
                 * (aGreen / (aGreen + aRed))
                 );
  xRed := aWidth - xGreen;
  if xGreen = 0 then
    Inc (xRed, 4);
  if xRed = 0 then
    Inc (xGreen, 4);
  result := TXml.CreateAsString('td', '');
  with result do
    with AddXml (TXml.CreateAsString('table', '')) do
      with AddXml (TXml.CreateAsString('tr', '')) do
      begin
        if xGreen > 0 then
          with AddXml (TXml.CreateAsString('td', '_')) do
            AddAttribute(TXmlAttribute.CreateAsString('style', Format('background-color:green;width:%d', [xgreen])));
        if xRed > 0 then
          with AddXml (TXml.CreateAsString('td', '_')) do
            AddAttribute(TXmlAttribute.CreateAsString('style', Format('background-color:red;width:%d', [xred])));
      end;
end;

function htmlFindContentXml (aXml : TXml ): TXml ;
begin
  result := aXml.FindXml('html.body.div');
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

