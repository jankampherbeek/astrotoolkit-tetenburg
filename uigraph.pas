{ This software is open source.
  Please check the file copyright.txt in the root of this application for further details. }
unit UiGraph;
{< Standard fonts and colors.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
{ Definition of standard fonts and colors, implemented as singleton. }
TStyling = class
private
  FDataBgColor, FInfoBgColor, FDataTableHeaderColor, FDataErrorColor, FDataAlternateRowColor: TColor;
  FDataFontHeading, FDataFontSubHeading, FDataFontText,
  FInfoFontHeading, FInfoFontSubheading, FinfoFontText, FInfoFontEmphasized: TFont;
  constructor Init;      // ignore the warning, a constructor in a singleton cannot be public
  function CreateFont(PName, PColor: String; PSize: Integer): TFont;
public
{ Static class to prevent using 'Create' as a constructor.
  The constructor is the private method 'Init' and this is how the singleton is implemented. }
  class function Create: TStyling;
  procedure PrepareDefinitions;
  property DataBgColor: TColor read FDataBgColor;
  property DataTableHeaderColor: TColor read FDataTableHeaderColor;
  property DataErrorColor: TColor read FDataErrorColor;
  property DataAlternateRowColor: TColor read FDataAlternateRowColor;
  property InfoBgColor: TColor read FInfoBgColor;
  property DataFontHeading: TFont read FDataFontHeading;
  property DataFontSubHeading: TFont read FDataFontSubHeading;
  property DataFontText: TFont read FDataFontText;
  property InfoFontHeading: TFont read FInfoFontHeading;
  property InfoFontSubHeading: TFont read FInfoFontSubHeading;
  property InfoFontText: TFont read FInfoFontText;
  property InfoFontEmphasized: TFont read FInfoFontEmphasized;
end;

implementation

const
  DATA_BG_COLOR = '$E1E1C6';
  DATA_FONT_COLOR = '$262600';
  DATA_TABLE_HEADER_COLOR = '$B3B399';
  DATA_ALTERNATE_ROW_COLOR = '$FEFFF2';
  INFO_BG_COLOR = '$E5EDF0';
  INFO_FONT_COLOR = '$3A7B79';
var
  StylingSingleton: TStyling = nil;

{ TStyling }
constructor TStyling.init;
begin
  inherited Create;
  PrepareDefinitions;
end;

class function TStyling.Create: TStyling;
begin
  if StylingSingleton = nil then begin
    StylingSingleton:= TStyling.Init;
  end;
  Result:= StylingSingleton;
end;

procedure TStyling.PrepareDefinitions;
begin
  FDataBgColor:= StringToColor(DATA_BG_COLOR);
  FDataTableHeaderColor:= StringToColor(DATA_TABLE_HEADER_COLOR);
  FDataAlternateRowColor:= StringToColor(DATA_ALTERNATE_ROW_COLOR);
  FInfoBgColor:= StringToColor(INFO_BG_COLOR);
  FDataErrorColor:= clYellow;
  FDataFontText:= CreateFont('Segoe UI', DATA_FONT_COLOR, 9);
  FDataFontHeading:= CreateFont('Liberation Sans', DATA_FONT_COLOR, 36);
  FDataFontSubHeading:= CreateFont('Liberation Sans', DATA_FONT_COLOR, 24);
  FInfoFontText:= CreateFont('Segoe UI', INFO_FONT_COLOR, 9);
  FInfoFontHeading:= CreateFont('Liberation Sans', INFO_FONT_COLOR, 36);
  FInfoFontSubHeading:= CreateFont('Liberation Sans', INFO_FONT_COLOR, 24);
  FInfoFontEmphasized:= CreateFont('Segoe UI', INFO_FONT_COLOR, 12);
end;

function TStyling.CreateFont(PName, PColor: String; PSize: Integer): TFont;
var
  MyFont: TFont;
begin
  MyFont:= TFont.Create;
  MyFont.Name:= PName;
  MyFont.Size:= PSize;
  MyFont.Color:= StringToColor(PColor);
  Result:= MyFont;
end;

end.


