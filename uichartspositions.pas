{ This software is open source.
  Please check the file copyright.txt in the root of the source for further details. }
unit UiChartsPositions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Domain, BeSharedSeFrontend, BeSwissDelphi;
type

  TObjectPositionsArray = Array of Double;
  TObjectGlyphsArray = Array of String;
  TObjectSeIdArray = Array of Integer;
  TMundPosArray = Array of Double;
  TSignGlyphsArray = Array of String;


{ TFormChartsPositions }

{ Shows positions and selections for further use. }
TFormChartsPositions = class(TForm)
  BtnClose: TButton;
  BtnHelp: TButton;
  LblTitle: TLabel;
  SGHouses: TStringGrid;
  SGPositions: TStringGrid;
  procedure BtnCloseClick;
  procedure FormCreate;
  procedure FormShow;
private
  FFullChartResponse: TFullChartResponse;
  FDateTimeDto: TDateTimeDto;
  FLocationDto: TLocationDto;
  HousesResponse: THousesResponse;
  DecimalValue: TDecimalValue;
  procedure ShowPositions;
  procedure HandlePositions;
  procedure InitValues;

public
  property FullChartResponse: TFullChartResponse write FFullChartResponse;
  property DateTimeDto: TDateTimeDto write FDateTimeDto;
  property LocationDto: TLocationDto write FLocationDto;
end;

var
  FormChartsPositions: TFormChartsPositions;
  CelestialObjectNames: Array of String;
  GridTextStyle: TTextStyle;
  SignDmsValue: TSignDMSValue;
  DecimalValue: TDecimalValue;
  ObjectPositionsArray: TObjectPositionsArray;
  ObjectGlyphsArray: TObjectGlyphsArray;
  ObjectSeIdArray: TObjectSeIdArray;
  MundPosArray: TMundPosArray;
  SignGlyphsArray: TSignGlyphsArray;
  TextRow: Array[0..13] of String;
  TextRowHouses: Array[0..6] of String;

implementation

{$R *.lfm}

{ TFormChartsPositions }
uses
  UiGraph;

procedure TFormChartsPositions.BtnCloseClick;
begin
  Close;
end;

procedure TFormChartsPositions.FormCreate;
begin

  GridTextStyle:= SGPositions.DefaultTextStyle;
  GridTextStyle.Alignment:= taRightJustify;
  SGPositions.DefaultTextStyle:= GridTextStyle;
end;


procedure TFormChartsPositions.FormShow;
begin
  InitValues;
  HandlePositions;
  ShowPositions;
end;

procedure TFormChartsPositions.HandlePositions;
var
  SeFrontend: TSeFrontend;
  JulianDay, Obliquity: Double;
  i, Flags: Integer;
begin
  SeFrontend:= TSeFrontend.Create;
  JulianDay:= SeFrontend.JulDay(FDateTimeDto);
  Obliquity:= SeFrontend.CalculateObliquity(JulianDay);
  Flags:= SEFLG_SWIEPH or SEFLG_SPEED;

  for i:= 0 to 11 do begin
    ObjectPositionsArray[i]:= SeFrontend.CalculateCelestialObject(Julianday, ObjectSeIdArray[i], Flags).MainPos;
  end;

  HousesResponse:= SeFrontend.CalculateHouses(JulianDay, Obliquity, FLocationDto);
  SetLength(MundPosArray, 2);
  MundPosArray[0]:= HousesResponse.Mc;
  MundPosArray[1]:= HousesResponse.Asc;


end;

procedure TFormChartsPositions.ShowPositions;
var
  i: Integer;
begin
  SignDmsValue:= TSignDmsValue.Create;
  for i:= 0 to 11 do begin
    SignDmsValue.SetNewLongitude(ObjectPositionsArray[i]);
    DecimalValue:= TDecimalValue.Create(ObjectPositionsArray[i]);
    SGPositions.Rows[i].add(ObjectGlyphsArray[i]);
    SGPositions.Rows[i].add(SignDmsValue.Text);
    SGPositions.Rows[i].add(SignGlyphsArray[SignDmsValue.SignId - 1]);
    SGPositions.Rows[i].add(DecimalValue.Text);
  end;
  SignDmsValue.SetNewLongitude(MundPosArray[0]);
  DecimalValue:= TDecimalValue.Create(MundPosArray[0]);
  SGHouses.Rows[0].Add('MC');
  SGHouses.Rows[0].Add(SignDmsValue.Text);
  SGHouses.Rows[0].Add(SignGlyphsArray[SignDmsValue.SignId]);
  SGHouses.Rows[0].Add(DecimalValue.Text);
  SignDmsValue.SetNewLongitude(MundPosArray[1]);
  DecimalValue:= TDecimalValue.Create(MundPosArray[1]);
  SGHouses.Rows[1].Add('Asc');
  SGHouses.Rows[1].Add(SignDmsValue.Text);
  SGHouses.Rows[1].Add(SignGlyphsArray[SignDmsValue.SignId]);
  SGHouses.Rows[1].Add(DecimalValue.Text);
end;


procedure TFormChartsPositions.InitValues;
begin
  SetLength(ObjectPositionsArray, 12);
  SetLength(ObjectGlyphsArray, 12);
  ObjectGlyphsArray[0]:= 'a';       // Sun
  ObjectGlyphsArray[1]:= 'b';       // Moon
  ObjectGlyphsArray[2]:= 'c';       // Mercury
  ObjectGlyphsArray[3]:= 'd';       // Venus
  ObjectGlyphsArray[4]:= 'f';       // Mars
  ObjectGlyphsArray[5]:= 'g';       // Jupiter
  ObjectGlyphsArray[6]:= 'h';       // Saturn
  ObjectGlyphsArray[7]:= 'i';       // Uranus
  ObjectGlyphsArray[8]:= 'j';       // Neptune
  ObjectGlyphsArray[9]:= 'k';       // Pluto
  ObjectGlyphsArray[10]:= 'w';      // Chiron
  ObjectGlyphsArray[11]:= '{';      // Lunar Node
  SetLength(ObjectSeIdArray, 12);
  ObjectSeIdArray[0]:= 0;        // Sun
  ObjectSeIdArray[1]:= 1;        // Moon
  ObjectSeIdArray[2]:= 2;        // Mercury
  ObjectSeIdArray[3]:= 3;        // Venus
  ObjectSeIdArray[4]:= 4;        // Mars
  ObjectSeIdArray[5]:= 5;        // Jupiter
  ObjectSeIdArray[6]:= 6;        // Saturn
  ObjectSeIdArray[7]:= 7;        // Uranus
  ObjectSeIdArray[8]:= 8;        // Neptune
  ObjectSeIdArray[9]:= 9;        // Pluto
  ObjectSeIdArray[10]:= 15;        // Chiron
  ObjectSeIdArray[11]:= 11;      // True node
  SetLength(SignGlyphsArray, 12);
  SignGlyphsArray[0]:= '1';       // Aries
  SignGlyphsArray[1]:= '2';       // Taurus
  SignGlyphsArray[2]:= '3';       // Gemini
  SignGlyphsArray[3]:= '4';       // Cancer
  SignGlyphsArray[4]:= '5';       // Leo
  SignGlyphsArray[5]:= '6';       // Virgo
  SignGlyphsArray[6]:= '7';       // Libra
  SignGlyphsArray[7]:= '8';       // Scorpio
  SignGlyphsArray[8]:= '9';       // Sagittarius
  SignGlyphsArray[9]:= '0';       // Capricornus
  SignGlyphsArray[10]:= '-';      // Aquarius
  SignGlyphsArray[11]:= '=';      // Pisces
end;

end.

