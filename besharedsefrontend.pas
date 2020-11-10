{ This software is open source.
  Please check the file copyright.txt in the root of this application for further details. }
unit BeSharedSeFrontend;
{< Contains a frontend for the Swiss Ephemeris (SE). }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Domain, BeSwissDelphi;

type

TDoubleArray = Array of Double;

{ Frontend to calculations with the Swiss Ephemeris (SE).
It is implemented as a singleton as the SE does not support multiple instances.}
TSeFrontend = class        // singleton based on last example at http://wiki.freepascal.org/Singleton_Pattern
 private
   constructor Init;

 public
   { Static class to prevent using 'Create' as a constructor.
   The constructor is the private method 'Init' and this is how the singleton is implemented. }
   class function Create: TSeFrontend;

   { Calculate Julian Day Number. Always use Universal time for input as DeltaT is automatically taken care off.}
   function JulDay(PDateTime:TDateTimeDto): Double;

   { Checks if a data is valid, takes calendar and leap years into account.}
   function CheckDate(PDay, PMonth, PYear: Integer): Boolean;

   { Returns the date for a Julian Day number. Calendar is always Gregorian. }
   function RevJulDay(PJd: Double): TDateTimeDto;

   { Calculate true value of obliquity (Epsilon) }
   function CalculateObliquity(PJulianDay:Double): Double;

   { Calculate a celestial object. Uses Julian day for UT, the id for the object (SeId) and the combined flags
   for the type of calculation.}
   function CalculateCelestialObject(PJulianDay:Double; PSeId:Integer; PFlags:LongInt): TCelObjectComponentDto;

   { Convert ecliptical coördinates into equatorial coördinates. The resulting array contains the right ascension
   at index 0 and the declination at index 1, index 2 can be ignored. }
   function Ecliptic2Equatorial(PLong, Plat, PEpsilon: Double): TDoubleArray;

   function CalculateHouses(PJulianDay, PObliquity:Double; PLocation: TLocationDto): THousesResponse ;

   { Calculates the ascendant from the ARMC }
   function AscFromMc(Armc, GeoLat, Eps: Double): Double;

   { Returns SeId for a BodyId }
   function DefineSeId(BodyId: Integer): Integer;

end;



implementation

var
  Singleton: TSeFrontend = nil;

{ TSeFrontend }

constructor TSeFrontend.init;
begin
  inherited Create;
end;

class function TSeFrontend.Create: TseFrontend;
begin
  if Singleton = nil then begin
    Singleton:= TSeFrontend.Init;
    swe_set_ephe_path('.\\se');   // required to use the SE Data and for initialization of the SE.
  end;
  Result:= Singleton;
end;

function TSeFrontend.JulDay(PDateTime:TDateTimeDto): Double;
var
  GregorianFlag: Integer;
  DateTime: TDateTimeDto;
begin
  DateTime:=PDateTime;
  GregorianFlag:=1;
  Result := swe_julday(DateTime.Year, DateTime.Month, DateTime.Day, DateTime.Time, GregorianFlag);
end;

function TSeFrontend.RevJulDay(PJd: Double): TDateTimeDto;
var
   year, month, day: Integer;
   time: Double;
begin
   swe_revjul(PJd, 1, year, month, day, time);
   Result:= TDateTimeDto.Create(year, month, day, time);
end;


function TSeFrontend.CheckDate(PDay, PMonth, PYear: Integer): Boolean;
var
  Day, Month, Year, ReturnStatus: Integer;
  JulianDay: Double;
begin
  JulianDay:=0.0;
  Day := PDay;
  Month := PMonth;
  Year := PYear;
  ReturnStatus := swe_date_conversion(Year, Month, Day, 0.0, 'g', JulianDay);
  Result := ReturnStatus = 0;
end;

function TSeFrontend.CalculateObliquity(PJulianDay:Double):Double;
var
  Positions: Array[0..6] of Double;
  CalcResult: Longint;
  ErrorText: Array[0..255] of Char;
begin
  CalcResult := swe_calc_ut(PJulianDay, SE_ECL_NUT, 0, Positions[0], ErrorText);  // todo check Calcresult, log errors
  Result:= Positions[0];     // uses true obliquity
end;

function TSeFrontend.CalculateCelestialObject(PJulianDay: Double; PSeId: Integer;
                                              PFlags: LongInt): TCelObjectComponentDto;
var
  Positions: Array[0..5] of Double;
  CalcResult: Longint;
  CelObject: TCelObjectComponentDto;
  ErrorText: Array[0..255] of Char;
begin
  CalcResult := swe_calc_ut(PJulianDay, PSeId, PFlags, Positions[0], ErrorText);  // todo check Calcresult, log errors
  CelObject:= TCelObjectComponentDto.Create(Positions);
  Result:= CelObject;
end;

function TSeFrontend.Ecliptic2Equatorial(PLong, Plat, PEpsilon: Double): TDoubleArray;
var
  EclipticValues, EquatorialValues: TDoubleArray;
  NegativeEpsilon: Double;  // for conversion ecliptic to equatorial, Epsilon must be negative.
begin
  SetLength(EclipticValues, 3);
  SetLength(EquatorialValues, 3);
  NegativeEpsilon:= -(Abs(PEpsilon));
  EclipticValues[0]:= PLong;
  EclipticValues[1]:= PLat;
  EclipticValues[2]:= 1.0;   // distance can be ignored, the value 1.0 is just a placeholder.
  swe_cotrans(EclipticValues[0], EquatorialValues[0], NegativeEpsilon);
  Result:= EquatorialValues;
end;


function TSeFrontend.CalculateHouses(PJulianDay, PObliquity:Double; PLocation: TLocationDto): THousesResponse ;
var
  HouseValues: Array of Double;
  AscMcValues: Array[0..10] of Double;
  ReturnValue: Integer;
  Mc, Asc: Double;
  SeChar: AnsiChar;
begin
  SetLength(HouseValues, 13);
  SeChar := 'O';    // use Porphyrius but ignore intermediate cusps
  ReturnValue:= swe_houses(PJulianDay, PLocation.Latitude, PLocation.Longitude, SeChar, HouseValues[0], AscMcValues[0]);
  Asc:= AscMcValues[0];
  Mc:= AscMcValues[1];
  Result:= THousesResponse.Create(Mc, Asc);
end;


function TSeFrontend.AscFromMc(Armc, GeoLat, Eps: Double): Double;
var
  HouseValues: Array of Double;
  AscMcValues: Array[0..10] of Double;
  ReturnValue: Integer;
begin
  SetLength(HouseValues, 13);
  ReturnValue:= swe_houses_armc(Armc, GeoLat, Eps, 'W', HouseValues[0], AscMcValues[0]);
  Result:= AscMcValues[0];
end;


function TSeFrontend.DefineSeId(BodyId: Integer): Integer;
  var SeId: Integer;
  begin
    case BodyId of
      0: SeId:= 0;        // Sun
      1: SeId:= 1;        // Moon
      2: SeId:= 2;        // Mercury
      3: SeId:= 3;        // Venus
      4: SeId:= 14;       // Earth
      5: SeId:= 4;        // Mars
      6: SeId:= 5;        // Jupiter
      7: SeId:= 6;        // Saturn
      8: SeId:= 7;        // Uranus
      9: SeId:= 8;        // Neptune
      10: SeId:= 9;       // Pluto
      11: SeId:= 15;      // Cheiron
      13: SeId:= 11;      // True Node
    else
      SeId:= -100;
    end;
    Result:= SeId;
  end;


end.



