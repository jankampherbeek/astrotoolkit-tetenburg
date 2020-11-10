unit Reincarnation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Domain;

type
  TPrevReincarnation = class
    public
      function Calculate(PMoon, PAsc: Double; PDateTime: TDateTimeDto): TDateTimeDto;
    private
      function CalcTimeSpanInDays(PFirstDate, PLastDate: TDateTimeDto): Double;

  end;

implementation

uses
  BeSharedSeFrontend, Utils;

var
   seFrontend: TSeFrontend;

{ TPrevReincarnation }

function TPrevReincarnation.Calculate(PMoon, PAsc: Double; PDateTime: TDateTimeDto): TDateTimeDto;
var
  moonAscOverflow : Boolean;
  moonAscSum: Double;
  baseFractDate, jdTotal: Double;
  baseYearStart, tempYear: Integer;
  yearFractionIn360Days: Double;
  daysInBirthYear: Double;
  baseYearStartDateTime: TDateTimeDto;
  timeTextConstructor: TTimeTextConstructor;
begin
  seFrontend := TSeFrontend.Create;
  timeTextConstructor:= TTimeTextConstructor.Create;
  moonAscOverflow:= false;                                            // calculate sum of Moon and Ascendant: moonAscSum
  moonAscSum:= PMoon + PAsc;
  if (moonAscSum >= 360.0) then begin
    moonAscSum:= moonAscSum - 360.0;
    moonAscOverflow:= true;
  end;
  baseFractDate := PDateTime.Year - moonAscSum;                       // subtract moonAscSum from birth year
  baseYearStart:= trunc(baseFractDate);                               // integer part is baseYearStart
  yearFractionIn360Days := (baseFractDate - baseYearStart) * 360.0;   // fractional part of base year, based on a year of 360 days
  daysInBirthYear:= CalcTimeSpanInDays(TDateTimeDto.Create(PDateTime.Year, 1, 1, 0.0), PDateTime);
  baseYearStartDateTime:= TDateTimeDto.Create(baseYearStart, 1, 1, 0.0);
  jdTotal:= seFrontEnd.JulDay(baseYearStartDateTime) + yearFractionIn360Days + daysInBirthYear;
  Result:= seFrontend.RevJulDay(jdTotal);
end;

function TPrevReincarnation.CalcTimeSpanInDays(PFirstDate, PLastDate: TDateTimeDto): Double;
begin

     result := seFrontend.JulDay(PLastDate) - seFrontend.JulDay(PFirstDate);
end;

end.

