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

  TFutureReincarnation = class
    public
      function Calculate(PSun, PMc: Double; PDateTimeBirth, PDateTimeDeath: TDateTimeDto): TDateTimeDto;
    private
      function CalcAgeAtDeath(PDateTimeBirth, PDateTimeDeath: TDateTimeDto): Integer;
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
  if (moonAscOverFlow) then Inc(baseYearStart);
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


{ TFutureReincarnation }

function TFutureReincarnation.Calculate(PSun, PMc: Double; PDateTimeBirth, PDateTimeDeath: TDateTimeDto): TDateTimeDto;
var
  sunMcOverflow: Boolean;
  sunMcSum, age, futureFractYear: Double;
  yearFractionIn360Days : Double;
  birthFractYear: Double;
  jdTotal: Double;
  futureIntYear: Integer;
  tempDate : TDateTimeDto;
  timeTextConstructor: TTimeTextConstructor;
  daysInBirthYear, daysInDeathYear : Double;
  //birthYearFraction, deathYearFraction: Double;
begin
   seFrontend := TSeFrontend.Create;
   timeTextConstructor:= TTimeTextConstructor.Create;
   sunMcOverflow:= false;
   sunMcSum:= PSun + PMc;
   if (sunMcSum >= 360.0) then begin
     sunMcSum:= sunMcSum - 360.0;
     sunMcOverflow:= true;
   end;
   daysInBirthYear:= CalcTimeSpanInDays(TDateTimeDto.Create(PDateTimeBirth.Year, 1, 1, 0.0), PDateTimeBirth);
   daysInDeathYear:= CalcTimeSpanInDays(TDateTimeDto.Create(PDateTimeDeath.Year, 1, 1, 0.0), PDateTimeDeath);
   //birthYearFraction:= daysInBirthYear / 360.0;
   //deathYearFraction:= daysInDeathYear / 360.0;
   age := CalcAgeAtDeath(PDateTimeBirth, PDateTimeDeath);
   futureFractYear:= PDateTimeBirth.Year + age + sunMcSum;
   futureIntYear:= Round(Int(futureFractYear));
   //yearFractionIn360Days:= ((futureFractYear - futureIntYear) * 360.0) + birthYearFraction + deathYearFraction;
   yearFractionIn360Days:= ((futureFractYear - futureIntYear) * 360.0);
   if (sunMcOverFlow) then Inc(futureIntYear);
   tempDate:= TDateTimeDto.Create(futureIntYear,1,1,0.0);
   //jdTotal:= seFrontend.JulDay(tempDate) + yearFractionIn360Days;
      jdTotal:= seFrontend.JulDay(tempDate) + yearFractionIn360Days + daysInBirthYear + daysInDeathYear;
   result:= seFrontend.RevJulDay(jdTotal);
end;

function TFutureReincarnation.CalcAgeAtDeath(PDateTimeBirth, PDateTimeDeath: TDateTimeDto): Integer;
var
   age: Integer;

begin
   age := PDateTimeDeath.Year - PDateTimeBirth.Year;
   if (PDateTimeDeath.Month <= PDateTimeBirth.Month) then dec(age)
   else if (PDateTimeDeath.Month = PDateTimeBirth.Month) then begin
     if (PDateTimeDeath.Day <= PDateTimeBirth.Day) then dec(age)
     else if (PDateTimeDeath.Time < PDateTimeBirth.Time) then dec(age)
   end;
   result:= age;
end;

function TFutureReincarnation.CalcTimeSpanInDays(PFirstDate, PLastDate: TDateTimeDto): Double;
begin
     result := seFrontend.JulDay(PLastDate) - seFrontend.JulDay(PFirstDate);
end;

end.

