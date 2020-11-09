unit CriticalPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BeSharedSeFrontend, Domain;

type

TCriticalPointCalculator = class
  public
    function Calculate(radixDate, eventDate: TDateTimeDto;
                       radixMc, solarSpeed, geoLat: Double): TSignDmsValue;

  end;



implementation

const
  TROPICAL_YEAR = 365.24219;

  { TCriticalPointCalculator }

  function TCriticalPointCalculator.Calculate(radixDate, eventDate: TDateTimeDto;
                                              radixMc, solarSpeed, geoLat: Double): TSignDmsValue;
  var
    jdRadix, jdEvent, jdDiff, nrOfYears, eps, progMc, progRaMc, progAsc: Double;
    SeFrontend: TSeFrontend;
    SignDmsValue: TSignDmsValue;
  begin
     SeFrontend:= TSeFrontend.Create;
     jdRadix := SeFrontend.JulDay(radixDate);
     jdEvent := SeFrontend.JulDay(eventDate);
     jdDiff := jdEvent - jdRadix;
     eps := SeFrontend.CalculateObliquity(jdRadix);
     nrOfYears := jdDiff / TROPICAL_YEAR;
     progMc := radixMc + nrOfYears * solarSpeed;
     progRaMc := SeFrontend.Ecliptic2Equatorial(progMc, 0.0, eps)[0];
     progAsc := SeFrontend.AscFromMc(progRaMc, geoLat, eps);
     SignDmsValue := TSignDMSValue.Create;
     SignDmsValue.SetNewLongitude(progAsc);
     Result := SignDMSValue;

  end;

end.

