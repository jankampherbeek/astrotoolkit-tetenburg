unit Validators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BeSharedSeFrontend, Domain;

type


{ Handles sexagesimal input and converts it to a TValidatedDouble. @br
  Expects a lower and upper limit during construction, respectively MinDHValue and MaxDHValue, which contain the
  limits for Degrees or Hours. The upper and lower limits of minutes and seconds are automatically checked.@br
  If the conversion was valid: Value contains the converted value and Valid is true.@br
  If the conversions was invalid: Value contains 0.0 and Valid is false. }
TSexagesimalValidator = class
strict private
  FMinDH, FMaxDH: Integer;
  FValid: Boolean;
  FValue: Double;
  procedure Validate(DegreesHours, Minutes, Seconds: Integer);
public
  { PMinDH and PMaxDH: Respectively the minimal and maximal value for degrees or hours. Values are inclusive. }
  constructor Create(PMinDH, PMaxDH: Integer);
  function CalcAndCheck(DHText, MinutesText, SecondsText: String; NegativeSign: Boolean): TValidatedDoubleDto;
end;



{ Handles input for geographic longitude and converts it to a TValidatedDouble. @br
  The Direction should be 'E' for East and 'W' for West.@br
  If the conversion was valid: Value contains the converted value and Valid is true.@br
  If the conversions was invalid: Value contains 0.0 and Valid is false. }
TLongitudeValidator = class
strict private
  FSexagesimalValidator: TSexagesimalValidator;
public
  constructor Create;
  destructor Destroy; override;
  function CalcAndCheck(Degrees, Minutes, Seconds, Direction: String): TValidatedDoubleDto;
end;

{ Handles input for geographic latitude and converts it to a TValidatedDouble.@br
  The Direction should be 'N' for North and 'S' for South.@br
  If the conversion was valid: Value contains the converted value and Valid is true.@br
  If the conversions was invalid: Value contains 0.0 and Valid is false.}
TLatitudeValidator = class
strict private
  FSexagesimalValidator: TSexagesimalValidator;
public
  constructor Create;
  destructor Destroy; override;
  function CalcAndCheck(Degrees, Minutes, Seconds, Direction: String): TValidatedDoubleDto;
end;

{ Handles input for a calendar date and converts it to a TValidatedDate.@br
  If the conversion was valid: Value contains the converted value and Valid is true.@br
  If the conversions was invalid: Value contains 0.0 and Valid is false. }
TDateValidator = class
private
  FSeFrontend : TSeFrontend;   // No destructor as TSeFrontend is a singleton.
public
  constructor Create;
  function CalcAndCheck(PYear, PMonth, PDay: String):TValidatedDateDto;
end;

{ Handles input for clock time and converts it to a TValidatedTime.@br
  Time is assumed to be always UT.@br
  If the conversion was valid: Value contains the converted value and Valid is true.@br
  If the conversions was invalid: Value contains 0.0 and Valid is false. }
TTimeValidator = class
public
  function CalcAndCheck(PHour, PMinute, PSecond: String): TValidatedTimeDto;
end;



implementation

uses
  xshareddictionary;

{ TSexagesimalValidator }
constructor TSexagesimalValidator.Create(PMinDH, PMaxDH: Integer);
begin
 FMinDH := PMinDH;
 FMaxDH := PMaxDH;
end;

procedure TSexagesimalValidator.Validate(DegreesHours, Minutes, Seconds: Integer);
begin
 if (DegreesHours < FMinDH)
 or (DegreesHours > FMaxDH)
 or not(Minutes in [MIN_MINUTE_SECOND..MAX_MINUTE_SECOND])
 or not(Seconds in [MIN_MINUTE_SECOND..MAX_MINUTE_SECOND]) then begin
   FValid := false;
   FValue := ZERO;
 end;
end;

function TSexagesimalValidator.CalcAndCheck(DHText, MinutesText, SecondsText: String;
                             NegativeSign: Boolean): TValidatedDoubleDto;
var
 DHValue, MinutesValue, SecondsValue, ErrorCode: Integer;
begin
 FValue := ZERO;
 FValid := true;
 Val(DHText, DHValue, ErrorCode);
 if ErrorCode <> 0 then FValid:= false;
 Val(Minutestext, MinutesValue, ErrorCode);
 if ErrorCode <> 0 then FValid:= false;
 Val(SecondsText, SecondsValue, ErrorCode);
 if ErrorCode <> 0 then FValid:= false;
 DHValue:= Abs(DHValue);
 MinutesValue:= Abs(MinutesValue);
 SecondsValue:= Abs(SecondsValue);
 if FValid then Validate(DHValue, MinutesValue, SecondsValue);
 if FValid then
 begin
   FValue := DHValue + MinutesValue/MINUTES_IN_HOUR + SecondsValue/SECONDS_IN_HOUR;
   if NegativeSign then FValue := -FValue;
 end
 else FValue := ZERO;
 Result := TValidatedDoubleDto.Create(FValue, FValid);
end;



{ TLongitudeValidationHandler }
constructor TLongitudeValidator.Create;
begin
  FSexagesimalValidator := TSexagesimalValidator.Create(MIN_LONGITUDE, MAX_LONGITUDE);
end;

destructor TLongitudeValidator.Destroy;
begin
  FSexagesimalValidator.Free;
  inherited;
end;

function TLongitudeValidator.CalcAndCheck(Degrees, Minutes, Seconds, Direction: String): TValidatedDoubleDto;
var
  NegativeSign: Boolean;
  SexagValue: TValidatedDoubleDto;
begin
  if Direction = LONGITUDE_WEST then NegativeSign := true
  else NegativeSign := false;
  SexagValue:= FSexagesimalValidator.CalcAndCheck(Degrees, Minutes, Seconds, NegativeSign);
  Result:= TValidatedDoubleDto.Create(SexagValue.Value,SexagValue.Valid);
end;

{ TLatitudeValidationHandler }
constructor TLatitudeValidator.Create;
begin
  FSexagesimalValidator := TSexagesimalValidator.Create(MIN_LATITUDE_DEGREES, MAX_LATITUDE_DEGREES);
end;

destructor TLatitudeValidator.Destroy;
begin
  FSexagesimalValidator.Free;
  inherited;
end;

function TLatitudeValidator.CalcAndCheck(Degrees, Minutes, Seconds, Direction: String): TValidatedDoubleDto;
var
  NegativeSign: Boolean;
  SexagValue: TValidatedDoubleDto;
begin
  if Direction = LATITUDE_SOUTH then NegativeSign := true
  else NegativeSign:= false;
  SexagValue:= FSexagesimalValidator.CalcAndCheck(Degrees, Minutes, Seconds, NegativeSign);
  Result:= TValidatedDoubleDto.Create(SexagValue.Value,SexagValue.Valid);
end;

{ TDateValidator }
constructor TDateValidator.Create;
begin
  FSeFrontend := TSeFrontend.Create;
end;

function TDateValidator.CalcAndCheck(PYear, PMonth, PDay: String):TValidatedDateDto;
var
  Year, Month, Day: Integer;
  DateInput: String;
  Valid: Boolean;
begin
  Valid := true;
  DateInput:= PYEar + DIVISION + PMonth + DIVISION + PDay;
  Val(PYear, Year, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  Val(PMonth, Month, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  Val(PDay, Day, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  if Valid and not(FSeFrontend.CheckDate(Day, Month, Year)) then Valid := false;
  if not Valid then begin
    Year:=0; Month:=1; Day:=1;
  end;
  Result:=TValidatedDateDto.Create( Year, Month, Day, DateInput, Valid);
end;


{ TTimeValidator }
function TTimeValidator.CalcAndCheck(PHour, PMinute, PSecond: String): TValidatedTimeDto;
var
  Valid: Boolean;
  Hour, Minute, Second: Integer;
  TimeInput: String;
begin
  Valid := true;
  Val(PHour, Hour, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  Val(PMinute, Minute, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  Val(PSecond, Second, ErrorCode);
  if ErrorCode <> 0 then Valid:= false;
  if Valid then begin
    if (Hour < MIN_HOUR) or (Hour > MAX_HOUR)
    or (Minute < MIN_MINUTE_SECOND) or (Minute > MAX_MINUTE_SECOND)
    or (Second < MIN_MINUTE_SECOND) or (Second > MAX_MINUTE_SECOND) then Valid:=False;
  end;
  if not Valid then begin
    Hour:=0; Minute:=0; Second:=0;
  end;
  TimeInput:= PHour + COLON + PMinute + COLON + PSecond;
  Result:=TValidatedTimeDto.Create(Hour + Minute/MINUTES_IN_HOUR + Second/SECONDS_IN_HOUR, TimeInput, Valid);
end;


end.

