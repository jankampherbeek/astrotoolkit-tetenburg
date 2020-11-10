unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTimeTextConstructor = class
    public
       function convert(decTime: Double): String;
  end;

implementation


{ TTimeTextConstructor }

function TTimeTextConstructor.convert(decTime: Double): String;
var
  hour, minute, second : Integer;
  hourTxt, minuteTxt, secondTxt, prefix: String;
  tempValue: Double;
begin
  hour:= trunc(decTime);
  tempValue:= decTime - hour;
  tempValue := tempValue * 60.0;
  minute:= trunc(tempValue);
  tempValue:= (tempValue - minute) * 60.0;
  second:= trunc(tempValue);
  if (hour < 10) then prefix:= '0'
  else prefix := '';
  hourTxt:= prefix + IntToStr(hour) + ':';
  if (minute < 10) then prefix := '0'
  else prefix:= '';
  minuteTxt:= prefix + IntToStr(minute) + ':';
  if (second < 10) then prefix:= '0'
  else prefix := '';
  secondTxt:= prefix + IntToStr(second);
  Result:= hourTxt + minuteTxt + secondTxt;
end;


end.

