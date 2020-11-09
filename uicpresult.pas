unit UiCpResult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Domain, CriticalPoint;

type

  { TFormCpResult }

  TFormCpResult = class(TForm)
    BtnClose: TButton;
    Label1: TLabel;
    lblGlyph: TLabel;
    LblValueDateTime: TLabel;
    LblValuePosition: TLabel;
    LblPosition: TLabel;
    LblDateTime: TLabel;
    procedure FormShow;
  private
    function constructDateTimeText(PEventDateTime: TDateTimeDto): String;
    procedure constructPositionText;
  end;

var
  FormCpResult: TFormCpResult;
  Date: TValidatedDateDto;
  Time: TValidatedTimeDto;
  EventDateTime: TDateTimeDto;


implementation

{$R *.lfm}

uses
  UiChartsPositions;

procedure TFormCpResult.FormShow;
var
  dateTimeText: String;
begin
  EventDateTime := FormChartsPositions.EventDateTimeDto;
  dateTimeText:= constructDateTimeText(EventDateTime);
  LblValueDateTime.Caption:= dateTimeText;
  constructPositionText;
end;

  function TFormCpResult.constructDateTimeText(PEventDateTime: TDateTimeDto): String;
  var
    dateTimeText: String;
  begin
    dateTimeText := IntToStr(PEventDateTime.Year) + '-' + IntToStr(PEventDateTime.Month) + '-' +
                    IntToStr(PEventDateTime.Day) + ' Tijd (UT, decimaal): ' + FloatToStr(PEventDateTime.Time);
    Result:= dateTimeText;
  end;

  procedure TFormCpResult.constructPositionText;
  var
    calculator: TCriticalPointCalculator;
    signDmsValue: TSignDmsValue;
    radixDateTimeDto: TDateTimeDto;
    radixMc, solarSpeed, geoLat: Double;
  begin
    calculator:= TCriticalPointCalculator.Create;
    radixDateTimeDto:= FormChartsPositions.DateTimeDto;
    radixMc := FormChartsPositions.Mc;
    solarSpeed:= FormChartsPositions.SolarSpeed;
    geoLat:= FormChartsPositions.GeoLat;

    signDmsValue := calculator.Calculate(radixDateTimeDto,
                                         EventDateTime,
                                         radixMc,
                                         solarSpeed,
                                         geoLat);
    LblValuePosition.Caption:= signDmsValue.Text;
    lblGlyph.Caption:= SignGlyphsArray[signDmsValue.SignId - 1];


  end;

end.

