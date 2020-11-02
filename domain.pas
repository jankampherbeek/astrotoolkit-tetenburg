unit Domain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBodyDto = class;
  TGlyphsBodiesDto = class;
  TGlyphsSignsDto = class;
  THousePositionDto = class;
  TCelObjectDto = class;
  TFullChartRequest = class;
  TIdNameTextDto = class;

TIdNameTextDtoArray = Array of TIdNameTextDto;
  TBodyDtoArray = Array of TBodyDto;
  TGlyphsBodiesDtoArray = Array of TGlyphsBodiesDto;
  TGlyphsSignsDtoArray = Array of TGlyphsSignsDto;
  THousePositionDtoArray = Array of THousePositionDto;
  TCelObjectDtoArray = Array of TCelObjectDto;
  TFullChartRequestArray = Array of TFullChartRequest;
  TIntArray = Array of Integer;

  { DTO with name and coördinates for location. @br
    The constructor accepts values as given, range checks for longitude and latitude should have been done earlier.  @br
    UnitTest: UtValidatedLocationDto. }
  TValidatedLocationDto = class
  strict private
    FName, FLongitudeInput, FLatitudeInput: String;
    FLongitude, FLatitude: Double;
  public
    constructor Create(PName, PLongitudeInput, PLatitudeInput: String; PLongitude, PLatitude: Double);
    property Name: String read FName;
    property LongitudeInput: String read FLongitudeInput;
    property LatitudeInput: String read FLatitudeInput;
    property Longitude: Double read FLongitude;
    property Latitude: Double read FLatitude;
  end;


    { DTO for calendar date and an indication if the input was valid. @br
    Will be used to create a DateTime.}
  TValidatedDateDto = class
  strict private
    FYear, FMonth, FDay: Integer;
    FDateInput: String;
    FValid: Boolean;
  public
    constructor Create(PYear, PMonth, PDay: Integer; PDateInput: String; PValid: Boolean);
    property Year: Integer read FYear;
    property Month: Integer read FMonth;
    property Day: Integer read FDay;
    property DateInput: String read FDateInput;
    property Valid: Boolean read FValid;
  end;

  { DTO for clock time and an indication if the input was valid. @br
    Time is in UT. }
  TValidatedTimeDto = class
    strict private
      FDecimalTime: Double;
      FTimeInput: String;
      FValid: Boolean;
    public
      constructor Create(PDecimalTime: Double; PTimeInput: String; PValid: Boolean);
      property DecimalTime: Double read FDecimalTime;
      property TimeInput: String read FTimeInput;
      property Valid: Boolean read FValid;
  end;

{ DTO for date and time }
TDateTimeDto = class
strict private
  FYear, FMonth, FDay:Integer;
  FTime: Double;
public
  constructor Create(PYear, PMonth, PDay: Integer; PTime: Double);
  property Year: Integer read FYear;
  property Month: Integer read FMonth;
  property Day: Integer read FDay;
  property Time: Double read FTime;
end;

{ DTO for location }
TLocationDto = class
strict private
  FLongitude, FLatitude: Double;
public
  constructor Create(PLongitude, PLatitude: Double);
  property Longitude: Double read FLongitude;
  property Latitude: Double read FLatitude;
end;


{ Dto for celestial bodies. }
TBodyDto = class
strict private
  FId, FSeId, FBodyCategory: Integer;
public
  constructor Create(PId: Integer; PSeId, PBodyCategory: Integer);
  property Id: Integer read FId;
  property SeId: Integer read FSeId;
  property BodyCategory: Integer read FBodyCategory;
end;



{ Dto for supported glyph-body combinations. }
TGlyphsBodiesDto = class
strict private
  FBodyId: Integer;
  FGlyph: String;
public
  Constructor Create(PBodyId: Integer; PGlyph: String);
  property BodyId : Integer read FBodyId;
  property Glyph: String read FGlyph;
end;

{ Dto for supported glyph-sign combinations. }
TGlyphsSignsDto = class
strict private
  FSignId: Integer;
  FGlyph: String;
public
  Constructor Create(PSignId: Integer; PGlyph: String);
  property SignId : Integer read FSignId;
  property Glyph: String read FGlyph;
end;



{ Dto for positions for a house cusp in ecliptic, equatorial and horizontal coördinates. @br
  No latitude, as this is always zero. @br
  UnitTest: UtHousePositionDto }
THousePositionDto = class
strict private
  FLongitude: Double;
public
  constructor Create(PLongitude: Double);
  property Longitude: Double read FLongitude;
end;

{ DTO for position of celestial object. }
TCelObjectComponentDto = class
strict private
  FMainPos: Double;
public
  constructor Create(CalculatedValues: Array of Double);
  property MainPos: Double read FMainPos;

end;

{ DTO for a fully defined position of a celestial object. }
TCelObjectDto = class
strict private
  FObjectId: Integer;
  FGlyph: String;
  FLongitude: Double;

public
  constructor Create(PObjectId: Integer; PGlyph: String; PLongitude: Double);
  property ObjectId: Integer read FObjectId;
  property Glyph: String read FGlyph;
  property Longitude: Double read FLongitude;
end;

{ Response to a request to calculate asc and mc. }
THousesResponse = class
strict private
  FMc, FAsc: Double;
public
  constructor Create(PMc, PAsc: Double);
  property Mc: Double read FMc;
  property Asc: Double read FAsc;
end;

{ Request for the calculation of a chart.  }
TFullChartRequest = class
strict private
  FName: String;
  FLocation: TValidatedLocationDto;
  FDate: TValidatedDateDto;
  FTime: TValidatedTimeDto;
public
  constructor Create(PName: String; PLocation: TValidatedLocationDto; PDate: TValidatedDateDto;
                     PTime: TValidatedTimedto);
  destructor Destroy; override;
  property Name: String read FName;
  property Location: TValidatedLocationDto read FLocation;
  property Date: TValidatedDateDto read FDate;
  property Time: TValidatedTimeDto read FTime;
end;

{ DTO containing all data for a calculated chart. }
TFullChartResponse = class
strict private
  FName: String;
  FAllObjects: TCelObjectDtoArray;
  FHouses: THousesResponse;
  FFullChartRequest: TFullChartRequest;
public
  constructor Create(PName: String;
                     PAllObjects: TCelObjectDtoArray;
                     PHouses: THousesResponse;
                     PFullChartRequest: TFullChartRequest);
  destructor Destroy; override;
  property Name: String read FName;
  property AllObjects: TCelObjectDtoArray read FAllObjects;
  property Houses: THousesResponse read FHouses;
  property FullChartRequest: TFullChartRequest read FFullChartRequest;
end;


{ Presentable instance for a decimal value, using decimal presentation and 8 positions for the fraction.
  If necessary the fraction is postfixed with zero's to arrive at 8 positions. }
TDecimalValue = class
private
  PresText: String;
  function ConvertInput(InputValue: Double): String;
public
  Constructor Create(PInputValue: Double);
  property Text: String read PresText;
end;

{ Presentable instance for a sexagesimal value, using degrees, minutes and seconds but no fraction.
  If necessary the minutes and seconds are prefixed with zero's to arrive at 2 positions. }
TSexagesimalValue = class
private
  PresText: String;
  function ConvertInput(InputValue: Double): String;
public
  Constructor Create(PInputValue: Double);
  property Text: String read PresText;
end;

{ Presentable instance for a sexagesimal value, using signs, degrees, minutes and seconds but no fraction.@br
  If necessary the minutes and seconds are prefixed with zero's to arrive at 2 positions.}
TSignDMSValue = class
private
  FSignId: Integer;
  FText: String;
  SexagValue: TSexagesimalValue;
  function ConvertInput(InputValue: Double): String;
public
  procedure SetNewLongitude(PLongitude: Double);
  property SignId: Integer read FSignId;
  property Text: String read FText;
end;




{ Dto for combinations of id, name and additional text (typically description or glyph). @br
  UnitTest: UtIdNameTextDto. }
TIdNameTextDto = class
strict private
  FId: Integer;
  FName, FText: String;
public
  constructor Create(PId: Integer; PName, PText: String);
  property Id: Integer read FId;
  property Name: String read FName;
  property Text: String read FText;
end;


{ DTO with a single double value and an indication if the input was valid. @br
  If Valid is false, value will contain 0.0. }
TValidatedDoubleDto = class
strict private
  FValue: Double;
  FValid: Boolean;
public
  constructor Create(PValue: Double; PValid: Boolean);
  property Value:Double read FValue;
  property Valid:Boolean read FValid;
end;



implementation

uses XSharedDictionary;


{ TValidatedLocationDto }
constructor TValidatedLocationDto.Create(PName, PLongitudeInput, PLatitudeInput: String; PLongitude, PLatitude: Double);
begin
  FName:= PName;
  FLongitudeInput:= PLongitudeInput;
  FLatitudeInput:= PLatitudeInput;
  FLongitude:= PLongitude;
  FLatitude:= PLatitude;
end;

{ TValidatedDatedto }
constructor TValidatedDateDto.Create(PYear, PMonth, PDay: Integer; PDateInput: String;  PValid: Boolean);
begin
  FYear:=PYear;
  FMonth:=PMonth;
  FDay:=PDay;
  FDateInput:= PDateInput;
  FValid:=PValid;
end;

{ TValidatedTimeDto }
constructor TValidatedTimeDto.Create(PDecimalTime: Double; PTimeInput: String; PValid: Boolean);
begin
  FDecimalTime:= PDecimalTime;
  FTimeInput:= PTimeInput;
  FValid:=PValid;
end;



{ TBodyDto }
constructor TBodyDto.Create(PId: Integer; PSeId, PBodyCategory: Integer);
begin
  FId:= Pid;
  FSeId:= PSeId;
  FBodyCategory:= PBodyCategory;
end;

{ TDateTimeDto }
constructor TDateTimeDto.Create(PYear, PMonth, PDay: Integer; PTime: Double);
begin
  FYear:= PYear;
  FMonth:= PMonth;
  FDay:= PDay;
  FTime:= PTime;
end;

{ TLocationDto }
constructor TLocationDto.Create(PLongitude, PLatitude: Double);
begin
  FLongitude:= PLongitude;
  FLatitude:= PLatitude;
end;

{ TGlyphsBodiesDto }
constructor TGlyphsBodiesDto.Create(PBodyId: Integer; PGlyph: String);
begin
  FBodyId:= PBodyId;
  FGlyph:= PGlyph;
end;

{ TGlyphsSignsDto }
constructor TGlyphsSignsDto.Create(PSignId: Integer; PGlyph: String);
begin
  if ((PSignId > 0) and (PSignId < 13)) then begin
     FSignId:= PSignId;
     FGlyph:= PGlyph;
  end
  else begin
    PSignId:= 0;
    FGlyph:= 'Error';
  end;
end;


{ THousePositionDto }
constructor THousePositionDto.Create(PLongitude: Double);
begin
  FLongitude:= PLongitude;
end;

{ TCelObjectComponentDto }
constructor TCelObjectComponentDto.Create(CalculatedValues: Array of Double);
begin
  FMainPos:= CalculatedValues[0];
end;


{ TCelObjectDto }
constructor TCelObjectDto.Create(PObjectId: Integer; PGlyph: String; PLongitude: Double);
begin
  FObjectId:= PObjectId;
  FGlyph:= PGlyph;
  FLongitude:= PLongitude;
end;


{ THouseResponse }
constructor THousesResponse.Create(PMc, PAsc: Double);
begin
  FMc:= PMc;
  FAsc:= PAsc;
end;


{ TFullChartRequest }
constructor TFullChartRequest.Create(PName: String; PLocation: TValidatedLocationDto;
                                     PDate: TValidatedDateDto; PTime: TValidatedTimeDto);
begin
  FName:= PName;
  FLocation:= PLocation;
  FDate:= PDate;
  FTime:= PTime;
end;

destructor TFullChartRequest.Destroy;
begin
  FLocation.Free;
  FDate.Free;
  FTime.Free;
  inherited;
end;

{ TFullChartResponse }
constructor TFullChartResponse.Create(PName: String; PAllObjects: TCelObjectDtoArray; PHouses: THousesResponse;
                                      PFullChartRequest: TFullChartRequest);
begin
  FName:= PName;
  FAllObjects:= PAllObjects;
  FHouses:= PHouses;
  FFullChartRequest:= PFullChartRequest;
end;

destructor TFullChartResponse.Destroy;
var
  i: Integer;
begin
  FHouses.Free;
  FFullChartRequest.Free;
  for i:= Low(FAllObjects) to High(FAllObjects) do FAllObjects[i].Free;
  SetLength(FAllObjects, 0);
  inherited;
end;


{ TDecimalValue }
Constructor TDecimalValue.Create(PInputValue: Double);
begin
  PresText:= ConvertInput(PInputValue);
end;

function TDecimalValue.ConvertInput(InputValue: Double): String;
begin
   Result:= Format('%3.8f',[InputValue]);
end;


{ TSexagesimalValue }
Constructor TSexagesimalValue.Create(PInputValue: Double);
begin
  PresText:= ConvertInput(PInputValue);
end;

function TSexagesimalValue.ConvertInput(InputValue: Double): String;
var
  DegTxt, MinTxt, SecTxt: String;
  Degrees, Minutes, Seconds: Integer;
  TempAmount, AbsInputValue: Double;
  IsNegative: Boolean;
begin
  IsNegative:= (InputValue < 0.0);
  AbsInputValue:= Abs(InputValue);
  Degrees:= Trunc(AbsInputValue);
  TempAmount:= Frac(AbsInputValue) * MINUTES_IN_DEGREE;
  Minutes:= Trunc(TempAmount);
  Seconds:= Trunc(frac(TempAmount) * SECONDS_IN_MINUTE);
  DegTxt := Format('%3d',[Degrees]);
  MinTxt := Format('%2d',[Minutes]);
  SecTxt := Format('%2d',[Seconds]);
  if IsNegative then DegTxt:= '-' + TrimLeft(DegTxt);
  Result:= Concat(DegTxt, DEGREE_SIGN, MinTxt, MINUTE_SIGN, SecTxt, SECOND_SIGN);
end;

{ TSignDMSValue }

procedure TSignDMSValue.SetNewLongitude(PLongitude: Double);
begin
  FText:= ConvertInput(PLongitude);
end;

function TSignDMSValue.ConvertInput(InputValue: Double): String;
var
  Degrees: Integer;
  DecDegreesWithoutSign: Double;
begin
  Degrees:= Trunc(InputValue);
  FSignId:= Trunc(Degrees DIV DEGREES_IN_SIGN) + 1;
  DecDegreesWithoutSign:= InputValue - ((FSignId - 1) * DEGREES_IN_SIGN);
  SexagValue:= TSexagesimalValue.Create(DecDegreesWithoutSign);
  Result:= SexagValue.Text;
end;


{ TIdNameTextDto }
constructor TIdNameTextDto.Create(PId: Integer; PName, PText: String);
begin
  FId:= PId;
  FName:= PName;
  FText:= PText;
end;


{ TValidatedDoubleDto }
constructor TValidatedDoubleDto.Create(PValue: Double; PValid: Boolean);
begin
  FValue:=PValue;
  FValid:=PValid;
end;




end.

