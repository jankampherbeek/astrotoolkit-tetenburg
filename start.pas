unit start;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ActnList, HelpIntfs,
  UiGraph, UiChartsPositions, Domain;

type

{ Form for inputting data for the calculation of a chart. }
{ TForm1 }
TForm1 = class(TForm)
  BtnCalc: TButton;
  BtnClose: TButton;
  EditDateMonth: TEdit;
  EditDateDay: TEdit;
  EditDateYear: TEdit;
  EditLatitudeDegrees: TEdit;
  EditLatitudeMinutes: TEdit;
  EditLatitudeSeconds: TEdit;
  EditName: TEdit;
  EditLongitudeDegrees: TEdit;
  EditTimeHour: TEdit;
  EditLongitudeMinutes: TEdit;
  EditTimeMinute: TEdit;
  EditLongitudeSeconds: TEdit;
  EditTimeSecond: TEdit;
  lblEnterData: TLabel;
  LblStatus: TLabel;
  LblTime: TLabel;
  LblDate: TLabel;
  LblLatitude: TLabel;
  LblName: TLabel;
  LblLongitude: TLabel;
  LblTitle: TLabel;
  RBLatitudeNorth: TRadioButton;
  RBLatitudeSouth: TRadioButton;
  RBLongitudeWest: TRadioButton;
  RBLongitudeEast: TRadioButton;
  RGLatitude: TRadioGroup;
  RGLongitude: TRadioGroup;
  procedure BtnCalcClick;
  procedure BtnCloseClick;
  procedure BtnHelpClick(Sender: TObject);
  procedure FormShow;

private
  procedure ProcessName;
  procedure ProcessLongitude;
  procedure ProcessLatitude;
  procedure ProcessDate;
  procedure ProcessTime;
  procedure DefineLookandFeel;
public

end;

var
  ChartReqLocationName, ErrorText, LongitudeInput, LatitudeInput: String;
  Rating: Integer;
  ReadyForCalc, ErrorFlag: Boolean;
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  XSharedDictionary, Validators;

const
  HELP_TXT = 'HTML/DataInvoer.html';

var
  Name, LocationName: String;
  Longitude, Latitude: TValidatedDoubleDto;
  Date: TValidatedDateDto;
  Time: TValidatedTimeDto;
  Styling: TStyling;

const
  ERROR_LONGITUDE = 'Corrigeer de invoer voor de lengte.';
  ERROR_LATITUDE = 'Corrigeeer de invoer voor de breedte.';
  ERROR_DATE = 'Corrigeer de invoer voor de datum.';
  ERROR_TIME = 'Corrigeer de invoer voor de tijd.';
  INSTRUCTION = 'Voer alle waarden in en klik op ''Bereken''.';
  EMPTY_SECONDS = '00';
  LAT_NORTH = 'N';
  LAT_SOUTH = 'Z';
  LONG_EAST = 'O';
  LONG_WEST = 'W';

{ TForm1 }
procedure TForm1.ProcessName;
begin
  EditName.Color:= clDefault;
  Name := Trim(EditName.Text);
end;

procedure TForm1.ProcessLongitude;
var
  LongitudeValidator: TLongitudeValidator;
  Direction: String;
begin
  EditLongitudeDegrees.Color:= clDefault;
  EditLongitudeMinutes.Color:= clDefault;
  EditLongitudeSeconds.Color:= clDefault;
  if RBLongitudeEast.Checked then Direction:= LONG_EAST else Direction := LONG_WEST;
  LongitudeValidator:= TLongitudeValidator.Create;
  if EditLongitudeSeconds.Text = '' then EditLongitudeSeconds.Text := EMPTY_SECONDS;
  Longitude := LongitudeValidator.CalcAndCheck(EditLongitudeDegrees.Text,
                                               EditLongitudeMinutes.Text,
                                               EditLongitudeSeconds.Text,
                                               Direction);
  LongitudeInput:= EditLongitudeDegrees.Text + DOT + EditLongitudeMinutes.Text +
                   DOT + EditLongitudeSeconds.Text + SPACE + Direction;
  if not Longitude.Valid then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_LONGITUDE + LineEnding;
    EditLongitudeDegrees.Color:= clYellow;
    EditLongitudeMinutes.Color:= clYellow;
    EditLongitudeSeconds.Color:= clYellow;
  end;
end;

procedure TForm1.ProcessLatitude;
var
  LatitudeValidator: TLatitudeValidator;
  Direction: String;
begin
  EditLatitudeDegrees.Color:= clDefault;
  EditLatitudeMinutes.Color:= clDefault;
  EditLatitudeSeconds.Color:= clDefault;
  if RBLatitudeNorth.Checked then Direction := LAT_NORTH
  else Direction := LAT_SOUTH;
  LatitudeValidator:= TLatitudeValidator.Create;
  if EditLatitudeSeconds.Text = '' then EditLatitudeSeconds.Text := EMPTY_SECONDS;
  Latitude := LatitudeValidator.CalcAndCheck(EditLatitudeDegrees.Text,
                                             EditLatitudeMinutes.Text,
                                             EditLatitudeSeconds.Text,
                                             Direction);
  LatitudeInput:= EditLatitudeDegrees.Text + DOT + EditLatitudeMinutes.Text +
                  DOT + EditLatitudeSeconds.Text + SPACE + Direction;
  if not Latitude.Valid then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_LATITUDE + LineEnding;
    EditLatitudeDegrees.Color:= clYellow;
    EditLatitudeMinutes.Color:= clYellow;
    EditLatitudeSeconds.Color:= clYellow;
  end;
end;

procedure TForm1.ProcessDate;
var
  DateValidator: TDateValidator;
begin
  EditDateDay.Color := clDefault;
  EditDateMonth.Color := clDefault;
  EditDateYear.Color := clDefault;

  DateValidator:= TDateValidator.Create;
  Date:=DateValidator.CalcAndCheck(EditDateYear.Text,
                                   EditDateMonth.Text,
                                   EditDateDay.Text);
  if not Date.Valid then  begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_DATE + LineEnding;
    EditDateDay.Color := clYellow;
    EditDateMonth.Color := clYellow;
    EditDateYear.Color := clYellow;
  end;
end;

procedure TForm1.ProcessTime;
var
  TimeValidator: TTimeValidator;
begin
  EditTimeHour.Color:=clDefault;
  EditTimeMinute.Color:=clDefault;
  EditTimeSecond.Color:=clDefault;
  TimeValidator:= TTimeValidator.Create;
  if EditTimeSecond.Text = '' then EditTimeSecond.Text := EMPTY_SECONDS;
  Time:=TimeValidator.CalcAndCheck(EditTimeHour.Text,
                                   EditTimeMinute.Text,
                                   EditTimeSecond.Text);
  if not Time.Valid then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_TIME + LineEnding;
    EditTimeHour.Color := clYellow;
    EditTimeMinute.Color := clYellow;
    EditTimeSecond.Color := clYellow;
  end;
end;



procedure TForm1.BtnCalcClick;
var
  Location: TValidatedLocationDto;
  DateTimeDto: TDateTimeDto;
  LocationDto: TLocationDto;
begin
  LblStatus.Color:= Styling.DataBgColor;
  LblStatus.Caption:= INSTRUCTION;
  ErrorFlag := False;
  ErrorText := EMPTY_STRING;
  ProcessName;
  ProcessLongitude;
  ProcessLatitude;
  ProcessDate;
  ProcessTime;

  if ErrorFlag then begin
    LblStatus.Color:= Styling.DataErrorColor;
    LblStatus.Caption:= ErrorText;
  end else begin
    LongitudeInput:= '';
    LatitudeInput:= '';
    Location:= TValidatedLocationDto.Create(LocationName, LongitudeInput, LatitudeInput, Longitude.Value, Latitude.Value);
    ReadyForCalc:= true;
    DateTimeDto:= TDateTimeDto.Create(Date.Year, Date.Month, Date.Day, Time.DecimalTime);
    LocationDto:= TLocationDto.Create(Location.Longitude, Location.Latitude);
    FormChartsPositions.LocationDto:= LocationDto;
    FormChartsPositions.DateTimeDto:= DateTimeDto;
    FormChartsPositions.ShowModal;
    Close;
  end;
end;


procedure TForm1.BtnCloseClick;
begin
  Close;
end;

procedure TForm1.BtnHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', HELP_TXT);
end;


procedure TForm1.FormShow;
begin
  DefineLookandFeel;
  LblStatus.Caption:= INSTRUCTION;
  ReadyForCalc:= false;
end;



procedure TForm1.DefineLookandFeel;
begin
  Styling:= TStyling.Create;
  Form1.Color:= Styling.DataBgColor;
  // Labels
  LblTitle.Font:= Styling.DataFontSubHeading;
  LblName.Font:= Styling.DataFontText;
  LblLongitude.Font:= Styling.DataFontText;
  LblLatitude.Font:= Styling.DataFontText;
  LblDate.Font:= Styling.DataFontText;
  LblTime.Font:= Styling.DataFontText;
  LblStatus.Color:= Styling.DataBgColor;
  // Radio buttons
  RBLongitudeEast.Font:= Styling.DataFontText;
  RBLongitudeWest.Font:= Styling.DataFontText;
  RBLatitudeNorth.Font:= Styling.DataFontText;
  RBLatitudeSouth.Font:= Styling.DataFontText;
  // Radio groups
  RGLongitude.Font:= Styling.DataFontText;
  RGLatitude.Font:= Styling.DataFontText;

end;

end.

