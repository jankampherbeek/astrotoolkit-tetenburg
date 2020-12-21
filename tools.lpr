program tools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, start, XSharedDictionary, BeSwissDelphi, BeSharedSeFrontend, UiGraph,
  UiChartsPositions, Validators, Domain, CriticalPoint,
  Reincarnation, Utils
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tetenburg';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormChartsPositions, FormChartsPositions);
  Application.Run;
end.

