program epidataanalysis;

{$codepage UTF-8}
{$mode objfpc}{$H+}

uses
  // System Units
  {$IFDEF UNIX}
    cthreads,
    clocale,
  {$ENDIF}
  {$IFDEF DARWIN}
    epi_osxlocale,
  {$ENDIF}
  Interfaces, sysutils, Forms,

  // Required EpiData units
  ana_procs, wizard_form, datamodule, main, script_runner,

  // ONLY ADD Stat Dialogs HERE
  freq_contribution, tables_contribution, ctable_contribution,
  means_contribution, describe_contribution, epicurve_contribution,
  histogram_contribution, scatter_contribution, survival_contribution,
  barchart_contribution, pareto_contribution,

  // ONLY ADD GRAPH COMMANDS HERE (delete all other automatically added units).
   scatter, barchart, survival, epicurve, histogram, pareto;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidataanalysis';
end;

function EpiDataVendorName: string;
begin
  result := 'epidata';
end;


begin
  Application.Title := 'Analysis';
  OnGetApplicationName := @EpiDataApplicationName;
  OnGetVendorName := @EpiDataVendorName;

  RequireDerivedFormResource := True;
  Application.Initialize;

  ParseCommandLineOpts;

  if (not CheckAndStartWizard(GetStartupPgm)) then
    Exit;

  Application.CreateForm(TaDM, aDM);
  Application.CreateForm(TMainForm, MainForm);
  ScriptMediator := MainForm;
  Application.Run;
end.

