program epidataanalysis;

{$codepage UTF-8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads,
    clocale,
  {$ENDIF}
  Interfaces,// this includes the LCL widgetset
  sysutils, Forms, ana_procs, wizard_form, datamodule, main,

  // ONLY ADD Stat Dialogs HERE
  freq_contribution, tables_contribution, ctable_contribution,
  means_contribution, describe_contribution, scatter_contribution,

  // ONLY ADD GRAPH COMMANDS HERE (delete all other automatically added units).
  scatter, chartpair, scatter_main_view, scatter_variables_model;

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
  Application.Run;
end.

