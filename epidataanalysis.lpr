program epidataanalysis;

{$codepage UTF-8}
{$mode objfpc}{$H+}

uses
  // System Units
  {$IFDEF UNIX}
    cthreads,
    clocale,
  {$ENDIF}
  Interfaces, sysutils, Forms,

  // Required EpiData units
  ana_procs, wizard_form, datamodule, main, about, ast, ast_builder, ast_types,
  cmdedit, executor, expr, parser, parser_types, result_variables, runtest,
  script_runner, select_stack, epi_script_function_createdate,
  epi_script_function_createtime, epi_script_function_datefunctions,
  epi_script_function_mathfunctions, epi_script_function_observations,
  epi_script_function_resourcestrings, epi_script_function_stringfunctions,
  epi_script_function_systemfunctions, epi_script_function_timefunctions,
  scandate_from_fpc, editor_form, editor_form2, editor_page,
  editor_pgm_highlighter, options_cssfileoption, options_filesoptions,
  options_fontoptions, options_hashmap, options_string_array, options_table,
  select_datafile, stat_dialog, stat_dialog_action, stat_dialog_contribution,
  stat_dialog_custom_view, stat_dialog_footer, fields_combobox, commandtree,
  history, statfunctions,
  {$IFDEF DARWIN}
  epi_osxlocale, 
  {$ENDIF}
  chart_options,
  chart_options_model, chart_options_view,

  // ONLY ADD Stat Dialogs HERE
  freq_contribution, freq_mainoptions_model, freq_mainoptions_view,
  freq_variable_model, freq_variable_view, tables_contribution, tables_model,
  tables_primaryoption_model, tables_primaryoption_view,
  tables_statisticoptions_model, tables_statisticoptions_view,
  tables_variables_view, ctable_contribution, ctable_model,
  ctable_primaryoption_model, ctable_primaryoption_view,
  ctable_statisticoptions_model, ctable_statisticoptions_view,
  ctable_variables_view, means_contribution, means_model,
  means_primaryoption_model, means_primaryoption_view,
  means_statisticoptions_model, means_statisticoptions_view,
  means_variables_view, describe_contribution, describe_model,
  describe_primaryoption_model, describe_primaryoption_view,
  describe_statisticoptions_model, describe_statisticoptions_view,
  describe_variables_view, epicurve_contribution, epicurve_model,
  epicurve_primaryoption_model, epicurve_primaryoption_view,
  epicurve_variables_view, histogram_contribution, histogram_model,
  histogram_primaryoption_model, histogram_primaryoption_view,
  histogram_variables_view, survival_contribution, survival_model,
  survival_primaryoption_model, survival_primaryoption_view,
  survival_statisticoptions_model, survival_statisticoptions_view,
  survival_variables_view, barchart_contribution, barchart_model,
  barchart_primaryoption_model, barchart_primaryoption_view,
  barchart_variables_view, pareto_contribution, scatter_contribution,
  scatter_main_view, scatter_primaryoption_model, scatter_primaryoption_view,
  scatter_variables_model,

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
  Application.Run;
end.

