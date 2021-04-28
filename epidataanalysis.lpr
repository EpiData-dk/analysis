program epidataanalysis;

{$codepage UTF-8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads,
    clocale,
  {$ENDIF}
  Interfaces,// this includes the LCL widgetset
  sysutils, Forms, FrameViewer09, lclextensions_package, lnetbase, main,
  executor, expr, ast, ast_types, ast_builder, datamodule, select_stack,
  result_variables, runtest, parser, about, epidatacore, statfunctions, means,
  generalutils, interval_types, outputcreator, outputgenerator_base,
  outputgenerator_txt, outputgenerator_html, options_hashmap, list, edit, drop,
  history, cmdedit, analysis_statusbar, selected_count_statusbar_item,
  workingdir_statusbar_item, systemcmd, parser_types, Variables, Token, Symbol,
  SourceFeeder, Rule, MemLeakFinder, LRAction, GrammarReader, GOLDParser,
  FAState, epi_script_function_mathfunctions, epi_script_function_createdate,
  epi_script_function_timefunctions, epi_script_function_stringfunctions,
  epi_script_function_resourcestrings, epi_script_function_datefunctions,
  epi_script_function_createtime, ana_documentfile, editor_form, freq,
  executing_statusbar_item, epi_script_function_systemfunctions, ana_procs,
  merge, integrity_tests,
  {$IFDEF EPI_CHROMIUM_HTML}
  htmlviewer, htmlviewer_osr,
  {$ENDIF}
  textviewer, outputviewer_types, oldhtmlviewer, options_utils, ana_globals,
  browse4, report, options_fontoptions, save_output,
  epi_script_function_observations, options_filesoptions, aggregate,
  aggregate_types, tables, tables_types, options_table, tables_stat_chi2,
  tables_stat_fexp, tables_stat_or, tables_stat_rr, ctable, wizard_form,
  commandtree, recode, select_datafile, history_form, varnames_form,
  projecttree_form, commandtree_form, auto_position_form, editor_form2,
  editor_page, editor_pgm_highlighter, scandate_from_fpc, options_string_array,
  options_cssfileoption, stat_dialog_contribution, stat_dialog,
  stat_dialog_action, fields_combobox, stat_dialog_footer, script_runner,
  freq_contribution, freq_variable_view, freq_variable_model,
  tables_contribution, tables_statisticoptions_view,
  tables_statisticoptions_model, freq_mainoptions_model, freq_mainoptions_view,
  stat_dialog_custom_view, ctable_contribution, ctable_model,
  ctable_primaryoption_model, ctable_primaryoption_view,
  ctable_statisticoptions_model, ctable_statisticoptions_view,
  ctable_variables_view, means_contribution, means_model, means_variables_view,
  means_primaryoption_model, means_primaryoption_view,
  means_statisticoptions_model, means_statisticoptions_view,
  describe_contribution, describe_model, describe_primaryoption_model,
  describe_primaryoption_view, describe_statisticoptions_model,
  describe_statisticoptions_view, describe_variables_view;

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

