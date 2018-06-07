program project1;

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
  tables_stat_fexp, tables_stat_or, tables_stat_rr;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidataanalysis';
//  result := '';
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
  Application.CreateForm(TaDM, aDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

