#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling ana_procs
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_procs.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_procs.s
if [ $? != 0 ]; then DoExitAsm ana_procs; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_procs.s
echo Assembling ast_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_types.s
if [ $? != 0 ]; then DoExitAsm ast_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_types.s
echo Assembling result_variables
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/result_variables.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/result_variables.s
if [ $? != 0 ]; then DoExitAsm result_variables; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/result_variables.s
echo Assembling options_hashmap
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_hashmap.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_hashmap.s
if [ $? != 0 ]; then DoExitAsm options_hashmap; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_hashmap.s
echo Assembling ana_globals
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_globals.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_globals.s
if [ $? != 0 ]; then DoExitAsm ana_globals; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_globals.s
echo Assembling outputcreator
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputcreator.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputcreator.s
if [ $? != 0 ]; then DoExitAsm outputcreator; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputcreator.s
echo Assembling symbol
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/symbol.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/symbol.s
if [ $? != 0 ]; then DoExitAsm symbol; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/symbol.s
echo Assembling rule
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/rule.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/rule.s
if [ $? != 0 ]; then DoExitAsm rule; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/rule.s
echo Assembling token
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/token.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/token.s
if [ $? != 0 ]; then DoExitAsm token; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/token.s
echo Assembling variables
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/variables.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/variables.s
if [ $? != 0 ]; then DoExitAsm variables; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/variables.s
echo Assembling lraction
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/lraction.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/lraction.s
if [ $? != 0 ]; then DoExitAsm lraction; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/lraction.s
echo Assembling fastate
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/fastate.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/fastate.s
if [ $? != 0 ]; then DoExitAsm fastate; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/fastate.s
echo Assembling sourcefeeder
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/sourcefeeder.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/sourcefeeder.s
if [ $? != 0 ]; then DoExitAsm sourcefeeder; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/sourcefeeder.s
echo Assembling grammarreader
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/grammarreader.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/grammarreader.s
if [ $? != 0 ]; then DoExitAsm grammarreader; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/grammarreader.s
echo Assembling goldparser
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/goldparser.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/goldparser.s
if [ $? != 0 ]; then DoExitAsm goldparser; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/goldparser.s
echo Assembling select_stack
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/select_stack.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/select_stack.s
if [ $? != 0 ]; then DoExitAsm select_stack; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/select_stack.s
echo Assembling parser_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser_types.s
if [ $? != 0 ]; then DoExitAsm parser_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser_types.s
echo Assembling ana_documentfile
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_documentfile.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_documentfile.s
if [ $? != 0 ]; then DoExitAsm ana_documentfile; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ana_documentfile.s
echo Assembling outputgenerator_base
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_base.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_base.s
if [ $? != 0 ]; then DoExitAsm outputgenerator_base; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_base.s
echo Assembling history
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/history.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/history.s
if [ $? != 0 ]; then DoExitAsm history; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/history.s
echo Assembling cmdedit
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/cmdedit.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/cmdedit.s
if [ $? != 0 ]; then DoExitAsm cmdedit; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/cmdedit.s
echo Assembling workingdir_statusbar_item
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/workingdir_statusbar_item.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/workingdir_statusbar_item.s
if [ $? != 0 ]; then DoExitAsm workingdir_statusbar_item; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/workingdir_statusbar_item.s
echo Assembling selected_count_statusbar_item
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/selected_count_statusbar_item.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/selected_count_statusbar_item.s
if [ $? != 0 ]; then DoExitAsm selected_count_statusbar_item; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/selected_count_statusbar_item.s
echo Assembling executing_statusbar_item
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/executing_statusbar_item.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/executing_statusbar_item.s
if [ $? != 0 ]; then DoExitAsm executing_statusbar_item; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/executing_statusbar_item.s
echo Assembling analysis_statusbar
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/analysis_statusbar.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/analysis_statusbar.s
if [ $? != 0 ]; then DoExitAsm analysis_statusbar; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/analysis_statusbar.s
echo Assembling commandtree
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree.s
if [ $? != 0 ]; then DoExitAsm commandtree; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree.s
echo Assembling auto_position_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/auto_position_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/auto_position_form.s
if [ $? != 0 ]; then DoExitAsm auto_position_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/auto_position_form.s
echo Assembling history_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/history_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/history_form.s
if [ $? != 0 ]; then DoExitAsm history_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/history_form.s
echo Assembling varnames_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/varnames_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/varnames_form.s
if [ $? != 0 ]; then DoExitAsm varnames_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/varnames_form.s
echo Assembling projecttree_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/projecttree_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/projecttree_form.s
if [ $? != 0 ]; then DoExitAsm projecttree_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/projecttree_form.s
echo Assembling commandtree_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree_form.s
if [ $? != 0 ]; then DoExitAsm commandtree_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/commandtree_form.s
echo Assembling stat_dialog_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_contribution.s
if [ $? != 0 ]; then DoExitAsm stat_dialog_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_contribution.s
echo Assembling stat_dialog_footer
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_footer.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_footer.s
if [ $? != 0 ]; then DoExitAsm stat_dialog_footer; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_footer.s
echo Assembling script_runner
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/script_runner.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/script_runner.s
if [ $? != 0 ]; then DoExitAsm script_runner; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/script_runner.s
echo Assembling stat_dialog
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog.s
if [ $? != 0 ]; then DoExitAsm stat_dialog; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog.s
echo Assembling macoshtmlviewer
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/macoshtmlviewer.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/macoshtmlviewer.s
if [ $? != 0 ]; then DoExitAsm macoshtmlviewer; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/macoshtmlviewer.s
echo Assembling outputgenerator_html
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_html.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_html.s
if [ $? != 0 ]; then DoExitAsm outputgenerator_html; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_html.s
echo Assembling cocoawebviewer
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/cocoawebviewer.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/cocoawebviewer.s
if [ $? != 0 ]; then DoExitAsm cocoawebviewer; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/cocoawebviewer.s
echo Assembling outputgenerator_txt
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_txt.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_txt.s
if [ $? != 0 ]; then DoExitAsm outputgenerator_txt; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputgenerator_txt.s
echo Assembling options_fontoptions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_fontoptions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_fontoptions.s
if [ $? != 0 ]; then DoExitAsm options_fontoptions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_fontoptions.s
echo Assembling ast_builder
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_builder.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_builder.s
if [ $? != 0 ]; then DoExitAsm ast_builder; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast_builder.s
echo Assembling parser
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser.s
if [ $? != 0 ]; then DoExitAsm parser; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/parser.s
echo Assembling about
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/about.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/about.s
if [ $? != 0 ]; then DoExitAsm about; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/about.s
echo Assembling editor_form
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form.s
if [ $? != 0 ]; then DoExitAsm editor_form; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form.s
echo Assembling browse4
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/browse4.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/browse4.s
if [ $? != 0 ]; then DoExitAsm browse4; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/browse4.s
echo Assembling editor_pgm_highlighter
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_pgm_highlighter.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_pgm_highlighter.s
if [ $? != 0 ]; then DoExitAsm editor_pgm_highlighter; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_pgm_highlighter.s
echo Assembling editor_page
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_page.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_page.s
if [ $? != 0 ]; then DoExitAsm editor_page; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_page.s
echo Assembling editor_form2
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form2.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form2.s
if [ $? != 0 ]; then DoExitAsm editor_form2; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/editor_form2.s
echo Assembling charttitles
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.s
if [ $? != 0 ]; then DoExitAsm charttitles; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.s
echo Assembling chartaxesconfiguration
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.s
if [ $? != 0 ]; then DoExitAsm chartaxesconfiguration; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.s
echo Assembling chartconfiguration
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.s
if [ $? != 0 ]; then DoExitAsm chartconfiguration; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.s
echo Assembling chartpair
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartpair.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartpair.s
if [ $? != 0 ]; then DoExitAsm chartpair; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartpair.s
echo Assembling chartcommandresult
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.s
if [ $? != 0 ]; then DoExitAsm chartcommandresult; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.s
echo Assembling chartcommandresult.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.impl.s
if [ $? != 0 ]; then DoExitAsm chartcommandresult.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommandresult.impl.s
echo Assembling chartaxesconfiguration.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.impl.s
if [ $? != 0 ]; then DoExitAsm chartaxesconfiguration.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartaxesconfiguration.impl.s
echo Assembling charttitles.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.impl.s
if [ $? != 0 ]; then DoExitAsm charttitles.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/charttitles.impl.s
echo Assembling chartconfiguration.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.impl.s
if [ $? != 0 ]; then DoExitAsm chartconfiguration.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartconfiguration.impl.s
echo Assembling chartfactory.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.impl.s
if [ $? != 0 ]; then DoExitAsm chartfactory.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.impl.s
echo Assembling chartfactory
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.s
if [ $? != 0 ]; then DoExitAsm chartfactory; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartfactory.s
echo Assembling graphform
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.s
if [ $? != 0 ]; then DoExitAsm graphform; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.s
echo Assembling savegraphaction
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphaction.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphaction.s
if [ $? != 0 ]; then DoExitAsm savegraphaction; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphaction.s
echo Assembling graphpopupmenu
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphpopupmenu.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphpopupmenu.s
if [ $? != 0 ]; then DoExitAsm graphpopupmenu; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphpopupmenu.s
echo Assembling savegraphdialogaction
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphdialogaction.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphdialogaction.s
if [ $? != 0 ]; then DoExitAsm savegraphdialogaction; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/savegraphdialogaction.s
echo Assembling graphform.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.impl.s
if [ $? != 0 ]; then DoExitAsm graphform.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphform.impl.s
echo Assembling graphformfactory.impl
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.impl.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.impl.s
if [ $? != 0 ]; then DoExitAsm graphformfactory.impl; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.impl.s
echo Assembling graphformfactory
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.s
if [ $? != 0 ]; then DoExitAsm graphformfactory; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphformfactory.s
echo Assembling main
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/main.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/main.s
if [ $? != 0 ]; then DoExitAsm main; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/main.s
echo Assembling textviewer
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/textviewer.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/textviewer.s
if [ $? != 0 ]; then DoExitAsm textviewer; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/textviewer.s
echo Assembling stat_dialog_action
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_action.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_action.s
if [ $? != 0 ]; then DoExitAsm stat_dialog_action; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_action.s
echo Assembling outputviewer_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputviewer_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputviewer_types.s
if [ $? != 0 ]; then DoExitAsm outputviewer_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/outputviewer_types.s
echo Assembling datamodule
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/datamodule.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/datamodule.s
if [ $? != 0 ]; then DoExitAsm datamodule; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/datamodule.s
echo Assembling epi_script_function_resourcestrings
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_resourcestrings.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_resourcestrings.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_resourcestrings; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_resourcestrings.s
echo Assembling epi_script_function_mathfunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_mathfunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_mathfunctions.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_mathfunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_mathfunctions.s
echo Assembling runtest
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/runtest.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/runtest.s
if [ $? != 0 ]; then DoExitAsm runtest; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/runtest.s
echo Assembling chartcommand
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommand.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommand.s
if [ $? != 0 ]; then DoExitAsm chartcommand; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/chartcommand.s
echo Assembling graphcommandexecutor
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphcommandexecutor.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphcommandexecutor.s
if [ $? != 0 ]; then DoExitAsm graphcommandexecutor; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/graphcommandexecutor.s
echo Assembling options_filesoptions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_filesoptions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_filesoptions.s
if [ $? != 0 ]; then DoExitAsm options_filesoptions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_filesoptions.s
echo Assembling options_table
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_table.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_table.s
if [ $? != 0 ]; then DoExitAsm options_table; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_table.s
echo Assembling options_string_array
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_string_array.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_string_array.s
if [ $? != 0 ]; then DoExitAsm options_string_array; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_string_array.s
echo Assembling options_cssfileoption
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_cssfileoption.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_cssfileoption.s
if [ $? != 0 ]; then DoExitAsm options_cssfileoption; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_cssfileoption.s
echo Assembling list
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/list.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/list.s
if [ $? != 0 ]; then DoExitAsm list; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/list.s
echo Assembling edit
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/edit.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/edit.s
if [ $? != 0 ]; then DoExitAsm edit; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/edit.s
echo Assembling drop
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/drop.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/drop.s
if [ $? != 0 ]; then DoExitAsm drop; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/drop.s
echo Assembling systemcmd
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/systemcmd.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/systemcmd.s
if [ $? != 0 ]; then DoExitAsm systemcmd; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/systemcmd.s
echo Assembling merge
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/merge.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/merge.s
if [ $? != 0 ]; then DoExitAsm merge; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/merge.s
echo Assembling integrity_tests
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/integrity_tests.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/integrity_tests.s
if [ $? != 0 ]; then DoExitAsm integrity_tests; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/integrity_tests.s
echo Assembling report
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/report.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/report.s
if [ $? != 0 ]; then DoExitAsm report; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/report.s
echo Assembling save_output
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/save_output.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/save_output.s
if [ $? != 0 ]; then DoExitAsm save_output; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/save_output.s
echo Assembling statfunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/statfunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/statfunctions.s
if [ $? != 0 ]; then DoExitAsm statfunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/statfunctions.s
echo Assembling generalutils
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/generalutils.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/generalutils.s
if [ $? != 0 ]; then DoExitAsm generalutils; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/generalutils.s
echo Assembling aggregate_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate_types.s
if [ $? != 0 ]; then DoExitAsm aggregate_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate_types.s
echo Assembling aggregate
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate.s
if [ $? != 0 ]; then DoExitAsm aggregate; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/aggregate.s
echo Assembling recode
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/recode.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/recode.s
if [ $? != 0 ]; then DoExitAsm recode; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/recode.s
echo Assembling interval_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/interval_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/interval_types.s
if [ $? != 0 ]; then DoExitAsm interval_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/interval_types.s
echo Assembling means
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means.s
if [ $? != 0 ]; then DoExitAsm means; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means.s
echo Assembling freq
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq.s
if [ $? != 0 ]; then DoExitAsm freq; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq.s
echo Assembling tables_types
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_types.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_types.s
if [ $? != 0 ]; then DoExitAsm tables_types; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_types.s
echo Assembling tables_stat_rr
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_rr.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_rr.s
if [ $? != 0 ]; then DoExitAsm tables_stat_rr; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_rr.s
echo Assembling tables_stat_or
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_or.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_or.s
if [ $? != 0 ]; then DoExitAsm tables_stat_or; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_or.s
echo Assembling tables_stat_chi2
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_chi2.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_chi2.s
if [ $? != 0 ]; then DoExitAsm tables_stat_chi2; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_chi2.s
echo Assembling tables_stat_fexp
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_fexp.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_fexp.s
if [ $? != 0 ]; then DoExitAsm tables_stat_fexp; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_stat_fexp.s
echo Assembling tables
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables.s
if [ $? != 0 ]; then DoExitAsm tables; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables.s
echo Assembling ctable
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable.s
if [ $? != 0 ]; then DoExitAsm ctable; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable.s
echo Assembling describe
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe.s
if [ $? != 0 ]; then DoExitAsm describe; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe.s
echo Assembling executor
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/executor.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/executor.s
if [ $? != 0 ]; then DoExitAsm executor; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/executor.s
echo Assembling options_utils
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_utils.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_utils.s
if [ $? != 0 ]; then DoExitAsm options_utils; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/options_utils.s
echo Assembling scandate_from_fpc
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scandate_from_fpc.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scandate_from_fpc.s
if [ $? != 0 ]; then DoExitAsm scandate_from_fpc; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scandate_from_fpc.s
echo Assembling epi_script_function_createdate
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createdate.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createdate.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_createdate; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createdate.s
echo Assembling epi_script_function_createtime
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createtime.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createtime.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_createtime; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_createtime.s
echo Assembling epi_script_function_datefunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_datefunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_datefunctions.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_datefunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_datefunctions.s
echo Assembling epi_script_function_timefunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_timefunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_timefunctions.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_timefunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_timefunctions.s
echo Assembling epi_script_function_stringfunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_stringfunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_stringfunctions.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_stringfunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_stringfunctions.s
echo Assembling epi_script_function_systemfunctions
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_systemfunctions.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_systemfunctions.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_systemfunctions; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_systemfunctions.s
echo Assembling epi_script_function_observations
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_observations.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_observations.s
if [ $? != 0 ]; then DoExitAsm epi_script_function_observations; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epi_script_function_observations.s
echo Assembling ast
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast.s
if [ $? != 0 ]; then DoExitAsm ast; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ast.s
echo Assembling stat_dialog_custom_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_custom_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_custom_view.s
if [ $? != 0 ]; then DoExitAsm stat_dialog_custom_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/stat_dialog_custom_view.s
echo Assembling freq_variable_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_model.s
if [ $? != 0 ]; then DoExitAsm freq_variable_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_model.s
echo Assembling freq_variable_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_view.s
if [ $? != 0 ]; then DoExitAsm freq_variable_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_variable_view.s
echo Assembling freq_mainoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_model.s
if [ $? != 0 ]; then DoExitAsm freq_mainoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_model.s
echo Assembling freq_mainoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_view.s
if [ $? != 0 ]; then DoExitAsm freq_mainoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_mainoptions_view.s
echo Assembling freq_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_contribution.s
if [ $? != 0 ]; then DoExitAsm freq_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/freq_contribution.s
echo Assembling tables_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_model.s
if [ $? != 0 ]; then DoExitAsm tables_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_model.s
echo Assembling tables_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm tables_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_model.s
echo Assembling tables_statisticoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_model.s
if [ $? != 0 ]; then DoExitAsm tables_statisticoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_model.s
echo Assembling fields_combobox
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/fields_combobox.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/fields_combobox.s
if [ $? != 0 ]; then DoExitAsm fields_combobox; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/fields_combobox.s
echo Assembling tables_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_variables_view.s
if [ $? != 0 ]; then DoExitAsm tables_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_variables_view.s
echo Assembling tables_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm tables_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_primaryoption_view.s
echo Assembling tables_statisticoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_view.s
if [ $? != 0 ]; then DoExitAsm tables_statisticoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_statisticoptions_view.s
echo Assembling tables_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_contribution.s
if [ $? != 0 ]; then DoExitAsm tables_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/tables_contribution.s
echo Assembling ctable_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_model.s
if [ $? != 0 ]; then DoExitAsm ctable_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_model.s
echo Assembling ctable_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm ctable_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_model.s
echo Assembling ctable_statisticoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_model.s
if [ $? != 0 ]; then DoExitAsm ctable_statisticoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_model.s
echo Assembling ctable_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_variables_view.s
if [ $? != 0 ]; then DoExitAsm ctable_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_variables_view.s
echo Assembling ctable_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm ctable_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_primaryoption_view.s
echo Assembling ctable_statisticoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_view.s
if [ $? != 0 ]; then DoExitAsm ctable_statisticoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_statisticoptions_view.s
echo Assembling ctable_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_contribution.s
if [ $? != 0 ]; then DoExitAsm ctable_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/ctable_contribution.s
echo Assembling means_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_model.s
if [ $? != 0 ]; then DoExitAsm means_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_model.s
echo Assembling means_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm means_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_model.s
echo Assembling means_statisticoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_model.s
if [ $? != 0 ]; then DoExitAsm means_statisticoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_model.s
echo Assembling means_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_variables_view.s
if [ $? != 0 ]; then DoExitAsm means_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_variables_view.s
echo Assembling means_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm means_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_primaryoption_view.s
echo Assembling means_statisticoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_view.s
if [ $? != 0 ]; then DoExitAsm means_statisticoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_statisticoptions_view.s
echo Assembling means_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_contribution.s
if [ $? != 0 ]; then DoExitAsm means_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/means_contribution.s
echo Assembling describe_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_model.s
if [ $? != 0 ]; then DoExitAsm describe_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_model.s
echo Assembling describe_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm describe_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_model.s
echo Assembling describe_statisticoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_model.s
if [ $? != 0 ]; then DoExitAsm describe_statisticoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_model.s
echo Assembling describe_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_variables_view.s
if [ $? != 0 ]; then DoExitAsm describe_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_variables_view.s
echo Assembling describe_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm describe_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_primaryoption_view.s
echo Assembling describe_statisticoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_view.s
if [ $? != 0 ]; then DoExitAsm describe_statisticoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_statisticoptions_view.s
echo Assembling describe_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_contribution.s
if [ $? != 0 ]; then DoExitAsm describe_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/describe_contribution.s
echo Assembling epicurve_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_model.s
if [ $? != 0 ]; then DoExitAsm epicurve_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_model.s
echo Assembling epicurve_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm epicurve_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_model.s
echo Assembling epicurve_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_variables_view.s
if [ $? != 0 ]; then DoExitAsm epicurve_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_variables_view.s
echo Assembling epicurve_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm epicurve_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_primaryoption_view.s
echo Assembling epicurve_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_contribution.s
if [ $? != 0 ]; then DoExitAsm epicurve_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve_contribution.s
echo Assembling histogram_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_model.s
if [ $? != 0 ]; then DoExitAsm histogram_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_model.s
echo Assembling histogram_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm histogram_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_model.s
echo Assembling histogram_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_variables_view.s
if [ $? != 0 ]; then DoExitAsm histogram_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_variables_view.s
echo Assembling histogram_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm histogram_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_primaryoption_view.s
echo Assembling histogram_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_contribution.s
if [ $? != 0 ]; then DoExitAsm histogram_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram_contribution.s
echo Assembling scatter_variables_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_variables_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_variables_model.s
if [ $? != 0 ]; then DoExitAsm scatter_variables_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_variables_model.s
echo Assembling scatter_main_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_main_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_main_view.s
if [ $? != 0 ]; then DoExitAsm scatter_main_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_main_view.s
echo Assembling scatter_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_contribution.s
if [ $? != 0 ]; then DoExitAsm scatter_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter_contribution.s
echo Assembling survival_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_model.s
if [ $? != 0 ]; then DoExitAsm survival_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_model.s
echo Assembling survival_primaryoption_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_model.s
if [ $? != 0 ]; then DoExitAsm survival_primaryoption_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_model.s
echo Assembling survival_statisticoptions_model
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_model.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_model.s
if [ $? != 0 ]; then DoExitAsm survival_statisticoptions_model; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_model.s
echo Assembling survival_variables_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_variables_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_variables_view.s
if [ $? != 0 ]; then DoExitAsm survival_variables_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_variables_view.s
echo Assembling survival_primaryoption_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_view.s
if [ $? != 0 ]; then DoExitAsm survival_primaryoption_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_primaryoption_view.s
echo Assembling survival_statisticoptions_view
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_view.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_view.s
if [ $? != 0 ]; then DoExitAsm survival_statisticoptions_view; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_statisticoptions_view.s
echo Assembling survival_contribution
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_contribution.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_contribution.s
if [ $? != 0 ]; then DoExitAsm survival_contribution; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival_contribution.s
echo Assembling scattersource
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scattersource.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scattersource.s
if [ $? != 0 ]; then DoExitAsm scattersource; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scattersource.s
echo Assembling scatter
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter.s
if [ $? != 0 ]; then DoExitAsm scatter; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/scatter.s
echo Assembling barchart
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/barchart.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/barchart.s
if [ $? != 0 ]; then DoExitAsm barchart; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/barchart.s
echo Assembling survival
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival.s
if [ $? != 0 ]; then DoExitAsm survival; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/survival.s
echo Assembling histogramdata
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramdata.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramdata.s
if [ $? != 0 ]; then DoExitAsm histogramdata; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramdata.s
echo Assembling histogramsource
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramsource.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramsource.s
if [ $? != 0 ]; then DoExitAsm histogramsource; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogramsource.s
echo Assembling epicurve
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.s
if [ $? != 0 ]; then DoExitAsm epicurve; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.s
echo Assembling histogram
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram.s
if [ $? != 0 ]; then DoExitAsm histogram; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/histogram.s
echo Assembling epidataanalysis
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epidataanalysis.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epidataanalysis.s
if [ $? != 0 ]; then DoExitAsm epidataanalysis; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epidataanalysis.s
echo Linking /Users/jamie/ed/analysis/epidataanalysis
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa   -dead_strip -no_dead_strip_inits_and_terms   -multiply_defined suppress -L. -o /Users/jamie/ed/analysis/epidataanalysis `cat /Users/jamie/ed/analysis/link.res` -filelist /Users/jamie/ed/analysis/linkfiles.res
if [ $? != 0 ]; then DoExitLink /Users/jamie/ed/analysis/epidataanalysis; fi
IFS=$OFS
