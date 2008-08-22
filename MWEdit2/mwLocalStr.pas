{+-----------------------------------------------------------------------------+
 | Unit:        mwLocalStr
 | Created:     1999-08
 | Version:     0.90
 | Last change: 1999-11-18
 | Description: put all strings that might need localisation into this unit
 +----------------------------------------------------------------------------+}

unit mwLocalStr;

{$I MWEDIT.INC}

interface

// NOTE: this is design-time stuff, so no need to have it in stringtables
const
  MWS_ComponentsPage           =  'EpiData';
  MWS_HighlightersPage         =  'EpiData';

{$IFDEF MWE_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}

  // names for highlighter attributes - general
  MWS_AttrAssembler            =  'Assembler';
  MWS_AttrAsm                  =  'Asm';
  MWS_AttrComment              =  'Comment';
  MWS_AttrIdentifier           =  'Identifier';
  MWS_AttrKey                  =  'Key';
  MWS_AttrNumber               =  'Number';
  MWS_AttrOperator             =  'Operator';
  MWS_AttrPreprocessor         =  'Preprocessor';
  MWS_AttrReservedWord         =  'Reserved word';
  MWS_AttrSpace                =  'Space';
  MWS_AttrSymbol               =  'Symbol';
  MWS_AttrString               =  'String';
  MWS_AttrText                 =  'Text';
  MWS_AttrVariable             =  'Variable';

  // names for highlighter attributes - special
  MWS_AttrAsmComment           =  'Asm comment';
  MWS_AttrAsmKey               =  'Asm key';
  MWS_AttrASP                  =  'Asp';
  MWS_AttrBlock                =  'Block';
  MWS_AttrBrackets             =  'Brackets';
  MWS_AttrCharacter            =  'Character';
  MWS_AttrDocumentation        =  'Documentation';
  MWS_AttrEscapeAmpersand      =  'Escape ampersand';
  MWS_AttrForm                 =  'Form';
  MWS_AttrFunction             =  'Function';
  MWS_AttrIcon                 =  'Icon reference';
  MWS_AttrIllegalChar          =  'Illegal char';
  MWS_AttrInvalidSymbol        =  'Invalid symbol';
  MWS_AttrInternalFunction     =  'Internal function';
  MWS_AttrMessage              =  'Message';
  MWS_AttrMiscellaneous        =  'Miscellaneous';
  MWS_AttrNull                 =  'Null';
  MWS_AttrPragma               =  'Pragma';
  MWS_AttrQualifier            =  'Qualifier';
  MWS_AttrRpl                  =  'Rpl';
  MWS_AttrRplKey               =  'Rpl key';
  MWS_AttrRplComment           =  'Rpl comment';
  MWS_AttrSASM                 =  'SASM';
  MWS_AttrSASMComment          =  'SASM Comment';
  MWS_AttrSASMKey              =  'SASM Key';
  MWS_AttrSecondReservedWord   =  'Second reserved word';
  MWS_AttrSection              =  'Section';
  MWS_AttrSpecialVariable      =  'Special variable';
  MWS_AttrSyntaxError          =  'SyntaxError';
  MWS_AttrSystem               =  'System functions and variables';
  MWS_AttrSystemValue          =  'System value';
  MWS_AttrUnknownWord          =  'Unknown word';
  MWS_AttrUser                 =  'User functions and variables';
  MWS_AttrValue                =  'Value';

  // mwCustomEdit scroll hint window caption
  MWS_ScrollInfoFmt            =  'TopLine: %d';

  // strings for property editors etc
  MWS_EDuplicateShortcut       =  'Shortcut already exists';
  MWS_ShortcutNone             =  '<none>';
  MWS_DuplicateShortcutMsg     =  'The keystroke "%s" is already assigned to ' +
                                  'another editor command.';
  MWS_DuplicateShortcutMsg2    =  'The keystroke "%s" is already assigned to ' +
                                  'another editor command.'#13#10'The short' +
                                  'cut for this item has not been changed.';

  // Filters used for open/save dialog
  MWS_FilterPascal             =  'Pascal files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc';
  MWS_FilterHP48               =  'HP48 files (*.s,*.sou,*.a,*.hp)|*.s;*.sou;*.a;*.hp';
  MWS_FilterCAClipper          =  'CA-Clipper files (*.prg, *.ch, *.inc)|*.prg;*.ch;*.inc';
  MWS_FilterCPP                =  'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp';
  MWS_FilterJava               =  'Java files (*.java)|*.java';
  MWS_FilterPerl               =  'Perl files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi';
  MWS_FilterAWK                =  'AWK Script (*.awk)|*.awk';
  MWS_FilterHTML               =  'HTML Document (*.htm,*.html)|*.htm;*.html';
  MWS_FilterVBScript           =  'VBScript files (*.vbs)|*.vbs';
  MWS_FilterGalaxy             =  'Galaxy files (*.gtv,*.galrep,*.txt)|*.gtv;*.galrep;*.txt';
  MWS_FilterPython             =  'Python files (*.py)|*.py';
  MWS_FilterSQL                =  'SQL files (*.sql)|*.sql';
  MWS_FilterHP                 =  'HP48 files (*.s,*.sou,*.a,*.hp)|*.S;*.SOU;*.A;*.HP';
  MWS_FilterTclTk              =  'Tcl/Tk files (*.tcl)|*.tcl';
  MWS_FilterRTF                =  'Rich Text Format (*.rtf)|*.rtf';
  MWS_FilterBatch              =  'MS-DOS Batch Files (*.bat)|*.bat';
  MWS_FilterDFM                =  'Delphi/C++ Builder Form Files (*.dfm)|*.dfm';
  MWS_FilterX86Asm             =  'x86 Assembly Files (*.asm)|*.ASM';
  MWS_FilterGembase            =  'GEMBASE files (*.dml,*.gem)|*.DML;*.GEM';
  MWS_FilterINI                =  'INI Files (*.ini)|*.ini';
  MWS_FilterML                 =  'Standard ML Files (*.sml)|*.sml';

  // Language names. Maybe somebody wants them translated / more detailed...
  MWS_LangHP48                 =  'HP48';
  MWS_LangCAClipper            =  'CA-Clipper';
  MWS_LangCPP                  =  'C++';
  MWS_LangJava                 =  'Java';
  MWS_LangPerl                 =  'Perl';
  MWS_LangBatch                =  'MS-DOS Batch Language';
  MWS_LangDfm                  =  'Delphi/C++ Builder Form Definitions';
  MWS_LangAWK                  =  'AWK Script';
  MWS_LangHTML                 =  'HTML Document';
  MWS_LangVBSScript            =  'MS VBScript';
  MWS_LangGalaxy               =  'Galaxy';
  MWS_LangGeneral              =  'General';
  MWS_LangPascal               =  'ObjectPascal';
  MWS_LangX86Asm               =  'x86 Assembly Language';
  MWS_LangPython               =  'Python';
  MWS_LangTclTk                =  'Tcl/Tk';
  MWS_LangSQL                  =  'SQL';
  MWS_LangGembase              =  'Gembase';
  MWS_LangINI                  =  'INI files';
  MWS_LangML                   =  'Standard ML';

  // Exporter names.
  MWS_ExportHTML               =  'HTML';
  MWS_ExportRTF                =  'RTF';

implementation

end.
