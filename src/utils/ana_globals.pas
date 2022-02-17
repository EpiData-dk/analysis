unit ana_globals;

{$mode objfpc}{$H+}

interface

uses
 sysutils;

const
  // Set Options Globals

  ANA_SO_BROWSER_FONT_SIZE             = 'BROWSER FONT SIZE';
  ANA_SO_BROWSER_FONT_NAME             = 'BROWSER FONT NAME';
  ANA_SO_BROWSER_FONT_COLOR            = 'BROWSER FONT COLOUR';
  ANA_SO_BROWSER_FONT_STYLE            = 'BROWSER FONT STYLE';
  ANA_SO_BROWSER_BG_COLOR              = 'BROWSER BG COLOUR';
  ANA_SO_BROWSER_OBS_DEFAULT_COLOR     = 'BROWSER OBS DEFAULT COLOUR';
  ANA_SO_BROWSER_OBS_DELETED_COLOR     = 'BROWSER OBS DELETED COLOUR';
  ANA_SO_BROWSER_OBS_VERIFIED_COLOR    = 'BROWSER OBS VERIFIED COLOUR';
  ANA_SO_CLIPBOARD_DELIMITER           = 'CSV DELIMITER';

  ANA_SO_CMDEDIT_FONT_SIZE             = 'COMMANDLINE FONT SIZE';
  ANA_SO_CMDEDIT_FONT_NAME             = 'COMMANDLINE FONT NAME';
  ANA_SO_CMDEDIT_FONT_COLOR            = 'COMMANDLINE FONT COLOUR';
  ANA_SO_CMDEDIT_FONT_STYLE            = 'COMMANDLINE FONT STYLE';
  ANA_SO_CMDEDIT_BG_COLOR              = 'COMMANDLINE BG COLOUR';

  ANA_SO_COMMANDLOG                    = 'COMMANDLOG';
  ANA_SO_COMMANDLOGFILE                = 'COMMANDLOGFILE';
  ANA_SO_COMMANDLOGLINES               = 'COMMANDLOGLINES';

  ANA_SO_DISPAY_HISTORY                = 'DISPLAY HISTORY WINDOW';
  ANA_SO_DISPAY_VARIABLE               = 'DISPLAY VARIABLE WINDOW';
  ANA_SO_DISPAY_DATASET                = 'DISPLAY DATASET WINDOW';
  ANA_SO_DISPAY_COMMANDTREE            = 'DISPLAY COMMANDTREE WINDOW';

  ANA_SO_ECHO                          = 'ECHO';
  ANA_SO_EDITOR_FONT_SIZE              = 'EDITOR FONT SIZE';
  ANA_SO_EDITOR_FONT_NAME              = 'EDITOR FONT NAME';
  ANA_SO_EDITOR_HISTORY                = 'EDITOR HISTORY';
  ANA_SO_EXITSAVE                      = 'EXITSAVE';


  ANA_SO_STATISTICS_VALUE_LABEL        = 'STATISTICS VALUE LABEL';
  ANA_SO_STATISTICS_VARIABLE_LABEL     = 'STATISTICS VARIABLE LABEL';
  ANA_SO_BROWSE_VALUE_LABEL            = 'BROWSER VALUE LABEL';
  ANA_SO_BROWSE_VARIABLE_LABEL         = 'BROWSER VARIABLE LABEL';

  ANA_SO_INCLUDE_DELETED               = 'INCLUDE DELETED';

  ANA_SO_OUTPUT_FONT_SIZE              = 'OUTPUT FONT SIZE';
  ANA_SO_OUTPUT_FONT_NAME              = 'OUTPUT FONT NAME';
  ANA_SO_OUTPUT_FONT_COLOR             = 'OUTPUT FONT COLOUR';
  ANA_SO_OUTPUT_FONT_STYLE             = 'OUTPUT FONT STYLE';
  ANA_SO_OUTPUT_BG_COLOR               = 'OUTPUT BG COLOUR';
  ANA_SO_OUTPUT_FORMAT                 = 'OUTPUT FORMAT';
  ANA_SO_OUTPUT_SAVE_FORMAT            = 'OUTPUT SAVE FORMAT';
  ANA_SO_OUTPUT_CSS_FILE               = 'OUTPUT CSS FILE';
  ANA_SO_OUTPUT_CSS_INTERNAL           = 'OUTPUT CSS INTERNAL';

  ANA_SO_SHORT_MONTH_NAMES             = 'SHORT MONTH NAMES';

  ANA_SO_SHOW_COMMAND                  = 'SHOW COMMAND';
  ANA_SO_SHOW_DEBUG                    = 'SHOW DEBUG';
  ANA_SO_SHOW_ERROR                    = 'SHOW ERROR';
  ANA_SO_SHOW_INFO                     = 'SHOW INFO';
  ANA_SO_SHOW_WARNING                  = 'SHOW WARNING';

  ANA_SO_CONFIDENCE_INTERVAL           = 'CONFIDENCE INTERVAL';

  ANA_SO_TABLE_PERCENT_FORMAT_COL      = 'TABLE PERCENT FORMAT COL';
  ANA_SO_TABLE_PERCENT_FORMAT_ROW      = 'TABLE PERCENT FORMAT ROW';
  ANA_SO_TABLE_PERCENT_FORMAT_TOTAL    = 'TABLE PERCENT FORMAT TOTAL';
  ANA_SO_TABLE_PERCENT_HEADER          = 'TABLE PERCENT HEADER';

  ANA_SO_TUTORIAL_FOLDER               = 'TUTORIAL FOLDER';
  ANA_SO_WEB_URL                       = 'WEB URL';

  //  Executor Field Names
  ANA_EXEC_PREPAREDS_OBSNO_FIELD       = '@obsno';


implementation

end.
