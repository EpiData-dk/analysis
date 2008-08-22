const
  EpiDataStat: string = '* EpiData Analysis default settings file' + #13 + 'set echo=off' + #13 +
                        'set viewer font name ="Verdana,Arial,Courier,System"' + #13;


  EpiOutCSS: string =
    ' p  {color: black   ;font-size: 1.0em ; font-family: monospace;font-weight: normal; margin: 0em }' + #13 +
    ' h1 {color: blue; font-size: 1.25em; font-family: proportional; font-weight: bold}' + #13 +
    ' h2 {color: blue; font-size: 1.20em; font-family: proportional; font-weight: bold}' + #13 +
    ' h3 {color: blue; font-size: 1.15em; font-family: proportional; font-weight: bold}' + #13 +
    ' h4 {color: blue; font-size: 1.10em; font-family: proportional; font-weight: bold}' + #13 +
    ' h5 {color: blue; font-size: 1.10em; font-family: proportional; font-weight: bold}' + #13 +
    ' .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}' + #13 +
    ' .warning {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
    ' .command {color: black; font-size: 1.0em; font-weight: normal; font-family: monospace ; margin: 0em}' + #13 +
    ' .sysinfo,' + #13 +
    ' .timing {color: blue; font-size: 0.85em; font-weight: normal}' + #13 +
    ' .info {color: green; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
    ' .result {color: green; font-weight: normal}' + #13 +
    ' .error {color: red; font-family: monospace}' + #13 + #13 +
    '/* EpiData Analysis style sheet - basic' + #13 +
    '   Get an updated style sheet from www.epidata.dk' + #13 +
    '   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + #13 +
    '*/';


EpiOutCSSWHITE: string =
'p  {color: white   ;font-size: 1.0em ; font-family: monospace;font-weight: normal; margin: 0em } ' + #13 +
'  h1 {color: #4040ff; font-size: 1.25em; font-family: proportional; font-weight: bold}' + #13 +
'  h2 {color: #4040ff; font-size: 1.20em; font-family: proportional; font-weight: bold}' + #13 +
'  h3 {color: #4040ff; font-size: 1.15em; font-family: proportional; font-weight: bold}' + #13 +
'  h4 {color: #4040ff; font-size: 1.10em; font-family: proportional; font-weight: bold}' + #13 +
'  h5 {color: #4040ff; font-size: 1.1em; font-family: proportional; font-weight: bold}' + #13 +
'  .sysinfo,' + #13 +
'  .timing {color: #4040ff; font-size: 0.85em; font-weight: normal}' + #13 +
'  .info {color: #40ff40; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
'  .error {color: #ff4040; font-family: monospace;}' + #13 +
'  .body {color: yellow; background-color: black; font-size: 1.0em; font-weight: normal}' + #13 +
'  .small {color: white; font-size: 0.85em; font-family: proportional,monospace; font-weight: normal}' + #13 +
'  .ci {color: white; font-size: 0.80em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  .result {color: white; font-weight: normal}' + #13 +
'  .warning {color: yellow; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
'  .command {color: yellow; font-size: 1.0em; font-weight: normal; font-family: monospace ; margin: 0em}' + #13 +
'  .cmd      {color: green; font-weight: normal; }' + #13 +
'  .cmdgroup {color: blue; font-weight: normal; background-color: yellow} ' + #13 +
'  .limit    {font-weight: bold; color: red; } ' + #13 +
'  .indent   {margin-left: 30px;}' + #13 +
'table.box .cell1,' + #13 +
'table.box .cell2,' + #13 +
'table.box .cellpct1,' + #13 +
'table.box .cellpct2,' + #13 +
'table.box .columnpct,' + #13 +
'table.box .columntotal,' + #13 +
'table.box .grandtotal,' + #13 +
'table.box .grandtotalpct,' + #13 +
'table.box .rowtotal1,' + #13 +
'table.box .rowtotal2,' + #13 +
'table.box .rowtotalpct1,' + #13 +
'table.box .rowtotalpct2,' + #13 +
'table.box th,' + #13 +
'table.box tr {}' + #13 +
'table.box td {border: solid 1px white; text-align: right; vertical-align: centre; padding-right: 10px}' + #13 +
'table.box  {color: white; font-size: 1.0em; font-family: proportional; font-weight: normal; border: none; border-spacing: 0;' +
' border-collapse: collapse; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.box .cellfoot {font-size: 0.8em; text-align: left;}' + #13 +
'table.box .caption {font-size: 1.1em; font-weight: bold; border: solid 1px white; }' + #13 +
'table.box .columntotalh,' + #13 +
'table.box .columnvariable,' + #13 +
'table.box .firstcol1,' + #13 +
'table.box .firstcol2,' + #13 +
'table.box .firstrow,' + #13 +
'table.box .firstrowpct,' + #13 +
'table.box .rowtotalh,' + #13 +
'table.box .rowtotalpcth {font-weight: bold; }' + #13 +
'table.filled .cell1,' + #13 +
'table.filled .cell2,' + #13 +
'table.filled .cellpct1,' + #13 +
'table.filled .cellpct2,' + #13 +
'table.filled .columnpct,' + #13 +
'table.filled .columntotal,' + #13 +
'table.filled .grandtotal,' + #13 +
'table.filled .grandtotalpct,' + #13 +
'table.filled .rowtotal1,' + #13 +
'table.filled .rowtotal2,' + #13 +
'table.filled .rowtotalpct1,' + #13 +
'table.filled .rowtotalpct2,' + #13 +
'table.filled th,' + #13 +
'table.filled tr {}' + #13 +
'table.filled  {color: white; background-color: #303030; font-size: 1.0em; font-family: proportional; font-weight: normal;' +
' border: solid 1px white; padding-left: .3em; padding-right: .3em; border-spacing: 0; border-collapse: collapse; ' +
' empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.filled .cellfoot {font-size: 0.8em; border-top: black 1px solid ; text-align: left;}' + #13 +
'table.filled .caption {font-size: 1.1em; font-weight: bold; }' + #13 +
'table.filled .columntotalh,' + #13 +
'table.filled .columnvariable,' + #13 +
'table.filled .firstcol1,' + #13 +
'table.filled .firstcol2,' + #13 +
'table.filled .firstrow,' + #13 +
'table.filled .firstrowpct,' + #13 +
'table.filled .rowtotalh,' + #13 +
'table.filled .rowtotalpcth {font-weight: bold; }' + #13 +
'table.filled td {text-align: left; vertical-align: top; padding-right: 10px}' + #13 +
'table.graph .cell1,' + #13 +
'table.graph .cell2,' + #13 +
'table.graph .cellpct1,' + #13 +
'table.graph .cellpct2,' + #13 +
'table.graph .columnpct,' + #13 +
'table.graph .columntotal,' + #13 +
'table.graph .columntotalh,' + #13 +
'table.graph .columnvariable,' + #13 +
'table.graph .firstcol1,' + #13 +
'table.graph .firstcol2,' + #13 +
'table.graph .firstrow,' + #13 +
'table.graph .firstrowpct,' + #13 +
'table.graph .grandtotal,' + #13 +
'table.graph .grandtotalpct,' + #13 +
'table.graph .rowtotal1,' + #13 +
'table.graph .rowtotal2,' + #13 +
'table.graph .rowtotalh,' + #13 +
'table.graph .rowtotalpct1,' + #13 +
'table.graph .rowtotalpct2,' + #13 +
'table.graph .rowtotalpcth,' + #13 +
'table.graph th,' + #13 +
'table.graph tr {}' + #13 +
'table.graph  {color: white; background-color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; ' +
' border: none; border-spacing: 0; border-collapse: collapse; empty-cells: show; empty-cells: show; vertical-align: top; ' +
' margin: 0; margin-top: 8px;}' + #13 +
'table.graph .cellfoot {font-size: 0.8em; text-align: left;}' + #13 +
'table.graph .caption {font-size: 1.1em; text-align: left; }' + #13 +
'table.graph td {text-align: left; vertical-align: top; }' + #13 +
'table.line .cell1,' + #13 +
'table.line .cell2,' + #13 +
'table.line .columntotal,' + #13 +
'table.line .grandtotal,' + #13 +
'table.line .rowtotal1,' + #13 +
'table.line .rowtotal2 {}' + #13 +
'table.line th,' + #13 +
'table.line tr {border-bottom: double 2px white; padding-left: .7em; padding-right: .7em; }' + #13 +
'table.line  {color: white; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: none; border-right: none;' +
' border-bottom: solid 2px white; border-spacing: 0; margin-top: 8px; }' + #13 +
'table.line .cellfoot {font-size: 0.8em; border-top: solid 1.5px; text-align: left; }' + #13 +
'table.line .caption {font-size: 1.1em; font-weight: bold; border-bottom: solid 1.5px; text-align: center; padding-right: 10px; }' + #13 +
'table.line .columnvariable,' + #13 +
'table.line .firstrow,' + #13 +
'table.line .firstrowpct,' + #13 +
'table.line .rowtotalh,' + #13 +
'table.line .rowtotalpcth {font-weight: bold; border-bottom: solid 1px; }' + #13 +
'table.line .columntotalh,' + #13 +
'table.line .firstcol1,' + #13 +
'table.line .firstcol2 {font-weight: bold; text-align: left; }' + #13 +
'table.line .cellpct1,' + #13 +
'table.line .cellpct2,' + #13 +
'table.line .columnpct,' + #13 +
'table.line .grandtotalpct,' + #13 +
'table.line .rowtotalpct1,' + #13 +
'table.line .rowtotalpct2 {text-align: left; }' + #13 +
'table.line td {text-align: right; vertical-align: top; padding-right: 10px}' + #13 +
'table.shaded .columntotal,' + #13 +
'table.shaded .grandtotal,' + #13 +
'table.shaded th,' + #13 +
'table.shaded tr {}' + #13 +
'table.shaded td {background-color: 404000; text-align: right; vertical-align: centre; padding-right: 10px}' + #13 +
'table.shaded .cell1,' + #13 +
'table.shaded .rowtotal1 {background-color: blue; }' + #13 +
'table.shaded .cell2,' + #13 +
'table.shaded .rowtotal2 {background-color: red; }' + #13 +
'table.shaded  {color: white; background-color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; ' +
' border: none; border-spacing: 0; border-collapse: collapse; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.shaded .cellfoot {font-size: 0.8em; background-color: black; border-top: white 1px solid ; text-align: left; }' + #13 +
'table.shaded .caption {font-size: 1.1em; font-weight: bold; text-align: center; padding-right: 10px; }' + #13 +
'table.shaded .columnpct,' + #13 +
'table.shaded .grandtotalpct {font-style: italic; }' + #13 +
'table.shaded .cellpct1,' + #13 +
'table.shaded .rowtotalpct1 {font-style: italic; background-color: blue; }' + #13 +
'table.shaded .cellpct2,' + #13 +
'table.shaded .rowtotalpct2 {font-style: italic; background-color: red; }' + #13 +
'table.shaded .firstrow,' + #13 +
'table.shaded .firstrowpct,' + #13 +
'table.shaded .rowtotalh,' + #13 +
'table.shaded .rowtotalpcth {font-weight: bold; }' + #13 +
'table.shaded .firstcol1 {font-weight: bold; background-color: blue; text-align: left; }' + #13 +
'table.shaded .firstcol2 {font-weight: bold; background-color: red; text-align: left; }' + #13 +
'table.shaded .columntotalh,' + #13 +
'table.shaded .columnvariable {font-weight: bold; text-align: left; }' + #13 +
'table.system .cell1,' + #13 +
'table.system .cell2,' + #13 +
'table.system .cellpct1,' + #13 +
'table.system .cellpct2,' + #13 +
'table.system .columnpct,' + #13 +
'table.system .columntotal,' + #13 +
'table.system .columntotalh,' + #13 +
'table.system .firstcol1,' + #13 +
'table.system .firstcol2,' + #13 +
'table.system .firstrowpct,' + #13 +
'table.system .grandtotal,' + #13 +
'table.system .grandtotalpct,' + #13 +
'table.system .rowtotal1,' + #13 +
'table.system .rowtotal2,' + #13 +
'table.system .rowtotalpct1,' + #13 +
'table.system .rowtotalpct2,' + #13 +
'table.system .rowtotalpcth,' + #13 +
'table.system th,' + #13 +
'table.system tr {}' + #13 +
'table.system  {color: white; background-color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; ' +
' border: none; border-spacing: 0; border-collapse: collapse; empty-cells: show; empty-cells: show; vertical-align: baseline;' +
' margin: 0; margin-top: 8px;}' + #13 +
'table.system .cellfoot {font-size: 0.8em; text-align: left;}' + #13 +
'table.system .caption {font-size: 1.1em; text-align: left; padding-right: 10px; }' + #13 +
'table.system .columnvariable,' + #13 +
'table.system .firstrow,' + #13 +
'table.system .rowtotalh {font-weight: bold; }' + #13 +
'table.system td {text-align: left; vertical-align: top; padding-right: 10px}' + #13 +
'/* EpiData Analysis style sheet - black background' + #13 +
'   v1.0' + #13 +
'   Use the design table.system as a template for a new design.' + #13 +
'   To be safe, define all styles for a design.' + #13 +
'   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + #13 +
'*/' + #13 ;



 epioutcssblack : string = 'p  {color: black   ;font-size: 1.0em ; font-family: proportional,monospace;font-weight: normal; margin: 0em } ' + #13 +
'  h1 {color: blue; font-size: 1.25em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  h2 {color: blue; font-size: 1.20em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  h3 {color: blue; font-size: 1.15em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  h4 {color: blue; font-size: 1.10em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  h5 {color: blue; font-size: 1.05em; font-family: proportional,monospace; font-weight: bold}' + #13 +
'  .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}' + #13 +
'  .warning {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
'  .small {color: black; font-size: 0.85em; font-family: proportional,monospace; font-weight: normal}' + #13 +
'  .ci {color: black; font-size: 0.80em; font-family: proportional; font-weight: bold}' + #13 +
'  .command {color: black; font-size: 1.0em; font-weight: normal; font-family: monospace ; margin: 0em}' + #13 +
'  .sysinfo,' + #13 +
'  .timing {color: blue; font-size: 0.85em; font-weight: normal}' + #13 +
'  .info {color: green; font-size: 0.85em; font-weight: normal; font-family: monospace}' + #13 +
'  .result {color: green; font-weight: normal}' + #13 +
'  .error {color: red; font-family: monospace}' + #13 +
'  .cmd      {color: green; font-weight: normal; }' + #13 +
'  .cmdgroup {color: blue; font-weight: normal; background-color: yellow} ' + #13 +
'  .limit    {font-weight: bold; color: red; } ' + #13 +
'  .indent   {margin-left: 30px;}' + #13 +
'table.box .cell1,' + #13 +
'table.box .cell2,' + #13 +
'table.box .cellpct1,' + #13 +
'table.box .cellpct2,' + #13 +
'table.box .columnpct,' + #13 +
'table.box .columntotal,' + #13 +
'table.box .grandtotal,' + #13 +
'table.box .grandtotalpct,' + #13 +
'table.box .rowtotal1,' + #13 +
'table.box .rowtotal2,' + #13 +
'table.box .rowtotalpct1,' + #13 +
'table.box .rowtotalpct2,' + #13 +
'table.box th,' + #13 +
'table.box tr {}' + #13 +
'table.box td {border: solid 1px black; text-align: right; vertical-align: centre; padding-right: 10px}' + #13 +
'table.box  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border: none; border-spacing: 0;'+
'  border-collapse: collapse; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.box .cellfoot {font-size: 0.8em; text-align: left}' + #13 +
'table.box .caption {font-size: 1.1em; font-weight: bold; border: solid 1px black; }' + #13 +
'table.box .columntotalh,' + #13 +
'table.box .columnvariable,' + #13 +
'table.box .firstcol1,' + #13 +
'table.box .firstcol2,' + #13 +
'table.box .firstrow,' + #13 +
'table.box .firstrowpct,' + #13 +
'table.box .rowtotalh,' + #13 +
'table.box .rowtotalpcth {font-weight: bold; }' + #13 +
'table.filled .cell1,' + #13 +
'table.filled .cell2,' + #13 +
'table.filled .cellpct1,' + #13 +
'table.filled .cellpct2,' + #13 +
'table.filled .columnpct,' + #13 +
'table.filled .columntotal,' + #13 +
'table.filled .grandtotal,' + #13 +
'table.filled .grandtotalpct,' + #13 +
'table.filled .rowtotal1,' + #13 +
'table.filled .rowtotal2,' + #13 +
'table.filled .rowtotalpct1,' + #13 +
'table.filled .rowtotalpct2,' + #13 +
'table.filled th,' + #13 +
'table.filled tr {}' + #13 +
'table.filled  {color: black; background-color: #d0d0d0; font-size: 1.0em; font-family: proportional; font-weight: normal; ' + 
' border: solid 1px black; padding-left: .3em; padding-right: .3em; border-spacing: 0; border-collapse: collapse; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.filled .cellfoot {font-size: 0.8em; border-top: black 1px solid; text-align: left;}' + #13 +
'table.filled .caption {font-size: 1.1em; font-weight: bold; text-align: center; padding-right: 10px; }' + #13 +
'table.filled .columntotalh,' + #13 +
'table.filled .columnvariable,' + #13 +
'table.filled .firstcol1,' + #13 +
'table.filled .firstcol2,' + #13 +
'table.filled .firstrow,' + #13 +
'table.filled .firstrowpct,' + #13 +
'table.filled .rowtotalh,' + #13 +
'table.filled .rowtotalpcth {font-weight: bold; }' + #13 +
'table.filled td {text-align: left; vertical-align: top; padding-right: 10px}' + #13 +
'table.graph .cell1,' + #13 +
'table.graph .cell2,' + #13 +
'table.graph .cellpct1,' + #13 +
'table.graph .cellpct2,' + #13 +
'table.graph .columnpct,' + #13 +
'table.graph .columntotal,' + #13 +
'table.graph .columntotalh,' + #13 +
'table.graph .columnvariable,' + #13 +
'table.graph .firstcol1,' + #13 +
'table.graph .firstcol2,' + #13 +
'table.graph .firstrow,' + #13 +
'table.graph .firstrowpct,' + #13 +
'table.graph .grandtotal,' + #13 +
'table.graph .grandtotalpct,' + #13 +
'table.graph .rowtotal1,' + #13 +
'table.graph .rowtotal2,' + #13 +
'table.graph .rowtotalh,' + #13 +
'table.graph .rowtotalpct1,' + #13 +
'table.graph .rowtotalpct2,' + #13 +
'table.graph .rowtotalpcth,' + #13 +
'table.graph th,' + #13 +
'table.graph tr {}' + #13 +
'table.graph  {color: black; background-color: white; font-size: 1.0em; font-family: proportional; font-weight: normal; ' +
' border-left: blue 2px solid; border-top: blue 2px solid; border-right: blue 2px solid; border-bottom: blue 2px solid; border-spacing: 0; border-collapse: collapse; empty-cells: show; empty-cells: show; vertical-align: top; margin: 0; margin-top: 8px;}' + #13 +
'table.graph .cellfoot {font-size: 0.8em ; text-align: left;}' + #13 +
'table.graph .caption {font-size: 1.1em; text-align: left; }' + #13 +
'table.graph td {text-align: left; vertical-align: top; }' + #13 +
'table.line .cell1,' + #13 +
'table.line .cell2,' + #13 +
'table.line .columntotal,' + #13 +
'table.line .grandtotal,' + #13 +
'table.line .rowtotal1,' + #13 +
'table.line .rowtotal2 {}' + #13 +
'table.line th,' + #13 +
'table.line tr {border-bottom: double 2px black; padding-left: .7em; padding-right: .7em; }' + #13 +
'table.line  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: none; border-right: none;' +
' border-bottom: solid 2px black; border-spacing: 0; margin-top: 8px; }' + #13 +
'table.line .cellfoot {font-size: 0.8em; border-top: solid 1.5px; text-align: left; }' + #13 +
'table.line .caption {font-size: 1.1em; font-weight: bold; border-bottom: solid 1.5px; text-align: center; padding-right: 10px; }' + #13 +
'table.line .columnvariable,' + #13 +
'table.line .firstrow,' + #13 +
'table.line .firstrowpct,' + #13 +
'table.line .rowtotalh,' + #13 +
'table.line .rowtotalpcth {font-weight: bold; border-bottom: solid 1px; }' + #13 +
'table.line .columntotalh,' + #13 +
'table.line .firstcol1,' + #13 +
'table.line .firstcol2 {font-weight: bold; text-align: left; }' + #13 +
'table.line .cellpct1,' + #13 +
'table.line .cellpct2,' + #13 +
'table.line .columnpct,' + #13 +
'table.line .grandtotalpct,' + #13 +
'table.line .rowtotalpct1,' + #13 +
'table.line .rowtotalpct2 {text-align: left; }' + #13 +
'table.line td {text-align: right; vertical-align: top; padding-right: 10px}' + #13 +
'table.shaded .columntotal,' + #13 +
'table.shaded .grandtotal,' + #13 +
'table.shaded th,' + #13 +
'table.shaded tr {}' + #13 +
'table.shaded td {background-color: #c0c0ff; text-align: right; vertical-align: centre; padding-right: 10px}' + #13 +
'table.shaded .cell1,' + #13 +
'table.shaded .rowtotal1 {background-color: #ffff80; }' + #13 +
'table.shaded .cell2,' + #13 +
'table.shaded .rowtotal2 {background-color: yellow; }' + #13 +
'table.shaded  {color: black; background-color: white; font-size: 1.0em; font-family: proportional; font-weight: normal; border: none; ' +
' border-spacing: 0; border-collapse: collapse; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.shaded .cellfoot {font-size: 0.8em; background-color:  white; border-top: black 1px solid; text-align: left; }' + #13 +
'table.shaded .caption {font-size: 1.1em; font-weight: bold; text-align: center; padding-right: 10px; }' + #13 +
'table.shaded .columnpct,' + #13 +
'table.shaded .grandtotalpct {font-style: italic; }' + #13 +
'table.shaded .cellpct1,' + #13 +
'table.shaded .rowtotalpct1 {font-style: italic; background-color: #ffff80; }' + #13 +
'table.shaded .cellpct2,' + #13 +
'table.shaded .rowtotalpct2 {font-style: italic; background-color: yellow; }' + #13 +
'table.shaded .firstrow,' + #13 +
'table.shaded .firstrowpct,' + #13 +
'table.shaded .rowtotalh,' + #13 +
'table.shaded .rowtotalpcth {font-weight: bold; }' + #13 +
'table.shaded .firstcol1 {font-weight: bold; background-color: #ffff80; text-align: left; }' + #13 +
'table.shaded .firstcol2 {font-weight: bold; background-color: yellow; text-align: left; }' + #13 +
'table.shaded .columntotalh,' + #13 +
'table.shaded .columnvariable {font-weight: bold; text-align: left; }' + #13 +
'table.system .cell1,' + #13 +
'table.system .cell2,' + #13 +
'table.system .cellpct1,' + #13 +
'table.system .cellpct2,' + #13 +
'table.system .columnpct,' + #13 +
'table.system .columntotal,' + #13 +
'table.system .columntotalh,' + #13 +
'table.system .firstcol1,' + #13 +
'table.system .firstcol2,' + #13 +
'table.system .firstrowpct,' + #13 +
'table.system .grandtotal,' + #13 +
'table.system .grandtotalpct,' + #13 +
'table.system .rowtotal1,' + #13 +
'table.system .rowtotal2,' + #13 +
'table.system .rowtotalpct1,' + #13 +
'table.system .rowtotalpct2,' + #13 +
'table.system .rowtotalpcth,' + #13 +
'table.system th,' + #13 +
'table.system tr {}' + #13 +
'table.system  {color: black; background-color: white; font-size: 1.0em; font-family: proportional; font-weight: normal; ' +
' border: none; border-spacing: 0; border-collapse: collapse; empty-cells: show; empty-cells: show; vertical-align: baseline; margin: 0; margin-top: 8px;}' + #13 +
'table.system .cellfoot {font-size: 0.8em; text-align: left; }' + #13 +
'table.system .caption {font-size: 1.1em; text-align: left; padding-right: 10px; }' + #13 +
'table.system .columnvariable,' + #13 +
'table.system .firstrow,' + #13 +
'table.system .rowtotalh {font-weight: bold; }' + #13 +
'table.system td {text-align: left; vertical-align: top; padding-right: 10px}' + #13 +
'' + #13 +
'/* EpiData Analysis style sheet - white background' + #13 +
'   v1.0' + #13 +
'   Use the design table.system as a template for a new design.' + #13 +
'   To be safe, define all styles for a design.' + #13 +
'   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + #13 +
'*/' + #13 ;


StartHTM: string =  '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">' + #13 +
    '<head>' + #13 +
    '<meta name="copyright" content="EpiData Association">' + #13 +
    '<meta name="description" content="Get latest version from: www.epidata.dk">' + #13 +
    '<title>EpiData Analysis</title>' + #13 +
    '<link href="epiout.css" rel="stylesheet" type="text/css" media="screen">' + #13 +
    '</HEAD>' + #13 +
    '<body class=body>' + #13 +
    '<h3 align="left">EpiData Analysis<font style="Font-size=0.9em" >' + #13 +
    'Copyright 2002-2008  Epidata Association <A HREF="HTTP:\\www.epidata.dk">www.epidata.dk</A></font></h3>' + #13;


 StartFontHtm1: string =     '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">' + #13 +
'<head>' + #13 +
'<meta name="copyright" content="EpiData Association">' + #13 +
'<meta name="description" content="Setup for EpiData Analysis">' + #13 +
'<LINK REL="stylesheet" TYPE="text/css" HREF="epiout.css">' + #13 +
'<TITLE>EpiData Analysis</TITLE>' + #13 +
'</HEAD>' + #13 +
'<body class=body>' + #13 ;

 font10: string =  '  <A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir";cd "@fn";copyfile "epidatastat.ini" "epidatastat.old" /replace;copyfile "epidatastat.10" "epidatastat.ini" /replace;'+'set viewer font size=10; set window font size=10; set editor font size= 10; set echo=on;' ;
 font12: string =  '  <A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir";cd "@fn";copyfile "epidatastat.ini" "epidatastat.old" /replace;copyfile "epidatastat.12" "epidatastat.ini" /replace;'+'set viewer font size=12;  set window font size=12; set editor font size= 12; set echo=on;';
 font15: string =  '  <A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir";cd "@fn";copyfile "epidatastat.ini" "epidatastat.old" /replace;copyfile "epidatastat.15" "epidatastat.ini" /replace;'+'set viewer font size=15;  set window font size=15; set editor font size= 15; set echo=on;';
 font20: string =  '  <A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir";cd "@fn";copyfile "epidatastat.ini" "epidatastat.old" /replace;copyfile "epidatastat.20" "epidatastat.ini" /replace;'+'set viewer font size=20;  set window font size=20; set editor font size= 20; set echo=on;';
 final : string = '<A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir/docs/en/";cd "@fn";copyfile "start0.htm" "start.htm" /replace;clh;cd "@sysdir";closehelp;set style sheet="@sysdir/epiout.css";cls;set echo=on;';


