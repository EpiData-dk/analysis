# EpiData Analysis

### Command and Function Reference Guide (version 3.3)

___

###Commands

| Manage data | Analyze data | Graph data | Write programs
:---|:---|:---|:---
[read](#read) or [save](#save) a project<br/>[append](#append) or [merge](#merge) data<br/>[aggregate](#aggregate) data<br/>[use](#use) datasets<br/>create [new](#new) content<br/>[list](#list) or [browse](#browse) content<br/>[edit](#edit) content<br/>[:=](#assign) to assign values to a variable<br/>[recode](#recode) a variable<br/>[reorder](#reorder) variables<br/>[delete](#drop) content<br/>Consistency and validity [check](#check)<br/>[report](#report)<br/> | [sort](#sort) records<br/>[select](#select) records<br/>[count](#count) records<br/>[describe](#describe) variables<br/>[freq](#freq)uencies<br/>[tables](#tables)<br/>[ctable](#ctable) with many variables<br/>[means](#means)<br/>[regress](#regress)ion analysis<br/>[survival](#survival) analysis | [scatter](#scatter) plot <br/>[line](#scatter) plot <br/>frequency [bar](#barchart) chart <br/>[histogram](#histogram)<br/>[epicurve](#epicurve)<br/>[Kaplan-Meier plot](#survival)<br/>*SPC Charts*<br/>[pareto chart](#pareto)<br/>[Options](#graphoptions) used in all graphs| [if-then](#if-then)<br/>[set](#set) parameters <br/>[Labels, Values and format in output](#commonoptions)<br/>[Types of Variables](#variabletype)<br/>[Variable lists](#variablelist)<br/>[Referenced variables](#referencedvars)<br/>[run](#run) scripts <br/>[Clean up & stop](#stop)<br/>[Functions](#functions)<br/>[Operators](#operators)<br/>[Startup options](#startup)

Some commands are currently only available in EpiData Analysis Classic. [Download EpiData Classic here](http://epidata.dk/download.php#ea)

*   SPC graphs - Ichart etc.

### Syntax for all commands

```
command <variables | expression> [!option] [!option := a|b]
```

In command descriptions, the following notation is used

```
  [ ] : optional specification of observation number.    
  {a|b|...} : indicates alternative choices  
  <...> : indicates user specified name/identifier  
```

If you are in doubt of when to use double quotes "" and when not, the rule is:  

* Use "..." for all external references (e.g. "file names.ext") or assignments of text values ( e.g. `set "COMMANDLOG" := "ON"`)  
* Double quotes are NOT needed for variables, defined value labels, or dataset names

# Disk commands

<a name="cd"></a>
## cd


```
cd ["<directory path>"]
```
Change the working directory (folder) to the specified path.
        If no path is given a dialog is shown to select the working directory.

<a name="dir"></a><a name="ls"></a>
## ls / dir

```
ls ["<directory path>" | "file name"]
dir ["<directory path" | "file name"]
```

List files in a directory

### parameters
- *directory path* or *file name* may include wild cards (* or ?)
        If no path is given, the working directory is assumed

<a name="erase"></a>
## erase

```
erase "<file name>"
```
Delete the file from disk.

### parameters
- <file name> may use wildcards (* or ?), but the directory name should not as this may or may not be allowed by the operating system
- If no path is given, the current working directory is used.

> Warning: The file is deleted (if the file exist) with no confirmatory question

# Read and Save Data
<a name=read></a>
## read

```
read [{"<filename>" | <expression>}] [!options ...]
```

Read a copy of the data file into memory.

> Note: The file name, including the file extension must be contained in " ".    
If you use a case sensitive operating system (Linux) then the case of filename and extension is important.  

### parameters

- `filename`

 An optional _filename_ may be given in quotes. The file format is detected based on the file extension, which must be one of {_csv_ | _epx_ | _epz_ | _dta_}. If no _filename_ is given, the open file dialog is started.

 All ***EpiData*** products support reading Stata files from version 4 to 16 (format 105 to 118). Stata has a special .dta version (format 119) which is only for datasets with more than 32767 variables. This format may also be used in Stata 15 and 16, but is not the standard format. This format is not supported by ***EpiData***.

 [See this link for at technical description of the .dta formats](https://www.stata.com/help.cgi?dta)

- `expression`

 An optional _expression_ (instead of <_filename_>) that resolves to a string specifying the filename. (see examples)

### options

- `!force`

 Will force reading a locked epx/epz file.
> Note: Use with caution since the project may be used by someone else

- `!c`

 Close the current project. It is only required when project (data, labels, variables, valuelabels, etc.) has changed.

- `!d := "<_single character_>"`

 Use the character as the delimiter used when reading .txt/.csv files. This option is only valid when importing delimited files. During import the delimiter will be validated. If the structure of the file does not fit with the selected delimiter, it will be rejected.

 If no delimiter is set for importing delimited files, the program will guess which delimiter is used.

- `!q := "<_single character_>"`

 Use the character for recognizing quoted strings when reading .txt/.csv files. This option is only valid when importing delimited files. If character is set to empty/nothing (e.g. !q := "") then the complete content of the file is used in guessing/validating delimiters.
If no quote characters is set, the default " (double quote) is used to identify strings.

- `!vn := {true|false}`

 Read the first line in a .txt/.csv file as either variable names or as data.

 - true: read the first line as variable names, with no regards to the actual content.
 - false: read the first line as data, with no regards to the actual content.

 If the option is not used when reading delimited files, the program will look at the line and guess if it is data or headers.

> Note: In files with all string data and where first line is known to be variable names, the program has a high probability of guessing incorrectly whether first line is headers or not. In such cases please use this option to ensure correct reading of data.

- `!pw := "<string>"`

 If you use this option with a password protected file (.epx | .epz | .rec), reading will occur directly, otherwise you will be prompted for a password.

 When an EpiData project (.epx or .epz) has Extended Access enabled, you will be prompted to log in with a password and a username. With this option, the file is opened with the supplied login. A password for that user may be supplied with option

- `!login := "<string>"`

### examples

```
read "bromar.rec";      // complete filename provided
read "bromar" + ".epx"; // expression using two strings in concatenation
new global fn string;
fn := "bromar.epx";
read fn;                // expression using the variable fn
```

<a name=save></a>
## save

```
save [{"<filename>" | <expression>}] [options]
```
Save a copy of all variables in memory to a file, to use the data again

### parameters

*   `filename`

 An optional filename may be given in quotes. The file format is detected based on the file extension which must be one of {csv|epx|epz|dta}. If no filename is given the save file dialog is started.

*   `expression`

 An optional expression (instead of *filename*) that resolves to a string with the filename. This can be e.g. a concatenation of strings, a global variable or something else. See [read](read) for examples.

### options

*   `!replace`

 Overwrite existing file

*   `!force`

 Will force overwriting a locked .epx/.epz file
 > Note: Use with caution since the project may be used by someone else

*   `!output [:= "html | text"\]`

 Instead of saving to the project, this options saves the output to a file. If no format is specified (text/html) then the current output format is used.

*   `!format := "{stata|epidata|csv}"`

 Save the project as a specific type.
 > This will ONLY change the content. The user must make sure the file has the correct extension.

*   `!d := "<single character>"`

 Will force the delimiter of values/names used when writing .txt/.csv files. This option is only valid when saving delimited files. The default value is "," (comma)

*   `!q := "<single character>"`

 Will force the character used for writing quoted strings. This option is only valid when saving delimited files. The default value is " (double quotation mark)

*   `!version := <integer>`

 Specify which stata version to save the data as. Accept values range from 4 -> 14, default is 14

*   `!vn := {true|false}`

 If true the first line in the .csv/.txt file is the variable names. Default value is "true"

*   `!dated := "<single character>"`

 Will force the character used for writing date delimiters. Default value is "/" (slash)

*   `!timed := "<single character>"`

 Will force the character used for writing time delimiters. Default value is ":" (colon)

*   `!decd:= "<single character>"`

 Will force the character used for writing decimal seperators. Default value is "." (dot)

*   `!nl := "<single character>"`

 Will force the end of line character used in the .txt/.csv file. Default value is that of the operating system:  
    Linux: #10 MacOS: #13 Windows: #13#10

*   `!memonl := "<single character>"`

 Will force the character used for writing the linebreaks from a memo variable. Default value is " " (space)

*   `!fixed`

 Forces the writing of the .csv/.txt in a fixed format, where the length of each variable is used to specify the length of each column.
 >Note: Using !fixed ignores the use of the option !d

*   `!bom`  

 This options adds the [UTF-8 Byte Order Mark](https://en.wikipedia.org/wiki/Byte_order_mark) to the .csv/.txt file.

# Combine and create data      

<a name="append"></a>
## append  

```
append [<var1> <var2>...] [!ds := <dataset>] [!fn := "<filename>"]
```

Add observations after all observations in current file

### options

- `!fn := "<filename>"`

  Append this file. Without !fn the file open dialog is shown. All known file types may be used and all options from [read](#read) associated with reading of data may be used. e.g. !d or !h options for reading csv files.

- `!ds := <dataset>`

  Specifies which dataset to use from the external file. It is only needed if the external file contains multiple datasets.

  Only fields with same name as variables in memory will be read. Variables from previous `read` which are not in the appended file will be set to missing for the appended observations.

See [variables](#referencedvars) on using referenced variables for this command

<a name="merge"></a>
## merge  

```
merge [<key1> <key2> ...]
      [!fn [:= "<filename>"]] [!ds := <dataset>]
      [!table] [!combine> !update> !replace]
```

Merge the current data file with another dataset file based on *key* variable(s). The result is a **NEW** dataset which is added to the top level of the project.

### options

- `!fn [:= "<filename>"]`

  Merge with a dataset in this file. Without !fn it is assumed that current used dataset should be merge with a related dataset. If no <filename> is provided then a dialog is shown to open the external file. All known file types may be used and all options from [read](#read) associated with reading of data can be used too. e.g. !d or !h options for reading csv files.

- `!ds := <dataset>`

  Specifies which dataset to use. It is only needed if the multiple datasets exist (either as related datasets or in an external file).

- `!label := "<text>"`

- `!l := "<text>"`

  Assign the descriptive text as a caption/label for the resulting dataset.

- `!table`

  The external file is used as a lookup table. e.g. to add person information to a file with clinical results

- `!combine`

  All non-missing values in the external dataset replaces all MISSING values for common variables

- `!update`

  All non-missing values in the external dataset replaces ALL values for common variables

- `!replace`

  All values in the external dataset replace all values for common variables

- `!nu`

  By default the resulting data is automatically used. By using this option the program will stay on the current dataset after the command has completed.

- `!r := "new dataset name"`

 Specify a different name for the resulting dataset. Default naming is a concatenation of the two used datasets

 To keep information in variables with an identical name (e.g. mergevar or name) from all files [rename](#edit) the variables **before** you merge the next file. e.g.

```            
read "pt.epx";
edit variable name !r := "ptname";
merge hospid !filename := "hospital.epx" !r := MergeDs;
use MergeDs;
edit variable name !r := "hospname";
etc ....
```

  After  `merge` the variable **mergevar** indicates source of information for each observation (records). **mergevar** is defined with variable labels for these values:
  
```
1 = In main dataset only
2 = In merged dataset only
3 = In both datasets
```

> Note: if *mergevar* already exists in the dataset, an error will occur and the merge will be stopped. Drop the *mergevar* variable first.

### example

```
// Load a project
read "Clinical Example.epx";

// A: Internal merge (1 related dataset)
// Since the current used dataset only has a single related dataset,
// there is enough information provided by the key variable to combine to two datasets.
merge;

// B: Internal merge (2+ related datasets)
// The currently used dataset have 2 or more related datasets, so we need to use the
// option !ds := <id> to specify which dataset we want to merge with.
// Use  list to see which dataset id's you have in the project
list ds        
merge !ds := labdata

// C: External merge (1+ dataset in external file - e.g. a .csv file)
// In order to merge with an external file the key variables to merge on must be provided
merge patientid !ds := "PatientNames.csv":labdata

// D: External merge (2+ datasets in external file)
// In order to merge with an external file that has 2+ dataset, both the key variables
// AND the dataset you wish to merge with must be provided!
merge patientid !ds := firstdataset !filename := "PatientNames.epx"
```       
See [variables](#referencedvars) on using referenced variables for this command

<a name="aggregate"></a><a name="agg"></a>
## aggregate

```
aggregate [<var1> <var2>...] [!options]
```

Aggregate - collapse - combine - data when you wish to change from individual to group level.

See [variables](#referencedvars) on using referenced variables for this command

### options

- `!m`

 Include observations with missing data (.)

- `!q`

 Hides all output

- `!nc

`Do not show counts of observations for each variable

- `!nt`

 Do not show total counts of observations

- `!ds := <dataset>`

 Save the aggregated dataset as it's own dataset with given name

- `!replace`

 use in combination with  !ds to replace an existing dataset

- `!caption := "<string>"`

 give the dataset a caption (used both in output and with !ds)

- `!hd := <global string vector>`

 give a custom label for the generated variables and show these as column descriptors in table head

- `!u`

 use in combination with !ds. Use the generated dataset after this command completes. Cannot be used with an active select!

- `!full`

 Expands the resulting dataset with ALL possible value-combinations from <var1> <var2>... All entries with no data will contain system missing.

See [Common options](#commonoptions) for options for labels and formats.

See [formatting](#formatting) for options on formatting percentages

### Summary statistic options

Each of these options must be followed by `:= variable`. Only one variable can be given, but each option can be used multiple times with different variables.

- `!mv` Count of missing values (system and user defined)

- `!mean` Mean of the variable

- `!sd` Standard deviation

- `!sv` Variance

- `!min` Minimum value

- `!med` Median value

- `!max` Maximum value

- `!pXX` XX percentile (XX = 1, 5, 10,25, 50, 75, 90, 95, 99)

- `!sum` Sum of values

- `!des` Min, Median and Max

- `!iqr` p25 and p75

- `!idr` p10 and p90

- `!isr` p5 and p95

- `!mci` Mean and 95% CI (low + high)

### example

 ```
// define a global vectore for column texts:
new global columntxt[5] string;
columntxt[1] := "Group";
columntxt[2] := "N Total";
columntxt[3] := "n (observed)";
columntxt[4] := "Mean";
columntxt[5] := .;
agg sex age family !hd:=columntxt  !mci:=economy !mci:=children ;
 ```       

<a name="use"></a>
## use

```
use <dataset>
```

Change the active dataset of a project.
### parameters

- `dataset`

  The name of a dataset within the current project, without quotes

See [variables](#referencedvars) on using referenced variables for this command

### example

```
read "Clinical Example.epx";
list dataset;
use datafile_id_2;
```      

<a name="new"></a>
## new project
</a>
```
new project [options]
new p [options]
```

Creates a new empty project, e.g. for simulation or testing.

### options            

- `!size:= <integer>`

 Adds an initial dataform with <size> observations in it. If omitted, no dataform is created and must be created manually.

- `!label:= "<text>"`

 Adds a title to the project. If not used a default title is given.

- `!c`

 Closes any open project if modified

- `!pw := <string>`

 Encrypts the data of the project with a single password. All data is encrypted with the [AES/Rijndael algorithm](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard), but metadata is not encrypted.

- `!showFieldNames := <boolean>`<br/>
  `!sfn := <boolean>`

 Show/Hide the field name next to the entry field in Manager and EntryClient

- `!showFieldBorders := <boolean>`<br/>
  `!sfb := <boolean>`

 Show/Hide the border of the entry field in Manager and EntryClient

- `!backupInterval := <integer>`<br/>
   `!bi := <integer>`

 Interval at witch Manager and EntryClient automatically saved the project

- `!backupOnShutdown := <boolean>`<br/>
  `!bos := <boolean>`

 Perform a backup when closing the project. The name for the backup is based on the current date/time.

## new dataset
```
new dataset <datasetname> [!options...]
```

Create a new dataset for the project. Use the options to specify relations between datasets.  If the command completes successfully, the newly created dataset is automatically [used](#use)

### parameters
- `datasetname`

  The name of the dataset <b>not enclosed in quotes</b>

### options            

- `!parent := "<parentform id>"`

 Used for creating parent-child relations. If omitted the dataform is created as a top-level dataform.

- `!label := "<text>"`<br/>
  `!l := "<text>"`


  Assign the descriptive text as a caption/label for the dataset.

- `!childobs := <integer>`


  Used only in combination with !parent. Gives the number of allowed child observations in the child dataset   (0 = no limit)

- `!afterobs := <integer>`


  Used only in combination with !parent. Tells EntryClient what happens after entry of one complete observation   

  - 0 = new observation
  - 1 = return to parent
  - 2 = return on max number of observataions
  - 3 = stay on the current observation

- `!statusbar := "<text>"`

  Sets the "content string" of a dataform (see manager for formatting).

- `!size := <integer>`

  Initialize the dataform with <size> empty observations.

See [variables](#referencedvars) on using referenced variables for this command

## new variable

```
new variable <variablename> <type> [:= expression] [!options...]
```

Create a new variable of a given type and optionally assign the value in expression. The variable type and expressions type must be compatible. Variables contain a value for each observation. If no expression is given, all values will be missing.

### parameters
- `variablename` 

  The new variable name, <b>not enclosed in quotes</b>

- `type`

  The [type](#variabletype) of variable, either the long or short version

### options            

- `!label := "<text>"`<br/>
   `!l := "<text>"`

  Assign the descriptive text as a label for the variable.   An existing variable label will be replaced with the new one.

- `!valuelabel := <valuelabel name>`<br/>
  `!vl := <valuelabel name>`

  Assign an existing valuelabel set to the variable.   An existing assignment will be replaced but not deleted. To delete the existing valuelabel   set see [deleting content](#deletegrp)

- `!length := <integer> `<br/>
   `!le := <integer>`

  Changes the entry length of a variable

- `!d := <integer>`<br/>

  Change the decimal entry length for floating point variables. Changing the decimal length for other variable types have no impact

- `!rangelow := <value>`

  Set the lower bound for a range of values. Must be used in combination with !rangehigh

- `!rangehigh:= <value>`

  Set the upper bound for a range of values. Must be used in combination with !rangelow

- `!entrymode := {0|1|2}`

  Change the entry mode used in EpiData EntryClient
  
  - 0 = default
  - 1 = must enter
  - 2 = no enter

- `!confirm`

  If used, the variable has the "confirm entry" flag set. Used in EpiData EntryClient

- `!key`

  Adds the variable to be part of the key for the current dataset

- `!cmpX := *variable`

  Where "X" is replaced with one of GT, LT, GE, LE, EQ, NE. Adds comparison between the new variable and the assigned variable

- `!u`<br/>`!memo`

  When creating a string variable it is possible to specify the sub type using one of the above options. !u specifies this is an uppercase string variable. !memo specifies this a memo variable

- `!dmy`<br/>`!mdy`<br/>`!ymd`

  When creating a date variable it is possible to specify the sub type using one of the above options. !dmy is the default type if no option is used else the specified sub type is used

- `!auto [{0|1|2}]`

  When creating a variable that supports automatic content (date, time or integer), using this option changes the default type to the automatic type. Integer become AutoIncrement, DMY becomes AutoDMY, etc. For time and date variables the number specifies when the variable is updated:

  - 0 = When obervation is created (default)
  - 1 = When observation is first saved
  - 2 = Each time the record is saved after being edited
  
### examples

Examples where all observations get the same value:

```
new variable v1 integer := 1 + 2 * 3 - 4;
new variable v2 float   := (2 * pi) * 5;
new variable v3 string  := "Hello World!";
new variable v4 time    := now();
new variable v5 boolean := (2 > 3);
new variable v6 date    := today();
```          

Examples where a value depends on other variables:

```
// v1 is equal to sum of v14 and v17
new variable v1 integer := v14 + v17;
// calculated age in whole years
new variable age date   := integer((today() - dateborn)/365.25)
```

See [variables](#referencedvars) on using referenced variables for this command

<a name="newglobal"></a>
## new global

```
new global <variable> <type> [:= expression]
new g <variable> <type> [:= expression]
```          

Create a new global variable

### parameters

- `variable`

  Variable must be unique. If the variable name is followed by square brackets [...], then a global *vector* is created, where each entry can be individually accessed using

- `type`

  a valid EpiData [type](#variabletype)
  
- `expression`

  a value assigned to the global variable. The global variable type and expressions type must be compatible

A global variable or parameter has only one value, whereas a standard variable has one value for each observation.  Global variables can for most parts be used like as a regular variable.

If a value is assigned when creating a new global vector all entries of the vector will have the same value!

### example
```
new global g1 integer     := 1 + 2 * 3 - 4;
new global g2 float       := (2 * pi) * 5;
new global g3 string      := "Hello World!";
new global g4 time        := now();
new global g5 boolean     := (2 > 3);
new global g6 date        := today();
new global g7[10] integer := 10;
g7[3]                     := 20;
```          

See [variables](#referencedvars) on using referenced variables for this command

<a name="newvaluelabel"></a>
## new valuelabel

```
new valuelabel <name> <type> (<value> , <label>) (...) [!m := <value>]
```

Create a new value label set with a given [type](#variabletype) (boolean not supported) and assign at least one (value, label) pair.
### parameters
- `name`

 The valuelabel name, which must be unique; it cannot be the same as any variable. A useful practice is to start the valuelabel name with an underscore: _

- `type` 

  a valid EpiData [type](#variabletype)
  
- `(value, label)`

  Each pair will be added to the newly created set. The datatype of the value MUST match the defined datatype for the value label set itself. It is not possible to create an empty valuelabel set.

> Note: An empty set will restrict data entry to system missing only!

### options

- `!m := <value>`

  Marks the given value in the value label set as missing.   If the value is not part of the (value, label) pairs or the datatype does not match an error will be reported.   This option can be used multiple times with different values.

### example
```
// "normal" value label
new valuelabel _VL1 integer (1, "Value A") (2, "Value B") (9, "Missing") !m := 9;
// using expression
new valuelabel _VL2 integer (0 + 1, "This " + "is " + "value " + 1) (1 + 1, "This " + "is " + "value " + 2) (2 + 1, "This " + "is" + "value " + 2);
```

See [edit valuelabels](#editvaluelabel)"> for more advanced use of variables and loops to create additional valuelabels

See [variables](#referencedvars) on using referenced variables for this command

# Listing content

<a name="browse"></a>
## browse

```
browse [variable list] ] [options]
```

Show the variables mentioned in a spreadsheet grid

### parameters
- `variable list`

  A single variable name or [list of variables](#variablelist); without variable names, browse all variables

After browse has started you may Right Click and see how to close or adapt columns. Browse will, by default, follow the show formats setting.

### options

- `!caption := "<string>"`

 Give the browser window a custom caption

- `!c ` 

 Close all currently open browsers

- `!a ` 

 Arrange all browsers in a cascade

- `!vn `

  Show variable names instead of following the 'set "FORMAT VALUE LABEL"' and  
 `set "FORMAT VARIABLE LABEL"` options

See [variables](#referencedvars) on using referenced variables for this command

> Note: browse is much faster than list

<a name="list"></a>
## list data

```
list data [variablelist]
```

Show values on the screen for all variables mentioned, with one observation per line (not limited by the width of the display)

## parameters
- `variablelist`

  A single variable name or [list of names](#variablelist); without variables, list all variables.

See [Common options](#commonoptions) for options for labels and formats.

> Note: browse is much faster than list.

> Note: When list follows `select` the sequence number is within the current select, not for the whole dataset.

See [variables](#referencedvars) on using referenced variables for this command

<a name="listproject"></a>
## list project

```
list project
```

Shows a brief overview of the project

###Options       

- `!info` 

  Also shows the study information

<a name="listdataset"></a>
## list dataset

```
list dataset
```
Shows a list of datasets for the project

### options        
- `!all` 

  Outputs additional information about the listed datasets

<a name="listvariable"></a>
## list variable

```
list variable
```

List all currently defined variable names, types, formats and labels

<a name="listvaluelabel"></a>
## list valuelabel

```
list valuelabel
```
Show the full list of all valuelabel sets. Each set is listed individually as value/label pair and marked whether a value is considered missing or not.

<a name="listresults"></a>
## list results

```
list results
```

List all current result variables and their values.

`means`, `describe`, `tables` and other estimation commands create result variables, e.g. $mean[1] or $count. All result variables for a command are cleared when running the same command again.

<a name="listglobal"></a>
## list global

```
list global
```

List currently defined global variables and their types and value. Global variables contain a single value and global vectors contain multiple values. The list shows both types.

## display
```
? <expression>
```

Show result of an expression. It is posible to use all types of variables (standard, results or global) in the expression.
>Note: if you are using standard variables, you can display a specific observation using the index `[ix]`, where `ix` is an integer > 0.

### parameter

* `<expression>`

  Is any expression that evaluates to a single value
  
### examples
```
// display theresult of 10 plus the 5th observation of v1
? v1[5] + 10; 
// g is a global integer
? g1 - 10;
// a result variable
? means_mean;
? 241/34;
? (23 > 19);
? "a " + "b " + "c";
```

# Editing variable and label definitions

<a name="edit"></a>
## edit project

```
edit project
```
Edits the open project.

### options            

- `!label:= "<text>"`

  Adds a title to the project. If not used a default title is given.

- `!showFieldNames := <boolean>`<br/>
   `!sfn := <boolean>`

Show/Hide the field name next to the entry field in Manager and EntryClient

- `!showFieldBorders := <boolean>`<br/>
  `!sfb := <boolean>`

Show/Hide the border of the entry field in Manager and EntryClient

- `!backupInterval := <integer>`

- `!bi := <integer>`

Interval at witch Manager and EntryClient automatically saved the project

- `!backupOnShutdown := <boolean>`<br/>
  `!bos := <boolean>`

Perform a backup when closing the project. The name for the backup is based on the current date/time.

<a name="editdataset"></a>
## edit dataset

```
edit dataset <datasetname> [!options...]
```
Edit an existing dataset in the project.

### parameters

- `datasetname`

  A dataset name, without quotes
  
### options           

- `!label := "<text>"`

  Assign the descriptive text as a caption/label for the dataset.

- `!childobs := <integer>`

  Used only if dataset is related to a parent. Gives the number of allowed child observations   (0 = no limit)

- `!afterobs := <integer>`

  Used only if the dataset is related to a parent. Tells EntryClient what happens after entering the whole observation
  
  - 0 = new observation,
  - 1 = return to parent
  - 2 = return on max observation
  - 3 = stay on current observation
  
- `!statusbar := "<text>"`

  Sets the "content string" of a dataform (see manager for formatting).

- `!size := <integer>`

  Changes the size of the dataset to <size> amount of observations.

- `!r := <new dataset name>`

  Changes the name of the dataset. If the name is already in use an error will occur.

- `!noparent`

  Moves the current dataset (and all related datasets) to be a top-level dataset.

> Note: create a new empty child dataset to restore the relate situation followed by a merge of data in child datasets.

See [variables](#referencedvars) on using referenced variables for this command

<a name="editvariable"></a>
## edit variable

```
edit variable *variable1 [!<options>...]
```

Edit the metadata of *variable1. The options specify which metadata are changed, multiple options may be used at once

### options         

- `!label := "<text>"`

  Assign the descriptive text as a label for the variable. An existing variable label will be replaced with the new one.

- `!vl := <valuelabel id>`

  Assign an existing valuelabel set to the variable. An existing assignment will be replaced but not deleted. To delete the existing valuelabel set, see [deleting content](#deletegrp)

- `!novl`

  Removes an existing valuelabel set from the variable.

- `!l := <integer>`

  Changes the entry length of a variable

- `!d := <integer>`

   Changes the decimal entry length for floating point variables. Changing the decimal length for other variable types have no impact

- `!min := <value>`

   Set the lower bound for a range of values. Must be used in combination with !max

- `!max:= <value>`

   Set the upper bound for a range of values. Must be used in combination with !min

- `!norange`

   Removes an existing defined range for the variable

- `!entry := <integer>`

   Changes the entry mode used in EpiData EntryClient

  - 0 = default
  - 1 = must enter
  - 2 = no enter
   
- `!cmpX := *variable`

  Where "X" is replaced with one of GT, LT, GE, LE, EQ, NE. Adds comparison between this variable and the assigned variable.

- `!confirm`

  If used, the variable has the "confirm entry" flag set. Used in EpiData EntryClient.

- `!noconfirm`

   If used, the variable has the "confirm entry" flag unset. Used in EpiData EntryClient.

- `!key`

   Adds the variable to be part of the key for the current dataset

- `!nokey`

   Removes the variable from being part of the key for the current dataset

- `!r := <new variable name>`

   Changes the name of the variable. If the name is already in use an error will occur.

> Note: Data values are NOT changed! Even if the new length or decimals is shorter than actual content. To keep the changes made you must save the data.

See [variables](#referencedvars) on using referenced variables for this command

<a name="editvaluelabel"></a>
## edit valuelabel

```
edit valuelabel <name> [(<value> , <text>) ...] [!m := <value>] [!delete := <value>] [!nomissing := <value>]
```

Edit an existing value label set and optionally assign any number of (value, label) pairs.

### parameters

- `name`

  Name of the valuelable, <b>not in quotes</b>
  
- `(value, label)`

  The value and its label. If a (value, label) pair already exist, the new label will replace the old label. Otherwise the (value, label) pair will be added to the set. The datatype of the value MUST match the datatype for the value label set itself.

### options        

- `!m := <value>`

  Marks the given value in the value label set as missing. If the value is not part of the (value, label) pairs, an already existing pair or the datatype does not match an error will be reported.

  This option can be used multiple times with different values.

- `!d := <value>`

 Deletes the value label pair with the given value. If no such pair exists and error will be reported.

 This option can be used multiple times with different values.

- `!nom := <value>`

 Removes the marks on the given value label pair that it should be considered missing. If no such pair exists and error will be reported.

 This option can be used multiple times with different values.

- `!r := <new value label name>`

 Changes the name of the value label. If the new name is already in use an error will occur.

> Note: All variables already using this value label will continue to have the same label.

To remove a valuelabel from a variable, see [edit variable](#editvariable)

### example
```
// Create a new simple valuelabel
new vl _VL1 int (1, "A");

// Simple edit: add another valuelabel
edit vl _VL1 (2, "B");

// Simple edit: replace an existing valuelabel
edit vl _VL1 (2, "Replaced B");

// Create a valuelabel set using a loop.
new valuelabel _VL2 int (1, "This is the first value label"); // create a new valuelabel set
new global i integer;                                         // we also need a loop variable

// now create 5 pairs (2, "... 2") (3, "... 3") ...
for i := 2 to 5 do
  edit valuelabel _VL2 (i, "This is valuelabel no: " + i);
```

See [variables](#referencedvars) on using referenced variables for this command

<a name="editdata"></a>
## edit data

```
edit data [!md] [!nomd] [!mv] [!nomv]
```

Edit the status of observations

### options           

- `!md / !nomd`

  Marks / Unmarks the current select observations for deletion

- `!mv / !nomv`

  Marks / Unmarks the current select observations as verified

<a name="recode"></a>
## recode

```
recode <from variable> to <to variable> !by := <value> [!options]
recode<from variable> to <to variable> (<lower bound>, <upper bound> [, <label lext> [, <label value>]]) .. [!options]
```

Recode transforms data from the <from variable> to the <to variable> but groups data within the same range to a given values.
Recode can be used in two forms:

* Recode using a specific interval

  In the simple form data are recoded using a specific interval. The size of this interval is specified with the !by := <value> option and starts a 0. If there is no data for a given interval, no value label will be created.
  
* Recode using custom intervals:

  With this form it is possible to control all (or partial) aspects of the intervals during the data transformation. At the least the range(s) needs to be specified by the <lower bound> and <upper bound>. In addition to this it is possible to specify a label for the interval and a user defined value for the interval.

## parameters
* `from variable`

  the original variable
* `to variable`

  the variable receiving the recoded values
### second form only
* `lower bound, upper bound`

  The range to be recoded, inclusive of `lower bound` and exclusive of `upper bound`
* `label text`

  optional string or global string variable with the label for this interval
*  

  optional value for the new interval
  
### options
* `!by := <value>`

  size of the intervals. Cannot be used in combination with custrom intervals
* `!replace`

  Replace the <to variable> or the resulting value label set, if they  already exist. Otherwise, an error will occur.
* `!max := <value>`

  All data above the max value will be recoded as missing data.
* `!nvl`

  Do not create a value label set for the TO variable
* `!i [:= <value>]`

  The lowest interval is recoded to the value 1 and given the value label according to the interval values. Subsequent intervals are recoded to 2,3,4 etc. If a value is specified, start with this instead of 1, subsequent intervals are recoded to <value>+1, +2, +3, etc.
* `!m := <value>`

  All observations with value . (system missing) in the <to variable> will get this value and the value will be marked as missing in the value label defined by the recode command.

  If the user indicates recoding of system missing values with the !m option, then the user must ensure this does not conflict with other recoding. The <to variable> variable can have sysmis (".") in the followoing ways:

  * Copy all sysmis values in the variable: Do not use the !m option
To recode an interval to sysmis: Exclude the interval from the declaration.
  * To recode values below or above a give value to sysmis: use !min and !max options, e.g.
Recode age to agegrp !by:=5 !min:=10 !max:=75

### examples

```
read "bromar.epx";
// Recode data in intervals of 10:  [0, 10) [10, 20) [20, 30) ...
recode age to ag10 !by := 10     
// Recode data to intervals: [0, 10) [10,25) [30, 50)
recode age to ag (0, 10, "Label A") (10, 25) (30, 50, "Label B", 2)
// note: [0, 10)  will have value label:  0 = "Label A"
//       [10, 25) will have value label: 10 = "10 - 25"  (default)
//       [30, 50) will have value label:  2 = "Label B"
```


See [variables](#referencedvars) on using referenced variables for this command

<a name="assignment"></a>
## assignment

```
<variable> := <expression>
```

Use `:=` to assign the value given in an expression. The variable type and expressions must be compatible otherwise an error will occur.

>Note: `=` is a comparison operator only.

### parameters
* `variable`

  Any global or data variable
  
* `expression`

  Any expression to evalues to either a single value or to values equivalent to a data variable
  
### examples
```
// all observations get the same value 
v1 := 1 + 2 * 3 - 4;
v2 := (2 * pi) * 5;
v3 := "Hello World!";
v4 := now();
v5 := (2 > 3);
v6 := today();
       
//assign a value to individual entries of a variable:
v1[1] := 3;
v2[2] := 31.41596;
v3[3] := "It works!";
v4[1] := Createtime(12, 34, 56);

// a calculated value is assigned to every observation
v1 := 1 + v2 * v3;
       
// use select to change values for a subgroup
select (v1 = 0) do v17 := 17;
select ((v1 = 0) and (v2 = .)) do v17 := 27;

// functions may be used:
select (age = .) do age := integer((today() - dateborn)/365.25)

```
<a name="reorder"></a>
## reorder

```
reorder variablelist [!options]
```

Reorders the variables specified. This can be used to place specific variables together to be better used with variable expansion

### parameters
* `variablelist`

  a list of variable to move
  
### options
*  `!before := <var>`


  Places the variables before this variable
* `!after:= <var>`

	Places the variables after this variable
* `!last`

  Places the variables at the end of the list

If no options are specified, the default is to place the variables before all other variables in the list. Use F3 or `list variable` to show the current order of variables.

### examples

```
read "bromar.epx";
// Move the variables kmgrpm, agegrp, and decgrp to the front of the list               
reorder kmgrp agegrp decgrp;     
// Move the variables age and km in front of agegrp
reorder age km !before := agegrp 
```

# Deleting content

<a name="drop"></a>
## drop dataset

```
drop dataset <name> [name2 ...]
```
 
Remove the listed datasets (and related datasets) from memory

### parameters

 - `name`, `name2`, etc

 The name(s) of the datasets to drop

See [variables](#referencedvars) on using referenced variables for this command

## drop data / drop d
```
drop data [!del]
```

Drop all data within current select from memory. Save the data first if you wish to keep any changes.

### options       

- `!del` 

  Drops all observations marked for deletion.

> Note: Make sure to test whether this creates a problem in a related dataset with the check command		

### example

```
read "bromar.epx";
select (id > 1000) do
  drop data;  // Drops all observations where id > 1000, but keeps the rest.

read "bromar.epx";
drop data !del ; // drop all observations "marked for deletion"
```        

## drop variable
```
drop variable [variablelist]
```

Remove the listed variables from memory
### parameters

- `variable`

  may be shortened to `var` or `v`
  
- `variablelist`

  The [list of variables](#variablelist) to drop

See [variables](#referencedvars) on using referenced variables for this command

## keep variable
```
keep variable [variablelist]
```

Sometimes it will be simpler to list the variables to keep in memory. With `keep` you drop all variables not included in the list
### parameters

- `variable`

  may be shortened to `var` or `v`

- `variablelist`

  The [list of variables](#variablelist) to keep

See [variables](#referencedvars) on using referenced variables for this command

# Consistency and Validity Check of data

<a name="check"></a>
## check data

```
check data [var1 ...]
```
Use this command to perform a check of the data in selected variables (if no variable are specified, then ALL variable are checked).

The data is checked for:

- Data length: Is the number of characters used in data within the length specified for the variable
- Range/Valuelabel: Is the data within the specified range and/or is it a legal value label
- Must Enter: Does the variable have data for all observations if it is marked as Must Enter
- Jumps: If a variable has jumps assigned, do the skipped fields have the correct values
- Comparison: If a variable is compared to another variable, is the comparison uphold.

### example

```
read "bromar.epx"
check data                   // checks all variable
check data dectime kmgrp age // Only checks the variables dectime, kmgrp and age
```

See [variables](#referencedvars) on using referenced variables for this command

<a name="checkkey"></a>
## check key

```
check key [var1 ...]
```
Check that the data in specified variables are unique and represent a key.

If no variables are specified and a key is already present in the current dataset, this key is checked.

### example

```
read "bromar.epx"
check key id                 // checks if the variable ID represents a unique key
```

See [variables](#referencedvars) on using referenced variables for this command

<a name="checkrelate"></a>
## check relate

```
check relate
```
Check that all observations have a valid parent observation

### example

```
read "related_data.epx";  // Load the project
use child_dataset;        // Change dataset to a related dataset
check relate;             // Perform the check from the child dataset "upwards" to the parent.
                          // Must be repeated if you have more levels
```
<a name="checkstudy"</a>>
## check study

```
check study
```
Check that the study information of is specified or not.

### example

```
read "samplev3.epx";  // Load the project
check study;          // Perform the check
```

<a name="report"></a>
# REPORTS


## report users

```
report users
```
If a project is using Extended Access control, this command will show a condensed report of the log entries and a list of failed login attempts.

If the project is not using Extended Access control, an error will be displayed.

## report validate

```
report validate [variable list] [!options]
```
Compares two dataset / projects against each other, validating the data content and outputs a report of differences based on the comparison.

### parameters

- `varlidate`

  may be shortened to `val`
    
- `variable list`

  denotes the sorting variables. This is required if not comparing whole projects OR if the datasets does not contain any key variables.

### options

- `!fn := "<string>"`

 Opens an external file to compare with.

- `!ds := <dataset id>`

 Specifies a single dataset (internal/external) to compare with.

- `!nos`

 Excludes all string types from comparison

- `!nodt`

 Excludes all date and time types from comparison

- `!noauto`

 Excludes all auto types from comparison

- `!noc`

 All text comparisons are done case in-sensitive

- `!nol`

 Only show the condensed report - do not show the list of observations

- `!val`

 All records that pass the comparison will be marked as verified. The pass is based on the option chosen from above!

### example
```
read "bromar.epx";               // Load the project

// Run a report based on the two internal datasets
// (1st is currently used, 2nd is the one marked with !ds :=...)
report val id !ds := ds2;

// Run a report based on the two datasets, one internal and one external
// (1st is currently used, 2nd is the one marked with !ds :=...)
report val id !fn := "double_entry.epx" !ds := ds1

// If you have two projects there are two ways compare there.
// If you wish to compare individual dataset, use the options above.
// If you have two project you wish to make a complete validation on, use following:

// Run a report based on the two complete projects, one internal and one external
report val !fn := "double_entry.epx"

// The last example is a special case where both the internal and external project only contains
// a single dataset each. In this case you only need to specify the sorting variable(s)
// and the external file. The dataset option is not needed since the external project only has a single dataset.
report val id !fn := "double_entry.epx"
```

## report countby
```
report countby [variable list] [!options]
```
Compares the combination of variables across several datasets. 

The output is a report with a condensed table of the found keys and a complete table with the found unique key values and the count of these in each dataset.

### parameters

- `countby`

  may be shortened to `cby`
  
- `variable list`

  The variables var1 .. varn is considere a "key" and each unique combination of this key is counted across all the specified datasets.
  
### options

- `!fn := <global string vector>` 

   This option accepts a global vector with the filenames that are included in the report. The files can be in different formats, but the variable names MUST be the same in each file.
   
   If a file name is sys.missing (.), the dataset in the currently opened project is used.

- `!ds := <global string vector>` 

   This option accepts a global vector with the dataset name that is included in the report. The number of entries in the dataset variable MUST be the same as the filenames.

- `!nol` 

   Only show the condensed report - do not show the list of observations

### example
```
// Setup the input for the report:
new global filenames[5] string;
filenames[1] := "count_file_1.epx";
filenames[2] := "count_file_2.rec";
filenames[3] := "count_file_3.dta";
filenames[4] := "count_file_4.csv";
filenames[5] := .;   // use the current file
// Setup the dataset names
new global datasets[5] string;
datasets[1] := "ds1";
datasets[2] := "ds1";
datasets[3] := "ds1";
datasets[4] := "ds1";
datasets[5] := "ds1";
// Run the report:
report cby id !fn := filenames !ds := datasets
```

# Descriptive statistics

<a name="count"></a>
## count

```
count
```
Counts number of observations. Count may be used with select to count within a subgroup. No parameters or options apply.

### result variables:  
- $count

<a name="sort"></a>
## sort

```
sort [variable list] [!option]
```

Sort the current dataset based on the given variables. Sort respects current select!

### parameters

- `variable list`

  The variables to sort on
  
### option

- `!descending`<br/>
  `!d`

 Sorts the dataset in decending order. The default is ascending order.

See [variables](#referencedvars) on using referenced variables for this command

<a name="select""></a>
# Select records

```
select (<condition>) do <command>
select (<condition>) do begin <command block> end;
```
Select a subset of records for the subsequent command or command block. The selection only has effect for this commands or commands. Unlike in classic EpiData, the selection is not retained.

### parameters
- `condition` can be any logical condition that compares individual data values
- `command` may be any command that operates on data except for `save`
- `command block` is a group of commands, each of which must end with a semicolon, as in any program

### example

```
// get the epicurve for children under 18
select (age < 18) do epicurve onsetdate;
// get mean age and a food-specific attack rate table for men only
// select (gender="M") do begin
means age;
ctable ill food1-food5 !ar;
end;
```

<a name="describe"></a>
## describe

```
describe <variable list> [option list]
```
Basic descriptive statistics and frequencies for a group of variables

With no options specified, a single table will be provided, with one row per variable showing: number of observations, number of unique values

For numerical variables, the output will also include mean, standard deviation, minimum, median, maximum

### parameters

- `variable list`

  The list of variables to provide statistics for
  
### statistic options
Use any combination of options to customize the output

- `!msd:` mean, standard deviation and sum

- `!mci:` mean and confidence interval. See [set](#set) to change the confidence interval

- `!rm:` minimum, median, maximum

- `!idr:` 10th percentile, median, 90th percentile

- `!iqr:` 25th percentile, median, 75th percentile

- `!fh:` 5 most frequent values

- `!fl:` 5 least frequent values

- `!fb:` 5 most frequent and 5 least frequent values

- `!ct:` force one row per variable when the above options are specified. This will be ignored if one of `!fh` `!fl` `!fb` is specified.

- `!m:`number of missing values

See [variables](#referencedvars) on using referenced variables for this command

See [Common options](#commonoptions) for options for labels and formats.

### methodology notes
- All statistics are based on the `means` command and all frequencies are based on the `freq` command, so results from `describe` will be exactly the same as those from `means` or `freq`.

<a name="freq"></a>
## freq

```
freq variable1 [!<option> ...]
```

Frequency distribution for `variable1`

### parameter
- `variable1` 

  may be any [type](#variabletype)

### options
- `!m`

 Include observations with missing data (.)

- `!cum`

 Add cumulative percentage

- `!pr`

 Add row percentage

- `!ci`

 Calculate confidence intervals for row percentage

- `!w:=weightVariable`

 weightVariable contains survey weights, which will be used to estimate population percentages.

See [Common options](#commonoptions) for options for labels and formats.

See [variables](#referencedvars) on using referenced variables for this command

<a name="tables"></a>
## tables

```
tables <column variable> <row variable> [!<option> ...]
```

Crosstabulate the variables chosen.

### parameters

- `column variable`

  This variable's values will be the column labels in the table
  
- `row variable`

  The variable's values will be the row labels in the table
  
###  Data and output options

- `!m`

  Include observations with missing data (.)

- `!w := <variable>`

 Use number of observations in the variable as frequency weight

- `!by := <variable>`

 Stratify the data by this variable. If multiple !by options are used, each unique combination of values from the by-variables will have it's own sub-table.

- `!q`

 Hide all output! Result variable are still calculated

- `!nc`

 Hide combined/unstratified tables

- `!nb`

 Hide sub/stratified tables

- `!ns`

 Hide summary table

### percentage options

- `!pr`

 Show row percents for each table cell and col/row totals

- `!pc`

 Show col percents for each table cell and col/row totals

- `!pt`

 Show total percents for each table cell and col/row totals

### sort optionlist

Indicate by !sxxx where the x may include<br/>
           r:row c:Column a:Ascending d:Descending t:Total l:label (else numerical)

- `!sa` Sort col & row in ascending value order

- `!sd` Sort col & row in descending value order

- `!sla` Sort col & row in ascending label order

- `!sld` Sort col & row in descending label order

- `!sca := <index>` Sort col ascending value order in given index

- `!scd := <index>` Sort col descending value order in given index

- `!sra := <index>` Sort row ascending value order in given index

- `!srd := <index>` Sort row descending value order in given index

- `!scta` Sort on col totals ascending order

- `!scta` Sort on col totals descending order

- `!srta` Sort on row totals ascending order

- `!srtd` Sort on row totals descending order

### estimation and testing options

- `!t`

 Chi<sup>2</sup>

- `!ex`

 Fisher Exact test for 2x2 tables only

- `!odds`

 Odds Ratio and confidence interval for 2x2 tables, including Mantel-Haenszel adjustment for stratified data

- `!rr`

 Risk Ratio and confidence interval for 2x2 tables, including Mantel-Haenszel adjustment for stratified data

> Note:The default is to estimate the 95% confidence interval for odds ratio or risk ratio. See the [set command](#set) to choose a different interval.

See [Common options](#commonoptions) for options for labels and formats.

See [formatting](#formatting) for options on formatting percentages

<a name="ctable"></a><a name="cta"></a>
## ctable

```
cta <column variable> <row variables> [!<option> ...]
```
The ctable command summarizes a series of cross tables for the first variable against each of the following variables.

### parameters
- `column variable`

  This variable should only have two values, as with an outcome
- `row variables`

  A [list of variables](#variablelist) that will form the rows ot the compact table
  
### options

The ctable options have the same meaning as in the tables command.

### data and output options

- `!m`

 Include observations with missing data (.)

- `!w := <variable>`

 Use number of observations in the variable as frequency weight

- `!by := <variable>`

 Stratify the data by this variable.  If multiple !by options are used, estimates of odds ratio, risk ratio and chi-square will be based on the combination of all stratifying variables.

> Note that attack rates and the Fisher Exact Test will be based on unstratified data always.

- `!q` Hide all output! Result variable are still calculated

### sort options
Sorting (applies to individual variable tables). Indicate by !sxxx where the x indicate:<br/>
          r:row c:Column a:Ascending d:Descending t:Total l:label (else numerical)

- `!sa` Sort col & row in ascending value order

- `!sd` Sort col & row in descending value order

- `!sla` Sort col & row in ascending label order

- `!sld` Sort col & row in descending label order

- `!sca := <integer>` Sort col ascending value order in given index

- `!scd := <integer>` Sort col descending value order in given index

- `!sra := <integer>` Sort row ascending value order in given index

- `!srd := <integer>` Sort row descending value order in given index

- `!scta` Sort on col totals ascending order

- `!scta` Sort on col totals descending order

- `!srta` Sort on row totals ascending order

- `!srtd` Sort on row totals descending order

### estimation and testing options

- `!t`

 Chi<sup>2</sup> and p-value

- `!ex`

 Fisher Exact test for 2x2 tables only

- `!odds`

 Odds Ratio for 2x2 tables, including Mantel-Haenszel adjustment for stratified data

- `!rr`

 Risk Ratio for 2x2 tables, including Mantel-Haenszel adjustment for stratified data

> Note:The default is to estimate the 95% confidence interval for odds ratio or risk ratio. See the [set command](#set) to choose a different interval.

### Attack rate table
An attack rate table is commonly used in food-borne outbreak investigations. These options simpligy review and reporting of multiple exposures.

- `!ar`

 Show unstratified 2x2 tables, attack rates and risk ratios

- `!en`

 Show unstratified 2x2 tables

### output table sort options
Only one may be given

- `!sn`

 Sort the table rows by variable name

- `!sl`

 Sort the table rows by variable label

- `!ss`

 Sort the table rows by key statistic, depending on the estimation options<br/>
 priority is given to RR then OR then Fisher Exact P then Chi<sup>2</sup> P

See [Common options](#commonoptions) for options for labels and formats.

See [variables](#referencedvars) on using referenced variables for this command

<a name="t-test"></a>
<a name="ttest"></a>
<a name="ftest"></a>
<a name="means"></a>
## means

```
means variable1 [!by:=variable2] [!t]
```
Basic descriptive statistics for `variable1`, optionally stratified by `variable2` with analysis of variance.

- Statistics: count, total, mean, variance, standard deviation, 95% confidence interval for the mean, standard error, skewness, excess kurtosis.
- Percentiles: minimum, 5%, 10%, 25%, median, 75%, 90%, 95%, maximum.

### parameters
- `variable1` must be numeric

### options
- `!by:`

 Stratify by this variable		    

- `!t:`

 Analysis of Variance to test for homogeneity of the mean across strata, including Bartletts test for homogeneity of variance.

 With `!by` (stratified), F-test is given.

 Without `!by` (one stratum), T-test that mean=0 (e.g. as a paired T-test for the difference in before and after measures)

> Warning:  Check results carefully if !by variable has only one observation in a stratum

Estimates are saved as result variables. Use the command `list results` for details

See [Common options](#commonoptions) for options for labels and formats.

> Note: The default is to estimate the 95% confidence interval for odds ratio or risk ratio. See the [set command](#set) to choose a different interval.

See [variables](#referencedvars) on using referenced variables for this command

### methodology notes:

- confidence intervals given are based on the t-distribution with N-1 degrees of freedom.
- adjusted Fisher-Pearson coefficient of skewness: see [NIST handbook 1.3.5.11](https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm)
- excess kurtosis: see [Wikipedia - Kurtosis (accessed 2020/02/08)](https://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis)
- Bartlett's Test: see [NIST handbook 1.3.5.7](https://www.itl.nist.gov/div898/handbook/eda/section3/eda357.htm)

<a name="regress"></a>
## regress

``` 
regress <dependent variable> <independent variable list> [options]
```

Linear regression analysis with one or more independent variables, which provides estimates for the model<br/>
y = b0 + b1 x var1 + b2 x var2 ...<br/>

### parameters
- `dependent variable`

  Must be numeric
  
- `independent variable list`

  One or more numeric variables
  
### options
- `nocon`

  Do not include the intercept in the model
- `est := <variable for estimates>`

  Save the estimated values in an existing variable of type `Float`
  
- `res := <variable for residuals>`

  Save the residuals in an existing variable of type `Float`

Estimates are saved as result variables. Use the command `list results` for details

See [Common options](#commonoptions) for other options, for labels and formats

See [variables](#referencedvars) on using referenced variables for this command
### methodology notes:
- estimates are calcualted using the standard least-squares method provided by the [LMATH library](https://wiki.freepascal.org/LMath)

  
# Graphs and charts

<a name="survival"></a><a name="sur"></a>
## survival

```
survival <outcomevariable> <timevariable> [!by:=stratifyvariable] [options]
survival <outcomevariable> <date1> <date2> [!by:=stratifyvariable] [options]
```

Kaplan-Meier plots and lifetables for time-to-failure data with censoring. Tabulations of survival at each time when there were deaths (failures), plus confidence intervals. A summary table shows the median survival by stratum. The KM plot is always provided in a separate window unless !q is specified as an option.

### parameters
- `outcomevariable` 

  The outcome usually has values that indicate 'died' and 'lost to followup'. It must have discrete values, one of which indicates failure or death. The outcome variable may be of type numeric or string.

  
- `timevariable` 

  A time, usually in days, which must be an integer
  
- `date1` and `date2` 

 both must be date variables. Elapsed time, effectively the time variable, is calculated as `date2 - date1`

### options
- `!o`

 Specify the value of outcome indicating death (failure), which may be numeric or text; the default is zero

- `!by:=stratifyvariable`

 Stratify by this variable, which should have a small number of unique values

- `!t`

 Log-rank test for equality of survival among strata

- `!ref:=value`

 reference value for the hazard ratio (only with !t)

- `!w:=weightVariable`

 Specify a weight variable

- `!mt` Missing values of date2 take the maximum value of date2

- `!exit:=datevalue`

 Missing values of date2 are assigned this date. It may be easiest so use the createdate function to specify the date.

- `!i:="t1,t2,t3,...tn"`

  Aggregate data to these time intervals. If the string is missing, the set value for LIFETABLE INTERVAL is used

- `!adj`

  When intervals are specified, adjust the number at risk to exclude half of the censored subjects (Hosmer, Lemeshow)

### output options

- `!nt` 

  Omit the lifetables

- `!nou` 

  Omit the unstratified lifetable

- `!nos` 

  Omit the stratified lifetables

- `!ns` 

  Omit the summary table
- `!ng` 

  Do not show the KM plot

### Kaplan-Meier plot options

- `!cb` 

  Copy the KM plot points to the clipboard for use in other software

By default, confidence intervals are shown as error bars. Change this with these options:

- `!cin` 

  Omit the confidence intervals from the KM plot

- `!cib` 

  Show the confidence intervals as shaded bands.
  
- `!cil` 

  Show the confidence intervals as dotted lines.

### Graph options

 `survival` is a graph command and any other [graph option](#graphoptions) may be specified except for `!xmin !xmax !ymin !ymax`
 

### result variables
Estimates are saved as result variables. Use  `list results` for details

### methodology
Confidence intervals are calculated using the method in <b>Statistics with Confidence</b>, referenced elsewhere.

See [Common options](#commonoptions) for other options, for labels and formats

See [variables](#referencedvars) on using referenced variables for this command

See [variables](#referencedvars) on using referenced variables for this command

<a name="line"></a>
<a name="scatter"></a>
## scatter

```
scatter <Xvariable> <Yvariable> [graphoptionlist]
```

Simple scatter or line plot for two variables.

### parameters
- `Xvariable`

  may also be integer, float or date/time
- `Yvariable`

  may be integers or float

### options
- `!l`

   Draw a line instead of points
   
- `!p`

   Draw points as well as a line (use if `!l` was specified)
   
- `!colors:="colorMap"`

	colorMap is a string of up to 10 digits mapping the Analysis colours to the chart series. For `scatter`, a single digit may be specified:
	
	`scatter xvar yvar !colors:="4"`
	
- `scatter` is a graph command and any [graph option](#graphoptions) may be specified

See [variables](#referencedvars) on using referenced variables for this command

<a name="barchart"></a>
## barchart

```
barchart <variable> [StratifyVariable] [options]
```
Will change to `fbarchart` in a future release

Draw a frequency barchart for `Variable`, showing frequencies or percentages at each indiviual value of the variable.

### parameters
- `variable`

  may be of any type
- `Stratifyvariable`

  may be of any type

### options
- `!pct`

  Y-axis values are percentage of the total across strata
- `!w:=weightVariable`

 for grouped data, specify the weights
- `!stack`

 stack bars for stratified data; !stack and !pct together will have stacked bars that sum to 100%

- graph options

 `barchart` is a graph command and any graph option may be specified except for `!xmin !xmax !ymin`

See [variables](#referencedvars) on using referenced variables for this command

<a name="histogram"></a>
## histogram

```
histogram <variable> [StratifyVariable] [options]
```
Draw a histogram for `variable`, based on consecutive integer or day intervals. The user is responsible for recoding variables so that consecutive intervals make sense.

A histogram is a frequency bar chart where every integer value within range is represented on the X-axis.

### parameters
- `variable`

  may be integer or date
- `Stratifyvariable

  may be of any type

### options
- `!interval:=i`

 where i is an integer > 1, will group bars; the default is 1
- `!w:=weightVariable`

 for grouped data, specify the weights.
- `!stack`

 stack bars for stratified data.
- graph options
 `histogram` is a graph command and any [graph option](#graphoptions) may be specified except for `!ymin`

See [variables](#referencedvars) on using referenced variables for this command

<a name="epicurve"></a>
## epicurve

```
epicurve <Variable> [StratifyVariable] [options]
```
Draw an epidemic curve for a variable, based on consecutive integer or day intervals. The user is responsible for recoding variables so that consecutive intervals make sense.

An epicurve is a stacked histogram, where individual boxes are shown for each subject

### parameters
- `Variable`

  may be integer or date
- `StratifyVariable`

  may be of any type

### options
- `!interval:=i`

 where i is an integer > 1, will group bars; the default is 1
 
- graph options 

 `epicurve` is a graph command and any [graph option](#graphoptions) may be specified except for `!ymin`

# SPC Charts
<a name="pareto"></a>
## pareto

```
pareto <Variable> [options]
```
Draw a pareto chart for a variable. The chart has two components: a bar chart showing counts for the variable in descending order by count and a line chart showing cumulative percentages.

### parameters
- `Variable`

  may be of any type

### options
- `!by:=sVariable`

  `sVariable` may be of any type
  
  There will be a chart for each value of `sVariable`. The charts will appear as tabs within a window.
  
- `!w:=wVariable`

   weight the counts using `wVariable`
   
- graph options

   `pareto` is a graph command and any graph option may be specified

See [variables](#referencedvars) on using referenced variables for this command


<a name="graphoptions"> </a>
## graph options


Any of the graph commands may use the following options.

- `!ti|title := "Custom main title"`

- `!fn|footnote:= "Custom footnote"`

- `!xt|xtitle := "Custom x-axis title"`

- `!yt|ytitle := "Custom y-axis title"`

- `!xmin := <real number|date>`

- `!xmax := <real number|date>`

- `!ymin := <real number|date>`

- `!ymax := <real number|date>`

	- axis minimum and maximum values are not available to all chart types [*see the indiviual commands*]
	- axis minumum and maximum values may be expressed for some graphs as numbers with or without a decimal place or as date values, depending on the variable type 
	- if a given value will excluded data from the graph, then it is ignored
	- for some graphs, specifing xmin or ymin := 0 may be required to force the axis to begin at zero

- `!c|colors := "color map"`

   `color map` can take two forms
   
   - up to ten digits (0-9) representing the order that the standard colors will be used. The standard colors (0-9) are Black, Blue, Red, Green, Yellow, White, SkyBlue, Fuchsia, Gray, Aqua. The default order is 1234567890. That means the first color for a graph is blue, then red, green, etc.

   - one or more hexadecimal color codes (#xxxxxx), where each 'x' can be any of {0..9 A..F}. Many online resources explain how to create color codes. #000000 is black, #FFFFFF is white, #0000FF is blue.

   Any errors in specification of the colors leads to an error message. However, the graph command will execute normally, with black being substituted for the incorrect color.
   
- `!e|export|s := "saveFile.ext"`

   `saveFile.ext` must use one of the legal graph export extensions (jpg|png|svg)
   
   If the chart command produces more than one graph, they will be saved as saveFile.ext, saveFile1.ext, etc.
   
### examples

```
   // use red, green, blue for the strata
   barchart var stratifyvar !c:="231"
   // use custom colors (dark blue, purple) for the survival curve strata
   survival outcome time !by:=group !c:="#00008B#C517FF"
   // extend an epicurve beyond the highest date in the data
   epicurve onset !xmax:=createdate(31,10,2022)
   // save multiple pareto charts
   pareto var !by:=stratifyvar !e:="pareto.jpg"
```

See [variables](#referencedvars) on using referenced variables for this command

<a name="if-then"></a>
# If ... then

```
if (<condition>) then do <command> [else do <command>]
if (<condition>) then do begin <command block> end;
if (<condition>) then do begin <command block> end else <command block> end;
```

### parameters
- `condition` can be any logical condition that uses constants, global variables, a single result variable, or a single data value
- `command` may be any command that operates on data except for `save`
- `command block` is a group of commands, each of which must end with a semicolon, as in any program


# Program-wide options

<a name="set"></a>
## set

```
set ["parameter"] [:= "value"]
```
Change the value of an EpiData setting. An example of this is colour or font selection.

All  `set` ["parameter"] definitions may be added to the file **startup.pgm** to define your own defaults. Edit this file using the menu as its location will vary, depending on the operating system.

- ***MacOS***: Analysis / Preferences
- ***Windows / Linux***: Edit / Options

### parameters

- Without parameters, provides a list of available parameters and their current values

- `parameter` any legal set option (must be enclosed in double quotes)
- `value` will be depending on the parameter, but may be a number, text, ON/OFF or a hexadecimal font colour [See colour examples here](https://www.w3schools.com/colors/colors_hexadecimal.asp))

  For settings with ON/OFF or a text value include this in double quotes

  If no value is specified, show the curent value

The case of parameters and values does not matter.

### examples
```
set "echo";
set "echo" := "off";
set "COMMANDLINE FONT COLOUR" := "#FFF000";
```
<!-- NOT IMPLEMENTED
For any command:  `set option [cmd] := [options]`
e.g.  
`set option means := "!t"`
When the specified command is executed the options mentioned will be added to the command.
-->

### Set parameters and defaults

Option | Possible values | Default Value | Comments or example
:---|:---|:---|:---
BROWSER BG COLOUR | hex colour code | "#FFFFFF" | Adjust the colour of the background. e.g. #000000 is black.  |
BROWSER FONT COLOUR | hex colour code | "#000000" | Adjust the colour of the font. e.g. #FFF000 is yellow.  |
BROWSER FONT NAME | string | (depends on the operating system) | Name of the font used in the browser.  |
BROWSER FONT SIZE | <integer> | 10 | Adjust the size of the font in the browser.  |
BROWSER FONT STYLE | <fsBold/fsItalic/fsUnderline> | " " | Adjust the style of the text in the browser. Eg. underlines text, bold text. |
BROWSER OBS DEFAULT COLOUR | hex colour code | "#F0F0F0" | Adjust the colour of "obs" column for normal/default observations |
BROWSER OBS DELETED COLOUR | hex colour code | "#FF0000" | Adjust the colour of "obs" column for observations marked for deletion |
BROWSER OBS VERIFIED COLOUR | hex colour code | "#008080" | Adjust the colour of "obs" column for verified observations |
BROWSER VALUE LABEL | L/V/LV/VL | V | Default option for output of variable data (value and/or label). See [Common options](#commonoptions) for options. This options applies to "list data" and "browse" only |
BROWSER VARIABLE LABEL | VLA / VLN / VN / VNL | VN | Default option for displaying variable name and/or label. See [Common options](#commonoptions) for options. This options applies to "list data" and "browse" only |
COMMANDLINE BG COLOUR | hex colour code | "#FFFFFF" | Adjust the colour of the background. e.g. #000000 is black.  |
COMMANDLINE FONT COLOUR | hex colour code | "#000000" | Adjust the colour of the font. e.g. #FFF000 is yellow.  |
COMMANDLINE FONT NAME | string | (depends on the operating system) | Name of the font used in the commandline edit.  |
COMMANDLINE FONT SIZE | <integer> | 10 | Adjust the size of the font in the commandline edit.  |
COMMANDLINE FONT STYLE | <fsBold/fsItalic/fsUnderline> | " " | Adjust the style of the font, e.g. bold, underline etc |
COMMANDLOG | ON/OFF | ON | When "ON" a complete list of executed commands is saved to a file in current active dir. |
COMMANDLOGFILE | string | commandlog.pgm | Name of the file to save the executed commands |
COMMANDLOGLINES | <integer> | 1000 | The number of lines kept in the commandlog file. If the number of lines is exceeded, the lines are dropped from the beginning |
CONFIDENCE INTERVAL | 90> 95> 99 | 95 | Set the default confidence interval to be estimated by `table` or `ctable`
CSV DELIMITER | <any desired delimiter> | , | The separator used between variables when you export to the clipboard from the browser.
DISPLAY COMMANDTREE WINDOW | ON/OFF | OFF | Opens/Closes the command tree window
DISPLAY DATASET WINDOW | ON/OFF | OFF | Opens/Closes the dataset window
DISPLAY HISTORY WINDOW | ON/OFF | OFF | Opens/Closes the history window
DISPLAY VARIABLE WINDOW | ON/OFF | OFF | Opens/Closes the variable window
ECHO | ON/OFF | ON | When = ON show results, OFF: "silent"<br/>Use `show error` if you wish to suppress errors too!
EDITOR FONT NAME | string | (depends on the operating system) | Name of the font used in the editor.  
EDITOR FONT SIZE | <integer> | 10 | Adjust the size of the font.  
EXITSAVE | YES/NO | NO | If "YES" the user is prompted on closing the program for saving if a project is open and has been modified
INCLUDE DELETED | ON/OFF | OFF | If "ON" then observations marked for deletion is also included in calculations
OUTPUT BG COLOUR | hex colour code | "#FFFFFF" | Adjust the colour of the output background. e.g. #000000 is black.  
OUTPUT CSS FILE | string | (empty) | When using HTML output it is possible to use an external CSS file. If the file name specified does not exist a file will be created with the content of the built in CSS.
OUTPUT CSS INTERNAL | YES/NO | YES | If set to YES, the content of the CSS FILE is embedded into the HTML. If set to NO the CSS FILE is referenced from within the HTML output.
OUTPUT FONT COLOUR | hex colour code | "#000000" | Adjust the colour of the output font. e.g. #FFF000 is yellow.  
OUTPUT FONT NAME | string | (depends on the operating system) | Name of the font used in the text output.  
OUTPUT FONT SIZE | <integer> | 10 | Adjust the size of the font in the text output.  
OUTPUT FONT STYLE | <fsBold/fsItalic/fsUnderline> | " " | Adjust the style of the font, e.g. bold, underline etc
OUTPUT FORMAT | TEXT/HTML | TEXT | Format of the output window HTML viewing is current in beta
OUTPUT SAVE FORMAT | TEXT/HTML | TEXT | Set the default format when saving the output to file.  
SHORT MONTH NAMES | string | (depends on the language of the operating system)eg. Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec | The function `createdate` will use these value when trying to match against the short month names. This list MUST contain 12 items seperated by commas
SHOW COMMAND | ON/OFF | ON | If "ON" then each line that is run (from command line or editor) is added to output as ".<command...>". "OFF" = no output
SHOW DEBUG | ON/OFF | ON | If "ON" then lines containing debug information is shown. "OFF" = no output
SHOW ERROR | ON/OFF | ON | If "ON" then lines containing error information is shown. "OFF" = no output
SHOW INFO | ON/OFF | ON | If "ON" then lines containing informational output is shown. "OFF" = no output
SHOW WARNING | ON/OFF | ON | If "ON" then lines containing warning information is shown. "OFF" = no output
STATISTICS VALUE LABEL | L/V/LV/VL | L | Default option for output of variable data (value and/or label). See [Common options](#commonoptions) for options. This options applies to commands not covered by "BROWSER VALUE LABEL"
STATISTICS VARIABLE LABEL | VLA / VLN / VN / VNL | VLA | Default option for displaying variable name and/or label. See [Common options](#commonoptions) for options. This options applies to commands not covered by "BROWSER VALUE LABEL"

<a name="commonoptions"></a>
# Common options

<a name="valuelabels"></a>
### Valuelabels


- `!v`  Show only the value, (**fallback if no label to corresponding value)

- `!l`  Show only the label (**default)

- `!vl`  Show the value then the label

- `!lv`  Show the label then the value

<a name="variablelabels"></a>
### Variable Labels


- `!vn`  Show only the name, (**fallback if no variable label assigned)

- `!vla`  Show only the label (**default)

- `!vnl`  Show the name then the label

- `!vln`  Show the label then the name

<a name="decimals"></a>
### Decimals for percentages or statistics


- `!d0` 0 decimals

- `!d1` 1 decimal

- `!d2` 2 decimals

- `!d3` 3 decimal

- `!d4` 4 decimals

- `!d5` 5 decimals

<a name="variabletype"></a>
## Variable types


- integer / int / i

  A variable (standard, result or global) that contains an integer value.

- float / f
  A variable (standard, result or global) that contains an floating point value.

> Note: all floating points shown on screen appear in the current national setting (locale),
        but input (from editor or command line) must always use "." (period) as the decimal separator.
         The saved data in a given project can be used in different national settings without giving problems or need for conversions.

- string / str / s

  A variable (standard, result or global) that may contain any string

- boolean / bool / b

  A variable (standard, result or global) that contains only true or false

- time / t

  A variable (standard, result or global) that contains a time value.

- date / d

  A variable (standard, result or global) that contains a date value. All new date variables created will be a DMY type, but this may change in the future.

<a name="variablelist"></a>
## Variable lists

Any command that accepts more than one variable as parameters can use the following schemes for variable expansion.

- `var1-var4` (dash) 

   Use the two variables given and all variables between them.
   
- `var*` (asterisk) 

   `*` is used as a replacement for 0 to many characters. This cannot be the first character.

- `var?` (question mark) 

   `?` is used as a replacement for exactly 1 character. This cannot be the first character.

> Note: If there are no variables matching the result then you get an error

It is possible to combine "*" and "?" for more elaborate expressions, but neither can be combined with "-"

### examples
```
// Consider the following set of variables (and in that order):
// V1, V2, V3, V4, V10, V11, V100
list data V2 - V10;      // V2 - V10 is expanded to the variables V2, V3, V4 and V10
list data V1* ;          // V1* is expanded to V1, V10, V11, V100 
list data V?  ;          // V?  is expanded to V1, V2, V3 and V4
list data V1??;          // V1?? is expanded to V100 only!
```

A `referenced` variable may also be used in the expansion. These will be evaluated before the expansion! See below.

<a name="referencedvariable"></a>
## Referenced Variable

```
@{variable1}
```
With a referenced variable, you essentially use the content of another variable (global, result) to provide the variable name.

### examples

```
new global gvar1 string := "sex";
read "bromar.epx";
freq sex;       // Outputs a frequency table for the variable "sex"
freq @{gvar1};  // Does the same as above, because the content of gvar1 is "sex"
```

This can be combined with indexing of a variable. Using some of the builtin result variables like $dataset and $variable:

```
new global i integer;
for i := 1 to size($variable) do
  begin
    // Here we output the name of all the variables:
    // - not using the @{..} because we want the content of the $variable result var.
    ? $variable[i]

    // Here we do a frequency table of the variable.
    // - using @{..} because "freq" needs a variable and not the content of $variable
    freq @{$variable[i]}
  end;
```

The Variable inside the @{..} may itself be another reference (with or without index), making it possible to combine multiple levels of references.

```
new global f    string := "age"
new global g[3] string := "f";  // All entries have the value "f", but that is fine for this example.
new global h[3] string := "g";  // All entries have the value "g", but that is fine for this example.

// The line below is a valid construction, which evalutes the following way
// 1: h[1] is evaluated into the string "g"
// 2: "g" is used in @{"g"}, which means - use the content of g as a variable
// 3: g[1] is evaluated into the string "f"
// 4: "f is used in @{"f"}, which means - use the content of f as a variable
// 5: @{f} is evalued to the variable AGE
// 6: The command freq is run on the variable AGE.

freq @{ @{h[1]}[1] };
```
Example of how to loop over a referenced variable:

```
// you wish to estimate the time for parts of an analysis and have created a number of time stamps:
new global tx t:= now(); // where x is 1 , 2, 3 etc.

// now to display these and the difference: - assume you had five of these:

new global i i;
for i:= 1 to 5 do
   begin
     ? i + " time: " + @{"t" + i};  // this works becaus the parenthesis will be t1 t2 t3 etc.
     end;

// now also calculate the difference in time between the two:

new global tdif t;   // tdif is a time difference

for i:= 2 to 5 do
   begin
     tdif := (@{"t" + i} - @{"t" + (i-1)});  // notice again the (tdif = t2 -t1 ) when i was = 2
     ? i + " difference : " + tdif;
   end;
```

#Programming aids

<a name="run"></a>
## run

```
run ["<filename.pgm>"]
```

Execute the commands saved in a .pgm file
### parameters
- `filename.pgm`

  may include a path

- without parameters, the open file dialogue is started

<a name="comment"></a>
## // comment


```
// include comments in your program files

/*
  multiline comments are also possible
*/

```

Comments can be on separate lines or go at the end of a command
 
<a name="runtest"></a>
## runtest

```
runtest ["<directory path>"]
```
Run all .pgm files in a given directory (folder) to verify function.

This is provided for testing of correct estimation etc.
        If no path is given, a dialog is shown to select the working directory.
### parameters
- `directory path`

  a directory that contains multiple .pgm files

- without parameters, the open file dialogue is started

<a name="version"></a>
## version

```
version
```

Display the Epidata Analysis software information. This is important to include if you are requesting help. It is the same information available from the Analysis menu.
 
# Clean up - stop
<a name="quit"></a>
## quit

```
quit
```

Close the current project and exit. Any unsaved changes will be lost.
<a name="close"></a>
## close

```
close
```

Stop using a project
- all unsaved variables and changes to existing variables and labels will be lost
- global variables will remain in memory

<a name="cls"></a>
## cls

```
cls
```

Clear the output screen

<a name="clh"></a>
## clh

```
clh
```

Clear the history of commands

<a name="reset"></a>
## reset

```
reset
```

Reset of all parameters of the program!
This is almost equivalent of doing:
```
close;
drop global !all;
cls;
clh;
```
> Note:  `reset` also clears all result variables!

<a name="functions"></a>
# Functions available in EpiData Analysis

In the following, *takes* indicates the variable type for each parameter and *result* indicates the type of the result of the function:

- s: string
- b: boolean
- d: date
- t: time
- i: integer
- f: floating point
- n: any numeric
- v: variable

Parameters may be variables read from fields, created variables, or any expression that evaluates to the correct type.

## String functions

function | takes | result | example
:---|:---|:---|:---
<a name="length"></a>length(str) | s | i | `length("Abcde")` => 5
<a name="post"></a>pos(instr, findstr) | s, s | i | `pos("Abcde", "cd")` => 3<br/>`pos("Abcde", "z")` => 0
<a name="substring"></a><a name="substr"></a>substring(str, start, len) | s, i, i | s | `substring("Abcde", 2, 3)` => "bcd"
<a name="trim"></a>trim(str) | s | s | trim("Abcde ") => "Abcde"<br/>`trim(" Abcde")` => "Abcde"
<a name="lower"></a>lower(str) | s | s | `lower("Abcde")` => "abcde"
<a name="upper"></a>upper(str) | s | s | `upper("Abcde")` => "ABCDE"
<a name="concat"></a>concat(X, s1, s2, ..., sn) | s, any, ... | s | Concat(...) concatenates values s1 -> sn into a string. If any of the sx parameters returns system missing it will be replaced by the value of X<br/>`concat("X", "a", v1)` => "aX" if v1 is missing, otherwise a + the value of v1<br/>For user defined missing values, the actual value is added to the string.

## Arithmetic functions (including Random numbers)
function | takes | result | example
:---|:---|:---|:---
<a name="abs"></a>abs(x) | n | n | `abs(-12)` => 12
<a name="exp"></a>exp(x) | n | f | `exp(1)` => 2.71828182845905
<a name="frac"></a><a name="fraction"></a>fraction(x) | f | f | `fraction(12.34)` => 0.34
<a name="ln"></a>ln(x) | n | f | `ln(2.71828182845905)` => 1<br/>`ln(0)` => missing
<a name="log"></a>log(x) | n | f | `log(10)` => 1<br/>`log(0)` => missing
<a name="round"></a>round(x, digits) | n, d, t | f | `round(12.44,1)` => 12.4<br/>`round(12.5,0)` => 13
<a name="sqrt"></a>sqrt(x) | n | f | `sqrt(4)` => 2
<a name="random"></a>random(x) | i | i | Random integer from 0 to x
<a name="sum"></a>sum(n1, n2, ..., nn) | n, ... | n | Sums the non-missing values n1 => nn;  missing or user defined missing values are ignored.

## Trigonomerty functions

function | takes | result | example
:---|:---|:---|:---
<a name="tan"></a>tan(x) | f | f | `tan(0)` => 0
<a name="arctan"></a>arctan(x) | f | f | `arctan(1)` => pi/2
<a name="cos"></a>cos(r) | f | f | `cos(pi/2)` => 6.12303176911189E-17<br/>`cos(pi)` => -1
<a name="arccos"></a>arccos(r) | f | f | `arccos(0)` => pi / 2
<a name="sin"></a>sin(r) | f | f | `sin(pi/2)` => 1<br/>`sin(pi)` => 6.12303176911189E-17
<a name="arcsin"></a>arcsin(r) | f | f | `arcsin(0)` => 0

## Date functions

function | takes | result | example
:---|:---|:---|:---
<a name="createdate"></a>createdate(datestr) | s | d | The form of datestr is automatically detected, but if the string is ambiguous the preference is always DMY over MDY.<br/>If parts of the datestr are omitted, then these parts are filled with todays values.<br/>If the string is not recognised as a date, system missing is returned.<br/>`createdate("31/12/2016")` => 31/12/2016
createdate(datestr,date-type) | s, s | d | `createdate("31/12/2016", "dmy")` => 31/12/2016<br/>`createdate("12/31/2016", "mdy")` => 31/12/2016<br/>`createdate("2016/12/31", "ymd")` => 31/12/2016
createdate(datestr,fmt-string) | s, s | d  | Converts any string to a date based on the format specified in fmt-string. The format options can be found in the [FPC source documentation](https://www.freepascal.org/docs-html/rtl/sysutils/formatchars.html)<br/>`createdate("31-dec-16", "dd-mmm-yy"`) => 31/12/2016<br/>For the "mmm" format it is possible to control the abbreviated month names using the [set options](#set). The default is based on the language of the Operating System.
createdate(d, m, y) | i, i, i | d | `createdate(31, 12, 2016)` => 31/12/2016
<a name="today"></a>today() | - | i | returns today's date; may be assigned to a date variable or an integer
<a name="day"></a>day(d) | d | i | `day(31/12/2004)` => 31
<a name="dayofweek"></a><a name="dow"></a>dayofweek(d) | d | i | `dayofweek(31/12/2004)` => 5<br/>Monday=1, Sunday=7
<a name="month"></a>month(d) | d | i | `month(31/12/2004)` => 12
<a name="week"></a>week(d) | d | i | `week(22/02/2001)` => 8
<a name="year"></a>year(d) | d | i | `year(31/12/2004)` => 2004

## Time functions

function | takes | result | example
:---|:---|:---|:---
<a name="createtime"></a>createtime(timestr) | s | t | `createtime("12:34:56")` => 12:34:56<br/>The form of *timestr is automatically detected. If parts of the timestr are omitted, then these parts are filled with 0 (zero).
<a name="createtime"></a> createtime(h, m, s) | i, i, i | t | `createtime(12, 34, 56)` => 12:34:56
<a name="now"></a>now() | - | f | returns the time right now. It can be assigned to a time or float variable
<a name="second"></a>second(t) | t | i | `second(12:34:56)` => 56
<a name="minute"></a>minute(t) | t | i | `minute(12:34:56)` => 34
<a name="hour"></a>hour(t) | t | i | `hour(12:34:56)` => 12

## Logic functions

function | takes | result | example
:---|:---|:---|:---
<a name="and"></a>b1 and b2 | b,b | b | `true and true`   => TRUE<br/>`true and false  `=> FALSE<br/>`false and true`  => FALSE<br/>`false and false` => FALSE
<a name="or"></a>b1 or b2 | b,b | b | `true or true`   => TRUE<br/>`true or false`  => TRUE<br/>`false or true`  => TRUE<br/>`false or false` => FALSE
<a name="xor"></a>b1 xor b2 | b,b | b | `true xor true `  => FALSE<br/>`true xor false`  => TRUE<br/>`false xor true`  => TRUE<br/>`false xor false` => FALSE
<a name="not">not | b | b | `not(false)` => TRUE

## Conversion functions

function | takes | result | example
:---|:---|:---|:---
<a name="boolean"></a>boolean(x) | any | b | `boolean(x)` => TRUE, for any non-zero x<br/>`boolean(0)` => FALSE<br/>`boolean("true")` => TRUE, "true" text is case in-sensitive<br/>`boolean(x)` => FALSE, for any text other than "true"
<a name="integer"></a>integer(x) | any | i | `integer(1.23)` => 1<br/>`integer(31/12/2016)` => 42735<br/>`integer("2")` => 2<br/>`integer("a")` => .<br/>Any input x that cannot be interpreted as an integer returns missing "."
<a name="float"></a>float(x) | any | f | `float(1)` => 1.00<br/>`float("12,34")` => 12.34<br/>Any input *x that cannot be interpreted as a float returns missing "."
<a name="string"></a>string(x) | n | s | `string(1.23)` => "1.23"

## Identifier functions

function | takes | result | example
:---|:---|:---|:---
<a name="exist"></a>exist(x) | v | b | Returns true/false whether the provided identifier exists
<a name="idtype"></a>idtype(x) | v | i | Returns the type of the identifier provided. This function can be used on all valid identifiers and the integer value returned has the following meaning:<br/>0: Global variable<br/>1: Global vector<br/>2: Regular Variable<br/>3: Dataset<br/>4: Valuelabel<br/>5: Result Variable<br/>6: Result Vector<br/>7: Result Matrix<br/>if using idtype(x) with the eval function "?", the output will be in text.
<a name="datatype"></a>datatype(x) | v | i | Returns the type of date stored in the variable. The integer value returned has the following meaning:<br/>-1: Variable has no data type - e.g. a dataset variable.<br/>0: Boolean<br/>1: Integer<br/>2: Auto Increment<br/>3: Float<br/>4: DMY Date<br/>5: MDY Date<br/>6: YMD Date<br/>7: DMY Auto Date<br/>8: MDY Auto Date<br/>9: YMD Auto Date<br/>10: Time<br/>11: Auto Time<br/>12: Uppercase String<br/>13: String<br/>14: Memo<br/>if using datatype(x) with the eval function "?", the output will be in text.
<a name="size"></a>size(x) | v | i | Size returns the size/length of an identifier (if applicable).<br/>Global & Result variables always have size 1<br/>Global vector, Result Vector, Variable & Valuelabel return the length/size/count of elements/data<br/>Result Matrix is not implemented yet - it returns -1;<br/>Dataset returns the total number of observations (even if a select is applied).
<a name="string"></a>label(v) | v | s | Return the descriptive label of the identifier. This is only possible for variables and datasets.

## Test and special functions

function | takes | result | example
:---|:---|:---|:---
<a name="lre"></a>lre(x,y) | n | n | `lre($mean1, 1.23456789123456)`<br/>returns number of digits precision of $mean1
<a name="iif"></a>iif(b, x, y) | b<br/>n<br/>n | n | iif(..., true value, false value) evaluates the boolean expression (b) inline, and based on the result either returns the true value or false value.<br/>`iif(2 = 3, "This is true", "This is false")` => "This is false"
<a name="samevalue"></a>samevalue<br/>(x, y, z) | n, d, t<br/>n, d, t<br/>i | b | `samevalue($mean1, 1.23456789123456, 10)`<br/>returns true or false indicating if \|x-y\| < 10<sup>-10</sup><br/>Best used for comparing floating point values. Since the internal binary representation of two seemingly similar numbers may differ, using `x = y` can fail.
samevalue<br/>(x, y) | n, d, t | b | `samevalue($mean1, 1.23456789123456)`<br/>returns true or false indicating if x = y<br/>The same as calling `samevalue(x, y, 15)`
cwd() | | s | Returns the current working directory
deleted([index]) | [i] |  b | Returns true/false whether the record is marked for deletion. If no index is supplied the current record number is tested<br/>`select deleted() do edit data !nomd`<br/>selects records marked for deletion and unmarks them
verified([index]) | [i] | b | Returns true/false whether the record is marked as verified. If no index is supplied the current record number is tested:<br/>`select verified() do edit data !nomd` <br/>selects records marked for deletion and unmarks them

<a name="operators" id="operators"></a>
## Operators used in EpiData Analysis

operator | syntax | result | meaning | example
:---|:---|:---|:---|:---
+ | n+n | n | addition | `1+2` => 3
+ | s+any<br/>any+s | s | concatenation | `"A"+"B"` => "AB"<br/>`"A"+1` => "A1"
+ | d+n | d | date addition | `"30/11/2004"+31` => "31/12/2004"
- | n-n | n | subtraction|`2-1` => 1
- | d-d | n | date subtraction | `"31/12/2004"-"30/11/2004"` => 31
- | d-n | d | date subtraction | `"31/12/2004"-31` => "30/11/2004"
\* | n\*n | n | multiplication | `2*3` => 6
/ | n/n | n | division | `5/2` => 2.5<br/>`5/0` => missing
div | n div n | i | integer result of division | `5 div 2` => 2<br/>`5 div 0` => missing
^ | n^n | f | exponentiation | `5^2` => 25 <br/> `4^0.`5 => 2
( ) | | | group expressions | `(5\*(2+4))/2` => 15<br/>`5\*2+4/2` == (5\*2)+(4/2) => 12
= | n = n | b | equal |`1 = 2` => FALSE
< | n < n | b | less than | `1<2` => TRUE
> | n > n | b | greater than | `1>2` => FALSE
<= | n <= n | b | less than or equal | `1<=2` => TRUE<br/>`2<=2` => TRUE
>= | n >= n | b | greater than or equal | `1<=2` => FALSE<br/>`2>=2` => TRUE
<> | n <> n | b | not equal to | `1<>2` => TRUE<br/>`1<>1` => FALSE
$ | $resultvar  | | result value | `? $count` => 4027

<a name="startup"></a>
## Startup options for EpiData Analysis

The use of startup options depends on the operating system. You may be able to create a desktop shortcut that includes these or start analysis from the command line.

epidataanalysis [options]

### options

- `-h or --help`

  Show this help and exit.

- `-v or --version`

  Show version info and exit.

- `-i or --inifile [FILE]`

  Uses [FILE] as startup program. If no location is specified startup.pgm is used.

### examples

With Linux:

```
./epidataanalysis -i /path/to/startup.pgm
```
