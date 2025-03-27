# Epidata Analysis 3.x

Hints and solutions for accomplishing certain tasks. Additions are welcome on the [Epidata list](mailto:epidata-list@lists.umanitoba.ca)

> Hint: it is better to copy the commands in the examples to the editor and execute them from there.

<a name="toc"></a>
## Table of contents

[TOC]

## Read data, folders, files

### Control set parameters	

Set parameters control how much information you get in the output window (e.g. set statistics), how commands are executed (e.g. set replacedatafile) and other aspects

Use the command `set` alone to see which options are defined. Note the use of double quotes around both the option and its value.

````
set; // see all settings
set "output fontname" := "Arial"; // change font to Arial
````

### Navigate folders

To make a particular folder the working folder for your projects, add a `cd ...` command to the configuration file. In Windows or Linux, use the Edit / settings menu. On a Mac, use the Preferences menu.

````
cd "/path/to/folder"
````

<a href=#toc>Back to contents</a>
## Basic analysis

### Select by date - existing variable

Use the built in date function, with your date format (dmy/mdy/ymd)
To select all subjects born before 12th of February 1988

````
select birthday < createdate(12,2,1988); // using function createdate(day,month,year)
select birthday < createdate("02/12/1988", "mdy"); // using createdate function with "mdy"
select birthday < createdate("12/02/1988", "dmy"); // using createdate function with "dmy"
````

### Select by date - create date variable

Assume you have age in 1996 in field `age`

````
new var born date;
new var yearborn int := 1996 - age;
// abitrarily give them a birthdate at mid year
born = createdate(1, 7, yearborn);
//select all born before 12th of february 1988
select born < createdate(12, 2, 1988);
  begin
   ...
  end;
````

### Test if a string variable contains legal dates

You have read a text (delimited) file, and wish to convert a string variable to a date
Assumed variable names: The string variable with dates is "txtdate", ID is id number.

````
read myfile.csv;
var ;// to see the variables
// Now convert to date:
new var mydate date := date(txtdate); // Incorrect date values will be "."
new var mydate1 date := date(txtdate,"V"); // Incorrect date values will be "01/01/1980"
new var mydateok int := (mydate = mydateok); // 0 if they are the same, 1 if different
freq mydateok; // to see the count
select (mydateok = 1) do list id txtdate mydate mydate1; // to see the values
````

### Check for missing data in several variables

Create an indicator variable `m` which takes on values `N` or `V` for each of 5 variables (`V1` to `V5`). `m` will have values like "NVVVV".

````
new var s m !length:= 5;
m := trim(m) + iif(V1=.,"N","V");
m := trim(m) + iif(V2=.,"N","V");
// etc. for all variables
// an easier way with a small number of variables
m := iif(V1=.,"N","V") + iif(V2=.,"N","V") ...
````

<a href=#toc>Back to contents</a>
## Basic graphs

### Multiple categories on a graph

Many graphs allow for 'stratification' by a variable, such as age group or gender.

````
epicurve onsetdate !by:=gender;
````

### Label graph axes

All graphs have options to change the titles, including axis titles.

````
scatter y x !title:="Income by age group" !xtitle:="Age group" !ytitle:="Household Income";
````
<hr>

<a href=#toc>Back to contents</a>
## Manage output

There are a number of settings that can be used to change the appearance of Analysis, including output, the command line, data browser and graphs. It is also possible to create a style sheet that will be used in the output window. You can also display the output window as plain text (works best with a fixed-width font) or as html (works well with all fonts). If you change output styles, the full output window contents are displayed in the new format.

````
set "output format" := "html";
set "output font name" := "Arial";
set "output font size" := 12;
````

<a href=#toc>Back to contents</a>
## View data

`browse` is best for looking at your data. You can choose to browse only selected variables by including them on the command. The browse menu will always display all data.

Right-click in the browser window to see various options. Most important is the ability to view values or value labels. These options are also available on the `browse` command.

````
browse age sex ill outcome !vl' // to view valuelabels
````
<hr>

<a href=#toc>Back to contents</a>
## Work with dates

### Date variables

In EpiData Analysis dates are used internally as the number of days since 1899/12/30 but they are shown in a known date format, such as

* dmy: "01/01/2000"
* mdy: "12/25/2000"
* ymd: "2000/01/25"

### Exclude some dates from an analysis command

````
select born < createdate(1,1,2024) do tab agegroup gender;
````

### Calculate age on day of visit

Assume you have date of birth in variable `dob` and date of visit in `visit`

````
new var int agedays := visit - dob; // days
new var int ageyear := integer(agedays / 365.25); // years
````

### Calculate age on day of visit - simulation example

To see the example you can copy this code

````
close;
new project;
new dataset sim !size := 10;
new v dob d := createdate(15,1,(1930+5*_n));
new v visit d := createdate(15,_n,2007);
new v age i := integer((visit-dob) / 365.25);
new v age1 i := integer(round((visit-dob)/365.25,0));
new v age2 f := (visit-dob)/365.25;
list d;    
````

Notice that by rounding (`age1`), you get incorrect age, whereas truncating (`age`) always gives the correct value.

### Test if a string variable contains legal dates

You have read a text (delimited) file, and wish to convert a string variable to a date.
In this example, the `txtdate` is the string variable with dates as YYYY/MM/DD, `id` is id number.

````
read "myfile.csv";
list v; // to see the variables
// Now convert to date:
new v mydate d := createdate(txtdate,'ymd'); // Incorrect date values will be "."
count;
select (mydate=.) do count;
````

<a href=#toc>Back to contents</a>
## Linear regression with a factored variable

Epidata Analysis does not have a factored variable type, but it is easy to create dummy variables for a categorical variable that will work just like a factored variable in other software. Typically there would be only a few categories.

In the example, there are three categories to the string variable `q`, Education, with three categories. 

Here are are the three variables

```
Name Type    Length Decimal Label             Valuelabels               Missing
 y    Float   5      1       Happiness                                          
 x    Float   5      1       Income                                             
 q    String  1      0       Education         a = Less than high school        
                                               b = Completed high school        
                                               c = Completed college            
```

To include this variable in a linear regression, we require two dummy variables, `q1` and `q2`. For the lowest level of education, both dummy variables are zero. 
> Note: there are other ways to define the dummy variables, but that is beyond the scope of these examples. 

```
// create two dummy variables for q
new v q1 i := 0;
new v q2 i := 0;
select (q="b") do q1 := 1;
select (q="c") do q2 := 1;
// create variable labels
edit v q1 !label:="Completed HS";
edit v q2 !label:="Completed college";
// do the linear regression
regress y x q1 q2
```

Here is the output of the regression.

```
                                                         
                   Regression Analysis                   
              Dependent variable: Happiness              
               Model : y ~ c + x + q1 + q2               
                                                         
              Term Coefficient     s.e.         t       p
         Intercept     -0.6176   0.3123   -1.9775   0.057
            Income      1.0020   0.0004 2353.0770 <0.0001
      Completed HS      0.6584   0.3676    1.7911   0.083
 Completed college      0.4233   0.3684    1.1490   0.259
 
               Residual variance:  0.7555                
           R^2:  1.0000  Adjusted R^2:  1.0000           
           F:1877785.4077 on 3 and 32 df (p=0)           
```

<a href=#toc>Back to contents</a>
## Further analysis

Epidata Analysis is meant to provide quick analyses that are typical of epidemiological studies, especially in the field. Some commands are easier to use and simpler than those in typical statistical programs. For example, for an outbreak investigation you can quickly produce an epidemic curve and even a food-specific attack rate table.

```
read "samples/oswego.epx";
epicurve onsethr gender;
// compact table 
// !ar requests attack rates
// !sd sorts the exposure and outcome values in descending order to get O+ and E+ correct
// !ss sorts the table by the key statistic, which is RR in this table
ctable Ill Chicken-Peppers !sd !ss !ar;
```

Here is the output from ctable

```
                                    Ill                                    
                          O+ = Ill / O- = Not ill                          
────────────────────────────────────────────────────────────────────────────
 by                       E+      E+ E+      E- E-                         
 Var               N    value     O+ O-   AR O+ O-   AR    RR    95% CI    
────────────────────────────────────────────────────────────────────────────
 Meatballs        37 Ate the food 17  3 0.85  1 16 0.06 14.45 (2.14, 97.61)
 Chicken          37 Ate the food 17 12 0.59  1  7 0.13  4.69 (0.73, 30.09)
 CabbageRolls     37 Ate the food 12  3 0.80  6 16 0.27  2.93  (1.42, 6.07)
 Potatoes         37 Ate the food 14  8 0.64  4 11 0.27  2.39  (0.97, 5.85)
 Strawberries     37 Ate the food  9  2 0.82  9 17 0.35  2.36  (1.30, 4.29)
 Beverages        37 Ate the food 16 14 0.53  2  5 0.29  1.87  (0.55, 6.31)
 Cheeses          37 Ate the food 11  6 0.65  7 13 0.35  1.85  (0.92, 3.70)
 Sausage          37 Ate the food  7  4 0.64 11 15 0.42  1.50  (0.80, 2.83)
 MarshmallowSalad 37 Ate the food  2  1 0.67 16 18 0.47  1.42  (0.59, 3.40)
 BeanSalad        37 Ate the food  5  3 0.63 13 16 0.45  1.39  (0.71, 2.73)
 GardenSalad      37 Ate the food 11  9 0.55  7 10 0.41  1.34  (0.67, 2.67)
 Cake             37 Ate the food  7  5 0.58 11 14 0.44  1.33  (0.69, 2.54)
 PotatoSalad      37 Ate the food  8  6 0.57 10 13 0.43  1.31  (0.69, 2.52)
 PorkBeans        37 Ate the food  3  2 0.60 15 17 0.47  1.28  (0.57, 2.86)
 Ham              37 Ate the food  9  9 0.50  9 10 0.47  1.06  (0.54, 2.05)
 MacaroniSalad    37 Ate the food  1  1 0.50 17 18 0.49  1.03  (0.25, 4.29)
 SalmonSalad      37 Ate the food  1  1 0.50 17 18 0.49  1.03  (0.25, 4.29)
 Peppers          37 Ate the food  1  1 0.50 17 18 0.49  1.03  (0.25, 4.29)
 Dip              37 Ate the food  9 10 0.47  9  9 0.50  0.95  (0.49, 1.84)
 Mushrooms        37 Ate the food  2  3 0.40 16 16 0.50  0.80  (0.26, 2.47)
 Rolls            37 Ate the food  8 13 0.38 10  6 0.63  0.61  (0.31, 1.18)
 PastaSalad       37 Ate the food  4  8 0.33 14 11 0.56  0.60  (0.25, 1.42)
 CaesarSalad      37 Ate the food  5 10 0.33 13  9 0.59  0.56  (0.25, 1.25)
────────────────────────────────────────────────────────────────────────────
```

However, it is very easy to export your data for use in other software. The most common format to use is Stata and data can be easily exported.

```
// save data in the most-compatible Stata format
save "oswego.dta";
```
