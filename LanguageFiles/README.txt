EpiDataStat.exe is compiled including the translationsstrings in english. This makes distribution of EpiDataStat.exe simpler because a english.lang.txt file is not needed.

To add translationstrings to the english version do this:

1) Add the strings to the file english.lang.txt in the form
   code=string
   e.g. 10433=My string~in english %s

   %s is an exsample of standard Delphi formatting codes
   ~ will be changed to newline (#13) during the translation

2) Compile a ressourcefile (english.res) which is included in the exe-file by UTranslation
   by running Make_language_res_file.cmd

   NOTE: Make_language_res_file.cmd needs Delphi7 to be in this path:
   c:\programmer\borland\delphi7\

3) Make a new compilation of the project EpiDataStat.dpr

