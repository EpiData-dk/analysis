#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling epicurve
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.s
if [ $? != 0 ]; then DoExitAsm epicurve; fi
rm /Users/jamie/ed/analysis/target/units/x86_64-darwin/epicurve.s
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
