#/bin/bash

# Programs
GOLD=/home/torsten/.PlayOnLinux/wineprefix/GOLD/drive_c/Program\ Files/GOLD\ Parser\ Builder/GOLDbuild.exe
BINOBJ=$(which bin2obj)

GRAMMAR=EpiDataAnalysisGrammar.grm
CGTFILE=EpiDataAnalysisGrammar.cgt
INCFILE=EpiDataAnalysisGrammar.inc
CONST=EpiDataAnalysisGrammar

WINGRAMMAR=$(winepath -w $GRAMMAR)

wine "$GOLD" $WINGRAMMAR $CGTFILE

$BINOBJ -o $INCFILE -c $CONST $CGTFILE


