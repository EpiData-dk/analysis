#/bin/bash

# Programs
WINE_PREFIX="/home/torsten/.winegui/prefixes/Gold Parser/"
GOLD="${WINE_PREFIX}/drive_c/GOLD-Builder-5.2.0-Cmd/GOLDbuild.exe"
BINOBJ=$(which bin2obj)

GRAMMAR=EpiDataAnalysisGrammar.grm
CGTFILE=EpiDataAnalysisGrammar.cgt
INCFILE=EpiDataAnalysisGrammar.inc
CONST=EpiDataAnalysisGrammar

WINGRAMMAR=$(winepath -w $GRAMMAR)

wine "$GOLD" +v1 $WINGRAMMAR $CGTFILE

$BINOBJ -o $INCFILE -c $CONST $CGTFILE


