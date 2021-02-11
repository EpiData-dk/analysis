#/bin/bash

lazbuild -B epidataanalysis.lpi
strip epidataanalysis

lazbuild -B --bm=win64 epidataanalysis.lpi
strip epidataanalysis.exe

