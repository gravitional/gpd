#!/usr/bin/env bash

declare -a lbdLst=("0.75" "0.80" "0.90" "1.00")

for lbd in ${lbdLst[@]}; do
    wolframscript -script ff.numeric-eval.wl --ord '\$ord0' --Lbd-num $lbd --Lbd-fit $lbd 
done

#declare -a lbdLst=("0.80" "1.00")
#
#for lbd in ${lbdLst[@]}; do
#    wolframscript -script ff.numeric-eval.wl --ord '\$ord0' --Lbd-num $lbd --Lbd-fit '0.90' 
#done
