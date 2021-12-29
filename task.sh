#!/usr/bin/env bash

#declare -a lbdLst=("0.80" "0.90" "1.00")
#
#for lbd in ${lbdLst[@]}; do
#    wolframscript -script ff.numeric-eval.wl --lbd-num $lbd --lbd-fit $lbd &>~/test/full-num-$lbd-fit-$lbd.log
#done

declare -a lbdLst=("0.80" "1.00")

for lbd in ${lbdLst[@]}; do
    wolframscript -script ff.numeric-eval.wl --lbd-num $lbd --lbd-fit '0.90' &>~/test/full-num-$lbd-fit-'0.90'.log
done
