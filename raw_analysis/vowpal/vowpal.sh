#!/bin/sh

export PATH=$PATH:$HOME/workspaces/r-workspace/vowpal_wabbit/vowpalwabbit/

# avg loss = 0.156352
vw -d data/Training.vw -f data/model.vw --binary --loss_function logistic --adaptive --normalized --passes 20 -b 24 -c -q ff --boosting 50 --bootstrap 10 --bs_type vote --top 25

vw -t -d data/Testing.vw -i model.vw -p data/predictions.txt --binary

> solution.csv
tail -n +2 Testing.csv | while read line1 && read -u 3 line2; do 
    echo ${line1: -16:-4},$([ ${line2: 0:2} == -1 ] && echo 0 || echo 1) >> solution.csv
done 3< predictions.txt

