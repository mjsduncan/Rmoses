### script for running moses through R in a docker container
source("/root/Rmoses/R/moses2.R")
wvsvar <- read.csv("scVwellfem_variants.csv", stringsAsFactors=FALSE)
wvsvar_data <- makeMpartitions(wvsvar, p = .5)
save(wvsvar_data(file = "wvsVar1data.rdata")
wvsvar_out <- runMfolder("-j8 --balance 1 -m 1000000 -W 1 --output-cscore 1 --result-count 100 --reduct-knob-building-effort=1 --hc-widen-search=1 --enable-fs=1 --fs-algo=smd --fs-target-size=4 --hc-crossover-min-neighbors=5000 --fs-focus=all --fs-seed=init --complexity-ratio=1 --hc-max-nn-evals=10000 --hc-crossover-pop-size=1000 -l debug")

