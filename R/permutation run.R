#ben#
#If k is the number of permutations, 5 permutations is not enough ... 20 is better...

#MIKE#
#it might be worth doing one meta-run of 10 permutations on
#a permuted data set on each for the 2 versions to see which is faster and to confirm
# that 20 runs per meta-run is the minimum to get good (bad) results.
#ben is probably right but if we can get away with fewer runs per meta-run we will save a lot of time!

# change these to match your machine
working_dir = "/mnt/biodata/sf2_data/meseret runs/mahlet/perm/"
source("~/R/Rmoses/R/moses2.R")

# set permutation run parameters
setwd(working_dir)
input <- read.csv("scVwellfem_variants.csv")
perms <- 3
training_fraction <- 0.8
cross_num <- 2
moses_params <- "-j8 --balance 1 -m 5000 -W 1 --output-cscore 1 --result-count 100"
# moses_params <- "-j8 --balance 1 -m 100000 -W 1 --output-cscore 1 --result-count 100 --reduct-knob-building-effort=1 --hc-widen-search=1 --enable-fs=1 --fs-algo=smd --fs-target-size=4 --hc-crossover-min-neighbors=5000 --fs-focus=all --fs-seed=init --complexity-ratio=3 --hc-max-nn-evals=10000 --hc-crossover-pop-size=1000 -l debug"

# get baseline score
make.dir("input")

# run moses
input_data <- makeMpartitions(input, p = training_fraction, k = cross_num)
input_out <- runMfolder(moses_params)
print(paste("input run complete"))

# get scores
input_combos <- moses2combo(input_out)
input_eval <- testClist(input_combos$combo, input_data)
save(input_eval, file = "input_eval.rdata")
setwd("..")


# make list to save permutation vectors
case = input[[1]]
rowperms <-vector("list", perms)
perm_base <-input[, -1]
for(i in seq_len(perms))
{
  # use "make.dir" defined in moses2.R
  make.dir(paste("perm", i, sep="_"))

  rowperms[[i]] <- sample(case)
  perm_input = cbind(case = rowperms[[i]], perm_base)
  print(rowperms[[i]])
  # run moses
  perm_data <- makeMpartitions(perm_input, p = training_fraction, k = cross_num)
  perm_out <- runMfolder(moses_params)
  print(paste("permutation run", i, "complete"))

  # get scores
  perm_combos <- moses2combo(perm_out)
  perm_eval <- testClist(perm_combos$combo, perm_data)
  save(perm_eval, file = paste("perm_", i, "_eval.rdata", sep = ""))

  setwd("..")
}

