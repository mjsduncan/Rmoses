### script to do multiple meta-runs with new seeds and tuning parameters
# set start directory
setwd("/mnt/biodata/svm_check2/basic")
source("~/R/Rmoses/R/moses2.R")
# make feature names for for running boolean regression with > 2 outcome/category values
feature_folders <- paste0("feature", c(9, 10, 11, 23, 24, 25)) #

# set meta-run parameters.  name will have meta-run number appended. each run has a new seed
run_prefix <- "check_"
run_count <- 10
run_name <- paste0(run_prefix, seq_len(run_count))
seeds <- sample(1000:9999, run_count)


# these are parameters used for all runs
moses_params <- "-j8 --balance 1 -m 100000 -W 1 --output-cscore 1 --result-count 100 --enable-fs=1 --fs-algo=simple --fs-seed=init --fs-target-size=5 --complexity-ratio=2 -l DEBUG"

# a list of vectors of parameter values named with the parameter flag name (without the "-" or "--" prefix):
# var_params <- list(complexity-ratio = c(2, 3.5, 5))

# cross-validation parameters
training_fraction <- 0.5
cross_num <- 2

# loop through the runs
for(j in seq_len(run_count)){

  # loop through folders
  for(i in seq_len(length(feature_folders)))
  {
    # use "make.dir" defined in moses2.R
    # this sassumes input data is csv file with same name as feature_folder + .csv
    setwd(feature_folders[i])
    print("loading moses input data...")
    if(!identical(list.files(pattern = "rdata", ignore.case = TRUE), character(0))) load(list.files(pattern = "rdata")) else
      {feature_input <- read.csv(paste0(feature_folders[i], ".csv")); print("saving csv file as rdata file...")
       save(feature_input, file = paste0(feature_folders[i], ".rdata"))}
    make.dir(run_name[j])

    # run moses
    feature_data <- makeMpartitions(feature_input, p = training_fraction, k = cross_num)
    rm(feature_input, feature_data)
    for (k in 1:10) gc(reset = T)
    feature_out <- runMfolder(paste(moses_params, "-r", seeds[j]))
    print(paste0(feature_folders[i], ", meta-run ", j, " is complete"))

    # get scores
    feature_data <- readRDS("feature_input.rds")
    feature_combos <- moses2combo(feature_out)
    saveRDS(feature_combos, file = paste0(feature_folders[[i]], "_combos_", run_name[j], ".rds"))
    # feature_eval <- testClist(feature_combos$combo, feature_data)
    # save(feature_eval, file = paste0(feature_folders[i], "_", run_name[j], "_eval.rdata"))

    setwd("../..")
    rm(feature_data, feature_combos, feature_out) # , feature_eval
    for (k in 1:10) gc(reset = T)

  }

}

