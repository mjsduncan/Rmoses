### My: run moses on wellfem vs superC 20 times.

#Run this from the folder written after wellf-vs-sc
#working_dir = "/home/opencog/eddy/Bio/wellf-vs-sc/20_permutations_5_in_metarun/"
working_dir = "~/eddy/docs/Bio/wellf-vs-sc/"



source("../moses2.R")  # my code
wvs_labeled <- read.csv("../wvsmoses.csv") #my code
wvs_untouched = wvs_labeled[,-1] #my code

rowid = seq(1:248)

              
              
for(iji in 3:20)
{
  dir.create(paste("",iji,"",sep="_"))
  setwd(paste(working_dir,iji,"/",sep="_")) # my code
  rowid_random = sample(rowid,length(rowid))
  #print(length(rowid_random))

  wvs = wvs_untouched[rowid_random,]
  wvs[1:231,1] = 0
  wvs[232:248,1] = 1
  
  wfVsc_test <- makeMpartitions(wvs, p = .8, k=5)
  wfVsc_out <- runMfolder("-j8 --balance 1 -m 50000 -W 1 --output-cscore 1 --result-count 100")
  print("moses training ended")
  save.image(paste("moses_runs_",iji,".Rdata",sep="_")) #my code
  
  
  wfVsc_combos <- moses2combo(wfVsc_out)
  wfVsc_parse <- parseMout(wfVsc_out)
  
  # replace "-" with "." to prevent error
  for(i in 1:10) colnames(wfVsc_test$train[[i]]) <- gsub("-", ".", colnames(wfVsc_test$train[[i]]), fixed = TRUE)
  for(i in 1:10) colnames(wfVsc_test$test[[i]]) <- gsub("-", ".", colnames(wfVsc_test$test[[i]]), fixed = TRUE)
  
  wfVsc_eval <- testClist(wfVsc_combos$combo, wfVsc_test)
  
  # combine results & get aggregate performance
  wfVsc_agg <- aggResults(wfVsc_eval)
  
  # generate reports
  # gene descriptions
  wfVsc_feat <- combo2fcount(wfVsc_combos$combo)
  
  save(wfVsc_agg, wfVsc_feat, file = paste("wellfemvssuperC_", i, "_best.rdata", sep = ""))
  save.image(paste("moses_runs_",iji,"_2.Rdata",sep="_")) #my code
  
  setwd("..")  
  
  
}

