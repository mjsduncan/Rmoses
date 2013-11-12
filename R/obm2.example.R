###  moses applied to hlungtx1_moses.csv

# output parsing
library(stringr)

setwd("~/Desktop/moses sandbox")
combo.v <- moses("-j2 -W1 -u controls -Y sample --hc-crossover=1 --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores 
# same result with w/o hc-crossover!
combo.v <- c(
  "-6 and(or($NM_006163_at !$XM_933006_at) or($XR_000222_at !$XR_000538_at) $NM_004486_at $NM_023930_at) "                    
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at) "               
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946269_at) "
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946270_at) "
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946278_at) " 
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946339_at) " 
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946340_at) " 
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946372_at) "
 , "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XR_000529_at) "
, "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XR_000541_at) ")

combo.vm <- moses("-j2 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores 
combo.vm <- c(
  "-1 and(or(and(or(and(!$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at) $XR_000970_at) or($NM_023930_at !$XM_933569_at) or($XM_933569_at $XM_944663_at) !$NM_057089_at) "
 , "-2 and(or(and($NM_001038628_at $NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at $XR_000970_at) $NM_023930_at !$NM_057089_at) "                                                            
 , "-2 and(or(and($NM_001038628_at !$XM_933006_at !$XR_000538_at) $NM_025103_at $XR_000970_at) $NM_004486_at $NM_023930_at !$NM_057089_at) "                                                            
 , "-2 or(and(or(and($NM_004486_at !$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at) $XR_000970_at) "                                                        
 , "-2 or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at !$NM_057089_at) $XR_000970_at) "                                                        
 , "-2 or(and(or(and(!$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at $NM_023930_at) $XR_000970_at) "                                                        
 , "-2 or(and(or(and(!$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at $NM_023930_at !$NM_057089_at) $XR_000970_at) "                                                        
 , "-2 and(or(and(or(and($NM_004486_at !$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at) $XR_000970_at) $NM_023930_at) "                                                   
 , "-2 and(or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at) $XR_000970_at) $NM_023930_at !$NM_057089_at) "                                                   
, "-2 and(or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at) $XR_000970_at) !$NM_057089_at)")

is.control <- combo2exp(is.setosa)                        # translate combo to R

# run functions on dataset
hlungtx1 <- read.delim("hlungtx1_moses.csv")
attach(hlungtx1)

#  compare classification from data set with evaluation
#  the highest ranked program has a perfect score:
hlungtx1$control == eval(parse(text = str_replace_all(combo2exp(is.control)[1], "~", "")))

#####  --hc-widen-search=1 returns TRUE or single feature with score -16 with or without  --hc-single-step=1

combo.v1 <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 --enable-fs=1","hlungtx1_moses.csv")
combo.v1 <- c(
  "-7 or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) or($NM_201539_at !$NM_201552_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) "                    
 , "-7 and(or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "               
 , "-7 and(or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at) $NM_004486_at) "               
 , "-7 and(or(and(or(and($NM_003040_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) " 
 , "-7 and(or(and(or(and(!$NM_003048_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 , "-7 and(or(and(or(and(!$NM_003049_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 , "-7 and(or(and(or(and(!$NM_003055_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 , "-7 and(or(and(or(and(!$NM_003056_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 , "-7 and(or(and(or(and(!$NM_003057_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
, "-7 and(or(and(or(and($NM_003068_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) " )

combo.v1m <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")
combo.v1m <- c(
  "-1 and(or(and(or($NM_003062_at !$NM_201552_at) or($NM_201443_at $NM_201541_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at !$NM_013339_at) !$NM_022358_at) "               
 , "-1 and(or(and(or($NM_003062_at !$NM_201552_at) or($NM_201443_at $NM_201541_at) !$XM_933006_at !$XR_000645_at) $NM_000804_at !$NM_013339_at) $NM_004486_at $NM_006187_at !$NM_022358_at) "               
 , "-1 or(and(or(and(or($NM_003062_at !$NM_201552_at) or($NM_201443_at $NM_201541_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) !$NM_013339_at) !$NM_022358_at) $NM_000804_at) "           
 , "-1 and(or(and(or(and(or($NM_201443_at $NM_201541_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at) or($NM_003062_at !$NM_201552_at)) !$NM_013339_at) !$NM_022358_at) "      
 , "-1 and(or(and(or(and(or($NM_201443_at $NM_201541_at) $NM_004486_at !$XM_933006_at !$XR_000645_at) $NM_000804_at) or($NM_003062_at !$NM_201552_at) $NM_006187_at) !$NM_013339_at) !$NM_022358_at) "      
 , "-1 and(or(and(or(and(or($NM_201443_at $NM_201541_at) $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at) or($NM_003062_at !$NM_201552_at) $NM_004486_at) !$NM_013339_at) !$NM_022358_at) "      
 , "-1 and(or(and(or(and(or($NM_201443_at $NM_201541_at) !$XM_933006_at !$XR_000645_at) $NM_000804_at) or($NM_003062_at !$NM_201552_at)) !$NM_013339_at) $NM_004486_at $NM_006187_at !$NM_022358_at) "      
 , "-1 and(or(and(or(!$NM_000317_at $NM_003062_at !$NM_201552_at) or($NM_201443_at $NM_201541_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at !$NM_013339_at) !$NM_022358_at) "
 , "-1 and(or(and(or(!$NM_000317_at $NM_201443_at $NM_201541_at) or($NM_003062_at !$NM_201552_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at !$NM_013339_at) !$NM_022358_at) "
, "-1 and(or(and(or(!$NM_000318_at $NM_003062_at !$NM_201552_at) or($NM_201443_at $NM_201541_at) $NM_004486_at $NM_006187_at !$XM_933006_at !$XR_000645_at) $NM_000804_at !$NM_013339_at) !$NM_022358_at) ")

combo.v2 <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores sa algo
combo.v2 <- c(
  "-15 $NM_014753_at " ,                    "-15 and($NM_014753_at $NM_014754_at) "  ,"-15 and($NM_014753_at $NM_014755_at) " 
 , "-15 and($NM_014753_at $NM_014756_at) " , "-15 and($NM_014753_at $NM_014757_at) " , "-15 and($NM_014753_at $NM_014758_at) " 
 , "-15 and($NM_014753_at !$NM_014772_at) ", "-15 and($NM_014753_at $NM_014773_at) ",  "-15 and($NM_014753_at $NM_014774_at) " 
, "-15 and($NM_014753_at !$NM_014775_at) ")

combo.v3 <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1 -m 100000","hlungtx1_moses.csv")   # run moses on 2 cores sa algo
combo.v3 <- c(
  "-6 or(and(!$NM_001632_at !$XR_000645_at) !$NM_000190_at !$NM_006456_at $XR_000963_at) "                        
 , "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "                    
 , "-6 or(and(or(!$NM_000190_at !$NM_006456_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "     
 , "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_000190_at !$NM_006456_at $XR_000963_at) "     
 , "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at !$NM_006713_at $XR_000963_at) "     
 , "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at $NM_006713_at) !$NM_006456_at $XR_000963_at) "      
 , "-6 or(and(or(!$NM_000190_at !$XR_000645_at $XR_000963_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "      
 , "-6 and(or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) $NM_006713_at) " 
 , "-6 or(and(or(and(!$NM_000167_at !$NM_000190_at) !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "
, "-6 or(and(or(and(!$NM_000190_at !$NM_000607_at) !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) ")

## extract samples

combo.all <- c(combo.v, combo.vm, combo.v1, combo.v1m, combo.v3, combo.v2)      # put all moses combo results in a vector
hlungtx1.combo <- str_split_fixed(combo.all, " ", 2)[,2]                        # extract combo program strings
hlungtx1.ranks <- as.numeric(str_split_fixed(combo.all, " ", 2)[,1])            # extract combo program scores
hlungtx1.probes <- str_replace_all(hlungtx1.combo, "and+", "")            ##
hlungtx1.probes <- str_replace_all(hlungtx1.probes, "or+", "")             #
hlungtx1.probes <- str_replace_all(hlungtx1.probes, "[()$]+", "")           ##  reduce combo program to string of probes
hlungtx1.probes <- str_trim(hlungtx1.probes)                              ##
hlungtx1.moseslist <- str_split(hlungtx1.probes, pattern = " ")              #  make list of vectors of probes
#  dataframe of all probe instances from list
hlungtx1.ranks <- rep(hlungtx1.ranks, sapply(hlungtx1.moseslist, length))
hlungtx1.probes <- data.frame(probe = unlist(hlungtx1.moseslist), score = hlungtx1.ranks, stringsAsFactors = FALSE)                
# create "not" variable to indicate controls correlated with low expression
hlungtx1.probes$con_low <- str_detect(hlungtx1.probes$probe, "!")
hlungtx1.probes$probe <- str_replace(hlungtx1.probes$probe, "!", "")
hlungtx1.probes <- unique(hlungtx1.probes[order(hlungtx1.probes$score, decreasing = TRUE),][, c(1,3)])
hlungtx1.probes$score <- hlungtx1.ranks[as.numeric(row.names(hlungtx1.probes))]
  
  ## alternate way to pick it-ems out of perens (not used)
# re <- "\\(([^()]+)\\)"
# gsub(re, "\\1", str_extract_all(combo.all, re)[[2]])
