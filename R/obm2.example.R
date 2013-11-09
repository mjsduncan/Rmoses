###  moses applied to hlungtx1_moses.csv

# output parsing
library(stringr)

setwd("~/Desktop/moses sandbox")
combo.v <- moses("-j2 -W1 -u controls -Y sample --hc-crossover=1 --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores 
# same result with w/o hc-crossover!
> combo.v
 [1] "-6 and(or($NM_006163_at !$XM_933006_at) or($XR_000222_at !$XR_000538_at) $NM_004486_at $NM_023930_at) "                    
 [2] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at) "               
 [3] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946269_at) "
 [4] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946270_at) "
 [5] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946278_at) " 
 [6] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946339_at) " 
 [7] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at $XM_946340_at) " 
 [8] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XM_946372_at) "
 [9] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XR_000529_at) "
[10] "-6 and(or(and($NM_004486_at !$XM_933006_at) $NM_006163_at) or($XR_000222_at !$XR_000538_at) $NM_023930_at !$XR_000541_at) "

combo.vm <- moses("-j2 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores 
> combo.vm
 [1] "-1 and(or(and(or(and(!$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at) $XR_000970_at) or($NM_023930_at !$XM_933569_at) or($XM_933569_at $XM_944663_at) !$NM_057089_at) "
 [2] "-2 and(or(and($NM_001038628_at $NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at $XR_000970_at) $NM_023930_at !$NM_057089_at) "                                                            
 [3] "-2 and(or(and($NM_001038628_at !$XM_933006_at !$XR_000538_at) $NM_025103_at $XR_000970_at) $NM_004486_at $NM_023930_at !$NM_057089_at) "                                                            
 [4] "-2 or(and(or(and($NM_004486_at !$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at) $XR_000970_at) "                                                        
 [5] "-2 or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at !$NM_057089_at) $XR_000970_at) "                                                        
 [6] "-2 or(and(or(and(!$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at $NM_023930_at) $XR_000970_at) "                                                        
 [7] "-2 or(and(or(and(!$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_004486_at $NM_023930_at !$NM_057089_at) $XR_000970_at) "                                                        
 [8] "-2 and(or(and(or(and($NM_004486_at !$NM_057089_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at) $XR_000970_at) $NM_023930_at) "                                                   
 [9] "-2 and(or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at) $XR_000970_at) $NM_023930_at !$NM_057089_at) "                                                   
[10] "-2 and(or(and(or(and($NM_004486_at !$XM_933006_at !$XR_000538_at) $NM_025103_at) $NM_001038628_at $NM_023930_at) $XR_000970_at) !$NM_057089_at)"

hlungtx1.combo <- str_split_fixed(combo.v, " ", 2)[,2]                        # extract combo program string
is.control <- combo2exp(is.setosa)                        # translate combo to R

# run functions on dataset
hlungtx1 <- read.delim("hlungtx1_moses.csv")
attach(hlungtx1)

#  compare classification from data set with evaluation
#  the highest ranked program has a perfect score:
hlungtx1$control == eval(parse(text = str_replace_all(combo2exp(is.control)[1], "~", "")))

#####  --hc-widen-search=1 returns TRUE or single feature with score -16 with or without  --hc-single-step=1

combo.v1 <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 --enable-fs=1","hlungtx1_moses.csv")
> combo.v1
 [1] "-7 or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) or($NM_201539_at !$NM_201552_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) "                    
 [2] "-7 and(or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "               
 [3] "-7 and(or(and(or(and(!$NM_005529_at $NM_201443_at) $NM_201541_at) !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at) $NM_004486_at) "               
 [4] "-7 and(or(and(or(and($NM_003040_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) " 
 [5] "-7 and(or(and(or(and(!$NM_003048_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 [6] "-7 and(or(and(or(and(!$NM_003049_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 [7] "-7 and(or(and(or(and(!$NM_003055_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 [8] "-7 and(or(and(or(and(!$NM_003056_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
 [9] "-7 and(or(and(or(and(!$NM_003057_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) "
[10] "-7 and(or(and(or(and($NM_003068_at !$NM_005529_at $NM_201443_at) $NM_201541_at) $NM_004486_at !$XM_933006_at) $NM_000804_at) or($NM_201539_at !$NM_201552_at)) " 
combo.v2 <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1","hlungtx1_moses.csv")   # run moses on 2 cores sa algo
> combo.v2
 [1] "-15 $NM_014753_at "                     "-15 and($NM_014753_at $NM_014754_at) "  "-15 and($NM_014753_at $NM_014755_at) " 
 [4] "-15 and($NM_014753_at $NM_014756_at) "  "-15 and($NM_014753_at $NM_014757_at) "  "-15 and($NM_014753_at $NM_014758_at) " 
 [7] "-15 and($NM_014753_at !$NM_014772_at) " "-15 and($NM_014753_at $NM_014773_at) "  "-15 and($NM_014753_at $NM_014774_at) " 
[10] "-15 and($NM_014753_at !$NM_014775_at) "

combo.v3 <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1 -m 100000","hlungtx1_moses.csv")   # run moses on 2 cores sa algo
> combo.v3
 [1] "-6 or(and(!$NM_001632_at !$XR_000645_at) !$NM_000190_at !$NM_006456_at $XR_000963_at) "                        
 [2] "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "                    
 [3] "-6 or(and(or(!$NM_000190_at !$NM_006456_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "     
 [4] "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_000190_at !$NM_006456_at $XR_000963_at) "     
 [5] "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at !$NM_006713_at $XR_000963_at) "     
 [6] "-6 or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at $NM_006713_at) !$NM_006456_at $XR_000963_at) "      
 [7] "-6 or(and(or(!$NM_000190_at !$XR_000645_at $XR_000963_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "      
 [8] "-6 and(or(and(or(!$NM_000190_at !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) $NM_006713_at) " 
 [9] "-6 or(and(or(and(!$NM_000167_at !$NM_000190_at) !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "
[10] "-6 or(and(or(and(!$NM_000190_at !$NM_000607_at) !$XR_000645_at) !$NM_001632_at) !$NM_006456_at $XR_000963_at) "