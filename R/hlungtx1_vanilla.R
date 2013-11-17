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

# library("ggplot2")
# library("reshape2")
library("lattice")
hc.m100k <- read.csv("~/GitHub/Rmoses/results/hc-m100000.csv")
# hc.m100k.melt <- melt(hc.m100k, ID = )

xyplot(total_evals ~ time, data = hc.m100k)
xyplot(deme_count + total_steps + num_evals/10 + new_instances/10 ~ time, data = hc.m100k, type = "l")
xyplot(deme_count/5 + (16 + best_raw) + complexity ~ total_evals, data = hc.m100k, type = "l")

