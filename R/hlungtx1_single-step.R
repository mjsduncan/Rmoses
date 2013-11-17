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

`hc-ss-100k` <- read.csv("~/GitHub/Rmoses/results/hc-ss-m100000.csv")

library("lattice")
xyplot(total_evals ~ time, data = `hc-ss-100k`)
xyplot(deme_count + total_steps + num_evals/10 + new_instances/10 ~ time, data = `hc-ss-100k`, type = "l")
xyplot(deme_count/5 + (16 + best_raw) + complexity ~ total_evals, data = `hc-ss-100k`, type = "l")

