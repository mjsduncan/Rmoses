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

