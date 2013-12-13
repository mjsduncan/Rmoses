## practice n-fold validation
require(caret)
require(stringr)
setwd("~/Desktop/sandbox")
hlungtx1 <- read.csv("~/Desktop/sandbox/hlungtx1_moses.csv")
rownames(hlungtx1) <- hlungtx1[,1]
hlungtx1[,1] <- NULL
setwd("~/Desktop/sandbox/hlungtx1_cross")

flds <- createDataPartition(as.factor(hlungtx1[,1]), times = 10, p = .8, list = TRUE)
write.csv(hlungtx1[flds[[1]],], "hlf1.csv", row.names = FALSE)
hlf1_v <- hlungtx1[-flds[[1]],]
hlf1_r <- moses("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 500000 --enable-fs=1 -G 1 -f hlf1.log","hlf1.csv")   # run moses on 2 cores 
hlf1.combo <- str_split_fixed(hlf1_r, " ", 2)[,2]                        # extract combo program strings
hlf1.ranks <- as.numeric(str_split_fixed(hlf1_r, " ", 2)[,1])            # extract combo program scores

> hlf1_r
[1] "0 and(or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$NM_207340_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "                   
[2] "0 and(or(and(or(!$NM_207293_at !$NM_207340_at) or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "
[3] "0 and(or(and(or(!$NM_207340_at $XR_000538_at) or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) " 
[4] "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) "                                      
[5] "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) "                                      
[6] "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) $NM_024628_at $XR_001400_at) "                                       
[7] "-1 and(or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "                                 
[8] "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) !$NM_001040022_at) "                                 
[9] "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) $NM_024628_at $XR_001400_at) !$NM_001040022_at) "                                  
[10] "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) !$NM_001040022_at $NM_004486_at) "                                 


combo.v1m <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")
combo.sa <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1 -m 1000000 ","hlungtx1_moses.csv")   # run moses on 2 cores sa algo


#######  combo -> R ii
# turn combo to lisp string
combo.edit <- function(str) {
  mc_ops <- list(c("and(", "('&' "), c("or(", "('|' "), c("$", ""))
  for(i in seq_along(mc_ops)) str <- str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2])                   # translate logical operators
  str_trim(str)
}
# convert lisp string to nested lists.
combo2list <- function(str){ paste("list",
  gsub("( \\()", " list\\(", gsub("[ ]{1,}", ", ",str), perl = TRUE),
  sep = "")
}
# parse lisp list to r list
is.int <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
#
lisp2r <- function(lst){
  if(class(lst) != "list") return(lst)
  rlst <- vector("list", 2 * length(lst) - 3)
  for(i in seq_along(rlst)){
    if(!is.int(i/2)) rlst[[i]] <- tail(lst, -1)[[(i + 1)/2]]
    else rlst[[i]] <- lst[[1]]
  }
return(rlst)
}

combo_eval <- function(list){
  state <- sapply(list, class)# if list is fully parsed to atomic vectors then return
  if(str_detect(str_c(state, collapse = "TRUE"), fixed("list")))
    return(toString(list))
  list <- lapply(list, lisp2r)
  list <- combo_eval(list)
}

eval(parse(text=combo2list(str)))  # evaluate string to form list
