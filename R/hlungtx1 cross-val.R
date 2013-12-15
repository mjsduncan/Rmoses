## noses n-fold validation
require(caret)
require(stringr)
setwd("~/Desktop/sandbox")

# start with dataset and set new working directory
hlungtx1 <- read.csv("~/Desktop/sandbox/hlungtx1_moses.csv")
rownames(hlungtx1) <- hlungtx1[,1]
hlungtx1[,1] <- NULL
setwd("~/Desktop/sandbox/hlungtx1_cross")

# create n partitions, write training partitions to moses files and construct dfs from test partitions
flds <- createDataPartition(as.factor(hlungtx1[,1]), times = 10, p = .8, list = TRUE)
write.csv(hlungtx1[flds[[1]],], "hlf1.csv", row.names = FALSE)
hlf1_v <- hlungtx1[-flds[[1]],]

# run moses on training files and extract combo strings & ranks
hlf1_r <- moses("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 500000 --enable-fs=1 -G 1 -f hlf1.log","hlf1.csv") 
hlf1.combo <- str_trim(str_split_fixed(hlf1_r, " ", 2)[,2])
hlf1.ranks <- as.numeric(str_split_fixed(hlf1_r, " ", 2)[,1])

# test results
attach(hlf1_v)
n <- length(hlf1_v[[1]])
m <- length(hlf1.combo)
hlf1.results <- matrix(nrow = m,ncol = n, dimnames = list(paste("C", 1:m, sep = ""), row.names(hlf1_v)))
  for(i in 1:m){
  hlf1.results[i,] <- evalstr(combo.edit(hlf1.combo[i]))
}
hlf1.results <- rbind(controls,hlf1.results)

# results are the same for all combo programs in hlf1
confusionMatrix(as.factor(hlf1.results[2,]), as.factor(hlf1.results[1,]), prev = .32)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction 0 1
#          0 3 1
#          1 0 5
#                                           
#                Accuracy : 0.8889          
#                  95% CI : (0.5175, 0.9972)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.1431          
#                                           
#                   Kappa : 0.7692          
#  Mcnemar's Test P-Value : 1.0000          
#                                           
#             Sensitivity : 1.0000          
#             Specificity : 0.8333          
#          Pos Pred Value : 0.7385          
#          Neg Pred Value : 1.0000          
#              Prevalence : 0.3200          
#          Detection Rate : 0.3333          
#    Detection Prevalence : 0.4444          
#                                           
#        'Positive' Class : 0               

# extract variable names from combo strings
hlf1.probes <- str_replace_all(hlf1.combo, "and+", "")
hlf1.probes <- str_replace_all(hlf1.probes, "or+", "")
hlf1.probes <- str_replace_all(hlf1.probes, "[()$!]+", "")
hlf1.probes <- str_trim(hlf1.probes)
hlf1.probes <- str_split(hlf1.probes, pattern = " ")

### functions for turning combo string to R string and evaluating
# translate logical operators
combo.edit <- function(str) {
   mc_ops <- list(c("(", "(list("), c(")", "))"), c(" ", ", "),c("$", ""))
   for(i in seq_along(mc_ops)) str <- str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2])
   str
}

# evaluate string
evalstr <- function(str){
  eval(parse(text=str))
}
# multi element boolean functions
and <- function(x) Reduce("&", x)
or <- function(x) Reduce("|", x)

hlf1_r <-
c("0 and(or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$NM_207340_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "                   
, "0 and(or(and(or(!$NM_207293_at !$NM_207340_at) or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "
, "0 and(or(and(or(!$NM_207340_at $XR_000538_at) or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) " 
, "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) "                                      
, "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) "                                      
, "-1 or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) $NM_024628_at $XR_001400_at) "                                       
, "-1 and(or(and(or($XR_000297_at !$XR_000538_at) !$NM_001040022_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_007175_at $NM_024628_at) $NM_004486_at) "                                 
, "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) !$NM_001040022_at) "                                 
, "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_004486_at $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) $NM_024628_at $XR_001400_at) !$NM_001040022_at) "                                  
, "-1 and(or(and(or($XR_000297_at !$XR_000538_at) $NM_006629_at $NM_024610_at $NM_024635_at !$XM_933006_at) !$NM_004733_at $NM_024628_at) !$NM_001040022_at $NM_004486_at) ")

combo.v1m <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")
combo.sa <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1 -m 1000000 ","hlungtx1_moses.csv")   # run moses on 2 cores sa algo


# #######  combo -> R failed attempts
# 
# # convert lisp string to nested lists.
# combo2lstr <- function(str){ paste("list",
#   gsub("( \\()", " list\\(", gsub("[ ]{1,}", ", ",combo.edit(str)), perl = TRUE),
#   sep = "")
# }
# # parse lisp list to r list
# is.int <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# #
# lisp2r <- function(lst){
#   if(class(lst) != "list") return(lst)
#   rlst <- vector("list", 2 * length(lst) - 3)
#   for(i in seq_along(rlst)){
#     if(!is.int(i/2)) rlst[[i]] <- tail(lst, -1)[[(i + 1)/2]]
#     else rlst[[i]] <- lst[[1]]
#   }
# return(rlst)
# }
# 
# # eval 
# combo_eval <- function(cstr){
#   rstr <- combo2lstr(cstr)
#   n <- str_count(rstr, fixed("list"))
#     lst <- evalstr(rstr)
#     for(j in seq(1, n)){
#       lst <- lapply(lst, lisp2r)
#       lst <- eval(parse(text = rstr))
#     }
#   return(toString(lst))
# }
