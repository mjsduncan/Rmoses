# convert logical and arithmetic combo program to R function
library(stringr)
# quote translated operators in combo string

# ?? ("log", "exp", "floor", "mod"=%%)  operator translation may not be necessary
mc_ops <- list(c("\\$", "~"), c("and", "&"), c("or", "|"), c("not", "!"))

quote_op <- function(str) {
  str <- str_replace_all(str, mc_ops[[1]][1], mc_ops[[1]][2])
  str <- str_replace_all(str, mc_ops[[2]][1], mc_ops[[2]][2])                   # translate logical operators TODO: simplify
  str <- str_replace_all(str, mc_ops[[3]][1], mc_ops[[3]][2])
  str <- str_replace_all(str, mc_ops[[4]][1], mc_ops[[4]][2])
   str <- gsub("(0<\\()", "<\\(0 ", str)                                         # translate 0< operator
  str <- gsub("([<>=]=)","'\\1'", str)                                          # quote double character operators >=, <=, ==
  str <- gsub("([-+*/<>!&|])(?=[^=0123456789])", "'\\1'", str, perl = TRUE)  # quote single character operators except in above
  gsub(" ", ", ", str_trim(str))
}
var_list <- function(str) sapply(
  sapply(str_extract_all(quote_op(str), "~(\\w+)"), unique)
    , paste, collapse = ", ")

combo2exp <- function(str) {
  exp <- quote_op(str)
  paste("function (", var_list(str), ") ", exp, sep = "")
}


# # convert string to nested lists of doubles & triples
# 
# combo2exp <- function(str){
#   paste(                            # add external list function to string
#   'list(', gsub(                    # replace spaces with commas
#     "( \\()", " list\\(", gsub(                 # add internal list functions
#       "[ ]{1,}", ", ",quote_op(str)
#       )
#     , perl = TRUE), ')', sep=""
#   )
# }
#
#  eval(parse(text=combo2exp(str)))  # evaluate string to form list
#
# # TODO: convert character vector of combo strings to list of lists and produce vector of results
#
# # evaluate atomic list
# 
# eval_alist <- function(list){
#   if(length(list)==2) return(do.call(unlist(head(list, n=1)),tail(list, n=1)))  # evaluate unitary operator
#   do.call(unlist(head(list, n=1)),tail(list, n=2))                              # evaluate binary operator
# }
# 
# # recursively evaluate nested list
# 
# eval_list <- function(list){
#   if(!is.list(list)) return(list)
#   if(length(list) > 3) return(print("error: list length > 3"))
#   if(is.list(unlist(head(list)))) return(print("error: first element is not an operator"))
#   if(sum(sapply(list, is.list)) == 0) return(eval_alist(list))
#   list <- lapply(list, eval_list)
#   eval_list(list)
# }
# 
# # put them together to evaluate combo string
# 
# eval_combo <- function(str) eval_list(combo2list(str))

# TODO: construct R function

test1 <- "+ 5 9"                                               # 14
test2 <- "+ 4 (- (+ 6 3) (- (* 2 2) 1))"                       # 10
test3 <- "! (> 6 7)"                                           # TRUE
test4 <- "! (& (< 4 5) (| (> 2 0) (< 2 0)))"                   # FALSE
test5 <- "& (> (* 3 2) (- 10 5)) (& (>= 3 2) (== 3 (/ 6 2)))"  # TRUE
test <- c(test1, test2, test3, test4, test5)