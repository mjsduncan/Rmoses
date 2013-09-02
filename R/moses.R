#  wrapper/interface functions for moses binary
moses <- function( flags = "", DSVfile = "", output = TRUE) {
	if(flags == "help") {flags = "--help"; output = ""} else {
	if(DSVfile != "") flags = paste("--input-file", DSVfile, flags) else {flags = "--version"; output = ""}}
	system2("moses", args=flags, stdout = output)
}

# output parsing
library(stringr)

setwd("~/Desktop/moses sandbox")
combo.v <- moses("-j2 -W1 -u CLASS","IrisSetosa.txt")
> str_split_fixed(combo.v, " ", 2)
     [,1] [,2]                                                    
 [1,] "0"  "not(0<(+(*(-2 exp(*(-1 $PW))) $PW))) "                 
 [2,] "0"  "not(0<(+(*(-1 +(1 $PW) exp(*(-1 $PW))) $PW))) "        
 [3,] "-1" "not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) "                 
 [4,] "-1" "and(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) 0<($SL)) "    
 [5,] "-1" "and(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) 0<($SW)) "    
 [6,] "-1" "and(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) 0<($PL)) "    
 [7,] "-1" "and(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) 0<($PW)) "    
 [8,] "-1" "or(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) not(0<($SL))) "
 [9,] "-1" "or(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) not(0<($SW))) "
[10,] "-1" "or(not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) not(0<($PL))) "