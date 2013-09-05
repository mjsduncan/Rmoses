# iris dataset example

# output parsing
library(stringr)

setwd("~/Desktop/moses sandbox")
combo.v <- moses("-j2 -W1 -u CLASS","IrisSetosa.txt")                    # run moses on subset of classic "iris data set
is.setosa <- str_split_fixed(combo.v, " ", 2)[,2]                        # extract combo program string

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

is.setosa <- combo2exp(is.setosa)                        # translate combo to R
 [1] "'!'('<'(0, '+'('*'(-2, exp('*'(-1, ~PW))), ~PW)))"                       
 [2] "'!'('<'(0, '+'('*'(-1, '+'(1, ~PW), exp('*'(-1, ~PW))), ~PW)))"          
 [3] "'!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW)))"                       
 [4] "'&'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '<'(0, ~SL))"     
 [5] "'&'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '<'(0, ~SW))"     
 [6] "'&'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '<'(0, ~PL))"     
 [7] "'&'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '<'(0, ~PW))"     
 [8] "'|'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '!'('<'(0, ~SL)))"
 [9] "'|'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '!'('<'(0, ~SW)))"
[10] "'|'('!'('<'(0, '+'('*'(-1, exp('*'(-1, ~PW))), ~PW))), '!'('<'(0, ~PL)))"

# run functions on dataset
IrisSetosa <- read.delim("IrisSetosa.txt")
attach(IrisSetosa)

#  compare classification from data set with evaluation
#  the highest ranked program has a perfect score:
IrisSetosa$CLASS == eval(parse(text = str_replace_all(combo2exp(is.setosa)[1], "~", "")))
  [1] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [24] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [47] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [70] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [93] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[116] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[139] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# the 3rd ranked combo function with score -1 has one mismatch, as expected:
IrisSetosa$CLASS == eval(parse(text = str_replace_all(combo2exp(is.setosa)[3], "~", "")))
  [1] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [24] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
 [47] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [70] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [93] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[116] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[139] TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# the values for the misclassified observation:
IrisSetosa[IrisSetosa$CLASS != eval(parse(text = str_replace_all(combo2exp(is.setosa)[3], "~", ""))),]
   SL  SW  PL  PW CLASS
44  5 3.5 1.6 0.6     1

# the combo program:
is.setosa[3]
[1] "not(0<(+(*(-1 exp(*(-1 $PW))) $PW))) "

# comparing the misclassified observation (44) with others reveals an unusual value for variable PW, as would be expected.
> IrisSetosa[40:50,]
    SL  SW  PL  PW CLASS
40 5.1 3.4 1.5 0.2     1
41 5.0 3.5 1.3 0.3     1
42 4.5 2.3 1.3 0.3     1
43 4.4 3.2 1.3 0.2     1
44 5.0 3.5 1.6 0.6     1  <- misclassified observation
45 5.1 3.8 1.9 0.4     1
46 4.8 3.0 1.4 0.3     1
47 5.1 3.8 1.6 0.2     1
48 4.6 3.2 1.4 0.2     1
49 5.3 3.7 1.5 0.2     1
50 5.0 3.3 1.4 0.2     1