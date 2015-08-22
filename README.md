---
output:
  html_document:
    smart: yes
---
Rmoses
======

### R wrappers and tools for opencog's
### Meta-Optimizing Semantic Evolutionary Search

Part of opencog, Meta-optimizing semantic evolutionary search" (MOSES) is a
supervised machine learning system that generates programs in a simple lisp-
like language to minimize a scoring function over the data to be catagorized.
It's two level algorithm generates a population of sample programs or "demes"
which are taken individually in the second level evolutionary loop and randomly
"mutated" to generate better scoring programs untill improvement plateaus. The
most successful programs are returned to the population and a new deme variant
is selected for evolution.
Programs are regularly normalized to reduce evaluation time and help keep code
human-readable. The Rmoses package provides a wrapper for the seperately
installed open source moses binary, interface functions with BioConductor
bioinformatics packages, and a combo program translator for application to new
data sets in the R environment.

### run the moses binary
`moses(flags = "", DSVfile = "", output = TRUE, ...)`  

* **flags:**  string of flags to pass to the binary (see moses binary man page)  
example: "-j8 --balance 1 -m 100000 -W 1 --output-cscore 1 --result-count 100"  
* **DSVfile:**  moses input file (see man page for possible formats)  
* **output, ...:**  output value is passed to system2(stdout = ), which runs the binary.  
the default value (TRUE) returns a character vector of the moses output.  see the  
system2() help page for other values and other system2() variable options.

### typical usage example
**go to empty folder to hold moses input files and logs**  
`setwd(~/path/to/moses_run)`  

**make training partition csv files and list of corresponding testing partitions**  
`listsOfDataPartitions <- makeMpartitions(mosesInput)`  

**run moses on training sets**  
`mosesOutput <- runMfolder("-j8 --balance 1 -m 100000 -W 1 -u case --output-cscore 1 --result-count 100")`  

**extract combo strings and scores from moses output**  
`combosNscores <- moses2combo(mosesOutput)`  

**run combos on training and testing sets and generate confusion matrix**  
`scoresNconfusionMatrix <- testClist(combosNscores$combo, listOfDataPartitions, fraction_of_cases_in_sample)`  

**get combos scoring better than chance on test set**
`testSetPerformance <- bestTestCombos(scoresNconfusionMatrix)`

**make dataframe of genes/feataures**
`featureCount <- combo2fcount(testSetPerformance[[1]])`

**if test set scores are sufficiently high, get scores for complete data set**
**combine fold results  and generate aggregate score and confusion matrix**  
`aggScores <- aggResults(scoresNconfusionMatrix)`  

*the "aggScores" dataframe is ranked by score so combos can be filtered by row index*  

**make dataframe of genes/feataures**  
`aggFeatureCount <- combo2fcount(names(aggScores[[2]]))`  

**TODO:**  
establish and implement a score cutoff to filter the combos  
wrap workflow into single function  
???  

