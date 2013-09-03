Rmoses
======

R wrappers and tools for opencog meta-optimizing semantic evolutionary search

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

nothing here is functional yet...
