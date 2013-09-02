\name{Rmoses-package}
\alias{Rmoses-package}
\alias{Rmoses}
\docType{package}
\title{
wrappers and tools for using opencog::moses
}
\description{
MOSES is a new approach to program evolution, based on representation-building and probabilistic modeling. MOSES has been successfully applied to solve hard problems in domains such as computational biology, sentiment evaluation, and agent control. Results tend to be more accurate and require less objective function evaluations, than other program evolution systems, such as genetic or evolutionary programming. Best of all, the result of running MOSES is a compact and comprehensible program written in a simple Lisp-like mini-language. 
\details{
\tabular{ll}{
Package: \tab Rmoses\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-07-20\cr
License: \tab apache 2.0\cr
}
"Meta-optimizing semantic evolutionary search (MOSES) is a supervised machine learning system that generates programs in a simple lisp-like language to minimize a scoring function over the data to be catagorized.  It's two level algorithm generates a population of sample programs or "demes"which are taken individually in the second level evolutionary loop and randomly"mutated" to generate better scoring programs untill improvement plateaus.  Themost successful programs are returned to the population and a new deme variant is selected for evolution.Programs are regularly normalized to reduce evaluation time and help keep code human-readable.  The Rmoses package provides a wrapper for the seperately installed open source moses binary, interface functions with BioConductor bioinformatics packages, and a combo program translator for application to newdata sets in the R environment.
}
\author{
mike d

Maintainer: <mjsduncan@gmail.com>
}
\references{
http://wiki.opencog.org/w/MOSES
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
~~ directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
%% ~~ simple examples of the most important functions ~~
}
