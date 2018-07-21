### how many folds to get each sample in at least one test partition?
# N is number of samples, n is size of test set, f is number of folds
library(extraDistr)

# trial function that checks coverage given N, n, and f.  vectorized over f

completeCoverage <- Vectorize(
  function(N, n, f) {
  trial <- rmvhyper(f, rep.int(1, N), n)
  suppressWarnings(all(apply(trial, 2, any)))
  }, "f")

# function to generate summary of experiment given number of trials per fold and fold range
MCvalidationFoldTest <- function(N, n, folds, trials = 10) {
  minFolds <- ceiling(N/n) - 1
  labels <- list(paste("trial", 1:trials), seq.int(minFolds + 1, length.out = folds))
  results <- matrix(0, nrow = trials, ncol = folds)
  results <- col(results) + minFolds
  results <- t(apply(results, 1, function(x) completeCoverage(N, n, x)))
  dimnames(results) <- labels
  summary <- apply(results, 2, function(x) sum(x)/length(x))
  return(summary)
}

MCvalidationFoldTest(10, 3, 10, 100)
#    4    5    6    7    8    9   10   11   12   13 
# 0.01 0.07 0.30 0.39 0.53 0.63 0.66 0.88 0.94 0.86 
MCvalidationFoldTest(10, 3, 10, 1000)
#     4     5     6     7     8     9    10    11    12    13 
# 0.011 0.084 0.227 0.386 0.536 0.635 0.738 0.812 0.843 0.893 
MCvalidationFoldTest(10, 3, 10, 10000)
#      4      5      6      7      8      9     10     11     12     13 
# 0.0159 0.0910 0.2144 0.3652 0.5175 0.6428 0.7439 0.8094 0.8684 0.9046 
MCvalidationFoldTest(10, 3, 10, 100000)
#       4       5       6       7       8       9      10      11      12      13 
# 0.01463 0.08852 0.22004 0.37168 0.51933 0.64220 0.73698 0.81170 0.86466 0.90458 

MCvalidationFoldTest(10, 1, 10, 10000)
#     10     11     12     13     14     15     16     17     18     19 
# 0.0005 0.0018 0.0045 0.0144 0.0260 0.0458 0.0712 0.1001 0.1357 0.1693 

split_3.10.20 <- MCvalidationFoldTest(10, 3, 20, 10000)
barplot(split_3.10.20, main = "samples = 10, test set = 3, folds tried = 20, trials = 10000")
split_30.100.20 <- MCvalidationFoldTest(100, 30, 20, 10000)
barplot(split_30.100.20, main = "samples = 100, test set = 30, folds tried = 20, trials = 10000")
split_1.10.50 <- MCvalidationFoldTest(10, 1, 50, 10000)
barplot(split_1.10.50, main = "samples = 100, test set = 10, folds tried = 30, trials = 10000")
split_10.100.50 <- MCvalidationFoldTest(10, 1, 50, 10000)
barplot(split_10.100.50, main = "samples = 100, test set = 10, folds tried = 50, trials = 10000")
split_30.100.30 <- MCvalidationFoldTest(100, 30, 30, 10000)
barplot(split_30.100.30, main = "samples = 100, test set = 30, folds tried = 30, trials = 10000")
