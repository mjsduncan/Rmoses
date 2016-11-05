### features2utterances function

##  given a vector of features and a list of vectors of combo strings, one vector for each tested utterance,
##  return a named vector of ensemble scores, each score named for it's utterance

## combo2R code from moses2.R
# define n >=2 argument boolean operators
and <- function(x) Reduce("&", x)
or <- function(x) Reduce("|", x)
true <- TRUE
false <- FALSE

# turn boolean combo string vector into list of R function combinations
combo.edit <- function(str) {
  require(stringr)
  mc_ops <- list(c("true", "TRUE"), c("(", "(list("), c(")", "))"), c(" ", ", "),c("$", ""))
  for(i in seq_along(mc_ops)) str <- str_trim(str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2]))
  return(str)
}

# evaluate translated expression
evalstring <- function(str, enviro = parent.env()){
  eval(parse(text=str), envir = enviro)
}

## get vector of unique features from vector of combo strings
comboFeatures <- function(combos) {
  require(stringr)
  # break combos into words
  combos <- unique(unlist(str_split(str_replace_all(combos, "[()!]", " "), "[ ]+")))
  # filter for words starting with feature marker "$"
  combos <- grep("$", combos, fixed = TRUE, value = TRUE)
  # drop "$"
  str_replace_all(combos, fixed("$"), "")
}

## function to make environment with an input vector of feature names set to FALSE
makeFeatureEnviro <- function(comboStr, parEnviro = emptyenv(), val = FALSE) {
  if(!is.environment(parEnviro)) stop("error: assigned parent environment doesn't exist.")
  enviro <- new.env(parent = parEnviro)
  comboStr <- comboFeatures(comboStr)
  for(i in comboStr) assign(i, val, envir = enviro)
  enviro$and <- function(x) Reduce("&", x)
  enviro$or <- function(x) Reduce("|", x)
  enviro$true <- TRUE
  enviro$false <- FALSE
  return(enviro)
}

## make testing objects
testCombos <- sample(testCombos$feature25$combo, 20)
testEnviro <- makeFeatureEnviro(testCombos)
testInput <- moses_input[sample(1:196803, 10), -c(1, 3401)]
testInput <- rbind(testInput, moses_input[moses_input[[3401]] == 25,][1:10, -c(1, 3401)])
row.names(testInput) <- NULL
table(colSums(testInput))
#    0    1    2    3    4   10   11
# 3350   41    4    1    1    1    1

# feature labels for testInput
# testInput[[3400]]
# [1] 18 22 23 22 22 22 22 22 19  3 25 25 25 25 25 25 25 25 25 25

## given an named integer feature vector, make a vector of feature names
input2featureVec <- function(input) names(input)[as.logical(unlist(input))]

## given a feature vector, add features to an environment
addFeatures <- function(featureVec, enviro, filter = TRUE, val = TRUE) {
  if(filter) featureVec <- intersect(featureVec, ls(envir = enviro))
  for(i in featureVec) assign(i, val, enviro)
  return(enviro)
}

## given a list of combo string ensemble vectors, return an environment with a nested environment for each ensemble
ensemble2env <- function(ensList, par = .GlobalEnv) {
  enviroList <- lapply(ensList, makeFeatureEnviro, par)
  return(list2env(enviroList))
}

## apply a vector of combo strings to a feature environment and return an ensemble score

## given an input feature vector and a list of combo string ensemble vectors, return a vector of ensemble scores
scoreFeatureVector <- function(features, comboList) {
  if(length(unique(names(comboList))) < length(names(comboList))) return("error: list of combo ensembles requires unique names.")
  enviroList <- ensemble2env(comboList)
  enviroList <- lapply(enviroList, function(x) addFeatures(features, x), )
  return(enviroList)
}



#### try lapply env= arg and list2env
