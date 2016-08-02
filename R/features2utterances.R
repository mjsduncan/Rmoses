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
  # break combos into words
  combos <- unique(unlist(str_split(str_replace_all(combos, "[()!]", " "), "[ ]+")))
  # filter for words starting with feature marker "$"
  combos <- grep("$", combos, fixed = TRUE, value = TRUE)
  # drop "$"
  str_replace_all(combos, fixed("$"), "")
}

## function to make environment with an input vector of feature names set to FALSE
makeFeatureEnviro <- function(comboStr, parEnviro = .GlobalEnv, val = FALSE) {
  if(!is.environment(parEnviro)) return("error: assigned parent environment doesn't exist.")
  enviro <- new.env(parent = parEnviro)
  comboStr <- comboFeatures(comboStr)
  for(i in comboStr) assign(i, val, envir = enviro)
  return(enviro)
}

## given a feature vector, add features to an environment
addFeatures <- function(featureVec, enviro, val = TRUE) {
  for(i in featureVec) assign(i, val, enviro)
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
