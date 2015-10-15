### graph log
# load file lines with match string
readMlogs <- function(dir = getwd(),  output = TRUE) {
  require(stringr)
  files <- list.files(path = dir)[str_detect(list.files(path = dir), "log$")]
  out <- vector("list", length(files))
  names(out) <- word(files, sep = fixed("."))
  for(i in seq_along(files)) {
    print(paste0(dir, files[i]))
    out[[i]] <- readLines(paste0(dir, files[i]))
  }
  return(out)
}
