###  wrapper/interface functions for moses binary

# run moses on file
moses <- function( flags = "", DSVfile = "", output = TRUE) {
	if(flags == "help") flags <- "--help"          # moses(help) -> moses --help
	if(flags == "version")  flags <- "--version"    # moses() -> moses --version
  if(DSVfile != "") flags <- paste("--input-file", DSVfile, "--log-file", paste(word(DSVfile, sep = fixed(".")), ".log", sep = ""), flags)
	system2("moses", args=flags, stdout = output)
}

# run moses on directory
runMfolder <- function(dir = getwd(), flags = "", output = TRUE) {
  files <- list.files(path = dir)
  out <- vector("list", length(files))
  names(out) <- paste(word(files, sep = fixed(".")), "_Mout", sep = "")
  for(i in seq_along(files)) out[[1]] <- moses(flags, files[i])
  return(out)
}



# TODO:  guided menu system to set moses flags for data appropriate analysis