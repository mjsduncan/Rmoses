#  wrapper/interface functions for moses binary

moses <- function( flags = "", DSVfile = "", output = TRUE) {
	if(flags == "help") {flags = "--help"; output = ""} else {                                                # moses(help) -> moses --help
	if(DSVfile != "") flags = paste("--input-file", DSVfile, flags) else {flags = "--version"; output = ""}}  # moses() -> moses --version
	system2("moses", args=flags, stdout = output)                                                             # run moses on file
}

# TODO:  guided menu system to set moses flags for data appropriate analysis