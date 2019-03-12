dyntaxa_rds <- NULL

.onAttach <- function(lib, pkg){

  # echo "dyntaxa" | toilet -f chunky

  welcome <-
"    __             __
.--|  .--.--.-----|  |_.---.-.--.--.---.-.
|  _  |  |  |     |   _|  _  |_   _|  _  |
|_____|___  |__|__|____|___._|__.__|___._|
      |_____|"
  packageStartupMessage(welcome)

  rds <- file.path(rappdirs::app_dir("dyntaxa")$config(), "dyntaxa.rds")
  if (!file.exists(rds)) {
    packageStartupMessage("Cannot find Dyntaxa data locally...")
    packageStartupMessage("... attempting download using library(dyntaxa); dyntaxa_init()")
    dyntaxa_init()
  }
  
}

.onLoad <- function(libname, pkgname){
  rds <- file.path(rappdirs::app_dir("dyntaxa")$config(), "dyntaxa.rds")
  if (file.exists(rds)) {
    dyntaxa_rds <<- readRDS(rds)
  }
}

