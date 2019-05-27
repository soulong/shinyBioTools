#'
#' @title start shinyBioTools
#'
#' @description  start shinyBioTools by initialize a shiny app
#'
#' @param port run app on localhost, define a port, default port is 5001
#'
#' @return a shiny web app
#'
#' @export
#'
#' @examples
#' libray("shinyBioTools")
#' runshinyBioTools(port=5001)
runshinyBioTools <- function(port=5001) {

  appDir <- system.file("example", "app", package="shinyBioTools")

  if (appDir=="") {
    stop("Could not find example directory. Try re-installing `shinySeqDB`.", call.=F)
  }

  options(shiny.maxRequestSize=100*1024^2)
  options(stringsAsFactors=F)
 # options(error=traceback)

  # mirE shRNA vector
  # shRNA_vector <- Biostrings::readDNAStringSet("./data/pGIPZ-mirE-control.fa",  "fasta")[[1]]
  # usethis::use_data(shRNA_vector)
  shRNA_vector <- shinyBioTools::shRNA_vector

  # import module
  source(system.file("module", "module_rtPCR.R", package="shinyBioTools"))
  source(system.file("module", "module_rtPCR_ui.R", package="shinyBioTools"))
  source(system.file("module", "module_shRNA.R", package="shinyBioTools"))
  source(system.file("module", "module_shRNA_ui.R", package="shinyBioTools"))


  # run APP
  shiny::runApp(appDir, host="0.0.0.0", port=port, display.mode="normal")

}
