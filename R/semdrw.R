#' Start semdrw
#' @title Launch semdrw Interface
#' @return Nothing
#' @description semdrw() loads interactive user interface built using R shiny. 
#' @details The interactive user interface is to provide an easy way for people who are learning sem  and writing research paper. Includes example data for testing out a few example analysis.
#' @keywords semdrw
#' @examples 
#' \dontrun{
#' library(shiny)
#' semdrw()
#' }

semdrw <- function() {


  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "semdrw"))
  Sys.setenv("R_TESTS" = "")
}
