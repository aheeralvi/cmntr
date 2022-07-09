#**************************************************************************
#* NAME: m_utl_sasmac.sas
#**************************************************************************
#* VERSION: 0.1
#* DESCRIPTION: Macro generates source code from a specified macro. Macro
#*   in the source code will be resolved so that the resulting code file
#*   can run stand-alone.
#* AUTHOR: David Bosak, COMSYS
#* DATE: 15 April 2007
#**************************************************************************
#* ASSUMPTIONS: Macro must have mprint statements to flag code generation.
#*
#* PARAMETERS:
#* - in_loc: The location of the input file
#* - in_flnm: The name of the input file
#* - out_loc: The location of the output file
#* - out_flnm: The name of the output file
#*
#* INPUTS:
#* - Datasets:
#* - Global Macro Variables: None
#*
#* OUTPUTS:
#* - Datasets: None
#* - Global Macro Variables: None
#*
#* DEPENDENCIES:
#* - Packages: None
#* - Functions: None
#* - Formats: None
#* - Metadata: None
#*
#**************************************************************************
#* Change History
#**************************************************************************
#* DATE:
#* - By:
#* - Description:
#**************************************************************************




#***************************************************************************
#***                        this is another test                         ***
#***************************************************************************


library(shiny)
library(miniUI)


bannerCommentDialog <- function() {
  ui <- miniPage(
    miniContentPanel(
      textInput("content", "Desired Text:"),
      HTML("<button id = 'okay'
           type ='button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Okay</button>"),

      HTML("<button id = 'cancel'
           type = 'button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Cancel</button>")
  )) # End UI
  server <- function(input, output, session) {
    observeEvent(input$okay, ignoreNULL = TRUE, ignoreInit = TRUE, {
      textInsert <- input$content
      comment <- cmnt_banner(textInsert)
      rstudioapi::insertText("\n")
      for(i in 1:length(comment)) {
        rstudioapi::insertText(comment[i])
        rstudioapi::insertText("\n")
      }
    })
  } # End Server
  viewer <- dialogViewer("Create Banner Comment")
  runGadget(ui, server, viewer = viewer)
} # End Gadget


#***************************************************************************
#***                           This is a test                            ***
#***************************************************************************



#***************************************************************************
#***                                asdf                                 ***
#***************************************************************************



#' @title Sample title
#' @encoding UTF-8
#' @description Put function description here.
#' @details
#' Elaborate on the function here.
#' @section Section Name:
#' Here is a sample section with a sample list.
#' \itemize{
#' \item{\strong{Item 1}: Item description.}
#' \item{\strong{Item 2}: Item description.}
#' \item{\strong{Item 3}: Item description.}
#' }
#' @param param1 Parameter description.
#' @param param2 Parameter description.
#' @return What the function returns.
#' @family FamilyName
#' @seealso References to related function
#' @examples
#' # Here is a simple example
#' @import package
#' @export




cmnt_roxygen <- function() {


}


