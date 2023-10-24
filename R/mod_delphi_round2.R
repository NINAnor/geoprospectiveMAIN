#' delphi_round2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_delphi_round2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' delphi_round2 Server Functions
#'
#' @noRd 
mod_delphi_round2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_delphi_round2_ui("delphi_round2_1")
    
## To be copied in the server
# mod_delphi_round2_server("delphi_round2_1")
