#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_training_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' training Server Functions
#'
#' @noRd 
mod_training_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_training_ui("training_1")
    
## To be copied in the server
# mod_training_server("training_1")
