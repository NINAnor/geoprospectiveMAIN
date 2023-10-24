#' questionnaire UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_questionnaire_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' questionnaire Server Functions
#'
#' @noRd 
mod_questionnaire_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_questionnaire_ui("questionnaire_1")
    
## To be copied in the server
# mod_questionnaire_server("questionnaire_1")
