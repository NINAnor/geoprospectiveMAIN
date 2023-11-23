#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel(title =  div(img(src="logo.png", width ='120'), 'Geoprospective round 1'), windowTitle = "Geopros 1" ),
      tabsetPanel(id = "inTabset",
                  tabPanel(title = "Load study", value = "p0",
                           h5("Please provide your study id that you received from the admin"),
                           br(),
                           textInput("site_id","Enter the site id from your invitation"),
                           # actionButton("sub0","submit")
                           uiOutput("cond_0")
                  ),
                  tabPanel(title = "user contact", value = "p1",
                           h5("to contact you for round 2 please provide the e-mail adress"),
                           br(),
                           textInput("email","email"),
                           actionButton("sub1","start")
                  ),
                  tabPanel(title = "Questionnaire", value = "p2",
                           mod_questionnaire_ui("questionnaire")),
                  tabPanel(title = "Your task", value = "p3",
                           mod_training_ui("training_1")
                  ),
                  tabPanel(title = "ES mapping 1", value = "p4",
                           mod_delphi_round1_ui("mapping_1")),
                  tabPanel(title = "ES mapping 2", value = "p5",
                           mod_delphi_round1_ui("mapping_2")),
                  tabPanel(title = "ES mapping 3", value = "p6",
                           mod_delphi_round1_ui("mapping_3")

                  ))
                  # uiOutput("dynamic_tabs")

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "geoprospectiveMAIN"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
