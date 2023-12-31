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
      useShinyjs(),
      titlePanel(title =  div(img(src="www/logo.png", width ='120'), 'First mapping round'), windowTitle = "Geoprospective 1" ),
      tabsetPanel(id = "inTabset",
                  tabPanel(title = "Load study", value = "p0",
                           h5("Please provide your study id that you received from the admin"),
                           br(),
                           textInput("site_id","Enter the site id from your invitation"),
                           # actionButton("sub0","submit")
                           uiOutput("cond_0")
                  ),
                  tabPanel(title = "user contact", value = "p1",
                           h5("to contact you for round 2 please provide the e-mail address"),
                           br(),
                           textInput("email","email"),
                           uiOutput("cond_1")
                  ),
                  tabPanel(title = "Questionnaire", value = "p2",
                           mod_questionnaire_ui("questionnaire")),
                  tabPanel(title = "Your task", value = "p3",
                           mod_training_ui("training_1")
                  )
                  ),
                  uiOutput("tabs"),
                  uiOutput("ahp_group"),
                  uiOutput("ahp_single"),
                  uiOutput("ahp_dist"),
                  uiOutput("final")

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
