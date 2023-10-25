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
    mainPanel(
      h6("Mapping ecosystem services"),
      br(),
      "Please read the following instructions carefully",
      br(),
      actionButton(ns("proc1"), "proceed"),
      uiOutput(ns("task_2")),
      uiOutput(ns("task_3")),
      uiOutput(ns("task_4")),
      uiOutput(ns("task_5"))


    )#/main panel

  )
}

#' training Server Functions
#'
#' @noRd
mod_training_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv1<-reactiveValues(
      u = reactive({})
    )

    #descr 0
    observeEvent(input$proc1,{
      output$task_2<-renderUI({
        tagList(
          h3("1. Description of ecosystem service"),
          br(),
          "In the mapping window, we show you an image and a description of the ecosystem service. Ecosystem services are complex, thus it is important that you first read this description and that you reflect on where, when or in which form you already experienced benefits from this service.",
          br(),
          h3("2. Indicating the importance"),
          br(),
          "Subsequently you are asked to indicate how important this ecosystem service is for you personally and in general for the society. You can ask yourselfe How much you might benefit from this particular service. Please make sure that you refer your rating only to the study area indicated in the introduction of the study.",
          br(),
          actionButton(ns("ok1"),"got it")
        )
      })

      removeUI(selector = paste0("#",ns("task_1")))

    })

    # description 1
    observeEvent(input$ok1,{
      output$task_3<-renderUI({
        tagList(
          h3("3. Can you map it?"),
          br(),
          "You are now going to be asked if you feel comfortable to indicate on a map within the study area where you think you or other can benefit from this eoscystem service",
          br(),
          h4("3.1. If not"),
          br(),
          "No problem, you are now just beeing asked if you would trust a map of this ecosystem service that has been developed by experts or national authorities.",
          "A next ecosystem service will appear which you might be able to map.",
          br(),
          actionButton(ns("ok2"),"What if I can map...")
        )
      })

      removeUI(selector = paste0("#",ns("task_2")))

    })

    # description 2
    observeEvent(input$ok2,{
      output$task_4<-renderUI({
        tagList(
          h3("4. You can map"),
          br(),
          "An interactive map with orange borders indicating the study area will appear. You can pan and zoom the map.",
          br(),
          "As shown below you can create and modify one or several rectangles.",
          "The rectangle(s) should meet the following aspects:",
          br(),
          fluidRow(strong("- deliniate as precise as possible areas of high ecosystem service benefit")),
          fluidRow(strong("- not too small areas: The squares should have an edge length of min 300m")),
          "Press save polygons once you are done!",
          br(),
          fluidRow(img(src="tutorial_selecting.gif", align = "central",height='310px',width='418px')),
          br(),
          fluidRow(actionButton(ns("ok3"),"proceed"))

        )
      })
      removeUI(selector = paste0("#",ns("task_3")))

    })

    # description 3
    observeEvent(input$ok3,{
      output$task_5<-renderUI({
        tagList(
          h3("5. Rate your polygons"),
          br(),
          "As soon as you have saved the polygons, they will appear on the map with a red number. Below you find for each polygon a slider with the same number.",
          "Now you can set the slider value for each polygon. Higher values indicate that the area serves very high quality to benefit from the ecosystem service",
          "Finally you need to write a few keywords why you choosed these areas",
          "Press submit",
          br(),
          fluidRow(img(src="tutorial_rating.gif", align = "central",height='310px',width='418px')),
          br(),
          fluidRow(actionButton(ns("ok4"),"proceed"))
        )
      })
      removeUI(selector = paste0("#",ns("task_1")))

      })

    # description 4
    observeEvent(input$ok4,{
      output$task_5<-renderUI({
        tagList(
          h3("6. Your Result"),
          br(),
          "Your polygons and ratings are stored and extrapolated to the whole study region - this can take up to 30 seconds. Please wait until the new map appears",
          "The final map shows you the probability to benefit from the respective ecosystem service, based on your inputs.",
          "Press next and this procedure will repeat four times in total",
          br(),
          actionButton(ns('sub3'), 'Go to first mapping task', class='btn-primary')
        )
      })


    })

    observeEvent(input$sub3,{
      rv1$u <-reactive({1})
    })

    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})

    return(cond)
  })
}

## To be copied in the UI
# mod_training_ui("training_1")

## To be copied in the server
# mod_training_server("training_1")
