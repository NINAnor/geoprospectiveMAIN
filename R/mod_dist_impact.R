#' dist_impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dist_impact_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Impact of wind turbines on potential use of ecosystem services"),
    "Imagine that you can see and/or hear a wind turbine from a certain area. How much is your potential use of the following ecosystem services affected by visual and/or acoustic effects of the turbine(s)?",
    br(),
    "0 = I am not affected at all, 1 = I won't use the area anymore",
    br(),
    uiOutput(ns("slider_impact")),
    actionButton(ns("conf"), "Next task", class='btn-primary')
    )
}

#' dist_impact Server Functions
#'
#' @noRd
mod_dist_impact_server <- function(id, userID, siteID, es_all, table_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$slider_impact<-renderUI({
      lapply(1:nrow(es_all),function(n){
        inputid<-paste0(es_all$esID[n],"_impact")
        label<-es_all$esNAME[n]


        sliderInput(
        inputId = ns(inputid),
        label = label,
        min = 0,
        max = 1,
        value =.5,
        step = .1,
        ticks = T
        )#/slider
      })#/lapply
    })#/slider

    observeEvent(input$conf,{
      val_list<-list()

      res<-lapply(1:nrow(es_all),function(a){
        var<-paste0(es_all$esID[a],"_impact")
        val_list[[a]]<-input[[var]]
        return(val_list)
      })
      val_vec <- unlist(res)
      es_all$impact_val<-val_vec


      es_all$userID<-rep(userID,nrow(es_all))
      es_all$siteID<-rep(siteID,nrow(es_all))
      es_all<-es_all%>%select(userID, siteID, esID, impact_val)
      colnames(es_all)<-c("userID","siteID","esID","impact_val")
      insert_upload_job(table_con$project, table_con$dataset, "es_impact", es_all)

    })


    cond <- reactive({input$conf})

    return(cond)

  })
}

## To be copied in the UI
# mod_dist_impact_ui("dist_impact_1")

## To be copied in the server
# mod_dist_impact_server("dist_impact_1")
