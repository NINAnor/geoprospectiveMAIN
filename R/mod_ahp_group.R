#' ahp_group UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ahp_group_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      h3("Comparison of ecosystem services"),
      br(),
      "First, you are going to compare the importance of grouped ecosystem services. Keep in mind that you should refer your ratings to the study area. The comparison might look different in other areas. In particular, you will compare the importance of the following three groups:",
      br(),
      h4(" - Cultural ecosystem services"),
      " Benefits that you and others gain from interactions with different areas and with people
      in these areas through a wide range of activities. This includes exploring areas on site with all our senses. In additon benefits we gain indirectly even if we are not
      present in the area. This might include learning from landscapes, feeling at home which gives us identity and cultural importance of certain areas.",
      br(),
      h4("  - Provisioning ecosystem services"),
      "These includes benefits that people can extract from the nature in a certain area. Along with food, other types of provisioning services include drinking water, timber, wood fuel, natural gas, oils, plants that can be made into clothes and other materials.",
      br(),
      h4("  - Regulation ecosystem services"),
      "Services that provides the basic needs and make our life possible. Plants clean air and filter water, bacteria decompose wastes, bees pollinate flowers, and tree roots hold soil in place to prevent erosion.",
      br(),
      br(),
      uiOutput(ns("slider")),


      actionButton(ns("conf2"), "Next task", class='btn-primary')
    )

  )
}

#' ahp_group Server Functions
#'
#' @noRd
mod_ahp_group_server <- function(id, userID, siteID, table_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    es_sec<-c("recr","cult","prov")

    es_sec<-unlist(as.vector(es_sec))
    sec_comb<-as.data.frame(t(combn(es_sec, 2)))

    output$slider <- shiny::renderUI({
      ns <- session$ns

      lapply(1:nrow(sec_comb),function(n){
        pair_id <- paste0(sec_comb[n,]$V1,"_",sec_comb[n,]$V2)
        pair_lable<-""
        choice1<-paste0(sec_comb[n,]$V1, " is overwhelmingly more important")
        choice2<-paste0(sec_comb[n,]$V1, " is very strongly more important")
        choice3<-paste0(sec_comb[n,]$V1, " is strongly more important")
        choice4<-paste0(sec_comb[n,]$V1, " is moderately more important")

        choice5<-paste0(sec_comb[n,]$V2, " is overwhelmingly more important")
        choice6<-paste0(sec_comb[n,]$V2, " is very strongly more important")
        choice7<-paste0(sec_comb[n,]$V2, " is strongly more important")
        choice8<-paste0(sec_comb[n,]$V2, " is moderately more important")
        choices<-c(choice1,
                   choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5)

        id_left<-paste0(sec_comb[n,]$V1," ecosystem services")
        id_right<-paste0(sec_comb[n,]$V2," ecosystem services")

        tagList(
          column(6, id_left),
          column(6, id_right),
          sliderTextInput(ns(pair_id),
                          pair_lable,
                          grid = F,
                          force_edges = TRUE,
                          choices = choices,
                          width = "75%",
                          selected = choices[5]

          )#/slider
        )#/tagList

      })



    })


    ### store the values
    observeEvent(input$conf2,{
      val_list<-list()
      # id_ist<-list()
      res<-lapply(1:nrow(sec_comb),function(a){


        var<-paste0(sec_comb[a,]$V1,"_",sec_comb[a,]$V2)
        val_list[[a]]<-input[[var]]
        return(val_list)
      })
      comp_val <- unlist(res)

      sec_comb$comp_val<-comp_val

      sec_comb$recode <- rep(0,nrow(sec_comb))

      n<-lapply(1:nrow(sec_comb),function(a){
        if(sec_comb[a,]$comp_val == "both are equally important"){
          sec_comb[a,]$recode <- 1
        } else if(grepl("is overwhelmingly more important",sec_comb[a,]$comp_val) == TRUE){
          sec_comb[a,]$recode <- 8
        } else if(grepl("is very strongly more important",sec_comb[a,]$comp_val) == TRUE){
          sec_comb[a,]$recode <- 6
        } else if(grepl("is strongly more important",sec_comb[a,]$comp_val) == TRUE){
          sec_comb[a,]$recode <- 4
        } else if(grepl("is moderately more important",sec_comb[a,]$comp_val) == TRUE){
          sec_comb[a,]$recode <- 2
        }

        if(grepl(sec_comb[a,]$V1,sec_comb[a,]$comp_val) == TRUE){
          sec_comb[a,]$recode<-sec_comb[a,]$recode*-1
        } else {
          sec_comb[a,]$recode<-sec_comb[a,]$recode
        }
      })
      sec_comb$recode <- as.integer(unlist(n))
      sec_comb$userID<-rep(userID,nrow(sec_comb))
      sec_comb$siteID<-rep(siteID,nrow(sec_comb))
      sec_comb$group <- rep(4,nrow(sec_comb))
      sec_comb$group <- as.integer(sec_comb$group)

      colnames(sec_comb)<-c("ES_left","ES_right","selection_text","selection_val","userID","siteID", "ahp_section")
      insert_upload_job(table_con$project, table_con$dataset, "es_pair", sec_comb)

    })

    cond <- reactive({input$conf2})

    return(cond)

  })
}

## To be copied in the UI
# mod_ahp_group_ui("ahp_group_1")

## To be copied in the server
# mod_ahp_group_server("ahp_group_1")
