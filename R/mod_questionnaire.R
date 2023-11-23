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
    mainPanel(
      "We are going to ask you a few general questions regarding your background and your environmental attitude. Please note, that we will handle your data with care and in fully anonymized form",
      br(),
      br(),
      numericInput(ns("age"),
                   "How old are you?",
                   NULL,
                   min=12,
                   max=110,
                   step=1),
      selectizeInput(ns("gender"),
                     "What is your gender?",
                     choices = c("Female" ="f",
                                 "Male" = "m",
                                 "Neutral" = "n",
                                 "Prefer not to say"="no_answ"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      selectizeInput(ns("edu"),
                     "What is the highest level of education you have attained?",
                     choices = c("Upper secondary education" ="upper_s",
                                 "Tertiary vocational programme" = "tertiary",
                                 "Bachelor"="bsc",
                                 "Master" = "msc",
                                 "Doctoral degree" = "phd",
                                 "Prefer not to say"="no_answ"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }'))

      ),
      selectizeInput(ns("work"),
                     "In which economic sector do you currently work?",
                     choices = c("Mining and quarrying" = "mining",
                                 "Manufacturing"= "man",
                                 "Electricity, gas, steam and air conditioning supply"= "energy",
                                 "Water supply; sewerage, waste management and remediation activities" =  "water",
                                 "Construction" = "cons",
                                 "Distributive trade sector" = "trade",
                                 "Transportation and storage services" = "transport",
                                 "Accommodation and food services" = "accomodation",
                                 "Information and communication services" = "inform",
                                 "Real estate activities" = "real_estate",
                                 "Professional, scientific and technical activities" = "science",
                                 "Administrative and support service activities" = "admin",
                                 "Repair of computers and personal and household goods" = "repair",
                                 "prefer not to say" = "no_w"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      br(),
      h5("The following map shows the study area you are going to map landscape values"),
      leafletOutput(ns("map_stud")),
      selectizeInput(ns("fam"),
                     "How familiar are you with the study area",
                     choices= c("poor" ="poor",
                                "fair" = "fair",
                                "good"="good",
                                "I don`t know" = "dont_know"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      uiOutput(ns("cond_map")),
      br(),
      # h5("Your general environmental attitude"),
      # br(),
      # radioMatrixInput(ns("matInput2"),
      #                  rowIDs = c("NEP1","NEP2","NEP3","NEP4"),
      #                  rowLLabels =  c("Naturen har en verdi i seg selv",
      #                                  "Jeg blir ofte trist når jeg ser større naturinngrep",
      #                                  "Naturens egenverdi er viktigere enn bruks- og nytteverdien",
      #                                  "Naturens tilstand sier noe om hvem vi er som samfunn og folk."),
      #                  choiceNames = c("1", "2", "3","4","5","99"),
      #                  # selected = character(0),
      #                  selected = c("I don`t know","I don`t know","I don`t know", "I don`t know"),
      #                  choices = c("strongly disagree", "agree", "unsure","disagree","strongly disagree", "I don`t know")
      # ),
      selectizeInput(ns("land"),
                     "If you could choose one of the following landscapes, which would you prefere",
                     choices = c("A well-ordered landscape, made by and for people" ="man_made",
                                 "A varied, park-like landscape. " = "park",
                                 "Untamed nature, with which one may have many interactions."="untamed_nat",
                                 "A landscape in which one may experience the greatness and forces of nature." = "compl_nature"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      uiOutput(ns("cond_b1"))
    ) #/main panel
  )
}

#' questionnaire Server Functions
#'
#' @noRd
mod_questionnaire_server <- function(id, user_id, site_id, sf_stud_geom, site_type, table_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #load quest
    # quest<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/questionnaire.rds")

    rv1<-reactiveValues(
      u = reactive({})
    )
    # print(user_id)

    output$map_stud<-renderLeaflet({
      leaflet(sf_stud_geom) %>%
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 13))
    })

    output$cond_b1<-renderUI({
      validate(
        need(input$age, 'Provide your age'),
        need(input$gender != '', 'Select a gender'),
        need(input$edu != '', 'Select an education'),
        need(input$work != '', 'Select a working industry'),
        need(input$land != '', 'provide a landscape type')
      )
      actionButton(ns('sub_quest'), 'submit answers', class='btn-primary')
    })
    if(site_type == "onshore"){
      output$cond_map<-renderUI({
        tagList(
          selectInput(ns("liv_in_area"),"do you live in the study area?", choices = c("","yes","no"),selected = ""),
          uiOutput(ns("cond2"))
        )
      })
    }

    observeEvent(input$liv_in_area,{
       req(input$liv_in_area)
      if(input$liv_in_area == "yes"){
        output$cond2<-renderUI({
          tagList(
            h5("select the rectangle you live in"),
            mapedit::selectModUI(ns("map_living")),
            br(),
            selectizeInput(ns("length_liv"),
                           "How long have you lived in the study area?",
                           choices = c("Less than 5 years" = "few",
                                       "5 - 10 years"="more",
                                       "10 - 20 years" = "more2",
                                       "more than 20 years" = "all",
                                       "Prefer not to say"="no_answ"),
                           options = list(
                             placeholder = 'Please select an option below',
                             onInitialize = I('function() { this.setValue(""); }')
                           ))
          )
        })

      }
    })

    map_liv<-eventReactive(input$liv_in_area,{
      if(input$liv_in_area == "yes"){
        grd<-st_make_grid(sf_stud_geom, cellsize = 0.02,
                          offset = st_bbox(sf_stud_geom)[1:2],  what = "polygons")

      #cellsiz= c(diff(st_bbox(sf_stud_geom)[c(1, 3)]),
        #diff(st_bbox(sf_stud_geom)[c(2,4)]))/10

        map_liv<- leaflet() %>%
          addProviderTiles(provider= "CartoDB.Positron")%>%
          addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))

      }
    })


    liv_pol <- callModule(module=selectMod,
                          leafmap=map_liv(),
                          id="map_living")








    observeEvent(input$sub_quest,{
      rv1$u <-reactive({1})
      if(site_type == "onshore"){
        liv_in_area <- input$liv_in_area
        if(input$liv_in_area == "yes"){
          length_liv<-input$length_liv
          liv_pol<-liv_pol()
          liv_pol<-st_sf(grd[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
          # only take first poly if user selected multiple
          liv_pol<-liv_pol[1,]
          cent<-st_centroid(liv_pol)
          user_lat <- st_coordinates(cent)[2]
          user_lng <- st_coordinates(cent)[1]
        }else{
          length_liv<-NULL
          user_lat <- NULL
          user_lng <- NULL
        }}else{
        liv_in_area<- "offshore"
      }


      quest<-  data.frame(
        userID = user_id,
        siteID = site_id,
        edu = input$edu,
        fam = input$fam,
        gen = input$gender,
        age = as.integer(input$age),
        work = input$work,
        userLAT = user_lat,
        userLNG = user_lng,
        liv_in_area = liv_in_area,
        length_liv = length_liv,
        pref_land_type = input$land,
        res_1 = as.integer(999),
        res_2 = as.integer(999),
        res_3 = as.integer(999),
        res_4 = as.integer(999),
        res_5 = as.integer(999),
        res_6 = as.integer(999),
        res_7 = as.integer(999),
        res_8 = as.integer(999),
        res_9 = as.integer(999),
        res_10 = as.integer(999),
        res_11 = as.integer(999),
        res_12 = as.integer(999),
        res_13 = as.integer(999),
        res_14 = as.integer(999),
        res_15 = as.integer(999),
        res_16 = as.integer(999),
        res_17 = as.integer(999),
        res_18 = as.integer(999),
        res_19 = as.integer(999),
        res_20 = as.integer(999)
      )
      insert_upload_job(table_con$project, table_con$dataset, "mapper", quest)


    })
    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})

    return(cond)

  })
}

## To be copied in the UI
# mod_questionnaire_ui("questionnaire_1")

## To be copied in the server
# mod_questionnaire_server("questionnaire_1")
