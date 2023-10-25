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
      h5("Click on the postal area where you currently live"),
      "If you don`t live in the area, do not select an area.",
      br(),
      # mapedit::selectModUI("map_living"),
      selectizeInput(ns("liv"),
                     "How long have you lived in the study area?",
                     choices = c(" I don`t  live in the Trondheim area" ="no_TRD",
                                 "Less than 5 years" = "few",
                                 "5 - 10 years"="more",
                                 "10 - 20 years" = "more2",
                                 "more than 20 years" = "all",
                                 "Prefer not to say"="no_answ"),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
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
      br(),
      h5("Your general environmental attitude"),
      br(),
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
mod_questionnaire_server <- function(id, user_id, proj_id, study_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #load quest
    quest<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/questionnaire.rds")

    rv1<-reactiveValues(
      u = reactive({})
    )
    # print(user_id)



    output$cond_b1<-renderUI({
      validate(
        need(input$age, 'Provide your age'),
        need(input$gender != '', 'Select a gender'),
        need(input$edu != '', 'Select an education'),
        need(input$work != '', 'Select a working industry'),
        need(input$liv != '', 'provide the years you live in the study area'),
        need(input$land != '', 'provide a landscape type')
      )
      actionButton(ns('sub_quest'), 'submit answers', class='btn-primary')
    })
    observeEvent(input$sub_quest,{
      rv1$u <-reactive({1})

      quest_new <-  data.frame(
        userID = user_id,
        studID = study_id,
        projID = proj_id,
        edu = input$edu,
        fam = input$fam,
        liv = input$liv,
        gen = input$gender,
        age = as.integer(input$age),
        work = input$work,
        land = input$land
      )
      quest<-rbind(quest,quest_new)
      saveRDS(quest,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/questionnaire.rds")



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
