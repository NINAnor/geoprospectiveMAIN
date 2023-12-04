#' ahp_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ahp_single_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      h3("Comparison of single ecosystem services"),
      br(),
      "You will now compare the importance of different ecosystem services within the
         study region. Click on the name of the respective ecosysystem service, if you need further information.",
      br(),
      br(),
      fluidRow(
        column(3),
        column(8,
               uiOutput(ns("slider_es1"))
        ),
        column(1)
      ),
      br(),
      br(),
      fluidRow(
        column(3),
        column(8,
               uiOutput(ns("slider_es2"))
        ),
        column(1)
      ),
      br(),
      br(),
      fluidRow(
        column(3),
        column(8,
               uiOutput(ns("slider_es3"))
        ),
        column(1)
      ),

      actionButton(ns("conf3"), "Next task", class='btn-primary')
    )

  )
}

#' ahp_single Server Functions
#'
#' @noRd
mod_ahp_single_server <- function(id, userID, siteID, es_all, table_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reg_full<-es_all%>%filter(esSECTION == "regulating")
    reg <- unlist(as.vector(reg_full%>%distinct(esID)))
    reg_comb<-as.data.frame(t(combn(reg, 2)))
    reg_comb$ind<-rep(1,nrow(reg_comb))

    cul_full<-es_all%>%filter(esSECTION == "cultural")
    cul<-cul_full%>%distinct(esID)
    cul <- unlist(as.vector(cul))
    cul_comb<-as.data.frame(t(combn(cul, 2)))
    cul_comb$ind<-rep(2,nrow(cul_comb))

    prov_full<-es_all%>%filter(esSECTION == "provisioning")
    prov<-prov_full%>%distinct(esID)
    prov <- unlist(as.vector(prov))
    prov_comb<-as.data.frame(t(combn(prov, 2)))
    prov_comb$ind<-rep(3,nrow(prov_comb))

    all_comb<-rbind(reg_comb,cul_comb,prov_comb)
    all_comb$ind<-as.integer(all_comb$ind)

    es1<-all_comb%>%filter(ind == 1)
    es2<-all_comb%>%filter(ind == 2)
    es3<-all_comb%>%filter(ind == 3)


    ## first ES block
    output$slider_es1 <- shiny::renderUI({

      # ns <- session$ns
      lapply(1:nrow(es1),function(n){

        pair_id <- paste0(es1[n,]$V1,"_",es1[n,]$V2)
        pair_lable<-""
        choice1<-paste0(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME), " is overwhelmingly more important")
        choice2<-paste0(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME), " is very strongly more important")
        choice3<-paste0(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME), " is strongly more important")
        choice4<-paste0(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME), " is moderately more important")

        choice5<-paste0(reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME), " is overwhelmingly more important")
        choice6<-paste0(reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME), " is very strongly more important")
        choice7<-paste0(reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME), " is strongly more important")
        choice8<-paste0(reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME), " is moderately more important")

        choices = c(choice1,
                    choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5)

        es_id_left<-es1[n,]$V1
        es_id_right<-es1[n,]$V2
        mod_id_left<-paste0(es_id_left,"_mod_left")
        mod_id_right<-paste0(es_id_right,"_mod_right")


        tagList(
          bsModal(id = ns(paste0(mod_id_left,n)), title = reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME), trigger = ns(paste0("l_",es_id_left)),
                  h6(es_id_left),
                  tags$ol(
                    tags$li(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es1[n,]$V1,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),

          bsModal(id = ns(paste0(mod_id_right,n)), title = reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME), trigger = ns(paste0("r_",es_id_right)),
                  h6(es_id_right),
                  tags$ol(
                    tags$li(reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es1[n,]$V2,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),


          column(6, actionLink(inputId = ns(paste0("l_",es_id_left)), label = reg_full%>%filter(esID %in% es1[n,]$V1)%>%select(esNAME))),
          column(6, actionLink(inputId = ns(paste0("r_",es_id_right)), label = reg_full%>%filter(esID %in% es1[n,]$V2)%>%select(esNAME))),


          sliderTextInput(ns(pair_id),
                          pair_lable,
                          grid = F,
                          force_edges = F,
                          choices = choices,
                          width = "75%",
                          selected = choices[5]
          ))#/slider
      })#/lapply
    })#/ui es 1

    ## second ES block
    output$slider_es2 <- shiny::renderUI({
      # ns <- session$ns
      lapply(1:nrow(es2),function(m){

        pair_id <- paste0(es2[m,]$V1,"_",es2[m,]$V2)
        pair_lable<-""
        choice1<-paste0(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME), " is overwhelmingly more important")
        choice2<-paste0(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME), " is very strongly more important")
        choice3<-paste0(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME), " is strongly more important")
        choice4<-paste0(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME), " is moderately more important")

        choice5<-paste0(cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME), " is overwhelmingly more important")
        choice6<-paste0(cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME), " is very strongly more important")
        choice7<-paste0(cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME), " is strongly more important")
        choice8<-paste0(cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME), " is moderately more important")



        es_id_left<-es2[m,]$V1
        es_id_right<-es2[m,]$V2
        mod_id_left<-paste0(es_id_left,"_mod_left")
        mod_id_right<-paste0(es_id_right,"_mod_right")
        choices<-c(choice1,
                   choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5)

        tagList(
          bsModal(id = ns(paste0(mod_id_left,m)), title = cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME), trigger = ns(paste0("l_",es_id_left)),
                  h6(es_id_left),
                  tags$ol(
                    tags$li(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es2[m,]$V1,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),

          bsModal(id = ns(paste0(mod_id_right,m)), title = cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME), trigger = ns(paste0("r_",es_id_right)),
                  h6(es_id_right),
                  tags$ol(
                    tags$li(cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es2[m,]$V2,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),


          column(6,actionLink(inputId = ns(paste0("l_",es_id_left)), label = cul_full%>%filter(esID %in% es2[m,]$V1)%>%select(esNAME))),
          column(6, actionLink(inputId = ns(paste0("r_",es_id_right)), label = cul_full%>%filter(esID %in% es2[m,]$V2)%>%select(esNAME))),

          sliderTextInput(ns(pair_id),
                          pair_lable,
                          grid = F,
                          force_edges = F,
                          choices = choices,
                          width = "75%",
                          selected = choices[5]
          ))#/slider
      })#/lapply

    })#/ui

    ## third ES block
    output$slider_es3 <- shiny::renderUI({
      lapply(1:nrow(es3),function(o){

        pair_id <- paste0(es3[o,]$V1,"_",es3[o,]$V2)
        # pair_lable<-paste0(reg_full%>%filter(esID %in% es3[n,]$V1)%>%select(esNAME)," - ",reg_full%>%filter(esID %in% es3[n,]$V2)%>%select(esNAME))
        pair_lable<-""
        choice1<-paste0(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME), " is overwhelmingly more important")
        choice2<-paste0(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME), " is very strongly more important")
        choice3<-paste0(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME), " is strongly more important")
        choice4<-paste0(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME), " is moderately more important")

        choice5<-paste0(prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME), " is overwhelmingly more important")
        choice6<-paste0(prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME), " is very strongly more important")
        choice7<-paste0(prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME), " is strongly more important")
        choice8<-paste0(prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME), " is moderately more important")



        es_id_left<-es3[o,]$V1
        es_id_right<-es3[o,]$V2
        mod_id_left<-paste0(es_id_left,"_mod_left")
        mod_id_right<-paste0(es_id_right,"_mod_right")
        choices<-c(choice1,
                   choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5)

        tagList(
          bsModal(id = ns(paste0(mod_id_left,o)), title = prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME), trigger = ns(paste0("l_",es_id_left)),
                  h6(es_id_left),
                  tags$ol(
                    tags$li(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es3[o,]$V1,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),

          bsModal(id = ns(paste0(mod_id_right,o)), title = prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME), trigger = ns(paste0("r_",es_id_right)),
                  h6(es_id_right),
                  tags$ol(
                    tags$li(prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esDESCR)),
                    tags$figure(
                      class = "centerFigure",
                      tags$img(
                        src = paste0(es3[o,]$V2,".jpg"),
                        width = 300,
                        alt = "Picture of an astragalus (bone die)"
                      ),
                      tags$figcaption("Image of Astragalus by Yaan, 2007")
                    )
                  ), easyClose = TRUE, footer = NULL),


          column(6, actionLink(inputId = ns(paste0("l_",es_id_left)), label = prov_full%>%filter(esID %in% es3[o,]$V1)%>%select(esNAME))),
          column(6, actionLink(inputId = ns(paste0("r_",es_id_right)), label = prov_full%>%filter(esID %in% es3[o,]$V2)%>%select(esNAME))),

          sliderTextInput(ns(pair_id),
                          pair_lable,
                          grid = F,
                          force_edges = F,
                          choices = choices,
                          width = "75%",
                          selected = choices[5]
          ))#/slider
      })#/lapply
    })


    ### store the values
    observeEvent(input$conf3,{
      val_list1<-list()
      val_list2<-list()
      val_list3<-list()

      res1<-lapply(1:nrow(es1),function(a){
        var<-paste0(es1[a,]$V1,"_",es1[a,]$V2)
        val_list1[[a]]<-input[[var]]
        return(val_list1)
      })
      comp_val1 <- unlist(res1)

      res2<-lapply(1:nrow(es2),function(b){
        var2<-paste0(es2[b,]$V1,"_",es2[b,]$V2)
        val_list2[[b]]<-input[[var2]]
        return(val_list2)
      })
      comp_val2 <- unlist(res2)

      res3<-lapply(1:nrow(es3),function(c){
        var3<-paste0(es3[c,]$V1,"_",es3[c,]$V2)
        val_list3[[c]]<-input[[var3]]
        return(val_list3)
      })
      comp_val3 <- unlist(res3)

      es1$comp_val<-comp_val1
      es1$recode <- rep(0,nrow(es1))
      es2$comp_val<-comp_val2
      es2$recode <- rep(0,nrow(es2))
      es3$comp_val<-comp_val3
      es3$recode <- rep(0,nrow(es3))

      es<-rbind(es1,es2,es3)

      n<-lapply(1:nrow(es),function(a){
        if(es[a,]$comp_val == "both are equally important"){
          es[a,]$recode <- 1
        } else if(grepl("is overwhelmingly more important",es[a,]$comp_val) == TRUE){
          es[a,]$recode <- 8
        } else if(grepl("is very strongly more important",es[a,]$comp_val) == TRUE){
          es[a,]$recode <- 6
        } else if(grepl("is strongly more important",es[a,]$comp_val) == TRUE){
          es[a,]$recode <- 4
        } else if(grepl("is moderately more important",es[a,]$comp_val) == TRUE){
          es[a,]$recode <- 2
        }

        if(grepl(es[a,]$V1,es[a,]$comp_val) == TRUE){
          es[a,]$recode<-es[a,]$recode*-1
        } else {
          es[a,]$recode<-es[a,]$recode
        }
      })

      es$recode <- as.integer(unlist(n))
      es$userID<-rep(userID,nrow(es))
      es$siteID<-rep(siteID,nrow(es))
      es<-es%>%select(userID,siteID,V1,V2,comp_val,recode,ind)
      colnames(es)<-c("userID","siteID","es_left","es_right","selection_text","selection_val", "ahp_section")
      insert_upload_job(table_con$project, table_con$dataset, "es_pair", es)

    })
    cond <- reactive({input$conf3})

    return(cond)

  })
}

## To be copied in the UI
# mod_ahp_single_ui("ahp_single_1")

## To be copied in the server
# mod_ahp_single_server("ahp_single_1")
