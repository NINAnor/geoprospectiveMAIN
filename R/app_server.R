#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import sf
#' @import stringi
#' @import rgee
#' @import leaflet
#' @import leafem
#' @import leaflet.extras
#' @import mapedit
#' @import bigrquery
#' @import DT
#' @import shinycssloaders
#' @import tibble
#' @import leafpop
#' @import mapview
#' @import leaflet.extras2
#' @import shinyWidgets
#' @import tidyverse
#' @noRd
#'
bq_auth(
  path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/eu-wendy-92974cdf189d.json"
)
mode<-"integrated"
project<-"eu-wendy"
table_con<-data.frame(
  project = project,
  dataset = paste0(mode,"_wendy"),
  billing =project
)


con <- dbConnect(
  bigrquery::bigquery(),
  project = table_con$project,
  dataset = table_con$dataset,
  billing = table_con$billing
)
study_site<-tbl(con, "study_site")
studies<-study_site%>%select(siteID,siteTYPE,siteNMAPPING)%>%collect()



app_server <- function(input, output, session) {
  # studies<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/study_geom.rds")

  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  hideTab(inputId = "inTabset", target = "p6")

  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({}),
    m1 = reactive({}),
    m2 = reactive({}),
    m3 = reactive({})
  )
  # hideTab(inputId = "inTabset", target = "p2")

  # validate site id
  observeEvent(input$site_id,{
    if(input$site_id %in% studies$siteID){
      output$cond_0<-renderUI(
        actionButton("sub0","load site")
      )
    }else{
      output$cond_0<-renderUI(
        h5("invalid id")
      )
    }
  })

  #switch tab 1
  observeEvent(input$sub0,{
      # rgee::ee_Initialize()
      # rgee::ee_install_set_pyenv("C:/Users/reto.spielhofer/AppData/Local/r-miniconda/envs/rgee/python.exe")
      # reticulate::use_python("C:/Users/reto.spielhofer/AppData/Local/r-miniconda/envs/rgee/python.exe")
      rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')

      updateTabsetPanel(session, "inTabset",
                        selected = "p1")
      hideTab(inputId = "inTabset",
              target = "p0")
      showTab(inputId = "inTabset", target = "p1")
  })

  site_id<-eventReactive(input$sub0,{
      site_id<-as.character(studies%>%filter(siteID==input$site_id)%>%select(siteID))
  })

  site_type<-eventReactive(input$sub0,{
    site_type<-as.character(studies%>%filter(siteID==input$site_id)%>%select(siteTYPE))
  })

  sf_stud_geom<-eventReactive(input$sub0,{
      # sf_stud_geom<-st_as_sf(studies%>%filter(studyID==input$study_id)%>%select(geometry))
          assetid <- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
          assetid <- ee$FeatureCollection(assetid)
          sf_stud_geom<-ee_as_sf(assetid)
  })

  ee_bbox_geom<-eventReactive(input$sub0,{
    req(sf_stud_geom)
    sf_stud_geom<-sf_stud_geom()
    bb_sf<-st_bbox(sf_stud_geom)

    rlist <- list(xmin = as.numeric(bb_sf[1]), xmax = as.numeric(bb_sf[3]),ymin = as.numeric(bb_sf[2]), as.numeric(bb_sf[4]))
    ROI <- c(rlist$xmin, rlist$ymin,
             rlist$xmax, rlist$ymin,
             rlist$xmax, rlist$ymax,
             rlist$xmin, rlist$ymax,
             rlist$xmin, rlist$ymin)
    ee_bbox_geom <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
      list() %>%
      st_polygon() %>%
      st_sfc() %>%
      st_set_crs(4326) %>%
      sf_as_ee()
  })

  comb<-eventReactive(input$sub0,{
    site_geom_ee<- paste0('eu-wendy/study_sites/', input$site_id)
    lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
    lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
    lulc<-lulc$clip(site_geom_ee)

#
#     acc_pat<-paste0(ee_get_assethome(), '/acc_old')
#     acc<-ee$Image(acc_pat)
#     acc<-acc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
#
#     nat_pat<-paste0(ee_get_assethome(), '/natu')
#     nat<-ee$Image(nat_pat)
#     nat<-nat$clip(ee_stud_geom)
#     nat<-nat$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
#     nat<-nat$rename("nat")

    # combine unique class count wdw and lulc
    # comb<-ee$Image$cat(lulc,acc, nat)
    comb<-ee$Image$cat(lulc)

  })

  stud_es<-eventReactive(input$sub0,{
    req(site_id)
    site_id<-site_id()
    es_study<-tbl(con, "es_study")
    stud_es<-es_study%>%filter(siteID == site_id)%>%collect()

  })

  num_tabs<-eventReactive(input$sub0,{
    req(site_id)
    site_id<-site_id()
    num_tabs<-as.integer(studies%>%filter(siteID == site_id)%>%select(siteNMAPPING))
  })

  # email submitted
  userID<-eventReactive(input$sub1,{
    userID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  })

  #questionnaire module and safe conf user
  observeEvent(input$sub1,{
    req(userID)
    userID<-userID()
    site_id<-site_id()
    sf_stud_geom<-sf_stud_geom()
    site_type<-site_type()
    ## save user_conf
    user_conf<-data.frame(
      userID = userID,
      userMAIL = input$email,
      userTLOG = Sys.time()
    )
    insert_upload_job(table_con$project, table_con$dataset, "user_conf", user_conf)

    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    rv$u<-mod_questionnaire_server("questionnaire",userID, site_id, sf_stud_geom, site_type, table_con)


  })



  # training module
  observeEvent(rv$u(),{
    # num_tabs<-num_tabs()
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset",
            target = "p2")
    showTab(inputId = "inTabset", target = "p3")
    rv$v<-mod_training_server("training_1")
  })


  observeEvent(rv$v(),{
    rand_es_sel<-stud_es()
    ee_bbox_geom<-ee_bbox_geom()
    sf_stud_geom<-sf_stud_geom()
    comb<-comb()
    userID<-userID()
    site_id<-site_id()
    site_type<-site_type()
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
    hideTab(inputId = "inTabset",
            target = "p3")
    showTab(inputId = "inTabset", target = "p4")
    rv$m1<-mod_delphi_round1_server("mapping_1",
                                    ee_bbox_geom,
                                    sf_stud_geom,
                                    comb,rand_es_sel,1,
                                    userID,
                                    site_id,
                                    table_con)
  })

  observeEvent(rv$m1(),{
    rand_es_sel<-stud_es()
    updateTabsetPanel(session, "inTabset",
                      selected = "p5")
    hideTab(inputId = "inTabset",
            target = "p4")
    showTab(inputId = "inTabset", target = "p5")
    rv$m2<-mod_delphi_round1_server("mapping_2",isolate(ee_bbox_geom()), isolate(sf_stud_geom()),
                                    isolate(comb()),rand_es_sel[2,],isolate(userID()), isolate(site_id()),
                                    isolate(site_type()), table_con)
  })

  observeEvent(rv$m2(),{
    rand_es_sel<-stud_es()
    updateTabsetPanel(session, "inTabset",
                      selected = "p6")
    hideTab(inputId = "inTabset",
            target = "p5")
    showTab(inputId = "inTabset", target = "p6")
    rv$m3<-mod_delphi_round1_server("mapping_3",isolate(ee_bbox_geom()), isolate(sf_stud_geom()),
                                    isolate(comb()),rand_es_sel[3,],isolate(userID()), isolate(site_id()),
                                    isolate(site_type()), table_con)
  })


    # hideTab(inputId = "inTabset", target = "p3")
    # appendTab("inTabset", tabPanel(1, tags$p(paste("I'm tab", 1))), select=TRUE,
    #           rv$w<-mod_delphi_round1_server(paste0("delphi_round1_",i), isolate(ee_bbox_geom()), isolate(sf_stud_geom()),
    #                                      isolate(comb()),rand_es_sel[i,],isolate(userID()), isolate(site_id()),
    #                                      isolate(site_type()), table_con)
    #           )
    # output$dynamic_tabs <- renderUI({
    #
    #   do.call(tabsetPanel, c(id='inTab',lapply(1:num_tabs(), function(i) {
    #     tabPanel(
    #       paste("Tab", i),
    #       h3(paste("This is content for Tab", i)),
    #       mod_delphi_round1_ui(paste0("delphi_round1_",i))
    #     )
    #   })))
    #
    # })
    # lapply(hideTab(inputId = "inTab", target = "p1"))
    #
    #
    # lapply(1:num_tabs(), function(i) {
    #   mod_delphi_round1_server(paste0("delphi_round1_",i), isolate(ee_bbox_geom()), isolate(sf_stud_geom()),
    #                            isolate(comb()),rand_es_sel[i,],isolate(userID()), isolate(site_id()),
    #                            isolate(site_type()), table_con)
    # })





}
