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
#' @import leaflet.extras
#' @import mapedit
#' @import bigrquery
#' @noRd
#'
bq_auth(
  path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/eu-wendy-92974cdf189d.json"
)


bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/eu-wendy-92974cdf189d.json")
con <- dbConnect(
  bigrquery::bigquery(),
  project = "eu-wendy",
  dataset = "integrated_wendy",
  billing = "eu-wendy"
)
study_site<-tbl(con, "study_site")
studies<-study_site%>%select(siteID)%>%collect()

app_server <- function(input, output, session) {
  # studies<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/study_geom.rds")

  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")

  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({})
  )
  # hideTab(inputId = "inTabset", target = "p2")

  observeEvent(input$sub0,{
    if(input$site_id %in% studies$siteID){
      # rgee::ee_Initialize()
      # rgee::ee_install_set_pyenv("C:/Users/reto.spielhofer/AppData/Local/r-miniconda/envs/rgee/python.exe")
      # reticulate::use_python("C:/Users/reto.spielhofer/AppData/Local/r-miniconda/envs/rgee/python.exe")
      rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')
      # bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/PAREUS/pareus-4c675ffa9441.json")
      # con <- dbConnect(
      #   bigrquery::bigquery(),
      #   project = "pareus",
      #   dataset = "soc_eco_map",
      #   billing = "pareus"
      # )
      updateTabsetPanel(session, "inTabset",
                        selected = "p1")
      hideTab(inputId = "inTabset",
              target = "p0")
      showTab(inputId = "inTabset", target = "p1")

    }
  })

  site_id<-eventReactive(input$sub0,{
    if(input$site_id  %in% studies$siteID){
      site_id<-as.character(studies%>%filter(studyID==input$site_id)%>%select(siteID))

    }else{
      study_id<-NULL
    }

  })
  # proj_id<-eventReactive(input$sub0,{
  #   if(input$site_id %in% studies$siteID){
  #     proj_id<-as.character(studies%>%filter(studyID==input$study_id)%>%select(projID))
  #
  #   }else{
  #     proj_id<-NULL
  #   }
  #
  # })

  # ee_stud_geom<-eventReactive(input$sub0,{
  #   if(input$site_id %in% studies$siteID){
  #     assetid <- paste0('eu-wendy/study_sites/', input$site_id)
  #     sf_stud_geom<-ee_as_sf(assetid
  #     )
  #     sf_stud_geom<-st_as_sf(studies%>%filter(studyID==input$study_id)%>%select(geometry))
  #     ee_stud_geom <- sf_as_ee(x = sf_stud_geom)
  #
  #
  #   }else{
  #     ee_stud_geom<-NULL
  #   }
  #
  # })



  sf_stud_geom<-eventReactive(input$sub0,{
    if(input$site_id %in% studies$siteID){
      # sf_stud_geom<-st_as_sf(studies%>%filter(studyID==input$study_id)%>%select(geometry))
          assetid <- paste0('eu-wendy/study_sites/', input$site_id)
          sf_stud_geom<-ee_as_sf(assetid
          )



    }else{
      sf_stud_geom<-NULL
    }

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
    if(input$study_id %in% studies$studyID){
      ee_stud_geom<-ee_stud_geom()
    lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
    lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
    lulc<-lulc$clip(ee_stud_geom)

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


    }else{
      comb<-NULL
    }

  })



  stud_es<-eventReactive(input$sub0,{
    req(site_id)
    site_id<-site_id()
    if(input$site_id %in% studies$siteID){
      stud_es<-es_study%>%filter(siteID == site_id)%>%collect()

    }

  })



  userID<-eventReactive(input$sub1,{
    userID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")

  })

  #questionnaire module
  observeEvent(input$sub1,{
    req(userID)
    userID<-userID()
    proj_id<-proj_id()
    study_id<-study_id()
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p2")

    rv$u<-mod_questionnaire_server("questionnaire",userID, proj_id, study_id)

  })



  #training module
  observeEvent(rv$u(),{
    n_es<-n_es()
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset",
            target = "p2")
    showTab(inputId = "inTabset", target = "p3")
    rv$v<-mod_training_server("training_1")
  })

  # num_tabs <- reactiveVal(4)
  num_tabs<-eventReactive(input$sub0,{
    stud_es<-stud_es()
    num_tabs<-as.numeric(nrow(stud_es))
    print(num_tabs)
  })


  # observeEvent(rv$v(),{
  observeEvent(input$sub1,{
    rand_es_sel<-stud_es()%>%slice_sample(n=num_tabs(), replace = F)
    output$dynamic_tabs <- renderUI({

      do.call(tabsetPanel, c(id='inTab',lapply(1:num_tabs(), function(i) {
        tabPanel(
          paste("Tab", i),
          h3(paste("This is content for Tab", i)),
          mod_delphi_round1_ui(paste0("delphi_round1_",i))
        )
      })))

    })

    lapply(1:num_tabs(), function(i) {
      mod_delphi_round1_server(paste0("delphi_round1_",i), isolate(ee_stud_geom()), isolate(ee_bbox_geom()), isolate(sf_stud_geom()), isolate(comb()),rand_es_sel[i,],
                               isolate(userID()), isolate(study_id()), isolate(proj_id()))
    })


  })


}
