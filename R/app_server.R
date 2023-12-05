#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
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
#' @import shinyBS
#' @import shinyWidgets
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
studies<-study_site%>%select(siteID,siteTYPE,siteNMAPPING,siteSTATUS)%>%collect()

es_descr<-tbl(con,"es_descr")
es_descr<-es_descr%>%collect()

app_server <- function(input, output, session) {
  # studies<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/study_geom.rds")

  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")

  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({}),
    x = reactive({}),
    y = reactive({}),
    z = reactive({})
  )

  # validate site id
  observeEvent(input$site_id,{
    if(input$site_id %in% studies$siteID & studies$siteSTATUS == "created_avtive"){
      output$cond_0<-renderUI(
        actionButton("sub0","load site")
      )
    }else{
      output$cond_0<-renderUI(
        h5("invalid id or study not active, contact the administrator")
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

  # ee_bbox_geom<-eventReactive(input$sub0,{
  #   req(sf_stud_geom)
  #   sf_stud_geom<-sf_stud_geom()
  #   bb_sf<-st_bbox(sf_stud_geom)
  #
  #
  #
  #   ee_bbox_geom <- ee$Geometry$Rectangle(
  #     coords = c(as.numeric(sf_stud_geom[1]), as.numeric(sf_stud_geom[2]), as.numeric(sf_stud_geom[3]), as.numeric(sf_stud_geom[4])),
  #     proj = "EPSG:4326",
  #     geodesic = FALSE
  #   )
  #
  # })

  comb<-eventReactive(input$sub0,{
    site_geom_ee<- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
    site_geom_ee <- ee$FeatureCollection(site_geom_ee)
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
    num_tabs<-as.numeric(studies%>%filter(siteID == site_id)%>%select(siteNMAPPING))
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

  #######################
  ### create N tabs

  observeEvent(rv$v(),{
    hideTab(inputId = "inTabset",
            target = "p3")
    num_tabs<-num_tabs()
    output$tabs <- renderUI({
      do.call(tabsetPanel, c(id="tabs_content",
                             lapply(1:num_tabs, function(i) {
                               tabPanel(title = paste("Mapping ", i), value = paste0("map_", i),
                                        mod_delphi_round1_ui(paste0("mapping_",i)),

                               )#/tabpanel
                             })#/lapply
      ))#/do.call
    })#/UI render
  })


  ## hide tabs
  observeEvent(input$tabs_content, {
    num_tabs<-num_tabs()
    rand_es_sel<-stud_es()
    sf_stud_geom<-sf_stud_geom()
    comb<-comb()
    userID<-userID()
    site_id<-site_id()
    site_type<-site_type()

    for (i in 2:num_tabs) {
      runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
    }

    lapply(1:num_tabs, function(i) {
      rv<-reactiveValues(
        a = reactive({})
      )

      rv$a<-mod_delphi_round1_server(paste0("mapping_",i),
                                     sf_stud_geom,
                                     comb,
                                     rand_es_sel,
                                     as.numeric(i),
                                     userID,
                                     site_id,
                                     table_con)
      #reactive value from module as event
      observeEvent(rv$a(), {
        next_tab <- i+1
        runjs(paste("$('.nav-tabs li:nth-child(", next_tab, ")').show();"))
        if(next_tab<=num_tabs){
          updateTabsetPanel(session, "tabs_content", selected=paste0("map_",next_tab) )
          runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
        }else{
          removeUI("#tabs")
          output$ahp_group<-renderUI({
            tagList(
              mod_ahp_group_ui("ahp_group_1")
            )
          })
        }

      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  }, once = TRUE)


  #######################
  rv$x <- mod_ahp_group_server("ahp_group_1", isolate(userID()), isolate(site_id()), table_con)

  observeEvent(rv$x(),{
    removeUI("#ahp_group")
    output$ahp_single<-renderUI({
      tagList(
        mod_ahp_single_ui("ahp_single_1")

      )
    })
  })
  rv$y <- mod_ahp_single_server("ahp_single_1", isolate(userID()), isolate(site_id()),es_descr, table_con)

  observeEvent(rv$y(),{
    removeUI("#ahp_single")
    output$ahp_dist<-renderUI({
      tagList(
        mod_dist_impact_ui("dist_impact_1")
      )
    })

  })
  rv$z <- mod_dist_impact_server("dist_impact_1", isolate(userID()), isolate(site_id()), isolate(stud_es()), table_con)
  #
  #
  observeEvent(rv$z(),{
    removeUI("#ahp_dist")
    output$final<-renderUI({
      tagList(
        h3("This is the end of the study, you can now close the browser. Thank you very much for your participation.")
      )
    })

  })

}
