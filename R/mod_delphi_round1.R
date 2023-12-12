#' delphi_round1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_delphi_round1_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      # title
      uiOutput(ns("title_es")),
      br(),
      fluidRow(
        column(5,
               textOutput(ns("descr_es"))),
        column(2),
        column(5,
               # uiOutput(ns("image_es"))
               )
      ),
      br(),
      # questions of importance
      uiOutput(ns("imp_text")),
      sliderInput(ns("imp_own"), "... for you personally in this area?",
                  min = 0, max = 5, value = 3),

      sliderInput(ns("imp_other"), "... for others and the society in this area?",
                  min = 0, max = 5, value = 3),

      br(),
      # are you able to map the ES?
      uiOutput(ns("map_poss")),
      br(),
      # if ES not mappable
      conditionalPanel(
        condition = "input.map_poss == 'No'", ns = ns ,
        selectizeInput(ns("expert_map"),label="Would you trust an expert map",choices = c("Yes","No"),options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')))
        # actionButton(ns("submit2"),"save")
      ),

      conditionalPanel(
        condition = "input.expert_map != ''", ns=ns,
        actionButton(ns("confirm"), "Next task", class='btn-primary')
      )
    )
  )
}

callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)

#' delphi_round1 Server Functions
#'
#' @noRd
mod_delphi_round1_server <- function(id, sf_stud_geom, comb, rand_es_sel, order, userID, site_id, table_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mapTIME_start <-Sys.time()
    order<-as.numeric(order)
    rand_es_sel<-rand_es_sel[order,]
    ## the band names of the predictor variables (might be adjusted in the future if predictors can be selected according to project)
    # bands <- list("landcover","b1","nat")
    bands <- list("landcover")
    ### visualization parameter for img, mean
    cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
    maxentviz = list(bands= 'probability',min= 0, max= 1, palette= cols)
    rv1<-reactiveValues(
      u = reactive({})
    )

    ## descriptives of ecosystem services
    output$title_es<-renderUI(h5(rand_es_sel$esNAME))
    output$descr_es<-renderText(rand_es_sel$esDESCR)
    output$res_text<-renderUI(h6(paste0("Your personal map of ",rand_es_sel$esNAME)))
    output$es_quest_where<-renderUI(h6(paste0("Where do you find good areas for ", rand_es_sel$esNAME,"?")))
    output$es_quest_how<-renderUI(h6(paste0("How do you rate the quality of ",rand_es_sel$esNAME, " for each of area")))
    output$imp_text<-renderUI(h6(paste0("How important is ", rand_es_sel$esNAME,"...")))

    # image is not clear yet how to do most efficiently
    # output$image_es<-renderUI({
    #   tags$figure(
    #     class = "centerFigure",
    #     tags$img(
    #       src = paste0(esID,".jpg"),
    #       width = 600,
    #       alt = "Picture of an astragalus (bone die)"
    #     ),
    #     tags$figcaption("Image of Astragalus by Yaan, 2007")
    #   )
    # })


    # UI rendered to ask if able to map ES
    output$map_poss<-renderUI({
      lable <- paste0("Are you able to map ", rand_es_sel$esNAME,"?")
      selectizeInput(ns("map_poss"),label=lable,choices = c("Yes","No"),options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    })

    map<-leaflet(sf_stud_geom)%>%
      addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0)%>%
      addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
      addDrawToolbar(targetGroup='drawPoly',
                     polylineOptions = F,
                     polygonOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = T,
                     singleFeature = FALSE,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))

    # second for results
    map_res<-leaflet(sf_stud_geom)%>%
      addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
      addDrawToolbar(targetGroup='drawPoly',
                     polylineOptions = F,
                     polygonOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = F,
                     singleFeature = FALSE,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))

    observeEvent(input$confirm,{
      rv1$u <-reactive({1})
    })

    rv<-reactiveValues(
      edits = reactive({})
    )

    ## call the edit map module from the mapedit package
    # edits<-mapedit::editMap(map, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))

    observeEvent(input$map_poss,{
      if(input$map_poss == "Yes"){

        rv$edits<-callModule(
          module = editMod,
          leafmap = map,
          id = "map_sel")
        # print(rv$edits())

        insertUI(selector =paste0("#",ns("map_poss")),
                 where = "afterEnd",
                 ui=tagList(
                   uiOutput(ns("es_quest_where")),
                   br(),
                   editModUI(ns("map_sel")),

                   htmlOutput(ns("overlay_result")),
                   uiOutput(ns("btn1")),

                 )
        )

      }#/if yes
    })#/map_poss

    ## if mapping not possible: (save results has to be added!)
    observeEvent(input$expert_map,{
      mapTIME_end <-Sys.time()
      if(input$expert_map !=""){
        train_param<-list(
          esID = rand_es_sel$esID,
          userID = userID,
          siteID = site_id,
          imp_acc= as.integer(0),
          imp_nat= as.integer(0),
          imp_lulc = as.integer(0),
          imp_own = as.integer(input$imp_own),
          imp_other = as.integer(input$imp_other),
          training_area = as.integer(0),
          n_poly = as.integer(0),
          blog = "NA",
          poss_mapping = "No",
          expert_trust = input$expert_map,
          mapping_order = as.numeric(order),
          extrap_RMSE = 0,
          extrap_accIMP = 0,
          extrap_lulcIMP = 0,
          extrap_natIMP = 0,
          mapTIME_start = mapTIME_start,
          mapTIME_end = mapTIME_end
        )
        train_param<-as.data.frame(train_param)
        insert_upload_job(table_con$project, table_con$dataset, "es_mappingR1", train_param)
        removeUI(
          selector = paste0("#",ns("expert_map"))
        )
      }


    })

    ##  check for intersecting training / study area and poly areas in general
    observe({
      req(rv$edits)
      rectangles <- rv$edits()$finished

      n_poly<-nrow(as.data.frame(rectangles))

      #with res of 250m grid we can sample at least 10 pts with variaton within 0.6km2
      A_min<-250*250*sqrt(10)
      A_max<-0.05*round(as.numeric(st_area(sf_stud_geom)),0)

      if(n_poly==1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_stud_geom)))
        if(n_within<n_poly){
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
          })
          removeUI(
            selector = paste0("#",ns("savepoly")))
        }else{
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>The area of the polygon is too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))

          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>The area of the polygon is too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))


          }else{
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })

          }

        }

      }else if (n_poly>1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_stud_geom)))
        n_inter<-nrow(as.data.frame(st_intersects(rectangles)))
        q=n_inter-n_poly
        if(q!=0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b><li>Place your last polygon completely into the the study area<li/><li>Remove overlays<li/></font>")

          })
        }else if(q==0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your last polygon completely into the the study area<li/></font>")

          })
        }else if(q!=0 & n_within==n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Remove overlays from last polygon<li/></font>")

          })
        }else if(q==0 & n_within==n_poly){
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>The area of the last polygon was too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))

          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>The area of the last polygon was too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))


          }else{
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })

          }
        }
      }

    })

    #remove mapping question as soon as decided
    observeEvent(input$map_poss,{
      if(input$map_poss !=""){
        removeUI(
          selector = paste0("#",ns("map_poss"))
        )
        removeUI(
          selector = paste0("#",ns("imp_own"),"-label"))
        removeUI(
          selector = paste0("#",ns("imp_own")))
        removeUI(
          selector = paste0("#",ns("imp_other"),"-label"))
        removeUI(
          selector = paste0("#",ns("imp_other")))
        removeUI(
          selector = paste0("#",ns("imp_text")))
      }
    })

    ### confirm the drawings and render the results table
    tbl_out<-eventReactive(input$savepoly,{
      tbl<-rv$edits()$finished

      # do not give possibility to submit map without polygons
      req(tbl, cancelOutput = FALSE)
      tbl<-tbl%>%st_drop_geometry()
      tbl$value_es<-rep(NA,(nrow(tbl)))
      tbl
    })

    ### confirm the drawings and render the leaflet map
    observeEvent(input$savepoly, {
      tbl<-tbl_out()
      polygon<-rv$edits()$finished

      # do not give possibility to submit map without polygons
      req(polygon, cancelOutput = FALSE)

      ## render new UI of polygon map and slider remove rest
      insertUI(
        selector = paste0("#",ns("savepoly")),
        where = "afterEnd",
        ui = tagList(
          uiOutput(ns("es_quest_how")),
          br(),
          leafletOutput(ns("map_res")),
          br(),
          uiOutput(ns("slider")),
          br(),
          # a short expl. why this sites
          uiOutput(ns("blogdescr")),
          textInput(ns("blog"), label = ""),
          br(),
          conditionalPanel(
            condition = "input.blog != ''", ns=ns,
            actionButton(ns("submit"),"save values")
          )
        )
      )

      removeUI(
        selector = paste0("#",ns("savepoly")))

      removeUI(
        selector = paste0("#",ns("map_sel"),"-map"))

      removeUI(
        selector = paste0("#",ns("es_quest_where")))



      cent_poly <- st_centroid(polygon)
      output$map_res<-renderLeaflet(map_res %>%
                                      addPolygons(data=polygon) %>%
                                      addLabelOnlyMarkers(data = cent_poly,
                                                          lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$`_leaflet_id`,
                                                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                      style = list(
                                                                                        "color" = "red",
                                                                                        "font-family" = "serif",
                                                                                        "font-style" = "bold",
                                                                                        "font-size" = "20px"
                                                                                      ))))

    ## create a slider for each of the polygons

    output$slider <- shiny::renderUI({
        ns <- session$ns
        tagList(
          paste0("The Nr. of the slider refer to the number of the rectangle in the map"),
          br(),
          br(),
          lapply(1:nrow(tbl),function(n){
            polynr <- tbl[n,]$`_leaflet_id`
            id<-paste0("id_",polynr)
            lable<-paste0("Polygon Nr in map: ",polynr)
            sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
          })
        )

      })
    })

    ## blog description
    output$blogdescr<-renderUI({
      h5(paste0("Please provide us a short explanation why you choosed these areas of good quality to provide ",rand_es_sel$esNAME))
    })

    ## keep mapping time
    mapTIME_end <-eventReactive(input$submit,{
      mapTIME_end <-Sys.time()
    })
    ## remove map UI and sliders show result
    observeEvent(input$submit, {

      insertUI(
        selector = paste0("#",ns("submit")),
        where = "afterEnd",
        ui = tagList(
          textOutput(ns("res_text")),
          br(),
          leafletOutput(ns("gee_map")),
          br(),
          uiOutput(ns("btn_cond"))

        )
      )

      removeUI(
        selector = paste0("#",ns("map_res"))
      )
      removeUI(
        selector = paste0("#",ns("es_quest_how"))
      )
      removeUI(
        selector = paste0("#",ns("slider"))
      )
      removeUI(
        selector = paste0("#",ns("blogdescr"))
      )
      removeUI(
        selector = paste0("#",ns("blog"))
      )
      removeUI(
        selector = paste0("#",ns("submit"))
      )

    })

    ### predict probability of ES with maxent, save prediction, save poly and save drawing summaries on gee/bq

    ### gather poly
    prediction<-eventReactive(input$submit, {
      req(mapTIME_end)
      mapTIME_end<-mapTIME_end()

      withProgress(message = "save your drawings",value = 0.1,{
        polygon<-rv$edits()$finished
        req(polygon, cancelOutput = FALSE)
        polygon<-as.data.frame(polygon)
        polygon$es_value<-rep(NA,nrow(polygon))
        sliderval<-list()

        # extract the values from the slider
        res<-lapply(1:nrow(polygon),function(a){
          var<-paste0("id_",polygon[a,]$`_leaflet_id`)
          # print(as.numeric(input[[var]]))
          # polygon$es_value[a]<-as.numeric(input[[var]])
          sliderval[[a]]<-input[[var]]
          return(sliderval)
        })
        vecA <- unlist(res)

        # write attributes to geometry
        polygon$es_value <- vecA
        polygon$esID <- rep(rand_es_sel$esID,nrow(polygon))
        polygon$userID <- rep(userID,nrow(polygon))
        polygon$studyID <- rep(site_id,nrow(polygon))
        polygon$delphi_round<-rep(1, nrow(polygon))

        n_polys <-nrow(polygon)
        polygon<-st_as_sf(polygon)


        train_area<-as.numeric(sum(st_area(polygon)))

        # make ee object and save
        gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
        assetId<-paste0("projects/eu-wendy/assets/es_poly_ind/",as.character(site_id),"_",as.character(rand_es_sel$esID),"_",userID,"_1")
        start_time<-Sys.time()
        task_tab <- ee_table_to_asset(
          collection = gee_poly,
          description = "test upload ind_area",
          assetId = assetId
        )

        task_tab$start()
        #set features
        # gee_poly <- gee_poly$set('es_id', esID,
        #                          'userID', userID,
        #                          'siteID', siteID,
        #
        #                          'delphi_round', 1)


        ############ training pts
        incProgress(amount = 0.2,message = "prepare training data")

        #cellsize
        resolution<-250*250

        ## N background (outside poly points) according to area of extrapolation
        A_roi<-as.numeric(st_area(sf_stud_geom))

        # max pts for efficient extrapolation each 250x250 cell
        all_back_pts<- round(A_roi/resolution,0)

        ## although zooming on the map while drawing is limited, we assure that at least 10pts are within a poly
        min_in_pts<-10

        pts_out = st_sample(sf_stud_geom, all_back_pts,type="random")

       # don`t allow intersection with polygons
        pts_out <- st_difference(st_combine(pts_out), st_combine(polygon)) %>% st_cast('POINT')
        pts_out<-st_as_sf(pts_out)
        pts_out$inside<-rep(0,nrow(pts_out))


        # inside pts are area + es value weighted
        for (i in 1:nrow(polygon)) {
          A_tmp <- as.numeric(st_area(polygon[i,]))
          tmp_ratio<-A_tmp/A_roi
          tmp_pts<-round(all_back_pts*tmp_ratio,0)

          if(tmp_pts<=min_in_pts){
            tmp_pts<-min_in_pts
          }else{
            tmp_pts<-tmp_pts
          }
          # npts in this poly must be max_pts*tmp_ratio*es_value
          tmp_pts = st_sample(polygon[i,], tmp_pts*polygon[i,]$es_value,type="random")
          tmp_pts<-st_as_sf(tmp_pts)
          tmp_pts$inside<-rep(1,nrow(tmp_pts))
          if(i==1){
            pts_in<-tmp_pts
          }else{
            pts_in<-rbind(pts_in,tmp_pts)
          }

        }
        pts_ee<-rbind(pts_out,pts_in)
        # ee object of sampling pts 6k pts = 7sec
        pts_ee<-rgee::sf_as_ee(pts_ee, via = "getInfo")

        # define target bands of comb (indep. var) and sample vars by pts
        pts_ee = comb$select(bands)$sampleRegions(collection= pts_ee,
                                                  properties = list("inside"),
                                                  geometries = T
        )

        ############ maxent
        incProgress(amount = 0.2,message = "calculate map")

        mEntclass = ee$Classifier$amnhMaxent()$train(
          features = pts_ee,
          classProperty = 'inside',
          inputProperties = bands
        )

        imageClassified = comb$select(bands)$classify(mEntclass)


        #############
        train_param <-
          list(
            esID = rand_es_sel$esID,
            userID = userID,
            siteID = site_id,
            imp_acc= as.integer(0),
            imp_nat= as.integer(0),
            imp_lulc = as.integer(0),
            imp_own = as.integer(input$imp_own),
            imp_other = as.integer(input$imp_other),
            training_area = as.integer(sum(st_area(polygon))),
            n_poly = as.integer(n_polys),
            blog = input$blog,
            poss_mapping = "Yes",
            expert_trust = "no_own_mapping",
            mapping_order = as.numeric(order),
            extrap_RMSE = 0,
            extrap_accIMP = 0,
            extrap_lulcIMP = 0,
            extrap_natIMP = 0,
            mapTIME_start = mapTIME_start,
            mapTIME_end = mapTIME_end

          )
        train_param<-as.data.frame(train_param)

        ############ maxent
        incProgress(amount = 0.1,message = "update data base")
        # write to bq
        insert_upload_job(table_con$project, table_con$dataset, "es_mappingR1", train_param)
        # write.csv(train_param,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/mapping_R1.csv")

        prediction<-imageClassified$select("probability")

        ############ save map
        incProgress(amount = 0.2,message = "store your map")
        img_assetid <- "projects/eu-wendy/assets/es_mapping/es_map_ind/"
        img_id<-paste0(img_assetid, site_id,"_",rand_es_sel$esID,"_",userID,"_1")
        #
        # #set features of img
        prediction <- prediction$set('esID', rand_es_sel$esID,
                                     'userID', userID,
                                     'siteID', site_id,
                                     'delphi_round', 1,
                                      'mapping_order', order)
        #
        coords <- st_coordinates(sf_stud_geom)
        coords<-as.data.frame(coords[,c(1,2)])
        geometry <- ee$Geometry$Rectangle(
          coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
          proj = "EPSG:4326",
          geodesic = FALSE
        )

        task_img <- ee_image_to_asset(
          image = prediction,
          assetId = img_id,
          overwrite = T,
          region = geometry
        )

        task_img$start()


        ############ prepare map
        incProgress(amount = 0.1,message = "prepare interactive map")
        Map$setCenter(mean(coords$X), mean(coords$Y),10)
        prediction<-Map$addLayer(
          eeObject = prediction,
          maxentviz,
          "Probability of ES",
          opacity = 0.4)
      })
      prediction<-prediction

    })

    observe({
      req(prediction)
      output$gee_map <- renderLeaflet({
        prediction()
      })
      # outputOptions(output, "gee_map", suspendWhenHidden = FALSE)
      output$btn_cond<-renderUI({
        req(prediction)
        actionButton(ns("confirm2"), "Next task", class='btn-primary')
      })


    })#/observe
    #modify reactive value to trigger cond
    observeEvent(input$confirm2,{
      rv1$u <-reactive({1})
    })
    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})

    return(cond)
  })
}

## To be copied in the UI
# mod_delphi_round1_ui("delphi_round1_1")

## To be copied in the server
# mod_delphi_round1_server("delphi_round1_1")
