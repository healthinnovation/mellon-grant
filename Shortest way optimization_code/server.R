#' VectorPoint 
#' Copyright (c) 2015-2017  VectorPoint team
#' See LICENSE.TXT for details
#' 
#' Server
#' 
#' 
#' 
#Set the file upload limit to 30 MB (Not Applicable) Dropbox only?

options(shiny.maxRequestSize = 30 * 1024 ^ 2)
set.seed(100)
shinyServer(function(input, output, session) {
  
  #Variables globales
  usernameCheck <- 'Failure'
  networkCheck <- 'Failure'
  reconnected <- FALSE
  var_sim_searches <- NULL
  #The authenatication system:
  sessionData     <- reactiveValues()
  
  #localidades total
  var_locality <- NULL
  
  observeEvent(input$userLogin,{
    #Almacena TRUE si es un usuario que existe y coincide la contraseña
    loginSuccess <- Login(input$username, input$password)
    reconnected <<- (input$reconnected == "true")
    
    if("usuario_correcto"==loginSuccess[[1]]) {
      
      usernameCheck <- 'Success'
      
      #Variable para dar permiso al administrador
      if ("ADMIN"== loginSuccess[[2]]) {
        admin_user <- 'Success'   
        
        data_mysql <- Report();
        
        output$casa_regular <- renderText({paste("Casa regular:",data_mysql[["casa_regular"]])})
        output$des <- renderText({paste("Deshabitadas:",data_mysql[["des"]])})
        output$lp <- renderText({paste("LP:",data_mysql[["lp"]])})
        output$lv <- renderText({paste("LV:",data_mysql[["lv"]])})
        output$c <- renderText({paste("Cerradas:",data_mysql[["c"]])})
        output$inspeccion <- renderText({paste("Inspeccionadas:",data_mysql[["inspeccion"]])})
        output$r <- renderText({paste("Renuentes:",data_mysql[["r"]])})
        output$v <- renderText({paste("V1:",data_mysql[["v"]])})
        output$num_elements <- renderText({paste("No DE REGISTROS:",data_mysql[["num_elements"]])})
        
        output$valores <- renderDataTable({data_mysql[["data"]]},
                                          options = list(
                                            pageLength = 10
                                          )
        )
        
        output$adminUser <- renderText({admin_user})
        outputOptions(output, 'adminUser', suspendWhenHidden = FALSE)  #keeps on top
        
      } else {
        admin_user <- "Failure"
        
        ###############
        #Leyendo el catchment area
        sessionData$searchdata <- as.data.table(read.csv(paste("catchment_area/",loginSuccess[[3]],".csv", sep = ""), stringsAsFactors = FALSE))
        sessionData$searchdata <- sessionData$searchdata[, LATITUDE:=as.double(LATITUDE)] 
        sessionData$searchdata <- sessionData$searchdata[, LONGITUDE:=as.double(LONGITUDE)] 
        
        #Obteniendo solo la columna de unicode
        catchment_area <- as.data.frame(sessionData$searchdata$UNICODE, stringsAsFactors=FALSE)
        
        sessionData$localities <- unique(sessionData$searchdata$codeLoc)
        sessionData$houseId    <- '' #set when the user selects a house
        sessionData$palForRisk <- function(probab){return('#dummy')}
        #if(!reconnected)
        #  updateSelectizeInput(session, "locality", choices = sessionData$localities, server = TRUE)
        
        ###############
        var_sim_searches    <<- loginSuccess[[4]]
      }
      
    } else if ("clave_incorrecta"==loginSuccess[[1]]) {
      #Mensaje cuando falla la clave ingresada
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "La CLAVE no es la correcta. Por favor inténtelo nuevamente")
      })
      
    } else {
      #Mensaje cuando falla el usuario ingresado
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "El USUARIO no existe. Por favor inténtelo nuevamente")
      })
    }
    
    output$validUser <- renderText({usernameCheck})
    outputOptions(output, 'validUser', suspendWhenHidden = FALSE)  #keeps on top
    
  })
  
  #Boton online
  observeEvent(input$networkOnline,{
    networkCheck <- 'Success'
    output$networkCheck <- renderText({networkCheck})
    outputOptions(output, 'networkCheck', suspendWhenHidden = FALSE)  #keeps on top
  })
  
  #Boton offline
  observeEvent(input$networkOffline,{
    networkCheck <- 'Success'
    output$networkCheck <- renderText({networkCheck})
    outputOptions(output, 'networkCheck', suspendWhenHidden = FALSE)  #keeps on top
  })
  
  output$inspectButton <- renderUI({})
  
  #' Submit of inspection report
  observeEvent(input$inputSubmit, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    #Llenado campos obligatorios
    if (input$P == '' ||
        input$D == '' ||
        input$L == '' ||
        input$V == '' ) {
      observe({
        session$sendCustomMessage(type = 'confirm-message',
                                  message = "El unicode de la vivienda es obligatorio.")
        #output$out <- renderPrint(input$midata)
      })
      #Campo Obs_text de unicode equivocado  
    } else if ( input$observaciones == TRUE && input$obs_unicode == 5 && input$obs_text1=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese el codigo correcto en la caja de texto")
      })
      #Campo Obs_text de unicode equivocado
    } else if ( input$observaciones == TRUE && input$obs_unicode == 8 && input$obs_text2=="") {
      
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese la observación sobre el codigo en la caja de texto")
      })
      #Campo tipo de local publico es obligatorio cuando se selecciona LP
    } else if (input$caract_predio=="LP" && input$tipo_lp=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el tipo de local publico")
      })
      #Campo motivo es obligatorio cuando se selecciona V
    } else if (input$status_inspec == 'V' && input$motivo_volver=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el motivo por el cual se tiene que volver")
      }) 
      #Campo de texto es obligatorio cuando se selecciona Renuente R6
    } else if (input$status_inspec == 'R' && input$renuente=="R6" && input$renuente_otro=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Explique por favor la causa de renuencia")
      }) 
    } else if (input$status_inspec == 'inspeccion' && input$lugar_inspeccion_intra==0 && input$lugar_inspeccion_peri==0) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el lugar donde se realizo la inspeccion")
      })
    } else if ( (input$chiris_intra==1 || input$rastros_intra==1) && input$lugar_inspeccion_intra == 0 ) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "No puede marcar en chiris o rastros sin haber seleccionado el lugar de inspección adecuado ")
      })
    } else if ( (input$chiris_peri==1 || input$rastros_peri==1) && input$lugar_inspeccion_peri == 0 ) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "No puede marcar en chiris o rastros sin haber seleccionado el lugar de inspección adecuado ")
      })
    } else {
      #wishlist: we want to run something like https://github.com/AugustT/shiny_geolocation
      inputData <- recordInspectionFields()
      
      inputData$USER_NAME      = input$username
      inputData$GROUP_NAME     = 'SIN_GRUPO'
      inputData$DATA_ACTION    = 'INSPECTION_NEW'  
      #future: lat and lon during this operation
      
      inputData$HORA_FIN <- as.character(Sys.time())
      
      save_search_data_mysql(inputData, TABLE_NAME = dbGlobalConfig$inspectionsTable)
      
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Los datos fueron GUARDADOS con éxito. Gracias.")
      })
      
      #Cerrando modal
      toggleModal(session, "inputForm", "close")
      
      cleanData()
      
      houseinLoc <- getLocalityData()
      leafletProxy("map", data = houseinLoc) %>% 
        addCircleMarkers(
          radius = 8
          ,color = "yellow"
          ,stroke = FALSE
          ,fillOpacity = .3#ifelse(groupParameters$CERTAINTY_CLOUD == 1, .3, .0)
        )%>%
        addCircleMarkers(
          fillColor = "red",
          radius = 4,
          stroke = TRUE,
          color = "black",
          weight = .4,
          fillOpacity = 1,
          layerId = ~ UNICODE,
          popup = paste(
            "<b>", houseinLoc[, UNICODE], "</b><br>",
            #Cambiando color antes era "blue" ahora es "black"
            "Ult. visita:","<b style='color: black;'>", houseinLoc[, inspectionText],"</b>"
          )
        )
    }
    
    #toggleModal(session, "inputForm")
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  #' Limpiando valores
  observeEvent(input$inputClear, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    cleanData()
    
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  observeEvent(input$enterData,{
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    
    #Almacena el tiempo en que se empieza a ingresar la información
      date_start <<- Sys.time()
    
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    #shows the menu ...
    
  })
  
  ## Interactive Map ##
  # Create the map
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(maxZoom=19,preferCanvas = TRUE)) %>%
      addTiles(urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = '<a href="http://openstreetmap.org">OpenStreetMap</a> contributors'
      ) %>%
      #addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE) ) %>%
      setView(
        lng = mean(-71.50001667),
        lat = mean(-16.36249883),
        zoom = 16
      )
    
  })

  # Load the interactive map on click
  observeEvent(input$load, {
    
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    
    var_locality <<- input$locality
    if (is.null(var_locality)) {
      var_locality <<- sessionData$localities
    }
    
    houseinLoc <- sessionData$searchdata
    
    houseinLoc1 <- houseinLoc[houseinLoc$COLOR==0,]
    houseinLoc2 <- houseinLoc[houseinLoc$COLOR==1,]
    
    leafletProxy("map", data = houseinLoc1) %>% 
      
      addCircleMarkers(
        radius = 5,
        stroke = TRUE,
        color = "black",
        weight = .4,
        fillOpacity = 1,
        layerId = ~ UNICODE,
        popup = paste(
          "<b>", houseinLoc[, UNICODE], "</b>"
        )
      )
    
    leafletProxy("map", data = houseinLoc2) %>% 
      setView(
        lng = mean(houseinLoc2[, LONGITUDE]),
        lat = mean(houseinLoc2[, LATITUDE]),
        
        zoom = 14
      ) %>% 
      addCircleMarkers(
        radius = 5,
        stroke = TRUE,
        color = "yellow",
        weight = .4,
        fillOpacity = 1,
        layerId = ~ UNICODE,
        popup = paste(
          "<b>", houseinLoc[, UNICODE], "</b>"
        )
      )
    
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    return(leafletMap)
  })
  
  observeEvent(input$map_marker_click, {
    #http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
    #MAPID_marker_click
    click<-input$map_marker_click
    if(is.null(click)) {
      return()
    }
    sessionData$houseId <- click$id
    output$houseId <- renderText(sessionData$houseId)
    output$inspectionUserMessage <- renderText("")
    if(!is.null(sessionData$houseId)) {
      session$sendCustomMessage(type = 'click-message',
                                message = toJSON(click, auto_unbox = TRUE, digits = 10))
    }
    
  })
  
  #' when a user selects a house
  observeEvent(sessionData$houseId, {
    if(sessionData$houseId == '') {
      return()
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    
    PDLV <- base::strsplit(sessionData$houseId, '\\.')[[1]]
    updateTextInput(session, "P", value = PDLV[1])
    updateTextInput(session, "D", value = PDLV[2])
    updateTextInput(session, "L", value = PDLV[3])
    updateTextInput(session, "V", value = PDLV[4])
    
    PREDICTED_PROBAB <- sessionData$searchdata$probability[sessionData$searchdata$UNICODE==sessionData$houseId]
    
    if(var_sim_searches== 0) {
      output$inspectButton <- renderUI({})
      output$houseProbab <- renderText( sprintf('probab: %.2f', PREDICTED_PROBAB)  )
      #wishlist: show in popup
    } else {
      output$inspectButton <- renderUI(actionButton("sim_inspect_house_button", label="", icon = icon("search", "fa-.5x")))
      output$houseProbab <- renderText({paste("")})
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  observeEvent(input$gps, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    if (!is.null(input$lat)) {
      output$userMessage <- renderText({ paste("") })
      leafletProxy("map") %>% addCircles(
        color = "blue",
        radius = 5,      #in meters
        lng = input$long,
        lat = input$lat
        #icon = icon("circle", "fa-.5x")
      ) %>% setView(lng = input$long,
                    lat = input$lat,
                    zoom = 19)
    } else {
      output$houseId <- renderText({paste("")})
      output$userMessage <- renderText({ paste("Location data unavailable") })
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  observeEvent(input$sim_inspect_house_button, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    if(sessionData$houseId == ''){
      session$sendCustomMessage(type = 'action-message',
                                message = "buscando_false")
      return();
    }
    inputData <- list(
      'USER_NAME'      = input$username,
      'GROUP_NAME'     = 'SIN_GRUPO',
      'DATA_ACTION'    = 'INSPECTION_CLICK',
      "UNI_CODE"       = sessionData$houseId,
      "FECHA"          = as.character(input$fecha),
      "INSPECTION_FLAG"= "inspected",
      "TEST_DATA"      = ifelse(input$testData, 1, 0)
      #wishlist: gps lat/lon
    )
    
    #record the model-based probability of infestation
    PREDICTED_PROBAB <- sessionData$searchdata[UNICODE==inputData$UNI_CODE, probability]
    PREDICTED_PROBAB <- PREDICTED_PROBAB[1] #in case there are multiple matches - wishlist: raise an error
    inputData$PREDICTED_PROBAB      <- PREDICTED_PROBAB
    inputData$PREDICTED_PROBAB_MEAN <- mean(sessionData$searchdata[, probability], na.rm = T) #reference probability for this dataset
    
    save_search_data_mysql(inputData, TABLE_NAME = dbGlobalConfig$simulationsTable)
    
    riskLevel <- which(sessionData$riskColors == sessionData$palForRisk(PREDICTED_PROBAB))
    output$houseProbab <- renderText(
      #(sprintf(  '<div>Riesgo: <font color="%s">%s</font></div>', sessionData$palForRisk(PREDICTED_PROBAB), sessionData$riskNames[riskLevel]))
      (sprintf(  '<div>Riesgo: <font           >%s</font></div>', sessionData$riskNames[riskLevel]))
      #sprintf('probab: %.2f', PREDICTED_PROBAB)
      #sprintf('%s probab: %.2f', inputData$UNI_CODE, PREDICTED_PROBAB)
    )
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    #http://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
  })
  
  #Accion al presionar el boton REPORTE
  observeEvent(input$reportUser,{
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    #VIVIENDAS INGRESADAS EN EL DIA
    viv_ingresadas <- HouseRegisterDay(Sys.Date(),input$username);
    #viv_ingresadas <- HouseRegisterDay("2017-08-24",input$username);
    
    if (nrow(viv_ingresadas)==0) {
      viv_ingresadas <- "Aun no se ingresaron viviendas el dia de hoy"
    }
    
    #Enviando los datos de volver a UI.R
    output$viv_ingresadas <- renderTable({ viv_ingresadas })
    
    #VIVIENDAS QUE SE TIENE QUE VOLVER
    viv_volver <- HouseGoBack(Sys.Date()-1,input$username);
    #viv_volver <- HouseGoBack("2017-08-23",input$username);
    
    if (nrow(viv_volver)==0) {
      viv_volver <- "No hay viviendas pendientes para volver el dia de hoy"
    } else {
      aux <- viv_ingresadas
      viv_volver$VIV_VOLVER <- 1
      aux$VIV_INGRESADAS <- 1
      viv_volver <- merge(viv_volver, aux, all.x=TRUE)
      viv_volver$VOLVER <- "pendiente"
      viv_volver$VOLVER[(1==viv_volver$VIV_VOLVER & 1==viv_volver$VIV_INGRESADAS)] <- "VISITADA"
      viv_volver <- viv_volver[,c("UNI_CODE", "USER_NAME", "VOLVER")]
    }
    
    #Enviando los datos de volver a UI.R
    output$viv_volver <- renderTable({ viv_volver })
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  getLocalityData <- function(){
    
    houseinLoc <- sessionData$searchdata[codeLoc %in% (var_locality)]
    
    #Tiempo (anos) : Colores de blanco hasta gris
    sessionData$palForTime <- colorFactor(c("white","gray90","gray80","gray75","gray70","gray65","gray60","gray55","gray40", "gray30", "gray20"), 
                                          domain = c(0,1,2,3,4,5,6,7,8,9,10))  #years.  wishlist: make this data-dependent with houseinLoc[, time]
    sessionData$palForRisk <- unique_colorQuantile(palette = "YlOrRd", domain=houseinLoc[, probability], n = 5)
    
    sessionData$riskNames  <- c("Mas Bajo", "Bajo", "Medio", "Alto", "Mas Alto")
    sessionData$riskColors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026") # "#808080" #1..5, NA
    lastInspections <- read_past_inspections(databaseName=dbGlobalConfig$authDatabaseName, UNICODE=houseinLoc$UNICODE)
    lastInspections[,positive:=(TOT_INTRA>0)|(TOT_PERI>0)|(!RASTROS=='0')]
    #wishlist: protect the html from bad data in the fecha field
    
    #Completar el status a palabra entera
    lastInspections[lastInspections$STATUS_INSPECCION == "inspeccion",4] <- "inspección"
    lastInspections[lastInspections$STATUS_INSPECCION == "C",4] <- "cerrada"
    lastInspections[lastInspections$STATUS_INSPECCION == "R",4] <- "renuente"
    lastInspections[lastInspections$STATUS_INSPECCION == "V",4] <- "volver"
    
    lastInspections[, inspectionText:=paste0(FECHA, ': ', STATUS_INSPECCION)]
    #Se quito el color rojo y se puso color negro
    lastInspections[positive==T, inspectionText:=paste0('<font  color="black">', inspectionText, '</font>')]
    
    lastInspections[positive==F, inspectionText:=paste0('<font            >', inspectionText, '</font>')]
    houseinLoc <- merge(houseinLoc, lastInspections[,.(UNICODE,inspectionText)], all.x=T, by='UNICODE')
    houseinLoc[is.na(houseinLoc$inspectionText), inspectionText:='--']
    return(houseinLoc)
  }
  
  #' graba los valores de input durante o despues de inspeccion
  recordInspectionFields <- function() {
    inputData <-
      list(
        #Almacenando campos
        "UNI_CODE" = paste(input$P,input$D,toupper(input$L),toupper(input$V),sep = "."),
        "CODE_LOCALITY" = paste(input$P,input$D,toupper(input$L),sep = "."),
        "OBS_UNICODE" = ifelse(input$observaciones == TRUE,input$obs_unicode, NA),
        "OBS_TEXT" = NA,
        "FECHA" = as.character(input$fecha),
        "CARACT_PREDIO" = input$caract_predio,
        "TIPO_LP" = NA,
        "STATUS_INSPECCION" = NA,
        "ENTREVISTA" = NA,
        "MOTIVO_VOLVER" = NA,
        "RENUENTE" = NA,
        "INTRA_INSPECCION" = NA,
        "INTRA_CHIRIS" = NA,
        "INTRA_RASTROS" = NA,
        "PERI_INSPECCION" = NA,
        "PERI_CHIRIS" = NA,
        "PERI_RASTROS" = NA,
        #eliminar luego de la base de datos, por que ya no se esta usando
        "LUGAR_INSPECCION" = NA,
        #-------------------------
        #eliminar luego
        "TOT_INTRA" = NA,
        "TOT_PERI" = NA,
        "RASTROS" = NA,
        #--------------
        "PERSONAS_PREDIO" = NA,
        
        "CANT_PERROS" = NA,
        "CANT_GATOS" = NA,
        "CANT_AVES_CORRAL" = NA,
        "CANT_CUYES" = NA,
        "CANT_CONEJOS" = NA,
        "TEXT_OTROS" = NA,
        "CANT_OTROS" = NA,
        "TEST_DATA"      = ifelse(input$testData, 1, 0),
        "HORA_INICIO" = as.character(date_start)
      )
    
    #Almacenar obs_text
    if (input$observaciones == TRUE) { 
      if (input$obs_unicode==5) {
        inputData$OBS_TEXT = input$obs_text1
      }else if (input$obs_unicode==8) {
        inputData$OBS_TEXT = input$obs_text2
      }
    }
    
    if(input$caract_predio == 'casa_regular' || input$caract_predio == 'LP' || input$caract_predio == 'DES'){
      inputData$STATUS_INSPECCION <- input$status_inspec
      
      #Si es local publico se pone que tipo es
      if (input$caract_predio == 'LP') {
        inputData$TIPO_LP <- (input$tipo_lp)
      }
      
      #Campo ENTREVISTA
      if(input$status_inspec =='entrevista'){
        inputData$ENTREVISTA <- input$entrevista
      }
      
      #Campo MOTIVOS_VOLVER
      if (input$status_inspec == 'V') {
        inputData$MOTIVO_VOLVER <- input$motivo_volver
      }
      
      #Campo RENUENTE
      if (input$status_inspec == "R") { 
        if (input$renuente=="R6") {
          inputData$RENUENTE = input$renuente_otro
        }else {
          inputData$RENUENTE = input$renuente
        }
      }
      
      if(input$status_inspec == 'inspeccion') {
        #Lugar de inspeccion
        inputData$INTRA_INSPECCION <- as.integer(input$lugar_inspeccion_intra)
        inputData$PERI_INSPECCION <- as.integer(input$lugar_inspeccion_peri)
        
        #Chiris en intra o peri
        inputData$INTRA_CHIRIS <- as.integer(input$chiris_intra)
        inputData$PERI_CHIRIS <- as.integer(input$chiris_peri)
        
        #Rastros en intra o peri
        inputData$INTRA_RASTROS <- as.integer(input$rastros_intra)
        inputData$PERI_RASTROS <- as.integer(input$rastros_peri)
        
        #Personas predio
        inputData$PERSONAS_PREDIO <- (input$personas_predio)
        
        inputData$CANT_PERROS <- 0
        inputData$CANT_GATOS <- 0
        inputData$CANT_AVES_CORRAL <- 0
        inputData$CANT_CUYES <- 0
        inputData$CANT_CONEJOS <- 0
        inputData$TEXT_OTROS <- 0
        inputData$CANT_OTROS <- 0
        
        if (input$perros) {
          inputData$CANT_PERROS <- (input$cant_perros)
        }
        if (input$gatos) {
          inputData$CANT_GATOS <- (input$cant_gatos)
        }
        if (input$aves_corral) {
          inputData$CANT_AVES_CORRAL <- (input$cant_aves_corral)
        }
        if (input$cuyes) {
          inputData$CANT_CUYES <- (input$cant_cuyes)
        }
        if (input$conejos) {
          inputData$CANT_CONEJOS <- (input$cant_conejos)
        }
        if (input$otros) {
          inputData$TEXT_OTROS <- (input$text_otros)
          inputData$CANT_OTROS <- (input$cant_otros)
        }
      }
    }
    return(inputData)
  }
  
  #Funcion para limpiar datos
  cleanData <- function(){
    #Almacena el tiempo en que se empieza a ingresar la información
    date_start <<- Sys.time()
    
    updateNumericInput(session, "P", value = 1)
    updateNumericInput(session, "D", value = "")
    updateTextInput(session, "L", value = "")
    updateTextInput(session, "V", value = "")
    updateCheckboxInput(session,"observaciones", "Observaciones", FALSE)
    updateSelectInput(session, "obs_unicode", selected = "1")
    updateTextAreaInput(session, "obs_text1", value = "")
    updateTextAreaInput(session, "obs_text2", value = "")
    updateDateInput(session, "fecha", value = Sys.Date())
    updateSelectInput(session, "caract_predio", selected = "casa_regular")
    updateTextInput(session, "tipo_lp", value = "")
    updateSelectInput(session, "status_inspec", selected = "C")
    updateRadioButtons(session, "entrevista", selected = "cree_no_tiene")
    updateTextAreaInput(session, "motivo_volver", value = "")
    updateTextAreaInput(session, "renuente_otro", value = "")
    updateCheckboxInput(session,"lugar_inspeccion_intra", NULL, FALSE)
    updateCheckboxInput(session,"lugar_inspeccion_peri", NULL, FALSE)
    updateCheckboxInput(session,"chiris_intra", NULL, FALSE)
    updateCheckboxInput(session,"chiris_peri", NULL, FALSE)
    updateCheckboxInput(session,"rastros_intra", NULL, FALSE)
    updateCheckboxInput(session,"rastros_peri", NULL, FALSE)
    updateNumericInput(session, "personas_predio", value = 1)
    updateCheckboxInput(session,"perros", "Perros", FALSE)
    updateCheckboxInput(session,"gatos", "Gatos", FALSE)
    updateCheckboxInput(session,"aves_corral", "Aves de corral", FALSE)
    updateCheckboxInput(session,"cuyes", "Cuyes", FALSE)
    updateCheckboxInput(session,"conejos", "Conejos", FALSE)
    updateCheckboxInput(session,"otros", "Otros", FALSE)
    
    #Esto es para que el ingreso de datos sea mas rapido.
    updateNumericInput(session, "cant_perros", value = 1)
    updateNumericInput(session, "cant_gatos", value = 1)
    updateNumericInput(session, "cant_aves_corral", value = 1)
    updateNumericInput(session, "cant_cuyes", value = 1)
    updateNumericInput(session, "cant_conejos", value = 1)
    updateTextInput(session, "text_otros", value = "")
    updateNumericInput(session, "cant_otros", value = 1)
    #Limpiando prueba
    #updateCheckboxInput(session, "testData", "Prueba", FALSE)
  }
  
  #REPORTE para admin
  Report <- function(date=NULL){
    result <- list()
    result[["data"]] <- "No se encontraron datos"
    result[["casa_regular"]] <- 0
    result[["des"]] <- 0
    result[["lp"]] <- 0
    result[["lv"]] <- 0
    result[["c"]] <- 0
    result[["inspeccion"]] <- 0
    result[["r"]] <- 0
    result[["v"]] <- 0
    
    data <- load_data_mysql(databaseName = dbGlobalConfig$databaseName,
                            tableName    = dbGlobalConfig$inspectionsTable, date)
    
    num_elements <- nrow(data)
    if (num_elements != 0) {
      casa_regular <- nrow(data[data$CARACT_PREDIO == "casa_regular",])
      des <-   nrow(data[data$CARACT_PREDIO == "DES",])
      lp <- nrow(data[data$CARACT_PREDIO == "LP",])
      lv <- nrow(data[data$CARACT_PREDIO == "LV",])
      c <- nrow(data[data$STATUS_INSPECCION == "C",])
      inspeccion <- nrow(data[data$STATUS_INSPECCION == "inspeccion",])
      r <- nrow(data[data$STATUS_INSPECCION == "R",])
      v <- nrow(data[grepl("V1",data$MOTIVO_VOLVER, fixed = TRUE),])
      
      result[["data"]] <- data
      result[["casa_regular"]] <- casa_regular
      result[["des"]] <- des
      result[["lp"]] <- lp
      result[["lv"]] <- lv
      result[["c"]] <- c
      result[["inspeccion"]] <- inspeccion
      result[["r"]] <- r
      result[["v"]] <- v
      result[["num_elements"]] <- num_elements
    }
    
    return(result)
  }
  
  #FILTRO para administrador
  observeEvent(input$btn_filter, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    
    if(length(input$filter_start)) {
      data_mysql <- Report(input$filter_start)
    } else {
      data_mysql <- Report()
    }
    
    output$casa_regular <- renderText({paste("Casa regular:",data_mysql[["casa_regular"]])})
    output$des <- renderText({paste("Deshabitadas:",data_mysql[["des"]])})
    output$lp <- renderText({paste("LP:",data_mysql[["lp"]])})
    output$lv <- renderText({paste("LV:",data_mysql[["lv"]])})
    output$c <- renderText({paste("Cerradas:",data_mysql[["c"]])})
    output$inspeccion <- renderText({paste("Inspeccionadas:",data_mysql[["inspeccion"]])})
    output$r <- renderText({paste("Renuentes:",data_mysql[["r"]])})
    output$v <- renderText({paste("V1:",data_mysql[["v"]])})
    output$num_elements <- renderText({paste("No DE REGISTROS:",data_mysql[["num_elements"]])})
    
    output$valores <- renderDataTable({data_mysql[["data"]]})
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  #Desconectar
  observeEvent(input$logout,{
    
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    
    cleanData()
    
    #output$map <- renderLeaflet({
    #  leaflet() %>%  clearMarkerClusters() %>%
    #    clearShapes() %>% clearMarkers %>% clearControls()
    #})
    
    session$sendCustomMessage(type = 'clear-polygons',
                              message = 'true')
    
    output$houseId <- renderText({paste("")})
    output$userMessage <- renderText({paste("")})
    output$validUser <- renderText({""})
    output$networkCheck <- renderText({""})
  })
  #Salir
  observeEvent(input$quit,{
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    
    cleanData()
    
    #output$map <- renderLeaflet({
    #  leaflet() %>%  clearMarkerClusters() %>%
    #    clearShapes() %>% clearMarkers %>% clearControls()
    #})
    
    session$sendCustomMessage(type = 'clear-polygons',
                              message = 'true')
    
    output$houseId <- renderText({paste("")})
    output$userMessage <- renderText({paste("")})
    output$validUser <- renderText({""})
    output$networkCheck <- renderText({""})
  })  
  
  #Recibe json del browser
  observeEvent(input$browser_msg,{
    
    if(nchar(input$browser_msg)>0) {
      
      type <- input$browser_msg$type
      if (type == 'post') {
        
        method <- input$browser_msg$method
        data <- input$browser_msg$data
        if (method == 'sync') {
          #save to database
          for (i in 1:length(data)) {
            for (j in 1:length(data[[i]]$data)){
              if (data[[i]][[1]][[j]]=="NA") 
                data[[i]][[1]][[j]] <- NA
            }
            #save_search_data_mysql(data[[i]]$data, TABLE_NAME = data[[i]]$dbtable)
            response <- list(status = 'success')
            session$sendCustomMessage(type = 'post-response-progress',
                                      message = toJSON(response, auto_unbox = TRUE, digits = 10))
          }
          response <- list(status = 'success')
          session$sendCustomMessage(type = 'post-response',
                                    message = toJSON(response, auto_unbox = TRUE, digits = 10))
        } else if (method == 'mordedura') {
          mord <- as.data.frame(unlist(data$VaccPoint))
          colnames(mord)= "VaccPoint"
          mord <- cbind(mord, PointName = unlist(data$PointName))
          #mord <- cbind(mord, UNICODE = unlist(data$UNICODE))
          mord <- cbind(mord, distancia = unlist(data$distancia))
          write.csv(mord,file = paste("result_shortest_way_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv", sep = ""),row.names = FALSE)
          #write.table( data.frame(data), 'mordeduras_x_postas.csv'  , append= T, sep=',' )
          response <- list(status = 'success')
          session$sendCustomMessage(type = 'post-response',
                                    message = toJSON(response, auto_unbox = TRUE, digits = 10))
        }
      } else if (type == 'get') {
        method <- input$browser_msg$method
        data <- input$browser_msg$data
        if (method == 'sync') {
          #read database and csv
          response <- getOfflineData(data$username,data$password)
          session$sendCustomMessage(type = 'get-response',
                                    message = toJSON(response, auto_unbox = TRUE, digits = 10))
        } else if (method == "mordedura") {
          response <- list()
          response$mordeduras <- read.csv(paste0("./supplypoints/",data))
          response$postas <- read.csv("./demandpoints/Puntos_ASA_no_enumeradas_campo_02092021.csv", sep = ";")
          response$status <- "success"
          session$sendCustomMessage(type = 'get-response',
                                    message = toJSON(response, auto_unbox = TRUE, digits = 10))
        }
      }
    }
  })
  
  getOfflineData <- function(username, password) {
    data <- list()
    loginSuccess <- Login(username, password)
    
    if("usuario_correcto"==loginSuccess[[1]]) {
      data$usernameCheck = 'Success'
      
      #Leyendo el catchment area
      data$sessionData <- list()
      data$sessionData$searchdata <- as.data.table(read.csv(paste("catchment_area/",loginSuccess[[3]],".csv", sep = ""), stringsAsFactors = FALSE, sep = " "))
      data$sessionData$localities <- unique(data$sessionData$searchdata$codeLoc)
      
      #clonando ultimas inspecciones
      var_locality <<- data$sessionData$localities
      houseinLoc <- data$sessionData$searchdata[codeLoc %in% (var_locality)]
      data$lastInspections <- read_past_inspections(databaseName=dbGlobalConfig$authDatabaseName,UNICODE=houseinLoc$UNICODE)
      
      #triangulation
      copy <- data$sessionData$searchdata
      data$sessionData$inspected <- findInspecciones(copy)
      
      data$loginSuccess <- loginSuccess
      
      data$status <- "success"
      
    } else {
      data$status <- "auth_failure"
    }
    
    return(data)
  }
})