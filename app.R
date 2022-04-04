library(shiny)
library(bs4Dash)
library(fresh)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(shinyWidgets)

# Configuration of sidebar - - - - - - - - - - - - - - - - - - - - - - 
sidebar <- dashboardSidebar(
  width = "300px",
  skin = "light",
  status = "primary",
  fixed = TRUE,
  id = "sidebar",
  minified = FALSE,
  sidebarMenu(
    id = "tabs",
    flat = FALSE,
    compact = FALSE,
    childIndent = TRUE,
    h5(
      "Elige un departamento",
      style = "padding-left:10px;padding-right:10px;font-weight:bold;"
    ),
    
    pickerInput(
      inputId = "select_dep",
      label = NULL,
      choices = dep_list,
      selected = "Todos los departamentos"),
    
    conditionalPanel(
      condition = "input.select_dep != 'TODOS'",
      pickerInput(
        inputId = "select_prov",
        label = "Elige una provincia",
        choices = "Todas las provincias",
        selected = "Todas las provincias"),
      
      conditionalPanel(
        condition = "input.select_prov != 'TODOS'",
        pickerInput(
          inputId = "select_distr",
          label = "Elige un distrito",
          choices = "Todos los distritos",
          selected = "Todos los distritos")
      )
    )
  )
)


# Configuration of body - - - - - - - - - - - - - - - - - - - - - - - -
body <- dashboardBody(
  title = "DashboardPage",
  fluidPage(
    tabsetPanel(
      tabPanel(
        "Mapa",
        icon = icon("globe"),
        tags$style(type = "text/css", "#map {height: calc(100vh - 170px) !important;}"),
        leafletOutput("map"),
        absolutePanel(
          id = "legend", class = "panel panel-default",
          top = "auto", bottom = 50, right = "auto", width = 20, fixed = FALSE,
          draggable = FALSE, height = "auto",
          img(src = "https://user-images.githubusercontent.com/23284899/161477212-ec505906-f5d4-4e4e-b8a7-c9237e32851a.png", width = 300)
        )
      ),
      tabPanel("Data","contents",icon = icon("table")),
      tabPanel("Ayuda","contents",icon = icon("question-circle"))
    )
  )
)


ui <- dashboardPage(
  skin = "ligth",
  options = NULL,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Mellon Grant",
      color = "primary",
      href = "#",
      image = "https://avatars.githubusercontent.com/u/46831228?s=200&v=4",
      opacity = 1
    )
  ),
  sidebar = sidebar,
  body = body,
  controlbar = dashboardControlbar(),
)


server = function(input, output, session) {
  
  # Update picker select UI - - - - - - - - - - - - - - - - - - - - - - - -
  observe({
    prov_list_pre <- areas_list %>%
      filter(dep %in% input$select_dep) %>%
      pull(prov) %>%
      unique() %>%
      sort()
    
    prov_list <- c("TODOS",prov_list_pre)
    
    names(prov_list) <- c("Todas las provincias",
                         str_to_title(prov_list_pre))
    
    updatePickerInput(session = session,
                      inputId = "select_prov",
                      choices = prov_list)
  })

  observe({

    dist_list_pre <- areas_list %>%
      filter(dep %in% input$select_dep,
             prov %in% input$select_prov) %>%
      pull(distr) %>%
      unique() %>%
      sort()

    dist_list <- c("TODOS",dist_list_pre)
    names(dist_list) <- c("Todos los distritos",str_to_title(dist_list_pre))

    updatePickerInput(session = session,
                      inputId = "select_distr",
                      choices = dist_list)
  })

  
  # Leaflet - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lat = -9.19, lng = -75.015, zoom = 4.5) 
  })
  
  # Condiciones en el mapa - - - - - - - - - - - - - - - - - - - - - - - - -
  map <- leafletProxy("map") %>%
    clearShapes() %>% 
    addPolygons(
      data = data,
      smoothFactor = 0.1,
      weight = 0,
      fillColor = data$rgb,
      fillOpacity = 0.8
    )
  
}

shinyApp(ui = ui,server = server)




