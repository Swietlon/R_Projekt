rm(list = ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)

#Ładowanie zbioru danych ####
parks <- read_csv('data/parks.csv')
species <- read_csv('data/species.csv')


#Obrobienie danych ####
#Funkcja mapująca nieokreślony Status ochrony na status 'Safe' - bezpieczny
safeMark <- function(x) {
  if (is.na(x)) return('Safe')
  else return(as.character(x))
}
#Rzutowanie kodów Stanów na pełne nazwy stanów
makeStateName <- function(abb) {
  states <- unlist(strsplit(abb, split = ', '))
  statesNames <- state.name[grepl(paste(states, collapse = '|'), state.abb)]
  return(paste(statesNames, collapse = ', '))
}
#Akry na km2
acresToKm <- function(acres) {
  return(acres * 4046.8564224 / 1000000)
}
#Nowa kolumna na podstawie Conservation Status, NA zastąpione 'Safe'
species$Status <- sapply(species$`Conservation Status`, safeMark)
#Pełna nazwa Stanów na których terenie znajduje się park
parks$statesNames <- sapply(parks$State, makeStateName)
#Km2 parku
parks$km2 <- sapply(parks$Acres, acresToKm)
#Złączenie dataframeów
total <- merge(species, parks, by = 'Park Name')
#usunięcie zbędnej kolumny
myData <- total[, -14]

# GUI ####
ui <- fluidPage(
  #CSS
  tags$head(
    tags$style(
      HTML('
           body { background-color: #ffffff; width: 100% }
           .container { background-color: #aaaaaa; padding: 60px; }
           .infodiv { background-color: #eeeeee; }
           .afterplot { margin-top: 20px; }
           ')
    )
  ),
  #Panele
  titlePanel(
    h1('Parki narodowe w USA', align='center'),
    windowTitle = 'Bioróżnorodność'
  ),
  mainPanel(
    class = 'container',
    tabsetPanel(
      type = 'pills',
      tabPanel( 
        'Informacje',
        div(
          h2('Wstęp'),
          p('Aplikacja do wizualizacji danych dotyczących bioróżnorodności
                 w parkach narodowych Stanów Zjednoczonych Ameryki.
                 Aplikacja zrealizowana w ramach projektu na zaliczenie
                 przedmiotu Przetwarzanie i Wizualizacja Danych.', br(),
            'Źródło danych: ', a('kaggle', 
                                 href='https://www.kaggle.com/datasets/nationalparkservice/park-biodiversity/data')),
          h2('Dane'),
          plotOutput(outputId = 'dataQualityBarPlot'),
          p('Powyższy wykres przedstawia procentowy brak danych konkretnych
                kolumn w zbiorów. Kolumny, które mają 100% wypełnienie zostały
                pominięte. `Conservation Status` odnosi się
                do statusu ochrony. Na potrzeby projektu przyjęto, że jeżeli
                status nie występuje, to gatunek jest bezpieczny (Safe).',class = 'afterplot'),
          p('Ilość poszczególnych gromad'),
          plotlyOutput(outputId = 'categoryBarPlot'),
          align = 'center',
          class = 'infodiv'
        )
      ),
      tabPanel(
        'Wizualizacja danych', 
         plotOutput(outputId = 'statesSpecies'),
         plotlyOutput(outputId = 'parksSpecies'),
         selectInput('park',
                     'Wybierz Park Narodowy',
                     choices = parks['Park Name']),
         selectInput('feature',
                     'Wybierz cechę',
                     choices = c('Category', 'Occurrence', 'Nativeness', 'Abundance', 'Seasonality', 'Status')),
         textOutput(outputId = 'parkInfo'),
         plotlyOutput(outputId = 'featureBarPlot'),
         plotlyOutput(outputId = 'categoryParkPlot'),
         plotlyOutput(outputId = 'occurrenceParkPlot')
        
      ),
      tabPanel(
        'Mapy',
        leafletOutput(outputId = 'parkMap'),
        leafletOutput(outputId = 'speciesPerParkMap'),
        leafletOutput(outputId = 'speciesPerAreaMap')
      )
    )
  )
)

#Serwer ####
server <- function(input, output) {
  #Informacje ####
  
  na_data <- apply(X = is.na(myData), MARGIN = 2, FUN = mean) * 100
  na_data <- data.frame(Columns = names(na_data), NullsPercentage = na_data)
  na_data <- na_data %>% filter(NullsPercentage > 0) %>% arrange(NullsPercentage)
  
  output$dataQualityBarPlot <- renderPlot(
    ggplot(na_data, aes(x = factor(Columns, level = c(Columns)), y = NullsPercentage)) +
      geom_bar(stat = 'identity', fill = 'skyblue') +
      labs(x = 'Kolumny', y = 'Procent niepełnych danych', title = 'Jakość danych') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  )
  
  output$categoryBarPlot <- renderPlotly(
    myData %>%
      group_by(Category) %>%
      summarise(count = n()) %>%
      ggplot(data = ., aes(x = Category, y = count, fill = Category)) +
      geom_bar(stat = 'identity') +
      labs(title = 'Liczba gatunków w poszczególnych gromadach', x = 'Gromady', y = 'Liczba')
  )
  
  #Wizualizacja danych ####
  StatesCount <- myData %>%
    group_by(State) %>% summarise(count = n())
  
  output$statesSpecies <- renderPlot(
    ggplot(data = StatesCount, aes(x = State, y = count)) +
      geom_bar(stat = 'identity', fill = 'skyblue') +
      labs(title = 'Wykres', x = 'Stany', y = 'Gatunki')
  )
  
  output$parksSpecies <- renderPlotly({
    ggplotly(myData %>%
               group_by(`Park Name`, `Park Code`, Category) %>%
               summarise(speciesCount = n()) %>%
               ggplot(aes(x = `Park Code`, y = speciesCount, fill = Category)) +
               geom_bar(stat = 'identity') +
               labs(x = 'Kod parku', 
                    y = 'Liczba gatunków', 
                    title = 'Liczba gatunków przypadająca na park z podziałami na rodzaj',
                    fill = 'Mode Type')
               )
  })
  
  output$parkInfo <- renderText({
    currentPark <- filter(parks, `Park Name` == input$park)
    paste0('Kod Parku: ', currentPark$`Park Name`, 
          ', Stany: ', currentPark$statesNames, 
          ', powierzchnia[km2]: ', round(currentPark$km2, digits = 2))
  })
  
  output$featureBarPlot <- renderPlotly({
    currentParkData <- filter(myData, `Park Name` == input$park)
    ggplotly(currentParkData %>% 
               group_by_at(input$feature) %>%
               summarise(count = n()) %>%
               ggplot(aes(x = eval(as.name(input$feature)), y = count
                          , fill = eval(as.name(input$feature)))) +
               geom_bar(stat = 'identity') +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
               labs(title = 'Wykres', x = input$feature , y = 'Liczba gatunków',
                    color = 'MyTitle'),
             tooltip = c(as.character(input$feature), 'count')
             )
    
  })
  
  
  output$categoryParkPlot <- renderPlotly({
    currentParkData <- filter(myData, `Park Name` == input$park)
    ggplotly(currentParkData %>% 
               group_by(Category) %>%
               summarise(count = n()) %>%
               ggplot(aes(x = Category, y = count, fill = Category)) +
               geom_bar(stat = 'identity') +
               coord_flip() +
               labs(title = 'Wykres gromady', x = 'Gromada', y = 'Liczba gatunków'),
             tooltip = c('Category', 'count'))
  })
  
  
  output$occurrenceParkPlot <- renderPlotly({
    currentParkData <- filter(myData, `Park Name` == input$park)
    ggplotly(currentParkData %>% 
               group_by(Occurrence) %>%
               summarise(count = n()) %>%
               ggplot(aes(x = Occurrence, y = count, fill = Occurrence)) +
               geom_bar(stat = 'identity') +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
               labs(title = 'Wykres występowania', x = 'Występowanie', y = 'Liczba gatunków'),
             tooltip = c('Occurrence', 'count'))
  })
  
  #Mapy ####
  
  speciesPerPark <- 
    left_join(
      parks,
      myData %>%
        group_by(`Park Code`) %>%
        summarise(count = n()),
      by = 'Park Code'
    ) %>% mutate(speciesPerArea = count/km2)
  countMax = max(speciesPerPark['count']) 
  
  output$parkMap <- renderLeaflet({
    leaflet(data = parks) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste('Park: <b>', `Park Name`, '</b><br>Stan: <b>', statesNames, '</b>')
      )
  })
  
  output$speciesPerParkMap <- renderLeaflet({
    myPal <- colorNumeric(palette = c('blue', 'red'), domain = speciesPerPark$count)
    
    speciesPerPark %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(weight = 1, 
                       radius = speciesPerPark$count/countMax * 30 , 
                       color =~myPal(speciesPerPark$count), 
                       popup = ~paste('Park: <b>', `Park Name`, '</b><br>Stan: <b>', 
                                      statesNames, '</b><br>Gatunki: <b>', count, '</b>'),
                       lng = ~Longitude,
                       lat = ~Latitude) %>%
      addLegend('topright',
                pal = myPal,
                values = ~speciesPerPark$count,
                title = 'Liczba gatunków')
  })
  
  output$speciesPerAreaMap <- renderLeaflet({
    myPal <- colorNumeric(palette = c('blue', 'red'), domain = speciesPerPark$speciesPerArea)
    
    speciesPerPark %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(weight = 1, 
                       radius = sqrt(speciesPerPark$speciesPerArea) * 5 , 
                       color =~myPal(speciesPerPark$speciesPerArea), 
                       popup = ~paste('Park: <b>', `Park Name`, '</b><br>Stan: <b>', 
                                      statesNames, '</b><br>Gatunki: <b>', count, '</b><br>',
                                      'Obszar[km2]: <b>', km2, '</b>'),
                       lng = ~Longitude,
                       lat = ~Latitude) %>%
      addLegend('topright',
                pal = myPal,
                values = ~speciesPerPark$speciesPerArea,
                title = 'Liczba gatunków na km2')
    
  })
}


# Uruchomienie apki ####
shinyApp(ui = ui, server = server)
