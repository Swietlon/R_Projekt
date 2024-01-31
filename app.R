rm(list = ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(DT)


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
           body { background-color: #F0FFFF; width: 100% }
           .container { background-color: #E0EEEE; padding: 60px; }
           .infodiv { background-color: #E0EEEE; }
           .afterplot { margin-top: 20px; }
           #parkInfo { margin-bottom: 20px; font-weight: 700; }
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
                kolumn w zbiorze. Kolumny, które mają 100% wypełnienie zostały
                pominięte. `Conservation Status` odnosi się
                do statusu ochrony. Na potrzeby projektu przyjęto, że jeżeli
                status nie występuje, to gatunek jest bezpieczny (Safe).',class = 'afterplot'),
          p('Ilość poszczególnych kategorii'),
          plotlyOutput(outputId = 'categoryBarPlot'),
          h3('Przegląd gatunków'),
          selectInput('category',
                      'Kategoria',
                      choices = add_row(unique(species['Category']),
                                        Category = 'All',
                                        .before = 1)),
          selectInput('occurrence',
                      'Występowanie',
                      choices = add_row(unique(species['Occurrence']),
                                        Occurrence = 'All',
                                        .before = 1)),
          DT::dataTableOutput('spieciesTable'),
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
         sliderInput('cut',
                     'Wybierz obcięcie',
                     min = 0,
                     max = 500,
                     value = 0
         ),
         textOutput(outputId = 'parkInfo'),
         plotlyOutput(outputId = 'featureBarPlot'),
         selectInput('categorySpeciesCount',
                     'Wybierz kategorię',
                     choices = unique(species['Category'])),
         DT::dataTableOutput('speciesTableCount')
        
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
    ggplot(na_data, aes(x = factor(Columns, level = c(Columns)), y = NullsPercentage, fill = Columns)) +
      geom_bar(stat = 'identity', width = 0.7) +
      geom_text(aes(label = sprintf("%.2f%%", NullsPercentage)), vjust = -0.5, size = 4) +
      labs(x = 'Kolumny', y = 'Procent niepełnych danych', title = 'Jakość danych') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
            plot.title = element_text(size = 14, face = "bold", color = "black"),
            axis.title.x = element_text(size = 12, face = "bold", color = "black"),
            axis.title.y = element_text(size = 12, face = "bold", color = "black"))

      
  )
  
  output$categoryBarPlot <- renderPlotly(
    myData %>%
      group_by(Category) %>%
      summarise(count = n()) %>%
      ggplot(data = ., aes(x = Category, y = count, fill = Category)) +
      geom_bar(stat = 'identity') +
      labs(title = 'Liczba gatunków w poszczególnych kategoriach', x = 'Kategorie', y = 'Liczba') +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold",color = "black"),
        plot.margin = margin(15, 15, 15, 15)  
      )
  )
  
  output$speciesTable <- DT::renderDataTable({
    species
  })
  
  #Wizualizacja danych ####
  StatesCount <- myData %>%
    group_by(State) %>% summarise(count = n())
  
  output$statesSpecies <- renderPlot(
    ggplot(data = StatesCount,aes(x = State, y = count)) +
      geom_bar(stat = 'identity', width = 0.7, fill="#BCEE68") +
      labs(title = 'Wykres', x = 'Stany', y = 'Gatunki')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(color = 'gray', linetype = 'dotted'),
            plot.title = element_text(size = 14, face = "bold", color = "black"),
            axis.title.x = element_text(size = 12, face = "bold", color = "black"),
            axis.title.y = element_text(size = 12, face = "bold", color = "black"))
    
      
      
  )
  
  output$parksSpecies <- renderPlotly({
    ggplotly(
      myData %>%
        group_by(`Park Name`, `Park Code`, Category) %>%
        summarise(speciesCount = n()) %>%
        ggplot(aes(x = `Park Code`, y = speciesCount, fill = Category)) +
        geom_bar(stat = 'identity') +
        labs(
          x = 'Kod parku',
          y = 'Liczba gatunków',
          title = 'Liczba gatunków przypadająca na park'
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1,size = 4),
          plot.title = element_text(size = 14, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"))
    )
  })
  
  output$parkInfo <- renderText({
    currentPark <- filter(parks, `Park Name` == input$park)
    paste('Kod Parku: ', currentPark$`Park Name`, 
          ', Stany: ', currentPark$statesNames, 
          ', powierzchnia[km2]: ', round(currentPark$km2, digits = 2)
          )
  })
  
  output$featureBarPlot <- renderPlotly({
    currentParkData <- filter(myData, `Park Name` == input$park)
    currentFeature <- input$feature

    p <- currentParkData %>%
      group_by_at(input$feature) %>%
      summarise(count = n()) %>%
      filter(count > input$cut & !is.na(eval(as.name(input$feature)))) %>%
      ggplot(aes(x = eval(as.name(input$feature)), y = count, fill = eval(as.name(currentFeature)))) +
      geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.6)) +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 1),
            axis.title = element_text(size = 11, color = "black", face = "bold"),
            axis.text = element_text(size = 10, color = "black", face = "bold"),
            plot.title = element_text(size = 15, hjust = 0.5, color = "black",face = "bold"),
            legend.position = "top") +
      labs(title = 'Wykres', x = currentFeature, y = 'Liczba gatunków', color = currentFeature) +
      theme(legend.text = element_blank())
    
    ggplotly(p, tooltip = c("Park Name", "count"))
  })
  
  output$speciesTableCount <- DT::renderDataTable({
    myData %>%
      filter(Category == input$categorySpeciesCount) %>%
      group_by(`Scientific Name`, `Common Names`) %>%
      summarise(count = n()) %>%
      arrange(-count) %>%
      DT::datatable()
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


