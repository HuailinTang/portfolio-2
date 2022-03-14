library(dplyr)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyr)
library(ggrepel)

fertility_rate <- read.csv("birthrate.csv")
region <- read.csv("region.csv")

table <- merge(x=fertility_rate,y=region,
               by.x="STATE", by.y="State.Code",all.x=TRUE) %>% 
  select(-c(STATE,BIRTHS,URL,Division)) %>% 
  rename(Year = YEAR, Fertility_rate = FERTILITY.RATE)

table$Year = table$Year - 2000 #turn year 2015 to 15

table = table %>% 
  filter(Year %in% 14:20) %>%  #only use fertility_rate from 14 to 20
  arrange(desc(Fertility_rate)) #rank fertility_rate

state = table %>% 
  pull(State) %>% 
  unique() #pull state with fertility rate from high to low

region <- table %>% 
  pull(Region) %>% 
  unique()

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # select region
      pickerInput("Region_input", "Region", choices = region, selected = region, 
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      # select state
      pickerInput("State_input", "State", choices = state, selected = state, 
                  options = list(`actions-box` = TRUE), multiple = TRUE), 
      width = 2
    ),
    mainPanel(
      plotOutput("graph_1", width = "1150px", height = "870px"),
      dataTableOutput("table_1")
    )
  )
)

server <- function(input, output, session) {
  table_1 <- reactive({
    table %>% 
      filter(Region %in% input$Region_input) %>% 
      filter(State %in% input$State_input)
  })
  
  observeEvent(input$Region_input,{
    # update state based on region input
    updatePickerInput(session, "State_input", choices = unique(table_1()$State),
                      selected = unique(table_1()$State))
  })
  
  output$graph_1 <- renderPlot({
    ggplot(table_1(), aes(x=Year, y=Fertility_rate, # color line by region
                          group=interaction(State, Region), color=Region)) + 
    geom_line() +       
    theme(legend.position = "bottom", axis.title = element_text(size = 16), 
          axis.text = element_text(size =12), panel.grid.major.y = element_blank(),
          legend.text=element_text(size=12)) +
    title("Fertility rate from 2014-2020") +
    ylab("Fertility rate") +
    scale_color_brewer(palette = "Set2") +
    geom_text_repel(aes(label = State), size = 5,
                    data = filter(table_1(), Year %in% c(14,20)), 
                    show.legend = FALSE)
  }, height = 870, width = 1150)
  
  output$table_1 <- renderDataTable({
    table_1()
  })
}

app <- shinyApp(ui, server)