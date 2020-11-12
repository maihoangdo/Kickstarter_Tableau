library(shiny)
library(data.table)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(rworldmap)
library(knitr)
library(googleVis)

# read the file 
ks <- read.csv(file = "/Users/maihoang/Maimai/ks_projects.csv", header = T)
# remove 7 observatins that have incorrect launch dates (year says "1970")
ks = ks[c(-2843, -48148, -75398, -94580, -247914, -273780, -319003),]
# covert deadline values to date type
ks$deadline <- as.Date(ks$deadline, "%Y-%m-%d")
#covert launched values to date type
ks$launched <- as.Date(ks$launched, '%Y-%m-%d %H:%M:%S')
# add a new column for project duration
ks$project_duration_days <- ks$deadline - ks$launched
# For heat map
countries.freq <- ks %>%
  filter(country!='N,0"') %>%
  group_by(country) %>%
  summarize(count=n())

countries.freq = countries.freq[c(-17),] 
# For main_category and subcategory isolation
#cate <- ks %>% dplyr::select(., main_category, category) %>% group_by(., main_category, category) %>% summarise(., Count=n())
#cate <- unique(cate)
#for bar graphs: Main Category
main_count  = ks %>% group_by(., main_category ) %>% summarise(., `Total Count`= n())
descend = main_count %>% arrange(., desc(main_count$`Total Count`)) #arrange categories in descending order
success_n = ks %>% dplyr::select(., main_category, state) %>% filter(., state=="successful") %>% group_by(., main_category) %>% summarise(., `Total Successful` = n())
failure_n = ks %>% dplyr::select(., main_category, state) %>% filter(., state!="successful") %>% group_by(., main_category) %>% summarise(., `Total Unsuccessful` = n())
des_success_n = merge(success_n,failure_n)
des_success_n = merge(des_success_n, descend)
des_success_n2 = des_success_n %>% arrange(., desc(des_success_n$`Total Count`))
des_success_n3 = des_success_n2 %>% mutate(., `Success Rate` = (`Total Successful`/`Total Count`)) %>% arrange(., desc(`Success Rate`))


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Kickstarter Projects",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("The Dataset", tabName = "table", icon = icon("table")),
      menuItem("HeatMaps", tabName = "map_dropdown", icon = icon("bars"),
               menuItem("Map", tabName = "heatmap", icon = icon("blue"))),
      menuItem("Graphs", tabName = 'graph_dropdown', icon = icon('bars'),
               menuItem("Most Popular Main Categories", tabName = 'bargraph', icon=icon('signal')),
               menuItem("Success Rate By Main Category", tabName = 'bargraph3', icon =icon('signal')))
               
    )),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = 'table',
              fluidRow(column(3, selectizeInput('categories', label='Pick a Category:', choices= NULL, multiple = T)),
                       column(3, selectizeInput('subcategories', label='Pick a Subcategory:', choices= NULL, multiple = T)),
                       dataTableOutput('table'))
      ),
      
      tabItem(tabName = 'heatmap',
              fluidPage(htmlOutput("Heatmap"))
      ),
      tabItem(tabName = 'bargraph',
              fluidPage(headerPanel(htmlOutput("bar_graph")))
              
      ),
      tabItem(tabName = 'bargraph3',
              fluidPage(htmlOutput("bar_graph3"))
              )
    )
  )
)
server <-function(input, output, session){
  
  
  updateSelectizeInput(session, "categories", choices = unique(ks$main_category), server = TRUE)
  updateSelectizeInput(session, "subcategories", choices = unique(ks$category), server = TRUE)
  
  ################################## FILTERING OF THE DATATABLE ##################################
  filtered_data = ks
  data_filter = reactive({
    if(length(input$categories)) {
      filtered_data = filtered_data %>% filter(., main_category == input$categories)
    }
    if(length(input$subcategories)) {
      filtered_data = filtered_data %>% filter(., category %in% input$subcategories)
    }
    return(filtered_data)
  })
    
  
  ################################### OUTPUTS #####################################################
  output$table <- renderDataTable({
    datatable(data_filter(), rownames=TRUE, options = list(columnDefs = list(list(visible = FALSE, targets = c(1,6,9,13))))) %>%
      formatStyle(input$selected,
                  background="skyblue", fontWeight='bold')
    
  })
  
  output$Heatmap <- renderGvis({
    map<-gvisGeoChart(countries.freq, locationvar='country', colorvar='count',
                      options=list(title='Number of Projects by Country', projection="kavrayskiy-vii", width='100%', colorAxis="{colors:['#FFcccc', '#FF0000']}"))
    
    return(map)
  })
  

  output$bar_graph <- renderGvis({
    Col1 <- gvisColumnChart(des_success_n2, xvar = "main_category", yvar = c("Total Unsuccessful","Total Successful"),  options = list(isStacked=T, title='Most Popular Project Categories', width=1000, height= 600))
    
    return(Col1)
  })
  

  output$bar_graph3 <- renderGvis({
    Col3 <- gvisColumnChart(des_success_n3, xvar = "main_category", yvar = "Success Rate",  options = list(title='Success Rate By Main Category', width=1000, height= 600))
    
    return(Col3)
  })
}

shinyApp(ui, server)

