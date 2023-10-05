library(shiny)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(DT)
library(magrittr)
library(dplyr)

#setwd("C:/Users/gabri/Documents/invacost_extrapolation/invacost_extrapolation_app")

world_data <- readRDS("world_data.rds")
cost_table <- read.csv("Costs_per_country.csv")

names(cost_table)[6] <- "Cost"

cost_table$Cost <- cost_table$Cost/(10^9)

cost_table$group <- NA

cost_table$group[cost_table$Phylum %in% c("Arthropoda")] <- "Arthropods"

cost_table$group[cost_table$Phylum %in% c("Ascomycota",
                                          "Basidiomycota",
                                          "Chytridiomycota",
                                          "Oomycota")] <- "Fungi"

cost_table$group[cost_table$Phylum %in% c("Chlorophyta",
                                          "Ochrophyta",
                                          "Chytridiomycota",
                                          "Tracheophyta")] <- "Plants"

cost_table$group[cost_table$Phylum %in% c("Chordata")] <- "Vertebrates"

cost_table$group[cost_table$Phylum %in% c("Mollusca",
                                          "Nematoda",
                                          "Platyhelminthes")] <- "Other invertebrates"

cost_table$group[cost_table$Phylum %in% c("Pisuviricota",
                                          "Proteobacteria")] <- "Microorganisms"

choices_group <- as.list(cost_table$group %>% unique)
names(choices_group) <- choices_group

choices_sector <- as.list(cost_table$Sector %>% unique)
names(choices_sector) <- choices_sector

worldMaps <- function(df, world_data){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () {
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "bottom",
                       panel.border = element_blank(),
                       strip.background = element_rect(fill = 'white', colour = 'white'),
                       plot.margin = margin(t = 0,  # Top margin
                                            r = 0,  # Right margin
                                            b = 0,  # Bottom margin
                                            l = 0))
  }
  
  # Select only the data that the user has selected to view
  
  plotdf <- df
  
  plotdf$Cost <- plotdf$Cost %>% round(3)
  
  # Add the data the user wants to see to the geographical world data
  world_data['Cost'] <- plotdf$Cost[match(world_data$iso2, plotdf$country)]
  
  # Specify the plot for the world map
  
  g1 <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90),
                             color = 'gray70', linewidth = 0.1,
                             aes(x = long, 
                                 y = lat, 
                                 fill = Cost, 
                                 group = group, 
                                 tooltip = sprintf("%s<br/>%s", iso2, Cost),
                                 data_id = iso2)) + 
    scale_fill_gradient(low = "blue", high = "red") + 
    labs(fill = "Billions of USD per Year", color = "Billions of USD per Year", title = NULL, x = NULL, y = NULL) + 
    coord_fixed() +
    my_theme()
  
  return(g1)
}

# Define the UI

ui = fluidPage(
  tags$head(HTML("<title>Economic Cost of invasive species per year</title>")),
  
  # App title
  titlePanel(title = div("Economic Cost of invasive species per year",
                         img(src='invacost_logo.jpg', align = "right", width = "10%"))),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectizeInput(inputId = "Sector",
                     label = "Choose sector:",
                     choices = choices_sector,
                     options = list(
                       placeholder = 'Please select sector',
                       onInitialize = I('function() { this.setValue(""); }')), 
                     multiple = TRUE),
      
      selectizeInput(inputId = "group",
                     label = "Choose group:",
                     choices = choices_group,
                     options = list(
                       placeholder = 'Please select group',
                       onInitialize = I('function() { this.setValue(""); }')), 
                     multiple = TRUE),
      
      actionButton("run", label = "Generate map"),
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Hide errors
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      uiOutput("distPlots")
      
    )
    
  )
)


# Define the server
server = function(input, output) {
  
  cost_df <- reactive({
    
    selected_phyla <-
      cost_table$group %in% input$group
    
    selected_sector <-
      cost_table$Sector %in% input$Sector
    
    cost_table[
      selected_phyla & 
        selected_sector,]
    
  })
  
  distPlot <- 
    eventReactive(input$run, {
      
      cost_country <-
        cost_df() %>% 
        dplyr::group_by(country) %>% 
        dplyr::summarise(Cost = sum(Cost, na.rm = TRUE))
      
      # Create the interactive world map
      girafe(code = print(worldMaps(
        cost_country,
        world_data)),
        options = list(selected = NULL,
                       opts_sizing(rescale = FALSE),
                       opts_selection(type = "single",
                                      css = "fill:green;stroke:gray;")),
        width_svg = 13.5, 
        height_svg = 6)
      
    })
  
  selected_countries <-  
    reactive({
      input$distPlot_selected
    })
  
  change <- reactive({
    paste(input$run, input$distPlot_selected)
  })
  
  bar_plot <- 
    eventReactive(change(), {
      
      all_Costs <- cost_df()
      
      if(length(selected_countries()) > 0){
        all_Costs <-
          all_Costs[all_Costs$country %in% selected_countries(),] %>% 
          dplyr::group_by(Sector, group) %>% 
          dplyr::summarise(Cost = sum(Cost, na.rm = TRUE))
      } else {
        all_Costs <-
          all_Costs %>% 
          dplyr::group_by(Sector, group) %>% 
          dplyr::summarise(Cost = sum(Cost, na.rm = TRUE)) 
      }
      
      plot_title <- 
        ifelse(length(selected_countries()) > 0,
               selected_countries(),
               "Costs per sector and taxonomic group")
      
      ggplot(all_Costs,aes(x=Cost,y=Sector, fill=group, color = group))+
        geom_col() +
        theme_bw() + 
        ggtitle(plot_title) +
        theme_classic() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),  
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.text = element_text(size=15)) +
        xlab("Billions of USD per Year") + ylab("Impacted Sector")
      
    })
  
  output$distPlot <- renderGirafe(distPlot())
  output$bar_plot <- renderPlot(bar_plot(), height = 400, width = 800)
  
  output$distPlots <- renderUI({
    fluidPage(girafeOutput("distPlot"),
              plotOutput("bar_plot"),
              align = "center")
    
  })
  
}

shinyApp(ui, server)