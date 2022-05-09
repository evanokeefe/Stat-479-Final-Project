library(tidyverse)
library(plotly)
library(shiny)
library("superheat")
library("dplyr")
library("tidyr")
library("ggplot2")
library("gridExtra")
library("networkD3")
library("ggraph")
library("tidygraph")

network <- read_csv('./data/network.csv')
data <- read_csv("./data/479_tidy_data.csv") %>%
  mutate(hash = str_remove(email, "@.*"),
         institution = str_to_lower(institution)) %>% 
  group_by(hash, institution) %>% 
  summarise(num_commit = sum(num_commit),
            name = max(name)) %>% 
  arrange(desc(num_commit))

colleges <- pull(data, institution) %>%
  unique() %>%
  na.omit()

univs <- data %>% 
  group_by(institution) %>% 
  summarise(num_commit = sum(num_commit)) %>% 
  arrange(desc(num_commit))

networkDiagram <- function(tempata) {
  E <- data.frame(
    source = tempata$src_committer,
    target = tempata$dst_committer,
    value = 1
  )
  
  G <- as_tbl_graph(E, directed = FALSE)
  
  G <- G %>%
    mutate(cluster = as.factor(group_louvain()))
  
  ggraph(G, layout = 'fr') + 
    geom_edge_link(colour = "#d3d3d3", width = 0.5, alpha = 0.5) +
    geom_node_label(aes(label = name, fill = cluster)) +
    coord_fixed() +
    labs(title = "Connection Betwwen Committers Within the Institution") + 
    theme_void() +
    theme(plot.title = element_text(size = 20, hjust = 0.5) , legend.position="none") 
}

institutionNetworkDiagram <- function(target_institution){
  institution_network <- network %>%
    group_by(src_institution, dst_institution) %>%
    summarize(value=n()) %>%
    filter(src_institution != dst_institution & value > 20)
  
  E <- data.frame(
    source = institution_network$src_institution,
    target = institution_network$dst_institution,
    value = institution_network$value
  )
  
  G <- as_tbl_graph(E, directed = FALSE)
  
  G <- G %>%
    activate(edges) %>%
    mutate(edge_weight = runif(value))
  
  ggraph(G, layout = 'fr') + 
    geom_edge_link(aes(col=edge_weight, width=edge_weight), alpha = 0.5) +
    scale_edge_width_continuous(range = c(0.2, 1)) +
    geom_node_label(aes(label = name, color=ifelse(name == target_institution, "#ff0000", "#000000"))) +
    coord_fixed() +
    labs(title = "Connection Betwwen Academic Institutions") + 
    theme_void() +
    scale_edge_colour_viridis() + 
    scale_color_identity() +
    guides(edge_width = FALSE) + 
    theme(plot.title = element_text(size = 20, hjust = 0.5))
}

top_univs <- function(df, n){
  df_slice <- head(df, n)
  div_factor <- 10^floor(log10(min(df_slice$num_commit)))
  div_txt <- format(div_factor, big.mark=",", scientific = FALSE)
  p <- ggplot(df_slice, aes(x = reorder(institution, -num_commit), y = num_commit/div_factor)) +
    geom_col(aes(text = paste(
      "Commits:", format(num_commit, big.mark=",")
    ))) +
    labs(
      title = "Academic Institutions With The Most GitHub Commits",
      x = "",
      y = paste("Number of Commits per", div_txt)
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))
  ggplotly(p, tooltip = c("text"))
}

top_person <- function(df, n){
  df_slice <- head(df, n)
  div_factor <- 10^floor(log10(min(df_slice$num_commit)))
  div_txt <- format(div_factor, big.mark=",", scientific = FALSE)
  p <- ggplot(df_slice, aes(x = reorder(name, -num_commit), y = num_commit/div_factor, fill=institution)) +
    geom_col(aes(text = paste(
      "Commits:", format(num_commit, big.mark=","), "\n",
      "Institution:", institution
    ))) +
    labs(
      title = "Users With The Most GitHub Commits",
      x = "",
      y = paste("Number of Commits per", div_txt)
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))
  ggplotly(p, tooltip = c("text"))
}

ui <- fluidPage(
  titlePanel("Exploring GitHub Commits By Academic Institution"),
  p("Section 1: Exploring Ranking", style = "font-size:25px; font-weight: bold;"),
  numericInput("num_disp",
               "Number of Bars",
               value = 10),
  radioButtons("choice",
               "Show Users or Academic Institutions",
               choices = c("Users", "Institutions")
  ),
  p("Click a bar to view the institutions top contributors. Double click anywhere to reset.", style = "font-size:15px"),
  plotlyOutput("bar"),
  p("Section 2: Exploring Connections", style = "font-size:25px; font-weight: bold;"),
  selectInput("colleges", "College", colleges),
  plotOutput("network", width = "100%", height = "1300px"),
  plotOutput("institution_network", width = "100%", height = "800px")
  
)

server <- function(input, output) {
  bar_plot <- reactive({
    click <- event_data("plotly_click")
    if(!is.null(click)){
      if(input$choice == "Users"){
        clicked <- head(data, input$num_disp)$institution[pull(click, x)]
        data %>% 
          filter(institution == clicked) %>% 
          top_person(input$num_disp)
      }else{
        clicked <- head(univs, input$num_disp)$institution[pull(click, x)]
        data %>% 
          filter(institution == clicked) %>% 
          top_person(input$num_disp)
      }
    }else if(input$choice == "Users"){
      top_person(data, input$num_disp)
    }else{
      top_univs(univs, input$num_disp)
    }
    
  })
  
  college_subset <- reactive({
    filter(network, src_institution==input$colleges & dst_institution==input$colleges) 
  })
  
  output$network <- renderPlot({
    networkDiagram(college_subset())
  })
  
  output$institution_network <- renderPlot({
    institutionNetworkDiagram(input$colleges)
  })
  
  output$bar <- renderPlotly(bar_plot())
}

shinyApp(ui, server)
