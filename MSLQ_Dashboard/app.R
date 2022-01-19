library(janitor)
library(Amelia)
library(Hmisc)
library(Rmisc)
library(clValid)
library(clustertend)
library(factoextra)
library(caret)
library(skimr)
library(tidyverse)
library(infer)
library(rstatix)
library(readxl)
library(lubridate)
library(RcppRoll)
library(parallel)
library(clustertend)
library(factoextra)
library(NbClust)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(ggpmisc)
library(ggstatsplot)
library(DT)
all

# getwd()
# setwd("C:/Users/Administrator.BENNNBX56000004/Documents/Matt DeLoia Files/CEMA Project Year2 Data/CEMA Year 2 Data Processing")

#read data
df <- read_rds("MSLQ_scored.rds") 
df2 <- read_rds("MSLQ_scored2.rds") %>%
    #mutate(part_id = as.character(part_id)) %>% 
    left_join(read_rds("proficiency.rds")) 


df_cluster <-  df2 %>%
    filter(method =="unscaled") %>% 
    select(part_id, measure, score) %>% 
    pivot_wider(names_from = "measure", values_from = "score") %>%
    mutate_if(is.numeric, impute) %>% 
    column_to_rownames("part_id") %>%
    as.matrix()

motivation <- c("Intrinsic_Goal_Orientation", "Extrinsic_Goal_Orientation", "Task_Value", 'Control_Beliefs_about_Learning', 'Selfefficacy_for_Learning_and_Performance', "Test_Anxiety")

learning <- c('Rehearsal', 'Elaboration', 'Organization', 'Critical_Thinking', 'Metacognitive_Selfregulation', 'Time_Study_Environment', 'Effort_Regulation', 'Peer_Learning', 'Help_Seeking')

all <- c(motivation, learning)

grouping_variables <- c("cluster", "college_degree",  "experience", "mos_transfer")

#############################

ui <- fluidPage(
    
    # Application title
    titlePanel("Cyber Trainee MSLQ Dashboard"),
  
                  tabsetPanel(
                    
                    tabPanel(title="Scale Plot",
                             dropdownButton(
                               tags$h3("List of Input"),
                               radioButtons("feature_x","Results Grouping variable:", c(grouping_variables), selected = "cluster"),
                               radioButtons("method", "Scoring approach:", c("unscaled", "scaled"), selected="unscaled"),
                             
                               sliderInput(inputId = 'clusters', label = 'Number of clusters', value = 2, min = 2, max = 8),
                               plotOutput("clusterplot2", height = "200px"),
                               circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to see inputs !")
                             ),
                             
                               plotOutput("plot1", height = "600px")),
                    
                    tabPanel(title="Results Table",
                             box(
                               dataTableOutput("table"),
                               dataTableOutput("table2"))),
                    
                    tabPanel(title="Comparison Plot",
                             dropdownButton(
                               tags$h3("List of Input"),
                               radioButtons("feature_x2","Results Grouping variable:", c(grouping_variables), selected = "cluster"),
                               selectInput("feature_y","Results Independent variable:", c(all), selected = "Proficiency"),
                               radioButtons("method2", "Scoring approach:", c("unscaled", "scaled"), selected="unscaled"),
                               circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to see inputs !")
                               ),
                             
                             plotOutput("results_plot", height = "600px"))
                    )
           
    )

server <- function(input, output) {
    
    observe(showNotification("Created by Peraton for Army Analysis", duration = 15))
    
    set.seed(111)
    kmeans_clust <- reactive({
      kmeans(df_cluster,input$clusters)
      })
    
    df_cluster2 <- reactive ({
      kmeans_clust()$cluster %>%
        as.data.frame() %>% 
        rownames_to_column("part_id") %>% 
        rename("cluster"=2) %>% 
        mutate(cluster = as.factor(cluster)) %>% 
        right_join(df2) %>%
        as.data.frame()
      })
    
    #main plot
    output$plot1 <- renderPlot({
      df_cluster2() %>% 
        gather(cluster, mos_transfer, experience, college_degree, key=feature, value = value) %>%
        filter(feature==input$feature_x,
               method==input$method) %>% 
        group_by(category, measure, value) %>% 
        summarise(group_mean = mean(score), sd = sd(score), n=n()) %>%
        ungroup() %>% 
        mutate(ci =1.65*sd/n^.5 ) %>% 
        left_join(
          df_cluster2() %>% 
            gather(cluster, mos_transfer, experience, college_degree, key=feature, value = value) %>%
            filter(feature==input$feature_x) %>% 
            group_by(measure, value) %>% 
            summarise(mean = mean(score)) %>% 
            group_by(measure) %>% 
            summarise(sd = sd(mean)) %>% 
            mutate(delta_rank = rank(sd)) %>% 
            select(-sd)
        ) %>%
          filter(measure %in%  c(motivation, learning)) %>%
        ggplot(aes(x=reorder(measure, delta_rank, FUN = mean), y=group_mean, color=value ))+
        geom_point(size = 1) +
        geom_errorbar(aes(ymin =group_mean-ci , ymax=group_mean+ci), width = .2) +
        facet_grid(category~., scales = "free") +
        coord_flip()+
        theme(legend.position = "right")+
        ylab("group average") +
        xlab("")+
        labs(title = paste("Grouping variable:", input$feature_x), caption = "Note: error bars represent 90% CI around mean score")
      
    })
    
    #Results tables  
    table <- reactive({
        df_cluster2() %>% 
            gather(cluster, mos_transfer, experience, college_degree, key=feature, value = value) %>%
            filter(feature==input$feature_x) %>% 
            select(part_id, feature, value) %>% 
            unique() %>% 
            left_join(
                df_cluster2() %>% 
                    select(part_id, cluster, mos_transfer, college_degree, advanced_degree, experience, college_years, experience_years, proficiency) %>% 
                  unique()
            ) %>% 
            group_by(feature, value) %>%
            summarise(n = n(),
                      college_years = round(mean(college_years, na.rm=TRUE),1), 
                      experience_years = round(mean(experience_years, na.rm=TRUE),1),
                      mos_transfer = sum(mos_transfer),
                      college_degree = sum(college_degree),
                      advanced_degree = sum(advanced_degree),
                      experience = sum(experience), 
                      proficiency = mean(proficiency))%>% 
            mutate(proficiency = round(proficiency, 1)) %>% 
            mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), ~round(.x/n*100,1)) %>%
            mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), ~paste(.x,"%", sep="")) %>% 
            select(feature, value, n, mos_transfer, college_degree, advanced_degree, experience, proficiency)
    })
    
    output$table <- renderDataTable( {
        table() %>%  datatable(class = "display compact", filter = 'none') 
        })
    
    #comparions of means
    
    p_values <- reactive ({
        df_cluster2() %>%
        gather(cluster, mos_transfer, experience, college_degree, key=feature, value = value) %>%
        filter(feature==input$feature_x) %>%  
        group_by(measure) %>%
        t_test(score ~ value)
    })
    

    table2 <- reactive({
        cbind(p_values()$measure,p_values()$p) %>% 
            as.data.frame() %>% 
            mutate(V2 = as.numeric(V2)) %>% 
            group_by(V1) %>%
            summarise(V2 = min(V2)) %>%
            rename(p_value = V2) %>%
            column_to_rownames("V1") %>%
            mutate(p_value = round(as.numeric(p_value),3)) %>%
            ungroup() %>%
            mutate(sig = if_else(p_value <= .05,"**",
                                 if_else(p_value <= .10, "*", ""))) %>%
            arrange(p_value)
    }) 
    
    output$table2 <- renderDataTable( {
      table2() %>%  datatable(class = "display compact", filter = 'none') 
    })
    
    #Proportions test and Info Box
    test_stat  <- reactive (prop.test(table()$Pass, table()$Total)$p.value)
    
    output$proportions_test_value <- renderText(test_stat())
    
 
    
    #lower left plot
    output$clusterplot2 <- renderPlot({
        fviz_nbclust(df_cluster, kmeans, method="silhouette")
    })
    

    #  results
    output$results_plot <- renderPlot({
        ggbetweenstats(
            df_cluster2() %>%
                gather(cluster, mos_transfer, experience, college_degree, key=feature, value = category) %>%
                filter(feature==input$feature_x2, 
                       measure == input$feature_y, 
                       method ==input$method2),
            x=category, 
            y=score,
            xlab = paste("Group variable: ", input$feature_x2),
            ylab = paste ("Measure: ", input$feature_y),
            #ggtheme = ggthemes::theme_fivethirtyeight(),
            pairwise.display = "all",
            conf.level = .90)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
