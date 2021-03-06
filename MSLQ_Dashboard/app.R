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
library(psych)
library(GPArotation)  


# getwd()
setwd("C:/Users/Administrator.BENNNBX56000004/Documents/Matt DeLoia Files/CEMA Project Year2 Data/CEMA Year 2 Data Processing")

#read data

mslq_questions <- read_csv("mslq_questions.csv")
df_scree <- read_rds("df_scree.rds")
corMat <- cor(df_scree)
df_item <- read_rds("df_item.rds") %>% 
  left_join(read_rds("mslq_scale_alphas.rds")) %>% 
  mutate(scale = paste(scale, " (alpha=", round(alpha,2), ")", sep="")) %>% 
  dplyr::select(-alpha)
item_mean <- mean(df_item$mean)
item_sd <- mean(df_item$sd)

df <- read_rds("MSLQ_scored2.rds") %>%
  group_by(part_id) %>% 
  mutate(missing = sum(is.na(unscaled))) %>% 
  filter(missing<=1) %>% 
  dplyr::select(-missing) %>% 
  ungroup()
 
df_cluster <-  df %>%
  dplyr::select(part_id, measure, scaled) %>% 
    pivot_wider(names_from = "measure", values_from = "scaled") %>%
    mutate_if(is.numeric, impute) %>% 
    column_to_rownames("part_id") %>%
    as.matrix()

Motivation <- c("Intrinsic_Goal_Orientation", "Extrinsic_Goal_Orientation", "Task_Value", 'Control_Beliefs_about_Learning', 'Selfefficacy_for_Learning_and_Performance', "Test_Anxiety")

Learning_Strategy <- c('Rehearsal', 'Elaboration', 'Organization', 'Critical_Thinking', 'Metacognitive_Selfregulation', 'Time_Study_Environment', 'Effort_Regulation', 'Peer_Learning', 'Help_Seeking')

all <- c(Motivation, Learning_Strategy)

all_scales <-  Motivation %>% as.data.frame() %>% 
  mutate(category ="Motivation") %>% 
  bind_rows(Learning_Strategy %>% 
              as.data.frame() %>% 
              mutate(category ="Learning Strategy")) %>% 
  dplyr::rename(scale = 1) %>% 
  left_join(read_rds("mslq_scale_alphas.rds")) %>% 
  mutate(scale = paste(scale, " (alpha=", round(alpha,2), ")", sep="")) %>% 
  arrange(-alpha) %>% 
  dplyr::select(-alpha)
  

grouping_variables <- c("cluster","mos_transfer")
grouping_variables2 <- c("college_degree", "advanced_degree",  "experience")
#############################

ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "Cyber Trainee MSLQ Dashboard"),
    
    dashboardSidebar(
   
        radioButtons("feature_x","Results Grouping variable:", c(grouping_variables), selected = "cluster"),
        radioButtons("score", "Scoring approach:", c("unscaled", "scaled"), selected="unscaled"),
        sliderInput(inputId = 'clusters', label = 'Number of clusters', value = 2, min = 2, max = 8),
        plotOutput("clusterplot2", height = "200px")
        ),
    
    dashboardBody(
  
                  tabsetPanel(
                    
                    tabPanel(title="Scale Plot",
                             
                             plotOutput("plot1", height = "600px")),
                    
                    tabPanel(title="Results Table",
                             fluidRow(
                             box(width=8,
                               tableOutput("table") ),
                             
                             box(width=4,
                                 radioButtons("feature_prop_test","Proportions Test variable:", c(grouping_variables2), selected = "college_degree"),
                                 valueBoxOutput("prop_test", width=12)
                                 )
                             ),
                             
                             box(width = 12, 
                               dataTableOutput("table2")
                               )
                             ),
                    
                    tabPanel(title="Comparison Plot",
                             dropdownButton(
                               tags$h3("List of Input"),
                              selectInput("feature_y","Results Independent variable:", c(all), selected = "Proficiency"),
                               
                               circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                               ),
                             
                             plotOutput("results_plot", height = "600px")),
                    
                    tabPanel(title = "Confirmatory Factor Analysis",
                             h5("Use the Tooltip to select scales and adjust the threshold of poor item loadings colored RED.  Cronbach alphas measure the internal consistency of scale items."),
                             h5("*Drag the cursor around items for more information. The BLUE dashed lines indicated overall mean values."),
                             dropdownButton(
                               tags$h3("List of Input"),
                               radioButtons("scale_category", "Scale Category", choices = c("Motivation", "Learning Strategy"), selected = "Motivation"),
                               sliderInput(inputId = 'load_thresh', label = 'Loading threshold (low):', value = .4, min = 0, max = 1),
                               checkboxGroupInput("item_scales", "Scales to Review:", choices = NULL, selected = NULL),
                               
                               circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             box(width = 12,
                             plotOutput("item_plot", brush="plot_brush", height = "400px")
                             ),
                             box(width=12,
                             tableOutput("data")
                             )
                             ),
                    
                    tabPanel(title = "Exploratory Factor Analysis",
                             fluidRow(
                             box(width=12,
                               sliderInput(inputId = 'factors', label = '# of Factors for efa (scree plot analysis suggests 8 factors):', value = 8, min = 0, max = 20),
                               verbatimTextOutput("eval"),
                               br(),
                               h4("Table listing the TOTAL number of Scale items aligned to each factor."),
                               dataTableOutput("efa_table")
                               )
                             ),
                             
                             fluidRow(
                               box(width = 12,
                                   h4("Table of scale items by principal components or factors."),
                               dataTableOutput("efa_table2")
                               )
                               
                               )
                             )
                  )
                  )
)
    
  
server <- function(input, output, session) {
    
    observe(showNotification("Created by Peraton for Army Analysis", duration = 15))
  
  category_scales <- reactive({filter(all_scales, category ==input$scale_category)})
  
  observeEvent(category_scales(), {
    choices <- category_scales()$scale
    updateCheckboxGroupInput(inputId = "item_scales", choices = choices, selected = choices)
  })
  
  
    
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
        right_join(df) %>%
        as.data.frame()
      })
    
    plot <- reactive({
      df_cluster2() %>% 
        group_by(.data[[input$feature_x]], category, measure) %>% 
        summarise(group_mean = mean(.data[[input$score]]), sd = sd(.data[[input$score]]), n=n()) %>%
        ungroup() %>% 
        mutate(ci =1.65*sd/n^.5 ) %>% 
        left_join(
          df_cluster2() %>% 
            group_by(.data[[input$feature_x]], measure) %>% 
            summarise(mean = mean(.data[[input$score]])) %>% 
            group_by(measure) %>% 
            summarise(sd = sd(mean)) %>% 
            mutate(delta_rank = rank(sd)) %>% 
            select(-sd)
        ) %>%
        filter(measure %in%  c(Motivation, Learning_Strategy))
    })
    
    #main plot
    output$plot1 <- renderPlot({
      plot() %>%
        mutate(feature = as.factor(.data[[input$feature_x]])) %>% 
        ggplot(aes(x=reorder(measure, delta_rank, FUN = mean), y=group_mean, color=feature ))+
        geom_point(size = 3) +
        geom_errorbar(aes(ymin =group_mean-ci , ymax=group_mean+ci), width = .2) +
        facet_grid(category~., scales = "free") +
        coord_flip()+
        theme(legend.position = "right", axis.text = element_text(size=12), strip.text = element_text(size=12))+
        ylab("group average") +
        xlab("")+
        labs(title = paste("Grouping variable:", input$feature_x), caption = "Note: error bars represent 90% CI around mean score") +
        scale_color_manual(name = input$feature_x, values=c("blue", "red", "green", "black", "orange"))
      
    })
    
    #Results tables  
    pre_table <- reactive({
        df_cluster2() %>% 
        select(part_id, cluster, mos_transfer, college_degree, advanced_degree, experience, college_years, experience_years) %>% 
        unique() %>%
        group_by(.data[[input$feature_x]]) %>%
        summarise(n = n(),
                      college_years = round(mean(college_years, na.rm=TRUE),1), 
                      experience_years = round(mean(experience_years, na.rm=TRUE),1),
                      mos_transfer = sum(mos_transfer),
                      college_degree = sum(college_degree),
                      advanced_degree = sum(advanced_degree),
                      experience = sum(experience)) 
      })
    
    table <- reactive ({ 
      pre_table()%>% 
            mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), ~round(.x/n*100,1)) %>%
            mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), ~paste(.x,"%", sep="")) %>% 
            select(.data[[input$feature_x]], n, mos_transfer, college_degree, advanced_degree, experience)
    })
    
    output$table <- renderTable( {
        table() 
        })
    
    #comparions of means
    
    p_values <- reactive ({
        df_cluster2() %>%
        mutate(feature = as.factor(.data[[input$feature_x]])) %>% 
        group_by(measure) %>%
        t_test(scaled ~ feature)
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
        as.data.frame() %>% 
        rownames_to_column("scale") %>%
        left_join(all_scales %>% separate(scale, into=c("scale"), sep = " ")) %>% 
            arrange(p_value)
    }) 
    
    output$table2 <- renderDataTable( {
      table2() %>%  datatable(class = "display compact", filter = 'none') 
    })
    
    #Proportions test and Info Box
    prop_test <- reactive ({
      pre_table() %>% select(n, .data[[input$feature_prop_test]])
                             })
    test_stat  <- reactive (prop.test(prop_test()[[input$feature_prop_test]], prop_test()$n)$p.value)
    
    output$prop_test <- renderValueBox({
      
      valueBox(paste("p value:",round(test_stat(),3)), width = NULL,
               subtitle = paste("Prop test:",input$feature_prop_test),
                        color = if_else(test_stat()<.10, "lime", "red")
               )
    })
    
    output$proportions_test_value <- renderText(test_stat())
    
 
    
    #lower left plot
    output$clusterplot2 <- renderPlot({
        fviz_nbclust(df_cluster, kmeans, method="silhouette")
    })
    

    #  results
    output$results_plot <- renderPlot({
        ggbetweenstats(
            df_cluster2() %>%
              mutate(feature = as.factor(.data[[input$feature_x]])) %>% 
                filter(measure == input$feature_y),
            x=feature, 
            y=scaled,
            xlab = paste("Group variable: ", input$feature_x),
            ylab = paste ("Measure: ", input$feature_y),
            #ggtheme = ggthemes::theme_fivethirtyeight(),
            pairwise.display = "all",
            conf.level = .90)
        
    })
    
    output$item_plot <- renderPlot({
      df_item %>% 
        mutate(loading2 = if_else (loading <= input$load_thresh, "low","normal")) %>% 
        filter (scale %in% input$item_scales) %>% 
        ggplot(aes(x=mean, y=sd, color=loading2)) +
        geom_jitter(size=2) +
        scale_color_manual(values=c("low" = "red", "normal"="darkgray")) +
        geom_vline(xintercept = item_mean, linetype="dashed", color="blue") +
        geom_hline(yintercept = item_sd, linetype="dashed", color="blue") +
        theme(legend.position = "blank", strip.text = element_text(size=10) ) +
        facet_wrap(scale~.)+
        xlim(2,7) + ylim (.5,2) +
        ylab("item score sd") + 
        xlab("item mean score") 
        
    })
    
    output$data <- renderTable({
      brushedPoints(df_item, input$plot_brush) %>% select(item, mean, sd, loading, reverse_scored, content) %>% arrange(-loading)
    })
    


#scree plot
  output$scree_plot <- renderPlot ({
    fa.parallel(df_scree, fm = "minres", fa = "fa") 
    })
  
  #factor analysis solution
  solution <- reactive ({
    fa(r=corMat, 
                 nfactors = input$factors, rotat="oblimin", fm="minres")
  })
  
  #results data frame
  df_efa <- reactive({
    solution()[["loadings"]] %>% 
    as.matrix.data.frame() %>% 
    as.data.frame() %>% 
    cbind(solution()$weights %>% 
            as.data.frame() %>% 
            rownames_to_column("item") %>% 
            dplyr::select(item)) %>% 
    dplyr::select(item, everything()) %>% 
    gather(2:(input$factors+1), key=factor, value=loading) %>% 
    dplyr::group_by(item) %>% 
    mutate(rank = rank(-1*abs(loading))) %>% 
    filter(rank<=1) %>% 
    arrange(factor, -loading) %>% 
      mutate(loading = round(loading,3)) %>% 
    left_join(mslq_questions) %>% 
      arrange(factor, -loading) %>% 
      dplyr::select(item:scale, content)
  })
  

  
  output$efa_table <- renderDataTable({
    df_efa() %>% group_by(factor, scale) %>% 
      summarise(count = n()) %>% 
      pivot_wider(names_from = "factor", values_from = "count") %>% 
      mutate_if(is.numeric, ~replace_na(.x, "-"))
  })
    
  output$efa_table2 <- renderDataTable({df_efa()})
  #model fit metrics
  
   eval <- reactive({
     cbind(solution()$rms, solution()$fit) %>% as.data.frame() %>% 
    mutate_if(is.numeric, ~round(.x, 3)) %>%
    rename(RMS = 1, Fit =2) 
   })
   
  output$eval <- renderPrint({
    eval <- paste("RMS for ", input$factors, "factors = ", eval()$RMS, "; Model Fit = ", eval()$Fit )
    eval
  })
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)
