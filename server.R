#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(scales)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #### Color References ----
    # https://colorhunt.co/palette/92339
    # https://colorhunt.co/palette/220248
    pal <- list(
        "background" = "#FAEFE8",
        "purple" = "#2A153D",
        "red" = "#F53D4C",
        "blue" = "#68F3E5",
        "yellow" = "#F2D73F",
        "orange" = "#FFA333",
        "grey" = "#d9d7db"
        
        
    )
    
    #### Helper Functions ----
    prop_test_results <- function(data, column, n_int){
        
        col <- sym(column)
        
        suppressMessages(temp <- data %>%
                             filter(!is.na(!!col)) %>%
                             group_by(!!col) %>%
                             summarise(tot = length(unique(job_id)),
                                       interview = length(unique(job_id[n_contact == n_int]))) %>%
                             filter(tot >= 10) %>%
                             ungroup())
        
        if(nrow(temp) < 2){
            return(NULL)
        } else{
            
            prop_res <- prop.test(x = temp$interview, n = temp$tot)
            names(prop_res$estimate) <- temp %>%
                pull(!!col) %>%
                unique()
            
            prop_res$sig <- ifelse(prop_res$p.value > 0.05, "ns", 
                                   ifelse(prop_res$p.value <= 0.05 & prop_res$p.value > 0.01, "*", "**"))
            
            if(prop_res$p.value <= 0.05 & nrow(temp) > 2){
                pair_res <- pairwise.prop.test(x = temp$interview, n = temp$tot, p.adjust.method = "holm")
                
                return(pair_res)
                
            } else {
                
                return(prop_res)
                
            }
        } 
    }
    
    
    
    #### Data Initialization ----
    
    # Sankey
    sankey_df <- data.frame("label"=c("Application", "Phone Interview", "Video Interview", "In-Person Interview", "No Response", "Denied", "Withdrew", "Job Offer")) %>%
        rownames_to_column(var = "id") %>%
        mutate(id = as.numeric(id)-1) %>%
        mutate(color = c(pal$purple, pal$orange, pal$orange, pal$orange, pal$red, pal$red, pal$red, pal$blue))
    
    sankey_data <- readRDS("./data/sankey_data.rds")
    
    # Timeline
    timeline_data <- readRDS("./data/timeline_data.rds")
    
    # Response Charts
    ## Industry Responses
    ind_pie_data <- readRDS("./data/ind_pie_data.rds")
    status_colors <- data.frame(
        "Status" = c("Denied", "No Response", "Withdrew", "Job Offer"),
        "status_color" = c(pal$orange, pal$red, pal$grey, pal$blue)
    )
    
    ind_pie_data <- ind_pie_data %>%
        left_join(status_colors, by = "Status") %>%
        filter(Status %in% c("Denied", "Ghosted"))
    inds <- ind_pie_data %>%
        arrange(-tot) %>%
        pull(name) %>%
        unique
    
    ## Year Response
    year_pie_data <- readRDS("./data/year_pie_data.rds")

    ## Company Size Responses
    csize_pie_data <- readRDS("./data/csize_pie_data.rds")

    ## Title Responses
    title_pie_data <- readRDS("./data/title_pie_data.rds")
    
    ## Site Responses
    site_pie_data <- readRDS("./data/site_pie_data.rds")
    
    ## Interview Changes
    proptest_data <- readRDS("./data/proptest_data.rds")
    proptest_data <- proptest_data %>%
        mutate(Company.Size = factor(Company.Size, levels = c("Small", "Medium", "Large", "Very Large")))
    
    

    
    #### Sankey Plot ----
    output$sankey <- renderPlotly({
        
        p <- plot_ly(
            type = "sankey",
            valueformat = ".0f",
            domain = c(
                x =  c(0,10),
                y =  c(0,10)
            ),
            orientation = "h",
            node = list(
                label = as.character(sankey_df$label),
                color = sankey_df$color,
                pad = 100,
                thickness = 25,
                hovertemplate = "%{label}",
                line = list(
                    color = "black",
                    width = 2
                ),
                x = c(
                    0, # Application
                    .4, .5,  .6,  # Interviews
                    .85, .85, .85, 1 # Outcomes
                ),
                y = c(
                    0, 
                    .75, .8, .85, # Interviews
                    .25, .6, .9, .95 # Outcomes
                )
                
            ),
            
            link = list(
                source = sankey_data$Source,
                target = sankey_data$target,
                value =  sankey_data$weight,
                hovertemplate = "%{source.label} -> %{target.label}",
                color = pal$grey
            )
        ) %>% 
            layout(
                # title = "Energy forecast for 2050, UK - Department of Energy & Climate Changes",
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                font = list(
                    size = 12
                ),
                xaxis = list(showgrid = F, zeroline = F),
                yaxis = list(showgrid = F, zeroline = F)
            )
        
        
        p %>%
            config(displayModeBar = FALSE) 
            
        
        
    })
    

    
    #### Timeline Plot ----
    output$timeline <- renderPlotly({
        p <- timeline_data %>%
            ggplot() + 
            geom_bar(
                color = "black",
                aes(x = Date, 
                    y = `Job Search Activities (n)`, 
                    fill = factor(Activity, levels = c("In-Person Interview", "Video Interview", "Phone Interview", "Application")),
                    text = paste0(Activity, " : ", `Job Search Activities (n)`)
                ),
                stat = "identity",
                position = "stack",
                alpha = .9
            ) +
            scale_x_datetime(
                date_breaks = "1 month", 
                labels = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n")) +
            scale_y_continuous(breaks = pretty_breaks()) +
            scale_fill_manual(values = c(
                pal$red,
                pal$orange,
                pal$yellow,
                pal$purple
            )) + 
            theme(panel.grid = element_blank())
        
        ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(
                    orientation = "h", 
                    xanchor = "center", 
                    x = 0.5,
                    bgcolor = 'rgba(0,0,0,0)'))
        
        
    })
    
    #### Response Plots ----
    # Industry Responses
    output$ind_response <- renderPlotly({
        
        ind_plot <- ind_pie_data %>%
            mutate(name = name %>% str_replace(" (?=\\()", "\n")) %>%
            mutate(ghost = pie[Status == "Ghosted"]) %>%
            ggplot() + 
            geom_bar(aes(x = reorder(name, ghost), y = pie, fill = Status,
                         text = paste0(Status, ": ", round(pie*100, 2), "%")),
                     color = "black",
                     stat = "identity",
                     position = "fill",
                     alpha = 0.9) +
            ylab("Responses") +
            xlab(NULL) +
            scale_fill_manual(values = c(pal$purple, pal$red)) +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() + 
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank())
        
        
        ggplotly(ind_plot, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(orientation = "h",   
                              xanchor = "center", 
                              x = 0.5,
                              y = -0.2,
                              bgcolor = 'rgba(0,0,0,0)')
                ) 


        
    })
    
    output$year_response <- renderPlotly({
        
        
        year_plot <- year_pie_data %>%
            mutate(name = name %>% str_replace(" (?=\\()", "\n")) %>%
            mutate(ghost = pie[Status == "Ghosted"]) %>%
            ggplot() + 
            geom_bar(aes(x = reorder(name, ghost), y = pie, fill = Status,
                         text = paste0(Status, ": ", round(pie*100, 2), "%")),
                     color = "black",
                     stat = "identity",
                     position = "fill",
                     alpha = 0.9) +
            ylab("Responses") +
            xlab(NULL) +
            scale_fill_manual(values = c(pal$purple, pal$red)) +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank())

        
        ggplotly(year_plot, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(orientation = "h",   
                              xanchor = "center", 
                              x = 0.5,
                              y = -0.2,
                              bgcolor = 'rgba(0,0,0,0)')
            ) 
        

    })
    
    output$csize_response <- renderPlotly({
        
        
        csize_plot <- csize_pie_data %>%
            mutate(name = name %>% str_replace(" (?=\\()", "\n")) %>%
            mutate(ghost = pie[Status == "Ghosted"]) %>%
            ggplot() + 
            geom_bar(aes(x = reorder(name, ghost), y = pie, fill = Status,
                         text = paste0(Status, ": ", round(pie*100, 2), "%")),
                     color = "black",
                     stat = "identity",
                     position = "fill",
                     alpha = 0.9) +
            ylab("Responses") +
            xlab(NULL) +
            scale_fill_manual(values = c(pal$purple, pal$red)) +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank())
        
        
        ggplotly(csize_plot, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(orientation = "h",   
                              xanchor = "center", 
                              x = 0.5,
                              y = -0.2,
                              bgcolor = 'rgba(0,0,0,0)')
            ) 
        
        
    })
    
    
    output$title_response <- renderPlotly({
        p <- title_pie_data %>%
            mutate(name = name %>% str_replace(" (?=\\()", "\n")) %>%
            filter(Status %in% c("Denied", "Ghosted")) %>%
            mutate(ghost = pie[Status == "Ghosted"]) %>%
            ggplot() + 
            geom_bar(aes(x = reorder(name, ghost), y = pie, fill = Status,
                         text = paste0(Status, ": ", round(pie*100, 2))),
                     color = "black",
                     stat = "identity",
                     position = "fill",
                     alpha = 0.9) +
            ylab("Responses") + 
            xlab(NULL) +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() + 
            scale_fill_manual(values = c(pal$purple, pal$red)) + 
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank())
        
        ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(orientation = "h",   
                              xanchor = "center", 
                              x = 0.5,
                              y = -0.2,
                              bgcolor = 'rgba(0,0,0,0)')
            ) 
    })
    
    output$site_response <- renderPlotly({
        
        p <- site_pie_data %>%
            mutate(name = name %>% str_replace(" (?=\\()", "\n")) %>%
            mutate(ghost = pie[Status == "Ghosted"]) %>%
            ggplot() + 
            geom_bar(aes(x = reorder(name, ghost), y = pie, fill = Status,
                         text = paste0(Status, ": ", round(pie*100, 2))),
                     color = "black",
                     stat = "identity",
                     position = "fill",
                     alpha = 0.9) +
            ylab("Responses") + 
            xlab(NULL) +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            scale_fill_manual(values = c(pal$purple, pal$red)) + 
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank())
        
        ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(orientation = "h",   
                              xanchor = "center", 
                              x = 0.5,
                              y = -0.2,
                              bgcolor = 'rgba(0,0,0,0)')
            ) 
        
    })
    

    

    

    #### Interview Stats ----
    output$interview_prob <- renderPlotly({
        
        clean_name <- tools::toTitleCase(str_replace_all(input$response_variable, "\\.", " "))
        
        prop <- prop_test_results(proptest_data, input$response_variable, n_int = 2)
        p_df <- prop$estimate %>% 
            data.frame("y"=.) %>%
            rownames_to_column() %>%
            mutate(y = y * 100) 
        
        if(input$response_variable == "Company.Size"){
            p_df <- p_df %>%
                mutate(rowname = factor(rowname, levels = c("Small", "Medium", "Large", "Very Large")))
        }
        
        p <- p_df %>%
            ggplot() + 
            geom_bar(aes(x = rowname, 
                         y = y, 
                         text = paste0(round(y, 2), "%")),
                     fill = pal$purple,
                     color = "black",
                     width = 1,
                     alpha = .9,
                     stat = "identity") +
            scale_fill_gradient2(low = pal$grey, 
                                 mid = pal$purple, 
                                 high = pal$blue, midpoint = 10) +
            ylim(c(0,100)) + 
            ggtitle(paste0("p: ", round(prop$p.value, 4), prop$sig)) +
            xlab(NULL) +
            ylab("Probability (%)") +
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle=45),
                  legend.position = "none",
                  plot.title = element_text(hjust = 0.5, size = 12)) 
        
        ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(bgcolor = 'rgba(0,0,0,0)')
            ) 
    })
    
    
    output$interview2_prob <- renderPlotly({
        
        clean_name <- tools::toTitleCase(str_replace_all(input$response_variable, "\\.", " "))

        prop <- prop_test_results(proptest_data, input$response_variable, n_int = 3)
        p_df <- prop$estimate %>% 
            data.frame("y"=.) %>%
            rownames_to_column() %>%
            mutate(y = y * 100) 
        
        if(input$response_variable == "Company.Size"){
            p_df <- p_df %>%
                mutate(rowname = factor(rowname, levels = c("Small", "Medium", "Large", "Very Large")))
        }
        
        p <- p_df %>%
            ggplot() + 
            geom_bar(aes(x = rowname, 
                         y = y, 
                         text = paste0(round(y, 2), "%")),
                     fill = pal$purple,
                     color = "black",
                     width = 1,
                     alpha = .9,
                     stat = "identity") +
            scale_fill_gradient2(low = pal$grey, 
                                 mid = pal$purple, 
                                 high = pal$blue, midpoint = 10) +
            ylim(c(0,100)) + 
            ggtitle(paste0("p: ", round(prop$p.value, 4), prop$sig)) +
            xlab(NULL) +
            ylab("Probability (%)") +
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle=45),
                  legend.position = "none",
                  plot.title = element_text(hjust = 0.5, size = 12)) 
        
        ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                legend = list(bgcolor = 'rgba(0,0,0,0)')
            ) 
    })
    
})
