#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# TODO: Change font
# TODO: Font colors
# TODO: Sankey link color
# TODO: Add fade to hr()


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        tags$style('#info_column {display: flex; align-items: center; justify-content: center;};'),
        tags$style(".btn-group button{background-color:#FAEFE8; border-color:#2A153D; color: #2A153D;}"),
        tags$style(".btn-group button.active{background-color:#2A153D; color: #FAEFE8}"),
        tags$style('.hr1 {border: 0; height: 1px; background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.3), rgba(0, 0, 0, 0));}'),
        setBackgroundColor(color = "#FAEFE8"),
        #### Header / Intro ----
        fluidRow(
            height = "200px",
            align="center",
            column(
                width = 6,
                offset = 3,
                tags$br(),
                tags$br(),
                tags$h1("Visualizing a Career Shift"),
            )
        ), # Header / Intro close
        column(
            width = 6,
            offset = 3,
            tags$br(),
            tags$hr(class="hr1"),
            tags$br(),
        ),
        
        #### Sankey ----
        fluidRow(
            height = "55%",
            column(
                width = 3,
                offset = 2,
                align = "center",
                id = "info_column",
                column(
                    width = 12,
                    tags$h3("Breaking into Tech"),
                    tags$p("After 4 years of working as a biostatistician and manager at a biotech start up, how hard could it be to enter into a more engineering focused data science role? - Harder than I thought."),
                    tags$p("Over the span of almost 2 years, 239 job applications were sent out to companies across Colorado, most of which were promptly rejected.")
                )
            ),
            
            column(
                width = 5,
                plotlyOutput("sankey", height = "600px")
            )
        ), # Sankey Close
        
        column(
            width = 6,
            offset = 3,
            tags$br(),
            tags$hr(class="hr1"),
            tags$br(),
        ),
        
        #### Timeline ----
        fluidRow(
            column(
                width = 8, 
                offset = 2, 
                align = "center",
                tags$h2("The Timeline"),
            )),
        fluidRow(
            column(
                width = 5,
                offset = 2,
                plotlyOutput("timeline")
            ),
            column(
                width = 3,
                align = "center",
                id = "info_column",
                column(
                    width = 12,
                    tags$h3("COVID-19 Shifts"),
                    tags$p("Interviews in 2020 (after the COVID-19 pandemic began) were frequently conducted remotely by video - a feature that didn't appear at all in 2019.")
                )
            ),
        ), # Timeline Close
        column(
            width = 6,
            offset = 3,
            tags$br(),
            tags$hr(class="hr1"),
            tags$br(),
        ),
        #### Response Plots ----
        fluidRow(
            column(
                width = 8,
                offset = 2,
                align = "center",
                tags$h2("Measuring Responses"),
                tags$h6("Select a category below to see information on how my applications were received based on different factors."),
                tags$br(),
                radioGroupButtons(
                    inputId = "response_variable",
                    label = NULL,
                    status="myClass",
                    choices = c("Industry",
                                "Year",
                                "Company Size"="Company.Size",
                                "Job Title"="Title",
                                "Application Site"="site"),
                    selected = "Industry",
                    direction = "horizontal"
                )
            )
        ),
        tags$br(),
        fluidRow(
            column(
                width = 4,
                offset = 2,
                align = "center",
                tags$h4("Probability of Getting Any Interview"),
                plotlyOutput("interview_prob")
            ),
            column(
                width = 4,
                align = "center",
                tags$h4("Probability of Getting a Second Interview"),
                plotlyOutput("interview2_prob")
            )
        ),
        tags$br(),
        tags$br(),
        fluidRow(
            column(
                width = 4,
                offset = 2,
                align = "center",
                tags$h4("Summary"),
                conditionalPanel(condition = "input.response_variable == 'Industry'",
                                 tags$p("Follow-up interviews were only obtained in Communications, Medical, Retail, and SaaS companies. 
                                            This left a large disparity between first and follow-up interviews in some fields like Marketing (20% interview vs 0% follow-up)"),
                                 tags$br(),
                                 tags$p("Not a single staffing agency provided notifications on whether or not I was still a candidate for a given position.")),
                conditionalPanel(condition = "input.response_variable == 'Year'",
                                 tags$p("Zero follow-up interviews were obtained in 2019 but nearly 7% of applications in 2020 resulted in multiple contacts. Likely, this is due to me starting a graduate degree Data Science late in 2019."),
                                 tags$p("The difference in follow-up between the years was the only statistically significant trend.")),
                conditionalPanel(condition = "input.response_variable == 'Company.Size'",
                                 tags$p("Small companies appear to be the most willing to consider applicants with varied backgrounds like me."),
                                 tags$p("Generally, the larger comapnies were more consistent about notifying me when I was no longer under consideration for the position."),
                                 tags$br(),
                                 tags$p("Out of the 239 applications, only 5 companies - all small-medium sized - responded with a non-automated email.")),
                conditionalPanel(condition = "input.response_variable == 'Title'",
                                 tags$p("I received very positive responses to applications for Data Scientist positions based on the depth of NLP work I had accomplished but, 
                                            according to some feedback, I was lacking on breadth at the time."),
                                 tags$p("Applications to other titles such as Statistician, Data Engineer, and Machine Learning Developer were the most commonly left un-notified.")),
                conditionalPanel(condition = "input.response_variable == 'site'",
                                 tags$p("Anecdotally, LinkedIn provided the best quality job postings as well as the highest chance for interviews and follow-ups."),
                                 tags$p("ZipRecruiter, likely due to the high prevalence of staffing agencies on the site, provided the worst experience in terms of communication."))
            ),
            column(
                width = 5,
                align = "center",
                tags$h4("What Happened When I Didn't Get The Job?"),
                         conditionalPanel(condition = "input.response_variable == 'Industry'",
                                          plotlyOutput("ind_response")),
                         conditionalPanel(condition = "input.response_variable == 'Year'",
                                          plotlyOutput("year_response")),
                         conditionalPanel(condition = "input.response_variable == 'Company.Size'",
                                          plotlyOutput("csize_response")),
                         conditionalPanel(condition = "input.response_variable == 'Title'",
                                          plotlyOutput("title_response")),
                         conditionalPanel(condition = "input.response_variable == 'site'",
                                          plotlyOutput("site_response"))
                 )),
        tags$br(),
        tags$br(),
        tags$br()
    )
)
