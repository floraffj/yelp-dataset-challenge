library(shiny)
library(shinydashboard)
library(plotly)


dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "Yelp Dataset Challenge Project"),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Introduction",tabName = 'intro',icon = icon("yelp")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("bar-chart"),
               menuItem("Check-In Map",tabName = "checkin_map",icon = icon("fa-check-circle")),
               menuItem("Star Rating & Price",tabName = "star_price",icon = icon("fa-star"))),
               
      menuItem("ANOVA for Model Selection", icon = icon("cubes"), tabName = "anova"),
      menuItem("Check-Ins Model",icon = icon("cube"),tabName = "model")
  )),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              
                fluidRow(
                  textOutput('title'),
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Mate SC"),
                            tags$style("#title{color: white;font-size: 40px;text-indent: 50px;text-align: center;background-color: #AF2323;
                                       font-weight: bold;font-family: 'Mate SC';letter-spacing: 3px;}"))
                  ),
              fluidRow(
                HTML('<br>')
              ),
              
                fluidRow(
                  HTML('<div style="text-align: center;"><img src="yelp-dataset.jpg" style="max-width: 100%; height: auto;padding-left: 70px;padding-right: 70px;padding-bottom: 30px;"></div>')
                ),

                fluidRow(
                  textOutput('execu_sum'),
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Galada"),
                            tags$style("#execu_sum{color: black;font-size: 20px;text-indent: 70px;list-style-type: circle;
                                       font-weight: bold;padding-top: 20px;font-family: 'Galada';}"))
                ),
                fluidRow(
                  textOutput('summary'),
                  tags$head(tags$style("#summary{color: black;font-size: 18px;text-indent: 20px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 20px;}"))
                ),
                fluidRow(
                  textOutput('DesData'),
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Galada"),
                            tags$style("#DesData{color: black;font-size: 20px;text-indent: 70px;list-style-type: circle;
                                       font-weight: bold;padding-top: 20px;font-family: 'Galada';}"))
                  ),
                fluidRow(
                  textOutput('des_data_1'),
                  tags$head(tags$style("#des_data_1{color: black;font-size: 18px;text-indent: 20px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 20px;}"))
                  ),
                fluidRow(
                  htmlOutput('des_data_bullet'),
                  tags$head(tags$style("#des_data_bullet{color: black;font-size: 18px;text-indent: 20px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
                  )
              
      ),
      
      tabItem(tabName = "checkin_map",
              fluidRow(
                textOutput('cmap_title'),
                tags$head(tags$style("#cmap_title{color: white;font-size: 25px;text-indent: 50px;text-align: center;background-color: #AF2323;
                                     font-weight: bold;letter-spacing: 5px;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                column(12,align="center",
                       selectInput("checkmap_time","Choose Meal Time",choices = c("Workday Lunch"="work_lunch","Workday Dinner"="work_dinner",
                                                                         "Weekend Lunch"="weekend_lunch","Weekend Dinner"="weekend_dinner")),
                       plotlyOutput("checkin_map")
                       )
                
              ),
              fluidRow(HTML("<br>")),
              fluidRow(textOutput('cscat_title'),
                       tags$head(tags$style("#cscat_title{color: white;font-size: 25px;text-indent: 50px;text-align: center;background-color: #AF2323;
                                            font-weight: bold;letter-spacing: 5px;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                column(12,align="center",
                       selectInput("scat_checkin_time","Time",choices = c("Workday Lunch"="work_lunch","Workday Dinner"="work_dinner",
                                                                          "Weekend Lunch"="weekend_lunch","Weekend Dinner"="weekend_dinner")),
                       plotlyOutput("scat_checkin")
                )
                
              ),
              fluidRow(HTML("<br>")),
              fluidRow(textOutput('sum_checkin_title'),
                       tags$head(tags$style("#sum_checkin_title{color: white;font-size: 25px;text-indent: 50px;text-align: center;background-color: #AF2323;
                                            font-weight: bold;letter-spacing: 5px;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(htmlOutput('sum_checkin'),
                       tags$head(tags$style("#sum_checkin{color: black;font-size: 18px;text-indent: 20px;
                                            padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              )
                       
              
      ),
      
      tabItem(tabName = "star_price",
              fluidRow(
                textOutput('s_title'),
                tags$head(
                          tags$style("#s_title{color: white;font-size: 25px;text-align: center;background-color: #AF2323;
                                     font-weight: bold;letter-spacing: 5px;padding-left: 70px;padding-right: 70px;}"))
              ),
              fluidRow(
                textOutput('s_summary'),
                tags$head(
                  tags$style("#s_summary{color: black;font-size: 18px;text-indent: 20px;
                                            padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                column(12,align="center",
                       plotlyOutput("star"))
                
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                textOutput('p_title'),
                tags$head(
                  tags$style("#p_title{color: white;font-size: 25px;text-align: center;background-color: #AF2323;
                                     font-weight: bold;letter-spacing: 5px;padding-left: 70px;padding-right: 70px;}"))
              ),
              fluidRow(
                textOutput('p_summary'),
                tags$head(
                  tags$style("#p_summary{color: black;font-size: 18px;text-indent: 20px;
                                            padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
                  ),
              fluidRow(HTML("<br>")),
              fluidRow(
                column(12,align="center",
                       plotlyOutput("price"))
                
                )
              
      ),
      
      
      tabItem(tabName = "anova",
              fluidRow(
                textOutput("anova_title"),
                tags$style("#anova_title{color: black;font-size: 20px;text-indent: 70px;list-style-type: circle;
                                     font-weight: bold;padding-top: 20px;}")
              ),
              fluidRow(
                htmlOutput("anova_text"),
                tags$head(tags$style("#anova_text{color: black;font-size: 18px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              ),
              fluidRow(
                withMathJax(),
                uiOutput("model1"),
                tags$head(tags$style("#model1{color: black;font-size: 18px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              ),
              fluidRow(
                htmlOutput("anova_decision"),
                tags$head(tags$style("#anova_decision{color: black;font-size: 18px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              ),
              fluidRow(
                tags$head(tags$style(HTML("div.checkbox {margin-left: 70px; font-size: 18px; width: 100%;}"))),
                checkboxInput("anova_checkbox","Show Raw Result of ANOVA from R")
              ),
              fluidRow(
                htmlOutput("anova_subtitle"),
                tags$head(tags$style("#anova_subtitle{color: black;font-size: 18px;
                                       padding-left: 70px;padding-right: 70px;padding-top: 10px;list-style-type: circle;}"))
              ),
              fluidRow(
                 verbatimTextOutput("anova1"),
                 tags$head(tags$style("#anova1{padding-left: 70px;padding-right: 70px;}"))
              ),
              fluidRow(
                verbatimTextOutput("anova2"),
                tags$head(tags$style("#anova2{padding-left: 70px;padding-right: 70px;}"))
              ),
              fluidRow(
                verbatimTextOutput("anova3"),
                tags$head(tags$style("#anova3{padding-left: 70px;padding-right: 70px;}"))
              ),
              fluidRow(
                verbatimTextOutput("anova4"),
                tags$head(tags$style("#anova4{padding-left: 70px;padding-right: 70px;}"))
              )
      ),
      
      tabItem(tabName = "model",
              fluidRow(
                textOutput("m_title"),
                tags$head(
                tags$style("#m_title{color: black;font-size: 20px;text-indent: 70px;list-style-type: circle;
                           font-weight: bold;padding-top: 20px;}"))
              ),
              fluidRow(
                htmlOutput("coefp_subtitle"),
                tags$head(
                  tags$style("#coefp_subtitle{color: black;font-size: 18px;padding-left: 60px;list-style-type: circle;
                             font-weight: bold;padding-top: 20px;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                plotlyOutput("coefp")
                ),
              fluidRow(
                htmlOutput("m_subtitle"),
                tags$head(
                tags$style("#m_subtitle{color: black;font-size: 18px;padding-left: 60px;list-style-type: circle;
                           font-weight: bold;padding-top: 20px;}"))
              ),
              fluidRow(
                htmlOutput("m_text"),
                tags$head(
                  tags$style("#m_text{color: black;font-size: 18px;list-style-type: circle;
                           padding-bottom: 20px;padding-left: 80px;padding-right: 80px;}"))
              ),
              fluidRow(
                htmlOutput("me_coefp_subtitle"),
                tags$head(
                  tags$style("#me_coefp_subtitle{color: black;font-size: 18px;padding-left: 60px;list-style-type: circle;
                             font-weight: bold;padding-top: 20px;}"))
              ),
              fluidRow(HTML("<br>")),
              fluidRow(
                plotOutput("me_coefp")
              ),
              fluidRow(
                htmlOutput("m_subtitle2"),
                tags$head(
                  tags$style("#m_subtitle2{color: black;font-size: 18px;padding-left: 60px;list-style-type: circle;
                           font-weight: bold;padding-top: 20px;}"))
              ),
              fluidRow(
                htmlOutput("m_text2"),
                tags$head(
                  tags$style("#m_text2{color: black;font-size: 18px;list-style-type: circle;
                             padding-bottom: 20px;padding-left: 80px;padding-right: 80px;}"))
              )
              )
)
)
)
  
