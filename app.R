# This is the user-interface definition of a Shiny web application. 

library(shiny)
library("dplyr")
library("knitr")
options(knitr.table.format = "html") 
library("httr")
library("DT")
library("maps")
library("ggplot2")
library("sp")
library(maptools)
library(rsconnect)
library('reshape2')
library('plotly')
library("shinythemes")

data <- read.csv("MERGED2015_16_PP1.csv")
data1 <- read.csv("Most-Recent-Cohorts-Scorecard-Elements.csv")
data2 <- head(data1, n=20) %>%
  select(MD_EARN_WNE_P10, RPY_3YR_RT_SUPP)

first <- c(29.9,40.2,40.1,45.6,26.7,42.7,27.2,38.5,33.5,47.1,
           45.6,26.3,20.6,30.3,29.8,28.7,35.9,25.4)
second <- c(0.246,0.52,0.233,0.55,0.20,0.60,0.26,0.49,0.416,0.731,
            0.748,0.29,0.12,0.23,0.44,0.36,0.30,0.4)
data3 <- data.frame(first, second)



my.ui <- fluidPage(    
  shinythemes::themeSelector(),
  # Give the page a title
  titlePanel(strong("Colleges Data")),
  
  navbarPage("Menu",
    tabPanel("Overview",
      h3("Overview"),
      p("The report provides a general overview of two of the most important things 
        college students worry about: picking majors while in the college and paying 
        back loans after college. With the audience of incoming and current college 
        students in mind, this report with data set from all the colleges over the 
        United States will give students some inspiration of majors to pick and a 
        general financial planning after college. We want to use this data set to 
        dispel some misunderstanding and present the data visually to the audience. 
        We wish this data set will inspire audience to pick the major they are interested 
        in but not that most popular major and prepare audience for post college loan repayment 
        plan."),
      h3("Audience"),
      p("The target audience of this report is incoming and current college students. For incoming students, 
        they will have a general overview about what majors college offers and can start thinking about picking majors. 
        For the current students, it will be helpful for them to have a better understanding of other majors without 
        the stereo type they have in their head. This report will also be helpful for the parents to see this report to 
        understand their children’s college life and help their children prepare for the post college repayment. We expect our 
        report will help our audience to pick a major and prepare for the post graduation."),
      h3("Data"),
      p("The dataset we are working with contains data on median income rate after 10 years of graduation, repayment rate, and 
        percentage of different major from different colleges across united state. This data was collected by the U.S. department 
        of education and if you are interested more about it , the whole dataset can be found link:",
        a("collegescorecard", href='https://collegescorecard.ed.gov/data/documentation/')),
      h3("Questions"),
      p("Some questions we had for this dataset included:"),
      p(em("Since some of the majors are really popular among students, are these majors have the  majority of student population? Does the unpopular majors really doesn't have that much students in the program?")),
      p(em("How long can you pay back all the loans in college, is it really that scary like people said that it will take a life time?")),
      h3("Structure"),
      p("The first tab contains a simple overview analysis of this report and the next two taps contains two different visualization 
        and tables information about the major and repayment rate. Users can change the table or data by using the sidebar on the left.
        Above each chart is an overview - describing the chart and its features, along with the conclusions that we have come to."),
      h3("Future Analysis"),
      p(em("How can user spend money wisely to lower to pay back after college ?")),
      p(em("Is it true that college on the west coast has more engineering students and colleges on the east coast has more finance students?"))),
    
    tabPanel("Repayment Rates",
      p("This table display median income after 10 years of graduation of college and repayment rate of 3 years based on user input of different school.
          The plot sum up different school with different repayment rate and income rate. It is clearly shown on the graph that with a higher income, 
          the repayment rate is higher. However, there are outliers that a relative low income still has a relative high repayment rate. Which gives us 
          the conclusion that on average after 6 years, people are able to repay their college loan. Even though the income is that high, its still manageable for people to pay back college loan."),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12,
                   selectInput("college",
                               label = "College:",
                               choices = c("All",
                                 unique(as.character(data$INSTNM))))
            ))),
        mainPanel(
          DT::dataTableOutput("return"),
          plotOutput("p1")
          ))),
    tabPanel("Majors Distribution",
      p("This graph displays the major percentage with a college user selected. It is clearly indicated on the graph that some of the popular major has only slightly 
        higher percentage of students than other majors which means college consider the balance of the different major and their values. Not everyone need to be a CSE 
        major to success. And students should pick what they want to study instead of what is popular. And based on the school picked on west coast and east coast, finance 
        major has slightly higher percentage on the east coast and engineering major has a slightly higher percentage on the west coast. "),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12,
                textInput("college1",
                  label = "College:",
                    #choices = c(unique(as.character(data$INSTNM))),
                  value = "University of Washington-Seattle Campus")
                   ))),
               mainPanel(
                plotOutput("plot"),
                img(src="legend.png",width = 200,height = 300))))
))
?textInput
my.server <- function(input, output) {
  
  majors <- reactive({
    temp1 <- select(data, INSTNM, PCIP03, PCIP04, PCIP09,
                     PCIP11, PCIP13, PCIP14, PCIP16, 
                     PCIP22, PCIP23, PCIP24, PCIP26, PCIP27,
                     PCIP30, PCIP38, PCIP40, PCIP42,
                     PCIP43, PCIP44, PCIP45, PCIP50,
                     PCIP51, PCIP52, PCIP54)
    temp1 <- temp1[temp1$INSTNM == input$college1 ,]
    return(temp1)
  })
  
  output$return <- DT::renderDataTable(DT::datatable({
    colleges <- input$college
    earning <- select(data1, INSTNM,MD_EARN_WNE_P10, RPY_3YR_RT_SUPP) 
    if (colleges != "All") {
      earning <- earning[earning$INSTNM == colleges,]
    }
    return(earning)
  }))
  
  output$p1 <- renderPlot({
    reg<-lm(second~first , data = data3)
    plot(data3,ylab='Repayment rate in 3 years',xlab = 'Median income after 10 years graduation(thousands)',main = 'Return Rate',col=4)
    abline(reg, col="red")
  })
  
  output$plot <- renderPlot({
    origin <- suppressWarnings(as.numeric(t(majors())))
    barplot(as.numeric(na.omit(origin)), col = c(1:23))
    })
}

shinyApp(ui = my.ui, server = my.server)

