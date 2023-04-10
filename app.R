# Load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(caret)
library(tidyverse)
library(broom)

setwd(getwd())

# Load Dataset
telecomCustomerChurn <-
  read.csv("dataset/Telco-Customer-Churn.csv");

telecomCustomerChurn <- telecomCustomerChurn %>%
  mutate(Streaming = ifelse(StreamingTV == "Yes" | StreamingMovies == "Yes", "Yes", "No"))

ui <- dashboardPage(
  dashboardHeader(title = 'Telecom Customer Analysis'),
  dashboardSidebar(sidebarMenu(
    menuItem(
      'Customer Demographics',
      tabName = 'demographics',
      icon = icon('users')
    ),
    menuItem(
      'Account Details',
      tabName = 'account',
      icon = icon('dollar-sign')
    ),
    menuItem(
      'Telecom Services',
      tabName = 'telecom',
      icon = icon('calendar-alt')
    ),
    menuItem(
      'Correlation',
      tabName = 'corrSummary',
      icon = icon('tachometer')
    )
  )),
  dashboardBody(tabItems(
    # Demographics tab -----------------------------------
    tabItem(
      tabName = 'demographics',
      fluidRow(
        box(
          title = "Customer Demographics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 15,
          box(
            title = "Number of Customers",
            width = 4,
            status = "primary",
            infoBoxOutput("total.customers")
          ),
          box(
            title = "Churned Customers",
            width = 4,
            status = "primary",
            infoBoxOutput("churned.customers")
          ),
          box(
            title = "Churned Rate",
            width = 4,
            status = "primary",
            infoBoxOutput("churnRate.customers")
          ),
          box(
            title = "Retained Customers",
            width = 4,
            status = "primary",
            infoBoxOutput("retained.customers")
          ),
          box(
            title = "Retention Rate",
            width = 4,
            status = "primary",
            infoBoxOutput("retainRate.customers")
          ),
          box(
            title = "Total Charges",
            width = 4,
            status = "primary",
            infoBoxOutput("totalCharges.customers")
          )
        ),
      ),
      tags$p('Expand to see details of below items',
             style = 'font-size: 170%;margin-left:2.5em;'),
      fluidRow(
        box(
          title = "Distribution Plots",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 6,
          selectInput("customerChurned", "Customer Churned:",
                      choices = c("All", unique(telecomCustomerChurn$Churn)),
                      selected = "All"),
          tabBox(
            width = NULL,
            title = "Distribution plots",
            side = "left",
            tabPanel("Gender", plotOutput("gender", height = 350)),
            tabPanel("Senior Citizens", plotOutput("seniorCitizen", height = 350)),
            tabPanel("Partner", plotOutput("Partner", height = 350)),
            tabPanel("Dependents", plotOutput("Dep", height = 350))
          )
        ),
        box(
          title = "Tenure",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 6,
          tabBox(
            width = NULL,
            title = "Tenure",
            side = "left",
            tabPanel("Churned", plotOutput("tenureVSchurned", height = 350)),
            tabPanel("Contract", plotOutput("tenureVScontract", height = 350)),
            tabPanel("Senior Citizen", plotOutput("tenureVSc", height = 350)),
            tabPanel("Gender", plotOutput("tenureVSGender", height = 350))
          )
        )
      )
    ),
    # Account Detail tab -----------------------------------
    tabItem(tabName = 'account',
            box(
              title = "Charges",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              background = 'olive',
              tabBox(
                width = NULL,
                title = "Charges",
                side = "left",
                tabPanel("Monthly Charges", plotOutput("MChargesByChurn", height = 200)),
                tabPanel("Total Charges", plotOutput("TChargesByChurn", height = 200))
              )
            ),
            box(
              title = "Tenure",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              background = "navy",
              plotOutput("ChurnByTenure", height = 200)
            ),
            box(
              title = "Billing",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              background = 'olive',
              selectInput("customer_type", "Customer Churned:",
                          choices = c("All", unique(telecomCustomerChurn$Churn)),
                          selected = "All"),
              tabBox(
                width = NULL,
                title = "Billing",
                side = "left",
                tabPanel("Contract Type", plotOutput("BillingByContract", height = 200)),
                tabPanel("Payment Methods", plotOutput("PaymentMeth", height = 200)),
                tabPanel("Paperless Billing", plotOutput("PaperlessBill", height = 200))
              )
            ),
            
    ),
    # Telecom Services tab -----------------------------------
    tabItem(tabName = 'telecom',
            box(
              title = "Services",
              solidHeader = TRUE,
              collapsible = FALSE,
              collapsed = FALSE,
              width = 12,
              background = 'maroon',
              selectInput("customertype", "Customer Churned:",
                          choices = c("All", unique(telecomCustomerChurn$Churn)),
                          selected = "All"),
              tabBox(
                width = NULL,
                title = "Entertainment Services",
                side = "left",
                tabPanel("Internet Service", plotOutput("ChurnByInternetServices", height = 200)),
                tabPanel("Phone Service", plotOutput("ChurnByPhoneServices", height = 200)),
                tabPanel("Streaming Service", plotOutput("ChurnByStreamingMovies", height = 200))
              )
            )),
    # Correlation Summary tab -----------------------------------
    tabItem(
      tabName = 'corrSummary',
      h2('Regression Model For Tenure and Total Charges', align = 'center'),
      tags$p(
        'Here tenure is an exploratory variable and total charges is the response variable, as
              the tenure increases the total charges that the customers paid for services of customer
              also increases.',
        style = 'font-size: 120%;margin-left:2.5em;'
      ),
      tags$p('Tenure = 76.27 * (TotalCharges) – 189.52', style = 'font-size:
             120%;margin-left:2.5em;'),
      tags$p('R-squared value = 0.6821', style = 'font-size:
             120%;margin-left:2.5em;'),
      h2('Regression Plot', align = 'center'),
      fluidRow(shinydashboard::box(
        width = 12, plotOutput("regModel", height = 200)
      ))
    )
    
  ))
)
server <- function(input, output) {
  
  total.customers <- dim(telecomCustomerChurn)
  churned.customers <- dim(telecomCustomerChurn %>%
                             filter(Churn == 'Yes'))
  retained.customers <- dim(telecomCustomerChurn %>%
                              filter(Churn == 'No'))
  customers <- telecomCustomerChurn %>%
    filter(!is.na(TotalCharges))
  totalCharges.customers <- sum(customers$TotalCharges)
  
  # Fit a linear regression model for tenure and total charges
  lmmodel <- lm(TotalCharges ~ tenure, data = telecomCustomerChurn)
  
  
  # info boxes
  output$total.customers <- renderInfoBox({
    x = format(total.customers[1], big.mark = ",")
    infoBox("", x,
            color = "purple",
            icon = icon("users"))
  })
  output$churned.customers <- renderInfoBox({
    x = format(churned.customers[1], big.mark = ",")
    infoBox("", x,
            color = "orange",
            icon = icon("users"))
  })
  output$churnRate.customers <- renderInfoBox({
    x = format(round(churned.customers[1] / total.customers[1] * 100),
               big.mark = ",")
    infoBox("", x,
            color = "purple",
            icon = icon("percentage"))
  })
  output$retained.customers <- renderInfoBox({
    x = format(retained.customers[1], big.mark = ",")
    infoBox("", x,
            color = "orange",
            icon = icon("users"))
  })
  output$retainRate.customers <- renderInfoBox({
    x = format(round(retained.customers[1] / total.customers[1] * 100),
               big.mark = ",")
    infoBox("",
            x,
            color = "purple",
            icon = icon("percentage"))
  })
  output$totalCharges.customers <- renderInfoBox({
    total_charges_millions <- totalCharges.customers / 1e6
    total_charges_formatted <-
      comma(total_charges_millions, digits = 1)
    
    # Add the "M" suffix
    total_charges_formatted <- paste0(total_charges_formatted, "M")
    #x = format_number(total_charges_millions,accuracy = 0.1, scale = "M", suffix = " ")
    infoBox(
      "",
      total_charges_formatted,
      color = "orange",
      icon = icon("dollar-sign")
    )
  })
  
  output$totalCharges <- renderInfoBox({
    total_charges_millions <- totalCharges.customers / 1e6
    total_charges_formatted <-
      comma(total_charges_millions, digits = 1)
    
    # Add the "M" suffix
    total_charges_formatted <- paste0(total_charges_formatted, "M")
    #x = format_number(total_charges_millions,accuracy = 0.1, scale = "M", suffix = " ")
    infoBox(
      "",
      total_charges_formatted,
      color = "orange",
      icon = icon("dollar-sign")
    )
  })
  
  
  output$avg.monthly.charges <- renderInfoBox({
    x = format(round(mean(customers$MonthlyCharges)),
               big.mark = ",")
    infoBox("",
            x,
            color = "purple",
            icon = icon("calendar-alt"))
  })
  
  #plots
  output$gender <- renderPlot({
    if (input$customerChurned == "All") {
    telecomCustomerChurn %>%
      group_by(gender) %>%
      summarize(n = n()) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ggplot(aes(x = "", y = n, fill = gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Gender") +
      ggtitle("Gender Distribution") +
      scale_fill_manual(values = c("orange", "purple")) +
      geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customerChurned)
      filteredData %>%
        group_by(gender) %>%
        summarize(n = n()) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ggplot(aes(x = "", y = n, fill = gender)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = "Gender") +
        ggtitle("Gender Distribution") +
        scale_fill_manual(values = c("orange", "purple")) +
        geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
  })
  
  output$seniorCitizen <- renderPlot({
    if (input$customerChurned == "All") {
    telecomCustomerChurn %>%
      group_by(SeniorCitizen) %>%
      summarize(n = n()) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ggplot( aes(
        x = "",
        y = n,
        fill = factor(SeniorCitizen)
      )) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(name = "Senior Citizen", labels = c("No", "Yes"),values = c("orange", "purple")) +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Senior Citizen Distribution") +
      geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customerChurned)
      filteredData %>%
        group_by(SeniorCitizen) %>%
        summarize(n = n()) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ggplot( aes(
          x = "",
          y = n,
          fill = factor(SeniorCitizen)
        )) +
        geom_bar(stat = "identity", width = 1) +
        scale_fill_manual(name = "Senior Citizen", labels = c("No", "Yes"),values = c("orange", "purple")) +
        coord_polar("y", start = 0) +
        theme_void() +
        ggtitle("Senior Citizen Distribution") +
        geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
  })
  
  output$Partner <- renderPlot({
    if (input$customerChurned == "All") {
    telecomCustomerChurn %>%
      group_by(Partner) %>%
      summarize(n = n()) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ggplot( aes(
        x = "",
        y = n,
        fill = factor(Partner)
      )) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = c("orange", "purple"), name = "Partner", labels = c("No", "Yes")) +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Partner Distribution") +
      geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customerChurned)
      filteredData %>%
        group_by(Partner) %>%
        summarize(n = n()) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ggplot( aes(
          x = "",
          y = n,
          fill = factor(Partner)
        )) +
        geom_bar(stat = "identity", width = 1) +
        scale_fill_manual(values = c("orange", "purple"), name = "Partner", labels = c("No", "Yes")) +
        coord_polar("y", start = 0) +
        theme_void() +
        ggtitle("Partner Distribution") +
        geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
      }
  })
  
  output$Dep <- renderPlot({
    if (input$customerChurned == "All") {
    telecomCustomerChurn %>%
      group_by(Dependents) %>%
      summarize(n = n()) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ggplot( aes(
        x = "",
        y = n,
        fill = factor(Dependents)
      )) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = c("orange", "purple"), name = "Dependents", labels = c("No", "Yes")) +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Dependents Distribution") +
      geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customerChurned)
      filteredData %>%
        group_by(Dependents) %>%
        summarize(n = n()) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ggplot( aes(
          x = "",
          y = n,
          fill = factor(Dependents)
        )) +
        geom_bar(stat = "identity", width = 1) +
        scale_fill_manual(values = c("orange", "purple"), name = "Dependents", labels = c("No", "Yes")) +
        coord_polar("y", start = 0) +
        theme_void() +
        ggtitle("Dependents Distribution") +
        geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
                  position = position_stack(vjust = 0.5))
    }
  })
  
  output$MChargesByChurn <- renderPlot({
    ggplot(telecomCustomerChurn, aes(Churn %in% c("Stayed", "Churn"))) +
      geom_point(aes(x = tenure, y = MonthlyCharges, color= Churn)) +
      scale_color_manual(values =  c("orange", "#5cb85c")) +
      xlab("Months with company")+
      ylab("Monthly Charges (USD)") +
      theme_minimal()
  })
  
  output$TChargesByChurn <- renderPlot({
    ggplot(telecomCustomerChurn, aes(Churn %in% c("Stayed", "Churn"))) +
      geom_point(aes(x = tenure, y = TotalCharges, color= Churn)) +
      scale_color_manual(values =  c("orange", "#5cb85c")) +
      xlab("Months with company")+
      ylab("Total Charges (USD)") +
      theme_minimal()
  })
  
  output$BillingByContract <- renderPlot({
    if (input$customer_type == "All") {
      ggplot(telecomCustomerChurn,aes(x=Contract, fill = factor(Contract)))+
        geom_bar(position = "dodge",  fill = c("#5b94c5","#337ab7","#286192"), color = "#1e496d") +
        coord_flip() +
        geom_text(aes(label = paste(round(..count../nrow(telecomCustomerChurn)*100,2), "%")),
                  stat = "count", position = "dodge", hjust = -0.1) +
        labs(title = "All Customers", fill = '')+
        theme_minimal() +
        theme(legend.position = "none",
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank())
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customer_type)
      ggplot(filteredData,aes(x=Contract, fill = factor(Contract)))+
        geom_bar(position = "dodge",  fill = c("#5b94c5","#337ab7","#286192"), color = "#1e496d") +
        coord_flip() +
        geom_text(aes(label = paste(round(..count../nrow(telecomCustomerChurn)*100,2), "%")),
                  stat = "count", position = "dodge", hjust = -0.1) +
        labs(title = "All Customers", fill = '')+
        theme_minimal() +
        theme(legend.position = "none",
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank())
    }
  })
  
  output$PaymentMeth <- renderPlot({
    if (input$customer_type == "All") {
      # Plot all data
      ggplot(telecomCustomerChurn, aes(x = PaymentMethod, fill = factor(PaymentMethod))) +
        geom_bar(
          position = "dodge",
          fill = c("#addbad", "#7cc67c", "#5cb85c", "#499349"),
          color = ("#376e37")
        ) +
        coord_flip() +
        geom_text(
          aes(label = paste(round(
            ..count.. / nrow(telecomCustomerChurn) * 100, 2
          ), "%")),
          stat = "count",
          position = "dodge",
          hjust = -0.1
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    } else {
      # Plot filtered data
      filteredData <- subset(telecomCustomerChurn, Churn == input$customer_type)
      ggplot(filteredData, aes(x = PaymentMethod, fill = factor(PaymentMethod))) +
        geom_bar(
          position = "dodge",
          fill = c("#addbad", "#7cc67c", "#5cb85c", "#499349"),
          color = ("#376e37")
        ) +
        coord_flip() +
        geom_text(
          aes(label = paste(round(
            ..count.. / nrow(filteredData) * 100, 2
          ), "%")),
          stat = "count",
          position = "dodge",
          hjust = -0.1
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    }
  })
  
  output$PaperlessBill <- renderPlot({
    if (input$customer_type == "All") {
      
      ggplot(telecomCustomerChurn, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
        geom_bar(position = "dodge", fill = c("#f0ad4e", "#d89b46"), color = "#ba7412") +
        coord_flip() +
        geom_text(aes(label = paste(round(..count../nrow(telecomCustomerChurn)*100,2), "%")),
                  stat = "count", position = "dodge", hjust = -0.1) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank())
    }
    else{
      filteredData <- subset(telecomCustomerChurn, Churn == input$customer_type)
      ggplot(filteredData, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
        geom_bar(position = "dodge", fill = c("#f0ad4e", "#d89b46"), color = "#ba7412") +
        coord_flip() +
        geom_text(aes(label = paste(round(..count../nrow(filteredData)*100,2), "%")),
                  stat = "count", position = "dodge", hjust = -0.1) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank())
    }
    
  })
  
  output$ChurnByPhoneServices <- renderPlot({
    if (input$customertype == "All") {
      ggplot(telecomCustomerChurn, aes(x = PhoneService, fill = factor(PhoneService))) +
        geom_bar() +
        scale_fill_manual(values = c("#7cc67c", "#499349"), name = "Phone Service") +
        coord_flip() +
        geom_text(
          aes(label = paste0(round(
            ..count.. / sum(..count..) * 100, 1
          ), "%")),
          stat = "count",
          position = position_dodge(width = 1),
          vjust = -0.5
        ) +
        labs(x = "", y = "Count", title = "Phone Service Distribution") +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    }
    else{
      filteredData <-
        subset(telecomCustomerChurn, Churn == input$customertype)
      ggplot(filteredData, aes(x = PhoneService, fill = factor(PhoneService))) +
        geom_bar() +
        scale_fill_manual(values = c("#7cc67c", "#499349"), name = "Phone Service") +
        coord_flip() +
        geom_text(
          aes(label = paste0(round(
            ..count.. / sum(..count..) * 100, 1
          ), "%")),
          stat = "count",
          position = position_dodge(width = 1),
          vjust = -0.5
        ) +
        labs(x = "", y = "Count", title = "Phone Service Distribution") +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    }
  })
  
  output$ChurnByInternetServices <- renderPlot({
    if (input$customertype == "All") {
      ggplot(telecomCustomerChurn,
             aes(x = InternetService, fill = factor(InternetService))) +
        geom_bar(
          position = "dodge",
          fill = c("#99bcdb", "#5b94c5", "#286192"),
          color = "#1e496d"
        ) +
        coord_flip() +
        geom_text(
          aes(label = paste(round(
            ..count.. / nrow(telecomCustomerChurn) * 100, 2
          ), "%")),
          stat = "count",
          position = "dodge",
          hjust = -0.1
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    }
    else{
      filteredData <-
        subset(telecomCustomerChurn, Churn == input$customertype)
      ggplot(filteredData, aes(x = InternetService, fill = factor(InternetService))) +
        geom_bar(
          position = "dodge",
          fill = c("#99bcdb", "#5b94c5", "#286192"),
          color = "#1e496d"
        ) +
        coord_flip() +
        geom_text(
          aes(label = paste(round(
            ..count.. / nrow(filteredData) * 100, 2
          ), "%")),
          stat = "count",
          position = "dodge",
          hjust = -0.1
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()
        )
    }
  })
  
  output$ChurnByStreamingMovies <- renderPlot({
    if (input$customertype == "All") {
      ggplot(telecomCustomerChurn, aes(x = Streaming, fill = Streaming)) +
        geom_bar() +
        scale_fill_manual(values = c("#f0ad4e", "#d89b46")) +
        coord_flip() +
        geom_text(
          aes(label = paste0(
            round(..count.. / sum(..count..) * 100, 1),
            "% (", ..count.., ")"
          )),
          stat = "count",
          position = position_stack(vjust = 0.5),
          color = "white"
        ) +
        labs(title = "Streaming Services", x = "", y = "Count") +
        theme_minimal()
      
    }
    else{
      filteredData <-
        subset(telecomCustomerChurn, Churn == input$customertype)
      ggplot(filteredData, aes(x = Streaming, fill = Streaming)) +
        geom_bar() +
        scale_fill_manual(values = c("#f0ad4e", "#d89b46")) +
        coord_flip() +
        geom_text(
          aes(label = paste0(
            round(..count.. / sum(..count..) * 100, 1),
            "% (", ..count.., ")"
          )),
          stat = "count",
          position = position_stack(vjust = 0.5),
          color = "white"
        ) +
        labs(title = "Streaming Services", x = "", y = "Count") +
        theme_minimal()
    }
  })
  
  output$ChurnByTenure <- renderPlot({
    bar_fills <- c("#ffa600", "#58508d")
    ggplot(telecomCustomerChurn, aes(x=tenure, fill= Churn))+
      geom_histogram(stat = 'bin',
                     bins = 50,
                     position = "dodge") +
      scale_fill_manual(values = bar_fills,
                        guide = "none") +
      xlab('Number of Months') +
      ylab('') +
      theme_minimal()
  })
  
  output$regModel <- renderPlot({
    
    ggplot(telecomCustomerChurn, aes(x = tenure, y = TotalCharges, color = Churn)) +
      geom_point() +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black") +
      labs(title = "Regression Model of Tenure and Total Charges",
           x = "Tenure",
           y = "Total Charges") +
      scale_color_manual(name = "Churn", labels = c("No", "Yes"), values = c("orange", "purple")) +
      theme_minimal()
  })
  
  output$ChurnVsRetention <- renderPlot({
    retention_churn <- data.frame(
      category = c("Retention Rate", "Churn Rate"),
      rate = c(retained.customers[1] / total.customers[1] * 100, 
               churned.customers[1] / total.customers[1] * 100)
    )
    
    # Create a ggplot object with a bar chart
    ggplot(retention_churn, aes(x = category, y = rate, fill = category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "", y = "Percentage", title = "Retention vs Churn Rate") +
      scale_fill_manual(values = c("orange", "purple")) # Set colors for the bars
    
  })
  
  output$tenureVSchurned <- renderPlot({
    ggplot(telecomCustomerChurn, aes(x = Churn, y = tenure, fill = Churn)) +
      geom_bar(stat = "summary",
               fun = "mean") +
      labs(title = "Average Tenure for Churned and Retained Customers",
           x = "Churn Status",
           y = "Average Tenure (Months)",
           fill = "Churn Status") +
      scale_fill_manual(values = c("orange", "purple")) +
      theme_minimal()
  })
  
  output$tenureVScontract <- renderPlot({
    ggplot(telecomCustomerChurn, aes(x = tenure, fill = Contract)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = c("#4b4b4b", "#e37222", "#79bcb8")) +
      labs(x = "Tenure (months)", y = "Density", title = "Tenure vs. Contract") +
      theme_minimal()
  })
  
  output$tenureVSc <- renderPlot({
    ggplot(telecomCustomerChurn, aes(x = SeniorCitizen, fill = factor(SeniorCitizen))) +
      geom_bar(aes(y = ..prop..), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(name = "Senior Citizen", labels = c("No", "Yes"), values = c("orange", "purple")) +
      xlab("Senior Citizen") +
      ylab("Percentage") +
      ggtitle("Senior Citizen Distribution by Tenure") +
      theme_minimal()
  })
  
  output$tenureVSGender <- renderPlot({
    ggplot(telecomCustomerChurn, aes(x = gender, y = tenure, fill = gender)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c("orange", "purple")) +
      labs(title = "Tenure by Gender", x = "Gender", y = "Tenure") +
      theme_minimal()
  })
}

shinyApp(ui, server)