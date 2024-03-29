# COVID-19 new cases within Canada
# 
# Based on the paper 
# Quarantine and the risk of COVID-19 importation
# Julien Arino, Nicolas Bajeux, Stephanie Portet and James Watmough
# Epidemiology and Infection 148:e298 (2020)
# https://doi.org/10.1017/S0950268820002988
# 
# See the paper for details
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Percentage of jurisdictions with new cases"),
    
    # Sidebar with a slider input for the number of days since last confirmed case
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            sliderInput("nb_days_delay",
                        "Number of days to look back:",
                        min = 0,
                        max = 100,
                        value = 21),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("a_distPlot", width = "800px", height = "600px"),
                                 textOutput(outputId = "desc_plot_CAN"),
                                 tags$a(href = "https://doi.org/10.1017/S0950268820002988", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        )
                        #tabPanel("Table", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    RESULTS <- reactive({
        # Prepare results
        OUT = list()
        OUT$nb_days_delay = as.numeric(input$nb_days_delay)
        return(OUT)
    })

    output$a_distPlot <- renderPlot({
        OUT = RESULTS()
        # Determine day of most recent data set for first events
        data_files_events = list.files(path = "DATA",
                                       pattern = glob2rx("CAN_incidence_first_events_*.Rds"))
        latest_data_file = sort(data_files_events, decreasing = TRUE)[1]
        # Read data set
        DATA = readRDS(sprintf("DATA/%s", latest_data_file))
        # Remove "Not reported" and "Repatriated" cases, which should not play a role in importations
        idx_to_remove = grep("Not Reported", DATA$incidence_province_hr$health_region)
        DATA$incidence_province_hr = DATA$incidence_province_hr[setdiff(1:dim(DATA$incidence_province_hr)[1],
                                                                        idx_to_remove),]
        idx_to_remove = grep("Repatriated", DATA$incidence_PT)
        DATA$incidence_PT = DATA$incidence_PT[setdiff(1:dim(DATA$incidence_PT)[1],
                                                      idx_to_remove),]
        # Keep only the numbers, not the names
        daily_activity_HR = DATA$incidence_province_hr[3:dim(DATA$incidence_province_hr)[2]]
        daily_activity_PT = DATA$incidence_PT[2:dim(DATA$incidence_PT)[2]]
        dates_data = as_date(colnames(daily_activity_HR))
        # Determine, at a given point in time, which jurisdictions had a case in the past N days. That's the "delay"
        delay = OUT$nb_days_delay
        t_start = dates_data[1]+delay
        t_end = dates_data[length(dates_data)]
        dates_checked = seq(from = t_start, to = t_end, by = "days")
        
        nb_HR_with_cases_Ndays = c()
        nb_PT_with_cases_Ndays = c()
        
        for (dd in dates_checked) {
            dd = as_date(dd)
            past_Ndays = seq(from = (dd-delay), to = dd, by = "days")
            tmp_HR = rowSums(daily_activity_HR[,as.character(past_Ndays)])
            nb_HR_with_cases_Ndays = c(nb_HR_with_cases_Ndays,
                                        length(tmp_HR[tmp_HR>0]))
            tmp_PT = rowSums(daily_activity_PT[,as.character(past_Ndays)])
            nb_PT_with_cases_Ndays = c(nb_PT_with_cases_Ndays,
                                        length(tmp_PT[tmp_PT>0]))
        }
        
        nb_PT_with_cases_Ndays = nb_PT_with_cases_Ndays/13*100 # 13 P/T
        nb_HR_with_cases_Ndays = nb_HR_with_cases_Ndays/112*100 # 112 HR
        y_max = max(max(nb_PT_with_cases_Ndays), max(nb_HR_with_cases_Ndays))
        
        
        plot(dates_checked, nb_PT_with_cases_Ndays,
             type = "b",
             lwd = 2,
             col = "red",
             pch = 21, 
             ylim = c(0, 100),
             xlab = "Date", 
             ylab = sprintf("Percent with new cases in past %d days", delay))
        lines(dates_checked, nb_HR_with_cases_Ndays,
              type = "b",
              lwd = 2,
              pch = 17,
              col = "dodgerblue4")
        for (h in seq(from = 0, to = 100, by = 10)) {
            abline(h = h, lty = 3, lwd = 0.5)
        }
        legend("bottomright", 
               legend = c("Provinces and Territories", "Health regions"),
               col = c("red", "dodgerblue4"), bg = "white",
               pch = c(21, 17),
               inset = 0.01)
    })
    
    # Explanation of alluvial plot
    output$desc_plot_CAN <- renderText({
        exp_text = "This plot shows the percentage of Provinces and Territories (red) and of Canadian Health Regions (blue) having reported new cases"
        exp_text = paste(exp_text, "in the number of days chosen by using the slider on the left. For instance, if the default N=21 days is selected")
        exp_text = paste(exp_text, "then a jurisdiction J is active on day D if it has declared new cases in the period [D-N,D].")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
