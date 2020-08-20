# COVID-19 new cases within Canada
# 
# Based on the paper "Assessing the risk of COVID-19 importation and the effect of quarantine"
# by Julien Arino, Nicolas Bajeux, Stephanie Portet and James Watmough
# https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1
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

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Single stimulation simulations"),
    
    # Sidebar with a slider input for the number of days since last confirmed case
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            sliderInput("nb_days_delay",
                        "Number of days to look back:",
                        min = 0,
                        max = 100,
                        value = 21,
                        step = 7),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("a_distPlot", width = "800px", height = "600px"),
                                 textOutput(outputId = "desc_sims"),
                                 tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        ),
                        tabPanel("Summary", verbatimTextOutput("summary"),
                                 tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1", 
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

    set_directories = function() {
        ### SET_DIRECTORIES
        # Need a special ad hoc version here because we are running in Shiny server
        # Create list to store the information
        DIRS = list()
        # Set code directory
        DIRS$CODE = "/home/ubuntu/github/covid-19-importation-risk"
        # Directories outside Github repo: DATA, others perhaps
        DIRS$OUTSIDE = "/home/ubuntu"
        # A few directories to store stuff
        DIRS$OUTPUT = sprintf("%s/OUTPUT", DIRS$OUTSIDE)
        DIRS$FIGS = sprintf("%s/FIGS", DIRS$OUTSIDE)
        DIRS$COVID_DATA = sprintf("%s/DATA", DIRS$OUTSIDE)
        return(DIRS)
    }
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    RESULTS <- reactive({
        # Prepare results
        OUT = list()
        OUT$nb_days_delay = input$nb_days_delay
        OUT$DIRS = set_directories()
        return(OUT)
    })

    output$a_distPlot <- renderPlot({
        OUT = RESULTS()
        # Determine day of most recent data set for first events
        data_files_events = list.files(path = OUT$DIRS$COVID_DATA,
                                       pattern = glob2rx("CAN_incidence_first_events_*.Rds"))
        latest_data_file = sort(data_files_events, decreasing = TRUE)[1]
        # Read data set
        DATA = readRDS(sprintf("%s/%s",
                               OUT$DIRS$COVID_DATA, latest_data_file))
        # Remove "Not reported"
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
        nb_HR_with_cases = c()
        nb_PT_with_cases = c()
        for (i in 1:length(dates_data)) {
            nb_HR_with_cases = c(nb_HR_with_cases, length(daily_activity_HR[daily_activity_HR[,i]>0,i]))
            nb_PT_with_cases = c(nb_PT_with_cases, length(daily_activity_PT[daily_activity_PT[,i]>0,i]))
        }
        
        # Determine, at a given point in time, which jurisdictions had a case in the past two weeks
        delay = OUT$nb_days_delay
        t_start = dates_data[1]+delay
        t_end = dates_data[length(dates_data)]
        dates_checked = seq(from = t_start, to = t_end, by = "days")
        
        nb_HR_with_cases_2weeks = c()
        nb_PT_with_cases_2weeks = c()
        
        for (dd in dates_checked) {
            dd = as_date(dd)
            past_2_weeks = seq(from = (dd-delay), to = dd, by = "days")
            tmp_HR = rowSums(daily_activity_HR[,as.character(past_2_weeks)])
            nb_HR_with_cases_2weeks = c(nb_HR_with_cases_2weeks,
                                        length(tmp_HR[tmp_HR>0]))
            tmp_PT = rowSums(daily_activity_PT[,as.character(past_2_weeks)])
            nb_PT_with_cases_2weeks = c(nb_PT_with_cases_2weeks,
                                        length(tmp_PT[tmp_PT>0]))
        }
        
        nb_PT_with_cases_2weeks = nb_PT_with_cases_2weeks/13*100
        nb_HR_with_cases_2weeks = nb_HR_with_cases_2weeks/112*100
        y_max = max(max(nb_PT_with_cases_2weeks), max(nb_HR_with_cases_2weeks))
        
        
        plot(dates_checked, nb_PT_with_cases_2weeks,
             type = "b",
             lwd = 2,
             col = "red",
             pch = 21, 
             ylim = c(0, 100),
             xlab = "Date", ylab = "Percent with new cases in past 3 weeks")
        lines(dates_checked, nb_HR_with_cases_2weeks,
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
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
        OUT = RESULTS_NEW()
        nb_extinctions = 0
        time_extinction = c()
        for (sim in 1:OUT$params[["nb_sims"]]) {
            if (OUT$RESULTS[sim,length(OUT$RESULTS[sim,])] == 0) {
                nb_extinctions = nb_extinctions+1
                tmp = OUT$RESULTS[sim,]
                idx_zero = which(tmp==0)[1]
                time_extinction = c(time_extinction,
                                    OUT$params[["times"]][idx_zero])
            }
        }
        cat(paste0("Out of ",OUT$params[["nb_sims"]],
                   " simulations, ",
                   round(nb_extinctions/OUT$params[["nb_sims"]]*100,2)," percent had extinctions. "))
        # Compute quartiles
        quantile_time_extinction = quantile(time_extinction)
        cat(paste0("The distribution of times to extinction is as follows, in days (0% is the minimun, ",
                   "25% is the \nfirst quartile, 50% is the median, 75% the third quartile and 100% the maximum):"))
        print(knitr::kable(quantile_time_extinction))
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        OUT = RESULTS_NEW()
        nb_extinctions = 0
        for (sim in 1:OUT$params[["nb_sims"]]) {
            if (OUT$RESULTS[sim,length(OUT$RESULTS[sim,])] == 0) {
                nb_extinctions = nb_extinctions+1
            }
        }
        print(round(nb_extinctions/OUT$params[["nb_sims"]]*100,2))
    })
    
    # Explanation of alluvial plot
    output$desc_sims <- renderText({
        exp_text = "This plot shows prevalence (of the selected variables) through time of 100 simulations of an SLIAR model."
        exp_text = paste(exp_text, "Initial importation is of one case of the selected type.")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
