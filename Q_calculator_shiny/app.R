# A COVID-19 quarantine calculator
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
library(Matrix)
library(alluvial)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID-19 quarantine efficacy calculator"),
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("t_q",
                        "Duration of quarantine (days):",
                        min = 0,
                        max = 40, # Hey, it's quarantaine, i.e., forty :)
                        value = 14),
            sliderInput("inv_epsilon",
                        "Avg. incubation period (days):",
                        min = 2,
                        max = 9,
                        value = 6),
            sliderInput("inv_gamma",
                        "Avg. infectious period (days):",
                        min = 2,
                        max = 15,
                        value = 10,
                        step = 0.1),
            sliderInput("pi",
                        "Prop. asymptomatic or undetected cases:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("delta",
                        "Case fatality ratio:",
                        min = 0,
                        max = 1,
                        value = 0.03)
        ),
        # Main panel with tabs
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Quarantine efficacy", 
                                 plotOutput("a_distPlot", width = "800px", height = "600px")),
                        tabPanel("Effect of quarantine",
                                 plotOutput("a_alluvialPlot", width = "800px", height = "600px"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    SLLIIAARRD_transition_matrix <- function (params, t_q) {
        dnames = c("L_1", "L_2", "I_1", "I_2", "A_1", "A_2", "R_I", "R_A", "D")
        T = mat.or.vec(nr = 9, nc = 9)
        rownames(T) = dnames
        colnames(T) = dnames
        T[1,1] = -params$epsilon
        T[2,1] = params$epsilon
        T[2,2] = -params$epsilon
        T[3,2] = (1-params$pi)*params$epsilon
        T[3,3] = -params$gamma
        T[4,3] = params$gamma
        T[4,4] = -params$gamma
        T[5,2] = params$pi*params$epsilon
        T[5,5] = -params$gamma
        T[6,5] = params$gamma
        T[6,6] = -params$gamma
        T[7,4] = (1-params$delta)*params$gamma
        T[8,6] = params$gamma
        T[9,4] = params$delta*params$gamma
        expT = expm(T * t_q)
        return(expT)
    }
    
    one_matrix <- function(sim, IC, params) {
        OUT = list()
        OUT$u = c(1,1,0,0,1,1,0,0,0)
        # Compute the matrix exponential
        OUT$expT = SLLIIAARRD_transition_matrix(params, params$t_q)
        OUT$u_expT = OUT$u %*% OUT$expT
        return(OUT)
    }

    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    RESULTS_NEW <- reactive({
        # Get parameters from console
        params <- list()
        # Set disease parameters
        params$pi = input$pi
        params$delta = input$delta
        # epsilon and gamma need to be computed
        if (input$inv_gamma>0) {
            params$gamma = 2/input$inv_gamma
        } else {
            params$gamma = 0
        }
        if (input$inv_epsilon>0) {
            params$epsilon = 2/input$inv_epsilon
        } else {
            params$epsilon = 0
        }
        # Duration of quarantine
        params$t_q = input$t_q
        # Store results
        OUT = one_matrix(sim, IC, params)
        # Store parameters just in case they are needed elsewhere
        OUT$params = params 
        return(OUT)
    })

    output$a_distPlot <- renderPlot({
        OUT = RESULTS_NEW()
        # Suppose all unobservable infected states L_1, L_2, A_1 and A_2 are equiprobable
        p_X = c(0.25,0.25,0,0,0.25,0.25,0,0,0)
        # Compute efficacy
        eff = as.numeric(1-OUT$u_expT %*% p_X)*100
        # Want to plot efficacy curve for these parameter values
        x = 0:40
        y = c()
        for (d in x) {
            tmp = SLLIIAARRD_transition_matrix(OUT$params, d)
            y = c(y, (1-as.numeric(OUT$u %*% tmp %*% p_X))*100)
        }
        plot(x, y,
             type = "b",
             lwd = 2,
             ylim = c(0,100),
             xaxs = "i", yaxs = "i",
             xlab = "Duration of quarantine (days)",
             ylab = "Efficacy of quarantine (%)")
        lines(x = c(OUT$params$t_q, OUT$params$t_q, 0),
              y = c(0, eff, eff),
              lty = 2, lwd = 2,
              col = "dodgerblue4")
        points(OUT$params$t_q, eff, pch = 19, col = "dodgerblue4", cex = 2)
        if (eff < 90) {
            text(x = OUT$params$t_q+2,
                 y = eff,
                 cex = 1.25,
                 labels = sprintf("%2.1f %%", round(eff,1)),
                 col = "dodgerblue4")
        } else {
            text(x = OUT$params$t_q+2,
                 y = eff-2,
                 cex = 1.25,
                 labels = sprintf("%2.1f %%", round(eff,1)),
                 col = "dodgerblue4")
        }
    })
    
    output$a_alluvialPlot <- renderPlot({
        OUT = RESULTS_NEW()
        data_raw = OUT$expT
        # Keep only unobservable states
        data_raw = data_raw[,c("L_1","L_2","A_1","A_2")]
        # Create edge list
        edges = data.frame(mat.or.vec(nr = length(data_raw), nc = 3),
                           stringsAsFactors = FALSE)
        idx = 1
        for (c in colnames(data_raw)) {
            for (r in rownames(data_raw)) {
                edges[idx,1] = c
                edges[idx,2] = r
                edges[idx,3] = data_raw[r,c]
                idx = idx+1
            }
        }
        # Change column names
        colnames(edges) = c("source", "target", "value")
        edges = edges[which(edges$value>0),]
        rownames(edges) = 1:dim(edges)[1]
        # Get index of ordered variables for the right column
        order_right = c()
        for (c in c("L_1","L_2","A_1","A_2","I_1","I_2","R_I","R_A","D")) {
            order_right = c(order_right,
                            which(edges$target == c))
        }
        # Plot
        alluvial(edges[,1:2],
                 freq = edges$value,
                 col = ifelse(edges$target %in% c("L_1","L_2","A_1","A_2"),
                              "dodgerblue4", "gray80"),
                 layer = rev(order_right),
                 axis_labels = c("At importation", "After quarantine"),
                 gap.width = 0.15,
                 blocks = FALSE,
                 ordering = list(1:dim(edges)[1],
                                 (order_right)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
