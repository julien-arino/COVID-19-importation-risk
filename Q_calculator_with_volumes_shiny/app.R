# A COVID-19 quarantine calculator
# 
# Based on the paper 
# Assessing the risk of COVID-19 importation and the effect of quarantine
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
library(Matrix)
library(alluvial)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID-19 quarantine efficacy calculator"),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    # Sidebar with sliders for the parameter values
    sidebarLayout(
        sidebarPanel(
            sliderInput("I_origin",
                        "Prevalence at origin (per 100K):",
                        min = 0,
                        max = 10000,
                        value = 50),
            sliderInput("pi_origin",
                        "Prop. undetected cases at origin:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            textInput(inputId = "inflow", 
                      label = "Inflow from origin (# people):", 
                      value = "1000"),
            hr(),
            sliderInput("t_q",
                        "Duration of quarantine (days):",
                        min = 0,
                        max = 40, # Hey, it's quarantaine, i.e., forty :)
                        value = 14),
            sliderInput("inv_epsilon",
                        "Avg. incubation period (days):",
                        min = 1,
                        max = 10,
                        value = 5,
                        step = 0.1),
            sliderInput("inv_gamma",
                        "Avg. infectious period (days):",
                        min = 1,
                        max = 20,
                        value = 12,
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
                        tabPanel("Rate of inflow and quarantine", 
                                 htmlOutput(outputId = "a_inflow_and_Q"),
                                 tags$a(href = "https://doi.org/10.1017/S0950268820002988", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        ),
                        tabPanel("Quarantine efficacy", 
                                 plotOutput(outputId = "a_distPlot", width = "800px", height = "600px"),
                                 textOutput(outputId = "desc_efficacy"),
                                 tags$a(href = "https://doi.org/10.1017/S0950268820002988", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        ),
                        tabPanel("Effect of quarantine",
                                 plotOutput("a_alluvialPlot", width = "800px", height = "600px"),
                                 textOutput(outputId = "desc_alluvial"),
                                 tags$a(href = "https://doi.org/10.1017/S0950268820002988", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        )
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

    create_text_lambda <- function(params) {
        OUT = RESULTS_NEW()
        incidence_origin = OUT$params$I_origin/100000
        lambda_computed = OUT$params$inflow * incidence_origin *
            OUT$params$pi_origin
        # First, compute lambda and 1/lambda without quarantine
        str = "With the parameters used for travel from the origin and "
        str = paste0(str, "conditions there, we find a rate of importation ")
        str = paste0(str, "lambda=", round(lambda_computed,3),
                     ", or, in other words, a mean time ")
        str = paste0(str, "between importations of ", 
                     round(1/lambda_computed,2), " days.<br><br>")
        # Now compute quarantine efficacy
        # Suppose all unobservable infected states L_1, L_2, A_1 and A_2 are equiprobable
        p_X = c(0.25,0.25,0,0,0.25,0.25,0,0,0)
        # Compute efficacy
        eff = as.numeric(1-OUT$u_expT %*% p_X)*100
        lambda_q = lambda_computed*(1-eff/100)
        str = paste0(str, "Parameters for the disease and quarantine in the ")
        str = paste0(str, "destination location imply a quarantine that is ")
        str = paste0(str, round(eff,2), "% efficacious.<br><br>")
        str = paste0(str, "As a consequence of quarantine, the quarantine ")
        str = paste0(str, "adjusted value of lambda, lambda_q=")
        str = paste0(str, round(lambda_q,3), 
                     " or, in other words, the mean time ")
        str = paste0(str, "between importations becomes ")
        str = paste0(str, round(1/lambda_q,2), " days.")
        # Return output
        return(str)    
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
        # Prevalence at origin
        params$I_origin = input$I_origin
        # Proportion undetected at origin
        params$pi_origin = input$pi_origin
        # Inflow from origin
        params$inflow = as.numeric(input$inflow)
        # Store results
        OUT = one_matrix(sim, IC, params)
        # Store parameters just in case they are needed elsewhere
        OUT$params = params 
        return(OUT)
    })
    
    output$a_inflow_and_Q <- renderText({
        str = create_text_lambda(params)
        print(str)
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
    
    # Explanation of efficacy
    output$desc_efficacy <- renderText({
        exp_text = "Quarantine efficacy is the probability (expressed in percentage chance) that quarantine is successful."
        exp_text = paste(exp_text, "Quarantine is successful if an individual who is undetectable when they enter the location,")
        exp_text = paste(exp_text, "in the sense that they are either incubating with the disease (L) or asymptomatically infected (A),")
        exp_text = paste(exp_text, "becomes detectable (any other state) during the course of their quarantine.")
        exp_text = paste(exp_text, "")
        exp_text = paste(exp_text, "")
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

        # Explanation of alluvial plot
    output$desc_alluvial <- renderText({
        exp_text = "This alluvial plot shows the state of individuals at the start and end of their quarantine. We focus on individuals"
        exp_text = paste(exp_text, "who are undetectable at the start of quarantine,")
        exp_text = paste(exp_text, "in the sense that they are either incubating (L) or asymptomatically infected (A) with the disease.")
        exp_text = paste(exp_text, "Blue flows show individuals who are still undetectable at the end of quarantine and thus are a danger")
        exp_text = paste(exp_text, "to the population they join after quarantine.")
        exp_text = paste(exp_text, "")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
