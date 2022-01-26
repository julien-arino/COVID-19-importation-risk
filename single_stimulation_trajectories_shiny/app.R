# COVID-19 single introduction response simulation
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
library(deSolve)
library(parallel)
library(adaptivetau)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Some play with fonts to make stuff a little smaller. Experimental..
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
          font-size: 85%;
        }"
    ))),

    # Application title
    titlePanel("Single stimulation simulations"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("show", "Show:", 
                        choices = c("I1+I2 (symptomatic cases)" = "I1+I2", 
                                    "A1+A2 (asymptomatic cases)" = "A1+A2",
                                    "L1+L2 (incubating cases)" = "L1+L2",
                                    "I1+I2+A1+A2 (infectious cases)" = "I1+I2+A1+A2",
                                    "L1+L2+I1+I2+A1+A2 (all cases)" = "L1+L2+I1+I2+A1+A2")),
            selectInput("import_type", "Type of import case:", 
                        choices = c("L1", "L2", "A1", "A2", "I1", "I2"),
                        selected = "L2"),
            numericInput("pop_size", "Population size:", value = 35874, min = 100),
            withMathJax(),
            sliderInput("R_0",
                        "Basic reproduction number  \\(R_0\\):",
                        min = 0.5,
                        max = 4,
                        value = 2.2,
                        step = 0.1),
            sliderInput("inv_epsilon",
                        "Average incubation period \\(2/\\varepsilon\\) (days):",
                        min = 2,
                        max = 9,
                        value = 6),
            sliderInput("eta",
                        "Infectiousness multiplicator \\(\\eta\\) in L2:",
                        min = 0,
                        max = 0.1,
                        value = 0.05),
            sliderInput("inv_gamma",
                        "Average infectious period \\(2/\\gamma\\) (days):",
                        min = 2,
                        max = 9,
                        value = 6),
            sliderInput("pi",
                        "Proportion  \\(\\pi\\) of asymptomatic cases:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("xi",
                        "Infectiousness multiplicator \\(\\xi\\) in A:",
                        min = 0,
                        max = 1,
                        value = 0.4),
            # Not used since the plots and information shown will not reflect changes in this
            # value. To use, uncomment the lines below and params$delta = input$delta below
            # to use slider for delta.
            # sliderInput("delta",
            #             "Proportion of symptomatic cases leading to death:",
            #             min = 0,
            #             max = 1,
            #             value = 0.03),
            sliderInput("tf",
                        "Final time:",
                        min = 1,
                        max = 60,
                        value = 30,
                        step = 1),
            # Not allowed to change for run on tiny AWS server. Uncomment this and the line
            # somewhere below that says params$nb_sims = input$nb_sims.
            # sliderInput("nb_sims",
            #             "Number of simulations:",
            #             min = 1,
            #             max = 100,
            #             value = 50,
            #             step = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("a_distPlot", width = "800px", height = "600px"),
                                 textOutput(outputId = "desc_sims"),
                                 tags$a(href = "https://doi.org/10.1017/S0950268820002988", 
                                        "See the paper for details.", target = "_blank"),
                                 tags$a(href = "https://github.com/julien-arino/covid-19-importation-risk", 
                                        "Download the code from Github.", target = "_blank")
                        ),
                        tabPanel("Summary", verbatimTextOutput("summary"),
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

    SLLIIAARRD_ODE_rhs <- function (t, x, params) {
        with(as.list(x), {
            Phi = params$beta*S*(params$eta*L2+I1+I2+params$xi*(A1+A2))
            dS <- -Phi
            dL1 <- Phi-params$epsilon*L1
            dL2 <- params$epsilon*(L1-L2)
            dI1 <- (1-params$pi)*params$epsilon*L2-params$gamma*I1
            dI2 <- params$gamma*(I1-I2)
            dA1 <- params$pi*params$epsilon*L2-params$gamma*A1
            dA2 <- params$gamma*(A1-A2)
            dRI <- (1-params$delta)*params$gamma*I2
            dRA <- params$gamma*A2
            dD <- params$delta*params$gamma*I2
            list(c(dS, dL1, dL2, dI1, dI2, dA1, dA2, dRI, dRA, dD))
        })
    }
    
    # Encode potential transitions:
    transitions_SLLIIAARRD = list(
        # New infection (-1S,+1L1)
        c(S = -1, L1 = +1),
        # Progression through incubation (-1L1,+1L2)
        c(L1 = -1, L2 = +1),
        # End of incubation, move to I1 (-1L2,+1I1)
        c(L2 = -1, I1 = +1),
        # End of incubation, move to A1 (-1L2,+1A1)
        c(L2 = -1, A1 = +1),
        # Progression through I (-1I1,+1I2)
        c(I1 = -1, I2 = +1),
        # Progression through A (-1A1,+1A2)
        c(A1 = -1, A2 = +1),
        # End I2, recover (-1I2,+1RI)
        c(I2 = -1, RI = +1),
        # End I2, die (-1I2,+1D)
        c(I2 = -1, D = +1),
        # End A2, recover (-1A2,+1RA)
        c(A2 = -1, RA = +1)) 

    # Function to calculate transition rates, given variables and parameters
    lvrates_SLLIIAARRD <- function(x, params, t) {
        return(c(
            # New infection (-1S,+1L1)
            params$beta*x["S"]*(params$eta*x["L2"]+x["I1"]+x["I2"]+params$xi*(x["A1"]+x["A2"])),
            # Progression through incubation (-1L1,+1L2)
            params$epsilon*x["L1"],
            # End of incubation, move to I1 (-1L2,+1I1)
            (1-params$pi)*params$epsilon*x["L2"],
            # End of incubation, move to A1 (-1L2,+1A1)
            params$pi*params$epsilon*x["L2"],
            # Progression through I (-1I1,+1I2)
            params$gamma*x["I1"],
            # Progression through A (-1A1,+1A2)
            params$gamma*x["A1"],
            # End I2, recover (-1I2,+1RI)
            (1-params$delta)*params$gamma*x["I2"],
            # End I2, die (-1I2,+1D)
            params$delta*params$gamma*x["I2"],
            # End A2, recover (-1A2,+1RA)
            params$gamma*x["A2"]
        ))
    }
    
    one_sim_ssa_tau_leap <- function(sim, IC, params) {
        # Perform the stochastic simulation!
        r = ssa.adaptivetau(init.values = IC, 
                            transitions = transitions_SLLIIAARRD, 
                            rateFunc = lvrates_SLLIIAARRD, 
                            params = params, 
                            tf = params$tf,
                            tl.params = list(epsilon=0.01))
        interp = list()
        if (params$show == "I1+I2") {
            interp[["I1"]] <- approx(r[,"time"], r[,"I1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["I2"]] <- approx(r[,"time"], r[,"I2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            results <- data.frame(interp[["I1"]]$y+interp[["I2"]]$y)
        } else if (params$show == "A1+A2") {
            interp[["A1"]] <- approx(r[,"time"], r[,"A1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["A2"]] <- approx(r[,"time"], r[,"A2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            results <- data.frame(interp[["A1"]]$y+interp[["A2"]]$y)
        } else if (params$show == "L1+L2") {
        interp[["L1"]] <- approx(r[,"time"], r[,"L1"], 
                                 params$times, ties = "ordered", 
                                 rule = 2)
        interp[["L2"]] <- approx(r[,"time"], r[,"L2"], 
                                 params$times, ties = "ordered", 
                                 rule = 2)
        results <- data.frame(interp[["L1"]]$y+interp[["L2"]]$y)
        } else if (params$show == "I1+I2+A1+A2") {
            interp[["I1"]] <- approx(r[,"time"], r[,"I1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["I2"]] <- approx(r[,"time"], r[,"I2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["A1"]] <- approx(r[,"time"], r[,"A1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["A2"]] <- approx(r[,"time"], r[,"A2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            results <- data.frame(interp[["I1"]]$y + interp[["I2"]]$y +
                                      interp[["A1"]]$y + interp[["A2"]]$y)
        }  else if (params$show == "L1+L2+I1+I2+A1+A2") {
            interp[["L1"]] <- approx(r[,"time"], r[,"L1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["L2"]] <- approx(r[,"time"], r[,"L2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["I1"]] <- approx(r[,"time"], r[,"I1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["I2"]] <- approx(r[,"time"], r[,"I2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["A1"]] <- approx(r[,"time"], r[,"A1"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            interp[["A2"]] <- approx(r[,"time"], r[,"A2"], 
                                     params$times, ties = "ordered", 
                                     rule = 2)
            results <- data.frame(interp[["L1"]]$y + interp[["L2"]]$y +
                                      interp[["I1"]]$y + interp[["I2"]]$y +
                                      interp[["A1"]]$y + interp[["A2"]]$y)
        }
    colnames(results) <- c("I1")
        return(results)
    }

    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    RESULTS_NEW <- reactive({
        # Prepare results
        OUT = list()
        # Get parameters from console
        params <- list()
        # Type of output
        params$show = input$show
        # Tailor to location
        params$S0 = input$pop_size
        # Set disease parameters
        params$xi = input$xi
        params$eta = input$eta
        params$pi = input$pi
        params$delta = 0.03
        # Uncomment this and slider above to use slider on delta
        #params$delta = input$delta
        params$R_0 = input$R_0
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
        # Beta is computed using classic R_0 formula
        params$beta = as.numeric(
            (params$gamma*params$R_0*params$epsilon) /
                ((params$eta*params$gamma +
                      2*params$epsilon*(params$pi*params$xi-params$pi+1))*
                     params$S0))
        # Initial condtion
        IC = c(S = params$S0,
               L1 = 0,
               L2 = 0,
               I1 = 0,
               I2 = 0,
               A1 = 0,
               A2 = 0,
               RI = 0,
               RA = 0,
               D = 0)
        # Set import case to be of the type desired
        IC[input$import_type] = 1 
        
        params$t0 = 0
        params$tf = input$tf
        params$times <- seq(from = params$t0, 
                            to = params$tf, 
                            by = 0.1)
        params$length_times <- length(params$times)
        params$nb_sims = 100
        # Uncomment this and the slider code at top to allow this to change.
        #params$nb_sims = input$nb_sims
        
        OUT$params = params 

        RESULTS <- mat.or.vec(params$nb_sims,
                              params$length_times)
        
        inputs = 1:params[["nb_sims"]]
        
        # Change FALSE to TRUE to run in parallel. Only useful if the number of simulations
        # is large enough.
        if (FALSE) {
            numCores <- detectCores()
            cl <- makeCluster(numCores)  
            clusterExport(cl,c("params",
                               "IC",
                               "ssa.adaptivetau",
                               "lvrates_SLLIIAARRD",
                               "transitions_SLLIIAARRD"),
                          envir=environment())
            RESULTS_TMP = parLapply(cl, inputs, one_sim_ssa_tau_leap)  
            stopCluster(cl)  
        } else {
            RESULTS_TMP = lapply(X = inputs, FUN = one_sim_ssa_tau_leap, IC, params) 
        }
        
        RESULTS <- matrix(unlist(RESULTS_TMP), 
                          ncol = params[["length_times"]], 
                          byrow = TRUE)
        
        OUT$RESULTS = RESULTS
        
        # Compute ODE solution
        sol = ode(IC,
                  params[["times"]],
                  SLLIIAARRD_ODE_rhs,
                  params)
        
        OUT$sol = sol
        return(OUT)
    })

    output$a_distPlot <- renderPlot({
        OUT = RESULTS_NEW()
        
        y_max = max(max(max(OUT$RESULTS)),max(OUT$sol[,"I1"]+OUT$sol[,"I2"]))
        nb_sims_non_extinct = 0
        results_raw <- OUT$params[["times"]]*0
        results_conditioned <- OUT$params[["times"]]*0
        # Now plot the results
        for (sim in 1:OUT$params[["nb_sims"]]) {
            if (OUT$RESULTS[sim,length(OUT$RESULTS[sim,])] == 0) {
                col = "blue"
            } else {
                col = "red"
            }
            if (sim == 1) {
                plot(OUT$params[["times"]],OUT$RESULTS[sim,],
                     xlab="Time (days)",
                     ylab="Number of infectious individuals",
                     type="l",lwd=0.5,ylim = c(0,y_max),
                     col = col)
            } else {
                lines(OUT$params[["times"]],OUT$RESULTS[sim,],lwd=0.5,type="l", col = col)
            }
            # Prepare sum of results for average
            results_raw = results_raw+OUT$RESULTS[sim,]
            # Prepare sum of results conditioned on non-extinction for average
            if (OUT$RESULTS[sim,OUT$params[["length_times"]]]>0) {
                results_conditioned <- results_conditioned+OUT$RESULTS[sim,]
                nb_sims_non_extinct <- nb_sims_non_extinct+1
            }
        }
        
        # Plot non-conditioned average
        results_raw = results_raw / OUT$params[["nb_sims"]]
        lines(OUT$params[["times"]],results_raw,col="dodgerblue4",type="l",lwd=4)
        # Plot average conditioned on non-extinction
        results_conditioned = results_conditioned / nb_sims_non_extinct
        lines(OUT$params[["times"]],results_conditioned,col="red",type="l",lwd=4)
        # Plot solution of ODE
        if (OUT$params$show == "I1+I2") {
            lines(OUT$params[["times"]],
                  OUT$sol[,"I1"]+OUT$sol[,"I2"],
                  col = "green", lwd = 4)
        } else {
            lines(OUT$params[["times"]],
                  OUT$sol[,"I1"]+OUT$sol[,"I2"]+OUT$sol[,"A1"]+OUT$sol[,"A2"],
                  col = "green", lwd = 4)
        }
        # Legend
        legend("topleft", legend = c("Trajectories going extinct",
                                     "Trajectories continuing",
                                     "Average (all realizations)",
                                     "Average (non-extinction)",
                                     "ODE"), 
               col=c("dodgerblue4","red","dodgerblue4","red","green"), 
               lwd = c(1,1,2,2), lty = c(1,1,1,1))
        
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
