# Load packages -----------------------------------------------------
library(shiny)
library(shinythemes)
library(episensr)

# Define UI ---------------------------------------------------------
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("episensr: Basic Sensitivity Analysis of Epidemiological Results"),
                sidebarLayout(
                    sidebarPanel(

                        ## Input A cell
                        numericInput(inputId = "a_cell",
                                     label = strong("Exposed + and Outcome +:"),
                                     value = 136),

                        ## Input B cell
                        numericInput(inputId = "b_cell",
                                     label = strong("Exposed - and Outcome +:"),
                                     value = 107),

                        ## Input C cell
                        numericInput(inputId = "c_cell",
                                     label = strong("Exposed + and Outcome -:"),
                                     value = 297),

                        ## Input D cell
                        numericInput(inputId = "d_cell",
                                     label = strong("Exposed - and Outcome -:"),
                                     value = 165),

                        ## br() element to introduce extra vertical spacing ----
                        br(),

                        ## Selection probability among cases exposed
                        sliderInput("bias_parms1",
                                    "Selection probability among cases exposed:",
                                    value = 0.94,
                                    min = 0,
                                    max = 1),

                        ## Selection probability among cases unexposed
                        sliderInput("bias_parms2",
                                    "Selection probability among cases unexposed:",
                                    value = 0.85,
                                    min = 0,
                                    max = 1),
                        ## Selection probability among noncases exposed
                        sliderInput("bias_parms3",
                                    "Selection probability among noncases exposed:",
                                    value = 0.64,
                                    min = 0,
                                    max = 1),
                        ## Selection probability among noncases unexposed
                        sliderInput("bias_parms4",
                                    "Selection probability among noncases unexposed:",
                                    value = 0.25,
                                    min = 0,
                                    max = 1)
                    ),

                    ## Output
                    mainPanel(
                        fluidRow(
                            column(width = 4,
                                   ## Observed data
                                   h3("Observed data"),
                                   tableOutput(outputId = "obs_data"),
                                   h3("Corrected data"),
                                   tableOutput(outputId = "corr_data")
                                   ),
                            column(width = 8,
                                   h3("Observed measures of relationship (and 95% CI)"),
                                   tableOutput(outputId = "obs_measures"),
                                   h3("Corrected measures of association"),
                                   tableOutput(outputId = "adj_measures"),
                                   h4("Selection bias odds ratio based on the bias parameters chosen"),
                                   tableOutput(outputId = "selbias_or")
                                   )
                        )

                    )
                )
                )

# Define server function --------------------------------------------
server <- function(input, output) {

    ## Data
    data = reactive({
        a = input$a_cell; b = input$b_cell; c = input$c_cell; d = input$d_cell
        return(matrix(c(a, b, c, d),
                      dimnames = list(c("Outcome +", "Outcome -"),
                                      c("Exposed +", "Exposed -")),
                      nrow = 2, byrow = TRUE))
    })
    
    ## Create an episensr reactive function
    episensrout = reactive({
        mat <- data()
        mod <- selection(mat,
                         bias_parms = c(input$bias_parms1, input$bias_parms2,
                                        input$bias_parms3, input$bias_parms4))
    })

    ## Output of observed data
    output$obs_data = renderTable({
        vals <- episensrout()
        vals$obs.data
    }, rownames = TRUE)

    ## Output of corrected data
    output$corr_data = renderTable({
        vals <- episensrout()
        vals$corr.data
    }, rownames = TRUE)

    ## Output of observed measures
    output$obs_measures = renderTable({
        vals <- episensrout()
        vals$obs.measures
    }, rownames = TRUE)

    ## Output of corrected measures
    output$adj_measures = renderTable({
        vals <- episensrout()
        vals$adj.measures
    }, rownames = TRUE)

    ## Output of selection OR
    output$selbias_or = renderTable({
        vals <- episensrout()
        vals$selbias.or
    }, colnames = FALSE)
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
