# Load packages -----------------------------------------------------
library(shiny)
library(shinythemes)
library(rmarkdown)
library(episensr)

# Define UI ---------------------------------------------------------
ui <- navbarPage(theme = shinytheme("united"),
                 "episensr: Basic Sensitivity Analysis of Epidemiological Results",
                 tabPanel("About",
                          column(1),
                          column(5, br(), br(), br(),
                                 p("Quantitative bias analysis allows to estimate nonrandom errors in epidemiologic studies, assessing the magnitude and direction of biases, and quantifying their uncertainties. Every study has some random error due to its limited sample size, and is susceptible to systematic errors as well, from selection bias to the presence of (un)known confounders or information bias (measurement error, including misclassification). Bias analysis methods were compiled by Lash et al. in their book", a("Applying Quantitative Bias Analysis to Epidemiologic Data.", href="https://www.springer.com/us/book/9780387879604"), "This Shiny app implements two bias analyses, selection and misclassification biases. More can be found in the", code("episensr"), "package available for download on", a("R CRAN", href="https://CRAN.R-project.org/package=episensr"), "."), br(), br(), br(), includeMarkdown("functions.md")
                                 ),
column(5, br(), br(), br(),
       wellPanel("Please report bugs at", a("https://github.com/dhaine/episensr_shiny/issues", href="https://github.com/dhaine/episensr_shiny/issues"), br(), br(), "Shiny app by", a("Denis Haine", href="https://www.denishaine.ca"), br(), br(), "episensr version:", verbatimTextOutput("versioning", placeholder = TRUE))),
column(1)
                 ),
                 tabPanel("Selection bias",
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
                                         h3("Observed measures of association"),
                                         tableOutput(outputId = "obs_measures"),
                                         h3("Corrected measures of association"),
                                         br(),
                                         tableOutput(outputId = "adj_measures"),
                                         h4("Selection bias odds ratio based on the bias parameters chosen"),
                                         tableOutput(outputId = "selbias_or")
                                         )
                              )
                          )
                          ),
                 tabPanel("Misclassification bias",
                          sidebarPanel(
                              ## Input A cell
                              numericInput(inputId = "a_cell2",
                                           label = strong("Exposed + and Outcome +:"),
                                           value = 215),
                              
                              ## Input B cell
                              numericInput(inputId = "b_cell2",
                                           label = strong("Exposed - and Outcome +:"),
                                           value = 1449),

                              ## Input C cell
                              numericInput(inputId = "c_cell2",
                                           label = strong("Exposed + and Outcome -:"),
                                           value = 668),

                              ## Input D cell
                              numericInput(inputId = "d_cell2",
                                           label = strong("Exposed - and Outcome -:"),
                                           value = 4296),

                              ## br() element to introduce extra vertical spacing ----
                              br(),
                              
                              ## Choice of misclassification
                              radioButtons(inputId = "misclass",
                                           label = "Misclassification of:",
                                           choices = c("exposure", "outcome")),

                              ## br() element to introduce extra vertical spacing ----
                              br(),
                              
                              ## Sensitivity of exposure/outcome classification among those with the outcome/exposure
                              sliderInput("bias_parms12",
                                          "Sensitivity of exposure (or outcome) classification among those with the outcome (or exposure):",
                                          value = 0.78,
                                          min = 0,
                                          max = 1),

                              ## Sensitivity of exposure/outcome classification among those without the outcome/exposure
                              sliderInput("bias_parms22",
                                          "Sensitivity of exposure (or outcome) classification among those without the outcome (or exposure):",
                                          value = 0.78,
                                          min = 0,
                                          max = 1),
                              ## Specificity of exposure/outcome classification among those with the outcome/exposure
                              sliderInput("bias_parms32",
                                          "Specificity of exposure (or outcome) classification among those with the outcome (or exposure):",
                                          value = 0.99,
                                          min = 0,
                                          max = 1),
                              ## Specificity of exposure/outcome classification among those without the outcome/exposure
                              sliderInput("bias_parms42",
                                          "Specificity of exposure (or outcome) classification among those without the outcome (or exposure):",
                                          value = 0.99,
                                          min = 0,
                                          max = 1)
                          ),
                          ## Output
                          mainPanel(
                              fluidRow(
                                  column(width = 4,
                                         ## Observed data
                                         h3("Observed data"),
                                         tableOutput(outputId = "obs_data2"),
                                         h3("Corrected data"),
                                         tableOutput(outputId = "corr_data2")
                                         ),
                                  column(width = 4,
                                         h3("Observed measures of association"),
                                         tableOutput(outputId = "obs_measures2"),
                                         h3("Corrected measures of association"),
                                         br(),
                                         tableOutput(outputId = "adj_measures2")
                                         )
                              )
                          )
                          )
                 )

# Define server function --------------------------------------------
server <- function(input, output) {

    ## version
    output$versioning <- renderPrint(packageVersion("episensr"))

    ## Data
    data1 = reactive({
        a = input$a_cell; b = input$b_cell; c = input$c_cell; d = input$d_cell
        return(matrix(c(a, b, c, d),
                      dimnames = list(c("Outcome +", "Outcome -"),
                                      c("Exposed +", "Exposed -")),
                      nrow = 2, byrow = TRUE))
    })
    data2 = reactive({
        a = input$a_cell2; b = input$b_cell2; c = input$c_cell2; d = input$d_cell2
        return(matrix(c(a, b, c, d),
                      dimnames = list(c("Outcome +", "Outcome -"),
                                      c("Exposed +", "Exposed -")),
                      nrow = 2, byrow = TRUE))
    })
    
    ## Create an episensr reactive function
    episensrout = reactive({
        mat <- data1()
        mod <- selection(mat,
                         bias_parms = c(input$bias_parms1, input$bias_parms2,
                                        input$bias_parms3, input$bias_parms4))
    })
    episensrout_misclass = reactive({
        mat <- data2()
        mod <- misclassification(mat,
                                 type = input$misclass,
                                 bias_parms = c(input$bias_parms12, input$bias_parms22,
                                                input$bias_parms32, input$bias_parms42))
    })

    ## Output of observed data
    output$obs_data = renderTable({
        vals <- episensrout()
        vals$obs.data
    }, rownames = TRUE)

    output$obs_data2 = renderTable({
        vals <- episensrout_misclass()
        vals$obs.data
    }, rownames = TRUE)

    ## Output of corrected data
    output$corr_data = renderTable({
        vals <- episensrout()
        vals$corr.data
    }, rownames = TRUE)

    output$corr_data2 = renderTable({
        vals <- episensrout_misclass()
        vals$corr.data
    }, rownames = TRUE)

    ## Output of observed measures
    output$obs_measures = renderTable({
        vals <- episensrout()
        vals$obs.measures
    }, rownames = TRUE)

    output$obs_measures2 = renderTable({
        vals <- episensrout_misclass()
        vals$obs.measures
    }, rownames = TRUE)
    
    ## Output of corrected measures
    output$adj_measures = renderTable({
        vals <- episensrout()
        vals$adj.measures
    }, rownames = TRUE)

    output$adj_measures2 = renderTable({
        vals <- episensrout_misclass()
        vals$adj.measures[, 1]
    }, colnames = FALSE, rownames = TRUE)
    
    ## Output of selection OR
    output$selbias_or = renderTable({
        vals <- episensrout()
        vals$selbias.or
    }, colnames = FALSE)
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
