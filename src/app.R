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
                          ),
                 tabPanel("Probabilistic sensitivity analysis",
                          sidebarPanel(
                              ## Input A cell
                              numericInput(inputId = "a_cell3",
                                           label = strong("Exposed + and Outcome +:"),
                                           value = 45),
                              
                              ## Input B cell
                              numericInput(inputId = "b_cell3",
                                           label = strong("Exposed - and Outcome +:"),
                                           value = 94),

                              ## Input C cell
                              numericInput(inputId = "c_cell3",
                                           label = strong("Exposed + and Outcome -:"),
                                           value = 257),

                              ## Input D cell
                              numericInput(inputId = "d_cell3",
                                           label = strong("Exposed - and Outcome -:"),
                                           value = 945),

                              ## br() element to introduce extra vertical spacing ----
                              br(),
                              
                              ## Choice of correction
                              radioButtons(inputId = "misclass2",
                                           label = "Misclassification of:",
                                           choices = c("exposure", "outcome")),

                              ## br() element to introduce extra vertical spacing ----
                              br(),
                              
                              ## Correlations
                              radioButtons(inputId = "corr",
                                          "Correlations between case and non-case?",
                                          choices = c("none", "yes")),
                              uiOutput("corr_se"),
                              uiOutput("corr_sp"),

                              ## Replications
                              numericInput("reps",
                                          "Number of replications to run:",
                                          value = 1000),

                              ## Distribution for seca
                              selectInput(inputId = "seca_parms",
                                          label = "Distribution, sensitivity of exposure classification among those with the outcome",
                                          choices = c("Uniform"        = "uniform",
                                                      "Triangular"     = "triangular",
                                                      "Trapezoidal"    = "trapezoidal",
                                                      "Logit-logistic" = "logit-logistic",
                                                      "Logit-normal"   = "logit-normal"),
                                          selected = "trapezoidal"),
                              uiOutput("seca_min"),
                              uiOutput("seca_lower"),
                              uiOutput("seca_upper"),
                              uiOutput("seca_max"),
                              uiOutput("seca_mode"),
                              uiOutput("seca_location"),
                              uiOutput("seca_scale"),
                              ## Distribution for seexp
                              conditionalPanel(
                                  condition = "input.corr=='yes'",
                              selectInput("seexp_parms",
                                          "Distribution, sensitivity of exposure classification among those without the outcome:",
                                          choices = c("Uniform" = "uniform",
                                                      "Triangular" = "triangular",
                                                      "Trapezoidal" = "trapezoidal",
                                                      "Logit-logistic" = "logit-logistic",
                                                      "Logit-normal" = "logit-normal"),
                                          selected = "trapezoidal"),
                              uiOutput("seexp_min"),
                              uiOutput("seexp_max"),
                              uiOutput("seexp_lower"),
                              uiOutput("seexp_upper"),
                              uiOutput("seexp_mode"),
                              uiOutput("seexp_location"),
                              uiOutput("seexp_scale")                                  
                              ),
                              ## Distribution for spca
                              selectInput("spca_parms",
                                          "Distribution, specificity of exposure classification among those with the outcome:",
                                          choices = c("Uniform" = "uniform",
                                                      "Triangular" = "triangular",
                                                      "Trapezoidal" = "trapezoidal",
                                                      "Logit-logistic" = "logit-logistic",
                                                      "Logit-normal" = "logit-normal"),
                                          selected = "trapezoidal"),
                              uiOutput("spca_min"),
                              uiOutput("spca_max"),
                              uiOutput("spca_lower"),
                              uiOutput("spca_upper"),
                              uiOutput("spca_mode"),
                              uiOutput("spca_location"),
                              uiOutput("spca_scale"),
                              ## Distribution for spexp
                              conditionalPanel(
                                  condition = "input.corr=='yes'",
                                  selectInput("spexp_parms",
                                              "Distribution, specificity of exposure classification among those without the outcome:",
                                              choices = c("Uniform" = "uniform",
                                                          "Triangular" = "triangular",
                                                          "Trapezoidal" = "trapezoidal",
                                                          "Logit-logistic" = "logit-logistic",
                                                          "Logit-normal" = "logit-normal"),
                                              selected = "trapezoidal"),
                                  uiOutput("spexp_min"),
                                  uiOutput("spexp_max"),
                                  uiOutput("spexp_lower"),
                                  uiOutput("spexp_upper"),
                                  uiOutput("spexp_mode"),
                                  uiOutput("spexp_location"),
                                  uiOutput("spexp_scale")                                 
                              )
                          ),
                          ## Output
                          mainPanel(
                              fluidRow(
                                  column(width = 4,
                                         ## Observed data
                                         h3("Observed data"),
                                         tableOutput(outputId = "obs_data3")
                                         ),
                                  column(width = 4,
                                         h3("Observed measures of association"),
                                         tableOutput(outputId = "obs_measures3"),
                                         h3("Corrected measures of association"),
                                         br(),
                                         tableOutput(outputId = "adj_measures3")
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
    data3 = reactive({
        a = input$a_cell3; b = input$b_cell3; c = input$c_cell3; d = input$d_cell3
        return(matrix(c(a, b, c, d),
                      dimnames = list(c("Outcome +", "Outcome -"),
                                      c("Exposed +", "Exposed -")),
                      nrow = 2, byrow = TRUE))
    })

  ## Correlations
  output$corr_se = renderUI(
  {
      if (input$corr == "yes")
      {
          sliderInput("corr_se",
                      "Correlations between case and non-case sensitivities:",
                      value = .8,
                      min = .1,
                      max = .99)
      } else if (input$corr == "none")
      {
          textOutput("")
          }
  })
  output$corr_sp = renderUI(
  {
      if (input$corr == "yes")
      {
          sliderInput("corr_sp",
                      "Correlations between case and non-case specificities:",
                      value = .8,
                      min = .1,
                      max = .99)
      } else if (input$corr == "none")
      {
          textOutput("")
          }
  })

  ## seca Distributions
  output$seca_min = renderUI(
  {
      if (input$seca_parms == "trapezoidal")
      {
          sliderInput("seca_min",
                      "Minimum",
                      value = .75,
                      min = 0,
                      max = 1)
      } else if (input$seca_parms == "uniform")
      {
          sliderInput("seca_min",
                      "Minimum",
                      value = .6,
                      min = 0,
                      max = 1)
      }
  })
    output$seca_lower = renderUI(
    {
      if (input$seca_parms == "triangular")
      {
          sliderInput("seca_lower",
                     "Lower limit",
                     value = .6,
                     min = 0,
                     max = 1)
      } else if (input$seca_parms == "trapezoidal")
      {
          sliderInput("seca_lower",
                      "Lower mode",
                      value = .85,
                      min = 0,
                      max = 1)
      } else if (input$seca_parms == "logit-logistic")
      {
          sliderInput("seca_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      } else if (input$seca_parms == "logit-normal")
      {
          sliderInput("seca_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      }
    })
      output$seca_upper = renderUI(
  {
      if (input$seca_parms == "triangular")
      {
          sliderInput("seca_upper",
                     "Upper limit",
                     value = .9,
                     min = 0,
                     max = 1)
      } else if (input$seca_parms == "trapezoidal")
      {
          sliderInput("seca_upper",
                      "Upper mode",
                      value = .95,
                      min = 0,
                      max = 1)
      } else if (input$seca_parms == "logit-logistic")
      {
          sliderInput("seca_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      } else if (input$seca_parms == "logit-normal")
      {
          sliderInput("seca_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      }
  })
    output$seca_max = renderUI(
    {
        if (input$seca_parms == "trapezoidal")
        {
            sliderInput("seca_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        } else if (input$seca_parms == "uniform")
        {
            sliderInput("seca_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        }
    })
    output$seca_mode = renderUI(
    {
        if (input$seca_parms == "triangular")
        {
            sliderInput("seca_mode",
                       "Mode",
                       value = .75,
                       min = 0,
                       max = 1)
        }        
    }
    )
    output$seca_location = renderUI(
    {
        if (input$seca_parms == "logit-logistic")
        {
            sliderInput("seca_location",
                        "Location",
                        value = 0,
                        min = 0,
                        max = 1)
        } else if (input$seca_parms == "logit-normal")
        {
            sliderInput("seca_location",
                        "Location",
                        value = 1.45,
                        min = 0,
                        max = 5)
        }
    }
    )
    output$seca_scale = renderUI(
    {
        if (input$seca_parms == "logit-logistic")
        {
            sliderInput("seca_scale",
                        "Scale",
                        value = 0.8,
                        min = 0,
                        max = 1)
        } else if (input$seca_parms == "logit-normal")
        {
            sliderInput("seca_scale",
                        "Scale",
                        value = 0,
                        min = 0,
                        max = 1)
        }
    }
    )
  ## seexp Distributions
  output$seexp_min = renderUI(
  {
      if (input$seexp_parms == "trapezoidal")
      {
          sliderInput("seexp_min",
                      "Minimum",
                      value = .75,
                      min = 0,
                      max = 1)
      } else if (input$seexp_parms == "uniform")
      {
          sliderInput("seexp_min",
                      "Minimum",
                      value = .6,
                      min = 0,
                      max = 1)
      }
  })
    output$seexp_lower = renderUI(
    {
      if (input$seexp_parms == "triangular")
      {
          sliderInput("seexp_lower",
                     "Lower limit",
                     value = .6,
                     min = 0,
                     max = 1)
      } else if (input$seexp_parms == "trapezoidal")
      {
          sliderInput("seexp_lower",
                      "Lower mode",
                      value = .85,
                      min = 0,
                      max = 1)
      } else if (input$seexp_parms == "logit-logistic")
      {
          sliderInput("seexp_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      } else if (input$seexp_parms == "logit-normal")
      {
          sliderInput("seexp_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      }
    })
      output$seexp_upper = renderUI(
      {
      if (input$seexp_parms == "triangular")
      {
          sliderInput("seexp_upper",
                     "Upper limit",
                     value = .9,
                     min = 0,
                     max = 1)
      } else if (input$seexp_parms == "trapezoidal")
      {
          sliderInput("seexp_upper",
                      "Upper mode",
                      value = .95,
                      min = 0,
                      max = 1)
      } else if (input$seexp_parms == "logit-logistic")
      {
          sliderInput("seexp_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      } else if (input$seexp_parms == "logit-normal")
      {
          sliderInput("seexp_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      }
  })
    output$seexp_max = renderUI(
    {
        if (input$seexp_parms == "trapezoidal")
        {
            sliderInput("seexp_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        } else if (input$seexp_parms == "uniform")
        {
            sliderInput("seexp_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        }
    })
    output$seexp_mode = renderUI(
    {
        if (input$seexp_parms == "triangular")
        {
            sliderInput("seexp_mode",
                       "Mode",
                       value = .75,
                       min = 0,
                       max = 1)
        }
    }
    )
    output$seexp_location = renderUI(
    {
        if (input$seexp_parms == "logit-logistic")
        {
            sliderInput("seexp_location",
                        "Location",
                        value = 0,
                        min = 0,
                        max = 1)
        } else if (input$seexp_parms == "logit-normal")
        {
            sliderInput("seexp_location",
                        "Location",
                        value = 1.45,
                        min = 0,
                        max = 5)
        }
    }
    )
    output$seexp_scale = renderUI(
    {
        if (input$seexp_parms == "logit-logistic")
        {
            sliderInput("seexp_scale",
                        "Scale",
                        value = 0.8,
                        min = 0,
                        max = 1)
        } else if (input$seexp_parms == "logit-normal")
        {
            sliderInput("seexp_scale",
                        "Scale",
                        value = 0,
                        min = 0,
                        max = 1)
        }
    }
    )
  ## spca Distributions
  output$spca_min = renderUI(
  {
      if (input$spca_parms == "trapezoidal")
      {
          sliderInput("spca_min",
                      "Minimum",
                      value = .75,
                      min = 0,
                      max = 1)
      } else if (input$spca_parms == "uniform")
      {
          sliderInput("spca_min",
                      "Minimum",
                      value = .6,
                      min = 0,
                      max = 1)
      }
  })
    output$spca_lower = renderUI(
    {
      if (input$spca_parms == "triangular")
      {
          sliderInput("spca_lower",
                     "Lower limit",
                     value = .6,
                     min = 0,
                     max = 1)
      } else if (input$spca_parms == "trapezoidal")
      {
          sliderInput("spca_lower",
                      "Lower mode",
                      value = .85,
                      min = 0,
                      max = 1)
      } else if (input$spca_parms == "logit-logistic")
      {
          sliderInput("spca_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      } else if (input$spca_parms == "logit-normal")
      {
          sliderInput("spca_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      }
    })
      output$spca_upper = renderUI(
  {
      if (input$spca_parms == "triangular")
      {
          sliderInput("spca_upper",
                     "Upper limit",
                     value = .9,
                     min = 0,
                     max = 1)
      } else if (input$spca_parms == "trapezoidal")
      {
          sliderInput("spca_upper",
                      "Upper mode",
                      value = .95,
                      min = 0,
                      max = 1)
      } else if (input$spca_parms == "logit-logistic")
      {
          sliderInput("spca_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      } else if (input$spca_parms == "logit-normal")
      {
          sliderInput("spca_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      }
  })
    output$spca_max = renderUI(
    {
        if (input$spca_parms == "trapezoidal")
        {
            sliderInput("spca_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        } else if (input$spca_parms == "uniform")
        {
            sliderInput("spca_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        }
    })
    output$spca_mode = renderUI(
    {
        if (input$spca_parms == "triangular")
        {
            sliderInput("spca_mode",
                       "Mode",
                       value = .75,
                       min = 0,
                       max = 1)
        }        
    }
    )
    output$spca_location = renderUI(
    {
        if (input$spca_parms == "logit-logistic")
        {
            sliderInput("spca_location",
                        "Location",
                        value = 0,
                        min = 0,
                        max = 1)
        } else if (input$spca_parms == "logit-normal")
        {
            sliderInput("spca_location",
                        "Location",
                        value = 1.45,
                        min = 0,
                        max = 5)
        }
    }
    )
    output$spca_scale = renderUI(
    {
        if (input$spca_parms == "logit-logistic")
        {
            sliderInput("spca_scale",
                        "Scale",
                        value = 0.8,
                        min = 0,
                        max = 1)
        } else if (input$spca_parms == "logit-normal")
        {
            sliderInput("spca_scale",
                        "Scale",
                        value = 0,
                        min = 0,
                        max = 1)
        }
    }
    )
  ## spexp Distributions
  output$spexp_min = renderUI(
  {
      if (input$spexp_parms == "trapezoidal")
      {
          sliderInput("spexp_min",
                      "Minimum",
                      value = .75,
                      min = 0,
                      max = 1)
      } else if (input$spexp_parms == "uniform")
      {
          sliderInput("spexp_min",
                      "Minimum",
                      value = .6,
                      min = 0,
                      max = 1)
      }
  })
    output$spexp_lower = renderUI(
    {
      if (input$spexp_parms == "triangular")
      {
          sliderInput("spexp_lower",
                     "Lower limit",
                     value = .6,
                     min = 0,
                     max = 1)
      } else if (input$spexp_parms == "trapezoidal")
      {
          sliderInput("spexp_lower",
                      "Lower mode",
                      value = .85,
                      min = 0,
                      max = 1)
      } else if (input$spexp_parms == "logit-logistic")
      {
          sliderInput("spexp_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      } else if (input$spexp_parms == "logit-normal")
      {
          sliderInput("spexp_lower",
                      "Lower bound shift",
                      value = .5,
                      min = 0,
                      max = 1)
      }
    })
      output$spexp_upper = renderUI(
  {
      if (input$spexp_parms == "triangular")
      {
          sliderInput("spexp_upper",
                     "Upper limit",
                     value = .9,
                     min = 0,
                     max = 1)
      } else if (input$spexp_parms == "trapezoidal")
      {
          sliderInput("spexp_upper",
                      "Upper mode",
                      value = .95,
                      min = 0,
                      max = 1)
      } else if (input$spexp_parms == "logit-logistic")
      {
          sliderInput("spexp_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      } else if (input$spexp_parms == "logit-normal")
      {
          sliderInput("spexp_upper",
                      "Upper bound shift",
                      value = .9,
                      min = 0,
                      max = 1)
      }
  })
    output$spexp_max = renderUI(
    {
        if (input$spexp_parms == "trapezoidal")
        {
            sliderInput("spexp_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        } else if (input$spexp_parms == "uniform")
        {
            sliderInput("spexp_max",
                        "Maximum",
                        value = 1,
                        min = 0,
                        max = 1)
        }
    })
    output$spexp_mode = renderUI(
    {
        if (input$spexp_parms == "triangular")
        {
            sliderInput("spexp_mode",
                       "Mode",
                       value = .75,
                       min = 0,
                       max = 1)
        }
    }
    )
    output$spexp_location = renderUI(
    {
        if (input$spexp_parms == "logit-logistic")
        {
            sliderInput("spexp_location",
                        "Location",
                        value = 0,
                        min = 0,
                        max = 1)
        } else if (input$spexp_parms == "logit-normal")
        {
            sliderInput("spexp_location",
                        "Location",
                        value = 1.45,
                        min = 0,
                        max = 5)
        }
    }
    )
    output$spexp_scale = renderUI(
    {
        if (input$spexp_parms == "logit-logistic")
        {
            sliderInput("spexp_scale",
                        "Scale",
                        value = 0.8,
                        min = 0,
                        max = 1)
        } else if (input$spexp_parms == "logit-normal")
        {
            sliderInput("spexp_scale",
                        "Scale",
                        value = 0,
                        min = 0,
                        max = 1)
        }
    }
    )

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
    episensrout_probsens = reactive({
        mat <- data3()
        dist_seca <- if (input$seca_parms == "trapezoidal") {
                          dist_seca <- c(input$seca_min, input$seca_lower,
                                          input$seca_upper, input$seca_max)
                      } else if (input$seca_parms == "triangular") {
                          dist_seca <- c(input$seca_lower, input$seca_upper,
                                          input$seca_mode)
                      } else if (input$seca_parms == "uniform") {
                          dist_seca <- c(input$seca_min, input$seca_max)
                      } else if (input$seca_parms == "logit-logistic") {
                          dist_seca <- c(input$seca_location, input$seca_scale,
                                          input$seca_lower, input$seca_upper)
                      } else if (input$seca_parms == "logit-normal") {
                          dist_seca <- c(input$seca_location, input$seca_scale,
                                          input$seca_lower, input$seca_upper)
                      }
        dist_seexp <- if (input$seexp_parms == "trapezoidal") {
                          dist_seexp <- c(input$seexp_min, input$seexp_lower,
                                          input$seexp_upper, input$seexp_max)
                      } else if (input$seexp_parms == "triangular") {
                          dist_seexp <- c(input$seexp_lower, input$seexp_upper,
                                          input$seexp_mode)
                      } else if (input$seexp_parms == "uniform") {
                          dist_seexp <- c(input$seexp_min, input$seexp_max)
                      } else if (input$seexp_parms == "logit-logistic") {
                          dist_seexp <- c(input$seexp_location, input$seexp_scale,
                                          input$seexp_lower, input$seexp_upper)
                      } else if (input$seexp_parms == "logit-normal") {
                          dist_seexp <- c(input$seexp_location, input$seexp_scale,
                                          input$seexp_lower, input$seexp_upper)
                      }
        dist_spca <- if (input$spca_parms == "trapezoidal") {
                          dist_spca <- c(input$spca_min, input$spca_lower,
                                          input$spca_upper, input$spca_max)
                      } else if (input$spca_parms == "triangular") {
                          dist_spca <- c(input$spca_lower, input$spca_upper,
                                          input$spca_mode)
                      } else if (input$spca_parms == "uniform") {
                          dist_spca <- c(input$spca_min, input$spca_max)
                      } else if (input$spca_parms == "logit-logistic") {
                          dist_spca <- c(input$spca_location, input$spca_scale,
                                          input$spca_lower, input$spca_upper)
                      } else if (input$spca_parms == "logit-normal") {
                          dist_spca <- c(input$spca_location, input$spca_scale,
                                          input$spca_lower, input$spca_upper)
                      }
        dist_spexp <- if (input$spexp_parms == "trapezoidal") {
                          dist_spexp <- c(input$spexp_min, input$spexp_lower,
                                          input$spexp_upper, input$spexp_max)
                      } else if (input$spexp_parms == "triangular") {
                          dist_spexp <- c(input$spexp_lower, input$spexp_upper,
                                          input$spexp_mode)
                      } else if (input$spexp_parms == "uniform") {
                          dist_spexp <- c(input$spexp_min, input$spexp_max)
                      } else if (input$spexp_parms == "logit-logistic") {
                          dist_spexp <- c(input$spexp_location, input$spexp_scale,
                                          input$spexp_lower, input$spexp_upper)
                      } else if (input$spexp_parms == "logit-normal") {
                          dist_spexp <- c(input$spexp_location, input$spexp_scale,
                                          input$spexp_lower, input$spexp_upper)
                      }
                      
        set.seed(123)
        mod <- if (input$corr == "none") {
                   mod <- probsens(mat,
                                   type = input$misclass2,
                                   reps = input$reps,
                                   seca.parms = list(input$seca_parms, dist_seca),
                                   spca.parms = list(input$spca_parms, dist_spca)
                                   )
               } else if (input$corr == "yes") {
                   mod <- probsens(mat,
                                   type = input$misclass2,
                                   reps = input$reps,
                                   seca.parms = list(input$seca_parms, dist_seca),
                                   seexp.parms = list(input$seca_parms, dist_seca),
                                   spca.parms = list(input$spca_parms, dist_spca),
                                   spexp.parms = list(input$spexp_parms, dist_spexp),
                                   corr.se = input$corr_se,
                                   corr.sp = input$corr_sp
                                   )
               }
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

    output$obs_data3 = renderTable({
        vals <- episensrout_probsens()
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

    output$obs_measures3 = renderTable({
        vals <- episensrout_probsens()
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

    output$adj_measures3 = renderTable({
        vals <- episensrout_probsens()
        vals$adj.measures
    }, colnames = FALSE, rownames = TRUE)

    ## Output of selection OR
    output$selbias_or = renderTable({
        vals <- episensrout()
        vals$selbias.or
    }, colnames = FALSE)
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
