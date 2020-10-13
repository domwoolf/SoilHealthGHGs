# ---------------------------------------------------------------------------------------------
# Title:   Soil Health Greenhouse Gas Calculator
# Author:    Dominic Woolf, Cornell University
# AuthorUrl: https://scs.cals.cornell.edu/people/dominic-woolf/
# License:   Copywrite Dominic Woolf
# ---------------------------------------------------------------------------------------------
library(shiny)
library(data.table)
library(shinyBS)
# library(bsplus)
library(shinyjs)

tables.path = 'tables/'
states = fread(paste0(tables.path, 'state.csv'))[, NAME]
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
setkey(counties, state, county)


shinyUI(fluidPage(
    includeCSS("www/myStyle.css"),
    useShinyjs(),
    div(img(src = "AtkinsonLogo.svg", height="50px"), style="position: absolute; top: 10px; right: 10px;"),
    titlePanel(div( 
             h1("FAST-GHG",
             h3("Fertilizer And Soil Tool for GreenHouse Gases"),
             h4("A FAST calculator for climate-change mitigation in agriculture"))), 
        windowTitle="FAST-GHG"),
    sidebarLayout(
        sidebarPanel(
            selectInput("State", "Select state", states, selected = "Alabama"),
            textOutput("county_selected"),
            tags$head(tags$style("#county_selected {color: firebrick}")),
            selectInput("County", "Select county", 'Unknown'),
            radioButtons("Crop", "Select main crops in the rotation", c('Maize', 'Wheat', 'Soybean')),
            textOutput("valid_yield"),
            tags$head(tags$style("#valid_yield {color: firebrick}")),
            textOutput("valid_Nrate"),
            tags$head(tags$style("#valid_Nrate {color: firebrick}")),
            radioButtons("Cover.Crop", "Cover crop type", c('None', 'Legume', 'Non-legume')),
            textOutput("valid_cc_climate"),
            tags$head(tags$style("#valid_cc_climate {color: firebrick}")),
            radioButtons("Tillage.Practice", "Tillage practice", 
                         c('Conventional', 'Reduced-till', 'No-till')),
            checkboxGroupInput("N.optimization", "Nitrogen fertilizer practice(s)", 
                               choiceValues = c('Model', 'VRT', 'Timing', 'Other'),
                               choiceNames = c('Model-based optimization', 
                                 'Variable Rate Technology', 
                                 'Improved Timing', 
                                 'Other')),
            bsTooltip("N.optimization", "", "right", options = list(container = "body")),
            numericInput("Delta.N", "Reduction in N fertilizer application (kg N / ha / year) - use negative values if N input increased", 0, step = 10),
            radioButtons('risk_assessment', label = 'Have you conducted a risk assessment?', 
                         choices = c('Yes', 'No'), selected = 'No'),
            sliderInput('R', "What is your estimate of the risk that cover cropping will cease within the next 100 years? (100% means absolute certainty that the practice will cease in the future; 0% means absolute certainty that the practice will continue over the century", 0, 100, 0, step = 10, value = 50, post = '%'),
            textAreaInput('permanence_info', ' Please upload a copy of the risk-assessment report, including which tool was used, and what risk mitigation practices were implemented)', height = "400%")
        ),
        mainPanel(tabsetPanel(
            tabPanel("Results", plotOutput('barplot', height = '250px')),
            tabPanel("Calculations", 
                     p("For complete documentation of the method, 
                       which includes the Tables and Equations referenced by number below, 
                       please refer to the documentation ",
                     a(href='https://github.com/domwoolf/SoilHealthGHGs/blob/master/man/OverallMethods_1.01.pdf', "available here.", target="_blank")),
                     tags$h3('User Inputs'),
                     tableOutput('input.report'), 
                     tags$br(),
                     tags$h3('Derived parameters'),
                     tableOutput('tables.report'),
                     tags$br(),
                     tags$h3('Calculations'),
                     tableOutput('eq.report')),
            tabPanel("About", width = 5,
                     p("FAST-GHG is a greenhouse-gas calculator tool 
                       designed to give rapid, yet robust, 
                       estimates of the potential to reduce agicultural emissions."),
                     p('The tool models the impacts of soil health 
                       ("regenerative agriculture") and fertilizer optimization practices 
                       on net greenhouse gas emissions and soil organic carbon sequestration.
                       It uses geospatial data on soil properties, climate, crop yields,
                       and fertilizer application rates to generate estimates based on simple user inputs.
                       Users only need to specify the location, the crop, and the management practice(s)
                       to obtain rapid estimates of the climate-change impact.'),
                     p("FAST-GHG currently provides calculations for the most widespread row crops 
                     (maize, wheat, and soybean), grown in the continental USA. 
                       We hope to expand the tool over time to include more crops, more countries, more practices, 
                       and more environmental impacts."),
                     p("The web application was written and designed by ", 
                       a(href = 'https://scs.cals.cornell.edu/people/dominic-woolf/', 'Dominic Woolf', .noWS = "outside", target="_blank"), 
                       .noWS = c("after-begin", "before-end")),
                     p("The underlying model was written by our scientific team at Cornell University
                       (listed in alphabetic order, as they all contributed equally):"),
                     p(a(href = 'https://www.systems.cs.cornell.edu/ctonitto/index.html', 'Christina Tonitto', target="_blank")),
                     p(a(href = 'https://scs.cals.cornell.edu/people/peter-woodbury/', 'Peter Woodbury', target="_blank")),
                     p(a(href = 'https://scs.cals.cornell.edu/people/dominic-woolf/', 'Dominic Woolf', target="_blank")),
                     p("You can find a complete description of how the calculations are performed, and the scientific basis for the method  ", 
                       a(href = 'https://github.com/domwoolf/SoilHealthGHGs/blob/master/man/OverallMethods_1.01.pdf', 'here.', target="_blank", .noWS = "outside", target="_blank"), 
                       .noWS = c("after-begin", "before-end"))
                     ),
            tabPanel("FAQs", width = 5,
                     tags$br(),
                     p(tags$b("The soil organic carbon (SOC) change in FAST-GHG seems low to me.  Why is that?"),
                     tags$br(),
                     "..."),
                     p(tags$b("I expected non-leguminous cover crops to reduce my emissions, but FAST-GHG
                              predicts that they will increase? What is the reason for this?"),
                       tags$br(),
                       "...")
            )
            ))
        )
    )
)