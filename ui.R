# ---------------------------------------------------------------------------------------------
# Title: Soil Health Greenhouse Gas Calculator - Draft version 0.01
# Author: Dominic Woolf, Cornell University
# AuthorUrl: https://scs.cals.cornell.edu/people/dominic-woolf/
# License: This is a draft (alpha) version, not for public release. Do not share, cite or copy.
# ---------------------------------------------------------------------------------------------
library(shiny)
library(data.table)
library(shinyjs)

tables.path = 'tables/'
states = fread(paste0(tables.path, 'state.csv'))[, NAME]
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
setkey(counties, state, county)

shinyUI(fluidPage(
    useShinyjs(),
    titlePanel("Soil Health GHG Calculator"),
    sidebarLayout(
        sidebarPanel(
            selectInput("State", "Select state", states, selected = "Alabama"),
            textOutput("county_selected"),
            tags$head(tags$style("#county_selected {color: firebrick}")),
            selectInput("County", "Select county", 'Unknown'),
            radioButtons("Crop", "Select main crops in the rotation", c('Maize', 'Wheat', 'Soybean')),
            radioButtons("Cover.Crop", "Cover crop type", c('None', 'Legume', 'Non-legume')),
            textOutput("valid_cc_climate"),
            tags$head(tags$style("#valid_cc_climate {color: firebrick}")),
            radioButtons("Tillage.Practice", "Tillage practice", 
                         c('Conventional', 'Reduced-till', 'No-till')),
            checkboxGroupInput("N.optimization", "Nitrogen fertilizer practice(s)", 
                               c('Model', 'VRT.Maize', 'VRT.Wheat', 'Timing', 'Other')),
            numericInput("Delta.N", "Reduction in N fertilizer application (kg N / ha / year) - use negative values if N input increased", 0, step = 10),
            radioButtons('risk_assessment', label = 'Have you conducted a risk assessment?', 
                         choices = c('Yes', 'No'), selected = 'No'),
            sliderInput('R', "What is your estimate of the risk that cover cropping will cease within the next 100 years? (100% means absolute certainty that the practice will cease in the future; 0% means absolute certainty that the practice will continue over the century", 0, 100, 0, step = 10, value = 50, post = '%'),
            textAreaInput('permanence_info', ' Please upload a copy of the risk-assessment report, including which tool was used, and what risk mitigation practices were implemented)', height = "400%")
        ),
        mainPanel(tabsetPanel(
            tabPanel("Plot", 
                tags$h3("Note, this an alpha-testing version.  
                        Look-up tables and equations subject to change.", style = "color:FireBrick"),
                plotOutput('barplot', height = '250px')
                ),
            tabPanel("Table", tableOutput('results.table')),
            tabPanel("Input", textOutput('input.dput')),
            tabPanel("Report", htmlOutput("results.report"))
        ), width = 5)
    )
))
