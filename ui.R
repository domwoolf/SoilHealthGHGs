# ---------------------------------------------------------------------------------------------
# Title:   FAST-GHG (TM)
# Purpose: This software is a "Fertilizer And Soil Tool for GreenHouse Gases", 
#          i.e. a calculator for climate-change mitigation in agriculture.
# Author:    Dominic Woolf, Cornell University
# AuthorUrl: https://scs.cals.cornell.edu/people/dominic-woolf/
# 
#     Copyright (C) 2020  Dominic Woolf
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ---------------------------------------------------------------------------------------------
library(shiny)
library(data.table)
library(shinyBS)
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
        h1("FAST-GHG™", 
           h3("Fertilizer And Soil Tool for GreenHouse Gases"),
           h4("A FAST calculator for climate-change mitigation in agriculture"))), 
        windowTitle="FAST-GHG"),
    sidebarLayout(
        sidebarPanel(width = 3,
                     selectInput("State", "Select state", states, selected = "Alabama"),
                     selectInput("County", "Select county", 'Unknown'),
                     bsTooltip("County", "County is optional, but will improve accuracy if specified.", 
                               options = list(container = "body")),
                     
                     radioButtons("Crop", "Select crop", c('Maize', 'Wheat', 'Soybean')),
                     bsTooltip("Crop", "For a rotation with more than one crop, run the model for each crop separately and take the average of results to calculate the overall GHG impact across the whole rotation.",
                               "right", options = list(container = "body")),
                     
                     radioButtons("Cover.Crop", "Cover crop type", 
                                  choiceNames = c('None', 'Legume', 'Non-legume', 'Mixed'),
                                  choiceValues = c('None', 'Legume', 'Non-legume', 'Mixed')),
                     
                     radioButtons("Tillage_Practice", "Tillage practice", 
                                  c('Conventional', 'Reduced-till', 'No-till')),
                     bsTooltip("Tillage_Practice", paste0(
                         "<b>Conventional</b> = ",
                         "Tillage that results in 0-15% residue cover.",
                         "<p><br><b>Reduced-till = </b>",
                         "Tillage that results in 16-50% residue cover for corn, or 16-30% residue cover for other crops.</p>",
                         "<p><br><b>No-Till = </b>",
                         "Tillage that results in 51-100% residue cover for corn, 31-100% residue cover for other crops.</p>"
                     ), 'right', options = list(container = "body")),
                     
                     checkboxGroupInput("N_optimization", "Nitrogen fertilizer practice(s)", 
                                        choiceValues = c('Model', 'VRT', 'Timing', 'Other'),
                                        choiceNames = c('Model-based optimization', 
                                                        'Variable Rate Application', 
                                                        'Improved Timing', 
                                                        'Other')),
                     bsTooltip("N_optimization", paste0(
                         "<b>Model-based optimization</b> = ",
                         "Agronomic modeling tools with real-time input data to optimize fertilizer rate and timing.",
                         "<p><br><b>Variable Rate Application = </b>",
                         "Use of technology (e.g., yield maps, crop or soil sensors, Variable Rate Technology (VRT), or in-field N rate tests) to vary N fertilizer application rate across or within farm fields to better match crop demand.</p>",
                         "<p><br><b>Improved Timing = </b>",
                         "Nitrogen application timed to optimize crop N uptake. For example, switching from fall to spring application or from pre-plant to side-dress application.</p>"
                     ), 'right', options = list(container = "body")),
                     
                     numericInput("Delta.N", "Reduction in N fertilizer application (kg N / ha / year) - use negative values if N input increased", 0, step = 10),
                     checkboxInput('Advanced', "Show advanced inputs"),
                     sliderInput('Yield', 'Crop Yield (tonnes / ha)', 0, 15, 5, 0.1),
                     sliderInput('Nrate', 'Mineral nitrogen fertilizer application rate (kg / ha)', 0 , 300, 150, 1),
                     radioButtons('risk_assessment', label = 'Have you conducted a risk assessment?', 
                                  choices = c('Yes', 'No'), selected = 'No'),
                     sliderInput('R', "What is your estimate of the risk that cover cropping will cease within the next 100 years? (100% means absolute certainty that the practice will cease in the future; 0% means absolute certainty that the practice will continue over the century", 0, 100, 0, step = 10, value = 50, post = '%')
        ),
        mainPanel(width = 9, tabsetPanel(
            tabPanel("Results",
                     div(style = "max-width: 600px;",
                         plotOutput('barplot', height = '250px'),
                         tags$br(),
                         hr(),
                         htmlOutput('results.text')
                     )
            ),
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
                     p("This software was authored by ", 
                       a(href = 'https://scs.cals.cornell.edu/people/dominic-woolf/', 'Dominic Woolf.', .noWS = "outside", target="_blank"), 
                       .noWS = c("after-begin", "before-end")),
                     p("The underlying model was written by our scientific team at Cornell University (in alphabetic order):",
                       a(href = 'https://www.systems.cs.cornell.edu/ctonitto/index.html', 'Christina Tonitto,', target="_blank"),
                       a(href = 'https://scs.cals.cornell.edu/people/peter-woodbury/', 'Peter Woodbury,', target="_blank"), 'and',
                       a(href = 'https://scs.cals.cornell.edu/people/dominic-woolf/', 'Dominic Woolf.', target="_blank")),
                     p("You can find a complete description of how the calculations are performed, and the scientific basis for the method ", 
                       a(href = 'https://github.com/domwoolf/SoilHealthGHGs/blob/master/man/OverallMethods_1.01.pdf', 'here.', target="_blank", .noWS = "outside", target="_blank"), 
                       .noWS = c("after-begin", "before-end")),
                     hr(),
                     p('Copyright (C) 2020  Dominic Woolf'), 
                     p('This program is free software: you can redistribute it and/or modify it under the terms of 
                     the GNU General Public License as published by the Free Software Foundation, 
                     either version 3 of the License, or (at your option) any later version.'),
                     p('This program is distributed in the hope that it will be useful,  
                     but WITHOUT ANY WARRANTY; without even the implied warranty of 
                     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.'),
                     p('You should have received a copy of the GNU General Public License along with this program.  
                     If not, see <https://www.gnu.org/licenses/>.'),
                     p('Source code available at https://sourceforge.net/projects/fast-ghg/')
            ),
            tabPanel("FAQs", width = 4,
                     br(),
                     div(style = "padding: 20px;", includeHTML("FAQ.html"))
                     # h3("Coming soon..."),
                     # tags$img(src = "under-construction.png", height="200px")
            ))
        )
    )
)
)