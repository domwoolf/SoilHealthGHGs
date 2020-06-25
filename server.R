# ---------------------------------------------------------------------------------------------
# Title: Soil Health Greenhouse Gas Calculator
# Author: Dominic Woolf, Cornell University
# AuthorUrl: https://scs.cals.cornell.edu/people/dominic-woolf/
# License: This is a beta version, not for public release. Do not share, cite or copy.
# 
# Notes:
#   1. This version does not support multiple crops in rotation or multiple cover crop types
# ---------------------------------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(cowplot)
library(data.table)
library(shinyjs)
theme_set(theme_cowplot())

# helper function for table lookups.  Returns TRUE, whatever the input, if lookup value is 'All'
`%is%` = function(a,b) a == 'All' | a == b

tables.path = 'tables/'
states = fread(paste0(tables.path, 'state.csv'))[, .(state = NAME)]
counties = fread(paste0(tables.path, 'county.csv'))[, .(state = STATE, county = NAME)]
setkey(states, state)
setkey(counties, state, county)

# ---------------------------------------------------------------------------------------------
# load the data files emission factors and default parameter values
# ---------------------------------------------------------------------------------------------
#  function reads csv data from given file & path into a data.table with R-friendly column names
read.tab = function(fname, path = tables.path) {
    tab = fread(paste0(path, fname))
    # function to remove Latex formatting from column headers, to make them into R-friendly names
    # note that base::make.names does not make exactly what we want here
    normal.names = function(x) {
        x = gsub('\\$|\\\\|\\{|\\}|\\$','',x)
        gsub('textrm| ','.', x)
    }
    setnames(tab, normal.names(names(tab)))
}
tab.names = list.files(tables.path, '.csv')
tabs = lapply(tab.names, read.tab)
names(tabs) = sub('.csv$', '', tab.names)
tabs$state[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]
tabs$county[, c('Temperature', 'Moisture') := transpose(strsplit(climate, ' '))]

# ---------------------------------------------------------------------------------------------
# reactive elements in the UI start here...
# ---------------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
    
    # Update County list whenever different State is selected
    observe({
        county_list = counties[state == input$State, c('Unknown', county)]
        updateSelectInput(session, "County", choices = county_list, selected = 'Unknown')
    })
    
    # check that at least one crop is specified in the rotation
    output$county_selected = reactive({
        validate(need(input$County != 'Unknown', 'Please specify a county to improve accuracy.'))
    })
    
    # Check for cover crops in dry climate
    output$valid_cc_climate = reactive({
        validate(need(input$Cover.Crop == 'None' | results.full()$Moisture == 'Moist', 
                      'Cover crops not supported in dry climate!'))
    })
    
    # Check whether user value of "change in N rate" is required.  If so, ask for amount
    observe({
        if ('Other' %in% input$N.optimization) {
            shinyjs::show(id = "Delta.N", anim = TRUE)
        } else {
            shinyjs::hide(id = "Delta.N", anim = TRUE)
        }
    })
    
    # Check whether risk assessment was conducted.  If so, ask for more info
    observe({
        if (input$risk_assessment == "Yes") {
            shinyjs::show(id = "R", anim = TRUE)
            shinyjs::show(id = "permanence_info", anim = TRUE)
        } else {
            shinyjs::hide(id = "R", anim = TRUE)
            shinyjs::hide(id = "permanence_info", anim = TRUE)
        }
    })
    
    # ----------------------------------------------------------
    # Results calculations start here...
    # Note: Table and Equation numbering in comments are cross-references to the "Overall Methods" document
    #       Please refer to the full documentation to follow the method applied here.
    # ----------------------------------------------------------
    results.full = reactive({
        # ----------------------------------------------------------
        # First do table lookups:
        # ----------------------------------------------------------
        results = list()
        # results$f_c = table(input$Crop)/length(input$Crop)
        results$f_c = 1 # fraction of crop in rotation - set to one for single crop
        results$legume_frac = 0.0
        if (input$Cover.Crop == 'Legume') results$legume_frac = 1.0
        
        # ----------------------------------------------------------
        # Appendix A or B (depending whether county provided):
        # ----------------------------------------------------------
        if (input$County == 'Unknown') {
            results$Temperature = tabs$state[NAME==input$State, Temperature]
            results$Moisture = tabs$state[NAME==input$State, Moisture]
            results$clay = tabs$state[NAME==input$State, clay/100]
            results$Soil = tabs$state[NAME==input$State, soil.class]
            # results$Yield = tabs$county[STATE==input$State, get(paste0(Crop,'.Yield'))]
            # results$Nrate = tabs$county[STATE==input$State, get(paste0(Crop,'.Nrate'))]
        } else {
            results$Temperature = tabs$county[STATE==input$State & NAME==input$County, Temperature]
            results$Moisture = tabs$county[STATE==input$State & NAME==input$County, Moisture]
            results$clay = tabs$county[STATE==input$State & NAME==input$County, clay/100]
            results$Soil = tabs$county[STATE==input$State & NAME==input$County, soil.class]
            # results$Yield = tabs$county[STATE==input$State & NAME==input$County, get(paste0(Crop,'.Yield'))]
            # results$Nrate = tabs$county[STATE==input$State & NAME==input$County, get(paste0(Crop,'.Nrate'))]
        }
        # set dummy values for yield and N-rate, for now (values not yet entered in table)
        results$Yield = c(10500, 4500, 3500)[match(input$Crop, c('Maize', 'Wheat', 'Soybean'))]# kg grain / ha / yr
        results$Nrate = c(170, 220, 0)[match(input$Crop, c('Maize', 'Wheat', 'Soybean'))] # kg N / ha / yr
        # ----------------------------------------------------------
        # Table 1:
        # ----------------------------------------------------------
        constants = as.list(fread('constants.csv')[, tibble::deframe(.SD[,1:2])])
        # ----------------------------------------------------------
        # Table 2: Only contains tillage definitions for reference, not needed here.
        # ----------------------------------------------------------
        # Table 3: SOC sequestration from cover crops
        # ----------------------------------------------------------
        results$Delta.SOC_CC = if (input$Cover.Crop == 'None') 0.0 else tabs$SOC_CC[Moisture == results$Moisture, Delta.SOC_CC] # Mg C / ha / yr
        # ----------------------------------------------------------
        # Table 4: SOC sequestration from tillage
        # ----------------------------------------------------------
        results$Delta.SOC_T = tabs$SOC_T[
            Tillage.Practice == input$Tillage.Practice & 
                Temperature %is% results$Temperature &
                Moisture %is%  results$Moisture &
                Soil %is% c('Other', 'Sandy')[(results$Soil == 'Sandy') + 1L], Delta.SOC_T]
        # ----------------------------------------------------------
        # Table 5: f100 permanence factor
        # ----------------------------------------------------------
        results$f_100_CC =  tabs$f_100[Temperature == results$Temperature & Moisture == results$Moisture, f_100_CC]
        results$f_100_T =  tabs$f_100[Temperature == results$Temperature & Moisture == results$Moisture, f_100_T]
        # ----------------------------------------------------------
        # Table 6: On farm CO2 emissions from cover crops
        # ----------------------------------------------------------
        tabs$CO2_CC = rbind(tabs$CO2_CC, list('None', 'All', 0, 0))
        results$Delta.CO2_I_CC = tabs$CO2_CC[Cover.crop == input$Cover.Crop & Tillage.Practice %is% input$Tillage.Practice, Delta.CO2_I]
        results$Delta.CO2_F_CC = tabs$CO2_CC[Cover.crop == input$Cover.Crop & Tillage.Practice %is% input$Tillage.Practice, Delta.CO2_F]
        # ----------------------------------------------------------
        # Table 7: On farm CO2 emissions from tillage
        # ----------------------------------------------------------
        tabs$CO2_T = rbind(tabs$CO2_T, list('Conventional', 'All', 0, 0))
        results$Delta.CO2_I_T = tabs$CO2_T[Tillage.Practice == input$Tillage.Practice & Crop %is% input$Crop, Delta.CO2_I]
        results$Delta.CO2_F_T = tabs$CO2_T[Tillage.Practice == input$Tillage.Practice & Crop %is% input$Crop, Delta.CO2_F]
        # ----------------------------------------------------------
        # Table 8: Change in yield from cover crops
        # ----------------------------------------------------------
        results$F_Y_CC = tabs$yield_CC[Crop == input$Crop & Moisture %is% results$Moisture, F_Y_CC/100]
        if (input$Cover.Crop == 'None') results$F_Y_CC = 0.0
        # ----------------------------------------------------------
        # Table 9: Change in yield from tillage
        # ----------------------------------------------------------
        tabs$yield_T = rbind(tabs$yield_T, data.table('Conventional', 'All', 'All', 'All', 0.0), use.names=FALSE)
        results$Rotation = 'Other'
        if ('Maize' %in% input$Crop) results$Rotation = 'Continuous Maize'
        if (all (c('Maize', 'Soybean') %in% input$Crop)) results$Rotation = 'Maize-Soybean'
        results$F_Y_T = tabs$yield_T[
            Tillage.Practice == input$Tillage.Practice & 
                Crop %is% input$Crop & 
                Moisture %is% results$Moisture &
                Rotation %is% results$Rotation,  F_Y_T/100]
        # ----------------------------------------------------------
        # Table 10: Leakage emissions
        # ----------------------------------------------------------
        results$F_i = tabs$F_iluc[Crop == input$Crop, F_i / 1000] # Mg Co2e / kg grain
        results$F_p = tabs$F_iluc[Crop == input$Crop, F_p / 1000] # Mg Co2e / kg grain
        # ----------------------------------------------------------
        # Table 11: Direct N2O emission factor for mineral fertilizer
        # ----------------------------------------------------------
        results$f_Nd = 0.0079 # kg N2O/ kg N
        if (input$Crop == 'Maize' & results$Moisture == 'Moist') results$f_Nd = 0.085 * results$clay
        if (input$Crop == 'Wheat' & results$Moisture == 'Moist') results$f_Nd = 0.0251 
        if (input$Crop == 'Soybean' & results$Moisture == 'Moist') results$f_Nd = 0.0251
        # ----------------------------------------------------------
        # Table 12: Direct and indirect N2O emission factor for organic N
        # ----------------------------------------------------------
        results$f_ONd = tabs$f_ONd[Moisture == results$Moisture, f_ONd] # kg N2O/ kg N
        results$f_ONv = tabs$f_ONd[Moisture == results$Moisture, f_ONv] # kg N2O/ kg N
        results$f_ONl = tabs$f_ONd[Moisture == results$Moisture, f_ONl] # kg N2O/ kg N
        # ----------------------------------------------------------
        # Table 13: Indirect N2O emission factors for mineral N
        # ----------------------------------------------------------
        results$f_Nv = tabs$f_Ni[Crop == input$Crop & Moisture %is% results$Moisture, f_Nv] # indirect N2O EF, volatilisation (kg N2O / kg N)
        results$f_Nl = tabs$f_Ni[Crop == input$Crop & Moisture %is% results$Moisture, f_Nl] # indirect N2O EF, leaching (kg N2O / kg N)
        # ----------------------------------------------------------
        # Table 14: Relative reduction in nitrogen leaching
        # ----------------------------------------------------------
        results$f_up = tabs$f_up[Cover.Crop == input$Cover.Crop & Tillage %is% input$Tillage.Practice, f_up]
        # ----------------------------------------------------------
        # Table 15: N-optimization emission factor
        # ----------------------------------------------------------
        N.optimization = input$N.optimization
        if (is.null(N.optimization)) N.optimization = 'None'
        results$f_O = if (length(setdiff(N.optimization, c('None', 'Other')))) tabs$N_opt[Practice %in% N.optimization, max(f_O)] else 0.0
        # ----------------------------------------------------------
        # Table 16: Nitrogen content of grain
        # ----------------------------------------------------------
        results$f_Ng = tabs$f_Ng[Crop == input$Crop, f_Ng] # crop N content (kg N / kg grain)
        # ----------------------------------------------------------
        # Table 17: Change in N leaching
        # ----------------------------------------------------------
        if (results$Moisture == 'Dry') {
            results$Delta.L = 0.0 #  kg N / ha / yr
        } else {
            if (input$Cover.Crop %in% c('Legume')) {
                results$Delta.L = 0.1 * results$Nrate
            } else {
                if (input$Cover.Crop %in% c('Non-legume')) {
                    results$Delta.L = 0.13 * results$Nrate
                } else {
                    results$Delta.L = switch (make.names(input$Tillage.Practice),
                                              Reduced.till = 0.03 * results$Nrate,
                                              No.till = 0.06 * results$Nrate,
                                              0.0)
                }
            }
        }
        # ----------------------------------------------------------
        # Table 18: Change in organic-N inputs
        # ----------------------------------------------------------
        results$Delta.ON =  if (input$Cover.Crop %in% c('Legume', 'Non-legume')) tabs$ON[Cover.Crop == input$Cover.Crop & Moisture==results$Moisture, Delta.ON] else 0.0
        
        ###############################################################################
        # Equations
        ###############################################################################
        # Note that order of evaulation is not the same as in the methods document
        # Equations are evaluated in order that ensures all dependencies are calculated first 
        results = within(results, {
            # ----------------------------------------------------------
            # Eq. 5:
            # ----------------------------------------------------------
            Delta.Y <- Yield * (F_Y_CC + F_Y_T) # kg grain / ha / yr
            # ----------------------------------------------------------
            # Table 15 (change in N input rate from each group of practices; kg N / ha / yr):
            # ----------------------------------------------------------
            Delta.N_A_legume <- 52 * constants$f_NUE -72 * Delta.SOC_CC # kg N / ha / yr 
            Delta.N_A_non.legume <- (Delta.L - 22) * constants$f_NUE    # kg N / ha / yr
            Delta.N_A <- Delta.N_A_legume*legume_frac + Delta.N_A_non.legume*(1-legume_frac)
            if (input$Cover.Crop == 'None') Delta.N_A = 0.0
            Delta.N_B <- Delta.L - 72 * Delta.SOC_T
            Delta.N_C <- f_O * (Nrate - Delta.N_A - Delta.N_B + Delta.Y * f_Ng)  
            Delta.N_D <- if ("Other" %in% input$N.optimization) input$Delta.N  else 0.0 # kg N / ha / yr
            # ----------------------------------------------------------
            # Equation 10: change in N input rate (kg N / ha / yr)
            # ----------------------------------------------------------
            Delta.N_tot <- 
                if ('Other' %in% input$N.optimization) {
                    Delta.N_D
                } else {
                    Delta.N_A + Delta.N_B + Delta.N_C - Delta.Y * f_Ng
                }
            # Delta.N_tot <- Delta.N_tot / 1e3 # convert to kg N / ha / yr
            # ----------------------------------------------------------
            # Equation 9: Change in CO2-equivalent GHG emissions from nitrogen fertilizer production (Mg CO2e / ha / yr).
            # ----------------------------------------------------------
            Delta.CO2_N <- Delta.N_tot * constants$f_Np
            # ----------------------------------------------------------
            # Equation 8: Change in indirect nitrous oxide emissions (kg N2O / ha / yr)
            # ----------------------------------------------------------
            # Delta.N2O_i <- Delta.N_tot * f_Ni + Delta.ON * f_ONi
            Delta.N2O_i <- Delta.N_tot * (f_Nv + (1-f_up)*f_Nl) + Delta.ON * (f_ONv + (1-f_up)*f_ONl)
            # ----------------------------------------------------------
            # Equation 7: Change in direct nitrous oxide emissions (kg N2O / ha / yr).
            # ----------------------------------------------------------
            Delta.N2O_d <- Delta.N_tot * f_Nd + Delta.ON * f_ONd
            # ----------------------------------------------------------
            # Eq. 6: Change in total nitrous oxide emissions (Mg N2O)
            # ----------------------------------------------------------
            Delta.N2O <- (Delta.N2O_d + Delta.N2O_i) / 1000
            # ----------------------------------------------------------
            # Eq. 4:  Note that ILUC is reversible, so we also apply the permancence-risk factor to it
            # ----------------------------------------------------------
            Delta.CO2_L <- f_c * Delta.Y * (F_p + (1-constants$f_i)*F_i*(1 - input$R/100)) 
            # ----------------------------------------------------------
            # Eq. 3:
            # ----------------------------------------------------------
            Delta.SOC <- f_100_CC * Delta.SOC_CC + f_100_T * Delta.SOC_T
            # ----------------------------------------------------------
            # Eq. 2:
            Delta.CO2_SOC <- Delta.SOC * (1 - input$R/100) * 44/12
            Delta.CO2 <- Delta.CO2_SOC + Delta.CO2_F_T + Delta.CO2_F_CC + Delta.CO2_I_T + Delta.CO2_I_CC + Delta.CO2_N + Delta.CO2_L
            # ----------------------------------------------------------
            # Eq. 1:
            Delta.CO2e_N2O = constants$GWP_N2O * Delta.N2O
            Delta.GHG <- Delta.CO2 + Delta.CO2e_N2O
        })
        results
    })
    
    # ----------------------------------------------------------
    results.summary = reactive({
        GHG.vbls = c('Delta.CO2_SOC', 'Delta.CO2_F_T', 'Delta.CO2_F_CC', 'Delta.CO2_I_T', 
                     'Delta.CO2_I_CC', 'Delta.CO2_L', 'Delta.CO2_N', 'Delta.CO2e_N2O')
        GHG.labs = c('SOC', 'Fuel (tillage)', 'Fuel (cover crops)', 
                     'Inputs (tillage)', 'Inputs (cover crops)', 
                     'Leakage', 'N fertilizer production', 'N2O emissions')
        results.GHG = as.numeric(unlist(lapply(results.full(), unname))[GHG.vbls])
        names(results.GHG) = GHG.labs
        results.GHG = stack(results.GHG)[, 2:1]
        results.GHG$group = 'A'
        results.GHG = rbind(results.GHG, data.frame(ind='Total', values = sum(results.GHG$values, na.rm = TRUE), group='B'))
        results.GHG
    })
    
    # ----------------------------------------------------------
    # outputs start here...
    # ----------------------------------------------------------
    output$results.table = renderTable(stack(results.full())[,2:1])
    output$input.dput = renderPrint(dput(reactiveValuesToList(input)))
    input.report = reactive({rbindlist(list(
        list(Description = 'State', Value = input$State, Units = ''),
        list('County', input$County, ''),
        list('Crop', input$Crop, ''),
        list('Cover Crop', input$Cover.Crop, ''),
        list('Tillage', input$Tillage.Practice, ''),
        list('N management practice(s)', paste0(input$N.optimization, collapse = ', '), ''),
        list('Decrease in N rate', if ('Other' %in% input$N.optimization) input$Delta.N else NA, 'kg N / ha / yr')
    ))})
    output$input.report = renderTable(input.report())
    
    tables.report = reactive({with(results.full(), rbindlist(list(
        list(Source = 'Table 18', Description = 'Temperature', Value = Temperature, Units = ''),
        list('Table 18', 'Moisture', Moisture, ''),
        list('Table 18', 'Clay', clay*100, '%'),
        list('Table 18', 'Soil', Soil, ''),
        list('Table 18', 'Yield', Yield, 'kg grain / ha /yr'),
        list('Table 18', 'N rate', Nrate, 'kg N / ha / yr'),
        list('Table 3', 'SOC change from cover crops', Delta.SOC_CC, 'Mg C / ha / yr'),
        list('Table 4', 'SOC change from tillage', Delta.SOC_T, 'Mg C / ha / yr'),
        list('Table 5', 'SOC permanence factor for cover crops', f_100_CC, ''),
        list('Table 5', 'SOC permanence factor for tillage', f_100_T, ''),
        list('Table 6', 'Input emissions for cover crops', Delta.CO2_I_CC, 'Mg CO2 / ha / yr'),
        list('Table 6', 'Fuel emissions for cover crops', Delta.CO2_F_CC, 'Mg CO2 / ha / yr'),
        list('Table 7', 'Input emissions for tillage', Delta.CO2_I_T, 'Mg CO2 / ha / yr'),
        list('Table 7', 'Fuel emissions for tillage', Delta.CO2_F_T, 'Mg CO2 / ha / yr'),
        list('Table 8', 'Relative change in yield due to cover crops', F_Y_CC, 'Fraction'),
        list('Table 9', 'Relative change in yield due to tillage', F_Y_T, 'Fraction'),
        list('Table 10', 'Carbon opportunity cost', F_i*1000, 'kg Co2e / kg grain'),
        list('Table 10', 'Production emissions from leakage', F_p*1000, 'kg Co2e / kg grain'),
        list('Table 11', 'N2O direct emissions factor for mineral N', f_Nd, 'kg N2O/ kg N'),
        list('Table 12', 'N2O direct emissions factor for organic N', f_ONd, 'kg N2O/ kg N'),
        list('Table 12', 'N2O volatilisation emissions factor for organic N', f_ONv, 'kg N2O/ kg N'),
        list('Table 12', 'N2O leaching emissions factor for organic N', f_ONl, 'kg N2O/ kg N'),
        list('Table 13', 'N2O volatilisation emissions factor for mineral N', f_Nv, 'kg N2O/ kg N'),
        list('Table 13', 'N2O leaching emissions factor for mineral N', f_Nl, 'kg N2O/ kg N'),
        list('Table 14', 'Relative reduction in nitrogen leaching', f_up, 'Fraction'),
        list('Table 15', 'N optimization factor (f_O)', f_O, 'Fraction'),
        list('Table 16', 'Nitrogen content of grain', f_Ng, 'kg N / kg grain'),
        list('Table 17', 'Change in N leachate (Delta.L)', Delta.L, 'kg N / ha / yr'),
        list('Table 18', 'Change in organic-N inputs', Delta.ON, 'kg N / ha / yr')
    )))})
    output$tables.report = renderTable(tables.report())
    
    eq.report = reactive({with(results.full(), rbindlist(list(
        list(Source = 'Eq. 5', Description = 'Change in yield', Value = Delta.Y, Units = 'kg grain / ha / yr'),
        list('Table 15', 'Change in N input from cover crops', Delta.N_A, 'kg N / ha / yr'),
        list('Table 15', 'Change in N input from tillage', Delta.N_B, 'kg N / ha / yr'),
        list('Table 15', 'Change in N input from group-C fertilizer management', Delta.N_C, 'kg N / ha / yr'),
        list('Table 15', 'Change in N input from group-D fertilizer management', Delta.N_D, 'kg N / ha / yr'),
        list('Eq. 10', 'Overall change in N input', Delta.N_tot, 'kg N / ha / yr'),
        list('Eq. 9', 'Change in emissions from N fertilizer production', Delta.CO2_N, 'Mg CO2e / ha / yr'),
        list('Eq. 8', 'Change in indirect N2O emissions', Delta.N2O_i, 'kg N2O / ha / yr'),
        list('Eq. 7', 'Change in direct N2O emissions', Delta.N2O_d, 'kg N2O / ha / yr'),
        list('Eq. 6', 'Change in overall N2O emissions', Delta.N2O, 'Mg N2O / ha / yr'),
        list('Eq. 4', 'Leakage emissions', Delta.CO2_L, 'Mg CO2e / ha / yr'),
        list('Eq. 3', 'Annualized SOC sequestration', Delta.SOC, 'Mg C / ha / yr'),
        list('Eq. 2', 'Risk-adjusted SOC sequestration credit', Delta.CO2_SOC, 'Mg CO2 / ha / yr'),
        list('Eq. 2', 'Overall CO2-reduction credit', Delta.CO2, 'Mg CO2 / ha / yr'),
        list('Eq. 1', 'Overall GHG-reduction credit', Delta.GHG, 'Mg CO2e / ha / yr')
    )))})
    output$eq.report = renderTable(eq.report())
    
    output$barplot = renderPlot({
        ggplot(results.summary(), aes(ind, values)) +
            geom_bar(stat = 'identity', width = 0.5, fill='grey30') +
            geom_hline(aes(yintercept = 0.0), linetype=2, color='grey', size=0.8) +
            coord_flip() +
            scale_y_continuous('Avoided GHG emissions (CO2e / ha / yr)')
    })
})

