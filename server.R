#####################################
###  Roth IRA Conversion ladder   ###
###  R. Okazaki, 2016             ###
#####################################
# server.R
library(shiny); library(shinydashboard); library(rhandsontable); library(DT); library(shinyjs); library(colourpicker)
library(data.table)
library(ggplot2); library(gridExtra); library(scales); library(plotly)#; library(viridis);  library(grid); library(cowplot)
library(dplyr);#

#Declare/upload variables
#  "FOLDER/FILE" the filepath on the server is "//srv/shiny-server/folder/helperFolder"
  #Note: filenames are case-sensitive!
#allvar.labels <-  readRDS("helper/allvar.labels.Rds") #Load variable graphing labels. Opposite of saveRDS()


#IRA conversion ladder

#Notes: 
#Returns on taxable Savings will need to be divided between regular income and dividend/capital gain income (taxed at lower rate).
#Decision trees could be added: i.e. prioritize living expenses, 401k, then HSA, then Roth, then taxable
#Adaptive calculations- change variables via rhandsontable and allow app to cascade the rest of the calculations.


shinyServer(function(input, output, session) {

##############################
###  Initial shiny setup   ###
##############################
  #Tax brackets
  taxBrackets.start <- data.frame(taxBracket = 1:7, taxRate = c(0.1, .15, .25, .28, .33, .35, .39), taxBracMin = c(0, 9275, 37650, 91150, 190150, 413350, 415050))
  taxBrackets.start$prevBracTax <- c(0, cumsum(head(taxBrackets.start$taxRate, -1) * diff(taxBrackets.start$taxBracMin)))
  
  #Tax Brackets rhandsontable
  hv <- reactiveValues(DF = taxBrackets.start) #hv = hot (hands-on-table) values
  #  observe({ 
  #    if(is.null(hv[["DF"]])) {      #initial value
  #     hv[["DF"]] <- taxBrackets.start
  #   }
  # })
  #Read input rhandsontable 
  observe({
    if(!is.null(input$hot)) {
      hv[["DF"]] <- hot_to_r(input$hot)
    }
  })
  taxBrackets <- reactive({
    dd <- hot_to_r(input$hot)
    return(dd)
    })
  
  #Write output rhandsontable
  output$hot <- renderRHandsontable({
    tempdf <- hv[["DF"]]
    if(!is.null(tempdf)) {
      rhandsontable(tempdf)
    }
  })
  
  # Create a Progress object. Ref: http://shiny.rstudio.com/gallery/progress-bar-example.html
  progress <- shiny::Progress$new()
  progress$set(message = "Running simulation", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a closure to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
  
  #roi.vector <- reactive(if(input$roi.simToggle == TRUE) { rnorm(input$n.years + 1, input$roi.resim^0*input$roi.mean, input$roi.simToggle* input$roi.sd)} else
  #  {rep(input$roi.mean, input$n.years + 1) })#button^0 allows for reactive updating without long observe() statements. if/else isnt needed normally, but added here to reduce # of calculations for the reactive variable.
  roi.vector <- reactive(rnorm(input$n.years + 1, input$roi.resim^0*input$roi.mean, input$roi.simToggle* input$roi.sd))
  
   #Create table
  dt <- reactive({

  zz <- data.frame(Year = seq(0, input$n.years), Age = seq(input$age, input$age + input$n.years))
  #Round 1 calculations: vector-based
  #Create True/False (as 1/0) columns for FI and ladder withdrawals
  zz[, c("FI", "start.ladder")] <- c(fi = as.numeric(zz$Year >= input$year.FI), strt = as.numeric(zz$Year >= input$year.useRoth -5) )
  #Future Value (FV) multiplier = Principal * (1 + rate) ^ time   #zz[, FV := 1*((1+input$inflation)^Year)]
  #Future value of salary and FI income zz[, c("FV.FIincome", "FV.salary") := list( FV*input$targetFIincome, (1-FI)*FV*input$salary)]
  #Simulate annual roi.  zz[, roi := rnorm(.N, input$roi.mean, input$roi.simToggle* input$roi.sd)]
  zz$roi <- roi.vector()#rnorm(nrow(zz), input$roi.resim^0*input$roi.mean, input$roi.simToggle* input$roi.sd)
  zz <- zz %>% mutate(FV = 1*((1+input$inflation)^Year),
                      FV.FIincome = FV*input$targetFIincome,
                      FV.salary = (1-FI)*FV*input$salary,
                      FV.col = FV*(FI == 0)*input$costOfLiving,
                      #roi = 0.07,
                      targetCntb.taxdef = FV*(FI == 0) * (input$contrib.401k + (Age >= 50) * input$catchup.401k + input$contrib.401k.employer + input$contrib.IRA + (Age >= 50) * input$catchup.IRA),
                      cntb.taxdef = (FV.salary - FV.col >= targetCntb.taxdef) * targetCntb.taxdef + (FV.salary - FV.col < targetCntb.taxdef)*(FV.salary - FV.col),
                      #FV*(FI == 0) * (1-0) * (contrib.IRA + (Age >= 50) * catchup.IRA),
                      targetCntb.HSA = FV*(FI == 0) * ((Age < input$age.startFamily)*input$contrib.HSA + (Age >= 55) * input$catchup.HSA + (Age >= input$age.startFamily)*(input$family.HSA)),
                      cntb.HSA = (FV.salary - FV.col - cntb.taxdef >= targetCntb.HSA)*targetCntb.HSA + (FV.salary - FV.col - cntb.taxdef < targetCntb.HSA) * (FV.salary - FV.col - cntb.taxdef),
                      targetCntb.Roth = FV*(FI == 0) * input$prop.Roth_v_IRA * (input$contrib.IRA + (Age >= 50) * input$catchup.IRA), #0 is proportion in Roth vs reular IRA
                      cntb.Roth = (FV.salary - FV.col - cntb.taxdef - cntb.HSA >= targetCntb.Roth) * targetCntb.Roth + (FV.salary - FV.col - cntb.taxdef - cntb.HSA < targetCntb.Roth) * (FV.salary - FV.col - cntb.taxdef - cntb.HSA),
                      targetCntb.taxable = FV*(FI == 0) * (input$contrib.taxable),
                      cntb.targetLadder = start.ladder * FI * FV.FIincome * (1+input$inflation)^5,
                      roi.taxable = 0,
                      roi.Roth = 0,
                      roi.HSA = 0,
                      contrib.ladder = 0,
                      bal.taxdef = input$i.401k + input$i.IRA, 
                      bal.Roth = input$i.Roth, 
                      bal.HSA = input$i.HSA, 
                      bal.taxable = input$i.taxableInvestments
                    )
  return(zz)
  })

  ladder <- function(data, taxes = NULL) {
    if(is.null(taxes)) { yourTaxBrackets <- taxBrackets.start } else {yourTaxBrackets <- taxes }
    require(dplyr)
    data[, c("draw.taxable","draw.Roth","taxableIncome","taxBracket","taxRate","taxBracMin","prevBracTax","taxLiab","taxOwed", "cntb.taxable", "netIncome","taxRate.actual")] <- NA #dummy values
    for(i in 2:nrow(data)) {
      d1 <- data[i - 1L,]
      d2 <- data[i,]
      #Assignments referencing previous row. Newly created variables in the same command are prefixed zz.
      d2 <- d2 %>% mutate(roi.taxable = roi * d1$bal.taxable,
                          contrib.ladder = (d1$bal.taxdef >= cntb.targetLadder) * cntb.targetLadder + (d1$bal.taxdef < cntb.targetLadder)* d1$bal.taxdef,
                          roi.Roth = roi * d1$bal.Roth,
                          roi.HSA = roi * d1$bal.HSA,
                          bal.HSA = d1$bal.HSA*(1+roi) + cntb.HSA*(1+roi/2),
                          draw.taxable = (FI*(FV.FIincome) >= d1$bal.taxable)* d1$bal.taxable + (FI*(FV.FIincome) < d1$bal.taxable)* FI*(FV.FIincome),
                          draw.Roth = (Year >= input$year.useRoth)*(FV.FIincome - draw.taxable),
                          taxableIncome = FV.salary + contrib.ladder + roi.taxable - (cntb.taxdef + cntb.HSA),
                          bal.taxdef = (d1$bal.taxdef - contrib.ladder)*(1+roi) + (cntb.taxdef)*(1+roi/2),
                          bal.Roth = d1$bal.Roth*(1+roi) + contrib.ladder + cntb.Roth*(1+roi/2) - draw.Roth,
                          taxBracket = findInterval(taxableIncome, yourTaxBrackets$taxBracMin*FV, left.open = TRUE, all.inside = TRUE), #taxBrackets.start$taxBracMin
                          taxRate = yourTaxBrackets[yourTaxBrackets$taxBracket == taxBracket, "taxRate"],
                          taxBracMin = yourTaxBrackets[yourTaxBrackets$taxBracket == taxBracket, "taxBracMin"],
                          prevBracTax = yourTaxBrackets[yourTaxBrackets$taxBracket == taxBracket, "prevBracTax"], #each column specified because left_join doesnt work within the function. left_join(taxBrackets, by = c("taxBracket" = "taxBracket")) %>%
                          taxLiab = (taxableIncome - taxBracMin*FV)*taxRate + prevBracTax*FV, 
                          taxOwed = (taxLiab > input$taxDeduction*FV)*(taxLiab - input$taxDeduction*FV),
                          cntb.taxable = (FV.salary - FV.col - cntb.taxdef - cntb.HSA - cntb.Roth - taxOwed >= targetCntb.taxable) * targetCntb.taxable + (FV.salary - FV.col - cntb.taxdef - cntb.HSA - cntb.Roth - taxOwed < targetCntb.taxable) * (FV.salary - FV.col - cntb.taxdef - cntb.HSA - cntb.Roth - taxOwed),
                          cntb.taxable = (cntb.taxable > 0)*cntb.taxable,
                          bal.taxable = d1$bal.taxable - draw.taxable + cntb.taxable*(1+roi/2) + roi.taxable,
                          #Net income as dispoable, after-tax income from all sources, i.e. not simply taxable income - tax.
                          netIncome = FV.salary + roi.taxable + draw.taxable + draw.Roth -(cntb.taxdef + cntb.HSA + cntb.Roth + cntb.taxable) - taxOwed,
                          taxRate.actual = taxOwed / taxableIncome
      )
      #d2 <- d2[]
      #return(d2) #If returning d2, which is a single-row calculation
      data[i,] <- d2[]
    }
    data[, grep("bal", names(data))] <- round(data[, grep("bal", names(data))], 0)   #Round to 2 digits
    data$stash <- data %>% select(bal.taxdef, bal.Roth, bal.HSA, bal.taxable) %>% rowSums()  #Total stash
    return(data)
   }
  dt1 <- reactive({    ladder(dt(), taxBrackets())  })
  
  #When roi_SIMZ action button is clicked, run Monte Carlo simulations
  balz <- isolate(c(names(dt())[grep("bal", names(dt()))], "stash"))
  dt2 <- eventReactive(input$roi_SIMZ, {
    withProgress(message = 'Running simulations', value = 0, {
    zz2 <- lapply(1:500, function(i) { 
      incProgress(1/500, detail = paste(i, "of 500")) #updateProgress()
      yy <-isolate( dt() )
      yy$roi <- rnorm(input$n.years + 1, input$roi.mean, input$roi.sd)
      ladder(yy, isolate(taxBrackets())) 
    })
    })
    xx <- array(unlist(zz2), c(dim(zz2[[1]]), length(zz2)), dimnames =list(NULL,names(zz2[[1]]),paste0("iteration", 1:length(zz2)) ))
    x1 <- data.frame(apply(xx, c(1,2), median))
    x2 <- data.frame(apply(xx[,"roi",], c(1), sd)); names(x2) <- "roi.sd"
    balz.newNames <- sub("bal.", "", balz)
    x3 <- data.frame(apply(xx[,balz,], c(1,2), quantile, probs = c(0.05))); names(x3) <- paste0("q05.", balz.newNames)
    x4 <- data.frame(apply(xx[,balz,], c(1,2), quantile, probs = c(0.25))); names(x4) <- paste0("q25.", balz.newNames)
    x5 <- data.frame(apply(xx[,balz,], c(1,2), quantile, probs = c(0.75))); names(x5) <- paste0("q75.", balz.newNames)
    x6 <- data.frame(apply(xx[,balz,], c(1,2), quantile, probs = c(0.95))); names(x6) <- paste0("q95.", balz.newNames)
    df <- cbind(x1, x2, x3, x4, x5, x6)
    return(df)
    }
  )
    
#  output$feedback <- renderPrint( list(taxBrackets(), names(dt1()), dt1()[, c("taxableIncome", "cntb.taxable", "taxRate", "taxOwed", "taxRate.actual", "stash")] ) )
#  output$feedback0 <- renderPrint( str(reactiveValuesToList(input))   ) #useful for examining all the inputs
#  output$feedback0 <- renderPrint( list(str(dt2() )  ) )
#  output$feedback1 <- renderPrint(list( roi.vector(), input$roi.resim, input$roi_SIMZ) )
  LS <- 1.4 #line size for ggplots
  #logScaleOption <- reactive(if(input$logScale == TRUE) { list(scale_y_log10()) } else {NULL}) #Ugly, not useful. Here for reference. Didn't work unless input$checkbox was in the renderPlot() statement
  output$plotLadder <- renderPlot(ggplot(dt1(), aes(Year))+ geom_rect(aes(xmin = input$year.useRoth - 5, xmax = input$year.useRoth, ymin = -Inf, ymax = Inf), fill = "grey80") + geom_line(aes(y= bal.Roth, color = "Roth"), size = LS)  + geom_vline(xintercept = input$year.FI) + geom_line(aes(y = bal.taxdef, color = "tax defered"), size = LS) + geom_line(aes(y = bal.taxable, color = "taxable"), size = LS) + #scale_y_log10(breaks = 10^(0:7)) +
                                  geom_line(aes(y = stash, color = "total stash"), size = LS) + geom_line(aes(y= FV.FIincome/0.04, color = "self-sustaining"), lty = 2, color = "black") + theme_classic() + scale_x_continuous(breaks = seq(0,100,by=2)) + 
                                    labs(y = "$", colour = "Fund source") + scale_y_continuous(labels = comma)
                      )
  output$plotROI <- renderPlot(ggplot(dt(), aes(Year, roi*100))+ geom_hline(yintercept = input$roi.mean*100, lty = 3)  + geom_line(size = 1.4) + labs(y = "Investment return, %") + theme_classic()  )

  plotSIM <- reactive({ggplot(dt2(), aes(Year))+ geom_ribbon(aes(ymin = q05.stash, ymax = q95.stash, fill = "90%"), alpha =0.4)+ geom_ribbon(aes(ymin = q25.stash, ymax = q75.stash, fill = "50%"), alpha =0.4) + geom_line(aes(y= bal.Roth, color = "Roth"), size = LS)  + geom_vline(xintercept = input$year.FI) + geom_line(aes(y = bal.taxdef, color = "tax defered"), size = LS) + geom_line(aes(y = bal.taxable, color = "taxable"), size = LS) + #scale_y_log10(breaks = 10^(0:7)) +
                        geom_line(aes(y = stash, color = "total stash"), size = LS) + geom_line(aes(y= FV.FIincome/0.04, lty = "self-sustaining\ntotal stash\n(4% rule)"), color = "black") + theme_classic() + scale_x_continuous(breaks = seq(0,100,by=2)) + 
                        labs(y = "$", title = "Median of Monte Carlo simulations", colour = "Fund source") + scale_fill_manual(name = "Total, probabilities", values = c("50%" = "mediumpurple4", "90%" = "mediumpurple4"), guide = guide_legend(override.aes=list(alpha=c(0.8, 0.4)))) +
                        scale_linetype_manual(name = NULL, values =c("self-sustaining\ntotal stash\n(4% rule)" = 2)) +
                        scale_y_continuous(labels = comma) + guides(colour = guide_legend(order=1), lty = guide_legend(order = 2))
                        
              })
  output$plotSIM <- renderPlot(  plotSIM()  )
  output$plotlySIM <- renderPlotly(  plotly::ggplotly(p = plotSIM() + aes(label = roi, label2 = q95.stash, label3 = stash, label4 = q05.stash),
                                                    tooltip = c("x","label", "label2", "label3", "label4") )
                                )
  output$plotSIM.roi <- renderPlot(ggplot(dt2(), aes(Year))+ geom_ribbon(aes(ymin = (roi - roi.sd)*100, ymax = (roi + roi.sd)*100), fill = "grey60", alpha =0.5)+ geom_line(aes(y= 100*roi), colour = "black", size = LS)+ theme_classic() + scale_x_continuous(breaks = seq(0,100,by=2)) + 
                                 labs(y = "%", title = "Median simulated annual return and \u00B1 SD simulated return", colour = NULL, fill = NULL) + geom_hline(yintercept = 0, lty= 1)+ geom_hline(yintercept = isolate(input$roi.mean)*100, lty=3) #\u00B1 for plus/minus symbol
  )
}) #Close shinyServer(function() {