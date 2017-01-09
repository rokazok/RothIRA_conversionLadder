library(shiny); library(rhandsontable); library(shinydashboard)#; library(shinyjs)

CSS_stuff <- list(tags$head(
  tags$style(
    ".shiny-progress { 
      position: absolute;
      right: 33%;
      top: 300px;
      width: 100px;
      opacity: 0.85;} 
    .shiny-progress .progress {height: 25px; width: 300px}
    .shiny-progress .progress-text { width: 0%; font-size: 24px; background-color: white; margin-top: -45px; }
    #plot-container { position: relative;  }
    #loading-spinner {
      position: absolute;
      left: 50%;
      top: 50%;
      z-index: -1;
      margin-top: -33px;  /* half of the spinner's height */
      margin-left: -33px; /* half of the spinner's width */
    }
    #plot.recalculating {    z-index: -2;    }
    #loadmessage {
      /* position: fixed; */
      /* top: 0px; */
      /* left: 0px; */
      width: 100%;
      padding: 5px 0px 5px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 100%;
      color: #000000;
      background-color: white; 
      /*z-index: 105;*/
    }
    ")
  ))
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ripple-200px.css"))

myHeader <-  dashboardHeader(title = "Roth IRA Conversion ladder")
mySidebar <-  dashboardSidebar(width = 150, disable = TRUE,
    sidebarMenu(
      #Icons from: http://fontawesome.io/icons/ and http://glyphicons.com/
      menuItem("Ladder", tabName = "ladderTab", icon = icon("money", lib="font-awesome"))#,
      #menuItem("About", tabName = "aboutTab", icon = icon("info", lib="font-awesome"))
    ) #close sidebarMenu
  ) #close dashboardSidebar


#Construct the body rows: frowA1 where A = First tab, 1 = first row
frowA0 <- fluidRow(
  box(title = "IRA Conversion ladder", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      verbatimTextOutput("feedback"),
      verbatimTextOutput("feedback0"),
      verbatimTextOutput("feedback1")
  ))

#Assumptions
frowA1 <- fluidRow(
  column(width = 6,
    box(title = "Assumptions", status = "danger", solidHeader = TRUE, width = NULL, collapsible = TRUE,
        numericInput("age", value = 33L, label="Current age" ),
        numericInput("age.startFamily", value = 999L, label="Year to start family (for HSA)" ),
        sliderInput("year.FI", label= "Year to start financial independence (FI)", min=1L, max=100L, value=11L ),
        sliderInput("year.useRoth", label= "Year to start Roth withdrawals", min=1L, max=100L, value=16L ),
        numericInput(inputId = "n.years", label = HTML("number of years to run calculations"), min=10L, max=100L, value = 30L, step = 1L),
        numericInput("salary", value = 50000L, label="annual salary", step=1000L ),
        numericInput("costOfLiving", value = 10000L, label="annual cost of living", step = 1000L ),
        numericInput("targetFIincome", value = 30000L, label="Target annual FI income (present-value)", step = 1000L ),
        HTML("Annual contributions to or investments made in:"), br(),
        sliderInput("contrib.401k", label= "401k ($18000 limit)", min=0L, max=18000L, value=18000L ),
        sliderInput("contrib.401k.employer", label= "401k employer contribution ($53000 total limit)", min=0L, max=53000L, value=0L ),
        sliderInput("contrib.IRA", label= "total IRA ($5500 limit)", min=0L, max=5550L, value=5500L ),
        sliderInput("contrib.HSA", label= "Single HSA ($3350 limit)", min=0L, max=3350L, value=3350L ),
        sliderInput("family.HSA", label= "Family HSA ($6750 limit)", min=0L, max=6750L, value=6750L ),
        sliderInput("prop.Roth_v_IRA", label= "% of IRA in Roth vs regular", min=0L, max=100L, value=0L ),
        numericInput("contrib.taxable", value = 10000L, label= "taxable investment accounts, i.e. brokerage", min=0L, step = 1000L ),
        helpText("Catch-up contributions?"), br(),
        sliderInput("catchup.401k", label= "catch-up 401k (ages >= 50)", min=0L, max=6000L, value=0L ),
        sliderInput("catchup.IRA", label= "catch-up IRA (ages >= 50)", min=0L, max=1000L, value=0L ),
        sliderInput("catchup.HSA", label= "catch-up HSA (ages >= 55)", min=0L, max=1000L, value=0L ),
        numericInput("i.401k", value = 0L, label="initial 401k balance", min = 0L, step = 1000L ),
        numericInput("i.IRA", value = 0L, label="initial IRA balance", min = 0L, step = 1000L ),
        numericInput("i.HSA", value = 0L, label="initial HSA balance", min = 0L, step = 1000L ),
        numericInput("i.Roth", value = 0L, label="initial Roth balance", min = 0L, step = 1000L ),
        numericInput("i.taxableInvestments", value = 0L, label="initial taxable investments balance", min = 0L, step = 1000L )
  )),
  column(width = 6,
   box(title = "Additional assumptions", status = "danger", solidHeader = TRUE, width = NULL, collapsible = TRUE, collapsed = FALSE,
         numericInput("inflation", value = "0.02", label="Assumed rate of inflation", step = 0.01 ),
         numericInput("roi.mean", value = "0.07", label="Assumed mean rate of return", step = 0.01 ),
         numericInput("roi.sd", value = "0.18", label="Assumed standard deviation of return (check randomized returns below)", step = 0.01 ),
         HTML("<b>Single scenario</b>"),
         checkboxInput("roi.simToggle", label = "Randomized returns?", value = FALSE),
         actionButton("roi.resim", label = "Re-randomize"), br(),
         HTML("<b>Monte Carlo simulations (n=500)</b>"),
         actionButton("roi_SIMZ", label = "Run simulation"),
         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          tags$div(HTML("Calculating..."),id="loadmessage")
         ),
         #Graph of simulated returns. Should do this as a Monte Carlo simulation with outcome probabilities: % negative, % break-even, % sustainable, etc.
         plotOutput("plotROI"),
         helpText("Taxes:"), 
         rHandsontableOutput("hot"),
         numericInput("taxDeduction", value = "6300", label="tax deduction" ),
         helpText("6300 = standard deduction")
     )
   )) #close fluidRow> column > fluidRow

frowA3 <- fluidRow(
  #box(title = "Plot Options", status = "primary", solidHeader = TRUE, width = 4, collapsible = TRUE,
  #    uiOutput("plotA_Options")
  #),
  box(title = "IRA Conversion ladder", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      plotOutput("plotLadder")#, 
      #fluidRow(column( 6, HTML("")), #blank row
      #         column( 3, downloadButton(outputId = "Download_plotA", label="Download Plot") ),
      #         column( 3, selectInput(inputId = "plotA_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
  ))
frowA4 <- fluidRow(
  box(title = "Monte Carlo simulations", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      conditionalPanel(condition = "input.roi_SIMZ == 0",
        HTML("Results will plot here once calculations are finished (~1-2 min)")
      ),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(h1("Calculating..."),id="loadmessage")
      ),
      tags$div(id = "loadmessage", HTML("<div class='uil-ripple-css' style='transform:scale(1);'><div></div><div></div></div>")),
      tags$div(id = "plot-container",
          tags$img(src = "spinner.gif", id = "loading-spinner"),
          plotOutput("plotSIM"),
          #plotlyOutput("plotSIM"),
          plotOutput("plotSIM.roi")
          )
      ))

myBody <-  dashboardBody(
    #useShinyjs(),
    CSS_stuff,
    tabItems(
    tabItem(tabName = "ladderTab",
      frowA1, frowA3, frowA4
      ),
    tabItem(tabName = "aboutTab",
      fluidRow(HTML("filler"))#frowB1, frowB2, frowB3
    ) 
  )
  ) #close tabItems and dashboardBody


dashboardPage(myHeader, mySidebar, myBody)

