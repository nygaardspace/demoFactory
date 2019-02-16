## app.R ##
library(shinydashboard)
library(curl)
library(tidyverse)
library(plyr)
library(DT)
library(formattable)
library(knitr)
library(kableExtra)
library(TTR)
library(quantmod)
library(reshape)
library(shinycssloaders)
library(scales)
library(lubridate)
library(ggrepel)
library(kernlab)
library(shinythemes)
library(htmltools)
library(bsplus)
library(plotly)
library(gapminder)
library(radarchart)
library(ggalt)
library(shinyjs)
library(treemapify)

con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
source(con)
close(con)

source('global.R')


ui <- navbarPage( title = div(
  div(
    id = "img-id",
    HTML("<img id='logoimage' src='https://norquant.no/images/logo.png' width='160' alt='norquant logo'>")
  ),
  "Reitan Kapital"
),theme = shinytheme("darkly"),
          #First main panel of three for Stock Strategy
          
          tabPanel("Stock Strategy",
                              fluidRow(
                                column(12,
                                       tabsetPanel(
                                              tabPanel("Universe Overview",
                                                       fluidRow(
                                                         column(12,
                                                                h3("Investing Universe"),
                                                                h6(paste("The universe consist of",nrow(ALL),"stocks from",length(unique(ALL$`Exchange Name`)), "different exchanges.")))),
                                                         fluidRow(
                                                         column(6,plotOutput("universeTot1",height = "750px")),
                                                         column(6,plotOutput("universeTot2",height = "750px")))),
                                              tabPanel("Norden Overview",
                                                       fluidRow(
                                                         column(12,
                                                                h3("Investing Universe Norden"))),
                                                       fluidRow(
                                                         column(6,plotOutput("nordenTot1",height = "750px")),
                                                         column(6,plotOutput("nordenTot2",height = "750px")))),
                                              tabPanel("Select", 
                                                       fluidRow(
                                                         column(12,
                                                                h3("Portfolio"),
                                                                shiny::numericInput(inputId = 'n',min = 1, label = 'Select no. of Stocks in Portfolio',value = 2))),
                                                       fluidRow(
                                                         column(4,
                                                           checkboxGroupInput(inputId = "countries", "Markets:",
                                                                              selected = c("Norway","Denmark","Finland","Sweden","Netherlands","Belgium","France","Canada","Italy","Switzerland","Brazil","South Africa","Egypt","Morocco","Russia","Vietnam", "Portugal","Hong Kong", "Germany","Japan","United Kingdom", "United States of America"),
                                                                              c("Norway","Sweden","Denmark", "Finland","Netherlands","Germany","United Kingdom","France","Italy","Belgium","Portugal","Russia","Switzerland","Brazil","South Africa","Egypt","Morocco","Vietnam","Hong Kong","Japan","Canada","United States of America"))
                                                         ),
                                                         column(4,
                                                           checkboxGroupInput(inputId = "sektors", "Sectors included:",
                                                                              selected = c("Energy","Materials","Industrials","Consumer Discretionary",
                                                                                           "Consumer Staples","Health Care",
                                                                                           "Financials","Information Technology Consumer Staples","Communication Services","Utilities","Real Estate"),
                                                                              c("Energy" = "Energy",
                                                                                "Materials" = "Materials",
                                                                                "Industrials" = "Industrials",
                                                                                "Consumer Discretionary" = "Consumer Discretionary",
                                                                                "Consumer Staples" = "Consumer Staples",
                                                                                "Health Care" = "Health Care",
                                                                                "Financials" = "Financials",
                                                                                "Information Technology Consumer Staples" = "Information Technology Consumer Staples",
                                                                                "Communication Services" = "Communication Services",
                                                                                "Utilities" = "Utilities",
                                                                                "Real Estate" = "Real Estate"))
                                                         ),
                                                         column(4,
                                                                # Input: Simple integer interval ----
                                                                sliderInput("momentum", HTML(paste0("Momentum <a href='https://norquant.no/blog/post/sats-pa-momentum/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100) %>%
                                                                  bs_embed_tooltip(title = "Momentum-faktoren viser at aksjer som har gjort det best de siste 3 til 12 månedene, gjennomsnittlig fortsetter å gjøre det best i månedene fremover.", placement = "right"),
                                                                # Input: Simple integer interval ----
                                                                sliderInput("volatilitet", HTML(paste0("Low risk <a href='https://norquant.no/blog/post/volatility-og-risiko/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100) %>%
                                                                  bs_embed_tooltip(title = "Aksjer med høyest risiko, det vil si de som har vist de største svingningene, gjør det gjennomsnittlig dårligere enn aksjer som har svingt minst.", placement = "right"),
                                                                # Input: Simple integer interval ----
                                                                sliderInput("value", HTML(paste0("Value <a href='https://norquant.no/blog/post/verdi-faktoren/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100) %>%
                                                                  bs_embed_tooltip(title = "Verdi-faktoren er  aksjekurs delt på fundamental verdi.", placement = "right"),
                                                                # Input: Simple integer interval ----
                                                                sliderInput("size", HTML(paste0("Size <a href='https://norquant.no/blog/post/storrelse/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100) %>%
                                                                  bs_embed_tooltip(title = "Når vi tenker på hvilke aksjer som det er best å investere i så er det lett å tro at det må være de mest kjente, største selskapene på børsen: Apple og Microsoft, i USA for eksempel, eller Statoil, DNB og Telenor her i Norge. Dette er selvfølgelig selskap som har gjort det veldig bra, og som vi kunne ha tjent mye på hvis vi hadde kjøpt dem for lenge siden og beholdt dem.", placement = "right"),
                                                                # Input: Simple integer interval ----
                                                                sliderInput("quality", HTML(paste0("Quality <a href='https://norquant.no/blog/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100),
                                                                # Input: Simple integer interval ----
                                                                sliderInput("liquidity", HTML(paste0("Liquidity <a href='https://norquant.no/blog/'>",icon("info-sign", lib = "glyphicon"),"</a>")),
                                                                            min = 0, max = 100,
                                                                            value = 100) 
                                                         )
                                                       )
                                                       ),
                                              tabPanel("Table", 
                                                       DT::dataTableOutput("mytable1"),
                                                       checkboxGroupInput("show_vars1", "Columns to show:",
                                                                          names(ALL), selected = names(ALL)[c(1,7,10,16)],inline=TRUE))
                                       )
                                )
                              )
                 ),
                 tabPanel("ETF Strategy",
                          fluidRow(
                            column(12,class="inputMenuDT",
                                   tabBox(width = 12,
                                          tabPanel("Info Table", checkboxGroupInput(inputId = "includeETF", "Choose your ETFs:",
                                                             selected = c("SPY","QQQ.O","IWM","IYR","EEM","EFA","TLT.O"),
                                                             c("SPY","QQQ.O","IWM","IYR","EEM","EFA","TLT.O"), inline = TRUE),
                                          tableOutput("tableETF")),
                                          tabPanel("Backtest Graph",
                                                   splitLayout(
                                                   dateInput("startdate", "Start Date:", value = "2012-02-29"),
                                                   shiny::numericInput(inputId = 'top_n',min = 2, label = 'No. of ETFs',value = 2),
                                                   shiny::numericInput(inputId = 'noMom',min = 2, label = 'No. of months mometum lookback',value = 6),
                                                   shiny::numericInput(inputId = 'noVol',min = 2, label = 'No. of days volatility lookback',value = 21)),
                                                   plotOutput("backtest",height = "750px") %>% withSpinner(color="#0dc5c1")),
                                          tabPanel("Equal Weight Trades", tableOutput("historyEQ") %>% withSpinner(color="#0dc5c1") ),
                                          tabPanel("Trygg Weight History", tableOutput("historyTR") %>% withSpinner(color="#0dc5c1") ),
                                          tabPanel("Modig Weight History", tableOutput("historyMD") %>% withSpinner(color="#0dc5c1") )
                                   )
                            )
                            
                          )
                 ),
                 tabPanel("Fund Analyzer",
                          fluidRow(
                            column(12,class="inputMenuDT",
                                   tabBox(width = 12,
                          tabPanel("Graph Excess return",HTML("<img src='graph.png' width='750'>"),
                                   HTML("<img src='table.png' width='750'>")),
                          tabPanel("Portfolio Visualizer",htmlOutput("frame") %>% withSpinner(color="#0dc5c1")))))),
                            tags$head(
                              tags$script(src = "java.js"),
                              tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                              tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans:300,400,400i,600,700")    
                            )
                 )





server <- function(input, output) {

  myReactives <- reactiveValues(stocks_df = NULL, etf_data = NULL)  
  observe( myReactives$etf_data <- foo3(etf_data,input$includeETF))
  observe( myReactives$etf_ts <- foo3(etf_ts,input$includeETF))
  observe( myReactives$stocks_df <- foo2(input$n, input$sektors, input$countries,
                                         input$momentum, input$volatilitet, input$value,
                                         input$size, input$liquidity, input$quality))

  
  
  output$tableSummary <- renderTable(algin = 'c', bordered = T,
    selectionParam <- data.frame(Ratio_Numer_of_Stocks = c(input$n/nrow(ALL)), 
                                 MarketCap_Ratio = c(sum(na.omit(as.numeric(myReactives$stocks_df$`Company Market Cap`)))/sum(na.omit(as.numeric(ALL$`Company Market Cap`)))),
                                 Revenue_Ratio = c(sum(na.omit(as.numeric(na.omit(myReactives$stocks_df$Revenue))))/sum(na.omit(as.numeric(ALL$Revenue)) )))
  )
  
  output$tableETF <- renderTable(
    algin = 'c', bordered = T,na.omit(myReactives$etf_data)[,c(1,8,11,12,14,18,19,21,27)]
  )

  
  output$selected_n <- renderText({ paste("You have selected ",input$n," stocks.") })
  
  
  #UniversePie Output
  output$universeTot1 <- renderPlot({

    stocks_df <- ALL
    stocks_df <- stocks_df %>% group_by(`GICS Sector Name`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Sector","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
      arrange(desc(Sector)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    
    ggplot(stocks_df, aes(x=factor(1),y = prop ,group = factor(Sector), label=as.character(stocks_df$Sector))) + 
      geom_col(aes(fill = factor(Sector)),width = 1) + 
      labs(caption = "") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values  = color_palette) +
      geom_label_repel(size = 4,label.padding = unit(0.5, "lines"),aes(y = prop  , label = paste0(stocks_df$Sector," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  
  #UniversePie Output
  output$universeTot2 <- renderPlot({
    
    stocks_df <- ALL
    stocks_df <- stocks_df %>% group_by(`Country of Exchange`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Country","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
      arrange(desc(Country)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    
    ggplot(stocks_df, aes(x=factor(1),y = prop ,group = factor(Country), label=as.character(stocks_df$Country))) + 
      geom_col(aes(fill = factor(Country)),width = 1) + 
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values  = color_palette) +
      geom_label_repel(size = 4,label.padding = unit(0.5, "lines"),aes(y = prop  , label = paste0(stocks_df$Country," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  #UniversePie Output
  output$nordenTot1 <- renderPlot({
    
    stocks_df <- subset(ALL, ALL$`Country of Exchange` %in% c("Norway","Sweden","Finland","Denmark"))
    stocks_df <- stocks_df %>% group_by(`GICS Sector Name`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Sector","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
      arrange(desc(Sector)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    
    ggplot(stocks_df, aes(x=factor(1),y = prop ,group = factor(Sector), label=as.character(stocks_df$Sector))) + 
      geom_col(aes(fill = factor(Sector)),width = 1) + 
      labs(caption = "") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values  = color_palette) +
      geom_label_repel(size = 4,label.padding = unit(0.5, "lines"),aes(y = prop  , label = paste0(stocks_df$Sector," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  
  #Norden Output
  output$nordenTot2 <- renderPlot({
    
    stocks_df <- subset(ALL, ALL$`Country of Exchange` %in% c("Norway","Sweden","Finland","Denmark"))
    stocks_df <- stocks_df %>% group_by(`Country of Exchange`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Country","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
      arrange(desc(Country)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    
    ggplot(stocks_df, aes(x=factor(1),y = prop ,group = factor(Country), label=as.character(stocks_df$Country))) + 
      geom_col(aes(fill = factor(Country)),width = 1) + 
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values  = color_palette) +
      geom_label_repel(size = 4,label.padding = unit(0.5, "lines"),aes(y = prop  , label = paste0(stocks_df$Country," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  #UniversePie Output
  output$universeBransje <- renderPlot({
    stocks_df <- myReactives$stocks_df
    stocks_df$universe <- "selection"
    stocks_df <- rbind(ALL, stocks_df)
    stocks_df <- stocks_df %>% group_by(`GICS Sector Name`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Sector","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
                          arrange(desc(Sector)) %>%
                  dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    
    ggplot(stocks_df, aes(x=factor(1),y = prop ,group = factor(Sector), label=as.character(stocks_df$Sector))) + 
      geom_col(aes(fill = factor(Sector)),width = 1) + 
      xlab('') +
      ylab('') +
      labs(caption = "Sector breakdown is based on the GICS coding system.") +
      coord_polar(theta = "y") +
      scale_fill_manual(values  = color_palette) +
      ggtitle("Sector breakdown Index vs Portfolio") +
      facet_wrap(~universe) +
      geom_label_repel(size = 4,aes(y = prop  , label = paste0(stocks_df$Sector," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  #UniversePie Output
  output$universeCountry <- renderPlot({
    stocks_df <- myReactives$stocks_df
    stocks_df$universe <- "selection"
    stocks_df <- rbind(ALL, stocks_df)
    stocks_df <- stocks_df %>% dplyr::group_by(`Country of Exchange`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    names(stocks_df) <- c("Country","universe","counts")
    stocks_df <- stocks_df %>% group_by(universe) %>% 
      arrange(desc(Country)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    print(stocks_df)
    
    ggplot(stocks_df, aes(x=factor(1),y = prop,group = factor(stocks_df$Country),label = as.character(stocks_df$Country))) + 
      geom_col(aes(fill = factor(Country)),width = 1) + 
      xlab('') +
      ylab('') +
      labs(caption = "Country breakdown") +
      coord_polar(theta = "y") +
      scale_fill_manual(values  = color_palette) +
      ggtitle("Country breakdown Index vs Portfolio") +
      facet_wrap(. ~ universe) +
      geom_label_repel(size = 4, aes(y = prop  , label = paste0(stocks_df$Country," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  #ETF PLOT
  output$overviewETF <- renderPlot({
    #stocks_df <- myReactives$stocks_df
    #stocks_df$universe <- "selection"
    #stocks_df <- rbind(ALL, stocks_df)
    #stocks_df <- stocks_df %>% dplyr::group_by(`Country of Exchange`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    #names(stocks_df) <- c("Country","universe","counts")
    #stocks_df <- stocks_df %>% group_by(universe) %>% 
    #  arrange(desc(Country)) %>%
    #  dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    #print(stocks_df)
    etf_temp <- subset(etf_data, etf_data$Instrument == "SPY")
    ggplot(etf_temp, aes(x=factor(1),y = `Percentage of Fund Assets`,group = factor(etf_temp$Instrument),label = as.character(etf_temp$`Holding RIC`))) + 
      geom_col(aes(fill = factor(etf_temp$`Holding Name`)),width = 1) + 
      xlab('') +
      ylab('') +
      labs(caption = "Country breakdown") +
      coord_polar(theta = "y") +
      scale_fill_manual(values  = color_palette) +
      ggtitle("Country breakdown Index vs Portfolio") +
      geom_label_repel(size = 5, aes(y = `Percentage of Fund Assets`  , label = paste0(etf_temp$`Holding Name`," (",`Percentage of Fund Assets`,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  #ETF Tree
  output$treeETF <- renderPlot({
    #stocks_df <- myReactives$stocks_df
    #stocks_df$universe <- "selection"
    #stocks_df <- rbind(ALL, stocks_df)
    #stocks_df <- stocks_df %>% dplyr::group_by(`Country of Exchange`,universe) %>% dplyr::summarize(counts = sum(as.numeric(`Company Market Cap`), na.rm = TRUE))
    #names(stocks_df) <- c("Country","universe","counts")
    #stocks_df <- stocks_df %>% group_by(universe) %>% 
    #  arrange(desc(Country)) %>%
    #  dplyr::mutate(prop = round(counts*100/sum(counts), 1)) 
    #print(stocks_df)

    ggplot(etf_data, aes(area = etf_data$`Percentage of Fund Assets`, fill = etf_data$Instrument, label = etf_data$`Holding Name`,
                    subgroup = etf_data$Instrument)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      scale_fill_manual(values  = color_palette) +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
      theme(legend.position="none") 
    
  }, bg="transparent", execOnResize = TRUE)
  
  
  #UniversePie Output
  output$CntDetailPlot <- renderPlot({
    stocks_df <- ALL
    stocks_df <- subset(stocks_df, stocks_df$`Country of Exchange` == input$selectCntDet)
    stocks_df <- stocks_df[!duplicated(stocks_df), ]

    stocks_df <- stocks_df %>% group_by(universe) %>% 
      dplyr::mutate(prop = round(`Company Market Cap`*100/sum(`Company Market Cap`), 1))  %>% 
      dplyr::top_n(5,prop) %>%
      arrange(desc(prop))
    print(stocks_df)
    
    ggplot(stocks_df, aes(x=stocks_df$`Company Common Name`,y = prop,label = as.character(stocks_df$`Company Common Name`))) + 
      geom_col(aes(fill = factor(`Company Common Name`)),width = 0.5, color = "white") + 
      xlab('') +
      ylab('') +
      coord_flip() + 
      labs(caption = "Country breakdown") +
      scale_fill_manual(values  = sample(color_palette,nrow(stocks_df),replace = T)) +
      ggtitle(paste("Sector breakdown Index vs Portfolio for",input$selectCntDet)) +
      geom_label_repel(size = 4, aes(y = prop  , label = paste0(stocks_df$`Company Common Name`," (",prop,"%)")), color = "black", position = position_stack(vjust=0.5)) +
      theme_void() +
      theme(legend.title = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.background = element_rect(fill = '#222222', colour = '#222222'),            
            panel.background = element_rect(fill = '#222222', colour = '#222222'),
            strip.text = element_blank(),strip.background = element_blank(),
            legend.position="none") 
  }, bg="transparent", execOnResize = TRUE)
  
  
  #UniversePie Output
  output$volPlot <- renderPlot({
    ALL %>%
      ggplot( aes(`Volatility - 25 days`, `Volatility - 250 days`,label=`Company Common Name`,color=`Country of Exchange`)) +
      geom_point() +
      scale_fill_manual(values  = color_palette) +
      geom_encircle(aes(group=`Country of Exchange`,colour=`Country of Exchange`)) +
      scale_x_log10() +
      theme_bw()
    
    #ggplotly(p)
  })
    
  #Graph Output
  output$plotGraph <- renderPlotly({
    stocks_df <- myReactives$stocks_df
    print("hello1")
    data = subset(eikon_data,Instrument == input$graphInput)
    data_xts <- mainEnv[[input$graphInput]]
    x_tekst <- paste(momentum(data_xts, 21, 252)[c(3)])
    xmin <- index(data_xts[nrow(data_xts) - 252])
    xmax <- index(data_xts[nrow(data_xts - 21)])
    y1 <- as.numeric(data_xts[nrow(data_xts) - 252,3])
    y2 <- as.numeric(data_xts[nrow(data_xts) -21,3])
    print(class(xmin))
    print(class(xmax))
    data_info = subset(stock_data,Instrument == input$graphInput)
    
    ###LONG MOMENTUM
    p <- ggplot(data, aes(x=TIMESTAMP,y=CLOSE,group=Instrument))  +
    geom_line(size=0.8,color="#A1D6E2")  +
    scale_x_date(breaks = date_breaks("1 year"), labels=date_format("%Y") ) +
    geom_segment(aes(x = xmin, y = y1, xend = xmax, yend = y2), arrow = arrow(length = unit(0.07, "npc"),type='closed',angle = 28),  show.legend = F,color="#FF6F00",alpha=0.3) +
    annotate("text", color="#011A27",size=6, x = xmin, y = y1+(y2-y1)/2, label = x_tekst) +
    ggtitle(data_info$`Company Common Name`) + xlab("") + ylab("") +
      scale_fill_manual(values  = color_palette) +
    scale_y_continuous(breaks=pretty_breaks(n=10), sec.axis = dup_axis()) +
    theme(axis.text=element_text(size=10),
                             axis.title=element_text(size=15,face="bold"),
                             plot.background = element_rect(fill = "#3CA2B0"),
                             panel.background = element_rect(fill = "white", colour = "grey50"),
                             panel.grid.major.y = element_line(colour = "grey30", size=0.1),
                             panel.grid.major.x = element_blank(),
                             plot.title = element_text(hjust = 0.5, color="white", size=18),
                             plot.margin=unit(c(0.6,0.6,0.9,0.8),"cm"),
                             axis.text.x = element_text(face="bold", color="white"),
                             axis.text.y = element_text(face="bold", color="white"),
                             axis.ticks = element_blank(),
                             panel.border = element_rect(fill=NA,color="#42A3B0", size=0.5,linetype="solid"))
    ggplotly(p)
  })
  
  #Slope return
  output$slopeReturn <- renderPlot({
    # prep data
    df <- myReactives$stocks_df[,c(7,18,19)]
    left_label <- paste(df$`Company Common Name`, df$`3 Month Total Return`,sep=", ")
    right_label <- paste(df$`Company Common Name`, df$`6 Month Total Return`,sep=", ")
    df$class <- ifelse((df$`6 Month Total Return` - df$`3 Month Total Return`) < 0, "red", "green")
    
    # Plot
    p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`3 Month Total Return`, yend=`6 Month Total Return`, col=class), size=.75, show.legend=F) + 
      geom_vline(xintercept=1, linetype="dashed", size=.1) + 
      geom_vline(xintercept=2, linetype="dashed", size=.1) +
      scale_color_manual(labels = c("Up", "Down"), 
                         values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
      labs(x="", y="Mean GdpPerCap") +  # Axis labels
      xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`3 Month Total Return`, df$`6 Month Total Return`))))  # X and Y axis limits
    
    # Add texts
    p <- p + geom_text(label=left_label, y=df$`3 Month Total Return`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
    p <- p + geom_text(label=right_label, y=df$`6 Month Total Return`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
    p <- p + geom_text(label="Time 1", x=1, y=1.1*(max(df$`3 Month Total Return`, df$`6 Month Total Return`)), hjust=1.2, size=5)  # title
    p <- p + geom_text(label="Time 2", x=2, y=1.1*(max(df$`3 Month Total Return`, df$`6 Month Total Return`)), hjust=-0.1, size=5)  # title
    
    # Minify theme
    p + theme(panel.background = element_blank(), 
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              panel.border = element_blank(),
              plot.margin = unit(c(1,2,1,2), "cm"))
  }, bg="transparent", execOnResize = TRUE)
  
  #Radarplot
  output$radarPlot <- renderChartJSRadar({
    
    
    labs <- c("Momentum", "Low risk", "Value",
              "Size",  "Quality", "Liquidity")
    
    scores <- list(
      "Country Average" = c(9, 7, 4, 5, 3, 7),
      "Sector Average" = c(7, 6, 6, 2, 6, 9),
      "Stock" = c(6, 5, 8, 4, 7, 6)
    )
    
    chartJSRadar(scores = scores, labs = labs, maxScale = 10)
  })
 
  output$frame <- renderUI({
    tags$iframe( src = "https://www.portfoliovisualizer.com/", width="100%",height="1000px") 
  })
  
  output$historyEQ <- renderTable({ 
    
    stocks_df <- myReactives$stocks_df
    validate(
      need(!is.na(input$n), "input NA")
    )
    
    load("mainEnv.RData")
    data <- mainEnv
    
    models <- backtest(ls(data),data,input$startdate,input$top_n,input$noMom,input$noVol)
    models[["equal.weight"]]$trade.summary$trades

    
    }) 
  
  output$historyTR <- renderTable({ 
    
    stocks_df <- myReactives$stocks_df
    validate(
      need(!is.na(input$n), "input NA")
    )
    
    load("mainEnv.RData")
    data <- mainEnv
    
    models <- backtest(ls(data),data,input$startdate,input$top_n,input$noMom,input$noVol)
    models[["trygg"]]$trade.summary$trades
    
    
  }) 
  
  output$historyMD <- renderTable({ 
    
    stocks_df <- myReactives$stocks_df
    validate(
      need(!is.na(input$n), "input NA")
    )
    
    load("mainEnv.RData")
    data <- mainEnv
    
    models <- backtest(ls(data),data,input$startdate,input$top_n,input$noMom,input$noVol)
    models[["modigplus"]]$trade.summary$trades
    
    
  }) 
  
  #UniversePie Output
  output$backtest <- renderPlot({
    stocks_df <- myReactives$stocks_df
    validate(
      need(!is.na(input$n), "input NA")
    )
    
    load("mainEnv.RData")
    data <- mainEnv
    
    models <- backtest(ls(data),data,input$startdate,input$top_n,input$noMom,input$noVol)
    aaa_xts <- merge(models$equal.weight$equity,models$trygg$equity,models$modigplus$equity)
    
    aaa_df <- data.frame(date=as.Date(index( aaa_xts),format = "%Y-%m-%d"), coredata( aaa_xts)) 
    names(aaa_df) <- c("date","Equal_weight","Trygg","Modig+")
    aaa_long <- melt(aaa_df,"date") 
    
    ggplot(aaa_long, aes(x=date,y=value, group = variable,colour=variable)) +
    geom_line(size=0.8,aes(group = variable))  +
    scale_x_date(breaks = date_breaks("1 year"), labels=date_format("%Y")) +
    coord_trans(y = "log10") +
      scale_fill_manual(values  = color_palette[c(1,6,12)]) +
      scale_color_manual(values  = color_palette[c(1,6,12)]) +
    scale_y_continuous(labels=percent) +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold", color="white"),
            plot.background = element_rect(fill = "#3CA2B0"),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major.y = element_line(colour = "grey30", size=0.1),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5, color="white", size=18),
            plot.margin=unit(c(0.6,0.6,0.9,0.8),"cm"),
            axis.text.x = element_text(face="bold", color="white"),
            axis.text.y = element_text(face="bold", color="white"),
            axis.ticks = element_blank(),
            panel.border = element_rect(fill=NA,color="#42A3B0", size=0.5,linetype="solid"))
    
  # theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size=25), legend.key.width = unit(6,"cm")) + guides(colour = guide_legend(override.aes = list(size = 10)))+
   #   theme(legend.key=element_rect(fill=NA), legend.title=element_blank(), legend.position="bottom")
    
  })
  
  #UniversePie Output
  output$btPlot <- renderPlot({
  
    stocks_df <- myReactives$stocks_df
    validate(
      need(!is.na(input$n), "input NA")
    )
    load("mainEnv.RData")
    data <- mainEnv
    models <- backtest(ls(data),data,input$startdate,input$top_n,input$noMom,input$noVol)
    weight = models[["equal.weight"]]$weight[, sort.list(colSums(models[["equal.weight"]]$weight!=0), decreasing=T)]
    weight <- weight[ endpoints(weight, on="months")]
    weight_df <- data.frame(date=as.Date(index(weight),format = "%Y-%m-%d"), coredata(weight))
    
    weight_long <- melt(weight_df,"date")
    ggplot(data = weight_long, aes(x=date, y=value, fill=variable,label=variable, cumulative = TRUE)) +
      geom_col(width = 18) +
      scale_fill_manual(values  = color_palette) +
      labs(x = "", y = "", title = " ", subtitle = "") +
      geom_text(aes(label = variable), color = "white", size = 4, position = position_stack(vjust = 0.5)) + 
      theme_minimal()
   
    
  })
  
  # 
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    stocks_df <- myReactives$stocks_df
    
    DT::datatable(stocks_df[, input$show_vars1, drop = FALSE], rownames=FALSE, 
                  selection = 'single', escape=FALSE,options = list(
      dom = '<f<t>ip>',
      language = list(url = "language.json"),
      scrollY=400, 
      scrollX=TRUE,
      pageLength = 50, 
      autoWidth = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3CA2B0', 'color': 'white'});",
        "}"))) %>%
      formatStyle(1:14,backgroundColor="white") %>%
      formatStyle(1:14,Color="black")
  })
  
  
  
}

shinyApp(ui, server)
