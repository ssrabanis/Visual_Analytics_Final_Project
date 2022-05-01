# Load packages -------------------------
library(shiny)
library(shinydashboard)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)


## read data -----------------------------

currency_data <- read_csv("data/Final Dataset.csv")
marketcap <- mean(currency_data$Marketcap)
changeprice <- ((currency_data$Close-currency_data$Open) / (currency_data$Open))*100
currency_datanew <- cbind(currency_data,changeprice)

## UI ------------------------------------

ui <- fluidPage(
  tags$head(
    
    tags$style(
      
      type = "text/css",
      
      ".irs-grid-text {font-size: 12pt !important; transform: translate(0px,40px);"
      
    )),
  tags$head(tags$style("
                  #container * {  
  display: flex;
  flex-wrap: wrap;
   
                  }
         
                    #dsc{
                    display: flex;
                    center-align;
                    padding-left: 5px;
                       }
                       ")),

  sidebarLayout(
    sidebarPanel(
      #create a drop down box
      #selectInput(inputId = "type", label = "Choose an option",
      #choices = ccurrency,
      #selected = "Overview"),
      ## add a paragraph -----------------------
      #p("This sheet tells us which are the top cryptocurrencies in the market based on their average market capacities, how the daily stock price changes with time for the same."),
      #p("A deep dive into the daily fluctuations in terms of closing price of the top 4 cryptocurrencies. This visualization shows the volatility of the currency and gives us insights on how stable it is in the market."),
      #p("A direct comparison between volume and price shows how people are responding to the dips and highs of a currency. People tend to buy more often when there is a dip in the price"),
      ## add action button to run the app -----------------------
      #actionButton(inputId = "action_button",
      #label = "Click")
      
      ## conditionalPanel() functions for selected tab
      
      conditionalPanel(condition = "input.tabselected==1",
                       
                       div(id="container", img(src = "btc.png", width="15%", height="15%", alt = "Bitcoin Logo"),
                       p(id="dsc","The world’s first cryptocurrency, Bitcoin is stored and exchanged 
                         securely on the internet through a digital ledger known as a blockchain.")),
                       
                       hr(),
                       div(id="container",img(src = "bnb.png", width="15%", height="15%", alt = "Binance Logo"),
                       p(id="dsc","Binance Coin is a cryptocurrency used to pay fees on the Binance 
                         cryptocurrency exchange. Fees paid in Binance Coin on the exchange receive 
                         a discount.")),
                       
                       hr(),
                       div(id="container",img(src = "eth.png", width="15%", height="15%", alt = "Ethereum Logo"),
                       p(id="dsc","Ethereum is a decentralized computing platform that uses ETH 
                         (also called Ether) to pay transaction fees (or “gas”). 
                         Developers can use Ethereum to run decentralized applications 
                         (dApps) and issue new crypto assets, known as Ethereum tokens.")),
                       
                       hr(),
                       div(id="container",img(src = "xrp.png", width="15%", height="15%", alt = "XRP Logo"),
                       p(id="dsc","XRP is the cryptocurrency used by the Ripple payment network. 
                         Built for enterprise use, XRP aims to be a fast, cost-efficient cryptocurrency 
                         for cross-border payments.")),
                       
                       hr(),
                       div(id="container",img(src = "usdt.png", width="15%", height="15%", alt = "Tether Logo"),
                       p(id="dsc", "Tether (USDT) is an Ethereum token that is pegged to the value of a U.S. 
                         dollar (also known as a stablecoin). Tether’s issuer claims that USDT is backed by 
                         bank reserves and loans which match or exceed the value of USDT in circulation.")),
                       
                       hr(),
                       div(id="container",img(src = "ada.png", width="15%", height="15%", alt = "Cardano Logo"),
                           p(id="dsc", "Cardano (ADA) is a blockchain platform built on a proof-of-stake consensus protocol 
                             (called Ouroboros) that validates transactions without high energy costs.")),
                       
                       hr(),
                       div(id="container",img(src = "ltc.png", width="15%", height="15%", alt = "Litecoin Logo"),
                           p(id="div","Litecoin is a peer-to-peer Internet currency that enables instant, 
                             near-zero cost payments to anyone in the world."))
      
                       ),
      
      conditionalPanel(condition = "input.tabselected==2",
                       
                       h4("Year on Year Comparison of the top 7 Cryptocurrencies"),
                       
                       p("\n Average Market Capacity is calculated by multiplying the total number
                         of coins that have been mined by the price of a single coin at any given time.
                         \n
                         Market capacity of $3billion and $10billion are considered mid-cap companies
                         with more room for growth"),
                       br(),
                       p("\nThe highest growth in Market Cap from 2018 to 2021 was observed for Binance Coin with a growth of 1760%. \n
                         The lowest growth from 2018 to 2021 was for LiteCoin with 10%. \n Another interesting observation is XRP where Avg 
                         Market Capacity reduced by 35% in the last 3 years."),
                       br(),
                       hr(),
                       
                       p("\n Make a selection of the year you want to be displayed."),
                       hr(),
                       
                       selectInput(inputId ="year1", label = "Select a Year",
                                         choices = c("2017", "2018", "2019", "2020", "2021"),
                                         selected = "2021")
                       ),
      
      conditionalPanel(condition = "input.tabselected==3",
                       
                       h4("% Change in Price"),
                       
                       p("\n The % Change In Price can be used to see how volatile the cryptocurrency is. 
                         Volatility is a measure of how much the price of an asset has moved up or down 
                         over time."),
                       
                       selectInput(inputId ="year2", label = "Select a Year",
                                   choices = c("2017", "2018", "2019", "2020", "2021"),
                                   selected = "2021"),
                       
                       selectInput(inputId ="mon1", label = "Select a Month",
                                   choices = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct","Nov","Dec"),
                                   selected = "Jan"),
                       
                       selectInput(inputId ="coin1", label = "Select Coin Type",
                                   choices = c("Binance Coin", "Bitcoin", "Cardano", "Ethereum", "Litecoin", "Tether", "XRP"),
                                   selected = "Bitcoin")
                       
                       ),
      
      conditionalPanel(condition = "input.tabselected==4",
                       
                       h4("Daily Trend"),
                       
                       h5("The Daily Stock Price shows the trend of highest and lowest price of all 
                          seven currencies for each month of each year. The red line 
                          represents the highest price of the currency whereas the blue line 
                          is showing the lowest price of that day for that coin."),
                       
                       selectInput(inputId ="year3", label = "Select a Year",
                                         choices = c("2017", "2018", "2019", "2020", "2021"),
                                         selected = "2021"),
                       
                       selectInput(inputId ="mon2", label = "Select a Month",
                                         choices = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct","Nov","Dec"),
                                         selected = "Jan"),
                       
                       selectInput(inputId ="coin2", label = "Select Coin Type",
                                         choices = c("Binance Coin", "Bitcoin", "Cardano", "Ethereum", "Litecoin", "Tether", "XRP"),
                                         selected = "Bitcoin")),
      
      conditionalPanel(condition = "input.tabselected==5",
                       h1("THANK YOU!"))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Overview",value = 1,tags$img(src = "crypto2.jpg", height = "100%", width = "100%")),
                  tabPanel(title = "Market Capacity", value = 2,plotOutput (outputId = "yearly_bar",height = 500, width = "100%")),
                  tabPanel(title = "% Change", value = 3,plotOutput (outputId = "monthly_chart",height = 500, width = "100%")),
                  tabPanel(title = "Daily Trend", value = 4,plotOutput (outputId = "daily_bar",height = 500, width = "100%")),
                  tabPanel(title = "About", value = 5,
                           h4("This app was developed by Bindu Madhavi, Sammie Srabani, and Shina Dhingra."),
                           br(),
                           p("It was the result of Chase Romano's Visual Analytics course at the University of 
                 North Carolina Charlotte through the Data Science and Business Analytics MS program."),
                           br(),
                           HTML('<a href="https://github.com/shinadhingra18" style="color: #e36209">View Code on GitHub</a>')
                           
                           
                           
                           
                           
                           ),
                  id = "tabselected"
      )
    )
  )
)
server <- function(input, output) {
  
  
  #Yearly Summarize
  data <- reactive({
    req(input$year1)
    df <- currency_data %>% filter(Year %in% input$year1) %>% group_by(Name) %>% summarise(Marketcap = sum(Marketcap))
  })
  #Plot the yearly graph
  output$yearly_bar <- renderPlot({
    g <- ggplot(data(), aes(y=round((Marketcap/1000000000),digits=0), x= reorder(Name, -Marketcap),fill=reorder(Name, -Marketcap)))
    g + geom_bar(stat = "sum",
                 aes(color=Name, fill=Name),
                 show.legend = FALSE) +
      geom_text(aes(label=round((Marketcap/1000000000),digits=0), vjust=1), size=6) +
      labs(title = "Top 7 Cryptocurrencies based on Market Capacity",
        x = "Currency", y="Avg Market Capacity (Billions)") +
      scale_y_continuous(labels = scales::comma)+
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2")+
      theme_bw() +
      theme(
            title = element_text(size = 17),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15, face = "bold"))
  })
  
  #Daily Summarize
  ddata <- reactive({
    req(input$year3,input$coin2,input$mon2)
    ddf <- currency_data %>% filter(Year %in% input$year3) %>% filter(Month %in% input$mon2) %>% filter(Name %in% input$coin2)
  })
  
  #Plot the daily graph
  output$daily_bar <- renderPlot({
    ggplot(ddata(), aes(x= Day))+
    #g + geom_line(stat = "sum") +
    geom_line(aes(y=High), color = "red",size=1) +
    geom_line(aes(y=Low), color = "blue",size = 1) +  
      labs(x = "Days", y="Price") +
      scale_x_continuous(name="Date", limits=c(1, 31),breaks=seq(1,31,2))+
      scale_y_continuous(labels = scales::comma)+
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2")+
      theme_bw() +
      theme(
        title = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, face = "bold"))
  })
  
  #Change in Price
  datamonthly <- reactive({
    req(input$year2,input$coin1,input$mon1)
    
    dfm <- currency_datanew %>% filter(Year %in% input$year2) %>% filter(Month %in% input$mon1) %>% filter(Name %in% input$coin1) 
  })
  
  
  #Plot the graph
  output$monthly_chart <- renderPlot({
    
    ggplot(datamonthly(), aes(x= Day))+
      
      geom_line(aes(y=changeprice,color="green"),
                show.legend = FALSE,size=1.5) +
      geom_point(aes(y=changeprice,color="green"),
                 show.legend = FALSE,size=3)+
      geom_label(aes(Day,changeprice,label = round(changeprice,digits=1)))+

      labs(title ="Daily % Change in Price",
        x = "Date", 
        y="Change in Price (Percentage)") +
      scale_x_continuous(name="Date", limits=c(1, 31),breaks=seq(1,31,2))+
      scale_y_continuous(labels = scales::comma)+
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2")+
      theme_bw() +
      theme(
        title = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, face = "bold"))
  })
}
shinyApp(ui = ui, server = server)