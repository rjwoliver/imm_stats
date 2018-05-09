library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)

eui_column_names <-c("declarantcode", "partnercode", "productcode", "year", "val", "qnt", "sup", "rwe", "flag") 
eui_column_classes <- c("character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character") 

#read trade data as data.table
eu_imports_yr <- fread('data/eu_imports_yr.csv', header = T, sep = ',', colClasses = eui_column_classes)
setnames(eu_imports_yr, eui_column_names)
products <- fread('data/products.csv', header = T, sep = ',')
declarants <- fread('data/declarants.csv', header = T, sep = ',')
productsummary <- fread('data/productsummary.csv', header = T, sep = ',')
vpapartners <- fread('data/vpapartners.csv', header = T, sep = ',')
partnergroups <- fread('data/partnergroups.csv', header = T, sep = ',')
vpapartners <- fread('data/vpapartners.csv', header = T, sep = ',')

#set data table keys
#setkey(eu_imports_mn, "declarantcode", "partnercode", "productcode")
setkey(eu_imports_yr, "declarantcode", "partnercode", "productcode")
setkey(products, "order", "productcode")
setkey(declarants, "declarantname")
setkey(productsummary, "order", "productgroup")
setkey(vpapartners, "order")

#eu_imports_mn[,posix:=fastPOSIXct(datadate)]

#create keyed list of unique partners
partners <- partnergroups[group == ".World"]
setkey(partners, "partnercode")

# prepare vpa partner groups for select input
vpagroups <- vpapartners[,.(ordervpagroups = str_sub(order,1,1),vpagroup)]
setkey(vpagroups, "ordervpagroups", "vpagroup")
vpagroups <- unique(vpagroups)

# prepare major product groups for select input
majorproductgroups <- productsummary[,.(ordermajorproductgroup = str_sub(order,1,1),productgroup)]
setkey(majorproductgroups, "ordermajorproductgroup", "productgroup")
majorproductgroups <- unique(majorproductgroups)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin="yellow",
                    
                    # Application title
                    dashboardHeader(title="FLEGT IMM dashboard"),
                    
                    dashboardSidebar(
                      sidebarMenu(id="sbmenu",
                                  menuItem("VPA partners", tabName = "vpapartners", icon = icon("globe")),
                                  menuItem("EU member states", tabName = "eumembers", icon = icon("eur")),
                                  menuItem("Products", tabName = "products", icon = icon("pagelines"))
                      )
                    ),
                    
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="vpapartners",
                                fluidRow(
                                  box(
                                    title="Select VPA Partner",
                                    width = 12,
                                    selectInput('vpagroupinput', 'Select FLEGT group:', choices = vpagroups$vpagroup),
                                    selectInput('vpapartnerinput', 'Select partner:', choices = vpapartners$partner)
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by EU member country",
                                    width = 12,
                                    plotlyOutput("p1a")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by product group",
                                    width = 12,
                                    plotlyOutput("p1b")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_all1", "All data in long format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_val1", "Value data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_qnt1", "Tonnage data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_rwe1", "RWE volume data in wide format")
                                  )
                                )
                        ),
                        
                        tabItem(tabName="eumembers",
                                fluidRow(
                                  box(
                                    title="Select EU member",
                                    width = 12,
                                    selectInput('declarantinput', 'Select EU member:', choices = declarants$declarantname)
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by VPA partner country",
                                    width = 12,
                                    plotlyOutput("p2a")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by product group",
                                    width = 12,
                                    plotlyOutput("p2b")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_all2", "All data in long format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_val2", "Value data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_qnt2", "Tonnage data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_rwe2", "RWE volume data in wide format")
                                  )
                                )
                        ),
                        tabItem(tabName="products",
                                fluidRow(
                                  box(
                                    title="Select product",
                                    width = 12,
                                    selectInput('productgroupinput', 'Select product group:', choices = majorproductgroups$productgroup),
                                    selectInput('productsummaryinput', 'Select product:', choices = productsummary$productsummary)
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by VPA partner country",
                                    width = 12,
                                    plotlyOutput("p3a")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Value by EU member",
                                    width = 12,
                                    plotlyOutput("p3b")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_all3", "All data in long format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_val3", "Value data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_qnt3", "Tonnage data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_rwe3", "RWE volume data in wide format")
                                  )
                                )
                        )
                      )
                    )
)    

# Define server logic
server <- function(input, output, session) {

  ax <- list(title = "", tickfont = list(color="red", size = 9))
  ay <- list(title = "value (1000 euro)")
  
  output$p1a <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("val")]	  
    setorder(dt_declarant, "declarantname","year")
    p1a <- plot_ly(dt_declarant, x = ~declarantname, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p1a$elementId <- NULL
    p1a
  })    
  output$p1b <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("val")]	  
    setorder(dt_product, "productgroup","year")
    p1b <- plot_ly(dt_product, x = ~productgroup, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p1b$elementId <- NULL
    p1b
  })    
  
  output$p2a <- renderPlotly({
    dt_partner <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("val")]	  
    setorder(dt_partner, "partnername","year")
    p2a <- plot_ly(dt_partner, x = ~partnername, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p2a$elementId <- NULL
    p2a
  })    
  output$p2b <- renderPlotly({
    dt_product <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("val")]	  
    setorder(dt_product, "productgroup","year")
    p2b <- plot_ly(dt_product, x = ~productgroup, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p2b$elementId <- NULL
    p2b
  })    
  
  output$p3a <- renderPlotly({
    dt_partner <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("val")]	  
    setorder(dt_partner, "partnername","year")
    p2a <- plot_ly(dt_partner, x = ~partnername, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p2a$elementId <- NULL
    p2a
  })    
  output$p3b <- renderPlotly({
    dt_declarant <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("val")]	  
    setorder(dt_declarant, "declarantname","year")
    p3b <- plot_ly(dt_declarant, x = ~declarantname, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay)
    p3b$elementId <- NULL
    p3b
  })
  
  output$downloadData_all1 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_vpapartner_full_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      headertxt <- paste("EU member state imports of timber products from ", input$vpapartnerinput, ": annual Value in 1000 euro & quantity in metric tonnes & rwe in cubic meters", sep="")
      write.table_with_header(vpapartnerdata_full(), file, headertxt, sep = ",", row.names=FALSE)
    }
  )

  output$downloadData_val1 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_vpapartner_val_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- vpapartnerdata_full() %>% 
        dcast(declarantname + productgroup ~ year, value.var = "val") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member state imports of timber products from ", input$vpapartnerinput, ": annual Value in 1000 euro", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )

    output$downloadData_qnt1 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_vpapartner_qnt_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- vpapartnerdata_full() %>% 
        dcast(declarantname + productgroup ~ year, value.var = "qnt") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member state imports of timber products from ", input$vpapartnerinput, ": annual quantity in metric tonnes", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  

  output$downloadData_rwe1 <- downloadHandler(
      
      filename = function() {
        paste("imm_stats_vpapartner_rwe_", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        dt <- vpapartnerdata_full() %>% 
          dcast(declarantname + productgroup ~ year, value.var = "rwe") %>% 
          setorder(-"yr_2017", na.last = TRUE)
        headertxt <- paste("EU member state imports of timber products from ", input$vpapartnerinput, ": annual roundwood equivalent volume in cubic meters", sep="")
        write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
      }
  )
    
  output$downloadData_all2 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_eumembers_full_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      headertxt <- paste(input$declarantinput, " imports of timber products from vpa partner countries: annual Value in 1000 euro & quantity in metric tonnes & rwe in cubic meters", sep="")
      write.table_with_header(eumemberdata_full(), file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_val2 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_eumembers_val_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- eumemberdata_full() %>% 
        dcast(partnername + productgroup ~ year, value.var = "val") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste(input$declarantinput, " imports of timber products from vpa partner countries: annual Value in 1000 euro", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_qnt2 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_eumembers_qnt_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- eumemberdata_full() %>% 
        dcast(partnername + productgroup ~ year, value.var = "qnt") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste(input$declarantinput, " imports of timber products from vpa partner countries: quantity in metric tonnes", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  
  output$downloadData_rwe2 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_eumembers_rwe_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- eumemberdata_full() %>% 
        dcast(partnername + productgroup ~ year, value.var = "rwe") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste(input$declarantinput, " imports of timber products from vpa partner countries: annual Value in 1000 euro & quantity in metric tonnes & rwe in cubic meters", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )        

  output$downloadData_all3 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_products_full_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      headertxt <- paste("EU member imports of ", input$productsummaryinput," from vpa partner countries: annual Value in 1000 euro & quantity in metric tonnes & rwe in cubic meters", sep="")
      write.table_with_header(productdata_full(), file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_val3 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_products_val_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- productdata_full() %>% 
        dcast(partnername + declarantname ~ year, value.var = "val") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member imports of ", input$productsummaryinput," from vpa partner countries: annual Value in 1000 euro", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_qnt3 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_products_qnt_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- productdata_full() %>% 
        dcast(partnername + declarantname ~ year, value.var = "qnt") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member imports of ", input$productsummaryinput," from vpa partner countries: quantity in metric tonnes", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  
  output$downloadData_rwe3 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_products_rwe_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- productdata_full() %>% 
        dcast(partnername + declarantname ~ year, value.var = "rwe") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member imports of ", input$productsummaryinput," from vpa partner countries: rwe in cubic meters", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )        
  
  
  #prepare vector of product codes from input product group and product summary
  filterproducts <- reactive({
    #if products tab is chosen - select products based on input
    if (input$sbmenu == "products") {
      validate(
        need(input$productgroupinput != "", "Please select a product group"),
        need(input$productsummaryinput != "", "Please select a product")
      )
      #use order field of form "A1", "AA", "AB" etc to identify product groups
      ordercode <- productsummary[productsummary==input$productsummaryinput, order]
      if (substr(ordercode[1],2,2) == "1" ) {
        startletter <- c(paste(LETTERS[1:10]))
        productcodevector <- products[substr(order,1,1) %in% startletter, productcode]
      } else if (substr(ordercode[1], 2,2) == "A") {
        productcodevector <- products[productgroup==input$productgroupinput, productcode]
      } else {
        productcodevector <- products[productgroup==input$productgroupinput & productsummary==input$productsummaryinput, productcode]
      }
    } else {
      # products tab not chosen - vector contains all products
      productcodevector <- products[,productcode]
    }
    return(productcodevector)
  })
  
  #prepare vector of declarant codes from input declarant name
  filterdeclarants <- reactive({
    #if eumembers tab is chosen - select declarants based on input
    if (input$sbmenu == "eumembers") {
      validate(
        need(input$declarantinput != "", "Please select an EU member country")
      )
      #lookup declarant codes    
      if (input$declarantinput == ".EU28") {
        declarantcodevector <- declarants[,declarantcode]
      } 
      else {
        declarantcodevector <- declarants[declarantname==input$declarantinput, declarantcode]
      }
    } else {
      # eumembers tab not chosen - vector contains all declarants
      declarantcodevector <- declarants[,declarantcode]
    }
    return(declarantcodevector)
  })
  
  #prepare vector of partner codes from input partner group name
  filterpartners <- reactive({
    #if vpa partners tab is chosen - select vpa partners based on input
    if (input$sbmenu == "vpapartners") {
      validate(
        need(input$vpagroupinput != "", "Please select a FLEGT partner group"),
        need(input$vpapartnerinput != "", "Please select a FLEGT partner")
      )
      #lookup partner codes    
      if (substr(input$vpapartnerinput, 1, 3) == "All") {
        vpapartnercodevector <- vpapartners[vpagroup==input$vpagroupinput, partnercode]
      } else {
        vpapartnercodevector <- vpapartners[partner==input$vpapartnerinput, partnercode]
      } 
    } else {
      # vpa partners tab not chosen - vector contains all vpa partners
      vpapartnercodevector <- vpapartners[, partnercode]
    }
    return(vpapartnercodevector)
  })
  
  #prepare dataset using vectors of declarant codes, partner codes and products codes
  filterdataset <- reactive({
    dt <- eu_imports_yr[declarantcode %in% filterdeclarants()][partnercode %in% filterpartners()][productcode %in% filterproducts()]
    return(dt)
  })
  
  #prepare full vpa partner tab data 
  vpapartnerdata_full <- reactive({
    dt <- filterdataset()[declarants, on="declarantcode"][products, on="productcode"]
    dt <- dt[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname,productgroup,year), .SDcols=c("val","qnt","rwe")]	  
    dt <- dt[majorproductgroups, on="productgroup"]
    dt <- dt[complete.cases(dt),]
    setcolorder(dt, c("declarantname", "ordermajorproductgroup",  "productgroup", "year","val", "qnt","rwe"))
    setorder(dt, "declarantname", "ordermajorproductgroup",  "productgroup", "year")
    return(dt)
  })        
  
  #prepare full eu member tab data
  eumemberdata_full <- reactive({
    dt <- filterdataset()[partners, on="partnercode"][products, on="productcode"]
    dt <- dt[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername,productgroup,year), .SDcols=c("val","qnt","rwe")]	  
    dt <- dt[majorproductgroups, on="productgroup"]
    dt <- dt[complete.cases(dt),]
    setcolorder(dt, c("partnername", "ordermajorproductgroup",  "productgroup", "year","val", "qnt","rwe"))
    setorder(dt, "partnername", "ordermajorproductgroup",  "productgroup", "year")
    return(dt)
  })        
  
  #prepare full product tab data 
  productdata_full <- reactive({
    dt <- filterdataset()[declarants, on="declarantcode"][partners, on="partnercode"]
    dt <- dt[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername,declarantname,year), .SDcols=c("val","qnt","rwe")]	  
    dt <- dt[complete.cases(dt),]
    setcolorder(dt, c("partnername", "declarantname", "year","val", "qnt","rwe"))
    setorder(dt, "partnername","declarantname","year")
  })        
  
  #function to add header to downloaded csv file 
  write.table_with_header <- function(x, file, header, ...){
    cat(header, '\n',  file = file)
    write.table(x, file, append = T, ...)
  }

  observe({ 
    #filter product group based on productgroupinput
    productsummaryfilter <- productsummary[productgroup==input$productgroupinput][order(order)]
    updateSelectizeInput(session, "productsummaryinput", choices = productsummaryfilter$productsummary, selected = productsummaryfilter$productsummary[1]) 
  }, priority = 1) # 
  
  observe({ 
    #filter partner  based on vpagroupinput
    vpapartnerfilter <- vpapartners[vpagroup==input$vpagroupinput][order(order)]
    updateSelectizeInput(session, "vpapartnerinput", choices = vpapartnerfilter$partner, selected = vpapartnerfilter$partner[1]) 
  }, priority = 1) # 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

