library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)

#read trade data as data.table
column_names <-c("declarantcode", "partnercode", "productcode", "year", "val", "qnt", "sup", "rwe", "flag") 
column_classes <- c("character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character") 
eu_imports_yr <- fread('data/eu_imports_yr.csv', header = F, sep = ',', colClasses = column_classes)
setnames(eu_imports_yr, column_names)

column_names <-c("order","productcode","productgroup","productsummary","description","eutr") 
column_classes <- c("character", "character", "character", "character", "character", "numeric")
products <- fread('data/products.csv', header = F, sep = ',', colClasses = column_classes)
setnames(products, column_names)

column_names <-c("declarantcode","startyr","endyr","declarantname","declarantlettercode","declarantisocode","euwithdrawal") 
column_classes <- c("character", "numeric", "numeric", "character", "character", "character", "numeric")
declarants <- fread('data/declarants.csv', header = F, sep = ',', colClasses = column_classes)
setnames(declarants, column_names)

column_names <-c("order","productgroup","productsummary") 
column_classes <- c("character", "character", "character")
productsummary <- fread('data/productsummary.csv', header = F, sep = ',', colClasses = column_classes)
setnames(productsummary, column_names)

column_names <-c("order","vpagroup","partner","partnercode") 
column_classes <- c("character", "character", "character", "character")
vpapartners <- fread('data/vpapartners.csv', header = F, sep = ',', colClasses = column_classes)
setnames(vpapartners, column_names)

column_names <-c("groupcategory","group","partnername","partnercode") 
column_classes <- c("character", "character", "character", "character")
partnergroups <- fread('data/partnergroups.csv', header = F, sep = ',', colClasses = column_classes)
setnames(partnergroups, column_names)

column_names <-c("order","partnercode","partner") 
column_classes <- c("character", "character", "character")
nonvpapartners <- fread('data/nonvpapartners.csv', header = F, sep = ',', colClasses = column_classes)
setnames(nonvpapartners, column_names)

#set data table keys
#setkey(eu_imports_mn, "declarantcode", "partnercode", "productcode")
setkey(eu_imports_yr, "declarantcode", "partnercode", "productcode")
setkey(products, "order", "productcode")
setkey(declarants, "declarantname")
setkey(productsummary, "order", "productgroup")
setkey(vpapartners, "order")
setkey(nonvpapartners, "order")

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
                    dashboardHeader(title = tags$a(href='http://www.flegtimm.eu',
                                                   tags$img(src='IMM_header_logo.png')
                                                   )
                                    ),
                    
                    dashboardSidebar(
                      sidebarMenu(id="sbmenu",
                                  menuItem("Stats intro", tabName = "intro", icon = icon("home")),
                                  menuItem("VPA partners", tabName = "vpapartners", icon = icon("globe")),
                                  menuItem("Other tropical suppliers", tabName = "nonvpapartners", icon = icon("circle")),
                                  menuItem("EU member states", tabName = "eumembers", icon = icon("eur")),
                                  menuItem("Products", tabName = "products", icon = icon("pagelines")),
                                  menuItem("Technical", tabName = "technical", icon = icon("question-circle"))
                      )
                    ),
                    
                    
                    dashboardBody(
                      tags$style(".nav-tabs-custom .nav-tabs li.active {
                                    border-top-color: #00a65a;
                                  }
                                  .skin-yellow .main-header .logo {
                                    background-color: #dddddd;
                                  }
                                 
                                  .skin-yellow .main-header .logo:hover {
                                    background-color: #dddddd;
                                  }
                                 
                                  .skin-yellow .main-header .navbar .sidebar-toggle:hover{
                                    background-color: #dddddd;
                                  }
                                   .skin-yellow .main-header .navbar {
                                   background-color: #dddddd;
                                  }"
                      ),
                      tabItems(
                        tabItem(tabName="intro",
                                fluidRow(
                                  box(
                                    title="EU FLEGT Trade Dashboard",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 9,
                                    includeHTML('www/home.html')
                                  ),
                                  box(
                                    title="Donors",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 3,
                                    includeHTML('www/supporters.html')
                                  )
                                )
                        ),

                        tabItem(tabName="vpapartners",
                                fluidRow(
                                  box(
                                    title="Select VPA Partner",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 12,
                                    selectInput('vpagroupinput', 'Select FLEGT group:', choices = vpagroups$vpagroup),
                                    selectInput('vpapartnerinput', 'Select partner:', choices = vpapartners$partner)
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="Imports by EU member country",
                                    id="vpapartnerstabset1",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p1a_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p1a_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p1a_rwe"))
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="EU imports  by product group",
                                    id="vpapartnerstabset2",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p1b_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p1b_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p1b_rwe"))
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
                        
                        tabItem(tabName="nonvpapartners",
                                fluidRow(
                                  box(
                                    title="Select non-VPA country",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 12,
                                    selectInput('nonvpapartnerinput', 'Select non-VPA country:', choices = nonvpapartners$partner)
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="Import value by EU member country",
                                    id="nonvpapartnerstabset1",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p4a_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p4a_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p4a_rwe"))
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="EU import Value by product group",
                                    id="nonvpapartnerstabset1",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p4b_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p4b_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p4b_rwe"))
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_all4", "All data in long format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_val4", "Value data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_qnt4", "Tonnage data in wide format")
                                  ),
                                  box(
                                    width = 2,
                                    downloadButton("downloadData_rwe4", "RWE volume data in wide format")
                                  )
                                )
                        ),

                        tabItem(tabName="eumembers",
                                fluidRow(
                                  box(
                                    title="Select EU member",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 12,
                                    selectInput('declarantinput', 'Select EU member:', choices = declarants$declarantname)
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="EU import by VPA partner country",
                                    id="eumemberstabset1",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p2a_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p2a_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p2a_rwe"))
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="EU import by product group",
                                    id="eumemberstabset2",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p2b_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p2b_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p2b_rwe"))
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
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 12,
                                    selectInput('productgroupinput', 'Select product group:', choices = majorproductgroups$productgroup),
                                    selectInput('productsummaryinput', 'Select product:', choices = productsummary$productsummary)
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="EU import by VPA partner country",
                                    id="productstabset1",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p3a_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p3a_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p3a_rwe"))
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title="Import by EU member",
                                    id="productstabset2",
                                    width = 12,
                                    tabPanel("Value 1000 euro", plotlyOutput("p3b_val")),
                                    tabPanel("Metric tonnes", plotlyOutput("p3b_qnt")),
                                    tabPanel("Roundwood equivalent m3", plotlyOutput("p3b_rwe"))
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
                        ),
                        tabItem(tabName="technical",
                                fluidRow(
                                  box(
                                    title="FLEGT IMM COMEXT data analysis procedures",
                                    solidHeader=TRUE,
                                    status="success",
                                    width = 12,
                                    includeHTML('www/technical.html')
                                  )
                                )
                        )
                      )
                    )
)    

# Define server logic
server <- function(input, output, session) {

  ax <- list(title = "", tickfont = list(color="red", size = 9))
  ay_val <- list(title = "value (1000 euro)")
  ay_qnt <- list(title = "metric tonnes")
  ay_rwe <- list(title = "RWE cubic meters")
  
  output$p1a_val <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("val")]	  
    setorder(dt_declarant, "declarantname","year")
    p1a_val <- plot_ly(dt_declarant, x = ~declarantname, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p1a_val$elementId <- NULL
    p1a_val
  })    
  output$p1b_val <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("val")]	  
    setorder(dt_product, "productgroup","year")
    p1b_val <- plot_ly(dt_product, x = ~productgroup, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p1b_val$elementId <- NULL
    p1b_val
  })    
  
  output$p1a_qnt <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("qnt")]	  
    setorder(dt_declarant, "declarantname","year")
    p1a_qnt <- plot_ly(dt_declarant, x = ~declarantname, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p1a_qnt$elementId <- NULL
    p1a_qnt
  })    
  output$p1b_qnt <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("qnt")]	  
    setorder(dt_product, "productgroup","year")
    p1b_qnt <- plot_ly(dt_product, x = ~productgroup, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p1b_qnt$elementId <- NULL
    p1b_qnt
  })
  
  output$p1a_rwe <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("rwe")]	  
    setorder(dt_declarant, "declarantname","year")
    p1a_rwe <- plot_ly(dt_declarant, x = ~declarantname, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p1a_rwe$elementId <- NULL
    p1a_rwe
  })    
  output$p1b_rwe <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("rwe")]	  
    setorder(dt_product, "productgroup","year")
    p1b_rwe <- plot_ly(dt_product, x = ~productgroup, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p1b_rwe$elementId <- NULL
    p1b_rwe
  })
  
  output$p4a_val <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("val")]	  
    setorder(dt_declarant, "declarantname","year")
    p4a_val <- plot_ly(dt_declarant, x = ~declarantname, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p4a_val$elementId <- NULL
    p4a_val
  })    
  output$p4b_val <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("val")]	  
    setorder(dt_product, "productgroup","year")
    p4b_val <- plot_ly(dt_product, x = ~productgroup, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p4b_val$elementId <- NULL
    p4b_val
  })
  
  output$p4a_qnt <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("qnt")]	  
    setorder(dt_declarant, "declarantname","year")
    p4a_qnt <- plot_ly(dt_declarant, x = ~declarantname, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p4a_qnt$elementId <- NULL
    p4a_qnt
  })    
  output$p4b_qnt <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("qnt")]	  
    setorder(dt_product, "productgroup","year")
    p4b_qnt <- plot_ly(dt_product, x = ~productgroup, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p4b_qnt$elementId <- NULL
    p4b_qnt
  })
  
  output$p4a_rwe <- renderPlotly({
    dt_declarant <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("rwe")]	  
    setorder(dt_declarant, "declarantname","year")
    p4a_rwe <- plot_ly(dt_declarant, x = ~declarantname, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p4a_rwe$elementId <- NULL
    p4a_rwe
  }) 
  
  output$p4b_rwe <- renderPlotly({
    dt_product <- vpapartnerdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("rwe")]	  
    setorder(dt_product, "productgroup","year")
    p4b_rwe <- plot_ly(dt_product, x = ~productgroup, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p4b_rwe$elementId <- NULL
    p4b_rwe
  })
  
  output$p2a_val <- renderPlotly({
    dt_partner <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("val")]	  
    setorder(dt_partner, "partnername","year")
    p2a_val <- plot_ly(dt_partner, x = ~partnername, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p2a_val$elementId <- NULL
    p2a_val
  })    
  output$p2b_val <- renderPlotly({
    dt_product <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("val")]	  
    setorder(dt_product, "productgroup","year")
    p2b_val <- plot_ly(dt_product, x = ~productgroup, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p2b_val$elementId <- NULL
    p2b_val
  })    
  
  output$p2a_qnt <- renderPlotly({
    dt_partner <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("qnt")]	  
    setorder(dt_partner, "partnername","year")
    p2a_qnt <- plot_ly(dt_partner, x = ~partnername, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p2a_qnt$elementId <- NULL
    p2a_qnt
  })    

  output$p2b_qnt <- renderPlotly({
    dt_product <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("qnt")]	  
    setorder(dt_product, "productgroup","year")
    p2b_qnt <- plot_ly(dt_product, x = ~productgroup, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p2b_qnt$elementId <- NULL
    p2b_qnt
  })
  
  output$p2a_rwe <- renderPlotly({
    dt_partner <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("rwe")]	  
    setorder(dt_partner, "partnername","year")
    p2a_rwe <- plot_ly(dt_partner, x = ~partnername, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p2a_rwe$elementId <- NULL
    p2a_rwe
  })    
  
  output$p2b_rwe <- renderPlotly({
    dt_product <- eumemberdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(productgroup, year), .SDcols=c("rwe")]	  
    setorder(dt_product, "productgroup","year")
    p2b_rwe <- plot_ly(dt_product, x = ~productgroup, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p2b_rwe$elementId <- NULL
    p2b_rwe
  })
  
  output$p3a_val <- renderPlotly({
    dt_partner <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("val")]	  
    setorder(dt_partner, "partnername","year")
    p2a_val <- plot_ly(dt_partner, x = ~partnername, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p2a_val$elementId <- NULL
    p2a_val
  })    
  output$p3b_val <- renderPlotly({
    dt_declarant <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("val")]	  
    setorder(dt_declarant, "declarantname","year")
    p3b_val <- plot_ly(dt_declarant, x = ~declarantname, y = ~val, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_val)
    p3b_val$elementId <- NULL
    p3b_val
  })
  
  output$p3a_qnt <- renderPlotly({
    dt_partner <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("qnt")]	  
    setorder(dt_partner, "partnername","year")
    p2a_qnt <- plot_ly(dt_partner, x = ~partnername, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p2a_qnt$elementId <- NULL
    p2a_qnt
  })    
  output$p3b_qnt <- renderPlotly({
    dt_declarant <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("qnt")]	  
    setorder(dt_declarant, "declarantname","year")
    p3b_qnt <- plot_ly(dt_declarant, x = ~declarantname, y = ~qnt, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_qnt)
    p3b_qnt$elementId <- NULL
    p3b_qnt
  })
  
  output$p3a_rwe <- renderPlotly({
    dt_partner <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(partnername, year), .SDcols=c("rwe")]	  
    setorder(dt_partner, "partnername","year")
    p2a_rwe <- plot_ly(dt_partner, x = ~partnername, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p2a_rwe$elementId <- NULL
    p2a_rwe
  })    
  output$p3b_rwe <- renderPlotly({
    dt_declarant <- productdata_full()[, lapply(.SD, sum, na.rm=TRUE), by=.(declarantname, year), .SDcols=c("rwe")]	  
    setorder(dt_declarant, "declarantname","year")
    p3b_rwe <- plot_ly(dt_declarant, x = ~declarantname, y = ~rwe, type = "bar", split = ~year, color = ~year, colors="OrRd") %>% 
      layout(xaxis = ax, yaxis = ay_rwe)
    p3b_rwe$elementId <- NULL
    p3b_rwe
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
    
  output$downloadData_all4 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_nonvpapartner_full_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      headertxt <- paste("EU member state imports of timber products from ", input$nonvpapartnerinput, ": annual Value in 1000 euro & quantity in metric tonnes & rwe in cubic meters", sep="")
      write.table_with_header(vpapartnerdata_full(), file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_val4 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_nonvpapartner_val_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- vpapartnerdata_full() %>% 
        dcast(declarantname + productgroup ~ year, value.var = "val") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member state imports of timber products from ", input$nonvpapartnerinput, ": annual Value in 1000 euro", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  output$downloadData_qnt4 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_nonvpapartner_qnt_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- vpapartnerdata_full() %>% 
        dcast(declarantname + productgroup ~ year, value.var = "qnt") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member state imports of timber products from ", input$nonvpapartnerinput, ": annual quantity in metric tonnes", sep="")
      write.table_with_header(dt, file, headertxt, sep = ",", row.names=FALSE)
    }
  )
  
  
  output$downloadData_rwe4 <- downloadHandler(
    
    filename = function() {
      paste("imm_stats_nonvpapartner_rwe_", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      dt <- vpapartnerdata_full() %>% 
        dcast(declarantname + productgroup ~ year, value.var = "rwe") %>% 
        setorder(-"yr_2017", na.last = TRUE)
      headertxt <- paste("EU member state imports of timber products from ", input$nonvpapartnerinput, ": annual roundwood equivalent volume in cubic meters", sep="")
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
        partnercodevector <- vpapartners[vpagroup==input$vpagroupinput, partnercode]
      } else {
        partnercodevector <- vpapartners[partner==input$vpapartnerinput, partnercode]
      } 
    } else if (input$sbmenu == "nonvpapartners") {
      # if non-vpa partners is chosen - vector contains selected non-vpa supplier
      partnercodevector <- nonvpapartners[partner==input$nonvpapartnerinput, partnercode]
    } else {
      # vpa partners and non-vpa partners tab not chosen - vector contains all vpa partners
      partnercodevector <- vpapartners[, partnercode]
    }
    return(partnercodevector)
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

