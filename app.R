library(shiny)
# library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(dplyr)
library(plotly)

L <- readRDS("./Data/L.Rda")
M <- readRDS("./Data/M.Rda")
N <- readRDS("./Data/N.Rda")
B <- readRDS("./Data/B.Rda")
D <- readRDS("./Data/D.Rda")
indicators = readRDS("./Data/Indicator_units.Rda")
sector_list = readRDS("Data/SectorList.Rda")
sector_DF = readRDS("./Data/sector_lookup.Rda")

useeio_link = a("USEEIO Page", href="https://www.epa.gov/land-research/us-environmentally-extended-input-output-useeio-technical-content")
naics_link = a("NAICS Page", href="https://www.census.gov/naics/")
ui <- fluidPage(
  titlePanel("USD ISYE 385 EIO-LCA (last update 3/3/2024)"),
  sidebarLayout(
    sidebarPanel(
      submitButton("Update Model", width="100%"),
      # width = 3,
      hr(),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector",
                 label = "Select sector #1",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value", "Amount for #1 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector2",
                 label = "Select sector #2",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value2", "Amount for #2 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector3",
                 label = "Select sector #3",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value3", "Amount for #3 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector4",
                 label = "Select sector #4",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value4", "Amount for #4 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector5",
                 label = "Select sector #5",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value5", "Amount for #5 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector6",
                 label = "Select sector #6",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value6", "Amount for #6 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector7",
                 label = "Select sector #7",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value7", "Amount for #7 ($M)", value=0, min = 0)
        )
      ),
      fluidRow(
        column(width=6,
               selectInput(
                 inputId = "select_sector8",
                 label = "Select sector #8",
                 choices = sector_list,
                 selected = sector_list[1],
                 multiple = FALSE
               )
        ),
        column(width=6,
               numericInput("input_value8", "Amount for #8 ($M)", value=0, min = 0)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Instructions",
          hr(),
          h4("Welcome to our EIO-LCA tool based on the EPA USEEIO models. General instructions are provided below:"),
          tags$ul(
            tags$li(tags$b("NAICS Look Up"), " - use this page to find the relevant sectors using NAICS codes"),
            tags$li(tags$b("Sidebar: Select sector"), " - enter the NAICS sector"),
            tags$li(tags$b("Sidebar: Enter amount ..."), " - enter the input amount in 2012 US dollars"),
            tags$li(tags$b("Sidebar: Add more sectors"), " - click to add more sectors to analysis"),
            tags$li(tags$b("Sidebar: Run model"), " - check all input values then click to Run/Update model"),
            tags$li(tags$b("Model Output"), " - see summary of model results"),
            tags$li(tags$b("By Sector"), " - see results by sector and category"),
            tags$li(tags$b("Units"), " - units and descriptions of indicators"),
            tags$li("More information on USEEIO:", useeio_link),
            tags$li("More information on NAICS:", naics_link)
          ),
          h4(tags$b("Make sure to press UPDATE MODEL whenever you make any changes!"))
        ),
        tabPanel(
          "NAICS Look Up",
          hr(),
          fluidRow(h4("Search for NAIC codes using the Search feature")),
          br(),
          dataTableOutput('sector_lookup')
        ),
        tabPanel(
          "Model Output",
		      hr(),
          p("The EIO-LCA model output based on your inputs:"),
		      tags$ul(
			      tags$li(tags$b("Sector:"),h4(textOutput("sector_selected"))),
			      tags$li(tags$b("Amount:"),h4(textOutput("amount_entered"))),
			      br(),
			      hr(),
			      selectInput(
			        inputId = "select_group_tab2",
			        label = "Select Category",
			        choice = c("Impact Potential","Resource Use","Waste Generated","Chemical Releases","Economic & Social"),
			        selected = "Impact Potential",
			        multiple = FALSE
			      ),
			      withSpinner(plotlyOutput('matrix_table_barplot')),
			      hr(),
            withSpinner(dataTableOutput('matrix_table_totals'))
			      )
        ),
        tabPanel(
          "By Sector",
          selectInput(
            inputId = "select_group",
            label = "Select Group",
            choices = unique(indicators$Group),
            selected = sector_list[1],
            multiple = FALSE
          ),
          selectInput(
            inputId = "select_sub_group",
            label = "Select Group",
            choices = unique(indicators$Group),
            selected = sector_list[1],
            multiple = FALSE
          ),
          selectInput(
            inputId = "select_top_n",
            label = "Top:",
            choices = c(10, 20, 40, 100),
            selected = 10
          ),
          hr(),
          downloadButton("downloadGroupData","Download Current Table"),
          br(),
          withSpinner(dataTableOutput("matrix_table"))
        ),
        tabPanel(
          "Units",
          withSpinner(dataTableOutput('indicator_lookup'))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  formatDemandVector <- function(dv,L) {
    #create a named vector using the first col of L
    d <- c(L[,1])
    #Set all values to 0
    d[0:nrow(L)] <- 0
    #merge in names from dv
    #replace the values from d
    d[match(names(dv),names(d))] <- dv
    return(d)
  }
  update_demand <- function(sec1,val1,sec2="1111A0",val2=0,sec3="1111A0",val3=0,sec4="1111A0",val4=0,sec5="1111A0",val5=0,sec6="1111A0",val6=0,sec7="1111A0",val7=0,sec8="1111A0",val8=0){
    demand_df = data.frame(Code=c(sec1,sec2,sec3,sec4,sec5,sec6,sec7,sec8),value=c(val1,val2,val3,val4,val5,val6,val7,val8))
    demand_df$Code = paste0(demand_df$Code, "/US")
    demand_df$value = demand_df$value*1000000
    demand_df = demand_df %>% group_by(Code) %>% summarize(value=sum(value), .groups = "drop")
    demand_df = data.frame(demand_df)
    y = setNames(demand_df$value, demand_df$Code)
    return(y)
  }
  getScalingVector <- function(L, demand) {
    s <- L %*% demand
    return(s)
  }
  calculateDirectPerspectiveLCI <- function(B, s) {
    lci_d <- t(B %*% diag(as.vector(s), nrow(s)))
    rownames(lci_d) <- rownames(s)
    return(lci_d)
  }
  calculateDirectPerspectiveLCIA <- function(D, s) {
    lcia_d <- t(D %*% diag(as.vector(s), nrow(s)))
    rownames(lcia_d) <- rownames(s)
    return(lcia_d)
  }
  find_results <- function(Lin, Din, demand){
    y = demand
    f = formatDemandVector(y, Lin)
    f = as.matrix(f)
    s <- getScalingVector(Lin, f)
    DF <- calculateDirectPerspectiveLCIA(Din, s)
    DF = as.data.frame(DF)
    DF$Sector = rownames(DF)
    last_col_index = ncol(DF)
    DF = DF[,c(last_col_index, 1:(last_col_index-1))]
    rownames(DF) = 1:nrow(DF)
    return(DF)
  }
  
  matrix_table_func <- function(Lin, Din, demand, indicators, sub_sector, sub_sector2, top_n){
    DF_in = find_results(Lin, Din, demand)
    DF_sub = DF_in[,indicators$Name[indicators$Group==sub_sector]]
    if(sub_sector2!="All"){
      DF_sub = DF_sub[,indicators$Name[indicators$Name==sub_sector2]]
      DF_sub = cbind(DF['Sector'],data.frame(sub_sector2=DF_sub))
      colnames(DF_sub)[2] = sub_sector2
    } else{
      DF_sub = cbind(DF['Sector'],DF_sub)
    }
    DF_sectors = sector_DF[c("Code","Name")]
    DF_sectors$Code = paste0(DF_sectors$Code,"/US")
    colnames(DF_sectors)[1] = "Sector"
    DF_sub = left_join(DF_sectors, DF_sub, by="Sector")
    
    if(sub_sector2=="All"){
      DF_sub$Total = rowSums(DF_sub[,c(3:dim(DF_sub)[2])])
      Total_by_col = colSums(DF_sub[,c(3:dim(DF_sub)[2])])
      headers = names(Total_by_col)
    } else{
      DF_sub$Total = DF_sub[,3]
      Total_by_col = sum(DF_sub$Total)
    }
    DF_sub = DF_sub %>% arrange(-Total)
    DF_sub$Percentage = DF_sub$Total/sum(DF_sub$Total)
    DF_sub = DF_sub[1:top_n,]
    
    if(sub_sector2=="All"){
      DF_sub_add = data.frame(t(as.matrix(Total_by_col)))
      colnames(DF_sub_add) = headers
      DF_sub_add$Sector = "Total"
      DF_sub_add$Name = ""
      DF_sub_add$Percentage = 1
      DF_sub = rbind(DF_sub, DF_sub_add)
      DF_sub$Percentage = sprintf("%.2f%%", DF_sub$Percentage*100)
    } else{
      DF_sub_add = DF_sub[1,]
      DF_sub_add$Sector = "Total"
      DF_sub_add$Name = ""
      DF_sub_add$Percentage = 1
      DF_sub_add[,3] = Total_by_col
      DF_sub_add[,4] = Total_by_col
      DF_sub$Percentage = sprintf("%.2f%%", DF_sub$Percentage*100)
    }
    return(DF_sub)
  }
  
  sector_output <- function(sec1,val1,sec2="1111A0",val2=0,sec3="1111A0",val3=0,sec4="1111A0",val4=0,sec5="1111A0",val5=0,sec6="1111A0",val6=0,sec7="1111A0",val7=0,sec8="1111A0",val8=0){
    demand_df = update_demand(sec1,val1,sec2,val2,sec3,val3,sec4,val4,sec5,val5,sec6,val6,sec7,val7,sec8,val8)
    demand_df = demand_df[demand_df!=0]
    paste(names(demand_df))    
  }
  amount_output <- function(sec1,val1,sec2="1111A0",val2=0,sec3="1111A0",val3=0,sec4="1111A0",val4=0,sec5="1111A0",val5=0,sec6="1111A0",val6=0,sec7="1111A0",val7=0,sec8="1111A0",val8=0){
    demand_df = update_demand(sec1,val1,sec2,val2,sec3,val3,sec4,val4,sec5,val5,sec6,val6,sec7,val7,sec8,val8)
    demand_df = demand_df[demand_df!=0]
    paste0("$",demand_df," million")   
  }
  
  output$sector_selected <- renderText({
    sector_output(input$select_sector, input$input_value,
                  input$select_sector2, input$input_value2,
                  input$select_sector3, input$input_value3,
                  input$select_sector4, input$input_value4,
                  input$select_sector5, input$input_value5,
                  input$select_sector6, input$input_value6,
                  input$select_sector7, input$input_value7,
                  input$select_sector8, input$input_value8)  
  }) 
  output$amount_entered <- renderText({
    amount_output(input$select_sector, input$input_value,
                  input$select_sector2, input$input_value2,
                  input$select_sector3, input$input_value3,
                  input$select_sector4, input$input_value4,
                  input$select_sector5, input$input_value5,
                  input$select_sector6, input$input_value6,
                  input$select_sector7, input$input_value7,
                  input$select_sector8, input$input_value8)  
  })
  
  output$matrix_table_barplot <- renderPlotly({
    demands = update_demand(input$select_sector, input$input_value,
                                           input$select_sector2, input$input_value2,
                                           input$select_sector3, input$input_value3,
                                           input$select_sector4, input$input_value4,
                                           input$select_sector5, input$input_value5,
                                           input$select_sector6, input$input_value6,
                                           input$select_sector7, input$input_value7,
                                           input$select_sector8, input$input_value8
                                           )
    DF = find_results(L, D, demands)
    totals = colSums(DF[,c(2:dim(DF)[2])])
    totals = as.data.frame(totals)
    colnames(totals) = "Total"
    totals$Name = rownames(totals)
    last_col_index = ncol(totals)
    totals = totals[,c(last_col_index, 1:(last_col_index-1))]
    rownames(totals) = 1:nrow(totals)
    totals = left_join(totals, indicators, by="Name")
    totals = totals %>% filter(Group==input$select_group_tab2)
    totals %>% ggplot(aes(x=Code,y=Total))+geom_bar(stat="identity", fill = "red") +
      labs(title= input$select_group_tab2, x="Categories", y="Values")
  })
  
  output$matrix_table_totals <- renderDataTable({
    demands = update_demand(input$select_sector, input$input_value,
                            input$select_sector2, input$input_value2,
                            input$select_sector3, input$input_value3,
                            input$select_sector4, input$input_value4,
                            input$select_sector5, input$input_value5,
                            input$select_sector6, input$input_value6,
                            input$select_sector7, input$input_value7,
                            input$select_sector8, input$input_value8
                            )
    DF = find_results(L, D, demands)
    totals = colSums(DF[,c(2:dim(DF)[2])])
    totals = as.data.frame(totals)
    colnames(totals) = "Total"
    totals$Name = rownames(totals)
    last_col_index = ncol(totals)
    totals = totals[,c(last_col_index, 1:(last_col_index-1))]
    rownames(totals) = 1:nrow(totals)
    totals = left_join(totals, indicators, by="Name")
    specific_order = c("Impact Potential","Resource Use","Waste Generated","Chemical Releases","Economic & Social")
    totals = totals %>% arrange(match(Group,specific_order))
    totals = totals %>% filter(Group==input$select_group_tab2)
    totals %>% mutate(across(all_of("Total"), ~format(., big.mark = ",", decimal.mark = ".", scientific = TRUE)))
  })

  observe({
    x <- input$select_group
    newlist = as.list(indicators$Name[indicators$Group==x])
    newlist = c(newlist, "All")
    updateSelectInput(session, "select_sub_group",
                      choices = newlist,
                      selected = tail(newlist,1)
    )
  })
  
  output$matrix_table <- renderDataTable({
    demands = update_demand(input$select_sector, input$input_value,
                            input$select_sector2, input$input_value2,
                            input$select_sector3, input$input_value3,
                            input$select_sector4, input$input_value4,
                            input$select_sector5, input$input_value5,
                            input$select_sector6, input$input_value6,
                            input$select_sector7, input$input_value7,
                            input$select_sector8, input$input_value8)
    DF = matrix_table_func(L, D, demands, indicators, input$select_group, input$select_sub_group, input$select_top_n)
    if(input$select_sub_group=="All"){
      a = indicators$Name[indicators$Group==input$select_group]
      a = c(a, "Total")
      DF %>% mutate(across(all_of(a), ~format(., big.mark = ",", decimal.mark = ".", scientific = FALSE)))
    } else{
      DF
    }
    
    # datatable(DF, options=list(pageLength=35, rownames=FALSE))
  })
  output$downloadGroupData <- downloadHandler(
    filename = function(){paste(input$select_sector,"_",Sys.Date(),".csv",sep="")},
    content = function(file) {
      write.csv(matrix_table_func(L, D, update_demand(input$select_sector, input$input_value,
                                                       input$select_sector2, input$input_value2,
                                                       input$select_sector3, input$input_value3,
                                                       input$select_sector4, input$input_value4,
                                                       input$select_sector5, input$input_value5,
                                                       input$select_sector6, input$input_value6,
                                                       input$select_sector7, input$input_value7,
                                                       input$select_sector8, input$input_value8), indicators, input$select_group, input$select_sub_group, input$select_top_n),file,row.names = FALSE)
    }
  )
  
  output$sector_lookup <- renderDataTable({
    sector_DF
  })
  output$indicator_lookup <- renderDataTable({
    indicators
  })
  # output$LCIA_heatmap <-renderPlot({
  #   create_heatmap(mod1, input$select_sector, input$input_value)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
