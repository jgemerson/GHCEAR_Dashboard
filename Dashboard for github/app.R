##################################################################
#                             Jojo Emerson                       #
#                      GH CEA Registry Dashboard                 #
#                   Main directory: Shiny app file               #
#                     Code changes: April 18, 2019               #
#                    GitHub Upload: April 18, 2019               #
#                             Version 1.0.11                     #
##################################################################


############              REQUIRED FILES              ############

# Main directory: 
# - app.R: R shiny app (current file)
# - cleaning.R: data cleaning file
# - interventions_cleaning.R: data cleaning for interventions specifically
# - DashboardReport.rmd: R Markdown file to create downloadable html report for disease tab
# - DashboardReport_country.rmd: R Markdown file to create downloadable html report for country tab
# - DashboardReport_interv.rmd: R Markdown file to create downloadable html report for intervention tab
# - google-analytics.js: google analytics (removed for GitHub upload)
# - cevr_dark.png and ghcea.png: logos for R Markdown 
# Data directory: 
# - METHODS.xlsx: GH CEA registry methods excel file from update to 2017 data
# - RATIOS.xlsx: GH CEA registry ratios excel file from update to 2017 data
# - COUNTRY CODES.csv: Country geographic information to create plotly map, updated per GH CEA Registry country names
# www directory:
# - BMGF.png: BMGF logo for footer
# - cevr.png: CEVR logo for footer
# rsconnect directory:
# - connection to shinyapps.io account for deployment (removed for GitHub upload)

#call libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(markdown)
library(treemapify)
library(XLConnect)

#call data cleaning
source("cleaning.R")

####UI####
ui <- dashboardPage(
  
  #styling
  skin = 'blue',
  
  #Header
  dashboardHeader(title = "GH CEA Registry Dashboard", titleWidth  = 350
  ), #close header
  
  
  #Sidebar
  dashboardSidebar(
    #default not collapsed
    collapsed = FALSE,
    
    #menu items
    sidebarMenu(
      
      #tabs
      menuItem("Disease", tabName = "disease", icon = icon("search")),
      menuItem("Country", tabName = "country", icon = icon("search")),
      menuItem("Intervention", tabName = "intervention", icon = icon("search")),
      

      #footer with citation, contact, and logos
      tags$div(style = "position:fixed; bottom: 0;padding:25px;",
      tags$br(),
      
      #citation
      tags$a("Cite the GH CEA Registry!", href="http://healtheconomics.tuftsmedicalcenter.org/orchard/cite-the-gh-cea-registry", target = "_blank"),
      
      tags$br(),
      "Developer:",
      tags$br(),
      tags$a("Joanna Emerson", href="mailto:jemerson@tuftsmedicalcenter.org"),
      
      tags$br(), tags$br(),
      tags$a(tags$img(src ="cevr.png",  width = "120px", height = "60px"), href="http://healtheconomics.tuftsmedicalcenter.org/orchard", target = "_blank"),
      tags$br(),
      tags$a(tags$img(src = "BMGF_white.png"), href="https://www.gatesfoundation.org", target = "_blank"))
    )#close sidebar menu 
  ), #close sidebar
  
  
  #Body
  dashboardBody(
    
    #link to google analytics javascript file (removed for Github)
    #tags$head(includeScript("google-analytics.js")),
    
    tabItems(
          
      ###Disease tab####
      tabItem(tabName = "disease",
        fluidRow(
          #change font
          tags$head(tags$style(HTML('
            * {
            font-family: Arial, sans-serif; !important
            }
          '))),
                
                
          #Disease input filter  
          column(width = 12, align = "center",
                 selectizeInput('disease', label = "Select your disease:", multiple = FALSE, options = list(placeholder = 'Begin typing disease...'),
                 choices = c("All diseases", disease_choices))
        
          )# close column 1
        ), # close row 1,
              
              
        fluidRow(
          #Registry contents table
          box(title = "Registry Contents:", width = 4, 
              solidHeader = TRUE,collapsible=TRUE,
              tableOutput("contents_table")
          ), #close box 1
                
          #Mini league table 
          box(title = "Top 5 Selected Cost-Effective Interventions:", width = 8, 
              solidHeader = TRUE,collapsible=TRUE,
              DT::dataTableOutput("league_table")
          ) #close box 2
        ), #close row 2
              
              
        fluidRow(
          #Map
          box(title = "Density of Ratios by Country", width = 7,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("map")
          ), #close box 1
                
          #Pie chart
          box(
            title = "Distribution of Ratios by Intervention Type", width = 5,
            solidHeader = TRUE,collapsible=TRUE,
            plotlyOutput("piechart")
          )#close box 3
        ),#close row 3
              
        fluidRow(
          #Bar chart of ratios by threshold
          box(title = "Distribution of Ratios by CE threshold", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("stackedbar")
          ),#close box 1
                
          #Stacked bar chart of funders over time
          box(title = "Distribution of CEAs Published Over Time", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput('regbar')
          )#close box 2
        ),#close row 4
        
        #Download buttons      
        fluidRow(
          #report download button
          column(width = 4, align = 'center',
          downloadButton("report", "Download full report", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #methods data download button
          column(width = 4, align = 'center',
            downloadButton("methodsdata_disease", "Download methods dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #ratios data download button
          column(width = 4, align = 'center',
                 downloadButton("ratiossdata_disease", "Download ratios dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )#close row 5
      ), #close tab 1
      
      ###Country tab####
      tabItem(tabName = "country",
      
        fluidRow(
          #change font
          tags$head(tags$style(HTML('
            * {
            font-family: Arial, sans-serif; !important
            }
          '))),
                
                
          #Country input filter  
          column(width = 12, align = "center",
                 selectizeInput('country', label = "Select your country:", multiple = FALSE, options = list(placeholder = 'Begin typing country...'),
                                choices = c('All countries', Names$Country))
          )# close column 1
        ), # close row 1
              
              
        fluidRow(
          #Registry contents table
          box(title = "Registry Contents:", width = 4, 
              solidHeader = TRUE,collapsible=TRUE,
              tableOutput("contents_table_country")
          ), #close box 1
                
          #Mini league table 
          box(title = "Top 5 Selected Cost-Effective Interventions:", width = 8, 
              solidHeader = TRUE,collapsible=TRUE,
              DT::dataTableOutput("league_table_country")
          ) #close box 2
        ), #close row 2
              
        fluidRow(
          #Pie chart
          box(title = "Distribution of Ratios by Intervention Type", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("piechart_interv_country")
          ), #close box 1
                
          #Treemap
          box(title = "Distribution of CEAs by Disease Area", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotOutput("treemap_country")
          )#close box 3
        ),#close row 3
              
              
        fluidRow(
          #Bar chart of ratios by threshold
          box(title = "Distribution of Ratios by CE threshold", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("stackedbar_country")
          ),#close box 1
            
          #Stacked bar chart of funders over time
          box(title = "Distribution of CEAs Published Over Time", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput('regbar_country')
          )#close box 2
                
        ),#close row 4
              
        #Download buttons
        fluidRow(
          #report download button
          column(width = 4, align = 'center',
            downloadButton("report_country", "Generate downloadable report", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #methods data download button
          column(width = 4, align = 'center',
            downloadButton("methodsdata_country", "Download methods dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #ratios data download button
          column(width = 4, align = 'center',
            downloadButton("ratiosdata_country", "Download ratios dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )#close row 5
              
      ), #close tab 2
      
      ###Intervention tab####
      tabItem(tabName = "intervention",
        fluidRow(
          #change font
          tags$head(tags$style(HTML('
            * {
            font-family: Arial, sans-serif; !important
            }
          '))),
                
                
          #Disease input filter  
          column(width = 12, align = "center",
                 selectizeInput('intervention', label = "Select your intervention:", multiple = FALSE, options = list(placeholder = 'Begin typing intervention...'),
                                choices = c("All interventions",Names$InterventionType))
          )# close column 1
        ),# close row 1
              
        fluidRow(
          #Registry contents table
          box(title = "Registry Contents:", width = 4, 
              solidHeader = TRUE,collapsible=TRUE,
              tableOutput("contents_table_interv")
          ), #close box 1
              
          #Mini league table 
          box(title = "Top 5 Selected Cost-Effective Interventions:", width = 8, 
              solidHeader = TRUE,collapsible=TRUE,
              DT::dataTableOutput("league_table_interv")
          ) #close box 2
        ), #close row 2
              
        fluidRow(
          #Map
          box(title = "Density of Ratios by Country", width = 7,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("map_interv")
          ), #close box 1
            
          #Pie chart
          box(
            title = "Distribution of CEAs by Disease Area", width = 5,
            solidHeader = TRUE,collapsible=TRUE,
            plotOutput("treemap_interv")
          )#close box 3
        ),#close row 3
              
        fluidRow(
          #Bar chart of ratios by threshold
          box(title = "Distribution of Ratios by CE threshold", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput("stackedbar_interv")
          ),#close box 1
                
          #Stacked bar chart of funders over time
          box(title = "Distribution of CEAs Published Over Time", width = 6,
              solidHeader = TRUE,collapsible=TRUE,
              plotlyOutput('regbar_interv')
          )#close box 2
                
        ),#close row 4
        
        #Download buttons    
        fluidRow(
          #report download button
          column(width = 4, align = 'center',
            downloadButton("report_interv", "Generate downloadable report", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #methods data download button
          column(width = 4, align = 'center',
            downloadButton("methodsdata_interv", "Download methods dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          #ratios data download button
          column(width = 4, align = 'center',
            downloadButton("ratiosdata_interv", "Download ratios dataset", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )#close row 5
      ) #close tab 3
    ) #close tab items
  ) #close dashboard body
) #close UI

####SERVER####
server <- function(input, output) {
  
  ###DISEASE TAB####
  
  #Create reactive table of methods, subset by filter selection
  subsetted_methods<-reactive({
    ifelse(input$disease == "All diseases",
            return(methods_raw),
            return(subset(methods_raw, methods_raw$GBDDis1_tier1 %in% input$disease | methods_raw$GBDDis1_tier2  %in% input$disease 
                   | methods_raw$GBDDis1_tier3  %in% input$disease | methods_raw$GBDDis1_tier4 %in% input$disease
                   | methods_raw$GBDDis2_tier1 %in% input$disease | methods_raw$GBDDis2_tier2  %in% input$disease 
                   | methods_raw$GBDDis2_tier3  %in% input$disease | methods_raw$GBDDis2_tier4 %in% input$disease
                   | methods_raw$GBDDis3_tier1 %in% input$disease | methods_raw$GBDDis3_tier2  %in% input$disease 
                   | methods_raw$GBDDis3_tier3  %in% input$disease | methods_raw$GBDDis3_tier4 %in% input$disease
                   | methods_raw$GBDDis4_tier1 %in% input$disease | methods_raw$GBDDis4_tier2  %in% input$disease 
                   | methods_raw$GBDDis4_tier3  %in% input$disease | methods_raw$GBDDis4_tier4 %in% input$disease
                   | methods_raw$GBDDis5_tier1 %in% input$disease | methods_raw$GBDDis5_tier2  %in% input$disease 
                   | methods_raw$GBDDis5_tier3  %in% input$disease | methods_raw$GBDDis5_tier4 %in% input$disease))
    )
  }) #close subsetted_methods table
  
  #Create reactive table of ratios, subset by filter selection
  subsetted_ratios<-reactive({
    ifelse(input$disease == "All diseases",
           return(leaguetable_data),
           return(subset(leaguetable_data, leaguetable_data$GBDDis1_tier1 %in% input$disease | leaguetable_data$GBDDis1_tier2  %in% input$disease 
                   | leaguetable_data$GBDDis1_tier3  %in% input$disease | leaguetable_data$GBDDis1_tier4 %in% input$disease
                   | leaguetable_data$GBDDis2_tier1 %in% input$disease | leaguetable_data$GBDDis2_tier2  %in% input$disease 
                   | leaguetable_data$GBDDis2_tier3  %in% input$disease | leaguetable_data$GBDDis2_tier4 %in% input$disease
                   | leaguetable_data$GBDDis3_tier1 %in% input$disease | leaguetable_data$GBDDis3_tier2  %in% input$disease 
                   | leaguetable_data$GBDDis3_tier3  %in% input$disease | leaguetable_data$GBDDis3_tier4 %in% input$disease
                   | leaguetable_data$GBDDis4_tier1 %in% input$disease | leaguetable_data$GBDDis4_tier2  %in% input$disease 
                   | leaguetable_data$GBDDis4_tier3  %in% input$disease | leaguetable_data$GBDDis4_tier4 %in% input$disease
                   | leaguetable_data$GBDDis5_tier1 %in% input$disease | leaguetable_data$GBDDis5_tier2  %in% input$disease 
                   | leaguetable_data$GBDDis5_tier3  %in% input$disease | leaguetable_data$GBDDis5_tier4 %in% input$disease))
    )
  }) #close subsetted_ratios table
  
  #Create reactive vector of Registry contents: # of studies and ratios, range of ICERs, # of interventions and countries
  reg_contents<-reactive({
    
    #Number of studies
    num_studies<-nrow(subsetted_methods())
    
    #Number of ratios
    num_ratios<-nrow(subsetted_ratios())
    
    #Range of ICERs
    suppressWarnings({
      high_ICER<-max(subsetted_ratios()$NumericICER)
      high_ICER<-ifelse(high_ICER == -999999, "Cost-Saving", high_ICER)
      high_ICER<-ifelse(high_ICER == 9999999, "Dominated", high_ICER)
      low_ICER<-min(subsetted_ratios()$NumericICER)
      low_ICER<-ifelse(low_ICER == -999999, "Cost-Saving", low_ICER)
      low_ICER<-ifelse(low_ICER == 9999999, "Dominated", low_ICER)
      range_ICER<-paste(low_ICER,high_ICER, sep = " - ")
    })
    
    #Number of Interventons
    unique_interventions<-length(unique(subsetted_ratios()$InterventionPhrase))
    
    #Number of countries
    unique_countries<-length(unique(subsetted_ratios()$Country))
    
    #Create and return named vector
    reg_contents<-c(num_studies, num_ratios, range_ICER, unique_interventions, unique_countries)
    names(reg_contents)<-c("Number of studies","Number of ratios", "Range of ICERs (2016 USD)", "Number of unique interventions", "Number of countries represented")
   
    return(reg_contents)
    
  }) #close reg_content table
  #Render reg_contents output table
  output$contents_table<-renderTable(reg_contents(), rownames = TRUE, colnames = FALSE, title = "Registry contents:")
  
  #Mini league table
  league_table<-reactive({
    #order subsetted ratios from most to least cost effective
    ordered_ratios <- subsetted_ratios()[order(subsetted_ratios()$NumericICER),] 
    #reduce to only top 5
    ordered_ratios<-ordered_ratios[1:5,]
    #create output table format
    league_table<-data.frame(ordered_ratios$InterventionCombined, ordered_ratios$TargetPopulation, ordered_ratios$DisplayRatio, ordered_ratios$TitleAut)
    colnames(league_table)<-c("Intervention", "Target Population", "Cost/DALY averted", "Source (Title, Author)")
    return(league_table)
  })
  #Render league_table output table
  output$league_table<-renderDataTable(
    league_table(),
    options = list(
      searching = FALSE,
      pageLength = 5, 
      dom = 't'
    ),
    rownames = FALSE)
  
  #MAP
  map<-reactive({
    #require a disease input
    req(input$disease)
    
    #create number of ratios dataframe
    numratios_country<-data.frame(table(subsetted_ratios()$Country))
    colnames(numratios_country)<-c("COUNTRY", "NUM_STUDIES")
    
    #merge with shapefile
    df <- merge(countries_data, numratios_country, on = "COUNTRY")
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    
    map <- plot_geo(df) %>%
      add_trace(
        z = ~NUM_STUDIES, color = ~NUM_STUDIES, colors = 'Blues',
        text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
      ) %>%
      colorbar(title = 'Number of cost/DALY Ratios') %>%
      layout(
        geo = g,
        images = list(
          list(
            source = "cevr_dark.png",
               xref = "paper",
               yref = "paper",
               x= .85,
               y= .2,
               sizex = 0.2,
               sizey = 0.2,
               opacity = 0.6
          )
        )
      )
    
    return(map)
  })
  #render map output
  output$map <- renderPlotly(map())
  
  #Pie chart of intervention types
  pie <- reactive({
    subsetted_ratios() %>%
      group_by(Intervention1) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Intervention1, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= 0,
                 y= .1,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.6
               )
             ))
  })
  #render Pie chart
  output$piechart <- renderPlotly(pie())
  
  
  #Stacked bar of thresholds
  stackedbar<-reactive({
    #require a disease input
    req(input$disease)
    
    #create stacked bar dataframe
    R_CS<-sum(subsetted_ratios()$R_CS)
    R_LT1<-sum(subsetted_ratios()$R_LT1)
    R_1to3<-sum(subsetted_ratios()$R_1to3)
    R_GT3<-sum(subsetted_ratios()$R_GT3)
    R_D<-sum(subsetted_ratios()$R_D)
    Thresholds<-data.frame(R_CS, R_LT1, R_1to3, R_GT3,R_D)
    colnames(Thresholds)<-c("R_CS", "R_LT1", "R_1to3", "R_GT3", "R_D")
    
    #create ratios stacked bar chart, stratified by ratios
    stackedbar<-plot_ly(Thresholds, x = ~Thresholds, y = ~R_CS, type = 'bar', name = 'Cost-Saving', 
                        hoverinfo = "y") %>%
      add_trace(y = ~R_LT1, name = 'Less than 1x GDP') %>%
      add_trace(y = ~R_1to3, name = '1-3x GDP') %>%
      add_trace(y = ~R_GT3, name = 'Greater than 3x GDP') %>%
      add_trace(y = ~R_D, name = 'Dominated') %>%
      layout(yaxis = list(title = 'Number of cost/DALY averted ratios'),
             xaxis = list(title = "", showticklabels=FALSE),
             barmode = 'stack',
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= 1,
                 y= 0,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.6
               )
             ))

    return(stackedbar)
  }) #close reactive
  #render stacked bar chart
  output$stackedbar <- renderPlotly(stackedbar())
  
  #REGULAR BAR of articles
  regbar<-reactive({
    req(input$disease)
    
    #create bar dataset
    
    articlecount<-aggregate(subsetted_methods()$ArticleID, by = subsetted_methods()["PubYear"] , FUN = length)
    colnames(articlecount)<-c("Year", "Article Count")
    
    articlecount$Year <- factor(articlecount$Year)
    
    regbar <- plot_ly(articlecount, x = ~Year, y = ~`Article Count`, type = "bar",
                          marker = list(color = 'rgb(158,202,225)',
                                        line = list(color = 'rgb(8,48,107)', width = 1.0))) %>%
      layout(xaxis = list(title = "", showticklabels=TRUE),
             yaxis = list(title = "Number of cost/DALY CEAs"),
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= .1,
                 y= 1,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.4
               )
             ))
    
    return(regbar)
  })
  #render regular bar chart
  output$regbar<-renderPlotly(regbar())

  #DOWNLOADABLE REPORT
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "DashboardReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it
      src <- normalizePath('DashboardReport.rmd')
      src2 <- normalizePath('cevr_dark.png')  
      src3 <- normalizePath('ghcea.png') 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'DashboardReport.rmd')
      file.copy(src2, 'cevr_dark.png')
      file.copy(src3, 'ghcea.png')
      
      # Set up parameters to pass to Rmd document
      params <- list(disease = input$disease, methods = subsetted_methods(), ratios = subsetted_ratios(), countries = countries_data,
                     logo = "cevr_dark.png", logo2 = "ghcea.png")
      
      # # Knit the document, passing in the `params` list
      rmarkdown::render('DashboardReport.rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    })#close download handler
  
  #Downloadable csv of filtered dataset, METHODS ----
  output$methodsdata_disease<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDmethods_disease.csv",
    content = function(file) {
      write.csv(subsetted_methods(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  #Downloadable csv of filtered dataset, RATIOS ----
  output$ratiossdata_disease<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDratios_disease.csv",
    content = function(file) {
      write.csv(subsetted_ratios(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  
  ###COUNTRY TAB####
  
  #Create reactive table of methods, subset by filter selection
  subsetted_methods_country<-reactive({
    ifelse(input$country == "All countries",
           return(methods_raw),
            return(subset(methods_raw,
                           Country_Afghanistan_s %in% input$country | 	Country_Albania_s %in% input$country | 	Country_Algeria_s %in% input$country | 	
                           Country_AmericanSamoa_s %in% input$country | 	Country_Andorra_s %in% input$country | 	Country_Angola_s %in% input$country | 	Country_AntiguaandBarbuda_s %in% input$country | 
                           Country_Argentina_s %in% input$country | 	Country_Armenia_s %in% input$country | 	Country_Aruba_s %in% input$country | 	Country_Australia_s %in% input$country | 
                           Country_Austria_s %in% input$country | 	Country_Azerbaijan_s %in% input$country | 	Country_Bahamas_s %in% input$country | 	Country_Bahrain_s %in% input$country | 
                           Country_Bangladesh_s %in% input$country | 	Country_Barbados_s %in% input$country | 	Country_Belarus_s %in% input$country | 	Country_Belgium_s %in% input$country | 
                           Country_Belize_s %in% input$country | 	Country_Benin_s %in% input$country | 	Country_Bermuda_s %in% input$country | 	Country_Bhutan_s %in% input$country | 
                           Country_Bolivia_s %in% input$country | 	Country_BosniaandHerzegovina_s %in% input$country | 	Country_Botswana_s %in% input$country | 	Country_Brazil_s %in% input$country | 
                           Country_BritishVirginIslands_s %in% input$country | 	Country_Brunei_s %in% input$country | 	Country_Bulgaria_s %in% input$country | 	
                           Country_BurkinaFaso_s %in% input$country | 	Country_Burundi_s %in% input$country | 	Country_CaboVerde_s %in% input$country | 	Country_Cambodia_s %in% input$country | 
                           Country_Cameroon_s %in% input$country | 	Country_Canada_s %in% input$country | 	Country_CaymanIslands_s %in% input$country | 	
                           Country_CentralAfricanRepublic_s %in% input$country | 	Country_Chad_s %in% input$country | 	Country_ChannelIslands_s %in% input$country | 	
                           Country_Chile_s %in% input$country | 	Country_China_s %in% input$country | 	Country_Colombia_s %in% input$country | 	Country_Comoros_s %in% input$country | 
                           Country_CongoDemRep_s %in% input$country | 	Country_CongoRep_s %in% input$country | 	Country_CostaRica_s %in% input$country | 	Country_CotedIvoire_s %in% input$country | 
                           Country_Croatia_s %in% input$country | 	Country_Cuba_s %in% input$country | 	Country_Curaco_s %in% input$country | 	Country_Cyprus_s %in% input$country | 
                           Country_CzechRepublic_s %in% input$country | 	Country_Denmark_s %in% input$country | 	Country_Djibouti_s %in% input$country | 	Country_Dominica_s %in% input$country | 
                           Country_DominicanRepublic_s %in% input$country | 	Country_Ecuador_s %in% input$country | 	Country_EgyptArabRep_s %in% input$country | 	Country_ElSalvador_s %in% input$country | 
                           Country_EquatorialGuinea_s %in% input$country | 	Country_Eritrea_s %in% input$country | 	Country_Estonia_s %in% input$country | 	Country_Ethiopia_s %in% input$country | 
                           Country_FaroeIslands_s %in% input$country | 	Country_Fiji_s %in% input$country | 	Country_Finland_s %in% input$country | 	Country_France_s %in% input$country | 
                           Country_FrenchPolynesia_s %in% input$country | 	Country_Gabon_s %in% input$country | 	Country_GambiaThe_s %in% input$country | 	Country_Georgia_s %in% input$country | 
                           Country_Germany_s %in% input$country | 	Country_Ghana_s %in% input$country | 	Country_Gibraltar_s %in% input$country | 	Country_Greece_s %in% input$country | 
                           Country_Greenland_s %in% input$country | 	Country_Grenada_s %in% input$country | 	Country_Guam_s %in% input$country | 	Country_Guatemala_s %in% input$country | 
                           Country_Guinea_s %in% input$country | 	Country_GuineaBissau_s %in% input$country | 	Country_Guyana_s %in% input$country | 	Country_Haiti_s %in% input$country | 
                           Country_Honduras_s %in% input$country | 	Country_HongKongSARChina_s %in% input$country | 	Country_Hungary_s %in% input$country | 	Country_Iceland_s %in% input$country | 
                           Country_India_s %in% input$country | 	Country_Indonesia_s %in% input$country | 	Country_IranIslamicRep_s %in% input$country | 	Country_Iraq_s %in% input$country | 
                           Country_Ireland_s %in% input$country | 	Country_IsleofMan_s %in% input$country | 	Country_Israel_s %in% input$country | 	Country_Italy_s %in% input$country | 
                           Country_Jamaica_s %in% input$country | 	Country_Japan_s %in% input$country | 	Country_Jordan_s %in% input$country | 	Country_Kazakhstan_s %in% input$country | 
                           Country_Kenya_s %in% input$country | 	Country_Kiribati_s %in% input$country | 	Country_KoreaDemPeoplesRep_s %in% input$country | 	Country_KoreaRep_s %in% input$country | 
                           Country_Kosovo_s %in% input$country | 	Country_Kuwait_s %in% input$country | 	Country_KyrgyzRepublic_s %in% input$country | 	Country_LaoPDR_s %in% input$country | 
                           Country_Latvia_s %in% input$country | 	Country_Lebanon_s %in% input$country | 	Country_Lesotho_s %in% input$country | 	Country_Liberia_s %in% input$country | 
                           Country_Libya_s %in% input$country | 	Country_Liechtenstein_s %in% input$country | 	Country_Lithuania_s %in% input$country | 	Country_Luxembourg_s %in% input$country | 
                           Country_MacaoSARChina_s %in% input$country | 	Country_MacedoniaFYR_s %in% input$country | 	Country_Madagascar_s %in% input$country | 	Country_Malawi_s %in% input$country | 
                           Country_Malaysia_s %in% input$country | 	Country_Maldives_s %in% input$country | 	Country_Mali_s %in% input$country | 	Country_Malta_s %in% input$country | 
                           Country_MarshallIslands_s %in% input$country | 	Country_Mauritania_s %in% input$country | 	Country_Mauritius_s %in% input$country | 	Country_Mexico_s %in% input$country | 
                           Country_MicronesiaFedSts_s %in% input$country | 	Country_Moldova_s %in% input$country | 	Country_Monaco_s %in% input$country | 	Country_Mongolia_s %in% input$country | 
                           Country_Montenegro_s %in% input$country | 	Country_Morocco_s %in% input$country | 	Country_Mozambique_s %in% input$country | 	Country_Myanmar_s %in% input$country | 
                           Country_Namibia_s %in% input$country | 	Country_Nauru_s %in% input$country | 	Country_Nepal_s %in% input$country | 	Country_Netherlands_s %in% input$country | 
                           Country_NewCaledonia_s %in% input$country | 	Country_NewZealand_s %in% input$country | 	Country_Nicaragua_s %in% input$country | 	Country_Niger_s %in% input$country | 
                           Country_Nigeria_s %in% input$country | 	Country_NorthernMarianaIslands_s %in% input$country | 	Country_Norway_s %in% input$country | 	Country_Oman_s %in% input$country | 
                           Country_Pakistan_s %in% input$country | 	Country_Palau_s %in% input$country | 	Country_Panama_s %in% input$country | 	Country_PapuaNewGuinea_s %in% input$country | 
                           Country_Paraguay_s %in% input$country | 	Country_Peru_s %in% input$country | 	Country_Philippines_s %in% input$country | 	Country_Poland_s %in% input$country | 
                           Country_Portugal_s %in% input$country | 	Country_PuertoRico_s %in% input$country | 	Country_Qatar_s %in% input$country | 	Country_Romania_s %in% input$country | 
                           Country_RussianFederation_s %in% input$country | 	Country_Rwanda_s %in% input$country | 	Country_Samoa_s %in% input$country | 	Country_SanMarino_s %in% input$country | 
                           Country_SaoTomeandPrincipe_s %in% input$country | 	Country_SaudiArabia_s %in% input$country | 	Country_Senegal_s %in% input$country | 	Country_Serbia_s %in% input$country | 
                           Country_Seychelles_s %in% input$country | 	Country_SierraLeone_s %in% input$country | 	Country_Singapore_s %in% input$country | 	Country_SintMaartenDutchpart_s %in% input$country | 
                           Country_SlovakRepublic_s %in% input$country | 	Country_Slovenia_s %in% input$country | 	Country_SolomonIslands_s %in% input$country | 	Country_Somalia_s %in% input$country | 
                           Country_SouthAfrica_s %in% input$country | 	Country_SouthSudan_s %in% input$country | 	Country_Spain_s %in% input$country | 	Country_SriLanka_s %in% input$country | 
                           Country_StKittsandNevis_s %in% input$country | 	Country_StLucia_s %in% input$country | 	Country_StMartinFrenchpart_s %in% input$country | 	
                           Country_StVincentandtheGrenadine_s %in% input$country | 	Country_Sudan_s %in% input$country | 	Country_Suriname_s %in% input$country | 	Country_Swaziland_s %in% input$country | 
                           Country_Sweden_s %in% input$country | 	Country_Switzerland_s %in% input$country | 	Country_SyrianArabRepublic_s %in% input$country | 	Country_Tajikistan_s %in% input$country | 
                           Country_Tanzania_s %in% input$country | 	Country_Thailand_s %in% input$country | 	Country_TimorLeste_s %in% input$country | 	Country_Togo_s %in% input$country | 
                           Country_Tonga_s %in% input$country | 	Country_TrinidadandTobago_s %in% input$country | 	Country_Tunisia_s %in% input$country | 	Country_Turkey_s %in% input$country | 
                           Country_Turkmenistan_s %in% input$country | 	Country_TurksandCaicosIslands_s %in% input$country | 	Country_Tuvalu_s %in% input$country | 	Country_Uganda_s %in% input$country | 
                           Country_Ukraine_s %in% input$country | 	Country_UnitedArabEmirates_s %in% input$country | 	Country_UnitedKingdom_s %in% input$country | 	
                           Country_UnitedStates_s %in% input$country | 	Country_Uruguay_s %in% input$country | 	Country_Uzbekistan_s %in% input$country | 	Country_Vanuatu_s %in% input$country | 
                           Country_VenezuelaRB_s %in% input$country | 	Country_Vietnam_s %in% input$country | 	Country_VirginIslandsUS_s %in% input$country | 	Country_WestBankandGaza_s %in% input$country | 
                           Country_YemenRep_s %in% input$country | 	Country_Zambia_s %in% input$country | 	Country_Zimbabwe_s %in% input$country | 	Country_TaiwanChina_s %in% input$country | 
                           Country_CookIslands_s %in% input$country | 	Country_Niue_s %in% input$country | 	Country_WallisandFutuna_s %in% input$country | 	Country_NA_s  %in% input$country
                         ))
           )
  }) #close subsetted_methods_country table
  
  #Create reactive table of ratios, subset by filter selection
  subsetted_ratios_country<-reactive({
    ifelse(input$country == "All countries",
           return(leaguetable_data),
            return(subset(leaguetable_data, leaguetable_data$Country_Afghanistan_s %in% input$country | 	leaguetable_data$Country_Albania_s %in% input$country 
                   | 	leaguetable_data$Country_Algeria_s %in% input$country | 	leaguetable_data$Country_AmericanSamoa_s %in% input$country 
                   | 	leaguetable_data$Country_Andorra_s %in% input$country | 	leaguetable_data$Country_Angola_s %in% input$country 
                   | 	leaguetable_data$Country_AntiguaandBarbuda_s %in% input$country | 	leaguetable_data$Country_Argentina_s %in% input$country 
                   | 	leaguetable_data$Country_Armenia_s %in% input$country | 	leaguetable_data$Country_Aruba_s %in% input$country 
                   | 	leaguetable_data$Country_Australia_s %in% input$country | 	leaguetable_data$Country_Austria_s %in% input$country 
                   | 	leaguetable_data$Country_Azerbaijan_s %in% input$country | 	leaguetable_data$Country_Bahamas_s %in% input$country 
                   | 	leaguetable_data$Country_Bahrain_s %in% input$country | 	leaguetable_data$Country_Bangladesh_s %in% input$country 
                   | 	leaguetable_data$Country_Barbados_s %in% input$country | 	leaguetable_data$Country_Belarus_s %in% input$country 
                   | 	leaguetable_data$Country_Belgium_s %in% input$country | 	leaguetable_data$Country_Belize_s %in% input$country 
                   | 	leaguetable_data$Country_Benin_s %in% input$country | 	leaguetable_data$Country_Bermuda_s %in% input$country 
                   | 	leaguetable_data$Country_Bhutan_s %in% input$country | 	leaguetable_data$Country_Bolivia_s %in% input$country 
                   | 	leaguetable_data$Country_BosniaandHerzegovina_s %in% input$country | 	leaguetable_data$Country_Botswana_s %in% input$country 
                   | 	leaguetable_data$Country_Brazil_s %in% input$country | 	leaguetable_data$Country_BritishVirginIslands_s %in% input$country 
                   | 	leaguetable_data$Country_Brunei_s %in% input$country | 	leaguetable_data$Country_Bulgaria_s %in% input$country 
                   | 	leaguetable_data$Country_BurkinaFaso_s %in% input$country | 	leaguetable_data$Country_Burundi_s %in% input$country 
                   | 	leaguetable_data$Country_CaboVerde_s %in% input$country | 	leaguetable_data$Country_Cambodia_s %in% input$country 
                   | 	leaguetable_data$Country_Cameroon_s %in% input$country | 	leaguetable_data$Country_Canada_s %in% input$country 
                   | 	leaguetable_data$Country_CaymanIslands_s %in% input$country | 	leaguetable_data$Country_CentralAfricanRepublic_s %in% input$country 
                   | 	leaguetable_data$Country_Chad_s %in% input$country | 	leaguetable_data$Country_ChannelIslands_s %in% input$country 
                   | 	leaguetable_data$Country_Chile_s %in% input$country | 	leaguetable_data$Country_China_s %in% input$country 
                   | 	leaguetable_data$Country_Colombia_s %in% input$country | 	leaguetable_data$Country_Comoros_s %in% input$country 
                   | 	leaguetable_data$Country_CongoDemRep_s %in% input$country | 	leaguetable_data$Country_CongoRep_s %in% input$country 
                   | 	leaguetable_data$Country_CostaRica_s %in% input$country | 	leaguetable_data$Country_CotedIvoire_s %in% input$country 
                   | 	leaguetable_data$Country_Croatia_s %in% input$country | 	leaguetable_data$Country_Cuba_s %in% input$country 
                   | 	leaguetable_data$Country_Curaco_s %in% input$country | 	leaguetable_data$Country_Cyprus_s %in% input$country 
                   | 	leaguetable_data$Country_CzechRepublic_s %in% input$country | 	leaguetable_data$Country_Denmark_s %in% input$country 
                   | 	leaguetable_data$Country_Djibouti_s %in% input$country | 	leaguetable_data$Country_Dominica_s %in% input$country 
                   | 	leaguetable_data$Country_DominicanRepublic_s %in% input$country | 	leaguetable_data$Country_Ecuador_s %in% input$country 
                   | 	leaguetable_data$Country_EgyptArabRep_s %in% input$country | 	leaguetable_data$Country_ElSalvador_s %in% input$country 
                   | 	leaguetable_data$Country_EquatorialGuinea_s %in% input$country | 	leaguetable_data$Country_Eritrea_s %in% input$country 
                   | 	leaguetable_data$Country_Estonia_s %in% input$country | 	leaguetable_data$Country_Ethiopia_s %in% input$country 
                   | 	leaguetable_data$Country_FaroeIslands_s %in% input$country | 	leaguetable_data$Country_Fiji_s %in% input$country 
                   | 	leaguetable_data$Country_Finland_s %in% input$country | 	leaguetable_data$Country_France_s %in% input$country 
                   | 	leaguetable_data$Country_FrenchPolynesia_s %in% input$country | 	leaguetable_data$Country_Gabon_s %in% input$country 
                   | 	leaguetable_data$Country_GambiaThe_s %in% input$country | 	leaguetable_data$Country_Georgia_s %in% input$country 
                   | 	leaguetable_data$Country_Germany_s %in% input$country | 	leaguetable_data$Country_Ghana_s %in% input$country 
                   | 	leaguetable_data$Country_Gibraltar_s %in% input$country | 	leaguetable_data$Country_Greece_s %in% input$country 
                   | 	leaguetable_data$Country_Greenland_s %in% input$country | 	leaguetable_data$Country_Grenada_s %in% input$country 
                   | 	leaguetable_data$Country_Guam_s %in% input$country | 	leaguetable_data$Country_Guatemala_s %in% input$country 
                   | 	leaguetable_data$Country_Guinea_s %in% input$country | 	leaguetable_data$Country_GuineaBissau_s %in% input$country 
                   | 	leaguetable_data$Country_Guyana_s %in% input$country | 	leaguetable_data$Country_Haiti_s %in% input$country 
                   | 	leaguetable_data$Country_Honduras_s %in% input$country | 	leaguetable_data$Country_HongKongSARChina_s %in% input$country 
                   | 	leaguetable_data$Country_Hungary_s %in% input$country | 	leaguetable_data$Country_Iceland_s %in% input$country 
                   | 	leaguetable_data$Country_India_s %in% input$country | 	leaguetable_data$Country_Indonesia_s %in% input$country 
                   | 	leaguetable_data$Country_IranIslamicRep_s %in% input$country | 	leaguetable_data$Country_Iraq_s %in% input$country 
                   | 	leaguetable_data$Country_Ireland_s %in% input$country | 	leaguetable_data$Country_IsleofMan_s %in% input$country 
                   | 	leaguetable_data$Country_Israel_s %in% input$country | 	leaguetable_data$Country_Italy_s %in% input$country 
                   | 	leaguetable_data$Country_Jamaica_s %in% input$country | 	leaguetable_data$Country_Japan_s %in% input$country 
                   | 	leaguetable_data$Country_Jordan_s %in% input$country | 	leaguetable_data$Country_Kazakhstan_s %in% input$country 
                   | 	leaguetable_data$Country_Kenya_s %in% input$country | 	leaguetable_data$Country_Kiribati_s %in% input$country 
                   | 	leaguetable_data$Country_KoreaDemPeoplesRep_s %in% input$country | 	leaguetable_data$Country_KoreaRep_s %in% input$country
                   | 	leaguetable_data$Country_Kosovo_s %in% input$country | 	leaguetable_data$Country_Kuwait_s %in% input$country 
                   | 	leaguetable_data$Country_KyrgyzRepublic_s %in% input$country | 	leaguetable_data$Country_LaoPDR_s %in% input$country 
                   | 	leaguetable_data$Country_Latvia_s %in% input$country | 	leaguetable_data$Country_Lebanon_s %in% input$country 
                   | 	leaguetable_data$Country_Lesotho_s %in% input$country | 	leaguetable_data$Country_Liberia_s %in% input$country 
                   | 	leaguetable_data$Country_Libya_s %in% input$country | 	leaguetable_data$Country_Liechtenstein_s %in% input$country
                   | 	leaguetable_data$Country_Lithuania_s %in% input$country | 	leaguetable_data$Country_Luxembourg_s %in% input$country 
                   | 	leaguetable_data$Country_MacaoSARChina_s %in% input$country | 	leaguetable_data$Country_MacedoniaFYR_s %in% input$country
                   | 	leaguetable_data$Country_Madagascar_s %in% input$country | 	leaguetable_data$Country_Malawi_s %in% input$country 
                   | 	leaguetable_data$Country_Malaysia_s %in% input$country | 	leaguetable_data$Country_Maldives_s %in% input$country 
                   | 	leaguetable_data$Country_Mali_s %in% input$country | 	leaguetable_data$Country_Malta_s %in% input$country 
                   | 	leaguetable_data$Country_MarshallIslands_s %in% input$country | 	leaguetable_data$Country_Mauritania_s %in% input$country 
                   | 	leaguetable_data$Country_Mauritius_s %in% input$country | 	leaguetable_data$Country_Mexico_s %in% input$country 
                   | 	leaguetable_data$Country_MicronesiaFedSts_s %in% input$country | 	leaguetable_data$Country_Moldova_s %in% input$country 
                   | 	leaguetable_data$Country_Monaco_s %in% input$country | 	leaguetable_data$Country_Mongolia_s %in% input$country 
                   | 	leaguetable_data$Country_Montenegro_s %in% input$country | 	leaguetable_data$Country_Morocco_s %in% input$country 
                   | 	leaguetable_data$Country_Mozambique_s %in% input$country | 	leaguetable_data$Country_Myanmar_s %in% input$country 
                   | 	leaguetable_data$Country_Namibia_s %in% input$country | 	leaguetable_data$Country_Nauru_s %in% input$country 
                   | 	leaguetable_data$Country_Nepal_s %in% input$country | 	leaguetable_data$Country_Netherlands_s %in% input$country 
                   | 	leaguetable_data$Country_NewCaledonia_s %in% input$country | 	leaguetable_data$Country_NewZealand_s %in% input$country 
                   | 	leaguetable_data$Country_Nicaragua_s %in% input$country | 	leaguetable_data$Country_Niger_s %in% input$country 
                   | 	leaguetable_data$Country_Nigeria_s %in% input$country | 	leaguetable_data$Country_NorthernMarianaIslands_s %in% input$country 
                   | 	leaguetable_data$Country_Norway_s %in% input$country | 	leaguetable_data$Country_Oman_s %in% input$country 
                   | 	leaguetable_data$Country_Pakistan_s %in% input$country | 	leaguetable_data$Country_Palau_s %in% input$country 
                   | 	leaguetable_data$Country_Panama_s %in% input$country | 	leaguetable_data$Country_PapuaNewGuinea_s %in% input$country 
                   | 	leaguetable_data$Country_Paraguay_s %in% input$country | 	leaguetable_data$Country_Peru_s %in% input$country 
                   | 	leaguetable_data$Country_Philippines_s %in% input$country | 	leaguetable_data$Country_Poland_s %in% input$country 
                   | 	leaguetable_data$Country_Portugal_s %in% input$country | 	leaguetable_data$Country_PuertoRico_s %in% input$country 
                   | 	leaguetable_data$Country_Qatar_s %in% input$country | 	leaguetable_data$Country_Romania_s %in% input$country 
                   | 	leaguetable_data$Country_RussianFederation_s %in% input$country | 	leaguetable_data$Country_Rwanda_s %in% input$country 
                   | 	leaguetable_data$Country_Samoa_s %in% input$country | 	leaguetable_data$Country_SanMarino_s %in% input$country 
                   | 	leaguetable_data$Country_SaoTomeandPrincipe_s %in% input$country | 	leaguetable_data$Country_SaudiArabia_s %in% input$country 
                   | 	leaguetable_data$Country_Senegal_s %in% input$country | 	leaguetable_data$Country_Serbia_s %in% input$country 
                   | 	leaguetable_data$Country_Seychelles_s %in% input$country | 	leaguetable_data$Country_SierraLeone_s %in% input$country 
                   | 	leaguetable_data$Country_Singapore_s %in% input$country | 	leaguetable_data$Country_SintMaartenDutchpart_s %in% input$country
                   | 	leaguetable_data$Country_SlovakRepublic_s %in% input$country | 	leaguetable_data$Country_Slovenia_s %in% input$country 
                   | 	leaguetable_data$Country_SolomonIslands_s %in% input$country | 	leaguetable_data$Country_Somalia_s %in% input$country
                   | 	leaguetable_data$Country_SouthAfrica_s %in% input$country | 	leaguetable_data$Country_SouthSudan_s %in% input$country 
                   | 	leaguetable_data$Country_Spain_s %in% input$country | 	leaguetable_data$Country_SriLanka_s %in% input$country 
                   | 	leaguetable_data$Country_StKittsandNevis_s %in% input$country | 	leaguetable_data$Country_StLucia_s %in% input$country
                   | 	leaguetable_data$Country_StMartinFrenchpart_s %in% input$country | 	leaguetable_data$Country_StVincentandtheGrenadine_s %in% input$country 
                   | 	leaguetable_data$Country_Sudan_s %in% input$country | 	leaguetable_data$Country_Suriname_s %in% input$country
                   | 	leaguetable_data$Country_Swaziland_s %in% input$country | 	leaguetable_data$Country_Sweden_s %in% input$country
                   | 	leaguetable_data$Country_Switzerland_s %in% input$country | 	leaguetable_data$Country_SyrianArabRepublic_s %in% input$country 
                   | 	leaguetable_data$Country_Tajikistan_s %in% input$country | 	leaguetable_data$Country_Tanzania_s %in% input$country 
                   | 	leaguetable_data$Country_Thailand_s %in% input$country | 	leaguetable_data$Country_TimorLeste_s %in% input$country 
                   | 	leaguetable_data$Country_Togo_s %in% input$country | 	leaguetable_data$Country_Tonga_s %in% input$country 
                   | 	leaguetable_data$Country_TrinidadandTobago_s %in% input$country | 	leaguetable_data$Country_Tunisia_s %in% input$country 
                   | 	leaguetable_data$Country_Turkey_s %in% input$country | 	leaguetable_data$Country_Turkmenistan_s %in% input$country 
                   | 	leaguetable_data$Country_TurksandCaicosIslands_s %in% input$country | 	leaguetable_data$Country_Tuvalu_s %in% input$country 
                   | 	leaguetable_data$Country_Uganda_s %in% input$country | 	leaguetable_data$Country_Ukraine_s %in% input$country 
                   | 	leaguetable_data$Country_UnitedArabEmirates_s %in% input$country | 	leaguetable_data$Country_UnitedKingdom_s %in% input$country 
                   | 	leaguetable_data$Country_UnitedStates_s %in% input$country | 	leaguetable_data$Country_Uruguay_s %in% input$country 
                   | 	leaguetable_data$Country_Uzbekistan_s %in% input$country | 	leaguetable_data$Country_Vanuatu_s %in% input$country 
                   | 	leaguetable_data$Country_VenezuelaRB_s %in% input$country | 	leaguetable_data$Country_Vietnam_s %in% input$country 
                   | 	leaguetable_data$Country_VirginIslandsUS_s %in% input$country | 	leaguetable_data$Country_WestBankandGaza_s %in% input$country 
                   | 	leaguetable_data$Country_YemenRep_s %in% input$country | 	leaguetable_data$Country_Zambia_s %in% input$country 
                   | 	leaguetable_data$Country_Zimbabwe_s %in% input$country | 	leaguetable_data$Country_TaiwanChina_s %in% input$country 
                   | 	leaguetable_data$Country_CookIslands_s %in% input$country | 	leaguetable_data$Country_Niue_s %in% input$country 
                   | 	leaguetable_data$Country_WallisandFutuna_s %in% input$country | 	leaguetable_data$Country_NA_s %in% input$country))
    )
  }) #close subsetted_ratios_country table
  
  #Create reactive vector of Registry contents: # of studies and ratios, range of ICERs, # of interventions and countries
  reg_contents_country<-reactive({
    
    #Number of studies
    num_studies_country<-nrow(subsetted_methods_country())
    
    #Number of ratios
    num_ratios_country<-nrow(subsetted_ratios_country())
    
    #Range of ICERs
    suppressWarnings({
      high_ICER_country<-max(subsetted_ratios_country()$NumericICER)
      high_ICER_country<-ifelse(high_ICER_country == -999999, "Cost-Saving", high_ICER_country)
      high_ICER_country<-ifelse(high_ICER_country == 9999999, "Dominated", high_ICER_country)
      low_ICER_country<-min(subsetted_ratios_country()$NumericICER)
      low_ICER_country<-ifelse(low_ICER_country == -999999, "Cost-Saving", low_ICER_country)
      low_ICER_country<-ifelse(low_ICER_country == 9999999, "Dominated", low_ICER_country)
      range_ICER_country<-paste(low_ICER_country,high_ICER_country, sep = " - ")
    })
    
    #Number of Interventons
    unique_interventions_country<-length(unique(subsetted_ratios_country()$InterventionPhrase))
    
    #Number of diseases
    disease_specific_country<-
      rbind(as.data.frame(table(subsetted_methods_country()$GBDDis1_tier1)),
            as.data.frame(table(subsetted_methods_country()$GBDDis1_tier2)),
            as.data.frame(table(subsetted_methods_country()$GBDDis1_tier3)),
            as.data.frame(table(subsetted_methods_country()$GBDDis1_tier4)),
            as.data.frame(table(subsetted_methods_country()$GBDDis2_tier1)),
            as.data.frame(table(subsetted_methods_country()$GBDDis2_tier2)),
            as.data.frame(table(subsetted_methods_country()$GBDDis2_tier3)),
            as.data.frame(table(subsetted_methods_country()$GBDDis2_tier4)),
            as.data.frame(table(subsetted_methods_country()$GBDDis3_tier1)),
            as.data.frame(table(subsetted_methods_country()$GBDDis3_tier2)),
            as.data.frame(table(subsetted_methods_country()$GBDDis3_tier3)),
            as.data.frame(table(subsetted_methods_country()$GBDDis3_tier4)),
            as.data.frame(table(subsetted_methods_country()$GBDDis4_tier1)),
            as.data.frame(table(subsetted_methods_country()$GBDDis4_tier2)),
            as.data.frame(table(subsetted_methods_country()$GBDDis4_tier3)),
            as.data.frame(table(subsetted_methods_country()$GBDDis4_tier4)),
            as.data.frame(table(subsetted_methods_country()$GBDDis5_tier1)),
            as.data.frame(table(subsetted_methods_country()$GBDDis5_tier2)),
            as.data.frame(table(subsetted_methods_country()$GBDDis5_tier3)),
            as.data.frame(table(subsetted_methods_country()$GBDDis5_tier4)))
    unique_diseases_country<-length(unique(disease_specific_country$Var1))
    
    #Create and return named vector
    reg_contents_country<-c(num_studies_country, num_ratios_country, range_ICER_country, unique_interventions_country, unique_diseases_country)
    names(reg_contents_country)<-c("Number of studies","Number of ratios", "Range of ICERs (2016 USD)", "Number of unique interventions", "Number of unique diseases")
    return(reg_contents_country)
  }) #close reg_content table
  #Render reg_contents_country output table
  output$contents_table_country<-renderTable(reg_contents_country(), rownames = TRUE, colnames = FALSE, title = "Registry contents:")
  
  
  #Mini league table
  league_table_country<-reactive({
    #order subsetted ratios from most to least cost effective
    ordered_ratios_country <- subsetted_ratios_country()[order(subsetted_ratios_country()$NumericICER),] 
    #reduce to only top 5
    ordered_ratios_country<-ordered_ratios_country[1:5,]
    #create output table format
    league_table_country<-data.frame(ordered_ratios_country$InterventionCombined, ordered_ratios_country$TargetPopulation, ordered_ratios_country$DisplayRatio, ordered_ratios_country$TitleAut)
    colnames(league_table_country)<-c("Intervention", "Target Population", "Cost/DALY averted", "Source (Title, Author)")
    return(league_table_country)
  })
  #Render league_table_country output table
  output$league_table_country<-renderDataTable(
    league_table_country(),
    options = list(
      searching = FALSE,
      pageLength = 5, 
      dom = 't'
    ),
    rownames = FALSE)
  
  #Pie chart of intervention types
  pie1 <- reactive({
    subsetted_ratios_country() %>%
      group_by(Intervention1) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Intervention1, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= 0,
                 y= .1,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.6
               )
             ))
  })
  #render Pie chart
  output$piechart_interv_country <- renderPlotly(pie1())
  
  #treemap of disease areas
  treemap<-reactive({
    req(input$country)

    #create dataframe
    treemap_data<-as.data.frame(table(subsetted_methods_country()$GBDDis1_tier2))
    colnames(treemap_data)<-c("Disease", "Number of CEAs")
    #rename diseases to shorthand
    treemap_data$disease_short[treemap_data$Disease == "Cardiovascular and circulatory diseases"]<-"Cardiovascular/circulatory"
    treemap_data$disease_short[treemap_data$Disease == "Chronic respiratory diseases"]<-"Chronic respiratory"
    treemap_data$disease_short[treemap_data$Disease == "Diabetes, urogenital, blood, and endocrine diseases"]<-"Diabetes and related"
    treemap_data$disease_short[treemap_data$Disease == "Diarrhea, lower respiratory infections, meningitis, and other common infectious diseases"]<-"Common infectious"
    treemap_data$disease_short[treemap_data$Disease == "Digestive diseases"]<-"Digestive"
    treemap_data$disease_short[treemap_data$Disease == "HIV/AIDS and tuberculosis"]<-"HIV/AIDS, TB"
    treemap_data$disease_short[treemap_data$Disease == "Maternal disorders"]<-"Maternal"
    treemap_data$disease_short[treemap_data$Disease == "Neoplasms"]<-"Neoplasms"
    treemap_data$disease_short[treemap_data$Disease == "Mental and behavioral disorders"]<-"Mental/behavioral"
    treemap_data$disease_short[treemap_data$Disease == "Musculoskeletal disorders"]<-"Musculoskeletal"
    treemap_data$disease_short[treemap_data$Disease == "Neglected tropical diseases and malaria"]<-"Neglected tropical"
    treemap_data$disease_short[treemap_data$Disease == "Neonatal disorders"]<-"Neonatal"
    treemap_data$disease_short[treemap_data$Disease == "Neurological disorders"]<-"Neurological"
    treemap_data$disease_short[treemap_data$Disease == "Nutritional deficiencies"]<-"Nutritional"
    treemap_data$disease_short[treemap_data$Disease == "Other communicable, maternal, neonatal, and nutritional disorders"]<-"Other communicable"
    treemap_data$disease_short[treemap_data$Disease == "Other non-communicable diseases"]<-"Other non-communicable"
    treemap_data$disease_short[treemap_data$Disease == "Self-harm and interpersonal violence"]<-"Violence"
    treemap_data$disease_short[treemap_data$Disease == "Transport injuries"]<-"Transport injury"
    treemap_data$disease_short[treemap_data$Disease == "Unintentional injuries other than transport injuries"]<-"Other injury"
    
    
    treemap_plot<-ggplot(treemap_data, aes(area = `Number of CEAs`, label = disease_short)) +
      geom_treemap(colour = 'white', fill = 'steelblue3')+geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE, min.size = 2, reflow = T)
    
    return(treemap_plot)
  })
  #render treemap
  output$treemap_country<-renderPlot(treemap())
  
  #Stacked bar of thresholds
  stackedbar_country<-reactive({
    #require a disease input
    req(input$country)
    
    #create stacked bar dataframe
    R_CS_country<-sum(subsetted_ratios_country()$R_CS)
    R_LT1_country<-sum(subsetted_ratios_country()$R_LT1)
    R_1to3_country<-sum(subsetted_ratios_country()$R_1to3)
    R_GT3_country<-sum(subsetted_ratios_country()$R_GT3)
    R_D_country<-sum(subsetted_ratios_country()$R_D)
    Thresholds_country<-data.frame(R_CS_country, R_LT1_country, R_1to3_country, R_GT3_country,R_D_country)
    colnames(Thresholds_country)<-c("R_CS", "R_LT1", "R_1to3", "R_GT3", "R_D")
    
    #create ratios stacked bar chart, stratified by ratios
    stackedbar_country<-plot_ly(Thresholds_country, x = ~Thresholds_country, y = ~R_CS, type = 'bar', name = 'Cost-Saving', 
                                hoverinfo = "y") %>%
      add_trace(y = ~R_LT1, name = 'Less than 1x GDP') %>%
      add_trace(y = ~R_1to3, name = '1-3x GDP') %>%
      add_trace(y = ~R_GT3, name = 'Greater than 3x GDP') %>%
      add_trace(y = ~R_D, name = 'Dominated') %>%
      layout(yaxis = list(title = 'Number of cost/DALY averted ratios'),
             xaxis = list(title = "", showticklabels=FALSE),
             barmode = 'stack',
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= 1,
                 y= 0,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.6
               )
             ))
    
     return(stackedbar_country)
  }) #close reactive
  #render stacked bar chart
  output$stackedbar_country<- renderPlotly(stackedbar_country())
  
  
  #Regular BAR of articles
  regbar_country<-reactive({
    req(input$country)
    
    #create bar dataset
    
    articlecount_country<-aggregate(subsetted_methods_country()$ArticleID, by = subsetted_methods_country()["PubYear"] , FUN = length)
    colnames(articlecount_country)<-c("Year", "Article Count")
    
    articlecount_country$Year <- factor(articlecount_country$Year)
    
    regbar_country <- plot_ly(articlecount_country, x = ~Year, y = ~`Article Count`, type = "bar",
                                  marker = list(color = 'rgb(158,202,225)',
                                                line = list(color = 'rgb(8,48,107)', width = 1.0))) %>%
      layout(xaxis = list(title = "", showticklabels=TRUE),
             yaxis = list(title = "Number of cost/DALY CEAs"),
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= .1,
                 y= 1,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.4
               )
             ))
    
    return(regbar_country)
  })
  #render regbar_country
  output$regbar_country<-renderPlotly(regbar_country())
  
  #DOWNLOADABLE REPORT
  output$report_country <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "DashboardReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it
      src4 <- normalizePath('DashboardReport_country.rmd')
      src5 <- normalizePath('cevr_dark.png')
      src6 <- normalizePath('ghcea.png') 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src4, 'DashboardReport_country.rmd')
      file.copy(src5, 'cevr_dark.png') #NEW
      file.copy(src6, 'ghcea.png') #NEW
                        
      # Set up parameters to pass to Rmd document
      params <- list(country = input$country, methods = subsetted_methods_country(), ratios = subsetted_ratios_country(), countries = countries_data,
                     logo = "cevr_dark.png", logo2 = "ghcea.png")
                        
      # Knit the document, passing in the `params` list
      rmarkdown::render('DashboardReport_country.rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    })#close download handler
  
  #Downloadable csv of filtered dataset, METHODS ----
  output$methodsdata_country<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDmethods_country.csv",
    content = function(file) {
      write.csv(subsetted_methods_country(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  #Downloadable csv of filtered dataset, RATIOS ----
  output$ratiosdata_country<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDratios_country.csv",
    content = function(file) {
      write.csv(subsetted_ratios_country(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  ###INTERVENTION TAB####
  
  #Create reactive table of methods, subset by filter selection
  subsetted_methods_interv<-reactive({
    ifelse(input$intervention == "All interventions",
           return(methods_raw),
           return(subset(methods_raw, Intervention_s_CD %in% input$intervention | Intervention_s_D %in% input$intervention |Intervention_s_EA %in% input$intervention |Intervention_s_ER %in% input$intervention |Intervention_s_HE %in% input$intervention |Intervention_s_I %in% input$intervention |
             Intervention_s_L %in% input$intervention |Intervention_s_ME %in% input$intervention |Intervention_s_MD %in% input$intervention |Intervention_s_MP %in% input$intervention |Intervention_s_P %in% input$intervention |Intervention_s_N %in% input$intervention |
             Intervention_s_O %in% input$intervention |Intervention_s_s %in% input$intervention |Intervention_s_sur %in% input$intervention))
    )
  }) #close subsetted_methods_interv table
  
  #Create reactive table of ratios, subset by filter selection
  subsetted_ratios_interv<-reactive({
    ifelse(input$intervention == "All interventions",
           return(leaguetable_data),
            return(subset(leaguetable_data, Intervention_s_CD %in% input$intervention| Intervention_s_D %in% input$intervention|Intervention_s_EA %in% input$intervention 
                   | Intervention_s_ER %in% input$intervention|Intervention_s_HE %in% input$intervention|Intervention_s_I %in% input$intervention
                   | Intervention_s_L %in% input$intervention|Intervention_s_ME %in% input$intervention|Intervention_s_MD %in% input$intervention
                   | Intervention_s_MP %in% input$intervention|Intervention_s_P %in% input$intervention|Intervention_s_N %in% input$intervention
                   | Intervention_s_O %in% input$intervention|Intervention_s_s %in% input$intervention|Intervention_s_sur %in% input$intervention))
    )
  }) #close subsetted_ratios_interv table
  
  #Create reactive vector of Registry contents: # of studies and ratios, range of ICERs, # of interventions and countries
  reg_contents_interv<-reactive({
    
    #Number of studies
    num_studies_interv<-nrow(subsetted_methods_interv())
    
    #Number of ratios
    num_ratios_interv<-nrow(subsetted_ratios_interv())
    
    #Range of ICERs
    suppressWarnings({
      high_ICER_interv<-max(subsetted_ratios_interv()$NumericICER)
      high_ICER_interv<-ifelse(high_ICER_interv == -999999, "Cost-Saving", high_ICER_interv)
      high_ICER_interv<-ifelse(high_ICER_interv == 9999999, "Dominated", high_ICER_interv)
      low_ICER_interv<-min(subsetted_ratios_interv()$NumericICER)
      low_ICER_interv<-ifelse(low_ICER_interv == -999999, "Cost-Saving", low_ICER_interv)
      low_ICER_interv<-ifelse(low_ICER_interv == 9999999, "Dominated", low_ICER_interv)
      
      range_ICER_interv<-paste(low_ICER_interv,high_ICER_interv, sep = " - ")
    })
    
    #Number of diseases
    #Number of diseases
    disease_specific_interv<-
      rbind(as.data.frame(table(subsetted_methods_interv()$GBDDis1_tier1)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis1_tier2)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis1_tier3)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis1_tier4)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis2_tier1)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis2_tier2)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis2_tier3)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis2_tier4)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis3_tier1)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis3_tier2)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis3_tier3)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis3_tier4)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis4_tier1)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis4_tier2)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis4_tier3)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis4_tier4)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis5_tier1)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis5_tier2)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis5_tier3)),
            as.data.frame(table(subsetted_methods_interv()$GBDDis5_tier4)))
    unique_diseases_interv<-length(unique(disease_specific_interv$Var1))
    
    #Number of countries
    unique_countries_interv<-length(unique(subsetted_ratios_interv()$Country))
    
    #Create and return named vector
    reg_contents_interv<-c(num_studies_interv, num_ratios_interv, range_ICER_interv, unique_diseases_interv, unique_countries_interv)
    names(reg_contents_interv)<-c("Number of studies","Number of ratios", "Range of ICERs (2016 USD)", "Number of unique diseases", "Number of countries represented")
    return(reg_contents_interv)
  }) #close reg_content table
  #Render reg_contents_interv output table
  output$contents_table_interv<-renderTable(reg_contents_interv(), rownames = TRUE, colnames = FALSE, title = "Registry contents:")
  
  
  #Mini league table
  league_table_interv<-reactive({
    #order subsetted ratios from most to least cost effective
    ordered_ratios_interv <- subsetted_ratios_interv()[order(subsetted_ratios_interv()$NumericICER),]
    #reduce to only top 5
    ordered_ratios_interv<-ordered_ratios_interv[1:5,]
    #create output table format
    league_table_interv<-data.frame(ordered_ratios_interv$InterventionCombined, ordered_ratios_interv$TargetPopulation, ordered_ratios_interv$DisplayRatio, ordered_ratios_interv$TitleAut)
    colnames(league_table_interv)<-c("Intervention", "Target Population", "Cost/DALY averted", "Source (Title, Author)")
    return(league_table_interv)
  })
  #Render league_table_interv output table
  output$league_table_interv<-renderDataTable(
    league_table_interv(),
    options = list(
      searching = FALSE,
      pageLength = 5,
      dom = 't'
    ),
    rownames = FALSE)

  #MAP
  map_interv<-reactive({
    #require a disease input
    req(input$intervention)

    #create number of ratios dataframe
    numratios_country_interv<-data.frame(table(subsetted_ratios_interv()$Country))
    colnames(numratios_country_interv)<-c("COUNTRY", "NUM_STUDIES")

    #merge with shapefile
    df_interv <- merge(countries_data, numratios_country_interv, on = "COUNTRY")

    # light grey boundaries
    l_interv <- list(color = toRGB("grey"), width = 0.5)

    # specify map projection/options
    g_interv <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )

    map_interv <- plot_geo(df_interv) %>%
      add_trace(
        z = ~NUM_STUDIES, color = ~NUM_STUDIES, colors = 'Blues',
        text = ~COUNTRY, locations = ~CODE, marker = list(line = l_interv)
      ) %>%
      colorbar(title = 'Number of cost/DALY Ratios') %>%
      layout(
        geo = g_interv,
        images = list(
          list(
            source = "cevr_dark.png",
            xref = "paper",
            yref = "paper",
            x= .85,
            y= .2,
            sizex = 0.2,
            sizey = 0.2,
            opacity = 0.6
          )
        )
      )

    return(map_interv)
  })
  #render map output
  output$map_interv <- renderPlotly(map_interv())


  #Treemap of disease areas
  treemap_interv <- reactive({
    req(input$intervention)
    #create dataframe
    treemap_data_interv<-as.data.frame(table(subsetted_methods_interv()$GBDDis1_tier2))
    colnames(treemap_data_interv)<-c("Disease", "Number of CEAs")
    #rename diseases to shorthand
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Cardiovascular and circulatory diseases"]<-"Cardiovascular/circulatory"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Chronic respiratory diseases"]<-"Chronic respiratory"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Diabetes, urogenital, blood, and endocrine diseases"]<-"Diabetes and related"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Diarrhea, lower respiratory infections, meningitis, and other common infectious diseases"]<-"Common infectious"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Digestive diseases"]<-"Digestive"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "HIV/AIDS and tuberculosis"]<-"HIV/AIDS, TB"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Maternal disorders"]<-"Maternal"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Neoplasms"]<-"Neoplasms"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Mental and behavioral disorders"]<-"Mental/behavioral"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Musculoskeletal disorders"]<-"Musculoskeletal"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Neglected tropical diseases and malaria"]<-"Neglected tropical"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Neonatal disorders"]<-"Neonatal"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Neurological disorders"]<-"Neurological"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Nutritional deficiencies"]<-"Nutritional"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Other communicable, maternal, neonatal, and nutritional disorders"]<-"Other communicable"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Other non-communicable diseases"]<-"Other non-communicable"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Self-harm and interpersonal violence"]<-"Violence"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Transport injuries"]<-"Transport injury"
    treemap_data_interv$disease_short[treemap_data_interv$Disease == "Unintentional injuries other than transport injuries"]<-"Other injury"
    
    
    treemap_plot_interv<-ggplot(treemap_data_interv, aes(area = `Number of CEAs`, label = disease_short)) +
      geom_treemap(colour = 'white', fill = 'steelblue3')+geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE, min.size = 2, reflow = T)
    
    return(treemap_plot_interv)
  })
  #render Pie chart
  output$treemap_interv <- renderPlot(treemap_interv())

  #Stacked bar of thresholds
  stackedbar_interv<-reactive({
    #require a disease input
    req(input$intervention)

    #create stacked bar dataframe
    R_CS_interv<-sum(subsetted_ratios_interv()$R_CS)
    R_LT1_interv<-sum(subsetted_ratios_interv()$R_LT1)
    R_1to3_interv<-sum(subsetted_ratios_interv()$R_1to3)
    R_GT3_interv<-sum(subsetted_ratios_interv()$R_GT3)
    R_D_interv<-sum(subsetted_ratios_interv()$R_D)
    Thresholds_interv<-data.frame(R_CS_interv, R_LT1_interv, R_1to3_interv, R_GT3_interv,R_D_interv)
    colnames(Thresholds_interv)<-c("R_CS", "R_LT1", "R_1to3", "R_GT3", "R_D")
    
    #create ratios stacked bar chart, stratified by ratios
    stackedbar_interv<-plot_ly(Thresholds_interv, x = ~Thresholds_interv, y = ~R_CS, type = 'bar', name = 'Cost-Saving', 
                               hoverinfo = "y") %>%
      add_trace(y = ~R_LT1, name = 'Less than 1x GDP') %>%
      add_trace(y = ~R_1to3, name = '1-3x GDP') %>%
      add_trace(y = ~R_GT3, name = 'Greater than 3x GDP') %>%
      add_trace(y = ~R_D, name = 'Dominated') %>%
      layout(yaxis = list(title = 'Number of cost/DALY averted ratios'),
             xaxis = list(title = "", showticklabels=FALSE),
             barmode = 'stack',
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= 1,
                 y= 0,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.6
               )
             ))
    
    return(stackedbar_interv)
  }) #close reactive
  #render stacked bar chart
  output$stackedbar_interv <- renderPlotly(stackedbar_interv())


  #STACKED BAR of articles
  regbar_interv<-reactive({
    req(input$intervention)

    #create bar dataset

    articlecount<-aggregate(subsetted_methods_interv()$ArticleID, by = subsetted_methods_interv()["PubYear"] , FUN = length)
    colnames(articlecount)<-c("Year", "Article Count")

    articlecount$Year <- factor(articlecount$Year)

    regbar_interv <- plot_ly(articlecount, x = ~Year, y = ~`Article Count`, type = "bar",
                                 marker = list(color = 'rgb(158,202,225)',
                                               line = list(color = 'rgb(8,48,107)', width = 1.0))) %>%
      layout(xaxis = list(title = "", showticklabels=TRUE),
             yaxis = list(title = "Number of cost/DALY CEAs"),
             images = list(
               list(
                 source = "cevr_dark.png",
                 xref = "paper",
                 yref = "paper",
                 x= .1,
                 y= 1,
                 sizex = 0.2,
                 sizey = 0.2,
                 opacity = 0.4
               )
             ))

    return(regbar_interv)
  })
  #render regbar_interv
  output$regbar_interv<-renderPlotly(regbar_interv())

  #DOWNLOADABLE REPORT
  output$report_interv <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "DashboardReport_interv.html",
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it
      src7 <- normalizePath('DashboardReport_interv.rmd')
      src8 <- normalizePath('cevr_dark.png') #NEW 
      src9 <- normalizePath('ghcea.png') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src7, 'DashboardReport_interv.rmd')
      file.copy(src8, 'cevr_dark.png') #NEW
      file.copy(src9, 'ghcea.png') #NEW
      
      # Set up parameters to pass to Rmd document
      params <- list(intervention = input$intervention, methods = subsetted_methods_interv(), ratios = subsetted_ratios_interv (), countries = countries_data,
                     logo = "cevr_dark.png", logo2 = "ghcea.png")
      
      # Knit the document, passing in the `params` list
      rmarkdown::render('DashboardReport_interv.rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    })#close download handler
  
  #Downloadable csv of filtered dataset, METHODS ----
  output$methodsdata_interv<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDmethods_intervention.csv",
    content = function(file) {
      write.csv(subsetted_methods_interv(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  #Downloadable csv of filtered dataset, RATIOS ----
  output$ratiosdata_interv<- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_FILTEREDratios_intervention.csv",
    content = function(file) {
      write.csv(subsetted_ratios_interv(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
} #close server

# Run the application 
shinyApp(ui = ui, server = server)

