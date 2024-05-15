# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bland&Altman PlotteR - A Shiny app for quantitative comparison of measurements
# Created by Joachim Goedhart (@joachimgoedhart), first version 2020
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Joachim Goedhart (C) 2020
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

boot_LoA = function(x) {
  quantile(sample(x, replace = TRUE), probs = c(0.05,0.5,0.95))
}


#We define a function `sos()` to calculate the 'sum of squares' (SoS). 
sos <- function(x) {
  sum((mean(x)-x)^2)
}


#number of replicates for bootstrap
nrep = 500


# Allow files up to 10 Mb
options(shiny.maxRequestSize=10*1024^2)

#Load necessary packages

library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(readxl)
library(DT)
library(RCurl)
library(readr)

df_example_1 <- read.csv("df_SystBloodPressure.csv", na.strings = "")
df_example_2 <- read.csv("df_PlasmaVolume.csv", na.strings = "")
df_example_3 <- read.csv("df_T4.csv", na.strings = "")


# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Bland&Altman Plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
          conditionalPanel(
            condition = "input.tabs=='Plot'",
            radioButtons("plot_type", "Select Plot:", choices = 
                           list(
                             "BA, y-axis: Difference [absolute]" = 1,
                             "BA, y-axis: Difference/Average [percentage]" = 2,
                             "BA, y-axis: Difference of log10 transformed data" = 3
                             # "Correlation (Measurement_2 vs. Measurement_1)"=5,
                             
                         ),
                         selected =  1),
            sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),  
            
            sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.8),  
            h4("Statistics"),
            
            
            radioButtons("LoA", "Limits of Agreement", choices = 
                           list(
                             "Ordinary" = 1,
                             "Regression" = 2,
                             "Non parametric" = 3
                             ),
                         selected =  1),
            
            
            conditionalPanel(condition = "input.LoA!='2'",
            checkboxInput(inputId = "add_CI",
                          label = "Show 95% confidence intervals",
                          value = FALSE)),            
            checkboxInput(inputId = "show_rugs",
                          label = "Show rugs",
                          value = FALSE),
            sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),
            

            checkboxInput(inputId = "show_labels",
                          label = "Label limits on secondary y-axis",
                          value = FALSE),
            conditionalPanel(condition = "input.show_labels == true",
                             numericInput("digits", "Number of digits:", value = 2)), 
            

            h4("Layout"),
            radioButtons("adjustcolors", "Color of datapoints", choices = 
                           list(
                             "Black" = 1,
                             "Grey" = 3,
                             "User defined"=5),
                         selected =  1),
            conditionalPanel(condition = "input.adjustcolors == 5",
                             textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2")), 
            
            
            # checkboxInput(inputId = "rotate_plot",
            #               label = "Rotate plot 90 degrees",
            #               value = FALSE),
            checkboxInput(inputId = "change_scale",
                          label = "Change scale",
                          value = FALSE),
            conditionalPanel(
              condition = "input.change_scale == true",
              
              
              textInput("range_x", "Range x-axis (min,max)", value = "")
              
            ),
            

            conditionalPanel(
              condition = "input.change_scale == true",
              textInput("range_y", "Range y-axis (min,max)", value = ""),            
              checkboxInput(inputId = "scale_log_10", label = "Log10 scale on y-axis", value = FALSE)
              
            ),
            numericInput("plot_height", "Plot height (# pixels): ", value = 600),
            numericInput("plot_width", "Plot width (# pixels):", value = 600),


            h4("Labels"),
  
            checkboxInput(inputId = "add_title",
                          label = "Add title",
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              checkboxInput("align", "Centre the title", value = FALSE),
              textInput("title", "Title:", value = "")
            ),
            
            checkboxInput(inputId = "label_axes",
                          label = "Change axis labels",
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")
              
            ),
            
            checkboxInput(inputId = "adj_fnt_sz",
                          label = "Change font size",
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_title", "Plot title:", value = 24),
              numericInput("fnt_sz_labs", "Axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Axis labels:", value = 18),
              numericInput("fnt_sz_cand", "Labels of hits:", value = 6)
              
            ),

              checkboxInput(inputId = "add_legend",
                            label = "Add legend",
                            value = FALSE),
            
            # 
            # conditionalPanel(
            #   condition = "input.add_legend == true",
            #   textInput("legend_title", "Legend title:", value = "")
            # ),
            checkboxInput(inputId = "add_description",
                          label = "Add figure description",
                          value = FALSE),

              NULL),
          
          
          conditionalPanel(
            condition = "input.tabs=='iPlot'",h4("iPlot")
          ),
              conditionalPanel(
                  condition = "input.tabs=='Data'",
              h4("Data upload"),
              
              radioButtons(
                "data_input", "",
                choices = 
                  list("Example data 1" = 1,
                       "Example data 2" = 2,
                       "Upload file (CSV, text, excel)" = 3,
                       "Paste data" = 4,
                       "URL (CSV files only)" = 5
                  )
                ,
                selected =  1),
              
              conditionalPanel(
                condition = "input.data_input=='1'",p('Systolic blood pressure measurements made simultaneously by two observers (J and R) and an automatic blood pressure measuring machine (S), each making three observations in quick succession. Data retrieved from table 1 by Bland & Altman (1999): '),a('https://doi.org/10.1177/096228029900800204', href ='https://doi.org/10.1177/096228029900800204'),hr()),             
              # conditionalPanel(
              #   condition = "input.data_input=='2'",p('Measurements of plasma volume expressed as a percentage of normal in 99 subjects, using two alternative sets of normal values due to Nadler and Hurley. Data retrieved from table 2 by Bland & Altman (1999): '), a('https://doi.org/10.1177/096228029900800204', href ='https://doi.org/10.1177/096228029900800204'),hr()),
              
              
              conditionalPanel(
                condition = "input.data_input=='3'",
        
                fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                
                selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
                                                                                 "Tab" = "\t",
                                                                                 "Semicolon" = ";",
                                                                                 "Space" = " ")),
                

                  selectInput("sheet", label = "Select sheet (for excel workbook):", choices = " ")


                ),
              
              conditionalPanel(
                condition = "input.data_input=='4'",
                h5("Paste data below:"),
                tags$textarea(id = "data_paste",
                              placeholder = "Add data here",
                              rows = 10,
                              cols = 20, ""),
                actionButton("submit_data_button", "Submit data"),
                radioButtons(
                  "text_delim", "Delimiter",
                  choices = 
                    list("Tab (from Excel)" = "\t",
                         "Space" = " ",
                         "Comma" = ",",
                         "Semicolon" = ";"),
                  selected = "\t")),
              ### csv via URL as input      
              conditionalPanel(
                condition = "input.data_input=='5'",
                #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"), 
                 textInput("URL", "URL", value = ""), 
                NULL
              ),
              h4('Select measurements'),
              
              selectInput("x_var", label = "Measurement 1", choices = "-"),
              selectInput("y_var", label = "Measurement 2", choices = "-"),
              textInput("unit", "Units of the variables (optional)", value = ""), 
              # selectInput("g_var", label = "Select column with names", choices = "-"),
              
              hr(),

              NULL
              ),
          
          conditionalPanel(
            condition = "input.tabs=='Data Summary'",
            h4("Data summary"),

            numericInput("digits_table", "Digits:", 2, min = 0),
            hr(),
            h4("Repeatability"),
            selectizeInput(inputId = 'repeats',
                           label = "Select Repeats:",
                           choices = "-",
                           selected = "-",
                           multiple = TRUE, # allow for multiple inputs
                           options = list(create = TRUE)) # if TRUE, allows newly created inputs

            
            ),
          
          
          
          
          conditionalPanel(
            condition = "input.tabs=='About'",
            
            #Session counter: https://gist.github.com/trestletech/9926129
            h4("About"),  "There are currently", 
            verbatimTextOutput("count"),
            "session(s) connected to this app.",
            hr(),
            h4("Find our other dataViz apps at:"),a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
          )
                   
                   
      ),   #Close sidebarPanel

      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="tabs",
                    tabPanel("Data", h4("Data as provided"),dataTableOutput("data_uploaded")),
                    tabPanel("Plot",h3("BA Plot"
                                       ),
                             downloadButton("downloadPlotPDF", "Download pdf-file"),
                             #                          downloadButton("downloadPlotSVG", "Download svg-file"),
                             downloadButton("downloadPlotPNG", "Download png-file"),
                             
                             # actionButton("settings_copy", icon = icon("clone"),
                             #              label = "Clone current setting"),

                             plotOutput("coolplot",
                                        height = 'auto'),htmlOutput("LegendText", width="200px", inline =FALSE),
                             
                            NULL

                              ),
                    # tabPanel("iPlot", h4("iPlot"), plotlyOutput("out_plotly")),
                    tabPanel("Data Summary",dataTableOutput('data_summary'),htmlOutput("stats", width="200px", inline =FALSE),
                             h4("QQ-plot:"),
                             plotOutput("qqplot", height = 'auto')

                    ),

                    tabPanel("About", includeHTML("about.html"))
                    )
        
      )   #Close mainPanel
      

   ) #Close sidebarLayout
) #Close fluidPage

server <- function(input, output, session) {
  
  # Session variable - initialize defaults
  x_var.selected <- "x"
  y_var.selected <- "y"
  repeats.selected <- ""
  g_var.selected <- ""
  sheet.selected <- " "
  
  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################
  
df_upload <- reactive({
    
    if (input$data_input == 1) {
      x_var.selected <<- "J1"
      y_var.selected <<- "S1"
      repeats.selected <<- c("J1","J2","J3")
      g_var.selected <<- "Subject"
      data <- df_example_1

    } else if (input$data_input == 2) {
      x_var.selected <<- "T4X"
      y_var.selected <<- "T4A"
      repeats.selected <<- ""
      g_var.selected <<- "Subject"

      data <- df_example_3
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Click 'Browse...' to select a datafile or drop file onto 'Browse' button"))
      # } else if (input$submit_datafile_button == 0) {
      #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        
        #Isolate extenstion and convert to lowercase
        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        
          if (fileext == "txt" || fileext=="csv") {
            
            data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
            updateSelectInput(session, "sheet", choices = " ", selected = " ")
          } else if (fileext=="xls" || fileext=="xlsx") {
            names <- excel_sheets(path = input$upload$datapath)
            # updateSelectInput(session, "sheet_names", choices = names)
            sheet.selected <<- input$sheet 
            updateSelectInput(session, "sheet", choices = names, selected = sheet.selected)

            if (input$sheet %in% names)
            {
              n <- which(names==input$sheet)
              # sheet.selected <<- input$sheet 
            } else {
              n <- 1
              #Ensures update and selection of first sheet upon loading the data
              updateSelectInput(session, "sheet", choices = names)
            }
            
            data <- read_excel(file_in$datapath, sheet = n , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
          } 
          
        # })
      }
      
      
    } else if (input$data_input == 4) {
      x_var.selected <<- ""      
      y_var.selected <<- ""
      g_var.selected <<- ""
      repeats.selected <<- ""
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          # isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          # })
        }
      }
    
  
      
      
    } else if (input$data_input == 5) {

      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/3713174/files/CSV_1-GFP-CSB-WT_vs_GFP-NLS.csv"))
      } else if (url.exists(input$URL) == FALSE) {
        return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read.csv(input$URL)}
    }
  
  
  #Replace space and dot of header names by underscore
  data <- data %>% select_all(~gsub("\\s+|\\.", "_", .))
  
  #Change empty cells to NA
  
    return(data)
  })
  
  
  #### DISPLAY UPLOADED DATA (as provided) ##################
  
output$data_uploaded <- renderDataTable(
    

    df_upload(),
    # df_transformed(),
    rownames = FALSE,
    options = list(pageLength = 20, autoWidth = FALSE,
                   lengthMenu = c(20, 100, 1000, 10000)),
    editable = FALSE,selection = 'none'
  )
  
##### Get Variables from the input ##############
  
observe({
  
    df <- df_upload()
    var_names  <- names(df)

    # Get the names of columns that are factors.
    nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) || is.logical(x) || is.character(x), df))
    nms_var <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), df))
    nms_fact <- c("-",nms_fact)
    
    updateSelectInput(session, "x_var", choices = nms_var, selected = x_var.selected)
    updateSelectInput(session, "y_var", choices = nms_var, selected = y_var.selected)
    updateSelectInput(session, "g_var", choices = nms_fact, selected = g_var.selected)
    updateSelectizeInput(session, "repeats", choices = nms_var, selected = repeats.selected)
   
  })
  
  
########### GET INPUT VARIABLEs FROM HTML ##############

observe({
  query <- parseQueryString(session$clientData$url_search)
  

  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    # updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    observe(print((query[['url']])))
    # updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?data ################
  
  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data)))
    
    updateRadioButtons(session, "data_input", selected = presets_data[1])    
    # updateCheckboxInput(session, "tidyInput", value = presets_data[2])
    
    # updateSelectInput(session, "x_var", selected = presets_data[3])
    # updateSelectInput(session, "y_var", selected = presets_data[4])    
    # updateSelectInput(session, "g_var", selected = presets_data[5])
    
    x_var.selected <<- presets_data[3]
    y_var.selected <<- presets_data[4]
    g_var.selected <<- presets_data[5]
    
    if (presets_data[1] == "1" || presets_data[1] == "2") {
      updateTabsetPanel(session, "tabs", selected = "Plot")
    }
  }
  
  
  
  ############ ?vis ################
  
  if (!is.null(query[['vis']])) {
    
    presets_vis <- query[['vis']]
    presets_vis <- unlist(strsplit(presets_vis,";"))
    observe(print((presets_vis)))
    
    updateSliderInput(session, "pointSize", value = presets_vis[1])
    updateSliderInput(session, "alphaInput", value = presets_vis[2])
    updateSliderInput(session, "fc_cutoff", value = unlist(strsplit(presets_vis[3],",")))
    updateSliderInput(session, "p_cutoff", value = presets_vis[4])
    updateSelectInput(session, "direction", selected = presets_vis[5])
    updateSelectInput(session, "criterion", selected = presets_vis[6])
    
  }
  
  
  ############ ?can ################
  
  if (!is.null(query[['can']])) {
    
    presets_can <- query[['can']]
    presets_can <- unlist(strsplit(presets_can,";"))
    observe(print((presets_can)))
    
    updateNumericInput(session, "top_x", value = presets_can[1])
    updateCheckboxInput(session, "show_table", value = presets_can[2])
    # updateCheckboxInput(session, "hide_labels", value= presets_can[3])
    # updateCheckboxInput(session, "user_selected", value= presets_can[4])
    # updateTextInput(session, "user_label_list", value= presets_can[4])
    

  }
  

  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    # observe(print((presets_layout)))
    
    # updateCheckboxInput(session, "rotate_plot", value = presets_layout[1])
    # updateCheckboxInput(session, "no_grid", value = (presets_layout[2]))
    
    updateCheckboxInput(session, "change_scale", value = presets_layout[3])
    updateTextInput(session, "range_x", value= presets_layout[4])
    updateTextInput(session, "range_y", value= presets_layout[5])
    # updateCheckboxInput(session, "transform", value = presets_layout[6])
    # updateRadioButtons(session, "transform_x", selected = presets_layout[7])
    # updateRadioButtons(session, "transform_y", selected = presets_layout[8])    
    #    updateCheckboxInput(session, "add_description", value = presets_layout[9])
    if ((presets_layout[6])=='X') {
      updateNumericInput(session, "plot_height", value= presets_layout[7])
      updateNumericInput(session, "plot_width", value= presets_layout[8])
    }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?color ################
  
  if (!is.null(query[['color']])) {

    presets_color <- query[['color']]
    presets_color <- unlist(strsplit(presets_color,";"))

    updateSelectInput(session, "adjustcolors", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
  }
  
  ############ ?label ################
  
  if (!is.null(query[['label']])) {
    
    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))

    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])
    
    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])
    
    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_title", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_labs", value= presets_label[8])
    
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[9])
    updateNumericInput(session, "fnt_sz_cand", value= presets_label[10])
    updateCheckboxInput(session, "add_legend", value = presets_label[11])    
    # updateTextInput(session, "legend_title", value= presets_label[12])


  }
  

  
  
  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    # observe(print((query[['url']])))
    updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  
})

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
})

######### GENERATE URL with the settings #########

url <- reactive({
  
  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)
  
  data <- c(input$data_input, "", input$x_var, input$y_var, input$g_var)
  
  # vis <- c(input$pointSize, input$alphaInput, input$fc_cutoff, input$p_cutoff, input$direction, input$criterion)
  #Convert upper/lower boundary to comma seperated values
  fc <- input$fc_cutoff
  fc <- paste(fc, collapse=",")

    vis <- c(input$pointSize, input$alphaInput, fc, input$p_cutoff, input$direction, input$criterion)
  
  #Convert the list of genes to a comma-seperated string  
  a <- input$user_label_list
  a <- paste(a, collapse=",")
  
  #as.character is necessary; if omitted TRUE is converted to 0 and FALSE to 1 which is undesired
  can <- c(input$top_x, as.character(input$show_table), a)

  layout <- c("", "", input$change_scale, input$range_x, input$range_y, "X", input$plot_height, input$plot_width)
  
  #Hide the standard list of colors if it is'nt used
  if (input$adjustcolors != "5") {
    color <- c(input$adjustcolors, "none")
  } else if (input$adjustcolors == "5") {
    color <- c(input$adjustcolors, input$user_color_list)
  }
  
  ############ COLOR ##########

  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_title, input$fnt_sz_labs, input$fnt_sz_ax, input$fnt_sz_cand, input$add_legend, input$legend_title, input$hide_labels_y)

  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data) 
  
  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis)
  
  
  can <- sub("FALSE", "", can)
  can <- paste(can, collapse=";")
  can <- paste0("can=", can)
  
  # 
  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout)
  # 
  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color)
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  # stim <- sub("FALSE", "", stim)
  # stim <- paste(stim, collapse=";")
  # stim <- paste0("stim=", stim) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}
  
  # parameters <- paste(data, vis,layout,color,label,stim,url, sep="&")
  
  parameters <- paste(data,vis,can,layout,color,label,url, sep="&")

  
    preset_URL <- paste(base_URL, parameters, sep="?")
  
  # observe(print(parameters))

  return(preset_URL)
})


############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch VolcaNoseR with the current setting"))
})

  ################ SELECT COLUMNS AND ANNOTATE CHANGES #########
df_filtered <- reactive({     
    
      df <- df_upload()
      
      observe({print('input')})
      observe({print(head(df))})
      
      x_choice <- input$x_var
      y_choice <- input$y_var

      koos <- df %>% select(`Measurement_1` = !!x_choice , `Measurement_2` = !!y_choice)
      
      #Filter empty rows
      koos <- koos %>% na.omit()

      koos <- koos %>% mutate(Difference = Measurement_1-Measurement_2, Average = 0.5*Measurement_1+0.5*Measurement_2, Ratio=Measurement_1/Measurement_2, Percentage=100*Difference/(Average))
    
    
    if (input$plot_type==1) {
      koos$y <- koos$Difference
      
      
    } else if (input$plot_type==2) {
      koos$y <- koos$Percentage
      
    } else if (input$plot_type==3) {
      koos$y <- log10(koos$Ratio)
      
    } else if (input$plot_type==5) {
    
      koos$y <- koos$Measurement_2
      }
    
    observe({(print(head(koos)))})
    
    ########## SOME STATISTICAL EVALUATION OF THE DATA ##########
    # Test the difference for normality
    test_result <- shapiro.test(koos$y)
    observe({print(test_result$p.value)})

    return(koos)

})
  
df_stats <- reactive({     

  df <- df_filtered()
  
  n <- length(df$y)
  
  if (input$LoA != '3') {
    diff <- mean(df$y)
    stdev <- sd(df$y)
    LoA_hi <- diff + 1.96* stdev
    LoA_lo <- diff - 1.96* stdev
    LoA_d <- LoA_hi - LoA_lo
    
    # Approximate standard error of the standard deviation; source: Bland&Altman (1999)
    SE_d <- stdev / sqrt(n - 1)
    SE_stdev <- 1.71*SE_d
    
    mean_CI_lo = diff + qt((1-0.95)/2, n - 1) * SE_d
    mean_CI_hi = diff - qt((1-0.95)/2, n - 1) * SE_d
    
    LoA_hi_CI_lo = LoA_hi + qt((1-0.95)/2, n - 1)*SE_stdev
    LoA_hi_CI_hi = LoA_hi - qt((1-0.95)/2, n - 1)*SE_stdev
    
    LoA_lo_CI_lo = LoA_lo + qt((1-0.95)/2, n - 1)*SE_stdev
    LoA_lo_CI_hi = LoA_lo - qt((1-0.95)/2, n - 1)*SE_stdev
    
    LoA_widest <- LoA_hi_CI_hi - LoA_lo_CI_lo
    
    
    df_difference <- data.frame(parameter=c('difference', 'upper Limit of Agreement', 'lower Limit of Agreement', 'Difference between LoA'),Value=c(diff,LoA_hi, LoA_lo, LoA_d), lo=c(mean_CI_lo,LoA_hi_CI_lo,LoA_lo_CI_lo, NA),hi=c(mean_CI_hi,LoA_hi_CI_hi,LoA_lo_CI_hi, NA))
    
  
  }
  else if (input$LoA == '3') {
    diff <- median(df$y)
    LoA_hi <- quantile(df$y, probs=0.95)
    
    LoA_lo <- quantile(df$y, probs=0.05)
    # observe({print(LoA_hi, LoA_lo)})
    
    LoA_d <- LoA_hi - LoA_lo

    # Bootstrap to determine median, and the 95% coverage that reflects the LoA
    df_boot <-  as.data.frame(t(replicate(nrep, boot_LoA(df$y))))
    
    # 95%CI of the median
    median_CI <- quantile(df_boot$`50%`, probs=c(0.025,0.975))
    
    LoA_lo_CI <- quantile(df_boot$`5%`, probs=c(0.025,0.975))
    
    LoA_hi_CI <- quantile(df_boot$`95%`, probs=c(0.025,0.975))
    
    LoA_widest <- LoA_hi_CI[2] - LoA_lo_CI[1]
    
    ###### NEED TO ADD 95%CI for Median and LoA #######
    # Draw median 1000x from bootstrapping, calculate percentiles as well:     boot_value <- quantile(new_sample, probs=c(0.05,0.5,0.95))
    
    df_difference <- data.frame(parameter=c('difference', 'upper Limit of Agreement', 'lower Limit of Agreement', 'Difference between LoA'),Value=c(diff,LoA_hi, LoA_lo, LoA_d), lo=c(median_CI[1],LoA_hi_CI[1],LoA_lo_CI[1], NA), hi=c(median_CI[2],LoA_hi_CI[2],LoA_lo_CI[2], NA))
    
  }
  # rownames(df_difference) <- NULL
  observe({print(df_difference)})
  

  
  
  linearMod <- lm(y ~ Average, data=df)
  x <- confint(linearMod, level=0.95)
  

  intercept_lo <-  x[1,1]
  intercept_hi<-  x[1,2]
  slope_lo <- x[2,1]
  slope_hi <- x[2,2] 
  
  intercept <- linearMod$coefficients[1]
  slope <- linearMod$coefficients[2]
  

  df_slopes <- data.frame(parameter=c('intercept', 'slope', 'widest possible LoA'),Value=c(intercept, slope, LoA_widest), lo=c(intercept_lo, slope_lo, NA), hi=c(intercept_hi,slope_hi, NA))
  # Calculate the CI of intercept and slope

  # df_x <- (data.frame(lo=x[,1], hi=x[,2]))
  # df_coef <- bind_cols(df_coef,df_x)
  

  

  df_difference <- bind_rows(df_difference,df_slopes)
  # df_coef <- data.frame(df_coef, row.names = 1)
  
  rownames(df_difference) <- NULL
  
  observe({print(df_difference)})


  return(df_difference)
  
})  

output$data_summary <- renderDataTable(
  datatable(
    df_filtered_stats(),
     colnames = c("Parameter","Value","95%CI lower limit","95%CI upper limit"),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = 2:4))
    ) 
  ) 
     # %>% formatRound(columns = names(df_filtered_stats()), digits=input$digits_table)
) 

  ##### Render the plot ############
  
  
  ##### Set width and height of the plot area
  width <- reactive ({ input$plot_width })
  height <- reactive ({ input$plot_height }) 
  
  output$coolplot <- renderPlot(width = width, height = height, {
    plot(plotdata())
  }
  )
  
  output$qqplot <- renderPlot(width = 400, height = 400, {
    plot(plotqq())
  }
  )

df_filtered_stats <- reactive({
    df <- df_stats()
    
    df <- df %>% mutate_at(c(2:4), round, input$digits_table)

    if (input$LoA=='1') {
      df <- df %>% filter(grepl("difference|upper Limit of Agreement|lower Limit of Agreement|Difference between LoA|intercept|widest possible LoA|slope", parameter))
    } else if (input$LoA=='2') {
      df <- df %>% filter(grepl("difference|intercept|slope", parameter))
    } else if (input$LoA=='3') {
      df <- df %>% filter(grepl("difference|upper Limit of Agreement|lower Limit of Agreement|Difference between LoA|intercept|widest possible LoA|slope", parameter))
    }
    
    # df <- df[-c(2,3,4),]
    return(df)
    
  })
  
  

plotqq <- reactive({
  df <- as.data.frame(df_filtered())
  p <-  ggplot(data = df)
  p <- p +  aes(sample=`y`) + stat_qq() + stat_qq_line()
  p <- p + theme_light(base_size = 16)
    
    #remove gridlines (if selected
    # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
})




plotdata <- reactive({
  
  df <- as.data.frame(df_filtered())


  df_stats <- as.data.frame(df_stats())
  
  slope_CI_lo = round(df_stats[6,3],2)
  slope_CI_hi = round(df_stats[6,4],2)

  
  #95CI of Slope != 0
  if ((slope_CI_lo > 0 && slope_CI_hi > 0) || (slope_CI_lo < 0 && slope_CI_hi < 0)) {
    showNotification(paste("The slope of the difference has a 95%CI [",slope_CI_lo,",",slope_CI_hi,"] that does not include zero suggesting proportional bias: using a regression-based LoA is recommended"), duration = 100, type = "warning")
  }
  
  ########## Determine color use #############
  
  newColors <- c("black", "","grey")
  if (input$adjustcolors < 5) {
    dot_color <- newColors[as.numeric(input$adjustcolors)]
    observe({print(dot_color)})
  } else if (input$adjustcolors == 5) {
    dot_color <- input$user_color_list
  }
  
  ############## Adjust X-scaling if necessary ##########
  
  #Adjust scale if range for x (min,max) is specified
  if (input$range_x != "" &&  input$change_scale == TRUE) {
    rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
    observe({ print(rng_x) })
  } else if (input$range_x == "" ||  input$change_scale == FALSE) {
    
    rng_x <- c(NULL,NULL)
  }
  
    
    ############## Adjust Y-scaling if necessary ##########
    
    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])
    } else if (input$range_y == "" ||  input$change_scale == FALSE) {
      
      rng_y <- c(NULL,NULL)
    }
  
  
  avg <- df_stats[1,2]

  # Calculate Stats for y-variable
  # avg <- mean(df$y)
  # stdev <- sd(df$y)
  
  LoA_hi <- df_stats[2,2]
  LoA_lo <- df_stats[3,2]
  
  n <- length(df$y)
  
  mean_CI_lo = df_stats[1,3]
  mean_CI_hi = df_stats[1,4]
  
  LoA_hi_CI_hi = df_stats[2,4]
  LoA_hi_CI_lo = df_stats[2,3]
  
  LoA_lo_CI_hi = df_stats[3,4]
  LoA_lo_CI_lo = df_stats[3,3]
  
    # Define the plotting object    
    p <-  ggplot(data = df)

    if (input$plot_type!=5) {
      
      #Linear Regression analysis
      linearMod <- lm(y ~ Average, data=df)
      x <- round(confint(linearMod, level=0.95),2)
      y <- (data.frame(lo=x[,1], hi=x[,2]))
      
      
      if(input$add_CI ==TRUE && (input$LoA=='1' || input$LoA=='3')) {

          p <- p + annotate("rect", xmin=-Inf, xmax=Inf, ymax=mean_CI_hi,ymin=mean_CI_lo, fill='grey80',alpha=input$alphaInput_summ) 
          p <- p + annotate("rect", xmin=-Inf, xmax=Inf, ymax=LoA_hi_CI_hi,ymin=LoA_hi_CI_lo, fill='grey80',alpha=input$alphaInput_summ) 
          p <- p + annotate("rect", xmin=-Inf, xmax=Inf, ymax=LoA_lo_CI_hi,ymin=LoA_lo_CI_lo, fill='grey80',alpha=input$alphaInput_summ) 
      }
      
      if (input$LoA =='1' || input$LoA =='3') {
      
      p <- p + geom_hline(yintercept = 0, linetype="solid", color="grey", size=0.5)
      
      p <- p + geom_hline(yintercept = avg, linetype="dotted", color="black", size=1,alpha=input$alphaInput_summ)
      p <- p + geom_hline(yintercept = c(LoA_hi, LoA_lo), linetype="dashed", color="black", size=1,alpha=input$alphaInput_summ)      
      
      
      } else if (input$LoA=='2') {
        
        #Linear regression of y versus Average

        b0 <- linearMod$coefficients[1]
        b1 <- linearMod$coefficients[2]

        
        #Determine residuals
        df$residuals <- linearMod$residuals
        df$abs_residuals <- abs(df$residuals)
        
        #Regress residuals on Average
        linearModRes <- lm(abs_residuals ~ Average, data=df)

        c0 <- linearModRes$coefficients[1]
        c1 <- linearModRes$coefficients[2]
        
        # According to Bland&Altman
        # b0+b1*Average±2.46*(c0+c1*A)
        
        df <- df %>% mutate(Diff = b0+b1*Average,LoA_regr_hi = b0+b1*Average+2.46*(c0+c1*Average), LoA_regr_lo = b0+b1*Average-2.46*(c0+c1*Average))
        
        p <- p+geom_line(data=df, aes(x=Average,y=Diff), linetype="dotted", color="black", size=1,alpha=input$alphaInput_summ)
        p <- p+geom_line(data=df,aes(x=Average,y=LoA_regr_hi), linetype="dashed", color="black", size=1,alpha=input$alphaInput_summ)
        p <- p+geom_line(data=df,aes(x=Average,y=LoA_regr_lo), linetype="dashed", color="black", size=1,alpha=input$alphaInput_summ)

        
      }
      
      
      
      
      p <- p +  aes(x=`Average`) +
        aes(y=`y`) +
        geom_point(alpha = input$alphaInput, size = input$pointSize, shape = 16, color=dot_color)
      
      # p <- p + scale_y_continuous(breaks=c(0, 10, 15, -10))
      # Label average and LoA on secondary axis
      
      if (input$show_labels && (input$LoA =='1' || input$LoA =='3')) {
      p <- p + scale_y_continuous(sec.axis = sec_axis(~ . * 1  , breaks = (round(c(avg, LoA_hi, LoA_lo),input$digits))))
      }
    
        
    } else if (input$plot_type==5) {
      #Indicate cut-offs with dashed lines
      
      p <- p + geom_abline(linetype="solid", color="black", size=1,alpha=input$alphaInput_summ)
      
      p <- p +  aes(x=`Measurement_1`) +
        aes(y=`Measurement_2`) +
        geom_point(alpha = input$alphaInput, size = input$pointSize, shape = 16)
      
      
    }
    if (input$show_rugs == TRUE) {p <- p+geom_rug(alpha=input$alphaInput_summ)}
      
      # This needs to go here (before annotations)
     p <- p+ theme_light(base_size = 16) +
    
      #remove gridlines (if selected
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      
      NULL
    
    #Indicate cut-offs with dashed lines

    
    # if log-scale checked specified
    if (input$scale_log_10)
      p <- p + scale_y_log10() 

    ########## User defined labeling     


    
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    #### If selected, rotate plot 90 degrees CW ####
    # if (input$rotate_plot == TRUE) { p <- p + coord_flip(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))}
    ########## Do some formatting of the lay-out ##########
    
    
    
    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    } else if (input$sheet !=" ") {
      title <- paste(input$sheet, "\n",sep="")
      observe({print('yay')})
      p <- p + labs(title = title)
    }
    
    if (input$align == TRUE)
      p <- p + theme(plot.title = element_text(hjust = 0.5))
    
    # if labels specified
    if (input$unit !="") {
      unit <- paste0("(",input$unit,")")
    } else {unit <- NULL}
    
    
    if (input$label_axes) {
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    } else {
      x_label <- paste0("Average of ",input$x_var, " and " , input$y_var ," ",unit)
      
      if (input$plot_type==1){
      y_label <- paste("Difference:",input$x_var, "-" , input$y_var, unit)
      }
      if (input$plot_type==2){
        y_label <- paste("% Difference (Difference/Average*100%)")
      }
      if (input$plot_type==3){
        y_label <- paste("Difference of the log10 transformed data")
      }
      if (input$plot_type==5){
        y_label <- paste(input$y_var, unit)
        x_label <- paste(input$x_var, unit)
      }
      
      p <- p + labs(x = x_label, y = y_label)
      
    }
    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
      p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }

    p
  })

  
  ######### DEFINE DOWNLOAD BUTTONS FOR ORDINARY PLOT ###########
  
  output$downloadPlotPDF <- downloadHandler(
    filename <- function() {
      paste("BAPlot_", Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = input$plot_width/72, height = input$plot_height/72)
      plot(plotdata())
      
      dev.off()
    },
    contentType = "application/pdf" # MIME type of the image
  )
  
  
  output$downloadPlotPNG <- downloadHandler(
    filename <- function() {
      paste("BAPlot_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
      plot(plotdata())

      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )  
  
  
  ###### Figure legend #########
Fig_legend <- renderText({
    
    df <- as.data.frame(df_filtered())
    
    # if (input$rotate_plot == FALSE) {
      x <- "horizontal"
      y <- "vertical"
    # }
    # else if (input$rotate_plot == TRUE) {
    #   x <- "vertical"
    #   y <- "horizontal"
    # }
    n <- length(df$Difference)
    
    
    avg <- mean((df$y))
    avg_round <- round(avg,input$digits)
    stdev <- sd((df$y))
    test_result <- shapiro.test(df$y)
    
  if (input$plot_type==1) {
      y_var <- c("difference")
      
  } else if (input$plot_type==2) {
    y_var <- c("percentage difference")

  } else if (input$plot_type==3) {
    y_var <- c("difference between log10 transformed data (which is identical to the log10 of the ratio)")

  }

    if (input$plot_type!=5) {
    pvalue <- round(test_result$p.value,4)
    if (pvalue==0) {pvalue <- c("<0.0001")}
    

    ##################################################    
    ######## Take values from df_stats here ##########
    ##################################################
    df_stats <- df_stats() 
    
    
    LoA_hi <- df_stats[2,1]
    LoA_lo <- df_stats[3,1]
    
    limits <- round(c(LoA_lo,LoA_hi),input$digits)
    
    if (input$LoA =='1' || input$LoA =='3') {
    stats <- paste("dotted line indicates the average",y_var," of ",avg_round," and the dashed lines indicate the Limits of Agreement:",limits[1], "&", limits[2],sep=" ")
    } else if (input$LoA =='2') {
      stats <- paste("dotted line indicates the difference and the dashed lines indicate the Limits of Agreement (LoA). Both the difference and LoA are based on regression analysis. ",sep=" ")
    }

      if (pvalue <0.05) {
        stats <- paste(stats,"The Shapiro-Wilk test of the difference returns a p-value of",pvalue,"which does NOT support a normal distribution.", sep=" ")
      } else if (pvalue >0.05) {
        stats <- paste(stats,"The Shapiro-Wilk test of the difference returns a p-value of",pvalue,"which does support a normal distribution.", sep=" ")
      }
    } else {
      stats <- paste("solid line reflects identical values (y=x).", sep="")
    }
    Legend <- c('</br></br><h4>Figure description</h4>')
    
    #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
    
    Legend<-append(Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
    
    Legend <- NULL
    
    Legend<-append(Legend, paste("Graph that shows the data as dots (visibility: ", input$alphaInput, "). ", sep=""))
    
    Legend<-append(Legend, paste("The ", stats, sep=""))
    
    if (input$add_CI==TRUE && input$plot_type!=5 && input$LoA =='1' ){
      
      Legend<-append(Legend, paste("The grey bands (visibility: ", input$alphaInput_summ, ") reflect the 95% confidence intervals. ", sep=""))
    }
    Legend <-append(Legend, paste("The number of datapoints is: " ,n, ". ",sep=""))
    
    return(Legend)
    
  })
  
  ######## Figure Legend in HTML format #############
  
  output$LegendText <- renderText({
    
    if (input$add_description == FALSE) {return(NULL)}
    
    HTML_Legend <- c('</br></br><h4>Figure description</h4>')
    
    #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
    
    HTML_Legend <-append(HTML_Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
    
    HTML_Legend <- append(HTML_Legend, Fig_legend())
    
    HTML_Legend <- append(HTML_Legend, paste("</p>"))
    
  })
  
  
  output$stats <- renderText({
    
    repeat_coeff <- c("not available (select multiple repeats)")
    
    if (length(input$repeats) >1) {
    koos <- df_upload() %>% dplyr::select(input$repeats) %>% pivot_longer(cols = everything(), names_to = "Replicate", values_to = "Value")
    
    #Calculate the RC and ICC
    df_id <- koos %>% group_by(Replicate) %>% dplyr::mutate(Subject=row_number()) %>% ungroup()
    
    k <- length(unique(df_id$Replicate))
    n <- length(unique(df_id$Subject))
    
    TSS <- df_id %>% dplyr::summarise(TSS = sos(Value)) %>% pull(TSS)
    SSw <- df_id %>% group_by(Subject) %>% dplyr::summarise(sos = sos(Value)) %>% pull(sos) %>% sum()
    SSb <- TSS-SSw
    MSw <- SSw / (n*(k-1))
    MSb <- SSb / (n-1)
    ICC <-  (MSb - MSw) / (MSb + ((k-1)*MSw))
    RC = 1.96*sqrt(2) * sqrt(MSw)
    RC_CI_upper <- 2.77*sqrt(SSw/qchisq(0.025, (n*(k-1))))
    RC_CI_lower <- 2.77*sqrt(SSw/qchisq(0.975, (n*(k-1))))
    
    
    # df_var <- df_id %>%
    #   group_by(Subject) %>%
    #   summarise(var = var(Value), mean = mean(Value)) %>%
    #   summarise(mean_var = mean(var), mean_mean = mean(mean), var_mean=var(mean))
    # 
    # df_var <- df_var %>% mutate(repeat_coeff = 1.96*sqrt(2*mean_var))
    
    # repeat_coeff <- RC %>% round(input$digits_table)
    repeat_coeff <- paste0(round(RC,input$digits_table) , " [",round(RC_CI_lower, input$digits_table),"-",round(RC_CI_upper, input$digits_table),"]")
    }
    
    
    
    #### Check for normality of the y-variable
    df <- df_filtered()
    
    avg <- mean(df$Average)
    
    #Average of the within sum of squares
    # sum_sqrs <- sum((df$Measurement_1-df$Average)^2+(df$Measurement_2-df$Average)^2)/length(df$Average)
    # 
    # rep <- sqrt(sum_sqrs)*2.77
    # rep2 <- rep
    # rep2 <- round(rep2, 1)
    # 
    # rep <- rep/length(df$Average)*100
    # 
    # 
    # rep <- round(rep, 1)
    

    
    test_result <- shapiro.test(df$y)
    pvalue <- (test_result$p.value)
    
    if (pvalue <0.05 && input$LoA=='1') {
      showNotification(paste("The Shapiro-Wilk test for normality yields a p-value <0.05, suggesting that an ordinary analysis is unsuitable. Data transformation and/or a regression or non-parametric analysis may be more suitable"), duration = 100, type = "warning")
    }
    
    
    
    # if (pvalue==0) {pvalue <- c("<0.001")}

    HTML_Legend <- c('</br></br><h4>Statistics of the distribution</h4>')
    
    #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
    
    # HTML_Legend <-append(HTML_Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
    
    HTML_Legend <- append(HTML_Legend, paste('<p>The Shapiro-Wilk test for normality of the distribution of the y-variable returns a p-value of ',formatC(pvalue, format = "e", digits = 2), sep=""))
    
    # if (length(input$repeats) >1) {
      HTML_Legend <- append(HTML_Legend, paste("</br><h4>Statistics of the Repeatability</h4>"))
    
    # HTML_Legend <- append(HTML_Legend, paste('</br>The old coefficient of reproducibility is ',rep,' %', sep=""))
    HTML_Legend <- append(HTML_Legend, paste('The repeatability coefficient [95%CI] for the selected repeats is ', repeat_coeff," ", input$unit, sep=""))
    # } 
    # else {
    #   HTML_Legend <- append(HTML_Legend, paste("</br>Select multiple repeats to calculate the repeatability coefficient"))
    # 
    # }
    # HTML_Legend <- append(HTML_Legend, paste('</br>The repeatability coefficient of measurement 2 is ',rep_2, sep=""))    
    HTML_Legend <- append(HTML_Legend, paste("</p><hr>"))
    
  })
  
  
  

########### Update count #########
# Reactively update the client.
output$count <- renderText({
  vals$count
})

# When a session ends, decrement the counter.
session$onSessionEnded(function(){
  isolate(vals$count <- vals$count - 1)
})

######## The End; close server ########################
} #Close server


# Run the application 
shinyApp(ui = ui, server = server)

