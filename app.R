
# color parameters from file
# make color range reactive
# allow asyncronic from excel

# filters
# heat map pixel highlight
# add button for d_folder in excel?

# check user access to data folder
# create bat file on different button
# clean app file

# check col row labels and mean vakues!


#====================================================

rm(list=ls())
graphics.off()


mirror = 'https://cloud.r-project.org/'

if (!is.element("shiny", installed.packages()[,1])) {install.packages("shiny", dependencies = TRUE, repos = mirror)}
if (!is.element("shinyjs", installed.packages()[,1])) {install.packages("shinyjs", dependencies = TRUE, repos = mirror)}
if (!is.element("shinyWidgets", installed.packages()[,1])) {install.packages("shinyWidgets", dependencies = TRUE, repos = mirror)}
if (!is.element("DT", installed.packages()[,1])) {install.packages("DT", dependencies = TRUE, repos = mirror)}
if (!is.element("readxl", installed.packages()[,1])) {install.packages("readxl", dependencies = TRUE, repos = mirror)}
if (!is.element("writexl", installed.packages()[,1])) {install.packages("writexl", dependencies = TRUE, repos = mirror)}
if (!is.element("tidyverse", installed.packages()[,1])) {install.packages("tidyverse", dependencies = TRUE, repos= mirror)}
if (!is.element("plotly", installed.packages()[,1])) {install.packages("plotly", dependencies = TRUE, repos = mirror)}
if (!is.element("readr", installed.packages()[,1])) {install.packages("readr", dependencies = TRUE, repos= mirror)}
if (!is.element("reshape2", installed.packages()[,1])) {install.packages("reshape2", dependencies = TRUE, repos= mirror)}


#------------------------


library(shiny)
library( shinyWidgets)
library(shinyjs)

library(DT)
library(readxl)
library(writexl)
library(tidyverse)
library(plotly)
library(readr)
library(reshape2)





infomessage = function(title, txt)
{
    showModal(
        modalDialog( title = title,   txt,   easyClose = TRUE,   footer = NULL))        
}





#   add all possible control and logging!!!!

w_folder = getwd()
if (w_folder == "H:/PD/Vel25/vel25_analysis/vel25_app1") { 
    w_folder = 'H:/PD/Vel25/vel25_analysis/bin/tmp/tmp/vel25_vna-master' 
    }
print(w_folder)


w_fname = gsub('bin/tmp/tmp/vel25_vna-master', 'work_file.xlsm', w_folder)
print(w_fname)


# d_folder= '\\\\tmi21\\Private\\R&D\\MechProdDev\\Projects\\Concept Development\\1805 Velocity25\\Testing\\Electrical\\Measurements\\V25-050'

# get user name
# Sys.getenv("USERNAME")





#============================ UI =========================================



# Define UI for application that draws a histogram
ui <- fluidPage(
    

    # Application title
    titlePanel(h3("VNA Data visualization")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("data_up_butt", "Load data", width='100%'),
            actionButton("data_rd_butt", "Read data",  width='100%'),

            br(), br(),
            pickerInput("Lev1_input", "Level 1:",  choices = c(''), 
                        options = list('actions-box' = TRUE), multiple = FALSE) ,
 
            pickerInput("Lev2_input", "Level 2:",  choices = c(''), 
                        options = list('actions-box' = TRUE), multiple = FALSE)             
        
         , width = 2   
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            fluidRow(width=16,
                     column(width=6,
                            plotlyOutput("heatmap1") ),
                     
                     column(width=6,
                            plotlyOutput("heatmap2") )

            ),
            fluidRow(width=16,
                     column(width=6,
                            plotlyOutput("heatmap3") ),
                     
                     column(width=6,
                            plotlyOutput("heatmap4") )
                     
            )            
        )
    )
)

#============================== SERVER =====================================

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    rea <- reactiveValues(df_ht = NULL)
    rea_st <- reactiveValues(max_col = NULL, fmax = NULL, fmin = NULL, zmax = NULL, 
                               zmin = NULL, rmax = NULL, rmin = NULL, xmax = NULL, xmin = NULL)    
    
    
    
    LoadSetts = function(w_fname) {
        
        if (!file.exists(w_fname)) {
            s = paste0('No file ', w_fname, '')
            infomessage('Error', s)
            return(-1) 
        }
        
        setts  <- read_excel(w_fname, sheet = "Data_Parameters")
        
        if (length(setts)==0) {
            s = paste0('Unable to read settings from the file: ', w_fname)
            infomessage('Error', s)
            return(-1)     
        }
        
        d_folder <<- setts[which(setts$Name == 'd_folder'), ]$Value
        rea_st$max_col <- as.numeric(setts[which(setts$Name == 'max_col'), ]$Value)
        rea_st$fmax <- as.numeric(setts[which(setts$Name == 'fmax'), ]$Value) *1e6
        rea_st$fmin <- as.numeric(setts[which(setts$Name == 'fmin'), ]$Value) *1e6
        rea_st$zmax <- as.numeric(setts[which(setts$Name == 'zmax'), ]$Value)
        rea_st$zmin <- as.numeric(setts[which(setts$Name == 'zmin'), ]$Value)  
        rea_st$rmax <- as.numeric(setts[which(setts$Name == 'rmax'), ]$Value)
        rea_st$rmin <- as.numeric(setts[which(setts$Name == 'rmin'), ]$Value)  
        rea_st$xmax <- as.numeric(setts[which(setts$Name == 'xmax'), ]$Value)
        rea_st$xmin <- as.numeric(setts[which(setts$Name == 'xmin'), ]$Value)      
        
        if (length(d_folder)==0) {
            s = paste0('Parameter d_folder not found!')
            infomessage('Error', s)
            return(-1)    
        }
        if (length(rea_st$max_col)==0) {
            s = paste0('Parameter max_col not found!')
            infomessage('Error', s)
            return(-1)    
        }
    
    
        return(0)   
    }
    
    observe(
        LoadSetts(w_fname) # call settings load
    )
    
    
 
    UploadData = function(d_folder) {
        
        # list of available files
        f_list = list.files(d_folder, include.dirs = FALSE, recursive=TRUE)

        #  check if the files are of csv type
        ix = c()
        f = f_list[1]
        for (f in f_list) { 
            s = unlist(strsplit(f, "\\."))
            ix = c(ix, (("csv" %in% s) ) & (substring(s[1], 1, 1) != '~') &  
                       (!('data_out' %in% s[1])) &  (!('info_out' %in% s[1])) ) 
        }
        f_list = f_list[ix]
        
        if (length(f_list) == 0) {
            df_error = data.frame(c('No available csv files!'))
            colnames(df_error) = 'Error'
            
            s = paste0('No available csv files!')
            infomessage('Error', s)
            return(-1)                 
        }
        
        f_list = gsub("/", "\\\\", f_list)
        
        #============================================================================
        
        # find maximum depth of levels
        max_lev = 1
        for (c in f_list) {
            k = length(unlist(strsplit(c, "\\\\")))
            if (k > max_lev) {max_lev = k}
        }
        levs =  paste0('lev_', seq( 1, max_lev-1 ) )
        
        
        out = data.frame()
        info = data.frame()
        
        withProgress({
            
            f_name = f_list[3]  
            for (f_name in f_list) {
                
                incProgress(1/length(f_list), detail = 'data loading')
                setProgress(message = f_name)
                
                print('......................................................')
                
                print(f_name)
                id = match(f_name, f_list)
                
                data  <- read_csv(paste0(d_folder, "\\\\", f_name ), skip=4, col_types = cols())
                data  <- data[, !is.na(colSums(data)) ]
                
                
                # file path parsing
                x = unlist(strsplit(f_name, "\\\\"))
                if (length(x) > max_lev) { 
                    s = paste0('Number of levels exceeds max_lev!')
                    infomessage('Error', s)
                    return(-1)                     
                    }
                
                tmp = data.frame(0)
                for (i in seq( 1, max_lev-1) ) { tmp[, i] <- NA }
                colnames(tmp) = levs
                for (c in head(x, -1) ) {tmp[paste0('lev_', match(c, x))] <- c}
                tmp['w_fname'] = tail(x, 1)
                
                tmp['ncols'] = ncol(data)
                tmp['nrows'] = nrow(data)
                tmp['path'] = f_name  
                tmp['id'] = id    
                info = rbind(info, tmp)
                
                
                if (ncol(data) > 1) {
                    
                    if (ncol(data)+1 < rea_st$max_col) {
                        for (i in seq( ncol(data)+1, rea_st$max_col, by=1) ) { data[, i] <- NA }
                    }
                    colnames(data) = c('F', 'S', 'Z', 'R', 'X') 
                    
                    data['Smin'] = data$S==min(data$S)
                    
                    data['id'] = id
                    
                    out = rbind(out, data)
                }    
            }
            
        })
        
        # parse coordinates: row and col from file name
        tmp = c()
        for (c in unique(info$w_fname) ) {
            
            pattern <- "[-]([0-9]{1,2})[-]([0-9]{1,2})[-]"
            b = str_match(c, pattern)
            r = c(c, b[2], b[3])
            
            tmp = rbind(tmp, r)
        }
        tmp = as.data.frame(tmp)
        colnames(tmp) = c('w_fname', 'col', 'row')
        tmp['col'] = str_pad(tmp$col, 2, pad = "0")
        tmp['row'] = str_pad(tmp$row, 2, pad = "0")
        
        info <- merge(x = info, y = tmp, by = 'w_fname', all.x = TRUE)
        info = info[ c("id",  "path", "nrows", "ncols", levs, "w_fname", "col", "row" )]
        
        
        # data for heatmaps
        df_ht = info[ c("id",  levs, "w_fname", "col", "row" )]
        df_ht = df_ht[which(!is.na(df_ht$col)), ]
        df_ht <- merge(x = df_ht, y = out[out$Smin, ], by = 'id', all.x = TRUE)
        
        return ( list('out' = out, 'info' = info, 'df_ht' = df_ht) )
    }
    
    

    SaveData = function(w_folder) {
        fdir = gsub('bin/tmp/tmp/vel25_vna-master', '', w_folder)
        
        outfilename = paste0(fdir, 'data_out.csv')
        write.table(data_0, outfilename, sep = ",",
                    col.names = !file.exists(outfilename), row.names = F, append = F)
        
        outfilename = paste0(fdir, 'info_out.xlsx')
        write_xlsx(list(Info = info_0, HM_data = df_ht_0), outfilename)        
        # write.table(info_0, outfilename, sep = ",",
        #             col.names = !file.exists(outfilename), row.names = F, append = F)
    }

    
    ReadData = function(w_folder) {
        fdir = gsub('bin/tmp/tmp/vel25_vna-master', '', w_folder)
        
        outfilename = paste0(fdir, 'data_out.csv')
        if (!file.exists(outfilename)) {
            infomessage('Error', 'Please load data')            
            return(-1)
        }
        data_0  <<- read_csv(outfilename,  col_types = cols())
        
        outfilename = paste0(fdir, 'info_out.xlsx')
        if (!file.exists(outfilename)) {
            infomessage('Error', 'Please load data')            
            return(-1)
        }
        info_0  <<- read_excel(outfilename, sheet = "Info")
        df_ht_0  <<- read_excel(outfilename, sheet = "HM_data")
        
        r = LoadSetts(w_fname)
        if (r == -1) {
            infomessage('Error', 'Settings are not loaded')            
            return(-1)
        } 
        return(0)
    }    
        
    #-----------------  interface functions --------------------
 

    # load data button   
    observeEvent(input$data_up_butt, {
        
        LoadSetts(w_fname)
        
        if (!dir.exists(d_folder)) {
            s = paste0('Folder ', d_folder, ' not found!')
            infomessage('Error', s)
            return()
        }  

        res = suppressWarnings(UploadData(d_folder)) 
        data_0 <<- res$out
        info_0 <<- res$info
        df_ht_0 <<- res$df_ht
        
        
        withProgress({
            
            incProgress(0.7, detail = '')
            setProgress(message = 'Data saving')
            
            SaveData(w_folder)
        })
        

        lev1_list <- sort(as.vector(unique(df_ht_0$lev_1)))
        updatePickerInput(session, "Lev1_input",
                          choices = lev1_list, selected = lev1_list  )
        
        lev2_list <- c()
        if ('lev_2' %in% colnames(df_ht_0)) {
            lev2_list <- sort(as.vector(unique(df_ht_0$lev_2)))
        }
        updatePickerInput(session, "Lev2_input",
                          choices = lev2_list, selected = lev2_list  )    
        

    })    
 
    
    # read data button
    observeEvent(input$data_rd_butt, {
        
        withProgress({
            
            incProgress(0.7, detail = '')
            setProgress(message = 'Data reading')
            
            r = ReadData(w_folder)
        })
        
        if (r == -1)   return()
        
        lev1_list <- sort(as.vector(unique(df_ht_0$lev_1)))
        updatePickerInput(session, "Lev1_input",
                          choices = lev1_list, selected = lev1_list  )
        
        lev2_list <- c()
        if ('lev_2' %in% colnames(df_ht_0)) {
            lev2_list <- sort(as.vector(unique(df_ht_0$lev_2)))
        }
        updatePickerInput(session, "Lev2_input",
                          choices = lev2_list, selected = lev2_list  )     
    })    
    
    
    observeEvent(input$Lev1_input, {
        if (exists('df_ht_0')) {
 
            tmp = df_ht_0 %>% filter(lev_1 %in% input$Lev1_input)
         
            if ('lev_2' %in% colnames(tmp)) {
                x <- sort(as.vector(unique(tmp$lev_2)))
                updatePickerInput(session, "Lev2_input", choices = x, selected = x  )
            } else { 
                rea$df_ht = tmp 
                }

        }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$Lev2_input, {
        if (exists('df_ht_0')) {
            
            tmp = df_ht_0 %>% filter(lev_1 %in% input$Lev1_input)
            
            if ('lev_2' %in% colnames(tmp)) {
                tmp = tmp %>% filter(lev_2 %in% input$Lev2_input)  
            }                        
            rea$df_ht = tmp
            
        }
    }, ignoreNULL = FALSE)
    
    
    
    
    
           
    output$heatmap1 <- renderPlotly({
        
        if (is.null(rea$df_ht)) return()
            
        if (nrow(rea$df_ht) > 0) {
            
            tmp = rea$df_ht

            v = "F"

            tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
            rownames(tmp) = paste0("r", tmp$row)
            colnames(tmp) = paste0("c", colnames(tmp))
            tmp = tmp[, 2:ncol(tmp)]

            palette <- colorRampPalette(c("red", "green", "red"))
            
            p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp), 
                         type = "heatmap", colors = palette(50), opacity=0.5,
                         zauto = FALSE, zmin = rea_st$fmin, zmax = rea_st$fmax) %>%
                
            layout(title="Frequency F")  %>%    
                
            config(displaylogo = FALSE) %>%
            config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))

            p       
        }
    })

    

    
    output$heatmap2 <- renderPlotly({

        if (is.null(rea$df_ht)) return()
        
        if (nrow(rea$df_ht) > 0) {

            tmp = rea$df_ht

            v = "Z"
            if (sum(!is.na(tmp[v])) >0) {
                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = T)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                palette <- colorRampPalette(c("red", "green", "red"))
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50),  opacity=0.5,
                             zauto = FALSE, zmin = rea_st$zmin, zmax = rea_st$zmax) %>%
                layout(title="Impedance Z")  %>%
    
                config(displaylogo = FALSE) %>%
                config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
                p
            }
        }
    })

    
    output$heatmap3 <- renderPlotly({

        if (is.null(rea$df_ht)) return()
        
        if (nrow(rea$df_ht) > 0) {

            tmp = rea$df_ht

            v = "R"
            
            if (sum(!is.na(tmp[v])) >0) {

                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                palette <- colorRampPalette(c("red", "green", "red"))
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50), opacity=0.5,
                             zauto = FALSE, zmin = rea_st$rmin, zmax = rea_st$rmax) %>%
    
                    layout(title="Resistance R")  %>%
    
                    config(displaylogo = FALSE) %>%
                    config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
                p
            }
        }
    })




    output$heatmap4 <- renderPlotly({

        if (is.null(rea$df_ht)) return()
        
        if (nrow(rea$df_ht) > 0) {

            tmp = rea$df_ht

            v = "X"
            
            if (sum(!is.na(tmp[v])) >0) {
                
                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                palette <- colorRampPalette(c("red", "green", "red"))
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50),  opacity=0.5,
                             zauto = FALSE, zmin = rea_st$xmin, zmax = rea_st$xmax) %>%
                    layout(title="Reactance X")  %>%
    
                    config(displaylogo = FALSE) %>%
                    config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
                p
            }
        }
    })
    
    session$onSessionEnded(function() {
        
        stopApp()   # close command window
    })    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
