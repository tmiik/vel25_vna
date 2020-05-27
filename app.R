



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


library(shiny)
library( shinyWidgets)
library(shinyjs)
#library(shinydashboard)
#library(shinyBS)

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



# move defects detection to backend server !!!


#w_folder= getwd()

folder= '\\\\tmi21\\Private\\R&D\\MechProdDev\\Projects\\Concept Development\\1805 Velocity25\\Testing\\Electrical\\Measurements\\V25-050'
folder = 'H:\\PD\\Vel25\\data2\\data_load'
max_col = 5  # maximum number of columns in dataset

lev2 = "TN004-Si"






SaveData = function() {
    outfilename = paste0(folder, "\\\\", 'data_out.csv')
    write.table(out, outfilename, sep = ",",
                col.names = !file.exists(outfilename), row.names = F, append = T)
    
    outfilename = paste0(folder, "\\\\", 'info_out.csv')
    write.table(info, outfilename, sep = ",",
                col.names = !file.exists(outfilename), row.names = F, append = T)

}





#=====================================================================



# Define UI for application that draws a histogram
ui <- fluidPage(
    

    # Application title
    titlePanel(h3("VNA Data visualization")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("data_up_butt", "Load data"),
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

            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rea <- reactiveValues(df_ht = NULL)
    
    
    
    UploadData = function() {
        
        #d_list = list.dirs(folder)
        #f_list = dir(folder, include.dirs = FALSE, recursive=TRUE)
        
        
        # list of available files
        f_list = list.files(folder, include.dirs = FALSE, recursive=TRUE)
        #View(f_list)
        
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
            print(df_error)
            
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
                
                incProgress(1/length(f_list), detail = '')
                setProgress(message = f_name)
                
                print('......................................................')
                
                print(f_name)
                id = match(f_name, f_list)
                
                data  <- read_csv(paste0(folder, "\\\\", f_name ), skip=4, col_types = cols())
                data  <- data[, !is.na(colSums(data)) ]
                
                
                # file path parsing
                x = unlist(strsplit(f_name, "\\\\"))
                if (length(x) > max_lev) { stop('Number of levels exceeds max_lev!') }
                
                tmp = data.frame(0)
                for (i in seq( 1, max_lev-1) ) { tmp[, i] <- NA }
                colnames(tmp) = levs
                for (c in head(x, -1) ) {tmp[paste0('lev_', match(c, x))] <- c}
                tmp['fname'] = tail(x, 1)
                
                tmp['ncols'] = ncol(data)
                tmp['nrows'] = nrow(data)
                tmp['path'] = f_name  
                tmp['id'] = id    
                info = rbind(info, tmp)
                
                
                if (ncol(data) > 1) {
                    
                    if (ncol(data)+1 < max_col) {
                        for (i in seq( ncol(data)+1, max_col, by=1) ) { data[, i] <- NA }
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
        for (c in unique(info$fname) ) {
            
            pattern <- "[-]([0-9]{1,2})[-]([0-9]{1,2})[-]"
            b = str_match(c, pattern)
            r = c(c, b[2], b[3])
            
            tmp = rbind(tmp, r)
        }
        tmp = as.data.frame(tmp)
        colnames(tmp) = c('fname', 'col', 'row')
        tmp['col'] = str_pad(tmp$col, 2, pad = "0")
        tmp['row'] = str_pad(tmp$row, 2, pad = "0")
        
        info <- merge(x = info, y = tmp, by = 'fname', all.x = TRUE)
        info = info[ c("id",  "path", "nrows", "ncols", levs, "fname", "col", "row" )]
        
        
        # data for heatmaps
        df_ht = info[ c("id",  levs, "fname", "col", "row" )]
        df_ht = df_ht[which(!is.na(df_ht$col)), ]
        df_ht <- merge(x = df_ht, y = out[out$Smin, ], by = 'id', all.x = TRUE)
        
        return ( list('out' = out, 'info' = info, 'df_ht' = df_ht) )
    }
    
    
    
    infomessage('location', getwd())
    
    
    
    
    
    observeEvent(input$data_up_butt, {
        
        res = suppressWarnings(UploadData()) 
        tmp = res$df_ht
        tmp = tmp[which(tmp$lev_2==lev2),]        
        rea$df_ht = tmp
        
    })    
    
    output$heatmap1 <- renderPlotly({

        if (length(rea$df_ht) > 0) {
            
            tmp = rea$df_ht

            v = "F"
            vavg = 1e7
            eps = 0.002
            
            
            tmp = dcast(tmp, row ~ col, value.var = v) #, fun.aggregate = sum)
            #rownames(tmp) = paste0('r', tmp$row)
            tmp = tmp[, 2:ncol(tmp)]
            #colnames(tmp) = paste0('c', colnames(tmp))
            
            tmp1 = rea$df_ht
            
            palette <- colorRampPalette(c("red", "green", "red"))
            
            p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp), 
                         type = "heatmap", colors = palette(50), opacity=0.5,
                         zauto = FALSE, zmin = vavg*(1+eps), zmax = vavg*(1-eps)) %>%
                
                
            config(displaylogo = FALSE) %>%
            config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))

            p       
        }
    })

    

    
    
    

    output$heatmap2 <- renderPlotly({
        
        if (length(rea$df_ht) > 0) {
            
            tmp = rea$df_ht
            
            v = "Z"
            vavg = 50
            eps = 0.1
    
            
            tmp = dcast(tmp, row ~ col, value.var = v) #, fun.aggregate = sum)
            #rownames(tmp) = paste0('r', tmp$row)
            tmp = tmp[, 2:ncol(tmp)]
            #colnames(tmp) = paste0('c', colnames(tmp))
            
            
            palette <- colorRampPalette(c("red", "green", "red"))
            
            p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp), 
                         type = "heatmap", colors = palette(50),  opacity=0.5,
                         zauto = FALSE, zmin = vavg*(1+eps), zmax = vavg*(1-eps)) %>%
                
            config(displaylogo = FALSE) %>%
            config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
            
            p       
        }
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
