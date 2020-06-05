

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



myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
            err <<- e
            NULL
        }), warning=function(w) {
            warn <<- w
            invokeRestart("muffleWarning")
        })
    list(value=value, warning=warn, error=err)
}


#========================================================================

w_folder = getwd()
if (w_folder == "H:/PD/Vel25/vel25_analysis/vel25_app1") { 
    w_folder = 'H:/PD/Vel25/vel25_analysis/bin/tmp/tmp/vel25_vna-master' 
    }
#print(w_folder)



if ( grepl('vel25_vna-master', w_folder, fixed = TRUE) ) {
    arcname = 'vel25_vna-master'
}

if ( grepl('test-master', w_folder, fixed = TRUE) ) {
    arcname = 'test-master'
}
w_fname = gsub(paste0('bin/tmp/tmp/', arcname), 'work_file.xlsm', w_folder) 
#print(w_fname)


palette <- colorRampPalette(c("indianred1", "chartreuse2", "indianred1"))


#============================ UI =========================================



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
            h5("Average values are shown on heatmaps in the case of multiple files!"),
            fluidRow(width=16,
                     column(width=12,
                            plotlyOutput("heatmap1", width = '100%', height = 500) ) ),
            br(), br(),
            fluidRow(width=12,
                     column(width=12,
                            plotlyOutput("heatmap2", width = '100%', height = 500) ) ), 
            
            br(), br(),
            fluidRow(width=16,
                     column(width=12,
                            plotlyOutput("heatmap3", width = '100%', height = 500) ) ),
            br(), br(),
            fluidRow(width=16,
                     column(width=12,
                            plotlyOutput("heatmap4", width = '100%', height = 500) ) ),
            br(), br(),
            fluidRow(width=12,
                     column(width=12,
                            plotlyOutput("heatmap5", width = '100%', height = 500) )
                     
            )            
        )
    )
)

#============================== SERVER =====================================


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
        
        withProgress({
        
            incProgress(0.7, detail = '')
            setProgress(message = 'Files analizing')
            
            # list of available files
            f_list = list.files(d_folder, include.dirs = FALSE, recursive=TRUE)
    
            #  check if the files are of csv type
            ix = c()
            f = f_list[253]
            for (f in f_list) { 
                s = unlist(strsplit(f, "\\."))  # split by dot '.'
                s1 = unlist(strsplit(f, "/"))  # split by slash '/'
                  
                ix = c(ix, (tail(s, 1)=='csv' ) & !grepl('~', f, fixed = TRUE) &
                           !("OLD" %in% toupper(s1)) & 
                           !grepl('DATA_OUT', toupper(tail(s1, 1)), fixed = TRUE) & 
                           !grepl('INFO_OUT', toupper(tail(s1, 1)), fixed = TRUE) )
                       
            }
            f_list = f_list[ix]
        })
        
        if (length(f_list) == 0) {
            df_error = data.frame(c('No available csv files!'))
            colnames(df_error) = 'Error'
            
            s = paste0('No available csv files!')
            infomessage('Error', s)
            return()                 
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
                
                incProgress(1/length(f_list), detail = f_name)
                setProgress(message = 'Data loading')
                
                print('......................................................')
                
                print(f_name)
                id = match(f_name, f_list)
                
                data  <- read_csv(paste0(d_folder, "\\\\", f_name ), skip=4, col_types = cols())
                if (ncol(data) < 2) next
                
                # exclude columns containing non-numeric data
                tmp = apply(data, MARGIN = c(1,2), 'as.logical')
                tmp = apply(tmp, MARGIN = c(1,2), 'as.integer')
                tmp = as.data.frame(apply(tmp, 2, prod))
               
                ix = which(tmp[,1] ==1)
                rownames(tmp)[ix]
                data  <- data[ rownames(tmp)[ix] ]
                if (ncol(data) < 2) next
                
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
                    
                    # adjust target columns (add new or remove columns)
                    cols = c('F', 'S', 'Z', 'R', 'X') 
                    lc = length(cols)
                    if (rea_st$max_col > lc) { cols = c(cols, paste0("D", seq(lc+1, rea_st$max_col)))}
                    if (rea_st$max_col < lc) { cols = head(cols, rea_st$max_col) }    
                        
                    colnames(data) = cols
                    
                    data['Smin'] = data$S==min(data$S)
                    
                    data['id'] = id
                    
                    out = rbind(out, data)
                }    
            }
            
        })
        # transformed resistance and reactance
        if (('R' %in% colnames(out)) & ('X' %in% colnames(out))) {
            r = out$R
            x = out$X
            out['RT'] <- (1 - r^2 - x^2) / ( (1 - r)^2 + x^2)
            out['XT'] <- 2*x  / ( (1 - r)^2 + x^2)
            cols = c('RT','XT')
        }  

        c = info$w_fname[1]
        # parse coordinates: row and col from file name
        pattern <- "[-]([0-9]{1,2})[-]([0-9]{1,2})[-]"
        tmp = c()
        for (c in unique(info$w_fname) ) {
            s = unlist(strsplit(c, "[(]"))[1]  # ignore all after bracket "("
            s = gsub(" ", "-", s, fixed = TRUE)
            s = gsub(".", "-", s, fixed = TRUE)
            
            b = str_match(s, pattern)
            r = c(c, b[2], b[3])
            
            tmp = rbind(tmp, r)
        }
        tmp = as.data.frame(tmp)
        colnames(tmp) = c('w_fname', 'col', 'row')
        tmp['col'] = str_pad(tmp$col, 2, pad = "0") # add leading zeros
        tmp['row'] = str_pad(tmp$row, 2, pad = "0")
        
        info <- merge(x = info, y = tmp, by = 'w_fname', all.x = TRUE)
        info = info[ c("id",  "path", "nrows", "ncols", levs, "w_fname", "col", "row" )]
        
        # data for heatmaps
        df_ht = info[ c("id",  levs,  "w_fname", "col", "row" )]
        df_ht = df_ht[which(!is.na(df_ht$col)), ]
        df_ht <- merge(x = df_ht, y = out[out$Smin, ], by = 'id', all.x = TRUE)
        
        return ( list('out' = out, 'info' = info, 'df_ht' = df_ht) )
    }
    
    

    SaveData = function(w_folder) {
        fdir = gsub(paste0('bin/tmp/tmp/', arcname), '', w_folder)
        
        outfilename = paste0(fdir, 'data_out.csv')

        s = 'write.table(data_0, outfilename, sep = ",", col.names = T, row.names = F, append = F)'
        s = myTryCatch ( eval(parse(text=s)) )        
        if (!is.null(s$error)) infomessage('Error', paste('Data is not saved. ','\nCheck the file', outfilename))      
        
        # write.table(data_0, outfilename, sep = ",",
        #             col.names = !file.exists(outfilename), row.names = F, append = F)
        
        outfilename = paste0(fdir, 'info_out.xlsx')
        s = 'write_xlsx(list(Info = info_0, HM_data = df_ht_0), outfilename)'
        s = myTryCatch ( eval(parse(text=s)) )            
        if (!is.null(s$error)) infomessage('Error', paste('Data is not saved. ','\nCheck the file', outfilename))      

    }

    
    ReadData = function(w_folder) {
        fdir = gsub(paste0('bin/tmp/tmp/', arcname), '', w_folder)
        
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
        if (is.null(res)) {return ()}
        
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
        
        lev2_list <- character(0)
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
        
        lev2_list <- character(0)
        if ('lev_2' %in% colnames(df_ht_0)) {
            lev2_list <- sort(as.vector(unique(df_ht_0$lev_2)))
        }
        updatePickerInput(session, "Lev2_input",
                          choices = lev2_list, selected = lev2_list  )     
    })    
    
    
    observeEvent(input$Lev1_input, {
        if (exists('df_ht_0')) {
 
            tmp = df_ht_0 %>% filter(lev_1 %in% input$Lev1_input)
            
            x = character(0)
            if ('lev_2' %in% colnames(tmp)) {
                x <- sort(as.vector(unique(tmp$lev_2)))
            } 
            rea$df_ht = tmp
            updatePickerInput(session, "Lev2_input", choices = x, selected = x  )
            
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
            
            anno_x <- paste0("c", tmp$col)
            anno_y <- paste0("r", tmp$row)
            anno_text <- paste0('[', tmp$col, ',', tmp$row, ']')
            
            
            v = "F"

            tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
            rownames(tmp) = paste0("r", tmp$row)
            colnames(tmp) = paste0("c", colnames(tmp))
            tmp = tmp[, 2:ncol(tmp)]


            p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp), 
                         type = "heatmap", colors = palette(50), opacity=0.6,
                         zauto = FALSE, zmin = rea_st$fmin, zmax = rea_st$fmax) %>%
                
            layout(title="<b>Frequency F</b>", title = list( font = list(size = 18)), xaxis = list( title = "Notch/Wafer location",
                                                      titlefont = list(size = 16)) )  %>%    
                
            add_annotations(x = anno_x, y = anno_y, text = anno_text,  xshift = 0,  yshift = 150/length(unique(anno_y)),
                            showarrow = FALSE, font=list(color='brown', size=8))  %>%
                
            config(displaylogo = FALSE) %>%
            config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))

            p       
        }
    })


    
    output$heatmap2 <- renderPlotly({

        if (is.null(rea$df_ht)) return()
        
        if (nrow(rea$df_ht) > 0) {

            tmp = rea$df_ht
            
            anno_x <- paste0("c", tmp$col)
            anno_y <- paste0("r", tmp$row)
            anno_text <- paste0('[', tmp$col, ',', tmp$row, ']')
            
            v = "Z"
            if (sum(!is.na(tmp[v])) >0) {
                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = T)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50),  opacity=0.6,
                             zauto = FALSE, zmin = rea_st$zmin, zmax = rea_st$zmax) %>%
                layout(title="<b>Impedance Z</b> ", title = list( font = list(size = 18)), xaxis = list( title = "Notch/Wafer location",
                                                          titlefont = list(size = 16)) )  %>%    
                    
                add_annotations(x = anno_x, y = anno_y, text = anno_text,  xshift = 0,  yshift = 150/length(unique(anno_y)),
                                    showarrow = FALSE, font=list(color='brown', size=8))  %>%

    
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
            
            anno_x <- paste0("c", tmp$col)
            anno_y <- paste0("r", tmp$row)
            anno_text <- paste0('[', tmp$col, ',', tmp$row, ']')
            
            v = "RT"
            
            if (sum(!is.na(tmp[v])) >0) {

                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50), opacity=0.6,
                             zauto = FALSE, zmin = rea_st$rmin, zmax = rea_st$rmax) %>%
    
                    layout(title="<b>Resistance R (transformed)</b>", title = list( font = list(size = 18)), xaxis = list( title = "Notch/Wafer location",
                                                               titlefont = list(size = 16)) )  %>%    
                    
                    add_annotations(x = anno_x, y = anno_y, text = anno_text,  xshift = 0,  yshift = 150/length(unique(anno_y)),
                                    showarrow = FALSE, font=list(color='brown', size=8))  %>%
                    

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
            
            anno_x <- paste0("c", tmp$col)
            anno_y <- paste0("r", tmp$row)
            anno_text <- paste0('[', tmp$col, ',', tmp$row, ']')
            
            v = "XT"
            
            if (sum(!is.na(tmp[v])) >0) {
                
                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
    
    
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50),  opacity=0.6,
                             zauto = FALSE, zmin = rea_st$xmin, zmax = rea_st$xmax) %>%
                    layout(title="<b>Reactance X (transformed)</b>", title = list( font = list(size = 18)), xaxis = list( title = "Notch/Wafer location",
                                                              titlefont = list(size = 16)) )  %>%    
                    
                    add_annotations(x = anno_x, y = anno_y, text = anno_text,  xshift = 0,  yshift = 150/length(unique(anno_y)),
                                    showarrow = FALSE, font=list(color='brown', size=8))  %>%
                    

                    config(displaylogo = FALSE) %>%
                    config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
                p
            }
        }
    })
    
    
    output$heatmap5 <- renderPlotly({
        
        if (is.null(rea$df_ht)) return()
        
        if (nrow(rea$df_ht) > 0) {
            
            tmp = rea$df_ht
            
            anno_x <- paste0("c", tmp$col)
            anno_y <- paste0("r", tmp$row)
            anno_text <- paste0('[', tmp$col, ',', tmp$row, ']')
            
            v = "S"
            
            if (sum(!is.na(tmp[v])) >0) {
                
                tmp = dcast(tmp, row ~ col, value.var = v, fun.aggregate = mean,  na.rm = F)
                rownames(tmp) = paste0("r", tmp$row)
                colnames(tmp) = paste0("c", colnames(tmp))
                tmp = tmp[, 2:ncol(tmp)]
                
                
                p <- plot_ly(z = data.matrix(tmp), x = colnames(tmp), y = rownames(tmp),
                             type = "heatmap", colors = palette(50),  opacity=0.6,
                             zauto = FALSE, zmin = rea_st$smin, zmax = rea_st$smax) %>%
                    layout(title="<b>Insertion Loss</b>", title = list( font = list(size = 18)), xaxis = list( title = "Notch/Wafer location",
                                                                                                            titlefont = list(size = 16)) )  %>%    
                    
                    add_annotations(x = anno_x, y = anno_y, text = anno_text,  xshift = 0,  yshift = 150/length(unique(anno_y)),
                                    showarrow = FALSE, font=list(color='brown', size=8))  %>%
                    
                    
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


shinyApp(ui = ui, server = server)




# get user name for  R & browser lists of multiple users
# Sys.getenv("USERNAME")

# check user access to data folder
# capture output cmd window in excel file
# check as.logical


