

#library(webr)
library(stringi)
library(stringr)
library(ggthemes)
#library(plyr)
library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
#library(shinyAce)
#source("chooser.R")
library(shinyscreenshot)
library(scales)
#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)
library(ggplot2)

########################################
########UI (User Interface)#############
########################################

modul_literature_review_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
  
    
    
    
    tabsetPanel(
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "icon_idx.png", width = "30px"), 'Artikel Jurnal dengan Data dari Bursa Efek Indonesia (IDX)'),
               
               br(),
               
               
               
               
               h3("Penelusuran Artikel Jurnal yang Menggunakan Data di Bursa Efek Indonesia (IDX) atau Luar Indonesia",
                  style="color:red; text-align:center;font-size:30px"         ),
               
               
               
               br(),
               
               
               
               uiOutput(ns("buka_pemilihan_variabel_data_idx")),
               br(),
               
               DT::DTOutput(ns("data_idx")),
               
               
               
               
               br()
               
               
               
               
      ), #tabpanel artikel jurnal dengan data dari IDX
      
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "rupiah.png", width = "30px"), 'Daftar Biaya Publikasi di Jurnal Nasional & Internasional'),
               
               br(),
               
               
               
               
               h3("Daftar Biaya Publikasi di Jurnal Nasional & Internasional",
                  style="color:red; text-align:center;font-size:30px"         ),
               
               
               br(),
               
               
               uiOutput(ns("buka_pemilihan_daftar_biaya_publikasi")),
               
              # br(),
               
               
               
              # uiOutput(ns("buka_pemilihan_variabel_data_idx")),
               br(),
               
               DT::DTOutput(ns("data_biaya_publikasi_di_jurnal")),
               
               
               
               
               br()
               
               
               
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "icon_saham.png", width = "30px"), 'Data Saham di Bursa Efek Indonesia (IDX)'),
              
               
               
               h3("Data Saham: Harga, Volume, Frekuensi, Offer, Bid, Foreign Sell & Buy",
                  style="color:red; text-align:center;font-size:30px"         ),
               
               
               br(),
               
               
               
               
               tabsetPanel(
                 
                 
                 
                 
                 
                 
                 tabPanel(title = tags$h5( tags$img(src = "perusahaan.gif", width = "30px"), 'Pilih Perusahaan'),
                          
                          br(),
                          
                          
                          
                          
                          
                          
                          
                          #uiOutput(ns("radio_button_pilih_topik")),
                          
                          uiOutput(ns("checkbox_pilih_perusahaan")),
                          
                          # br(),
                          # br(),
                          
                          
                          
                          
                          br()
                          
                          
                 ),
               
               
                 tabPanel(title = tags$h5( tags$img(src = "data_saham.png", width = "30px"), 'Dataset'),
                          
                          br(),
                          
                          
                          
                          DT::DTOutput(ns("buka_data_idx_harga_saham")),
                          
                          
                          
                          
                          br()
                          
                          
                 ), #akhir tabpanel dataset
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel(title = tags$h5( tags$img(src = "visual.gif", width = "30px"), 'Visualisasi Data: Tipe 1'),
                          
                          
                          
                          
                          
                          tabsetPanel(
                            
                            
                            
                            
                            
                            
                            tabPanel(title = tags$h5( tags$img(src = "perusahaan2.gif", width = "30px"), 'Pilih Perusahaan'),
                                     
                                     
                                     
                                     
                                     
                                     h2("Pilih Perusahaan untuk Ditampilkan ke dalam Grafik", style="
    font-family: 'cursive';
    color: #0000CD;
    text-align:center
    "),
                                     
                                     
                                     
                                     br(),
                                     
                                     
                                     
                                     
                                     uiOutput(ns("checkbox_pilih_perusahaan_dari_tab_pilihperusahaan")),
                                     
                                     br(),
                                     
                                     
                                     br(),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     br()
                                     
                                     
                            ), #pilih tabpanel pilih perusahaan
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            tabPanel(title = tags$h5( tags$img(src = "icon_seting.gif", width = "30px"), 'Pengaturan Grafik'),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     h2("Pengaturan Grafik", style="
    font-family: 'cursive';
    color: #0000CD;
    text-align:center
    "),
                                     
                                     
                                     
                                     br(),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     fluidRow(
                                       column(3,
                                              
                                              
                                              
                                              
                                              
                                              #2025-11-03
                                              #13 Feb 2026

                                              
                                              dateRangeInput(ns("daterange3"), "Date range:",
                                                             start  = "2025-11-03",
                                                             end    = "2026-02-13",
                                                             min    = "2025-11-03",
                                                             max    = "2026-02-13",
                                                             format = "mm/dd/yy",
                                                             separator = " - "),
                                              
                                              
                                       
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              radioButtons(ns("bar_theme"), h4("Theme:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("gray" = "1", "BW"="2",
                                                             "dark"="3", "classic"="4", "linedraw"="5", "economist"="6", "theme_wsj" = "7", "theme_solarized" = "8",
                                                             "theme_gdocs" = "9", "theme_fivethirtyeight" = "10", "theme_calc" = "11"), inline=TRUE, selected = "4"   ),
                                              
                                              
                                              
                                              
                                              sliderInput( ns("number_column"),      h4("Number of Column (Facet):",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 1,20,2, step = 1 ),
                                              
                                              
                                              
                                              
                                              sliderInput( ns("size_point"),      h4("Point Size:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 1,20,1, step = 0.5 ),
                                              
                                              
                                              
                                              
                                              
                                              br()
                                              
                                       ),
                                       
                                       column(3,
                                              
                                              
                                              
                                              
                                              radioButtons(ns("display_mean_sd"), h4("Display Average & Standard Deviation:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("Average" = "1", "Average & Standard Deviation"="2",
                                                             "No"="4"), inline=TRUE, selected = "4"   ),
                                              
                                              
                                              
                                              
                                              sliderInput( ns("box_padding"),      h4("Box Padding:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), -10,10,0.5, step = 0.1 ),
                                              
                                              
                                              
                                              sliderInput( ns("min_segment_length"),      h4("Min Segment Length:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 0,10,0, step = 0.1 ),
                                              
                                              
                                              
                                              
                                              # min.segment.length
                                              
                                              
                                              #box.padding
                                              
                                              
                                              br()
                                              
                                       ),
                                       
                                       
                                       column(3,
                                              
                                              
                                              
                                              
                                              
                                              sliderInput( ns("line_size"),      h4("Line Size:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 1,20,1, step = 0.5 ),
                                              
                                              
                                              
                                              
                                              
                                              radioButtons(ns("remove_legend"), h4("Remove Legend:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("No" = "1", "Yes"="2"), inline=TRUE, selected = "1"   ),
                                              
                                              
                                              
                                              
                                              
                                              sliderInput( ns("text_size"),      h4("Text Size:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 0,20,3, step = 0.5 ),
                                              
                                              
                                              
                                              
                                              
                                              sliderInput( ns("text_size_title_legend"),      h4("Size: Title of Legend:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 0,20,14, step = 0.5 ),
                                              
                                              
                                              
                                              
                                              sliderInput( ns("text_size_text_legend"),      h4("Size: Text of Legend:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 0,20,14, step = 0.5 ),
                                              
                                              
                                              
                                              
                                              br()
                                              
                                       ),
                                       
                                       
                                       column(3,
                                              
                                              
                                              sliderInput( ns("get_decimals"),      h4("Decimals:",style="color:orange;text-shadow: -1px 0 black,
                                                                   0 1px black, 1px 0 black, 0 -1px black; text-align:left"), 0,10,3, step = 1 ),
                                              
                                              
                                              
                                              
                                              
                                              radioButtons(ns("position_vertical"), h4("Position:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("Vertical" = "1", "Horizontal"="2"), inline=TRUE, selected = "1"   ),
                                              
                                              
                                              
                                              
                                              
                                              
                                              radioButtons(ns("text_color_1"), h4("Text Color:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("black" = "black", "white"="white",
                                                             "green"="green", "blue"="blue", "red"="#8B0000"), inline=TRUE, selected = "black"   ),
                                              
                                              
                                              
                                              
                                              
                                              # h4("Title (X-Axis):",style="color:orange;text-shadow: -1px 0 black,
                                              #0 1px black, 1px 0 black, 0 -1px black; text-align:left"),
                                              #        shinyAce::aceEditor(ns("title_x_axis"), value="X-Axis", mode="r", theme="cobalt", height = 20),
                                              
                                              
                                              
                                              textAreaInput(ns("title_x_axis"), 
                                                            "Title (X-Axis):", value = "X Axis", height = 50, width = 460),
                                              
                                              
                                              
                                              
                                              
                                              # h4("Title (Y-Axis):",style="color:orange;text-shadow: -1px 0 black,
                                              #0 1px black, 1px 0 black, 0 -1px black; text-align:left"),
                                              #        shinyAce::aceEditor(ns("title_y_axis"), value="Y-Axis", mode="r", theme="cobalt", height = 20),
                                              
                                              textAreaInput(ns("title_y_axis"), 
                                                            "Title (Y-Axis):", value = "Y Axis", height = 50, width = 460),
                                              
                                              
                                              
                                              
                                              
                                              br()
                                              
                                       )
                                       
                                       
                                       
                                     ),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     radioButtons(ns("set_your_graph"), h4("Set Your Graph:",style="color:orange;text-shadow: -1px 0 black,
0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c("Type-1" = "1", "Type-2"="2", "Type-3"="3", "Type-4"="4", "Type-5"="5"), inline=TRUE, selected = "1"   ),
                                     
                                     
                                     
                                     br(),
                                     
                                     
                                     # plotOutput(ns("grafik_batang_rata_rata_1"), width = "100%"),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     br()
                                     
                                     
                                     
                            ), #pengaturan grafik
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            tabPanel(title = tags$h5( tags$img(src = "grafik1.gif", width = "30px"), 'Grafik'),
                                     
                                     
                                     
                                     
                                     br(),
                                     
                                     
                                     
                                     
                                     
                                     
                                     tabsetPanel(
                                       
                                       
                                       tabPanel("300 x 300",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_300_300_type1"), width = "300px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("500 x 300",
                                                
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_500_300_type1"), width = "500px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("700 x 300",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_700_300_type1"), width = "700px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("900 x 300",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_900_300_type1"), width = "900px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1100 x 300",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1100_300_type1"), width = "1100px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1200 x 300",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1200_300_type1"), width = "1200px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       tabPanel("1300 x 300",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1300_300_type1"), width = "1300px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1400 x 300",
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1400_300_type1"), width = "1400px", height = "300px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       tabPanel("300 x 500",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_300_500_type1"), width = "300px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("500 x 500",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_500_500_type1"), width = "500px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("700 x 500",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_700_500_type1"), width = "700px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("900 x 500",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_900_500_type1"), width = "900px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1100 x 500",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1100_500_type1"), width = "1100px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1200 x 500",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1200_500_type1"), width = "1200px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       tabPanel("1300 x 500",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1300_500_type1"), width = "1300px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1400 x 500",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1400_500_type1"), width = "1400px", height = "500px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       tabPanel("300 x 700",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_300_700_type1"), width = "300px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("500 x 700",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_500_700_type1"), width = "500px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("700 x 700",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_700_700_type1"), width = "700px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("900 x 700",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_900_700_type1"), width = "900px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1100 x 700",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1100_700_type1"), width = "1100px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1200 x 700",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1200_700_type1"), width = "1200px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       tabPanel("1300 x 700",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1300_700_type1"), width = "1300px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1400 x 700",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1400_700_type1"), width = "1400px", height = "700px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("300 x 900",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_300_900_type1"), width = "300px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("500 x 900",
                                                
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_500_900_type1"), width = "500px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("700 x 900",
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_700_900_type1"), width = "700px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       
                                       tabPanel("900 x 900",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_900_900_type1"), width = "900px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1100 x 900",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1100_900_type1"), width = "1100px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1200 x 900",
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1200_900_type1"), width = "1200px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       tabPanel("1300 x 900",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1300_900_type1"), width = "1300px", height = "900px" )),
                                                
                                                br()
                                                
                                       ),
                                       
                                       
                                       
                                       tabPanel("1400 x 900",
                                                
                                                
                                                
                                                shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_tipe1_1400_900_type1"), width = "1400px", height = "900px" )),
                                                
                                                br()
                                                
                                       )
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                     ), #akhir dari tabset panel
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     #shinycssloaders::withSpinner(plotOutput(ns("grafik_garis"), width = "1000px", height = "1000px" )),
                                     
                                     
                                     
                                     
                                     
                                     br(),
                                     
                                     DT::DTOutput(ns("data_tab_visualisasi")),
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     br()
                                     
                                     
                            ) #grafik
                            
                            
                            
                            
                            
                          ), #akhir tabsetpanel
                                     
                                     
                                     
                                     
                                     
                          
                          
                          
                          
                          
                          br()
                          
                          
                 ) #Akhir tabpanel dataset
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               ), #akhir tabsetpanel
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               br()
               
               
      ), #Akhir tabpanel pilih topik
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ), #Akhir tabsetpanel
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
      
    
    
    
    
    br(),
    

    
    
    
    
   # uiOutput(ns("buka_pemilihan_informasi_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM")),
    
    
    #uiOutput(ns("buka_pemilihan_informasi")),
    
  
    br(),
    
    
    #DT::DTOutput(ns("buka_data")),


               

    
    
    br(),
    
    
    #DT::DTOutput(ns("buka_data")),

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_literature_review_ui

#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui











































































########################################
################Server##################
########################################



modul_literature_review_server <- function(input, output, session) {
  
  
  
  nama_topik <- function()
  {
    
    
    nama_topik <- c("Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)",
                    
                    
              
 
                    "Jurnal Bidang Psikologi Terindeks SINTA (1 Jurnal)",
 
 
 "Jurnal Bidang Pendidikan Terindeks SINTA (2 Jurnal)"
 
 
 
 
 
                    
     
                    
                    
                    
                 
                    )
    
    
    
  }
  
  
  output$radio_button_pilih_topik <- renderUI({
    
    
    
    
    
    
    radioButtons(session$ns("terpilih_topik_paper"), 
                       label="Pilih Topik:", choices = c(nama_topik()), 
                       selected=c("Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)"), inline = FALSE)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  #########################
  
  
  nama_variabel_jurnal_nasional_akuntansi_sinta <- function()
  {
    
    dat <- read_xlsx("Daftar Jurnal Nasional Akuntansi.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  
  
  
  output$buka_pemilihan_informasi_jurnal_nasional_akuntansi_sinta <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_jurnal_nasional_akuntansi_sinta"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_jurnal_nasional_akuntansi_sinta()), 
                         selected=c("Nama Jurnal", "E-ISSN", "Sinta", "Sinta Berlaku Sampai", "Alamat Jurnal", "Frekuensi Publikasi dalam 1 Tahun", "Biaya"), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_artikel_jurnal_nasional_akuntansi_sinta <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)")
    {
      
      dat <- read_xlsx("Daftar Jurnal Nasional Akuntansi.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel_jurnal_nasional_akuntansi_sinta <- input$terpilih_variabel_jurnal_nasional_akuntansi_sinta
      
      dat_baru <- dat[c(terpilih_variabel_jurnal_nasional_akuntansi_sinta)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  #########################
  
  
  nama_variabel_jurnal_nasional_psikologi_sinta <- function()
  {
    
    dat <- read_xlsx("Daftar Jurnal Nasional Psikologi.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  
  
  
  output$buka_pemilihan_informasi_jurnal_nasional_psikologi_sinta <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Psikologi Terindeks SINTA (1 Jurnal)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_jurnal_nasional_psikologi_sinta"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_jurnal_nasional_psikologi_sinta()), 
                         selected=c("Nama Jurnal", "E-ISSN", "Sinta", "Sinta Berlaku Sampai", "Alamat Jurnal", "Frekuensi Publikasi dalam 1 Tahun", "Biaya"), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_artikel_jurnal_nasional_psikologi_sinta <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Psikologi Terindeks SINTA (1 Jurnal)")
    {
      
      dat <- read_xlsx("Daftar Jurnal Nasional Psikologi.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel_jurnal_nasional_psikologi_sinta <- input$terpilih_variabel_jurnal_nasional_psikologi_sinta
      
      dat_baru <- dat[c(terpilih_variabel_jurnal_nasional_psikologi_sinta)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  #########################
  
  
  nama_variabel_jurnal_nasional_pendidikan_sinta <- function()
  {
    
    dat <- read_xlsx("Daftar Jurnal Nasional Pendidikan.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  
  
  
  output$buka_pemilihan_informasi_jurnal_nasional_pendidikan_sinta <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Pendidikan Terindeks SINTA (2 Jurnal)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_jurnal_nasional_pendidikan_sinta"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_jurnal_nasional_pendidikan_sinta()), 
                         selected=c("Nama Jurnal", "E-ISSN", "Sinta", "Sinta Berlaku Sampai", "Alamat Jurnal", "Frekuensi Publikasi dalam 1 Tahun", "Biaya"), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_artikel_jurnal_nasional_pendidikan_sinta <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Jurnal Bidang Pendidikan Terindeks SINTA (2 Jurnal)")
    {
      
      dat <- read_xlsx("Daftar Jurnal Nasional Pendidikan.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel_jurnal_nasional_pendidikan_sinta <- input$terpilih_variabel_jurnal_nasional_pendidikan_sinta
      
      dat_baru <- dat[c(terpilih_variabel_jurnal_nasional_pendidikan_sinta)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################Project IDX Mulai
  
  
  nama_variabel_idx <- function()
  {
    dat <- read_xlsx("data_idx_gabung.xlsx")
    dat <- as.data.frame(dat)
    nama <- colnames(dat)
    
    return(nama)
    
  }
  
  
  
  
  #############
  #############
  
  
  
  
  output$buka_pemilihan_variabel_data_idx <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_variabel_idx"), 
                       label="Pilih Informasi:", choices = c(nama_variabel_idx()), 
                       selected=c("Title of Article", "Author(s)", "Year", "Journal"  , "Keywords", "Indexed by Sinta", 
                                   "Software", "About Data"), inline = TRUE)
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$data_idx <- DT::renderDT({
    
    

      
      dat <- read_xlsx("data_idx_gabung.xlsx")
      dat <- as.data.frame(dat)
      
      terpilih_nama_variabel_idx <- input$terpilih_nama_variabel_idx
      
      dat2 <- dat[c(terpilih_nama_variabel_idx)]
      
  print(dat2)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############Daftar Biaya Publikasi
  
  nama_variabel_biaya_publikasi <- function()
  {
    dat <- read_xlsx("daftar_biaya_publikasi.xlsx")
    dat <- as.data.frame(dat)
    nama <- colnames(dat)
    
    return(nama)
    
  }
  
  
  
  
  output$buka_pemilihan_daftar_biaya_publikasi <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_variabel_biaya_publikasi"), 
                       label="Pilih Informasi:", choices = c(nama_variabel_biaya_publikasi()), 
                       selected=c("Journal", "ISSN", "Indexed by Sinta", "Scopus", "Article Processing Charge (APC)"), inline = TRUE)
    
    
    
    
  })
  
  
  
  
  
  
  
  ##########
  
  
  
  
  
  output$data_biaya_publikasi_di_jurnal <- DT::renderDT({
    
    
    
    
    dat <- read_xlsx("daftar_biaya_publikasi.xlsx")
    dat <- as.data.frame(dat)
    
    terpilih_nama_variabel_biaya_publikasi <- input$terpilih_nama_variabel_biaya_publikasi
    
    dat2 <- dat[c(terpilih_nama_variabel_biaya_publikasi)]
    
    print(dat2)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################
  
  
  
  
  fungsi_nama_perusahaan_idx <- function()
  {
    
    data_harga_saham <- read_xlsx("data harga saham.xlsx")
    
    #dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    #dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    #dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    #dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    #dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    #dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    #dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    #dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    #dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    #dat_2026_feb <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
     #                     dat_20260212)
    
    
    
    nama_perusahaan <- data_harga_saham[,"Kode Saham"]
    nama_perusahaan <- unlist(nama_perusahaan)
    
    names(nama_perusahaan) = NULL
    
    nama_perusahaan <- unique(nama_perusahaan)
    
    return(nama_perusahaan)
    
    
  }
  
  
  ##############
  
  
  
  output$checkbox_pilih_perusahaan <- renderUI({
    
    
    
    
    
    
    #radioButtons(session$ns("terpilih_checkbox_pilih_perusahaan"), 
     #            label="Choose Companies:", choices = c(fungsi_nama_perusahaan_idx()), 
      #           selected=c("Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)"), inline = FALSE)
    
    
    
    checkboxGroupInput(session$ns("terpilih_checkbox_pilih_perusahaan"), 
                       label="Pilih Perusahaan:", choices = c( fungsi_nama_perusahaan_idx()), selected=c(), inline = TRUE   )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############
  
  
  
  
  
  output$testing <- DT::renderDT({
    
    
    
    
    #2026-Februari
    
    
   # a <- c(1,2,3)
   # b <- c(1,2,3)
    
   # dframe <- data.frame(a,b)
    
    
    
    #dat_20260202 <  read_xlsx("Ringkasan Saham 20260202.xlsx")
    #print(dframe)
    
    
    dat <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    dat <- as.data.frame(dat)
    
    print(dat)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$buka_data_idx_harga_saham <- DT::renderDT({
    
    
    data_harga_saham <- read_xlsx("data harga saham.xlsx")
    
    #2026-Februari
    
    
    #dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    #dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    #dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    #dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    #dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    #dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    #dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    #dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    #dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    #dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
     #                     dat_20260212)
    
    nama_perusahaan_lengkap <- data_harga_saham[,"Kode Saham"]
    nama_perusahaan_lengkap <- unlist(nama_perusahaan_lengkap)
    names(nama_perusahaan_lengkap) = NULL
    nama_perusahaan_lengkap <- as.character(nama_perusahaan_lengkap)
   # print(nama_perusahaan_lengkap)
    
    
    
    
    
    perusahaan_terpilih <- input$terpilih_checkbox_pilih_perusahaan
    perusahaan_terpilih <- unlist(perusahaan_terpilih)
    perusahaan_terpilih <- as.character(perusahaan_terpilih)
    
    #print(perusahaan_terpilih)
    
    
    indeks <-  nama_perusahaan_lengkap %in% perusahaan_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- data_harga_saham[c(indeks),]
    
    
    
    print(data_terpilih)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############untuk tabvisualisasi
  
  
  
  
  fungsi_terpilih_perusahaan_dari_tab_visualisasi <- function()
  {
    
    
    
    perusahaan_terpilih <- input$terpilih_checkbox_pilih_perusahaan
    perusahaan_terpilih <- unlist(perusahaan_terpilih)
    perusahaan_terpilih <- as.character(perusahaan_terpilih)
    
    return(perusahaan_terpilih)
    
  }
  
  
  
  output$checkbox_pilih_perusahaan_dari_tab_pilihperusahaan <- renderUI({
    
    
    
    
    
    
    #radioButtons(session$ns("terpilih_checkbox_pilih_perusahaan"), 
    #            label="Choose Companies:", choices = c(fungsi_nama_perusahaan_idx()), 
    #           selected=c("Jurnal Bidang Akuntansi Terindeks SINTA (2 Jurnal)"), inline = FALSE)
    
    
    
    checkboxGroupInput(session$ns("terpilih_checkbox_pilih_perusahaan_dari_tab_pilihperusahaan"), 
                       label="Pilih Perusahaan:", choices = c( fungsi_terpilih_perusahaan_dari_tab_visualisasi()), selected=c(), inline = TRUE   )
    
    
    
    
  })
  
  
  
  
  ##############
  
  
  
  
  
  
  
  output$data_tab_visualisasi <- DT::renderDT({
    
    perusahaan_terpilih_tab_visualisasi <- input$terpilih_checkbox_pilih_perusahaan_dari_tab_pilihperusahaan
    perusahaan_terpilih_tab_visualisasi <- unlist(perusahaan_terpilih_tab_visualisasi)
    perusahaan_terpilih_tab_visualisasi <- as.character(perusahaan_terpilih_tab_visualisasi)
    
    
    
    #############
    #############
    
    #2026-Februari
    
    data_harga_saham <- read_xlsx("data harga saham.xlsx")
    
    
    
   # dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    #dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    #dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    #dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    #dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    #dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    #dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    #dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    #dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    #dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
     #                             dat_20260212)
    
    nama_perusahaan_lengkap <- data_harga_saham[,"Kode Saham"]
    nama_perusahaan_lengkap <- unlist(nama_perusahaan_lengkap)
    names(nama_perusahaan_lengkap) = NULL
    nama_perusahaan_lengkap <- as.character(nama_perusahaan_lengkap)
    # print(nama_perusahaan_lengkap)
    
    
    
    
    
   # perusahaan_terpilih <- input$terpilih_checkbox_pilih_perusahaan
    
    perusahaan_terpilih <- perusahaan_terpilih_tab_visualisasi
    
    perusahaan_terpilih <- unlist(perusahaan_terpilih)
    perusahaan_terpilih <- as.character(perusahaan_terpilih)
    
    #print(perusahaan_terpilih)
    
    
    indeks <-  nama_perusahaan_lengkap %in% perusahaan_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- data_harga_saham[c(indeks),]
    
    print(data_terpilih)
    
    
  })
  
  
  
  
  
  ####################
  ####################
  
  
  fungsi_visualisasi_grafik_garis <- function()
  {
    
    
    perusahaan_terpilih_tab_visualisasi <- input$terpilih_checkbox_pilih_perusahaan_dari_tab_pilihperusahaan
    perusahaan_terpilih_tab_visualisasi <- unlist(perusahaan_terpilih_tab_visualisasi)
    perusahaan_terpilih_tab_visualisasi <- as.character(perusahaan_terpilih_tab_visualisasi)
    
    
    
    #############
    #############
    
    #2026-Februari
    
    data_harga_saham <- read_xlsx("data harga saham.xlsx")
    
    
    #dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    #dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    #dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    #dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    #dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    #dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    #dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    #dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    #dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    #dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
     #                             dat_20260212)
    
    nama_perusahaan_lengkap <- data_harga_saham[,"Kode Saham"]
    nama_perusahaan_lengkap <- unlist(nama_perusahaan_lengkap)
    names(nama_perusahaan_lengkap) = NULL
    nama_perusahaan_lengkap <- as.character(nama_perusahaan_lengkap)
    # print(nama_perusahaan_lengkap)
    
    
    
    

    
    #print(perusahaan_terpilih)
    
    
    indeks <-  nama_perusahaan_lengkap %in% perusahaan_terpilih_tab_visualisasi
    indeks <- which(indeks == TRUE)
    data_terpilih <- data_harga_saham[c(indeks),]
    
    #print(data_terpilih)
    
    
    dat <- data_terpilih
    dat <- as.data.frame(dat)
    
    
    
    
    
    #colnames(dat["Tanggal Perdagangan Terakhir"]) = c("Tanggal")
    
    
    colnames(dat)[c(7)] <- "Tanggal"
    
    #2 kode saham, 7 tanggal, 11 penutupan
    dat2 <- dat[c(2,7,11)]
    
    dat2 <- as.data.frame(dat2)
    
    
    #x1 = kode saham, x2 = tanggal, y = harga penutupan
    colnames(dat2) = c("x1", "x2", "y")
    
    
    dat2[,"x2"] <- str_replace_all(dat2[,"x2"], " ", "")
    
    
    dat2[,"x2"] <- as.Date(dat2[,"x2"], "%d%b%Y")
    
    
    
    
   # p <- ggplot(dat2, 
    #       aes(x = x2, y = y, group = 1)) +
     # geom_line()
    
 
    
    
    
    
    
    
    
    
    

    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    print(dat2)
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    print("Ugi")
    print("Ugi")
    print("Ugi")
    
    
    susun_data = reshape2::melt(data = dat2, id.vars = c("x1","x2")   )
    
    
    susun_data = as.data.frame(susun_data)
    
    
    print("prana ugiana gio")
    print("prana ugiana gio")
    print("prana ugiana gio")
    print("prana ugiana gio")
    
    print(susun_data)
    print(susun_data)
    
    
    DatesMerge <- input$daterange3
    print(DatesMerge)
    print(DatesMerge)
    
    print(DatesMerge[1])
    print(DatesMerge[2])
    
    #st <- as.Date("2025-12-25")
    #en <- as.Date("2026-01-05")
    
    st <- DatesMerge[1]
    st <- as.Date(st)
    en <- DatesMerge[2]
    en <- as.Date(en)
    
    buat_tanggal <- seq.Date(from = st, to = en, by = 1)
    
    print(buat_tanggal)
    print(buat_tanggal)
    
    
  #  p <-   ggplot(susun_data, 
   #               aes(x = x2, y = value))+
    #  geom_line( aes(group = x1, colour = x1), size = 1 ) +
     # geom_point(color = "black", size = 1) +
      #facet_wrap(~x1, scales = "free", ncol = 2) +
      #labs(color='')
    
    
    
    data_lengkap_tanggal <- susun_data[,2]  #2 itu kolom tanggal
    
    indeks_tanggal <-  data_lengkap_tanggal %in% buat_tanggal
    indeks_tanggal <- which(indeks_tanggal == TRUE)
    susun_data <- susun_data[c(indeks_tanggal),]
    
    
    
    
    p <-   ggplot(susun_data, 
                  aes(x = x2, y = value))+
      geom_line( aes(group = x1, colour = x1), size = input$line_size ) +
      geom_point(color = "black", size = input$size_point) +
      facet_wrap(~x1, scales = "free", ncol = input$number_column) +
      labs(color='')
    
    
    #gambar1 <-   ggplot(p, aes(x = y2, y = rata_rata))+
     # geom_line( aes(group = variable, colour = variable), size = input$line_size ) +
      #geom_point(color = "black", size = input$size_point) +
      #facet_wrap(~y1, scales = "free", ncol = input$number_column) +
      #labs(color='')
    
    
    
    
    
    
    
    
    ####################
    ####################
    
    
    
    
    
    #p = susun_data %>%
     # dplyr::group_by(y1, y2, variable) %>%
      #dplyr::summarise(rata_rata = mean(value),
       #                standar_deviasi = sd(value),
        #               minimum =  min(value),
         #              maximum =   max(value)   ) %>%
      #dplyr::arrange((y1))
    
    
    # colnames(p)[c(1,2,3, 4, 5)] = c(variabel_kategori_pertama, variabel_kategori_kedua, "Variable", "Mean", "Std. Deviation")
    
    
    #p[,4] = round(p[,4], digits = input$number_decimals)
    #p[,5] = round(p[,5], digits = input$number_decimals)
    #p[,6] = round(p[,6], digits = input$number_decimals)
    #p[,7] = round(p[,7], digits = input$number_decimals)
    
    
    
    
    #############
    #############
    
    
    
    
    
    title_x_axis <- input$title_x_axis
    title_x_axis <- unlist(title_x_axis)
    title_x_axis <- as.character(title_x_axis)
    
    
    title_y_axis <- input$title_y_axis
    title_y_axis <- unlist(title_y_axis)
    title_y_axis <- as.character(title_y_axis)
    
    
    
    
    
    
    gambar1 <-   p
    
    
    gambar2 <-   p
    
    
    
    gambar3 <-   p
    
    
    
    
    gambar4 <-   p
    
    
    
    
    
    gambar5 <-   p
    
    
    gambar = gambar1
    
    
    gambar = gambar + ylab(paste0(title_y_axis)) + xlab(paste0(title_x_axis)) +
      labs(colour="")
    
    
    
    set_your_graph = input$set_your_graph
    set_your_graph = as.numeric(set_your_graph)
    
    if(set_your_graph == 1)
    {
      gambar = gambar1
    }
    
    if(set_your_graph == 2)
    {
      gambar = gambar2
    }
    
    
    
    if(set_your_graph == 3)
    {
      gambar = gambar3
    }
    
    
    
    if(set_your_graph == 4)
    {
      gambar = gambar4
    }
    
    
    
    
    if(set_your_graph == 5)
    {
      gambar = gambar5
    }
    
    
    
    
    
    #########Theme
    
    tema <- input$bar_theme
    
    tema = as.numeric(tema)
    
    
    if(tema == 1)
    {
      gambar <- gambar + theme_gray()
    }
    if(tema == 2)
    {
      gambar <- gambar + theme_bw()
    }
    if(tema == 3)
    {
      gambar <- gambar + theme_dark()
    }
    if(tema == 4)
    {
      gambar <- gambar + theme_classic()
    }
    if(tema == 5)
    {
      gambar <- gambar + theme_linedraw()
    }
    if(tema == 6)
    {
      gambar <- gambar + theme_economist()
    }
    
    
    
    
    
    
    if(tema == 7)
    {
      gambar <- gambar + theme_wsj()
    }
    
    
    
    if(tema == 8)
    {
      gambar <- gambar + theme_solarized()
    }
    
    
    
    if(tema == 9)
    {
      gambar <- gambar + theme_gdocs()
    }
    
    
    
    if(tema == 10)
    {
      gambar <- gambar + theme_fivethirtyeight()
    }
    
    
    
    if(tema == 11)
    {
      gambar <- gambar + theme_calc()
    }
    
    
    
    
    #########remove legend
    
    
    remove_legend = input$remove_legend
    remove_legend = as.numeric(remove_legend)
    
    if(remove_legend == 2)
    {
      gambar = gambar +  theme(legend.position = "none") + labs(colour="")
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    gambar <- gambar + ylab(paste0(title_y_axis)) + xlab(paste0(title_x_axis))
    
    
    
    gambar <- gambar + 
      theme(axis.text.x = element_text( color = input$text_color_1, size = input$axis.text.x),
            axis.text.y = element_text( color = input$text_color_1, size = input$axis.text.y),
            axis.title = element_text( color=input$text_color_1 , size = input$axis.title),
            strip.text.x = element_text( color=input$text_color_1 , size = input$strip.text.x),
            legend.title=element_text(size = input$text_size_title_legend),
            legend.text = element_text(size = input$text_size_text_legend)
      ) 
    
    
    
    
    
    position_vertical = input$position_vertical
    
    position_vertical = as.numeric(position_vertical)
    
    
    if(position_vertical == 2)
    {
      
      gambar = gambar + coord_flip()
    }
    
    
    
    return(gambar)    
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################
  ################
  
  
  output$grafik_garis <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  ###################
  ###################
  
  
  
  
  
  
  ##########300 x 300
  
  output$grafik_garis_tipe1_300_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  ##########500 x 300
  
  output$grafik_garis_tipe1_500_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########700 x 300
  
  output$grafik_garis_tipe1_700_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########900 x 300
  
  output$grafik_garis_tipe1_900_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1100 x 300
  
  output$grafik_garis_tipe1_1100_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 300
  
  output$grafik_garis_tipe1_1200_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########1300 x 300
  
  output$grafik_garis_tipe1_1300_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########1400 x 300
  
  output$grafik_garis_tipe1_1400_300_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 500
  
  output$grafik_garis_tipe1_300_500_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  ##########500 x 500
  
  output$grafik_garis_tipe1_500_500_type1 <- renderPlot({
    
    fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 500
  
  output$grafik_garis_tipe1_700_500_type1 <- renderPlot({
    
    fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  ##########900 x 500
  
  output$grafik_garis_tipe1_900_500_type1 <- renderPlot({
    
    fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########1100 x 500
  
  output$grafik_garis_tipe1_1100_500_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  ##########1200 x 500
  
  output$grafik_garis_tipe1_1200_500_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  ##########1300 x 500
  
  output$grafik_garis_tipe1_1300_500_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########1400 x 500
  
  output$grafik_garis_tipe1_1400_500_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 700
  
  output$grafik_garis_tipe1_300_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  ##########500 x 700
  
  output$grafik_garis_tipe1_500_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 700
  
  output$grafik_garis_tipe1_700_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########900 x 700
  
  output$grafik_garis_tipe1_900_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  ##########1100 x 700
  
  output$grafik_garis_tipe1_1100_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 700
  
  output$grafik_garis_tipe1_1200_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 700
  
  output$grafik_garis_tipe1_1300_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  ##########1400 x 700
  
  output$grafik_garis_tipe1_1400_700_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 900
  
  output$grafik_garis_tipe1_300_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  ##########500 x 900
  
  output$grafik_garis_tipe1_500_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 900
  
  output$grafik_garis_tipe1_700_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########900 x 900
  
  output$grafik_garis_tipe1_900_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 900
  
  output$grafik_garis_tipe1_1100_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 900
  
  output$grafik_garis_tipe1_1200_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 900
  
  output$grafik_garis_tipe1_1300_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 900
  
  output$grafik_garis_tipe1_1400_900_type1 <- renderPlot({
    
    p <- fungsi_visualisasi_grafik_garis()
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_literature_review_server

#akhir dari modul_literature_review_server
#akhir dari modul_literature_review_server
#akhir dari modul_literature_review_server

















































































ui <- fluidPage(
  
  
  includeHTML("intro_home.html"),
  
  
  uiOutput("modul_literature_review"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_literature_review <- renderUI({
    
    
    
    #source("module//modul_literature_review.R")
    callModule(module = modul_literature_review_server, id = "modul_literature_review")
    modul_literature_review_ui(id = "modul_literature_review")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














