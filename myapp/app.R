

#library(webr)
library(stringi)
library(stringr)

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
      
      tabPanel(title = tags$h5( tags$img(src = "icon_select.gif", width = "30px"), 'Pilih Perusahaan'),
              
               
               # DT::DTOutput(ns("testing")),
               
               
               
               #uiOutput(ns("radio_button_pilih_topik")),
               
               uiOutput(ns("checkbox_pilih_perusahaan")),
               
              # br(),
              # br(),
               
          
               
               
               
               
               
               
               
               
               
               
               br()
               
               
      ), #Akhir tabpanel pilih topik
      
      
      tabPanel(title = tags$h5( tags$img(src = "icon_literatur.gif", width = "30px"), 'Dataset'),
               
               
               
               
               DT::DTOutput(ns("buka_data_idx_harga_saham")),
               
               
               
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_akuntansi_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_akuntansi_sinta")),
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_psikologi_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_psikologi_sinta")),
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_pendidikan_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_pendidikan_sinta")),
               
               
               br()
               
               
      ), #Akhir tabpanel dataset
    
      
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "visual.gif", width = "30px"), 'Visualisasi Data'),
               
               
               
               uiOutput(ns("checkbox_pilih_perusahaan_dari_tab_pilihperusahaan")),
               
               br(),
               
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis"), width = "1000px", height = "1000px" )),
               
               
               
               
               
               br(),
               
               DT::DTOutput(ns("data_tab_visualisasi")),
               
               
               
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_akuntansi_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_akuntansi_sinta")),
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_psikologi_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_psikologi_sinta")),
               
               
               #uiOutput(ns("buka_pemilihan_informasi_jurnal_nasional_pendidikan_sinta")),
               #DT::DTOutput(ns("buka_data_artikel_jurnal_nasional_pendidikan_sinta")),
               
               
               br()
               
               
      ) #Akhir tabpanel dataset
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
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
  
  
  
  
  ###########################
  
  
  
  
  fungsi_nama_perusahaan_idx <- function()
  {
    
    dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    dat_2026_feb <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
                          dat_20260212)
    
    
    
    nama_perusahaan <- dat_2026_feb[,"Kode Saham"]
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
    
    
    
    
    #2026-Februari
    
    
    dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
                          dat_20260212)
    
    nama_perusahaan_lengkap <- dat_2026_feb_lengkap[,"Kode Saham"]
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
    data_terpilih <- dat_2026_feb_lengkap[c(indeks),]
    
    
    
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
    
    
    dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
                                  dat_20260212)
    
    nama_perusahaan_lengkap <- dat_2026_feb_lengkap[,"Kode Saham"]
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
    data_terpilih <- dat_2026_feb_lengkap[c(indeks),]
    
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
    
    
    dat_20260202 <- read_xlsx("Ringkasan Saham 20260202.xlsx")
    dat_20260203 <- read_xlsx("Ringkasan Saham 20260203.xlsx")
    dat_20260204 <- read_xlsx("Ringkasan Saham 20260204.xlsx")
    dat_20260205 <- read_xlsx("Ringkasan Saham 20260205.xlsx")
    dat_20260206 <- read_xlsx("Ringkasan Saham 20260206.xlsx")
    dat_20260209 <- read_xlsx("Ringkasan Saham 20260209.xlsx")
    dat_20260210 <- read_xlsx("Ringkasan Saham 20260210.xlsx")
    dat_20260211 <- read_xlsx("Ringkasan Saham 20260211.xlsx")
    dat_20260212 <- read_xlsx("Ringkasan Saham 20260212.xlsx")
    
    
    
    
    
    dat_2026_feb_lengkap <- rbind(dat_20260202, dat_20260203, dat_20260204, dat_20260205, dat_20260206, dat_20260209, dat_20260210, dat_20260211, 
                                  dat_20260212)
    
    nama_perusahaan_lengkap <- dat_2026_feb_lengkap[,"Kode Saham"]
    nama_perusahaan_lengkap <- unlist(nama_perusahaan_lengkap)
    names(nama_perusahaan_lengkap) = NULL
    nama_perusahaan_lengkap <- as.character(nama_perusahaan_lengkap)
    # print(nama_perusahaan_lengkap)
    
    
    
    

    
    #print(perusahaan_terpilih)
    
    
    indeks <-  nama_perusahaan_lengkap %in% perusahaan_terpilih_tab_visualisasi
    indeks <- which(indeks == TRUE)
    data_terpilih <- dat_2026_feb_lengkap[c(indeks),]
    
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
    
    
    
    print(susun_data)
    
    
    
    p <-   ggplot(susun_data, 
                  aes(x = x2, y = value))+
      geom_line( aes(group = x1, colour = x1), size = 1 ) +
      geom_point(color = "black", size = 1) +
      facet_wrap(~x1, scales = "free", ncol = 2) +
      labs(color='')
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    return(p)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################
  ################
  
  
  output$grafik_garis <- renderPlot({
    
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














