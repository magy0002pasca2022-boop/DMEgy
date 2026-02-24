library(shiny)
library(shinythemes)
library(mirt)
library(dplyr)
library(openxlsx)
library(tools)

# =========================
# Helper: baca data
# =========================
read_response_data <- function(path, sep = ";") {
  df <- read.csv(path, sep = sep, check.names = FALSE)
  # Ambil hanya kolom numerik (jaga-jaga ada ID/Nama)
  df_num <- df[, sapply(df, is.numeric), drop = FALSE]
  
  if (ncol(df_num) < 2) {
    stop("Data tidak terbaca sebagai numerik. Pastikan item berupa 0/1 dan dipisah separator yang benar.")
  }
  
  # Pastikan dikotomus 0/1
  uniq_vals <- sort(unique(as.vector(as.matrix(df_num))))
  uniq_vals <- uniq_vals[!is.na(uniq_vals)]
  if (!all(uniq_vals %in% c(0, 1))) {
    stop("Data harus dikotomus 0/1. Ditemukan nilai selain 0/1.")
  }
  
  df_num
}

# =========================
# UI
# =========================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div(
      style = "text-align:center;",
      h3("Pengolahan Data Kemampuan dan Nilai Tes Pengukuran (IRT 2PL Dikotomus)")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(
        style = "background:#f7f7f7; padding:12px; border-radius:10px; border:1px solid #e5e5e5;",
        fileInput("datafile", "Upload Data Respons Siswa (.csv)", accept = ".csv"),
        selectInput("sep", "Pemisah (separator) CSV", choices = c("Titik koma ;"=";", "Koma ,"=",", "Tab \\t"="\t"), selected = ";"),
        checkboxInput("use_example", "Gunakan data contoh dari package (inst/extdata/Data_IRT DM.csv)", value = FALSE),
        actionButton("run", "Hitung (Model 2PL)", class = "btn-primary"),
        tags$hr(),
        downloadButton("download_xlsx", "⬇️ Download Excel (Semua Output)")
      ),
      width = 4
    ),
    
    mainPanel(
      h4("📊 Deskripsi Skor 0–100"),
      verbatimTextOutput("desc_out"),
      
      h4("📈 Ringkasan Kategori Penilaian"),
      tableOutput("summary_out"),
      
      h4("🧾 Parameter Item (2PL)"),
      tableOutput("itempar_out"),
      
      h4("👤 Hasil Theta & Skor Siswa"),
      tableOutput("theta_out")
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    theta_df = NULL,
    desc_df  = NULL,
    sum_df   = NULL,
    item_df  = NULL
  )
  
  observeEvent(input$run, {
    
    # ---- Ambil data
    data_path <- NULL
    
    if (isTRUE(input$use_example)) {
      # File contoh harus kamu taruh di: inst/extdata/Data_IRT DM.csv
      data_path <- system.file("extdata", "Data_IRT DM.csv", package = "DM-Egy")
      if (data_path == "") stop("File contoh tidak ditemukan. Pastikan Data_IRT DM.csv ada di inst/extdata/ dan package diinstall ulang.")
    } else {
      req(input$datafile)
      data_path <- input$datafile$datapath
    }
    
    df_num <- read_response_data(data_path, sep = input$sep)
    
    # ---- Fit model 2PL (unidimensi) untuk data dikotomus
    # itemtype = "2PL" -> estimasi a (discrimination) dan b (difficulty)
    mod <- mirt(df_num, 1, itemtype = "2PL", verbose = FALSE)
    
    # ---- Theta (EAP)
    th <- fscores(mod, method = "EAP")
    theta <- as.numeric(th[,1])
    
    theta_df <- data.frame(
      ID = seq_along(theta),
      Theta = round(theta, 3)
    )
    
    # ---- Konversi skor 0–100
    theta_df$Skor_0_100 <- round(theta_df$Theta * 12.5 + 50, 2)
    
    # ---- Kategori
    kategori_skor <- function(x) {
      if (x <= mu - 1.5 * sigma) {
        return("Sangat Rendah")
      } else if (x <= mu - 0.5 * sigma) {
        return("Rendah")
      } else if (x <= mu + 0.5 * sigma) {
        return("Sedang")
      } else if (x <= mu + 1.5 * sigma) {
        return("Tinggi")
      } else {
        return("Sangat Tinggi")
      }
    }
    
    # ---- Deskripsi skor
    desc_df <- data.frame(
      Statistik = c("Mean", "SD", "Var", "Min", "Max"),
      Nilai = round(c(
        mean(theta_df$Skor_0_100),
        sd(theta_df$Skor_0_100),
        var(theta_df$Skor_0_100),
        min(theta_df$Skor_0_100),
        max(theta_df$Skor_0_100)
      ), 2)
    )
    
    # ---- Ringkasan kategori
    sum_df <- theta_df %>%
      group_by(Kategori_Penilaian) %>%
      summarise(Jumlah_Responden = n(), .groups = "drop") %>%
      arrange(factor(Kategori_Penilaian,
                     levels = c("Sangat Rendah","Rendah","Sedang","Tinggi","Sangat Tinggi"))) %>%
      mutate(
        Persen = round((Jumlah_Responden / sum(Jumlah_Responden)) * 100, 1),
        Label = paste0(Persen, "%")
      )
    
    # ---- Parameter item 2PL: a dan b
    # coef(..., IRTpars=TRUE) akan mengeluarkan parameter a dan b
    cf <- coef(mod, IRTpars = TRUE, simplify = TRUE)
    # cf$items biasanya berisi a dan b
    item_mat <- cf$items
    item_df <- data.frame(
      Item = rownames(item_mat),
      a = round(item_mat[,"a"], 4),
      b = round(item_mat[,"b"], 4),
      row.names = NULL
    )
    
    # ---- Simpan ke reactiveValues
    rv$theta_df <- theta_df
    rv$desc_df  <- desc_df
    rv$sum_df   <- sum_df
    rv$item_df  <- item_df
  })
  
  # ---- Output UI
  output$theta_out <- renderTable({
    req(rv$theta_df)
    head(rv$theta_df, 20)
  })
  
  output$desc_out <- renderPrint({
    req(rv$desc_df)
    print(rv$desc_df, row.names = FALSE)
  })
  
  output$summary_out <- renderTable({
    req(rv$sum_df)
    rv$sum_df
  })
  
  output$itempar_out <- renderTable({
    req(rv$item_df)
    rv$item_df
  })
  
  # ---- Download Excel (1 file berisi banyak sheet)
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("Output_IRT_2PL_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rv$theta_df, rv$desc_df, rv$sum_df, rv$item_df)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Theta_Skor")
      addWorksheet(wb, "Deskripsi_Skor")
      addWorksheet(wb, "Ringkasan_Kategori")
      addWorksheet(wb, "Parameter_Item_2PL")
      
      writeData(wb, "Theta_Skor", rv$theta_df)
      writeData(wb, "Deskripsi_Skor", rv$desc_df)
      writeData(wb, "Ringkasan_Kategori", rv$sum_df)
      writeData(wb, "Parameter_Item_2PL", rv$item_df)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
