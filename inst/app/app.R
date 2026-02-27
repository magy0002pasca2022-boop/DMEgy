library(shiny)
library(shinythemes)
library(dplyr)
library(openxlsx)

# ==========================================
# 1) PARAMETER 2PL TETAP (dari kamu)
# ==========================================
item_par <- data.frame(
  a = c(
    2.8988935, 1.3363979, 0.8181315, 1.1318613, 1.0297154,
    1.3575799, 0.7130952, 1.0140341, 0.8626368, 0.7715575,
    0.8394456, 1.3911706, 1.4892590, 1.3787792, 1.3451154,
    1.3751151, 1.2963058, 1.0190617, 0.7176878, 0.8798495,
    1.1116752, 1.0704317, 1.3108939
  ),
  b = c(
    -0.2271802, -0.5101542, -0.9389003, -0.8774746, -0.2548735,
    -0.7450610, -0.6898078, -0.6787294, -0.1974448, -0.7988038,
    -0.7664025, -0.3598970, -0.6957651, -0.6205043, -0.2672510,
    -0.1240160, -0.7935651, -0.7981230, -0.1111726, -0.5177161,
    -1.0951214, -0.4188787,  0.3482089
  ),
  g = 0,
  u = 1,
  row.names = c(
    "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12",
    "B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23"
  )
)

# ==========================================
# 2) Helper: deteksi separator CSV
# ==========================================
detect_sep <- function(path) {
  x <- readLines(path, n = 5, warn = FALSE)
  if (length(x) == 0) stop("File kosong atau tidak bisa dibaca.")
  candidates <- c(";"=";", ","=",", "\t"="\t")
  counts <- sapply(candidates, function(sep) sum(lengths(strsplit(x, sep, fixed=TRUE)) - 1))
  candidates[[names(which.max(counts))]]
}

# ==========================================
# 3) Helper: baca respons 0/1 + ambil kolom item saja
#    (boleh ada kolom Nama/ID, akan diabaikan jika non-numeric)
# ==========================================
read_response_data <- function(path) {
  sep <- detect_sep(path)
  df <- read.csv(path, sep = sep, header = TRUE, check.names = FALSE)
  
  df2 <- df
  df2[] <- lapply(df2, function(x) suppressWarnings(as.numeric(as.character(x))))
  
  keep <- sapply(df2, function(x) !all(is.na(x)))
  df2 <- df2[, keep, drop = FALSE]
  
  if (ncol(df2) < 1) stop("Tidak ada kolom item yang terbaca.")
  
  # cek 0/1/NA
  uniq_vals <- sort(unique(as.vector(as.matrix(df2))))
  uniq_vals <- uniq_vals[!is.na(uniq_vals)]
  if (!all(uniq_vals %in% c(0, 1))) stop("Data harus dikotomus 0/1 (selain NA).")
  
  df2
}

# ==========================================
# 4) Scoring theta EAP (2PL) tanpa fit model
# ==========================================
logistic <- function(z) 1 / (1 + exp(-z))

theta_eap_2pl <- function(resp_mat, a, b, theta_grid = seq(-4, 4, length.out = 81)) {
  # prior N(0,1)
  log_prior <- dnorm(theta_grid, mean = 0, sd = 1, log = TRUE)
  
  n <- nrow(resp_mat)
  k <- ncol(resp_mat)
  
  out <- numeric(n)
  
  for (i in seq_len(n)) {
    u <- resp_mat[i, ]
    obs <- !is.na(u)
    
    if (!any(obs)) {
      out[i] <- NA_real_
      next
    }
    
    ai <- a[obs]
    bi <- b[obs]
    ui <- u[obs]
    
    # log-likelihood per theta (stabil)
    # P = logistic(a*(theta - b))
    # log L = sum( u*log(P) + (1-u)*log(1-P) )
    ll <- sapply(theta_grid, function(th) {
      p <- logistic(ai * (th - bi))
      sum(ui * log(p) + (1 - ui) * log(1 - p))
    })
    
    log_post <- log_prior + ll
    log_post <- log_post - max(log_post) # stabilisasi
    w <- exp(log_post)
    
    out[i] <- sum(theta_grid * w) / sum(w) # EAP
  }
  
  out
}

# ==========================================
# 5) UI
# ==========================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(div(style="text-align:center;",
                 h3("Pengukuran Daya Matematis"))),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(
        style="background:#f7f7f7; padding:12px; border-radius:10px; border:1px solid #e5e5e5;",
        fileInput("datafile", "Upload Data Respons Siswa (.csv)", accept = ".csv"),
        actionButton("run", "Hitung Theta", class="btn-primary"),
        tags$hr(),
        downloadButton("download_xlsx", "⬇️ Download Excel (Output)"),
        tags$hr(),
        verbatimTextOutput("status")
      ),
      width = 4
    ),
    
    mainPanel(
      h4("📊 Deskripsi Skor 0–100"),
      verbatimTextOutput("desc_out"),
      
      h4("📈 Ringkasan Kategori Penilaian"),
      tableOutput("summary_out"),
      
      h4("👤 Theta & Skor Siswa"),
      tableOutput("theta_out")
    )
  )
)

# ==========================================
# 6) Server
# ==========================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(theta_df=NULL, desc_df=NULL, sum_df=NULL, msg=NULL)
  
  observeEvent(input$run, {
    req(input$datafile)
    dat <- read_response_data(input$datafile$datapath)
    
    # Cocokkan kolom data dengan item_par berdasar nama item
    items_needed <- rownames(item_par)
    items_in_data <- colnames(dat)
    
    if (!all(items_needed %in% items_in_data)) {
      missing_items <- setdiff(items_needed, items_in_data)
      stop(paste0(
        "Item berikut tidak ada di file guru: ",
        paste(missing_items, collapse = ", "),
        "\nPastikan nama kolom sama seperti: ",
        paste(items_needed, collapse = ", ")
      ))
    }
    
    dat <- dat[, items_needed, drop = FALSE]  # urutkan sesuai parameter
    
    theta <- theta_eap_2pl(
      resp_mat = as.matrix(dat),
      a = item_par[items_needed, "a"],
      b = item_par[items_needed, "b"]
    )
    
    theta_df <- data.frame(
      ID = seq_along(theta),
      Theta = round(theta, 3)
    )
    
    # Skor 0-100 (sesuai formula kamu)
    theta_df$Skor_0_100 <- round(theta_df$Theta * 12.5 + 50, 2)
    
    mu <- mean(theta_df$Skor_0_100, na.rm = TRUE)
    sigma <- sd(theta_df$Skor_0_100, na.rm = TRUE)
    
    kategori_skor <- function(x) {
      if (is.na(x)) return(NA_character_)
      if (x <= mu - 1.5 * sigma) "Sangat Rendah"
      else if (x <= mu - 0.5 * sigma) "Rendah"
      else if (x <= mu + 0.5 * sigma) "Sedang"
      else if (x <= mu + 1.5 * sigma) "Tinggi"
      else "Sangat Tinggi"
    }
    
    theta_df$Kategori_Penilaian <- vapply(theta_df$Skor_0_100, kategori_skor, character(1))
    
    desc_df <- data.frame(
      Statistik = c("Mean", "SD", "Var", "Min", "Max"),
      Nilai = round(c(
        mean(theta_df$Skor_0_100, na.rm = TRUE),
        sd(theta_df$Skor_0_100, na.rm = TRUE),
        var(theta_df$Skor_0_100, na.rm = TRUE),
        min(theta_df$Skor_0_100, na.rm = TRUE),
        max(theta_df$Skor_0_100, na.rm = TRUE)
      ), 2)
    )
    
    sum_df <- theta_df %>%
      filter(!is.na(Kategori_Penilaian)) %>%
      group_by(Kategori_Penilaian) %>%
      summarise(Jumlah_Responden = n(), .groups = "drop") %>%
      arrange(factor(Kategori_Penilaian,
                     levels = c("Sangat Rendah","Rendah","Sedang","Tinggi","Sangat Tinggi"))) %>%
      mutate(
        Persen = round((Jumlah_Responden / sum(Jumlah_Responden)) * 100, 1),
        Label = paste0(Persen, "%")
      )
    
    rv$theta_df <- theta_df
    rv$desc_df  <- desc_df
    rv$sum_df   <- sum_df
    rv$msg <- paste0("✅ Scoring selesai. Jumlah siswa: ", nrow(dat),
                     " | Jumlah item dipakai: ", ncol(dat),
                     " | Separator terdeteksi otomatis.")
  })
  
  
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
  
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("Output_Scoring_2PL_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$theta_df, rv$desc_df, rv$sum_df)
      wb <- createWorkbook()
      addWorksheet(wb, "Theta_Skor")
      addWorksheet(wb, "Deskripsi_Skor")
      addWorksheet(wb, "Ringkasan_Kategori")
      writeData(wb, "Theta_Skor", rv$theta_df)
      writeData(wb, "Deskripsi_Skor", rv$desc_df)
      writeData(wb, "Ringkasan_Kategori", rv$sum_df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
