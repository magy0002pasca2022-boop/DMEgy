#' Jalankan Aplikasi Pengolahan Data IRT 2PL (Dikotomus)
#'
#' Membuka aplikasi Shiny untuk mengolah data respons 0/1 dengan model IRT 2PL.
#' @export
run_Dayamatematis_app <- function() {
  appDir <- system.file("app", package = "DMEgy")
  if (appDir == "") stop("Folder app tidak ditemukan dalam package.")
  shiny::runApp(appDir, display.mode = "normal")
}
