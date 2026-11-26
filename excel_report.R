#' Write a named list of data frames to a formatted .xlsx workbook (needs the openxlsx package)
build_excel_report <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#66C2A5", border = "Bottom")
  for (nm in names(sheets)) {
    sheet <- substr(nm, 1, 31)
    df <- as.data.frame(sheets[[nm]], stringsAsFactors = FALSE)
    df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, df, headerStyle = header)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    if (ncol(df) > 0) openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
