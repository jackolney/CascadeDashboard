ggColorHue <- function(n) {
    hues = seq(15, 375, length = n+1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

DownloadButton <- function(outputId, label = "Download", class = NULL) {
    aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link",
        class), href = "", target = "_blank", icon("file-pdf-o"),
        label)
}
