output$downloadReport <- downloadHandler(
    filename = 'cascade-report.pdf',

    content = function(file) {
        src <- normalizePath('server/report.Rmd')

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        # owd <- setwd(tempdir())
        # on.exit(setwd(owd))
        # file.copy(src, 'report.Rmd')

        out <- render('server/report.Rmd', pdf_document())
        file.rename(out, file)
    }
)
