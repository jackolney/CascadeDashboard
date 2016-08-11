output$downloadReport <- downloadHandler(
    filename = 'cascade-report.pdf',

    content = function(file) {
        # Identify absolute path of report.Rmd and any relevant images
        src <- normalizePath('server/report.Rmd')
        srcImg <- normalizePath('www/ModelSimple.png')
        # store original working directory in owd, but setwd() to tempdir()
        owd <- setwd(tempdir())
        # at end of function, setwd to original working directory
        on.exit(setwd(owd))
        # copy original report.Rmd to current working directory (temp)
        file.copy(src, 'report.Rmd')
        # copy any images that are used in the report to a the tempdir()
        file.copy(srcImg, 'ModelSimple.png')
        # call Rmarkdown::render
        out <- render('report.Rmd', pdf_document())
        # rename 'out' as 'file'
        file.rename(out, file)
    }
)
