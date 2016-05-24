# Intervention Control
intSwitch <- reactiveValues(
    testing =      TRUE,
    linkage =      TRUE,
    preRetention = TRUE,
    initiation =   TRUE,
    adherence =    TRUE,
    retention =    TRUE
    )

observeEvent(input$intCheck_testing, {
    if (intSwitch$testing == TRUE) {
        intSwitch$testing <- FALSE
        updateButton(session,
            inputId = "intCheck_testing",
            label = "Testing",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$testing <- TRUE
        updateButton(session,
            inputId = "intCheck_testing",
            label = "Testing",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

observeEvent(input$intCheck_linkage, {
    if (intSwitch$linkage == TRUE) {
        intSwitch$linkage <- FALSE
        updateButton(session,
            inputId = "intCheck_linkage",
            label = "Linkage",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$linkage <- TRUE
        updateButton(session,
            inputId = "intCheck_linkage",
            label = "Linkage",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

observeEvent(input$intCheck_preRetention, {
    if (intSwitch$preRetention == TRUE) {
        intSwitch$preRetention <- FALSE
        updateButton(session,
            inputId = "intCheck_preRetention",
            label = "Pre-ART Retention",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$preRetention <- TRUE
        updateButton(session,
            inputId = "intCheck_preRetention",
            label = "Pre-ART Retention",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

observeEvent(input$intCheck_initiation, {
    if (intSwitch$initiation == TRUE) {
        intSwitch$initiation <- FALSE
        updateButton(session,
            inputId = "intCheck_initiation",
            label = "ART Initiation",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$initiation <- TRUE
        updateButton(session,
            inputId = "intCheck_initiation",
            label = "ART Initiation",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

observeEvent(input$intCheck_adherence, {
    if (intSwitch$adherence == TRUE) {
        intSwitch$adherence <- FALSE
        updateButton(session,
            inputId = "intCheck_adherence",
            label = "Adherence",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$adherence <- TRUE
        updateButton(session,
            inputId = "intCheck_adherence",
            label = "Adherence",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})

observeEvent(input$intCheck_retention, {
    if (intSwitch$retention == TRUE) {
        intSwitch$retention <- FALSE
        updateButton(session,
            inputId = "intCheck_retention",
            label = "ART Retention",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        intSwitch$retention <- TRUE
        updateButton(session,
            inputId = "intCheck_retention",
            label = "ART Retention",
            style = "success",
            size = "default",
            block = TRUE,
            icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    }
})
