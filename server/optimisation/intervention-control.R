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
    if (intSwitch$testing) {
        message("Testing Intervention - OFF")
        intSwitch$testing <- FALSE
        updateButton(session,
            inputId = "intCheck_testing",
            label = "Testing",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("Testing Intervention - ON")
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
    if (intSwitch$linkage) {
        message("Linkage Intervention - OFF")
        intSwitch$linkage <- FALSE
        updateButton(session,
            inputId = "intCheck_linkage",
            label = "Linkage",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("Linkage Intervention - ON")
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
    if (intSwitch$preRetention) {
        message("Pre-ART Retention Intervention - OFF")
        intSwitch$preRetention <- FALSE
        updateButton(session,
            inputId = "intCheck_preRetention",
            label = "Pre-ART Retention",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("Pre-ART Retention Intervention - ON")
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
    if (intSwitch$initiation) {
        message("ART Initiation Intervention - OFF")
        intSwitch$initiation <- FALSE
        updateButton(session,
            inputId = "intCheck_initiation",
            label = "ART Initiation",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("ART Initiation Intervention - ON")
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
    if (intSwitch$adherence) {
        message("Adherence Intervention - OFF")
        intSwitch$adherence <- FALSE
        updateButton(session,
            inputId = "intCheck_adherence",
            label = "Adherence",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("Adherence Intervention - ON")
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
    if (intSwitch$retention) {
        message("ART Retention Intervention - OFF")
        intSwitch$retention <- FALSE
        updateButton(session,
            inputId = "intCheck_retention",
            label = "ART Retention",
            style = "danger",
            size = "default",
            block = TRUE,
            icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        message("ART Retention Intervention - ON")
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
