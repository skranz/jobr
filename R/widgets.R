example.widgets = function() {
  app=eventsApp()
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))

  app$ui = fluidPage(
#    tags$head(
      tags$script(src="jobr/helplabel.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "jobr/helplabel.css"),
#    ),
    labelWithHelp("myinput","Eingabe",helpText("Dies ist mein Hilfetext."))
  )
  viewApp(app)
}


selectInput2 = function (inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, help=NULL,label.extras=NULL)
{
    selected <- restoreInput(id = inputId, default = selected)
    choices <- shiny:::choicesWithNames(choices)
    if (is.null(selected)) {
        if (!multiple)
            selected <- firstChoice(choices)
    }
    else selected <- as.character(selected)
    if (!is.null(size) && selectize) {
        stop("'size' argument is incompatible with 'selectize=TRUE'.")
    }
    selectTag <- tags$select(id = inputId, class = if (!selectize)
        "form-control", size = size, shiny:::selectOptions(choices,
        selected))
    if (multiple)
        selectTag$attribs$multiple <- "multiple"
    labelWidget = labelWithHelp(inputId, label, help, label.extras)


    res <- div(class = "form-group shiny-input-container",
        style = if (!is.null(width))
            paste0("width: ", validateCssUnit(width), ";"),
        labelWidget, div(selectTag))
    if (!selectize)
        return(res)
    shiny:::selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in%
        choices))
}
textInput2 = function(inputId, label, value = "", width = NULL, placeholder = NULL, help=NULL,label.extras=NULL){
    value <- restoreInput(id = inputId, default = value)
    labelWidget = labelWithHelp(inputId, label, help, label.extras)

    div(class = "form-group shiny-input-container", style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"),
        labelWidget, tags$input(id = inputId,
            type = "text", class = "form-control",
            value = value, placeholder = placeholder))
}

textAreaInput2 = function (inputId, label, value = "", help=NULL, width = "100%", height = NULL, cols = NULL, rows = NULL, placeholder = NULL, resize = NULL, div.style="", area.style = "", label.extras=NULL)
{
    value <- restoreInput(id = inputId, default = value)
    if (!is.null(width)) div.style = paste0("width: ", width,";", div.style)
    if (!is.null(height)) area.style = paste0("height: ", height,";", area.style)

    labelWidget = labelWithHelp(inputId, label, help, label.extras)

    div(class = "form-group shiny-input-container",style=div.style,labelWidget, tags$textarea(id = inputId, class = "form-control",
        placeholder = placeholder, style = area.style, rows = rows,
        cols = cols, value))
}

labelWithHelp = function(inputId, label, help=NULL, extras=NULL) {
  if (is.null(help) & is.null(extras))
    return(shiny:::shinyInputLabel(inputId,label))

  if (is.character(help) & !isTRUE(attr(help,"html"))) help = div(class="help-block",help)

  tagList(div(class = "input-group-sm label-with-help",
      shiny:::shinyInputLabel(inputId,label),
      if (!is.null(help)) icon("info-circle", class = "label-help-icon blue-hover", lib = "font-awesome"),
      extras,
      if (!is.null(help)) div(id=paste0(inputId, "-helpdiv"),class="help-below-label", style="display: none", help)
    ),
  )
}

myCollapsePanel = function (title, ..., value = title, panel.style = "default", header.style="", body.style="") {
    content <- list(...)
    id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1,
        1, 1e+06))))
    if (is.null(value)) {
        value = title
    }
    bsTag <- shiny::tags$div(class = paste0("panel panel-", panel.style),
        value = value, shiny::tags$div(style=header.style,class = "panel-heading",
            role = "tab", id = paste0("heading_", id), shiny::tags$h4(class = "panel-title",
                shiny::tags$a(`data-toggle` = "collapse", href = paste0("#",
                  id), title))), shiny::tags$div(id = id, class = "panel-collapse collapse",
            role = "tabpanel", shiny::tags$div(style=body.style,class = "panel-body",
                content)))
    #htmltools::attachDependencies(bsTag, shinyBSDep)
}

dateInput2 = function (inputId, label, value = NULL, min = NULL, max = NULL,
    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
    language = "en", width = NULL, autoclose = TRUE, datesdisabled = NULL,
    daysofweekdisabled = NULL, help=NULL, label.extras = NULL) {
  value <- shiny:::dateYMD(value, "value")
  min <- shiny:::dateYMD(min, "min")
  max <- shiny:::dateYMD(max, "max")
  datesdisabled <- shiny:::dateYMD(datesdisabled, "datesdisabled")
  value <- restoreInput(id = inputId, default = value)

  labelWidget = labelWithHelp(inputId, label, help, label.extras)


  tags$div(id = inputId, class = "shiny-date-input form-group shiny-input-container",
      style = if (!is.null(width))
          paste0("width: ", validateCssUnit(width), ";"),
      labelWidget, tags$input(type = "text",
          class = "form-control", `data-date-language` = language,
          `data-date-week-start` = weekstart, `data-date-format` = format,
          `data-date-start-view` = startview, `data-min-date` = min,
          `data-max-date` = max, `data-initial-date` = value,
          `data-date-autoclose` = if (autoclose)
              "true"
          else "false", `data-date-dates-disabled` = jsonlite::toJSON(datesdisabled,
              null = "null"), `data-date-days-of-week-disabled` = jsonlite::toJSON(daysofweekdisabled,
              null = "null")), shiny:::datePickerDependency)
}


simpleButtonVector = actionButtonVector = function(id, label="",icon=NULL, size=c("default","sm","xs")[1], class=paste0("btn btn-default action-button", if (size != "default") paste0(" btn-",size)), extra.class = "", extra.head="") {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="',class, ' ',extra.class,'" ',extra.head,'>',icon,label,'</button>')
}
