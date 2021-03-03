textAreaInput2 = function (inputId, label, value = "", width = "100%", height = NULL,
    cols = NULL, rows = NULL, placeholder = NULL, resize = NULL, div.style="", area.style = "")
{
    value <- restoreInput(id = inputId, default = value)
    if (!is.null(width)) div.style = paste0("width: ", width,";", div.style)
    if (!is.null(height)) area.style = paste0("height: ", height,";", area.style)

    div(class = "form-group shiny-input-container",style=div.style, shiny:::shinyInputLabel(inputId,
        label), tags$textarea(id = inputId, class = "form-control",
        placeholder = placeholder, style = area.style, rows = rows,
        cols = cols, value))
}
