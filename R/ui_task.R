example_ui_task = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()
  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))
  taskid = "SJLg9EZfCZsRaJSZo"

  task = load.task.with.job(taskid = taskid)
  task = fill.task.placeholder(task)

  job = task$job
  app$task = task
  inner.ui = lang_fun("task_ui",task=task)
  app$ui = fluidPage(
    inner.ui
  )
  init.task.handlers()
  viewApp()

}


set_task_ui = function(id="innerUI", task = app$task, app=getApp()) {
  ui = lang_fun("task_ui",task=task)
  init.task.handlers()
  setUI(id, ui)
}


init.task.handlers = function() {
  buttonHandler("saveTaskBtn", save.task.click)
}

save.task.click = function(formValues,..., app=getApp()) {
  restore.point("save.task.click")
  task = app$task; job = task$job
  num.inputs = length(job$input_fields)

  if (num.inputs > 0) {
    task$values = formValues[paste0("input_",1:num.inputs)]
    names(task$values) = NULL
  }
  taskdone = formValues[["taskdone"]]

  task$taskstate = ifelse(taskdone,"d","o")
  task$values_txt = paste0(task$values, collapse=field.sep())

  update.task(task)

  timedMessage("taskMsgUI",html = colored.html("Vielen Dank, ihre Rückmeldung wurde gespeichert."))
}


task_ui_de = function(task=app$task, app=getApp()) {
  body = gsub("\n","<br>", task$body, fixed=TRUE)
  num.inputs = length(task$job$input_fields)
  taskdone = task$taskstate=="d"
  tagList(
    HTML("<h4>Rückmeldung von <b>", task$receiver,"</b> zum Auftrag </h4>"),
    myCollapsePanel(task$title,HTML(body)),
    task_inputs_ui(task),
    h4("Alles erledigt?"),
    checkboxInput("taskdone",label = "Bitte markieren sie die Box, wenn sie alles erledigt haben und speichern Sie danach.",value = taskdone, width = "100%"),
    uiOutput("taskMsgUI"),
    simpleButton("saveTaskBtn", "Speichern", form.ids = c("taskdone", if (num.inputs > 0) paste0("input_", 1:num.inputs))),
  )
}

task_inputs_ui = function(task=app$task,job=task$job, app=getApp()) {
  restore.point("task_inputs_ui")
  if (length(job$input_fields)==0) return(NULL)

  uis = input_fields_ui(job$input_fields, task$values)
  div(
    h4("Eingaben für Rückmeldung:"),
    uis
  )
}
