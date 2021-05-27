example_showjob_ui = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()
  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))
  jobid = "THXthZ0Vn3hYo52a"

  app$job = job = load.job.with.tasks(jobid)
  app$nav = NULL

  app$ui = fluidPage(
    jobr.header(),
    uiOutput("navUI"),
    uiOutput("innerUI")
  )
  set_showjob_ui()
  viewApp()

}

set_showjob_ui = function(id="innerUI", job = app$job, app=getApp()) {
  ui = lang_fun("showjob_ui",job=job)
  init.show.job.handlers()
  setUI(id, ui)
}

init.show.job.handlers = function() {
  setDownloadHandler("jobExcelBtn",
    filename=function(app = getApp()) paste0("job.xlsx"),
    content = function(file, ...) {
      restore.point("downloadContent")
      app=getApp()
      withProgress(message="Die Exceltabelle wird erstellt...",
        make.job.xlsx(file)
      )
    }
  )
  classEventHandler("editTaskBtn",event = "click",function(data=NULL, app=getApp(), ...) {
    restore.point("editTaskBtn")
    taskid = data$taskid
    job = app$job
    row = which(job$tasks$taskid == taskid)
    task = as.list(app$job$tasks[row,])

    task$job = job
    task = init.task(task)
    app$task = task
    app$nav = c(app$nav, "job")
    set_task_ui()
    set_nav_ui()
  })


}

make.job.xlsx = function(file, job=app$job, app=getApp()) {
  restore.point("make.job.xlsx")
  df = job$tasks %>%
    select(taskstate, receiver) %>%
    mutate(taskstate=ifelse(taskstate=="d","erledigt","offen"))

  val_tab = job_tasks_values_df(job)
  val_tab = convert_task_values_df(val_tab, job)

  #val_tab = NULL
  df = bind_cols(df, val_tab)
  df = arrange(df, taskstate, receiver)
  header=c("Status", "Empfänger", colnames(val_tab))

  colnames(df) = header
  write.xlsx(df, file = file, colNames = TRUE)

}

showjob_ui_de = function(job, app=getApp()) {
  restore.point("job_ui_de")
  tasks = job$tasks
  num.done = sum(tasks$taskstate=="d")
  num.tasks = NROW(tasks)

  ui = tagList(
    h4(paste0("Job: ",job$tpl_title)),
    p(num.done, " von ", num.tasks, " Empfänger haben Job erledigt. Deadline: ", lang_fun("format_deadline", job$deadline)),
    div(class="input-group",
      selectInput2("jobStatus","Status", choices = c("Job abgeschlosssen"="d", "Job offen"="s"),selected = ifelse(job$jobstate=="d","d","s"),width = "15em")
    ),
    div(class="input-group",
      downloadButton("jobExcelBtn", "Excel",icon = icon("download")),
      simpleButton("jobWordBtn", "Word", icon = icon("download")),
      simpleButton("jobSendReminder", "Sende Erinnerung"),
      simpleButton("jobExtraReceiver", "Zusaetzliche Empfaenger")
    ),
    #h4("Empfänger:"),
    hr(),
    tasks.html.table()
  )
  ui
}

convert_task_values_df = function(df, job) {
  types = sapply(job$input_fields, function(field) field$type)
  cols = which(types=="checkbox")
  for (col in cols) {
    vals = as.character(df[[col]])
    df[[col]] = ifelse(vals=="TRUE","ja","nein")
  }
  df
}

tasks.html.table = function(tasks = job$tasks, job=app$job, app=getApp()) {
  restore.point("set.job.task.table")


  df = job$tasks %>%
    select(taskstate, receiver) %>%
    mutate(taskstate=ifelse(taskstate=="d","erledigt","offen"))

  val_tab = job_tasks_values_df(job)
  val_tab = convert_task_values_df(val_tab, job)

  #val_tab = NULL
  edit.btns = simpleButtonVector(paste0("editTaskBtn_",job$tasks$taskid),icon=icon(name = "pencil"), size="xs",extra.class = "editTaskBtn",extra.head=paste0('data-taskid="',job$tasks$taskid,'"'))


  df = bind_cols(edit.btns, df, val_tab)
  df = arrange(df, taskstate, receiver)

  header=c("", "Status", "Empfänger", colnames(val_tab))

  MyDataTable(id="jobTasksTable",df,header=header)

}
