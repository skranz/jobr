example_tasklist_ui = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()
  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  app$tasks = tasks = load.tasks()

  app$nav = NULL

  app$ui = fluidPage(
    jobr.header(),
    uiOutput("mainUI")
  )
  set_tasklist_ui()
  viewApp()

}

set_tasklist_ui = function(id="mainUI", app=getApp()) {
  ui = lang_fun("tasklist_ui")
  init.tasklist.handlers()
  setUI(id, ui)
}

init.tasklist.handlers = function() {
  setDownloadHandler("tasklistExcelBtn",
    filename=function(app = getApp()) paste0("tasks.xlsx"),
    content = function(file, ...) {
      restore.point("downloadContent")
      app=getApp()
      withProgress(message="Die Exceltabelle wird erstellt...",
        make.tasklist.xlsx(file)
      )
    }
  )
  classEventHandler("showTaskBtn",event = "click",function(data=NULL, app=getApp(), ...) {
    restore.point("showTaskBtn")
    taskid = data$taskid

    app$task = task = load.task.with.job(taskid)
    app$main_tab = "tasklist"
    app$nav = c(app$nav, "main")
    set_task_ui()
  })


}

make.tasklist.xlsx = function(file, tasks=app$tasks, app=getApp()) {
  restore.point("make.tasklist.xlsx")
  df = tasks
  write.xlsx(df, file = file, colNames = TRUE)
}

tasklist_ui_de = function(tasks = app$tasks, app=getApp()) {
  restore.point("tasklist_ui_de")
  num.open = sum(tasks$taskstate!="d")

  ui = tagList(
    h4(paste0("Erhaltene Aufträge (",num.open, " offen)")),
#    div(class="input-group",
#      downloadButton("tasklistExcelBtn", "Excel",icon = icon("download"))
#    ),
    hr(),
    tasklist.html.table()
  )
  ui
}

tasklist.html.table = function(tasks = app$tasks, app=getApp()) {
  restore.point("tasklist.html.table")

  taskstates = c("o"="Offen", "f"="Offen","d" ="Fertig")

  df = tasks %>%
    select(taskstate, createtime,deadline, tpl_title, sender) %>%
    mutate(taskstate=taskstates[taskstate])

  #val_tab = NULL
  show.btns = simpleButtonVector(paste0("showTaskBtn_",tasks$taskid),icon=icon(name = "eye"), size="xs",extra.class = "showTaskBtn",extra.head=paste0('data-taskid="',tasks$taskid,'"'))


  df = bind_cols(show.btns, df)
  df = arrange(df, desc(createtime),desc(deadline))

  header=c("","Status","Erhalten","Deadline","Titel", "Sender")

  MyDataTable(id="tasklistTable",df,header=header, searching=TRUE)
}
