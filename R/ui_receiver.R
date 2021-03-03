example.ui_reciever = function() {
  setwd("C:/libraries/jobr")
  restore.point.options(display.restore.point=TRUE)
  res = load.task.with.job("xTlsSKB5kSCkGIlbG")
  job = res$job; task = res$task
  db = get.jobdb()

  tasks = load.tasks(receivers="sebastian.kranz@uni-ulm.de", startdate=as.Date("2021-02-24"))

  app = eventsApp()


  #rui = receiver_task_ui(job,task)

  rui = tasks_ui(tasks)
  app$ui = fluidPage(
    rui
  )
  viewApp()

  proxy = "sebastian.kranz@uni-ulm.de"
  tasks = load.tasks.of.proxy(proxy)

}

task_list_ui = function(tasks, lang=get.lang()) {
  lang_fun("task_list_ui", tasks=tasks, lang=lang)
  lang_fun("update_task_list_ui", tasks=tasks, lang=lang)

}

task_list_ui_de = function(tasks) {
  restore.point("tasks_ui_de")
  ui= tagList(
    h4("Erhaltene Aufgaben:"),
    div(id="tasksTableDiv",
      dataTableOutput("tasksTable")
    )
  )
  ui
}

update_task_list_ui_de = function(...) {
  udpate.task.list.ui
}

update_task_list_ui = function(tasks, container = "tasksTable") {
  restore.point("update.task.list.ui")
  dt.df = make.tasks.datatable.df(tasks)
  dt = datatable(dt.df,selection = 'none',escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=FALSE),
    class="display compact",
    autoHideNavigation = TRUE, extensions = c('Select'),options = list(
    dom = 't<"bottom"irp><"clear">',
    select = FALSE,
#    columnDefs = list(
#      list(width="4em", targets=c(0)),
#      list(width="8em", targets=c(1)),
#      list(width="11em", targets=c(3,6)),
#      list(width="6em", targets=c(4,5))
#    ),
    autoWidth=TRUE,
    fixedColumns = list(leftColumns = 3)))

  #dt = dt %>% formatStyle("Status", target="row",fontWeight = styleEqual(names(glob$sets$jobstate), c("bold","bold","normal", "normal")))

  shinyEvents::setDataTable(container, dt,server=FALSE)
  return(ui)
}

make.tasks.datatable.df = function(tasks) {
  restore.point("make.tasks.datatable.df")

  if (NROW(tasks)==0) return(NULL)

  tasks$stateorder = ifelse(tasks$taskstate %in% c("open","fresh"), 1, 2)
  tasks = arrange(tasks, stateorder, createtime)

  ids = tasks$taskid

  btns= paste0(
    simpleButtonVector(paste0("editTaskBtn_",ids),icon=icon(name = "pencil"), size="sm",extra.class = "editTaskBtn",extra.head=paste0('data-taskid="',ids,'"'))
  )

  df = transmute(tasks,
    Aktion=btns, Status=state.label(tasks$taskstate), Titel=title, Empfaenger=person.name(receiver) ,
    #Datum=format(givetime, "%d.%m.%y"),
    Frist = deadline,
    Datum=format(createtime, "%d.%m.%y"),
    Sender=givers
  )

  df
}



receiver_task_ui = function(job, task, lang=get.lang(job)) {
  job$lang = "de"
  if (task$taskstate=="fresh")
    task$taskstate = "open"
  lang_fun("receiver_task_ui", job=job, task=task, lang=lang)


}



receiver_task_ui_de = function(job, task) {
  restore.point("receiver_task_ui_de")
  ui = tagList(
    h3(task$title),
    p("Empfänger: ", get.person.name(task$receiver)),
    selectInput("taskstate","Status", choices = list("Offen"="open", "Erledigt"="done"),selected = task$taskstate),
    simpleButton("taskSaveBtn", "Speichern",form.ids = "taskstate"),
    simpleButton("taskCancelBtn","Abbruch")
  )
  buttonHandler("taskSaveBtn", function(formValues,...) {
    restore.point("taskSaveBtn")
    task[names(formValues)] = formValues
    update.task(task)
  })
  ui
}
