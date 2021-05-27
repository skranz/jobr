example_joblist_ui = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()
  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  app$jobs = jobs = dbGet(db, "job")

  app$nav = NULL

  app$ui = fluidPage(
    jobr.header(),
    uiOutput("navUI"),
    uiOutput("innerUI")
  )
  set_joblist_ui()
  viewApp()

}

set_joblist_ui = function(id="innerUI", app=getApp()) {
  ui = lang_fun("joblist_ui")
  init.joblist.handlers()
  setUI(id, ui)
}

init.joblist.handlers = function() {
  setDownloadHandler("joblistExcelBtn",
    filename=function(app = getApp()) paste0("jobs.xlsx"),
    content = function(file, ...) {
      restore.point("downloadContent")
      app=getApp()
      withProgress(message="Die Exceltabelle wird erstellt...",
        make.joblist.xlsx(file)
      )
    }
  )
  classEventHandler("showJobBtn",event = "click",function(data=NULL, app=getApp(), ...) {
    restore.point("showJobBtn")
    jobid = data$jobid
    app$job = job = load.job.with.tasks(jobid)
    app$nav = c(app$nav, "joblist")
    set_showjob_ui()
    set_nav_ui()
  })


}

make.joblist.xlsx = function(file, jobs=app$jobs, app=getApp()) {
  restore.point("make.joblist.xlsx")
  df = jobs
  write.xlsx(df, file = file, colNames = TRUE)
}

joblist_ui_de = function() {
  restore.point("joblist_ui_de")
  ui = tagList(
    h4(paste0("Verteilte Jobs")),
    div(class="input-group",
      downloadButton("joblistExcelBtn", "Excel",icon = icon("download"))
    ),
    hr(),
    joblist.html.table()
  )
  ui
}

joblist.html.table = function(jobs = app$jobs, app=getApp()) {
  restore.point("joblist.html.table")

  jobstates = c("c"="Noch nicht versandt", "s"="In Bearbeitung","d" ="Fertig")

  df = jobs %>%
    select(jobstate, createtime,deadline, tpl_title,sender) %>%
    mutate(jobstate=jobstates[jobstate])

  #val_tab = NULL
  show.btns = simpleButtonVector(paste0("showJobBtn_",jobs$jobid),icon=icon(name = "eye"), size="xs",extra.class = "showJobBtn",extra.head=paste0('data-jobid="',jobs$jobid,'"'))

  copy.btns = simpleButtonVector(paste0("copyJobBtn_",jobs$jobid),icon=icon(name = "copy"), size="xs",extra.class = "copyJobBtn",extra.head=paste0('data-jobid="',jobs$jobid,'"'))


  df = bind_cols(show.btns, copy.btns, df)
  df = arrange(df, desc(createtime),desc(deadline))

  header=c("","","Status","Erstellt","Deadline","Titel","Sender")


  MyDataTable(id="joblistTable",df,header=header, searching=TRUE)
}
