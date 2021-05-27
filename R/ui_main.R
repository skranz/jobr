example_tasklist_ui = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()
  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  app$tasks = tasks = load.tasks()
  app$jobs = jobs = dbGet(db, "job")


  app$nav = NULL

  app$ui = fluidPage(
    jobr.header(),
    uiOutput("mainUI")
  )
  set_main_ui()
  viewApp()

}

set_main_ui = function(id="mainUI", app=getApp()) {
  ui = lang_fun("main_ui")
  init.main.handlers()
  setUI(id, ui)

  set_tasklist_ui("tasklistUI")
  set_joblist_ui("joblistUI")

}

init.main.handlers = function() {

}

main_ui_de = function(tasks = app$tasks, app=getApp()) {
  restore.point("main_ui_de")
  num.open = sum(tasks$taskstate!="d")

  ui = tagList(
    tabsetPanel(
      tabPanel("Erhaltene Jobs", uiOutput("tasklistUI")),
      tabPanel("Verteilte Jobs", uiOutput("joblistUI")),
      tabPanel("Empfängerlisten"),
      tabPanel("Einstellungen")
    )
  )
  ui
}
