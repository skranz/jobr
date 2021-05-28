set_nav_ui = function(nav = app$nav, app=getApp(), id = "navUI"){
  restore.point("set_nav_ui")
  if (length(nav)<=0) {
    setUI(id, div())
    return()
  }

  btns = lapply(nav[[length(nav)]], function(nav.el) {
    lang_fun("nav_el_btn", nav.el)
  })
  ui = tagList(
    btns,
    hr()
  )
  setUI(id, ui)
}

nav_el_btn_de = function(nav.el) {

  if (nav.el == "job") {
    btn = smallButton("backToJobBtn", "Zurück zu Job",style = "margin-right: 1em; margin-top: 4px;")
    buttonHandler("backToJobBtn", function(..., app=getApp()) {
      reset_nav("job")
      app$job = load.job.with.tasks(app$job$jobid)
      set_showjob_ui()
      #set_nav_ui()
    })
  } else if (nav.el == "main") {
    btn = smallButton("backToJoblistBtn", "Zurück",style = "margin-right: 1em; margin-top: 4px;")
    buttonHandler("backToJoblistBtn", function(..., app=getApp()) {
      reset_nav("main")
      set_main_ui()
    })
  } else {
    btn = NULL
  }
  return(btn)
}

reset_nav = function(backto, app=getApp()) {
  row = which(app$nav == "backto")
  ind = row -1
  if (isTRUE(ind)>=1) {
    app$nav = app$nav[1:ind]
  } else {
    app$nav = NULL
  }
}
