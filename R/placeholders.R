fill.task.placeholder = function(task, job=task$job, ph.funs = get.ph.funs()) {

  phs = names(ph.funs)

  body = job$tpl_body
  title = job$tpl_title
  txt = paste0(title, body)

  for (ph in phs) {
    curley = paste0("{",ph,"}")
    if (!(has.substr(body,curley) | has.substr(title, curley)))
      next
    val = ph.funs[[ph]](job=job, task=task)
    body = gsub(curley, val, body, fixed=TRUE)
    title = gsub(curley, val, title, fixed=TRUE)
  }
  task$body = body
  task$title = title
  return(task)
}


get.ph.funs = function() {
  funs = default.ph.funs()
  custom = custom.ph.funs()
  funs[names(custom)] = custom
  funs
}


get.global.ph.values = function() {
  list()
}


custom.ph.funs = function() {
  list()
}

default.ph.funs = function() {
  list(
    deadline = function(job, task=NULL, ...) {
      lang_fun("format_deadline", job$deadline)
    },
    tasklink = function(job, task=NULL,ph, ...) {
      if (is.null(task)) return(ph)
      get.links.urls(task$tasklink)
    }
  )
}
