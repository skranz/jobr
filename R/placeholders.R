
get.template.funs = function() {
  funs = default.template.funs()
  custom = custom.template.funs()
  funs[names(custom)] = custom
  funs
}


get.global.template.values = function() {
  list()
}


custom.template.funs = function() {
  list()
}

default.template.funs = function() {
  list(
    deadline = function(job, task=NULL, ...) {
        format(job$deadline,get.date.format())
    },
    tasklink = function(job, task=NULL,ph, ...) {
      if (is.null(task)) return(ph)
      get.links.urls(task$tasklink)
    }
  )
}
