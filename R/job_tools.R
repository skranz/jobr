person.name = function(id) {
  id
}

state.label = function(state, lang=get.lang()) {
  state
}

date.to.time.number = function(date=Sys.Date()) {
  as.numeric(as.POSIXct(date))
}

get.valid.senders = function(app=getApp()) {
  if (!is.null(app$valid.senders)) return(app$valid.sender)
  "no valid sender"
}

get.valid.email.domain = function() {
  glob = get.glob()
  glob[["valid.email.domain"]]
}

get.groups = function(glob=get.glob()) {
  groups = glob[["groups"]]
  groups
}

lang_fun = function(fun.name, ..., lang=get.lang()) {
  lang.fun = paste0(fun.name,"_", lang)
  if (exists(lang.fun)) {
    do.call(lang.fun, list(...))
  } else if (lang=="de") {
    stop(paste0("Function ", lang.fun, " does not exist."))
  } else {
    lang_fun(fun.name, ..., lang="de")
  }
}

jobr_error = function(msg) {
  stop(msg)
}

get.lang = function(job=NULL,default="de") {
  if (!is.null(job)) return(job$lang)
  glob = get.glob()
  if (is.null(glob[["lang"]]))
    glob$lang = default
  glob$lang
}

get.job.givers.vec = function(job) {
  trimws(strsplit(job$givers,",",fixed=TRUE)[[1]])
}

get.jobr.url = function() {
  "127.0.0.1"
}

get.links.urls = function(link_ids) {
  base.url = get.jobr.url()
  urls = paste0(base.url, "?link=",link_ids)
  names(urls) = str.left.of(names(link_ids), "_id")
  urls

}

get.date.format = function() {
  "%a %d.%m.%y"
}

get.person.name = function(id) {
  paste0("[Name of ", id,"]")
}

get.proxy.table = function() {
  data.frame(receiver=character(0), proxy.email=character(0))
}

# Get emails of all proxies
get.receiver.proxies = function(receiver) {
  receiver
}

create.job.id = function(job=NULL) {
  random.string(1,16)
}

create.task.id = function(task=NULL) {
  random.string(1,17)
}

create.link.id = function(task=NULL) {
  random.string(1,18)
}

