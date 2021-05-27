get.jobdb.dir = function() {
  return(getwd())
}

jobdb.schemas = function() {
  schema.file = system.file("schema/jobdb.yaml",package = "jobr")
  schemas = dbmisc::load.and.init.schemas(schema.file)
  schemas
}

get.jobdb = function(db.dir=getwd(), db.name="jobdb.sqlite", glob=get.glob(), schemas=jobdb.schemas()) {
  restore.point("get.jobdb")
  db = glob$db

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    db = set.db.schemas(db, schemas)
    glob$db = db
  }
  db
}



insert.job.and.tasks = function(job, tasks=job$tasks, db = get.jobdb(), delete.prev=TRUE) {
  restore.point("insert.job.and.tasks")
  giver.df = data.frame(jobid = job$jobid, giver = get.job.givers.vec(job))

  job = add_job_json_fields(job)

  dbWithTransaction(db,{
    if (delete.prev) {
      dbDelete(db,"job",list(jobid=job$jobid))
      dbDelete(db,"jobgiver",list(jobid=job$jobid))
      dbDelete(db,"jobattach",list(jobid=job$jobid))
      dbDelete(db,"task",list(jobid=job$jobid))
    }
    dbInsert(db,"job", job)
    for (task in tasks) {
      dbInsert(db,"task", task)
    }
    dbInsert(db,"jobgiver", giver.df)
  })
  return(invisible(job))
}

delete.all.from.jobdb = function(db = get.jobdb()) {
  restore.point("insert.job.and.tasks")
  giver.df = data.frame(jobid = job$jobid, giver = get.job.givers.vec(job))
  dbWithTransaction(db,{
    dbDelete(db,"job",list())
    dbDelete(db,"jobgiver",list())
    dbDelete(db,"task",list())
#    dbDelete(db,"proxy", list())
  })
}

load.job.with.tasks = function(jobid, db = get.jobdb()) {
  restore.point("load.job.with.tasks")
  job = dbGet(db, "job", list(jobid=jobid)) %>% as.list()
  job = from_job_json_fields(job)
  job$tasks = dbGet(db, "task", list(jobid=jobid))
  job
}

init.task = function(task) {
  task = as.list(task)
  task = from_task_json_fields(task)
  task = fill.task.placeholder(task)
  task
}

load.task.with.job = function(taskid, db = get.jobdb()) {
  restore.point("load.task.with.job")
  task = dbGet(db, "task", list(taskid=taskid))
  if (NROW(task)==0) stop("Task with taskid ", taskid, " is not in the database.")
  task = as.list(task)
  task = from_task_json_fields(task)
  job = dbGet(db, "job", list(jobid=task$jobid)) %>% as.list()
  job = from_job_json_fields(job)
  task$job = job
  task
}

update.task = function(task, db=get.jobdb()) {
  restore.point("update.task")
  dbInsert(db,"task",as.list(task),mode = "replace")
}

load.users.of.proxy = function(proxy, db=get.jobdb(), include.proxy=TRUE) {
  users = dbGet(db,"proxy", list(proxy=proxy))$user
  union(users, proxy)
}

load.tasks.of.proxy = function(proxy,...) {
  receivers = load.users.of.proxy(proxy,...)
  load.tasks(receivers,...)
}

load.tasks = function(receivers=NULL, db=get.jobdb(), taskstatus=NULL, startdate = NULL) {
  restore.point("load.tasks")
  if (!is.null(receivers)) {
    param = list(receiver=receivers)
  } else {
    param = list()
  }
  if (!is.null(taskstatus))
    param = c(param, list(taskstatus=taskstatus))
  where.in = length(receivers) > 1

  where.sql = sql.where.code(db, param, where.in=where.in)
  if (!is.null(startdate)) {
    starttime = to.db.datetime(startdate)
    where.sql = paste0(where.sql, " AND task.createtime >= :starttime")
    param = c(param, list(starttime = starttime))
  }
  #sql = paste0("select task.*, job.givers from task
  #INNER JOIN job
  #ON task.jobid = job.jobid ", where.sql)
  #tasks = dbGet(db,"task",param,sql = sql, where.in = where.in)
  if (where.sql == "") param = NULL

  tasks = dbGet(db,c("task","job"),param,fields="task.*, job.givers, job.sender, job.tpl_title",joinby="jobid", where.in = where.in, where.sql=where.sql)

  tasks = from_tasks_json_fields(tasks)
  tasks
}

json_to_list = function(json, use.lapply=FALSE, simplify=FALSE) {
  if (use.lapply) {
    res = lapply(json, json_to_list, use.lapply=FALSE, simplify=simplify)
    return(res)
  }
  if (is.null(json) | is.na(json)) {
    list()
  } else{
    fromJSON(json, simplifyVector=simplify)
  }
}

from_job_json_fields = function(job) {
  job$input_fields = json_to_list(job$input_fields_json, use.lapply = is.data.frame(job))
  job$input_field_names = unlist(lapply(job$input_fields, function(field) field$var))
  job
}

add_job_json_fields = function(job) {
  if (is.data.frame(job)) {
    job$input_fields_json = sapply(job$input_fields, toJSON)
  } else {
    job$input_fields_json = toJSON(job$input_fields)
  }
  job
}

add_task_json_fields = function(task) {
  sep = field.sep()
  if (is.data.frame(task)) {
    task$values_txt = sapply(task$values, paste0, collapse=sep)
  } else {
    task$values_txt = paste(task$values, collapse=sep)
  }
  task
}

job_tasks_values_df = function(job, tasks=job$tasks, field_names=job$input_field_names) {
  sep = field.sep()
  empty = paste0(rep(sep, length(field_names)),collapse="")
  values_txt = tasks$values_txt
  values_txt[is.na(values_txt)] = empty

  mat = do.call(rbind,strsplit(values_txt,field.sep(),fixed=TRUE))
  colnames(mat) = field_names
  as.data.frame(mat)
}


from_tasks_json_fields = function(tasks) {
  tasks$values = strsplit(tasks$values_txt,field.sep(),fixed=TRUE)
  tasks
}

from_task_json_fields = function(task) {
  if (is.na(task$values_txt)) {
    task$values = list()
  } else {
    task$values = strsplit(task$values_txt,field.sep(),fixed=TRUE)[[1]]
  }
  task
}
