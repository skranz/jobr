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

add_json_fields = function(job) {
  if (is.data.frame(job)) {
    job$inputs_json = sapply(job$inputs, toJSON)
    job$values_json = sapply(job$values, toJSON)
  } else {
    job$inputs_json = toJSON(job$inputs)
    job$values_json = toJSON(job$values)
  }

  job
}

from_json_fields = function(job) {
  if (is.data.frame(job)) {
    job$inputs = lapply(job$inputs_json, fromJSON)
    job$values = lapply(job$values_json, fromJSON)
  } else {
    job$inputs = fromJSON(job$inputs_json)
    job$values = fromJSON(job$values_json)
  }
  job
}


insert.job.and.tasks = function(job, tasks=job$tasks, db = get.jobdb(), delete.prev=TRUE) {
  restore.point("insert.job.and.tasks")
  giver.df = data.frame(jobid = job$jobid, giver = get.job.givers.vec(job))

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
  return(invisible())
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

load.task.with.job = function(taskid, db = get.jobdb()) {
  restore.point("load.task.with.job")
  task = dbGet(db, "task", list(taskid=taskid))
  task = from_json_fields(task)
  job = dbGet(db, "job", list(jobid=task$jobid))
  job = from_json_fields(job)
  task$givers = job$givers
  list(job=job, task=task)
}

update.task = function(task, db=get.jobdb()) {
  restore.point("update.task")
  task = add_json_fields(task)
  # Since taskid is a unique key we can just
  # insert with replace mode to update
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

load.tasks = function(receivers, db=get.jobdb(), taskstatus=NULL, startdate = NULL) {
  restore.point("load.tasks")
  param = list(receiver=receivers)
  if (!is.null(taskstatus))
    param = c(param, list(taskstatus=taskstatus))
  where.in = length(receivers) > 1

  where.sql = sql.where.code(db, param, where.in=where.in)
  if (!is.null(start.date)) {
    starttime = to.db.datetime(startdate)
    where.sql = paste0(where.sql, " AND createtime >= :starttime")
    param = c(param, list(starttime = starttime))
  }
  #sql = paste0("select task.*, job.givers from task
  #INNER JOIN job
  #ON task.jobid = job.jobid ", where.sql)
  #tasks = dbGet(db,"task",param,sql = sql, where.in = where.in)
  tasks = dbGet(db,c("task","job"),param,fields="*, job.givers",joinby="jobid", where.in = where.in, where.sql=where.sql)

  tasks = from_json_fields(tasks)
  tasks
}
