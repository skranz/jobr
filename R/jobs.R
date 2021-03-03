examples.jobr = function() {
  setwd("C:/libraries/jobr")
  restore.point.options(display.restore.point=TRUE)

  job = list(
    jobid = create.job.id(),
    createtime = Sys.time(),
    jobtype = "in",
    jobstate = "open",
    tpl_title = "Lehrprogramm {semester} überprüfen (Koordinator: {receiver_name})",
    tpl_body = "Liebe Kolleginnen und Kollegen,
bitte überprüfen Sie bis zum {deadline} alle Veranstaltungen für das {semester} die von ihnen koordiniert werden. Schauen Sie hierfür bitte in die jobr-Software.

Wenn alles erfolgreich abgeschlossen wurde, klicken Sie bitte hier um es in unserer Job-App zu vermerken:

{link_task}

Sollten Fragen auftauchen, kontaktieren Sie mich bitte per Email.

Beste Grüße,
Sebastian Kranz

",
    values = list(semester="SoSe 2021"),
    inputs = data.frame(input="Zahl", label="Geben Sie bitte ein Zahl ein.", value=""),
    deadline = as.Date("2021-03-02"),
    tpl_receivers = "{wiwi_koordinatoren},studiendekan_inf",
    sender = "sebastian.kranz@uni-ulm.de",
    tpl_givers = "sebastian.kranz@uni-ulm.de, alexander.rieber@uni-ulm.de"
  )

  job = fill_job_templates(job)
  job$tasks = job_to_tasks(job)
  task = job$tasks[[1]]
  names(task)

  db = get.jobdb()
  delete.all.from.jobdb()
  insert.job.and.tasks(job)
}


fill_job_task_template = function(txt,job, task=NULL) {
  restore.point("fill_job_task_template")
  pos = str.blocks.pos(txt, "{", "}")

  tpl.funs = get.template.funs()
  values = c(job$values, task$values, get.global.template.values())

  tpl.names = str.at.pos(txt, pos$inner)

  unique.vals = lapply(unique(tpl.names), function(tpl.name) {
    ph = paste0("{", tpl.name, "}")
    if (tpl.name %in% names(tpl.funs))
      return(tpl.funs[[tpl.name]](job,task,txt=txt, tpl.name=tpl.name, ph=ph))
    if (tpl.name %in% names(values))
      return(values[[tpl.name]])
    return(ph)
  })
  names(unique.vals) = unique(tpl.names)

  tpl.vals = unique.vals[tpl.names]
  new.txt = stringtools::str.replace.at.pos(txt,pos$outer,tpl.vals)
  new.txt
}

fill_job_templates = function(job) {
  restore.point("fill_job_templates")
  fields = names(job)
  tpl_fields = fields[startsWith(fields,"tpl_")]
  value_fields = str.right.of(tpl_fields, "tpl_")
  for (i in seq_along(tpl_fields)) {
    txt = job[[tpl_fields[i]]]
    job[[value_fields[i]]] = fill_job_task_template(txt, job)
  }
  job
}

job_to_tasks = function(job) {
  restore.point("job_to_tasks")
  recs = check.and.extract.job.receivers(job)
  tasks = lapply(recs,task_from_job, job=job)
  tasks
}

task_from_job = function(rec, job) {
  restore.point("task_from_job")
  task = list(
    jobid = job$jobid,
    taskid = create.task.id(),
    taskstate = "f",
    deadline = job$deadline,
    createtime = Sys.time(),
    receiver = rec,
    proxies = get.receiver.proxies(rec),
    comment = "",

    input_ans = job$inputs
  )

  link_ids = task[(startsWith(names(task),"link_"))]
  task$values = as.list(get.links.urls(link_ids))


  # Replace remaining placeholders in job values
  # that were originally templates
  fields = setdiff(names(job),"tpl_receivers")
  tpl_fields = fields[startsWith(fields,"tpl_")]
  value_fields = str.right.of(tpl_fields, "tpl_")

  for (i in seq_along(value_fields)) {
    txt = job[[value_fields[i]]]
    value = fill_job_task_template(txt, job, task)
    if (has.placeholder(value)) {
      ph = find.unique.placeholders(job$receivers)
      jobr_error("When creating the concrete tasks, we still could not evaluate the placeholder variables ",paste0(ph,collapse=", "),". You either entered the wrong variable name or forgot to assign a value in your job specification.")
    }
    task[[value_fields[i]]] = value
  }
  task
}


check.and.extract.job.receivers = function(job) {
  if (is.empty(job$receivers))
    jobr_error("You must specify at least on receiver for the job.")
  if (has.placeholder(job$receivers)) {
    ph = find.unique.placeholders(job$receivers)
    jobr_error("Your job receivers have placeholders ",paste0(ph,collapse=", ")," that can not be evaluated.")
  }
  rli = strsplit(job$receivers,",", fixed=TRUE)[[1]] %>% trimws()

  # Check for non-email placeholders and transform them

  return(unlist(rli))
}

