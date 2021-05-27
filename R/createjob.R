# Functions to check, parse and create new jobs
# The UI functions are in ui_createjob

check.and.send.job.click = function(formValues, ..., app=getApp()) {
  restore.point("check.and.send.job.click")
  glob = app$glob
  job = try(parse.and.check.new.job(formValues, job=app$job))

  error = get.check.error(job)
  if (!is.null(error)) {
    show.check.error(error)
    return()
  }

  job$tasks = job.to.tasks(job)

  res = try(insert.job.and.tasks(job))

  if (is(res,"try-error")) {
    error = list(step=0, var=NA, msg=as.character(res),head="Fehler beim Speichern des Jobs in der Datenbank")
    show.check.error(error)
    return()
  }

  html = colored.html("Ihr Job wurde korrekt gespeichert.",color = c("#000055"))
  setUI("checkJobMsg", HTML(html))
}

new.job.test.email.click = function(formValues, ..., app=getApp()) {
  restore.point("new.job.test.email.click")
  html = colored.html("Testemail noch nicht implementiert.",color = c("#000055"))
  setUI("checkJobMsg", HTML(html))
}



job.to.tasks = function(job) {
  restore.point("job.to.tasks")
  recs = unique(unlist(job$receivers_list))
  tasks = lapply(recs,task.from.job, job=job)
  tasks
}

task.from.job = function(rec, job) {
  restore.point("task.from.job")
  task = list(
    jobid = job$jobid,
    taskid = create.task.id(),
    taskstate = "f",
    deadline = job$deadline,
    createtime = Sys.time(),
    receiver = rec,
    proxies = get.receiver.proxies(rec),
    comment = "",
    input_ans = ""
  )
  task
}



parse.and.check.new.job = function(formValues,job = lang_fun("empty_job"),..., app=getApp()) {
  args = list(...)
  restore.point("parse.and.check.new.job")
  glob = get.glob(); glob$check.error = NULL

  job[names(formValues)] = formValues

  job = lang_fun("check_receivers", job)
  job = lang_fun("check_further_givers", job)
  job = lang_fun("check_deadline", job)

  job = lang_fun("check_tpl_title", job)
  job = lang_fun("check_tpl_body", job)

  # Check particular inputs
  if (job$input_type != "n") {
    job$input_fields = parse_input_txt(formValues$input_fields_txt)
  }
  attach = bind_rows(app$temp.job.attach)
  if (NROW(attach)==0) attach = NULL
  job$attach = attach

  if (is.null(job[["jobid"]]))
    job$jobid = create.job.id()

  job$jobstate = "c"

  return(job)
}

check_tpl_title_de = function(job) {
  if (is.empty(job[["tpl_title"]])) {
    check.job.error(step=2, "tpl_title","Bitte geben Sie einen Titel für Ihren Job ein.", head="Es gibt einen Fehler in Schritt 2:")
  }
  job
}

check_deadline_de = function(job) {
  if (is.empty(job[["deadline"]])) {
    check.job.error(step=1, "deadline","Bitte geben Sie eine Deadline ein.", head="Es gibt einen Fehler in Schritt 1:")
  }
  job
}


check_tpl_body_de = function(job) {
  if (is.empty(job[["tpl_body"]])) {
    check.job.error(step=2, "tpl_title","Bitte geben Sie einen Emailtext für Ihren Job ein.", head="Es gibt einen Fehler in Schritt 2:")
  }
  job
}


get.check.error = function(try.error) {
  if (!is(try.error,"try-error")) return(NULL)
  glob = get.glob()
  if (!is.null(glob[["check.error"]]))
    return(glob$check.error)
  list(step=0, var=NA, msg=paste0("Error in R code: \n", as.character(try.error)), head=NULL)
}

show.check.error = function(error, ui="checkJobMsg", app=getApp()) {
  restore.point("show.check.error")
  html = colored.html(paste0(error$head, if(!is.empty(head)) "<br>", error$msg),color = c("#990000"))
  setUI(ui, HTML(html))
}

check.job.error = function(step, var, msg, head=NULL) {
  glob = get.glob()
  glob$check.error = list(step=step, var=var, msg=msg,head=head)
  stop("Job not correctly checked.")
}


parse_persons_txt = function(txt, allow.empty=FALSE, groups=get.groups(), valid.email.domain = get.valid.email.domain()) {
  restore.point("check_persons")

  txt = trimws(txt)
  txt = gsub(" ",",",txt, fixed=TRUE)
  txt = strsplit(txt,",", fixed=TRUE)
  if (length(txt)>0) txt = txt[[1]]
  txt = txt[txt != ""]
  txt = tolower(txt)
  if (length(txt)==0) {
    if (!allow.empty) {
      return(list(ok=FALSE,error="empty"))
    }
    return(list(ok=TRUE, persons=NULL, emails=NULL))
  }
  is.email = has.substr(txt,"@")
  no.email = txt[!is.email]

  no.group = no.email[!no.email %in% names(groups)]
  if (length(no.group)>0) {
    return(list(ok=FALSE, error="nogroup", which=no.group))
  }

  email = txt[is.email]
  domain = str.right.of(email,"@")
  if (!is.null(valid.email.domain)) {
    invalid.domain = email[!domain %in% valid.email.domain]
    if (length(invalid.domain)>0) {
      return(list(ok=FALSE, error="wrongdomain", which=invalid.domain, domains=valid.email.domain))
    }
  }

  emails = as.list(txt)
  names(emails) = txt
  emails[no.email] = groups[no.email]

  return(list(ok=TRUE, persons = txt, emails=emails))
}

check_receivers_de = function(job) {
  restore.point("check_receivers")
  head = "Es gibt einen Fehler in Schritt 1 bei der Spezifikation der Empfänger:"

  res = parse_persons_txt(job$receivers, allow.empty=FALSE)
  if (res$ok) {
    job$receivers = paste0(res$persons, collapse=", ")
    job$receivers_list = res$emails
    return(job)
  }
  if (res$error == "empty") {
    check.job.error(step=1, var="receivers", msg="Sie haben keine Empfänger angegeben.", head=head)
  }
  if (res$error == "nogroup") {
    if (length(res$which)==1) {
      check.job.error(step=1, var="receivers", msg=paste0("Der Empfänger ", paste0(res$which, collapse=", "), " ist weder eine valide Gruppenabkürzung noch eine Emailadresse."), head=head)
    } else {
      check.job.error(step=1, var="receivers", msg=paste0("Die Empfänger ", paste0(res$which, collapse=", "), " sind weder valide Gruppenabkürzungen noch Emailadressen."), head=head)
    }
  }
  if (res$error == "wrongdomain") {
    check.job.error(step=1, var="receivers", msg=paste0("Sie dürfen die Aufträge nur an Empfänger mit Emaildomain ", paste0(res$domains, collapse=" oder "), " senden. Folgende Empfänger sind ungültig:",paste0(res$which, collapse=", "), "."), head=head)
  }
  stop("")

}

check_further_givers_de = function(job) {
  restore.point("check_further_givers")
  head = "Es gibt einen Fehler in Schritt 1 bei der Spezifikation der weiteren Auftraggeber:"

  res = parse_persons_txt(job$further_givers, allow.empty=TRUE)
  if (res$ok) {
    sender.list = list(job$sender)
    names(sender.list) = job$sender
    job$giver_list = c(sender.list, res$emails)
    job$givers = paste0(unique(c(job$sender), job$further_givers), collapse=", ")
    return(job)
  }
  if (res$error == "nogroup") {
    if (length(res$which)==1) {
      check.job.error(step=1, var="further_givers", msg=paste0("Die Bezeichnung ", paste0(res$which, collapse=", "), " ist weder eine valide Gruppenabkürzung noch eine Emailadresse."), head=head)
    } else {
      check.job.error(step=1, var="further_givers", msg=paste0("Die Bezeichungen ", paste0(res$which, collapse=", "), " sind weder valide Gruppenabkürzungen noch Emailadressen."), head=head)
    }
  }
  if (res$error == "wrongdomain") {
    check.job.error(step=1, var="further_givers", msg=paste0("Sie dürfen nur an Personen mit Emaildomain ", paste0(res$domains, collapse=" oder "), " eintragen. Folgende Personen sind ungültig:",paste0(res$which, collapse=", "), "."), head=head)
  }
  stop("")
}

