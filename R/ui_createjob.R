example.ui_createjob = function() {
  setwd("C:/libraries/jobr")
  db = get.jobdb()
  app = eventsApp()

  app$glob$groups = list(seb="sebastian.kranz@uni-ulm.de")
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))
  init.create.job()
  job = lang_fun("empty_job")
  app$valid_senders = "sebastian.kranz@uni-ulm.de"
  app$job = job

  inner.ui = lang_fun("jobui",job=job)
  app$ui = fluidPage(
    tags$script(src="jobr/createjob.js"),
    tags$script(src="jobr/helplabel.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "jobr/helplabel.css"),
    inner.ui
  )
  viewApp()

  proxy = "sebastian.kranz@uni-ulm.de"
  tasks = load.tasks.of.proxy(proxy)

}

init.create.job = function(app=getApp()) {
  app$temp.job.attach = lapply(1:4, function(i) NULL)
  init.create.job.handlers()
}

init.create.job.handlers = function() {
  changeHandler("attach_upload", attach.upload.handler)

  buttonHandler("newJobTestEmailBtn", new.job.test.email.click)
  buttonHandler("checkAndSendJobBtn", check.and.send.job.click)

  classEventHandler("preview-fields-icon",event="click", fun = function(formValues,...) {
    input_fields_txt = getInputValue("input_fields_txt")
    restore.point("preview.fields.click")

    fields = try(parse_input_txt(input_fields_txt))
    error = get.check.error(fields)
    if (!is.null(error)) {
      show.check.error(error,ui = "previewFieldsUI")
      return()
    }
    fields_ui = input_fields_ui(fields)
    setUI("previewFieldsUI", wellPanel(fields_ui))
    dsetUI("previewFieldsUI", wellPanel(fields_ui))
  })

  buttonHandler("jobcreate_nextbtn1", function(...) {
    show.job.create.step(2)
  })
  buttonHandler("jobcreate_nextbtn2", function(...) {
    show.job.create.step(3)
  })
  buttonHandler("jobcreate_nextbtn3", function(...) {
    show.job.create.step(4)
  })
  buttonHandler("jobcreate_nextbtn4", function(...) {
    show.job.create.step(5)
  })
  buttonHandler("jobcreate_prevbtn2", function(...) {
    show.job.create.step(1)
  })
  buttonHandler("jobcreate_prevbtn3", function(...) {
    show.job.create.step(2)
  })
  buttonHandler("jobcreate_prevbtn4", function(...) {
    show.job.create.step(3)
  })
  buttonHandler("jobcreate_prevbtn5", function(...) {
    show.job.create.step(4)
  })
}

attach.upload.handler = function(value, ...,  app=getApp()) {
  restore.point("attach.upload.handler")

  url = value$datapath
  url = gsub("\\","/", url, fixed=TRUE)
  url = paste0("file:///", url)
  new = data.frame(jobid=NA, filename=value$name, path=NA, mb=value$size/1000, temp_path = value$datapath, url = url)

  if (is.null(app$job$attach_df)) {
    app$job$attach_df = new
  } else {
    app$job$attach_df = bind_rows(app$job$attach_df,new)
  }
  attach.list.ui(set=TRUE)
}

attach.list.ui = function(attach_df = app$job$attach_df, id="attach_list_ui", app=getApp(), set=FALSE) {
  restore.point("attach.list.ui")

  if (NROW(attach_df)==0) {
    html = "<p>---</p>"
  } else {
    n = NROW(attach_df)
    btns = simpleButtonVector(id = paste0("del_attach_btn",1:n),label = "Remove")
    links = paste0("<a href='", attach_df$url,"', target='_blank'>",attach_df$filename,"</a>")

    inner = paste0("<tr><td>", btns,"</td><td style='padding-left: 1em'>",links,"</td></tr>", collapse="\n")
    html = paste0("<table id='attach_list_table'>",inner ,"</table>")
  }
  if (set) {
    setInnerHTML(id, html)
    return(invisible(html))
  }
  return(HTML(html))
}
empty_job_de = function() {
  job = list(
    receivers = "seb",
    tpl_title = "Mein Titel",
    tpl_body = "Liebe Kolleginnen und Kollegen,

ich würde Sie bitten bis zum {deadline} Folgendes zu machen:

1.


2. Klicken Sie auf folgenden Link, um kurz zu vermerken, dass Sie alles erledigt haben:

{tasklink}

Sollten Fragen auftauchen, kontaktieren Sie mich bitte per Email.

Viele Grüße,

",
    response_type = "s",
    input_type = "n",
    upload_type = "n",
    response_items_txt = "Kurse für nächstes Semester überprüft.\nViersemesterplanung überprüft.\nPrüfungstermine für Seminare eingetragen.",
    input_fields_txt = "Name des Doktoranden = Max Mustermann
Promotion abgeschlossen? [ ]
Gebiet [ VWL | BWL | anderes ]
Beschreibung Promotion+ = Längere Beschreibung.",
    jobcomment = "",
    attach_df = NULL
  )

}


jobui_de = function(job=NULL, lang="de", start.pane = "step1", new=TRUE) {
  if (new) {
    title = "Neuer Job"
  } else {
    title = "Bearbeite Job"
  }

  ui = navlistPanel(id="createJobPanel", selected=start.pane,
    title,
    tabPanel("Schritt 1: Empfänger, Sender, Deadline",value="step1", lang_fun("jobui1",job=job,lang=lang)),
    tabPanel("Schritt 2: Titel und Nachricht",value="step2",lang_fun("jobui2",job=job,lang=lang)),
    tabPanel("Schritt 3: Dateianhänge",value="step3", lang_fun("jobui3",job=job,lang=lang)),
    tabPanel("Schritt 4: Rückmeldung",value="step4", lang_fun("jobui4",job=job,lang=lang)),
    tabPanel("Schritt 5: Überprüfen und Senden",value="step5", lang_fun("jobui5",job,lang=lang)),
    widths = c(2,10)
  )
  ui
}


# Sender und Empfaenger
jobui1_de = function(job,...) {
  valid.senders =  get.valid.senders()
  if (!isTRUE(job$sender %in% valid.senders))
    job$sender = valid.senders[1]
  ui = tagList(div(id="new-job-4",
    h3("Schritt 1: Empfänger, Sender, Deadline"),
    div(class="help-block", HTML("Für Informationen zu den Eingabefeldern klicken Sie auf das jeweilige &#9432; Symbol.")),
    textAreaInput2("receivers", "Empfänger",value = job$receivers, rows=1,width="100%",help = "Geben Sie die Emailadressen der Empfänger per Komma getrennt ein. Sie können auch gespeicherte Gruppenbezeichnungen eingeben. Klicken Sie auf das Adresskartensymbol für eine Liste.", label.extras=icon("address-card", class = "receiver-address-book-icon blue-hover")),
    selectInput2("sender", "Sender",choices = valid.senders,selected = job$sender, width="100%", help = helpText("Offizieller Absender können nur Sie selbst sein, oder eine Person für die sie offizieller Vertreter sind.")),
    textInput2("further_givers","Weitere Personen, die Rückmeldungen sehen können.",value = job$further_givers,  width="100%", help=helpText("Diese Personen erhalten eine Kopie des Job-Links, können den Fortschritt betrachten und Aktionen durchführen, z. B. eine Errinerungsemail versenden.")),
    dateInput2("deadline","Deadline für Job", value=job$deadline, language="de", help="Rückmeldungen sind auch noch nach der Deadline möglich, solange sie nicht manuell den Job geschlossen haben."),
    textAreaInput2("jobcomment", "Interne Notizen zum Job (optional)",value = job$jobcomment, rows=2,width="100%", help="Wenn Sie diesen Job als Vorlage für einen neuen Job nehmen, (sinnvoll bei sich wiederholenden Aufgaben), werden die internen Notizen mitkopiert. Die Notizen sind somit ggf. hilfreich als Gedächtnisstütze."),
    simpleButton("jobcreate_nextbtn1","Weiter")
  ))
  ui

}

jobui2_de = function(job,...) {
  restore.point("jobui2")
  ui = tagList(
    h3("Schritt 2: Nachricht formulieren"),
      helpText("Bitte setzen Sie in der Nachricht den Platzhalter  {tasklink} für einen Link zum Auftrag. Sie können auch den Platzhalter {deadline} beliebig nutzen."),
      textInput("tpl_title","Titel",value = job$tpl_title, width="100%"),
      textAreaInput2("tpl_body","Nachricht",value = job$tpl_body, width="100%", rows=12),
    simpleButton("jobcreate_prevbtn2","Zurück"),
    simpleButton("jobcreate_nextbtn2","Weiter")
  )
  ui
}

jobui3_de = function(job,...) {
  restore.point("jobui3")
  allowed = c(".pdf",".xlsx",".docx",".rmd",".md",".txt",".zip")

  ui = tagList(div(id="new-job-3",
    h3("Schritt 3: Dateianhänge hinzufügen"),
    helpText("Beachten Sie dass nur bestimmte Formate für Anhänge freigeschaltet sind. Nutzen Sie z. B. aus Sicherheitsgründen für Worddokumente .docx statt .doc. Bei mehr als 4 Anhängen packen Sie die Dateien bitte vorher in eine ZIP Datei."),
    fileInput("attach_upload", "Neuen Dateianhang hochladen", accept = allowed,multiple = TRUE),
    HTML('<label class="control-label" for="attach_list_ui">Hochgeladene Dateianhänge</label>'),
    div(id="attach_list_ui", attach.list.ui(set=FALSE)),
    tags$br(),
    simpleButton("jobcreate_prevbtn3","Zurück"),
    simpleButton("jobcreate_nextbtn3","Weiter")
  ))
  ui
}

# Rueckmeldung
jobui4_de = function(job,...) {
  response_descr = c(
    s = "Bestätige wenn Auftrag erledigt",
#    m = "Hake mehrere Punkte separat ab",
    n = "Kein Bestätigung erforderlich"
  )

  input_descr = c(
    n = "Keine Daten eingeben",
    s = "Eingabe eines Datensatzes",
    m = "Eingabe mehrerer Datensätze",
    x = "Eintrag in Exceltabelle"
  )

  upload_descr = c(
    n = "Nein.",
    s = "Ja, nur eine Datei.",
    m = "Ja, mehrere Dateien möglich."
  )

  inputHelp = div(class="help-block",HTML(
"<p>Schreiben Sie jede Eingabevariable in eine Zeile. Sie können verschiedene Eingabtypen definieren:<p>
<ul>
<li>Textfeld: Feldname = Beispieltext</li>
<li>Langer Text: Feldname+ = Beispieltext</li>
<li>Checkbox: Feldenname [ ]</li>
<li>Auswahl: Feldname [option1 | option2 | option3 | option4 ]</li>
</ul>
Klicken Sie auf das &#128065; für eine Vorschau.

Vorgegeben ist ein Beispiel.
"))

  descrHelp = helpText("Sie können hier einen kurzen Text schreiben, der erklärt wie die Rückmeldung gemacht werden soll. Falls Sie das schon in der Email erklärt haben, können Sie das Feld einfach leer lassen und es wird ein kurzer Standardtext gezeigt.")
  ui = tagList(div(id="new-job-4",
    h3("Schritt 4: Rückmeldung / Dateneingabe"),
    selectInput("upload_type","Soll Empfänger eine Datei hochladen?", swap.names.values(upload_descr),selected = job$upload_type),
    selectInput("input_type","Dateneingabe durch Empfänger", swap.names.values(input_descr),selected = job$input_type),
    div(id="input_fields_div", style="display: none;",
      textAreaInput2("input_fields_txt", "Definition der Eingabefelder", value=job$input_fields_txt,help=inputHelp,rows=4, area.style="white-space: nowrap;  overflow: auto;", label.extras=icon("eye", class = "preview-fields-icon blue-hover", lib = "font-awesome")),
      uiOutput("previewFieldsUI")
    ),
    textAreaInput2("response_descr", "Erklärungstext", value="",rows=2, area.style="white-space: nowrap;  overflow: auto;", help=descrHelp),
    simpleButton("jobcreate_prevbtn4","Zurück"),
    simpleButton("jobcreate_nextbtn4","Weiter")
  ))
  ui


}

jobui5_de = function(job,...) {
  form.ids = c("receivers", "sender","futher_givers","deadline", "tpl_title","tpl_body","response_type","input_type","upload_type","response_items_txt", "input_fields_txt")
  ui = tagList(div(id="new-job-5",
    h3("Schritt 5: Überprüfen und senden"),
    simpleButton("newJobTestEmailBtn","Versende Testemail", form.ids = form.ids),
    simpleButton("checkAndSendJobBtn","Erstelle und Sende Job", form.ids = form.ids),
    uiOutput("checkJobMsg"),
    simpleButton("jobcreate_prevbtn5","Zurück")
  ))
  ui
}


jobtype_ui_de = function() {
  jobtypes_descr = c(
    so = "Auftrag den Empfänger bestätigen soll sobald erledigt.",
    mo = "Auftrag mit mehreren Punkten den Empfänger separat bestätigen soll, wenn erledigt.",
    si = "Empfänger soll online einen Datensatz eingeben.",
    mi = "Empfänger soll online einen oder mehrere Datensätze eingeben.",
    xi = "Empfänger soll einen oder mehrere Datensätze in eine vorgegebene Exceltabelle eintragen und hochladen.",
    no = "Nur eine Nachricht ohne Rückmeldung der Empfänger."
  )
  choices = swap.names.values(jobtypes_descr)
  ui= tagList(
    radioButtons("jobtype",label = "Wählen Sie die Art des neuen Jobs", choices = choices,width = "100%")
  )

  ui = tagList(
    h4("Kein Dateneingabe"),
    simpleButton("tasktype_so","Nutzer bestätigt wenn fertig."),
    simpleButton("tasktype_so","Nutzer bestätigt einzelne Punkte."),
    simpleButton("tasktype_no","Nur Nachricht ohne Rückmeldung."),
    h4("Nutzer geben Daten ein"),
    simpleButton("tasktype_so","Nur ein Datensatz."),
    simpleButton("tasktype_so","Mehrere Datensätze."),
    simpleButton("tasktype_no","Exceltabelle.")
  )
  ui
}

show.job.create.step = function(step = 1, app=getApp()) {
  panel = paste0("step", step)
  updateNavlistPanel(app$session, "createJobPanel", panel)
}

check_input_fields_txt_de = function(txt) {
  restore.point("check.input.items.txt")
  txt = sep.lines(txt)
  txt = trimws(txt)
  txt = txt[txt!=""]
  head = "Es gibt einen Fehler in Schritt 4 bei der Spezifikation der Eingabevariablen:"

  if (length(txt)==0) {
    check.job.error(step=4, var="input_fields_txt", msg="Sie haben keine Eingabevariablen angegeben.", head=head)
  }
  no.eq = which(!has.substr(txt,"="))
  if (length(no.eq)>0) {
    check.job.error(step=4, var="input_fields_txt", msg="Nicht alle Zeilen haben ein Gleichheitszeichen (=). Achten Sie auf das Format für jede Zeile: Variablenamen = Beispielwert.", head=head)
  }

  var.name = trimws(str.left.of(txt, "="))
  if (any(var.name=="")) {
    check.job.error(step=4, var="input_fields_txt", msg="Nicht alle Zeilen der Eingabevariablen haben ein Gleichheitszeichen (=). Achten Sie auf das Format für jede Zeile: Variablenamen = Beispielwert.",head=head)
  }

  dupl = unique(var.name[duplicated(var.name)])
  if (length(dupl)>0) {
    check.job.error(step=4, var="input_fields_txt", msg=paste0("Sie haben die Variable(n) ", paste0(dupl, collapse=", "), " mehrfach angegeben."), head=head)
  }

  if (length(var.name) > 100) {
    check.job.error(step=4, var="input_fields_txt", msg="Sie können maximal 100 Variablen eintragen lassen.", head=head)
  }
  #example = trimws(str.right.of(txt, "="))
  return(merge.lines(trimws(txt)))
}
