parse_input_txt = function(txt) {
  restore.point("parse_input_txt")
  txt = sep.lines(txt)
  txt = trimws(txt)
  txt = txt[txt!=""]
  head = "Es gibt einen Fehler in Schritt 4 bei der Spezifikation der Eingabevariablen:"



  if (length(txt)==0) {
    check.job.error(step=4, var="input_fields_txt", msg="Sie haben keine Eingabevariablen angegeben.", head=head)
  }
  if (length(txt) > 100) {
    check.job.error(step=4, var="input_fields_txt", msg="Sie können maximal 100 Felder eintragen lassen.", head=head)
  }

  has.eq = has.substr(txt,"=")
  has.bracket = has.substr(txt, "[") & has.substr(txt, "]")

  if (any(has.eq & has.bracket)) {
    check.job.error(step=4, var="input_fields_txt", msg="Sie können entweder ein Gleichheitszeichen (=) oder eckige Klammern [ ] für eine Felddefinition nutzen, aber nicht beides.",head=head)
  }

  fields = lapply(seq_along(txt), function(i) {
    str = txt[i]
    if (has.eq[i]) {
      var = str.left.of(str,"=") %>% trimws()
      val = str.right.of(str,"=") %>% trimws()
      long = endsWith(var,"+")
      if (long) var = substring(var,1,nchar(var)-1) %>% trimws()
      list(var=var, val=val, type=ifelse(long,"longtext","text"))
    } else if (!has.bracket[i]) {
      var = trimws(str)
      long = endsWith(var,"+")
      if (long) var = substring(var,1,nchar(var)-1) %>% trimws()
      list(var=var, val="", type=ifelse(long,"longtext","text"))
    } else {
      var = str.left.of(str,"[") %>% trimws()
      content = str.between(str,"[","]") %>% trimws()
      if (content == "") {
        list(var=var, val="FALSE", type="checkbox")
      } else {
        choices = strsplit(content,"|",fixed = TRUE)[[1]] %>%trimws()
        list(var=var, val=NA, choices=choices, type="select")
      }
    }
  })
  fields
}

input_field_ui = function(field,i=1, id=paste0("input_",i), value=NULL) {
  restore.point("input_field_ui")
  if (is.null(value))
    value=field$val
  if (field$type == "text") {
    textInput(id,label = field$var,value = value, width = "100%")
  } else if (field$type == "longtext") {
    textAreaInput2(id,label = field$var,value = value, width = "100%")
  } else if (field$type == "checkbox") {
    checkboxInput(id,label = field$var,value = as.logical(value))
  } else if (field$type == "select") {
    selectInput(id,label = field$var,selected = value,choices = field$choices)
  }
}

input_fields_ui = function(fields, values=NULL) {
  restore.point("input_fields_ui")
  if (length(values)==0) values = NULL
  uis = lapply(seq_along(fields), function(i) {
    input_field_ui(fields[[i]],i=i, value=values[[i]])
  })
}

