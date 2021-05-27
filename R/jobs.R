items_txt_to_list = function(txt) {
  txt = sep.lines(txt)
  txt = trimws(txt)
  txt = txt[txt!=""]
  vars = trimws(str.left.of(txt, "="))
  vals = str.right.of(txt, "=") %>% trimws()
  li = as.list(vals)
  names(li) = vars
  li
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

