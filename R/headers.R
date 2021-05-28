jobr.header = function(just.task = FALSE) {
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))

  dep = attr(icon("test"),"html_dependencies")

  header = tags$head(
    dep,
  HTML('
<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.24/css/jquery.dataTables.css">
<script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.24/js/jquery.dataTables.js"></script>'),
  htmlDependency("font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny", stylesheet = c("css/all.min.css", "css/v4-shims.min.css"))

  )

  header
}
