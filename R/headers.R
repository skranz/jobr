jobr.header = function(just.task = FALSE) {
  shiny::addResourcePath("jobr",system.file("www", package="jobr"))

  header = tags$head(HTML('
<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.24/css/jquery.dataTables.css">
<script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.24/js/jquery.dataTables.js"></script>'))
  header
}
