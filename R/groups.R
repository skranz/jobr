example.groups = function() {
  setwd("C:/libraries/jobr")
  app = eventsApp()
  get.groups()
  app$ui = fluidPage(
    show.address.book()
  )
  #show.address.book("addressBookUI")
  viewApp(app)
}



get.groups = function(glob=get.glob(), db=get.jobdb()) {
  if (is.null(glob[["groups"]])) {
    glob$groups = dbGet(db,"groups")
  }
  glob$groups
}

show.address.book = function(containerID=NULL) {
  restore.point("show.adress.book")
  groups = get.groups()

  inner.tab = paste0("<tr><td>", groups$groupname, "</td><td>",
    groups$members, "</td></tr>", collapse="\n")
  tab = paste0("<table class='address-table'><tr><th>Gruppe</th><th>Mitglieder</th></tr>",inner.tab,"</table>")

  ui = div(
    h5("Gruppen"),
    HTML(tab)
  )
  if (!is.null(containerID)) {
    setUI(containerID, ui)
    dsetUI(containerID, ui)
  }
  invisible(ui)

}
