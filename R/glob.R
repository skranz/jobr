get.glob = function() {
  app=getApp()
  if (!is.null(app)) return(app$glob)
  glob = getOption("jobr.glob")
  if (is.null(glob)) {
    glob = new.env()
    options(jobr.glob = glob)
  }
  glob
}

init.jobr.glob = function(main.dir = getwd(), glob = get.glob()) {
  glob$main.dir = main.dir()

}

get.placeholder.info = function(glob=get.glob()) {
  if (!is.null(glob$place.holder.info))
    return(glob$place.holder.info)

}
