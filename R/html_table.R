MyDataTable= function(id, df, header=colnames(df), min.width="40em", class="stripe compact", ...) {
 extra = paste0("class = '",class,"' style='width: 100%; min-width: ", min.width,"'")
 html = simple_html_table(id, df, header, extra=extra)
 tagList(
   html,
   DataTableScript(id, ...)
 )
}

simple_html_table = function(id="",df,header=colnames(df), extra=NULL) {
  restore.point("simple_html_table")
  str = paste.df.cols(df,sep="</td><td>")
  inner.tab = paste0("<tr><td>",str,"</td></tr>", collapse="\n")
  str = paste0(header, collapse="</th><th>")
  head.tab = paste0("<tr><th>",str,"</th></tr>", collapse="\n")

  tab = paste0('<table', if(!is.null(id)) paste0(' id="',id,'" '),extra,'>\n<thead>\n', head.tab,'\n</thead>\n<tbody>\n',inner.tab,'\n</tbody>\n</table>')
  HTML(tab)
}

DataTableScript = function(id,paging=FALSE,scrollY="12em", scrollX=TRUE, searching=FALSE, info=FALSE,...) {
  options = list(paging=paging, scrollY=scrollY, scrollX=scrollX, searching=searching, info=info, ...)
  js.opts = list2js(options)
  #js.opts = '{ paging: false, scrollY: "12em","scrollX": true, searching: true, info: false}'

  restore.point("DataTableScript")
  tags$script(paste0('$("#',id,'").DataTable(', js.opts,');'))
}

list2js = function(x) {
  x = lapply(x, function(el) {
    if (is.logical(el)) return(tolower(as.character(el)))
    if (is.character(el)) return(paste0("'",el,"'"))
    el
  })
  paste0("{",paste0(names(x),": ", x, collapse=", "),"}")
}
