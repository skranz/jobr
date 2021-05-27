field.sep = function() "°|°"

swap.names.values = function(x) {
  res = names(x)
  names(res) = x
  res
}

remove.null = function(li) {
  if (length(li)==0) return(li)
  nu = sapply(li, is.null)
  li[!nu]
}

find.unique.placeholders = function(txt, keep.outer=TRUE) {
  #restore.point("find.unique.placeholders")
  pos = str.blocks.pos(txt, "{", "}")
  if (NROW(pos$inner)==0) return(NULL)
  if (keep.outer) {
    ph = str.at.pos(txt,pos$outer)
  } else {
    ph = str.at.pos(txt,pos$inner)
  }
  unique(ph)
}

random.string = function(n=1, nchar=16) {
  if (n == 1) {
    paste0(c(sample(c(LETTERS,letters),1,TRUE),sample(c(LETTERS,letters,0:9), nchar-1, TRUE)), collapse="")
  } else {
    unlist(replicate(n,paste0(sample(c(LETTERS,letters), nchar, TRUE),collapse="")))
  }
}

has.placeholder = function(txt) {
 has.substr(txt, "{") & has.substr(txt, "}")
}

is.empty = function(x) {
  if (is.null(x)) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  if (identical(x,"")) return(TRUE)
  FALSE
}

timedMessage = function (id, msg = "", html = msg, ui = HTML(html), millis = 3000,empty.msg = "", empty.ui = HTML(empty.msg), app = getApp()) {
    restore.point("timedMessage")
    try({
        setUI(id, ui)
        dsetUI(id, ui)
    })
    obs.id = paste0("..timedMessage..", id)
    flag.id = paste0("flag", obs.id)
    app[[flag.id]] = FALSE
    if (!is.null(app[[obs.id]]))
        try(app[[obs.id]]$destroy())
    if (!is.finite(millis))
        return()
    app[[obs.id]] = observe({
        if (!isTRUE(app[[flag.id]])) {
            app[[flag.id]] = TRUE
            invalidateLater(millis)
            return()
        }
        try(app[[obs.id]]$destroy())
        try({
            setUI(id, empty.ui)
            dsetUI(id, empty.ui)
        })
    })
}

paste.df.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]], ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],sep=sep, ...))
    }
  } else if (NROW(cols) == 3) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      sep2 = ifelse(!empty.sep | nchar(mat[[cols[2]]])>0 | nchar(mat[[cols[3]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]],sep2, mat[[cols[3]]],
          ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],mat[[cols[3]]],sep=sep, ...))
    }
  } else {
      if (is.character(cols))
        cols = match(cols, colnames(mat))
      code = paste("mat[[", cols, "]]", collapse = ",sep,")
      code = paste("paste0(", code, ",...)", sep = "")
      return(eval(parse(text = code)))
  }
}
