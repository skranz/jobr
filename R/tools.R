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
