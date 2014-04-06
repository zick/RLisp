kLPar <- '('
kRPar <- ')'
kQuote <- "'"
kNil <- list('tag' = 'nil', 'data' = 'nil')

safeCar <- function(obj) {
  if (obj[['tag']] == 'cons') {
    return(obj[['car']])
  }
  return(kNil)
}

safeCdr <- function(obj) {
  if (obj[['tag']] == 'cons') {
    return(obj[['cdr']])
  }
  return(kNil)
}

makeError <- function(str) {
  return(list('tag' = 'error', 'data' = str))
}

sym_table <- list()
makeSym <- function(str) {
  if (str == 'nil') {
    return(kNil)
  } else if (is.null(sym_table[[str]])) {
    sym_table[[str]] <<- list('tag' = 'sym', 'data' = str)
  }
  return(sym_table[[str]])
}

makeNum <- function(num) {
  return(list('tag' = 'num', 'data' = num))
}

makeCons <- function(a, d) {
  return(list('tag' = 'cons', 'car' = a, 'cdr' = d))
}

makeSubr <- function(fn) {
  return(list('tag' = 'subr', 'data' = fn))
}

makeExpr <- function(args, env) {
  return(list('tag' = 'expr',
              'args' = safeCar(args),
              'body' = safeCdr(args),
              'env' = env))
}

isSpace <- function(c) {
  return(c == ' ' || c == '\t' || c == '\r' || c == '\n')
}

isDelimiter <- function(c) {
  return(c == kLPar || c == kRPar || c == kQuote || isSpace(c))
}

skipSpaces <- function(str) {
  for (i in 1:nchar(str)) {
    if (!isSpace(substr(str, i, i))) {
      return(substr(str, i, nchar(str)))
    }
  }
  return('')
}

makeNumOrSym <- function(str) {
  if (length(grep('^[+-]?\\d+$', str))) {
    return(makeNum(as.numeric(str)))
  }
  return(makeSym(str))
}

readAtom <- function(str) {
  nxt <- ''
  for (i in 1:nchar(str)) {
    if (isDelimiter(substr(str, i, i))) {
      nxt = substr(str, i, nchar(str))
      str <- substr(str, 1, i - 1)
      break
    }
  }
  return(list(makeNumOrSym(str), nxt))
}

read <- function(str) {
  str <- skipSpaces(str)
  if (str == '') {
    return(list(makeError('empty input'), ''))
  } else if (substr(str, 1, 1) == kRPar) {
    return(list(makeError(paste('invalid synta: ', str)), ''))
  } else if (substr(str, 1, 1) == kLPar) {
    return(list(makeError('noimpl'), ''))
  } else if (substr(str, 1, 1) == kQuote) {
    return(list(makeError('noimpl'), ''))
  } else {
    return(readAtom(str))
  }
}

con <- file(description='stdin', open='r')
cat('> ')
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  cat(paste(read(line), '\n'))
  cat('> ')
}
close(con)
