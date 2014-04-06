kLPar <- '('
kRPar <- ')'
kQuote <- "'"
kNil <- list('tag' = 'nil', 'data' = 'nil')

safeCar <- function(obj) {
  if (obj[['tag']] == 'cons') {
    return(obj[['car']]())
  }
  return(kNil)
}

safeCdr <- function(obj) {
  if (obj[['tag']] == 'cons') {
    return(obj[['cdr']]())
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
  car <- a  # Avoid lazy evaluation.
  cdr <- d  # Avoid lazy evaluation.
  ret <- list('tag' = 'cons')
  ret[['car']] <- function() { return(car) }
  ret[['cdr']] <- function() { return(cdr) }
  ret[['set_car']] <- function(x) { car <<- x }
  ret[['set_cdr']] <- function(x) { cdr <<- x }
  return(ret)
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

nreverse <- function(lst) {
  ret <- kNil
  while (lst[['tag']] == 'cons') {
    tmp <- lst[['cdr']]()
    lst[['set_cdr']](ret)
    ret <- lst
    lst <- tmp
  }
  return(ret)
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
    return(readList(substr(str, 2, nchar(str))))
  } else if (substr(str, 1, 1) == kQuote) {
    tmp <- read(substr(str, 2, nchar(str)))
    return(list(makeCons(makeSym('quote'), makeCons(tmp[[1]], kNil)), tmp[[2]]))
  } else {
    return(readAtom(str))
  }
}

readList <- function(str) {
  ret <- kNil
  repeat {
    str <- skipSpaces(str)
    if (str == '') {
      return(list(makeError('unfinished parenthesis'), ''))
    } else if (substr(str, 1, 1) == kRPar) {
      break
    }
    tmp <- read(str)
    elm <- tmp[[1]]
    nxt <- tmp[[2]]
    if (elm[['tag']] == 'error') {
      return(list(elm, ''))
    }
    ret <- makeCons(elm, ret)
    str <- nxt
  }
  return(list(nreverse(ret), substr(str, 2, nchar(str))))
}

printObj <- function (obj) {
  if (obj[['tag']] == 'num' || obj[['tag']] == 'sym' || obj[['tag']] == 'nil') {
    return(paste(obj[['data']]))
  } else if (obj[['tag']] == 'error') {
    return(paste('<error:', obj[['data']], '>'))
  } else if (obj[['tag']] == 'cons') {
    return(printList(obj))
  } else if (obj[['tag']] == 'subr') {
    return('<subr>')
  } else if (obj[['tag']] == 'expr') {
    return('<expr>')
  }
  return('<unknown>')
}

printList <- function(obj) {
  ret <- ''
  first = TRUE
  while (obj[['tag']] == 'cons') {
    if (first) {
      ret <- printObj(obj[['car']]())
      first <- FALSE
    } else {
      ret <- paste(ret, printObj(obj[['car']]()))
    }
    obj <- obj[['cdr']]()
  }
  if (obj[['tag']] == 'nil') {
    return(paste('(', ret, ')', sep=''))
  }
  return(paste('(', ret, ' . ', printObj(obj), ')', sep=''))
}

findVar <- function(sym, env) {
  while (env[['tag']] == 'cons') {
    alist <- env[['car']]()
    while (alist[['tag']] == 'cons') {
      if (identical(alist[['car']]()[['car']](), sym)) {
        return(alist[['car']]())
      }
      alist <- alist[['cdr']]()
    }
    env <- env[['cdr']]()
  }
  return(kNil)
}

addToEnv <- function(sym, val, env) {
  env[['set_car']](makeCons(makeCons(sym, val), env[['car']]()))
}

g_env = makeCons(kNil, kNil)

eval1 <- function(obj, env) {
  if (obj[['tag']] == 'nil' || obj[['tag']] == 'num' ||
      obj[['tag']] == 'error') {
    return(obj)
  } else if (obj[['tag']] == 'sym') {
    bind <- findVar(obj, env)
    if (identical(bind, kNil)) {
      return(makeError(paste(obj[['data']], 'has no value')))
    }
    return(bind[['cdr']]())
  }
}

addToEnv(makeSym('t'), makeSym('t'), g_env)

con <- file(description='stdin', open='r')
cat('> ')
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  tmp <- read(line)
  cat(paste(printObj(eval1(tmp[[1]], g_env)), '\n', sep=''))
  cat('> ')
}
close(con)
