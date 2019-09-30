
setClass("expr_num")
expr_num <- function(object) if(is(object, "expr_frac")) show.expr_frac(object) else if(is(object, "expr_real")) show.expr_real(object) else show(object)

setClass("expr_frac", slots = c(num = "integer", den = "integer"), prototype = list(num = 0L, den = 1L), contains = "expr_num")
show.expr_frac <- function(object) if(object@den!=1L) cat(sprintf("%d/%d", object@num, object@den)) else cat(object@num)
#setMethod("show", "expr_frac", show.expr_frac)

setClass("expr_real", slots = c(num = "numeric"), prototype = list(num = 0), contains = "expr_num")
show.expr_real <- function(object) cat(object@num)
#setMethod("show", "expr_real", show.expr_real)

setClass("expr_coef", slots = c(num = "expr_num", var = "character"))
show.expr_coef <- function(object) {
  if(as.numeric.expr_num(object@num) != 1 || length(object@var) == 0) {
    if(as.numeric.expr_num(object@num) == -1) {
      cat("-")
    } else {
      show(object@num)
      if(length(object@var) > 0) {
        cat("*")
      }
    }
  }
  cat(paste(object@var, collapse = "*"))
}
#setMethod("show", "expr_coef", show.expr_coef)

setClass("expr_sum", contains = "list")
show.expr_sum <- function(object) {
  if(length(object) > 0) {
    for(i in seq_along(object)) {
      if(i>1) {
        if(as.numeric.expr_num(object[[i]]@num) >= 0) {
          cat(" + ")
          show.expr_coef(object[[i]])
        } else {
          cat(" - ")
          show.expr_coef(multiply_expr(object[[i]], new("expr_coef", num = new("expr_frac", num = -1L))))
        }
      } else {
        show.expr_coef(object[[i]])
      }
    }
  } else {
    cat("0")
  }
}
#setMethod("show", "expr_sum", show.expr_sum)


as.numeric.expr_num <- function(x) {
  if(is(x, "expr_frac")) x@num/x@den
  else if(is(x, "expr_real")) x@num
  else as.numeric(x)
}

as.fractions.expr_num <- function(x) {
  if(is(x, "expr_frac")) MASS::as.fractions(x@num)/MASS::as.fractions(x@den)
  else MASS::as.fractions(x@num)
}

#setMethod("as.numeric", signature(x="expr_num"), as.numeric.expr_num)
#setMethod("as.fractions", signature(x="expr_num"), as.fractions.expr_num)

`*.expr_num` <- function(a, b) {
  if(is(a, "expr_frac") && is(b, "expr_frac")) {
    new("expr_frac", num = a@num*b@num, den = a@den*b@den)
  } else {
    new("expr_real", num = as.numeric.expr_num(a)*as.numeric.expr_num(b))
  }
}
#setMethod("*", signature("expr_num", "expr_num"), function(e1, e2) `*.expr_num`(e1, e2))

`/.expr_num` <- function(a, b) {
  b <- if(is(b, "expr_frac")) new("expr_frac", num = b@den, den = b@num) else new("expr_real", num = 1/as.numeric.expr_num(b))
  `*.expr_num`(a, b)
}
#setMethod("/", signature("expr_num", "expr_num"), function(e1, e2) `/.expr_num`(e1, e2))

`+.expr_num` <- function(a, b) {
  if(is(a, "expr_frac") && is(b, "expr_frac")) {
    if(a@den!=b@den) {
      lcm <- as.integer(pracma::Lcm(a@den, b@den))
      new("expr_frac", num = a@num * lcm %/% a@den +  b@num * lcm %/% b@den, den = lcm)
    } else {
      new("expr_frac", num = a@num+b@num, den = a@den)
    }
  } else {
    new("expr_real", num = as.numeric.expr_num(a)+as.numeric.expr_num(b))
  }
}
#setMethod("+", signature("expr_num", "expr_num"), function(e1, e2) `+.expr_num`(e1, e2))

`-.expr_num` <- function(a, b) {
  b <- if(is(b, "expr_frac")) new("expr_frac", num = -b@num, den = b@den) else new("expr_real", num = -as.numeric.expr_num(b@num))
  `+.expr_num`(a, b)
}
#setMethod("-", signature("expr_num", "expr_num"), function(e1, e2) `-.expr_num`(e1, e2))
#setMethod("-", signature("expr_num"), function(e1) new("expr_frac", num=0L, den=1L) - e1)

`==.expr_num` <- function(a, b) as.numeric.expr_num(a) == as.numeric.expr_num(b)
#setMethod("==", signature("expr_num", "expr_num"), function(e1, e2) `==.expr_num`(e1, e2))
`<.expr_num` <- function(a, b) as.numeric.expr_num(a) < as.numeric.expr_num(b)
#setMethod("<", signature("expr_num", "expr_num"), function(e1, e2) `<.expr_num`(e1, e2))
#setMethod("<=", signature("expr_num", "expr_num"), function(e1, e2) `<.expr_num`(e1, e2) || `==.expr_num`(e1, e2))
`>.expr_num` <- function(a, b) as.numeric.expr_num(a) > as.numeric.expr_num(b)
#setMethod(">", signature("expr_num", "expr_num"), function(e1, e2) `>.expr_num`(e1, e2))
#setMethod(">=", signature("expr_num", "expr_num"), function(e1, e2) `>.expr_num`(e1, e2) || `==.expr_num`(e1, e2))

multiply_expr <- function(rh, mult) {
  if(is(mult, "expr_coef")) {
    if(is(rh, "expr_coef")) {
      rh@num <- `*.expr_num`(mult@num, rh@num)
      rh@var <- sort(c(mult@var, rh@var))
      return(rh)
    } else if(is(rh, "expr_sum")) {
      for(i in seq_along(rh)) {
        rh[[i]]@num <- `*.expr_num`(mult@num, rh[[i]]@num)
        rh[[i]]@var <- sort(c(mult@var, rh[[i]]@var))
      }
      return(rh)
    }
  } else if(is(mult, "expr_sum")) {
    if(is(rh, "expr_coef")) {
      for(i in seq_along(mult)) {
        mult[[i]]@num <- `*.expr_num`(mult[[i]]@num, rh@num)
        mult[[i]]@var <- sort(c(mult[[i]]@var, rh@var))
      }
      return(mult)
    } else if(is(rh, "expr_sum")) {
      ret <- new("expr_sum")
      for(i in seq_along(rh)) {
        for(j in seq_along(mult)) {
          ret[[length(ret)+1]] <- multiply_expr(rh[[i]], mult[[j]])
        }
      }
      return(simplify_expr_sum(ret))
    }
  }
  stop(sprintf("Unknown case for `mult` (classes %s and %s)!", class(rh), class(mult)))
}

simplify_expr_sum <- function(expr) {
  ret <- list()
  for(el in expr) {
    if(el@num == 0) {
      next
    }
    found <- FALSE
    for(i in seq_along(ret)) {
      if(setequal_exact(ret[[i]]@var, el@var)) {
        f <- `+.expr_num`(ret[[i]]@num, el@num)
        ret[[i]]@num <- f
        found <- TRUE
        break
      }
    }
    if(!found) {
      ret[[length(ret)+1]] <- el
    }
  }
  return(as(ret[vapply(ret, function(x) as.numeric.expr_num(x@num) != 0, logical(1))], "expr_sum"))
}

setequal_exact <- function(x, y) {
  for(el in x) {
    i <- match(el, y)
    if(is.na(i)) {
      return(FALSE)
    } else {
      y <- y[-i]
    }
  }
  return(length(y) == 0L)
}

simplify_expr <- function(expr) {
  if(is.expression(expr)) {
    ret <- simplify_expr(expr[[1]])
    if(is(ret, "expr_num")) {
      ret <- new("expr_coef", num = ret)
    }
    if(is(ret, "expr_coef")) {
      ret <- as(list(ret), "expr_sum")
    }
    return(as(ret[vapply(ret, function(x) as.numeric.expr_num(x@num) != 0, logical(1))], "expr_sum"))
  } else if(is.numeric(expr)) {
    if(expr %% 1 == 0) {
      num <- new("expr_frac", num = as.integer(expr), den = 1L)
    } else {
      num <- new("expr_real", num = expr)
    }
    return(new("expr_coef", num = num))
  } else if(is.symbol(expr)) {
    return(new("expr_coef", num = new("expr_frac", num = 1L, den = 1L), var = as.character(expr)))
  } else if(is.call(expr)) {
    if(expr[[1]] == "-") {
      if(length(expr) == 3) {
        return(simplify_expr(call("+", expr[[2]], call("*", expr[[3]], -1L))))
      } else {
        return(simplify_expr(call("*", expr[[2]], -1L)))
      }
    } else if(expr[[1]] == "/") {
      if(!is.numeric(expr[[3]])) {
        stop("Denominators must be numeric!")
      }
      if(is.numeric(expr[[2]])) {
        if(expr[[2]] %% 1 == 0 && expr[[3]] %% 1 == 0) {
          num <- new("expr_frac", num = as.integer(expr[[2]]), den = as.integer(expr[[3]]))
        } else {
          num <- new("expr_real", num = expr[[2]]/expr[[3]])
        }
        return(new("expr_coef", num = num))
      } else {
        lh <- simplify_expr(expr[[2]])
        if(expr[[3]] %% 1 == 0) {
          num <- new("expr_frac", num = 1L, den = as.integer(expr[[3]]))
        } else {
          num <- new("expr_real", num = 1/expr[[3]])
        }
        return(multiply_expr(lh, new("expr_coef", num = num)))
      }
    } else if(expr[[1]] == "*") {
      lh <- simplify_expr(expr[[2]])
      rh <- simplify_expr(expr[[3]])
      return(multiply_expr(lh, rh))
    } else if(expr[[1]] == "(") {
      return(simplify_expr(expr[[2]]))
    } else if(expr[[1]] == "+") {
      lh <- simplify_expr(expr[[2]])
      rh <- simplify_expr(expr[[3]])
      if(is(lh, "expr_coef") && is(rh, "expr_coef")) {
        ret <- new("expr_sum")
        ret[[1]] <- lh
        ret[[2]] <- rh
      } else if(is(lh, "expr_coef") && is(rh, "expr_sum")) {
        ret <- as(c(list(lh), rh), "expr_sum")
      } else if(is(lh, "expr_sum") && is(rh, "expr_coef")) {
        ret <- as(c(lh, list(rh)), "expr_sum")
      } else {
        stop(sprintf("Unknown summands %s and %s!", class(lh), class(rh)))
      }
      return(simplify_expr_sum(ret))
    } else {
      stop(sprintf("Unknown function `%s`!", as.character(expr[[1]])))
    }
  } else {
    stop(sprintf("Unknown expression `%s`!", as.character(expr)))
  }
}
