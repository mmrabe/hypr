
#' @importFrom MASS fractions
#' @importFrom pracma gcd Lcm


setClass("expr_num")
.as.character.expr_num <- function(object) if(is(object, "expr_frac")) .as.character.expr_frac(object) else if(is(object, "expr_real")) .as.character.expr_real(object) else as.character(object)

setClass("expr_frac", slots = c(num = "numeric", den = "numeric"), prototype = list(num = 1, den = 1), contains = "expr_num")
.as.character.expr_frac <- function(object) if(object@den!=1L) sprintf("%.0f/%.0f", object@num, object@den) else as.character(object@num)
#setMethod("show", "expr_frac", show.expr_frac)

setClass("expr_real", slots = c(num = "numeric"), prototype = list(num = 1), contains = "expr_num")
.as.character.expr_real <- function(object) as.character(object@num)
#setMethod("show", "expr_real", show.expr_real)

setClass("expr_coef", slots = c(num = "expr_num", var = "character"))
.as.character.expr_coef <- function(object) {
  ret <- character(0)
  if(.as.numeric.expr_num(object@num) != 1 || length(object@var) == 0) {
    if(.as.numeric.expr_num(object@num) == -1) {
      ret <- c(ret, "-")
    } else {
      ret <- c(ret, .as.character.expr_num(object@num))
      if(length(object@var) > 0) {
        ret <- c(ret, "*")
      }
    }
  }
  ret <- c(ret, paste(object@var, collapse = "*"))
  paste(ret, collapse="")
}
#setMethod("show", "expr_coef", show.expr_coef)
.as.expression.expr_coef <- function(x, ...) {
  if(is(x@num, "expr_real")) {
    ret <- x@num@num
  } else if(is(x@num, "expr_frac")) {
    if(x@num@den != 1) {
      ret <- call("/", as.double(x@num@num), as.double(x@num@den))
    } else {
      ret <- as.double(x@num@num)
    }
  }
  if(length(x@var) > 0) {
    var_calls <- as.symbol(x@var[1])
    for(var in x@var[-1]) {
      var_calls <- call("*", var_calls, as.symbol(var))
    }
    if(.as.numeric.expr_num(x@num) == 1) {
      ret <- var_calls
    } else if(.as.numeric.expr_num(x@num) == -1) {
      ret <- call("-", var_calls)
    } else {
      ret <- call("*", ret, var_calls)
    }
  }
  ret
}
#setMethod("as.expression", "expr_coef", .as.expression.expr_coef)

setClass("expr_sum", contains = "list", slots = c(num = "expr_num"), prototype = prototype(num = new("expr_frac", num=1L, den=1L)))
.as.character.expr_sum <- function(object) {
  if(length(object) > 0) {
    # reformat
    if(length(object) > 1) {
      if(all(vapply(object, \(x) is(x@num, "expr_frac"), logical(1)))) {
        den_gcd <- gcd(object[[1]]@num@den, object[[2]]@num@den)
        for(i in seq_along(object)[c(-1,-2)]) {
          den_gcd <- gcd(den_gcd, object[[i]]@num@den)
        }
        num_gcd <- gcd(object[[1]]@num@num, object[[2]]@num@num)
        for(i in seq_along(object)[c(-1,-2)]) {
          num_gcd <- gcd(num_gcd, object[[i]]@num@num)
        }
        for(i in seq_along(object)) {
          object[[i]]@num@num <- object[[i]]@num@num / num_gcd
          object[[i]]@num@den <- object[[i]]@num@den / den_gcd
        }
        object@num <- new("expr_frac", num = num_gcd, den = den_gcd)
      }
    }

    ret <- character(0)
    for(i in seq_along(object)) {
      if(i>1) {
        if(.as.numeric.expr_num(object[[i]]@num) >= 0) {
          ret <- c(ret, "+", .as.character.expr_coef(object[[i]]))
        } else {
          ret <- c(ret, "-", .as.character.expr_coef(multiply_expr(object[[i]], new("expr_coef", num = new("expr_frac", num = -1L)))))
        }
      } else {
        ret <- c(ret, .as.character.expr_coef(object[[i]]))
      }
    }
    if(.as.numeric.expr_num(object@num) != 1) {
      if(is(object@num, "expr_frac") && object@num@num == 1) paste0("(", paste(ret, collapse=" "), ")", "/", object@num@den)
      else if(is(object@num, "expr_frac") && object@num@den == 1) paste0(object@num, "*", "(", paste(ret, collapse=" "), ")")
      else paste0(.as.character.expr_num(object@num), "*", "(", paste(ret, collapse=" "), ")")
    }
    else paste(ret, collapse=" ")
  } else {
    "0"
  }
}
#setMethod("show", "expr_sum", show.expr_sum)

.as.formula.expr_sum <- function(object, env = parent.frame()) {
  if(length(object) == 0) {
    return(~0)
  }
  ret <- .as.expression.expr_coef(object[[1]])
  for(el in object[-1]) {
    if(.as.numeric.expr_num(el@num) == 0) {
      next
    } else if(.as.numeric.expr_num(el@num) < 0) {
      ret <- call("-", ret, .as.expression.expr_coef(multiply_expr(el, new("expr_coef", num = new("expr_frac", num = -1L)))))
    } else {
      ret <- call("+", ret, .as.expression.expr_coef(el))
    }
  }
  call("~", ret)
}


.as.numeric.expr_num <- function(x) {
  if(is(x, "expr_frac")) x@num/x@den
  else if(is(x, "expr_real")) x@num
  else as.numeric(x)
}

.as.fractions.expr_num <- function(x) {
  if(is(x, "expr_frac")) MASS::as.fractions(x@num)/MASS::as.fractions(x@den)
  else MASS::as.fractions(x@num)
}

#setMethod("as.numeric", signature(x="expr_num"), .as.numeric.expr_num)
#setMethod("as.fractions", signature(x="expr_num"), .as.fractions.expr_num)

`*.expr_num` <- function(a, b) {
  if(is(a, "expr_frac") && is(b, "expr_frac")) {
    simplify.expr_num(new("expr_frac", num = a@num*b@num, den = a@den*b@den))
  } else {
    new("expr_real", num = .as.numeric.expr_num(a)*.as.numeric.expr_num(b))
  }
}
#setMethod("*", signature("expr_num", "expr_num"), function(e1, e2) `*.expr_num`(e1, e2))

`/.expr_num` <- function(a, b) {
  b <- if(is(b, "expr_frac")) new("expr_frac", num = b@den, den = b@num) else new("expr_real", num = 1/.as.numeric.expr_num(b))
  `*.expr_num`(a, b)
}
#setMethod("/", signature("expr_num", "expr_num"), function(e1, e2) `/.expr_num`(e1, e2))

`+.expr_num` <- function(a, b) {
  if(is(a, "expr_frac") && is(b, "expr_frac")) {
    if(a@den!=b@den) {
      lcm <- Lcm(a@den, b@den)
      simplify.expr_num(new("expr_frac", num = a@num * lcm %/% a@den +  b@num * lcm %/% b@den, den = lcm))
    } else {
      simplify.expr_num(new("expr_frac", num = a@num+b@num, den = a@den))
    }
  } else {
    new("expr_real", num = .as.numeric.expr_num(a)+.as.numeric.expr_num(b))
  }
}
#setMethod("+", signature("expr_num", "expr_num"), function(e1, e2) `+.expr_num`(e1, e2))

`-.expr_num` <- function(a, b) {
  b <- if(is(b, "expr_frac")) new("expr_frac", num = -b@num, den = b@den) else new("expr_real", num = -.as.numeric.expr_num(b@num))
  `+.expr_num`(a, b)
}


simplify.expr_num <- function(a) {
  if(is(a, "expr_frac")) {
    div <- gcd(a@num, a@den)
    if(a@den %/% div > 1000) {
      a <- new("expr_real", num = a@num/a@den)
    } else {
      a@num <- a@num %/% div
      a@den <- a@den %/% div
    }
  }
  a
}

#setMethod("-", signature("expr_num", "expr_num"), function(e1, e2) `-.expr_num`(e1, e2))
#setMethod("-", signature("expr_num"), function(e1) new("expr_frac", num=0L, den=1L) - e1)

`==.expr_num` <- function(a, b) .as.numeric.expr_num(a) == .as.numeric.expr_num(b)
#setMethod("==", signature("expr_num", "expr_num"), function(e1, e2) `==.expr_num`(e1, e2))
`<.expr_num` <- function(a, b) .as.numeric.expr_num(a) < .as.numeric.expr_num(b)
#setMethod("<", signature("expr_num", "expr_num"), function(e1, e2) `<.expr_num`(e1, e2))
#setMethod("<=", signature("expr_num", "expr_num"), function(e1, e2) `<.expr_num`(e1, e2) || `==.expr_num`(e1, e2))
`>.expr_num` <- function(a, b) .as.numeric.expr_num(a) > .as.numeric.expr_num(b)
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
  return(as(ret[vapply(ret, function(x) .as.numeric.expr_num(x@num) != 0, logical(1))], "expr_sum"))
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
    return(as(ret[vapply(ret, function(x) .as.numeric.expr_num(x@num) != 0, logical(1))], "expr_sum"))
  } else if(is.numeric(expr)) {
    if(expr %% 1 == 0) {
      num <- new("expr_frac", num = expr, den = 1L)
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
          num <- new("expr_frac", num = round(expr[[2]]), den = round(expr[[3]]))
        } else {
          num <- new("expr_real", num = expr[[2]]/expr[[3]])
        }
        return(new("expr_coef", num = num))
      } else {
        lh <- simplify_expr(expr[[2]])
        if(expr[[3]] %% 1 == 0) {
          num <- new("expr_frac", num = 1L, den = expr[[3]])
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
      } else if(is(lh, "expr_sum") && is(rh, "expr_sum")) {
        ret <- as(c(lh, rh), "expr_sum")
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
