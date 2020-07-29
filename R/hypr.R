#' @include equations.R
#' @importFrom methods as is new show
NULL


check_argument <- function(val, ...) {
  val <- tryCatch(val, error = function(e) e)
  if(is(val, "error")) stop(val$message, call. = FALSE)
  argname <- as.character(as.expression(match.call()$val))
  if(length(argname) > 1) stop("Must be single character")
  for(tst in list(...)) {
    if(is.numeric(tst) && is.vector(val)) {
      if(length(val) != tst) {
        stop(sprintf("`%s` has a length of %d but must have a length of %d.", argname, length(val), tst), call. = FALSE)
      }
    } else if(is.character(tst)) {
      classValid <- FALSE
      for(cls in tst) {
        if(cls == "numeric" && is.numeric(val)) {
          classValid <- TRUE
        } else if(grepl("^list:", cls)) {
          classValid <- is.list(val) && all(vapply(val, function(x) is(x, substring(cls, 6)), logical(1)))
        } else {
          classValid <- is(val, cls)
        }
        if(classValid) break
      }
      if(!classValid) {
        stop(sprintf("`%s` must be of type %s but is %s.", argname, paste(tst, collapse=","), paste(class(val), collapse=",")), call. = FALSE)
      }
    } else if(is.function(tst)) {
      if(!all(tst(val))) {
        stop(sprintf("`%s` has an invalid value.", argname), call. = FALSE)
      }
    } else if(is.expression(tst)) {
      if(!isTRUE(all(eval(tst, list(x = val))))) {
        test_string <- if(tst[[1]][[1]] == "<" && tst[[1]][[2]] == "x") {
          sprintf("be smaller than %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == "<=" && tst[[1]][[2]] == "x") {
          sprintf("be smaller than or equal to %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == ">" && tst[[1]][[2]] == "x") {
          sprintf("be greater than %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == ">=" && tst[[1]][[2]] == "x") {
          sprintf("be greater than or equal to %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == "==" && tst[[1]][[2]] == "x") {
          sprintf("be equal to %s", as.character(tst[[1]][[3]]))
        } else {
          sprintf("satisfy %s", as.character(tst))
        }
        stop(sprintf("`%s` must %s!", argname, test_string), call. = FALSE)
      }
    }
  }
}


#' S4 class “hypr” and its methods
#'
#' A \code{hypr} object contains equations, a hypothesis matrix and a contrast matrix, all of which are related to each other. See below for methods.
#'
#' To generate a hypr object, use the \code{\link[hypr:hypr]{hypr}} function.
#'
#' @param object,x a hypr object
#' @param value New value (\code{list} of equations for \code{formula}, \code{character} vector for \code{levels} and \code{names})
#' @param ... (ignored)
#'
#' @slot eqs List of null hypotheses
#' @slot hmat Hypothesis matrix
#' @slot cmat Contrast matrix
#'
#' @examples
#' # Equations and matrices in a hypr object are always congruent
#' # Therefore creating a hypr object h and then copying ...
#' h <- hypr(mu1~0, mu2~mu1)
#'
#' # ... its equations, ...
#' h2 <- hypr()
#' formula(h2) <- formula(h)
#'
#' # ... its hypothesis matrix, ...
#' h3 <- hypr()
#' hmat(h3) <- hmat(h)
#'
#' # ... or its contast matrix ...
#' h4 <- hypr()
#' cmat(h4) <- cmat(h)
#'
#' # ... over to another hypr object is the same as copying the object:
#' h5 <- h
#'
#' # check that hypr objects are equal by comparing hmat() and cmat()
#' stopifnot(all.equal(hmat(h), hmat(h2)))
#' stopifnot(all.equal(cmat(h), cmat(h2)))
#' stopifnot(all.equal(hmat(h), hmat(h3)))
#' stopifnot(all.equal(cmat(h), cmat(h3)))
#' stopifnot(all.equal(hmat(h), hmat(h4)))
#' stopifnot(all.equal(cmat(h), cmat(h4)))
#' stopifnot(all.equal(hmat(h), hmat(h5)))
#' stopifnot(all.equal(cmat(h), cmat(h5)))
#'
#'
#' @seealso \code{\link[hypr]{hypr}}, \code{\link[hypr]{cmat}}, \code{\link[hypr]{hmat}}
#'
setClass("hypr", slots=c(eqs = "list", hmat = "matrix", cmat = "matrix"))

show.hypr <- function(object) {
  check_argument(object, "hypr")
  if(length(object@eqs) == 0) {
    cat("This hypr object does not contain hypotheses.")
  } else {
    if(length(object@eqs) == 1) {
      cat("hypr object containing one (1) null hypothesis:")
    } else {
      cat(sprintf("hypr object containing %d null hypotheses:", length(object@eqs)))
    }
    cat("\n")
    eq.names <- sprintf("H0.%s", if(is.null(names(object@eqs))) seq_along(object@eqs) else names(object@eqs))
    for(i in seq_along(object@eqs)) {
      cat(sprintf("%*s: 0 = ", max(nchar(eq.names)), eq.names[i]))
      show.expr_sum(object@eqs[[i]])
      cat("\n")
    }
    cat("\nHypothesis matrix (transposed):\n")
    show(thmat(object))
    cat("\nContrast matrix:\n")
    show(cmat(object, remove_intercept = FALSE, add_intercept = FALSE))
  }
}

#' @describeIn hypr Show summary of hypr object, including contrast equations, the (transposed) hypothesis matrix and the derived contrast matrix.
#'
#' @export
setMethod("show", "hypr", show.hypr)

parse_hypothesis <- function(expr, valid_terms = NULL) {
  check_argument(expr, c("expression","formula","call"))
  check_argument(valid_terms, c("NULL","character"))
  if(length(expr) == 2) {
    ret <- simplify_expr_sum(simplify_expr(call("-", expr[[2]], 0)))
  } else if(length(expr) == 3) {
    ret <- simplify_expr_sum(simplify_expr(call("-", expr[[2]], expr[[3]])))
  } else {
    stop("Expression has an unusual length!")
  }
  for(el in ret) {
    if(length(el@var) == 0) {
      stop("Equation should not have terms without variables!")
    }
    if(!is.null(valid_terms) && !all(el@var %in% valid_terms)) {
      stop("Equation contains invalid variables!")
    }
  }
  ret
}

check_names <- function(nvec) {
  if(is.null(nvec)) {
    TRUE
  } else if(is.character(nvec)) {
    all(grepl("^[a-zA-Z][a-zA-Z0-9_.]*$|^\\.{1,2}([a-zA-Z_][a-zA-Z0-9._]*)?$|^\\.{3}[a-zA-Z0-9._]+$", nvec))
  } else {
    FALSE
  }
}

#' Shorthand versions for simple hypothesis translation
#'
#' These functions can be used to translate between null hypothesis equations, hypothesis matrices, and contrast matrices without defining a \code{hypr} object. Note that some of these functions do generate a \code{hypr} object internally but they never return one.
#'
#' @name conversions
#' @param eqs A \code{list} of equations
#' @param levels (optional) A \code{character} vector of variables to be expected (if not provided, automatically generated from all terms occurring in the equations list)
#' @param order_levels (optional) Whether to alphabetically order appearance of levels (rows in transposed hypothesis matrix or contrast matrix). Default is \code{TRUE} if \code{levels} were not explicitly provided.
#' @param cmat Contrast matrix
#' @param hmat Hypothesis matrix
#' @param as_fractions (optional) Whether to output matrix using fractions formatting (via \code{\link[MASS:as.fractions]{MASS::as.fractions}}). Defaults to \code{TRUE}.
#'
#' @return A \code{list} of equations (\code{hmat2eqs} and \code{cmat2eqs}), a contrast matrix (\code{hmat2cmat}, \code{eqs2cmat}), or a hypothesis matrix (\code{cmat2hmat}, \code{eqs2hmat}).
#'
#' @examples
#'
#' # The following examples are based on a 2-level treatment contrast (i.e., baseline and treatment).
#' hypotheses <- list(baseline = mu1~0, treatment = mu2~mu1)
#' hypothesis_matrix <- matrix(
#'     c(c(1, -1), c(0, 1)), ncol = 2, dimnames = list(c("baseline","treatment"), c("mu1", "mu2")))
#' contrast_matrix <- matrix(
#'     c(c(1, 1), c(0, 1)), ncol = 2, dimnames = list(c("mu1","mu2"), c("baseline", "treatment")))
#'
#' # Convert a list of null hypothesis equations to ...
#' # ... a hypothesis matrix:
#' eqs2hmat(hypotheses)
#' # ... a contrast matrix:
#' eqs2cmat(hypotheses)
#'
#' # Convert a hypothesis matrix to...
#' # ... a list of null hypothesis equations:
#' hmat2eqs(hypothesis_matrix)
#' # ... a contrast matrix:
#' hmat2cmat(hypothesis_matrix)
#'
#' # Convert a contrast matrix to...
#' # ... a list of null hypothesis equations:
#' cmat2eqs(contrast_matrix)
#' # ... a hypothesis matrix:
#' cmat2hmat(contrast_matrix)
#'
#'
#' # Are all functions returning the expected results?
#' stopifnot(all.equal(eqs2hmat(hypotheses, as_fractions = FALSE), hypothesis_matrix))
#' stopifnot(all.equal(eqs2cmat(hypotheses, as_fractions = FALSE), contrast_matrix))
#' stopifnot(all.equal(hmat2cmat(hypothesis_matrix, as_fractions = FALSE), contrast_matrix))
#' stopifnot(all.equal(cmat2hmat(contrast_matrix, as_fractions = FALSE), hypothesis_matrix))
#'
NULL

#' @describeIn conversions Convert null hypothesis equations to hypothesis matrix
#'
#' @export
eqs2hmat <- function(eqs, levels = NULL, order_levels = missing(levels), as_fractions = TRUE) {
  check_argument(eqs, "list:formula")
  check_argument(levels, c("NULL", "character"))
  check_argument(order_levels, "logical", 1)
  check_argument(as_fractions, "logical", 1)
  expr <- lapply(eqs, parse_hypothesis, valid_terms = levels)
  expr2hmat(expr, levels = levels, order_levels = order_levels, as_fractions = as_fractions)
}

expr2hmat <- function(expr, levels = NULL, order_levels = missing(levels), as_fractions = TRUE) {
  check_argument(expr, "list:expr_sum")
  check_argument(levels, c("NULL", "character"))
  check_argument(order_levels, "logical", 1)
  check_argument(as_fractions, "logical", 1)
  if(is.null(levels)) {
    levels <- unique(unlist(lapply(expr, function(h) unlist(lapply(h, function(x) x@var)))))
  }
  if(isTRUE(order_levels)) {
    levels <- sort(levels)
  }
  ret <- as.matrix(vapply(seq_along(expr), function(i) {
    vapply(levels, function(j) {
      for(el in expr[[i]]) {
        if(setequal_exact(el@var, j)) {
          return(as.fractions.expr_num(el@num))
        }
      }
      return(0)
    }, double(1))
  }, double(length(levels))))
  rownames(ret) <- levels
  colnames(ret) <- names(expr)
  if(as_fractions)
    MASS::as.fractions(t(ret))
  else
    t(ret)
}

#' @describeIn conversions Convert null hypothesis equations to contrast matrix
#' @export
eqs2cmat <- function(eqs, as_fractions = TRUE) hmat2cmat(eqs2hmat(eqs), as_fractions = as_fractions)

#' @describeIn conversions Convert hypothesis matrix to contrast matrix
#' @export
hmat2cmat <- function(hmat, as_fractions = TRUE) {
  if(nrow(hmat) > 0 && ncol(hmat) > 0)
    ginv2(hmat, as_fractions = as_fractions)
  else
    matrix(0, ncol = 0, nrow = 0)
}

#' @describeIn conversions Convert contrast matrix to hypothesis matrix
#' @export
cmat2hmat <- function(cmat, as_fractions = TRUE) {
  if(nrow(cmat) > 0 && ncol(cmat) > 0)
    ginv2(cmat, as_fractions = as_fractions)
  else
    matrix(0, ncol = 0, nrow = 0)
}

#' @describeIn conversions Convert hypothesis matrix to null hypothesis equations
#' @export
hmat2eqs <- function(hmat, as_fractions = TRUE) sapply(hmat2expr(hmat, as_fractions = as_fractions), as.formula.expr_sum, simplify = FALSE)

hmat2expr <- function(hmat, as_fractions = TRUE) {
  check_argument(hmat, "matrix", "numeric")
  check_argument(as_fractions, "logical", 1)
  ret <- lapply(seq_len(nrow(hmat)), function(j) {
    simplify_expr_sum(
      as(lapply(seq_len(ncol(hmat)), function(i) {
        if(as_fractions) {
          frac <- strsplit(attr(MASS::as.fractions(hmat[j,i]), "fracs"), "/", TRUE)[[1]]
          num <- new("expr_frac", num = as.integer(frac[1]), den = if(length(frac)>1) as.integer(frac[2]) else 1L)
        } else {
          num <- new("expr_real", num = hmat[j,i])
        }
        new("expr_coef", num = num, var = colnames(hmat)[i])
      }), "expr_sum")
    )
  })
  names(ret) <- rownames(hmat)
  ret
}

#' @describeIn conversions Convert contrast matrix to null hypothesis equations
#' @export
cmat2eqs <- function(cmat, as_fractions = TRUE) hmat2eqs(cmat2hmat(cmat), as_fractions = as_fractions)

is.formula <- function(x) is(x, "formula") || is.call(x) && x[[1]] == "~"

#' Create a hypr object
#'
#' Use this function to create hypr objects from null hypothesis equations. Each argument should be one equation. For example, a null hypothesis for the grand mean (GM), often used as the intercept, is usually coded as \code{mu~0}.
#'
#' You may call the function without any arguments. In that case, an empty hypr object is returned. This is useful if you want to derive equations from a known hypothesis matrix or contrast matrix.
#'
#' Basic specification of contrasts in R is accomplished with basic R functions \code{\link[stats:contrasts]{stats::contrasts()}} and \code{\link[stats:C]{stats::C()}} (Chambers & Hastie, 1992). Other relevant packages for this topic are \code{multcomp} (Bretz et al., 2010), \code{contrast} (Kuhn et al., 2016), and, including also various vignettes, \code{emmeans} (Lenth, 2019).
#'
#' @param ... A list of null hypothesis equations
#' @param levels (Optional) A list of terms/levels to use. If supplied, matrix rows/columns will be in this order. An error will be thrown if an equation contains a level that is not in this vector.
#' @param order_levels (Optional) Whether to order the rows/columns of the hypothesis/contrast matrices alphabetically. Default is \code{TRUE} if \code{levels} were not explicitly provided.
#'
#' @return A \code{hypr} object
#'
#' @seealso \code{\link[stats]{contrasts}} and \code{\link[stats]{C}} for basic specification of contrasts in R, S4 class \code{\link[hypr:hypr-class]{hypr}}, \code{\link[hypr]{cmat}}, \code{\link[hypr]{contr.hypothesis}} for retrieval of contrast matrices from \code{hypr} objects
#'
#' @references
#' Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical models}. Chapter 2 of \emph{Statistical Models} in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#'
#' Frank Bretz, Torsten Hothorn and Peter Westfall (2010), \emph{Multiple Comparisons Using R}, CRC Press, Boca Raton.
#'
#' Max Kuhn, contributions from Steve Weston, Jed Wing, James Forester and Thorn Thaler (2016). \emph{contrast: A Collection of Contrast Methods}. R package version 0.21. \url{https://CRAN.R-project.org/package=contrast}
#'
#' Lenth, R. (2019). \emph{emmeans: Estimated Marginal Means, aka Least-Squares Means}. R package version 1.4.1. \url{https://CRAN.R-project.org/package=emmeans}
#'
#' @examples
#'
#' # Create an empty hypr object (no hypotheses):
#' h <- hypr()
#'
#' # Treatment contrast:
#' h <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)
#'
#' # Identical version:
#' h <- hypr(~mu1, ~mu2-mu1, ~mu3-mu1, ~mu4-mu1)
#'
#' contr.hypothesis(h)
#'
#'
#' # Generate a dataset
#' set.seed(123)
#' M <- c(mu1 = 10, mu2 = 20, mu3 = 10, mu4 = 40) # condition means
#' N <- 5 # number of observations per condition
#' SD <- 10 # residual SD
#' simdat <- do.call(rbind, lapply(names(M), function(x) {
#'   data.frame(X = x, DV = as.numeric(MASS::mvrnorm(N, unname(M[x]), SD^2, empirical = TRUE)))
#' }))
#' simdat$X <- factor(simdat$X, levels=levels(h))
#' simdat
#'
#' # Check agreement of hypothesis levels and factor levels
#' stopifnot(levels(h) == levels(simdat$X))
#'
#' # Linear regression
#' contrasts(simdat$X) <- contr.hypothesis(h)
#'
#' round(coef(summary(lm(DV ~ X, data=simdat))),3)
#'
#' @export
hypr <- function(..., levels = NULL, order_levels = missing(levels)) {
  hyps = list(...)
  check_argument(levels, c("NULL","character"))
  check_argument(order_levels, "logical", 1)
  if(length(hyps) == 0) {
    return(new("hypr"))
  } else if(length(hyps) == 1 && is.list(hyps[[1]])) {
    hyps <- hyps[[1]]
  } else if(length(hyps) == 1 && is.matrix(hyps[[1]])) {
    h <- hypr()
    cmat(h) <- hyps[[1]]
    return(h)
  }
  if(!all(vapply(hyps, is.formula, logical(1)))) {
    stop("Arguments to hypr() must be formulas or a list() of those.")
  }
  if(!is.null(names(hyps)) && any(names(hyps) == "")) {
    stop("If there is at least one named hypothesis, all must be named.")
  }
  parsed_hypotheses <- lapply(hyps, parse_hypothesis, valid_terms = levels)
  which_empty <- which(vapply(parsed_hypotheses, function(x) length(x) == 0, FALSE))
  if(length(which_empty) > 0) {
    stop(sprintf("List contains at least one empty hypothesis: %s", paste(vapply(hyps, function(x) as.character(as.expression(x)), ""), collapse=", ")))
  }
  hmat <- expr2hmat(parsed_hypotheses, levels = levels, order_levels = order_levels, as_fractions = FALSE)
  cmat <- hmat2cmat(hmat, as_fractions = FALSE)
  new("hypr", eqs = parsed_hypotheses, hmat = hmat, cmat = cmat)
}

`+.hypr` <- function(e1, e2) {
  check_argument(e1, "hypr")
  check_argument(e2, "hypr")
  cmat1 <- cmat(e1)
  cmat2 <- cmat(e2)
  if(is.null(rownames(cmat1))) {
    rownames(cmat1) <- sprintf("mu%d", seq_len(nrow(cmat1)))
  }
  if(is.null(rownames(cmat2))) {
    rownames(cmat2) <- sprintf("mu%d", seq_len(nrow(cmat2)))
  }
  mat <- do.call(rbind, lapply(seq_len(nrow(cmat1)), function(i) {
    cbind(cmat1[rep(i, each=nrow(cmat2)), , drop = FALSE], cmat2)
  }))
  colnames(mat) <- if(is.null(colnames(cmat1)) || is.null(colnames(cmat2))) NULL else c(colnames(cmat1), colnames(cmat2))
  rownames(mat) <- sprintf("%s.%s", rep(rownames(cmat1), each=nrow(cmat2)), rep(rownames(cmat2), nrow(cmat1)))
  ret <- hypr()
  cmat(ret) <- mat
  ret
}

`:.hypr` <- function(e1, e2) {
  check_argument(e1, "hypr")
  check_argument(e2, "hypr")
  cmat1 <- cmat(e1)
  cmat2 <- cmat(e2)
  if(is.null(rownames(cmat1))) {
    rownames(cmat1) <- sprintf("mu%d", seq_len(nrow(cmat1)))
  }
  if(is.null(rownames(cmat2))) {
    rownames(cmat2) <- sprintf("mu%d", seq_len(nrow(cmat2)))
  }
  mat <- do.call(cbind, lapply(seq_len(ncol(cmat1)), function(i) {
    do.call(rbind, lapply(seq_len(nrow(cmat1)), function(j) {
      cmat1[j,i] * cmat2
    }))
  }))
  colnames(mat) <- if(is.null(colnames(cmat1)) || is.null(colnames(cmat2))) NULL else sprintf("%s.%s", rep(colnames(cmat1), each=ncol(cmat2)), rep(colnames(cmat2), ncol(cmat1)))
  rownames(mat) <- sprintf("%s.%s", rep(rownames(cmat1), each=nrow(cmat2)), rep(rownames(cmat2), nrow(cmat1)))
  ret <- hypr()
  cmat(ret) <- mat
  ret
}

`*.hypr` <- function(e1, e2) {
  ret <- hypr()
  cmat(ret) <- cbind(cmat(`+.hypr`(e1,e2)), cmat(`:.hypr`(e1,e2)))
  ret
}

`/.hypr` <- function(e1, e2) {
  ret <- hypr()
  e3 <- hypr()
  cmat(e3) <- diag(length(levels(e1)))
  names(e3) <- levels(e1)
  levels(e3) <- levels(e1)
  cmat1 <- cmat(e1)
  cmat2 <- cmat(`:.hypr`(e3,e2))
  cmat(ret) <- cbind(cmat1[rep(seq_len(nrow(cmat1)), each=length(levels(e2))),], cmat2)
  levels(ret) <- rownames(cmat2)
  ret
}

#' Combining hypr objects by addition or interaction
#'
#' You can combine one or more \code{hypr} objects, i.e. combine their hypothesis to a single \code{hypr} object, by adding them with the \code{+} or \code{\*} operators.
#'
#'
#' @param e1,e2 \code{hypr} objects to concatenate
#' @return The combined \code{hypr} object
#'
#' @rdname combination
#'
#' @examples
#'
#' (h1 <- hypr(a~i, b~i)) # a hypr object of two treatments
#'
#' (h2 <- hypr(i~0)) # an intercept-only hypr object
#'
#' hc <- h1 + h2
#'
#' hc
#'
#' interaction <- h1 & h2
#'
#' interaction_and_main <- h1 * h2
#'
#' @export
#'
setMethod("+", c("hypr","hypr"), `+.hypr`)

#' @describeIn combination Interaction of \code{e1} and \code{e2}
#' @export
setMethod("*", c("hypr","hypr"), `*.hypr`)

#' @describeIn combination Interaction and main contrasts of \code{e1} and \code{e2}
#' @export
setMethod("&", c("hypr","hypr"), `:.hypr`)

#' @describeIn combination Nesting levels of \code{e2} within \code{e1}
#' @export
setMethod("/", c("hypr","hypr"), `/.hypr`)

setMethod("seq", c("hypr"), `:.hypr`)

#' Retrieve and set hypothesis matrix
#'
#' Use these functions to retrieve or set a hypr object's hypothesis matrix. If used for updating, the contrast matrix and equations are derived automatically.
#'
#' @rdname hmat
#' @param x A hypr object
#' @param as_fractions Whether to format matrix as fractions (via \code{\link[MASS:as.fractions]{MASS::as.fractions}})
#' @param value Hypothesis matrix
#'
#' @return Hypothesis matrix of \code{x}
#'
#' @examples
#'
#' h <- hypr(mu1~0, mu2~mu1)
#'
#' # To retrieve the hypothesis matrix of `h`:
#' hmat(h)
#'
#' # To retrieve the transposed hypothesis matrix of `h`:
#' thmat(h)
#'
#' # Setting the hypothesis matrix of `h`:
#' hmat(h) <- matrix(c(1,-1,0,1), ncol=2, dimnames=list(NULL, c("mu1","mu2")))
#' h
#'
#' h2 <- hypr() # an empty hypr object
#' thmat(h2) <- matrix(c(1,0,-1,1), ncol=2, dimnames=list(c("mu1","mu2"), NULL))
#' h2
#'
#' # `h` and `h2` should be identical:
#' stopifnot(all.equal(hmat(h), hmat(h2)))
#' stopifnot(all.equal(cmat(h), cmat(h2)))
#'
#' @export
hmat <- function(x, as_fractions = TRUE) if(as_fractions) MASS::as.fractions(x@hmat) else x@hmat

#' @describeIn hmat Retrieve transposed hypothesis matrix
#' @export
thmat <- function(x, as_fractions = TRUE) t(hmat(x, as_fractions = as_fractions))

#' @describeIn hmat Set hypothesis matrix
#' @export
`hmat<-` <- function(x, value) {
  check_argument(x, "hypr")
  check_argument(value, "matrix", "numeric")
  if(!check_names(colnames(value))) {
    colnames(value) <- make.names(colnames(value))
  }
  class(value) <- setdiff(class(value), "fractions")
  x@hmat <- value
  x@eqs <- hmat2expr(value)
  x@cmat <- hmat2cmat(value, as_fractions = FALSE)
  x
}


#' @describeIn hmat Set transposed hypothesis matrix
#' @export
`thmat<-` <- function(x, value) {
  hmat(x) <- t(value)
  x
}

levels.hypr <- function(x) {
  check_argument(x, "hypr")
  colnames(x@hmat)
}

#' @describeIn hypr Retrieve the levels (variable names) used in a \code{hypr} object
#'
#' @return A character vector of level names
#'
#' @export
setMethod("levels", signature(x="hypr"), levels.hypr)

names.hypr <- function(x) {
  check_argument(x, "hypr")
  rownames(x@hmat)
}

#' @describeIn hypr Retrieve the contrast names used in a \code{hypr} object
#'
#' @return A character vector of contrast names
#'
#' @export
setMethod("names", signature(x="hypr"), names.hypr)

`names<-.hypr` <- function(x, value) {
  check_argument(x, "hypr")
  check_argument(value, c("NULL","character"))
  mat <- hmat(x)
  rownames(mat) <- value
  `hmat<-`(hypr(), mat)
}

#' @describeIn hypr Set the contrast names used in a \code{hypr} object
#'
#' @export
setMethod("names<-", signature(x="hypr"), `names<-.hypr`)

`levels<-.hypr` <- function(x, value) {
  check_argument(x, "hypr")
  check_argument(value, c("NULL","character"))
  mat <- hmat(x)
  colnames(mat) <- value
  `hmat<-`(hypr(), mat)
}

#' @describeIn hypr Set the levels used in a \code{hypr} object
#'
#' @export
setMethod("levels<-", signature(x="hypr"), `levels<-.hypr`)



#' Manipulate the formulas of an S4 object
#'
#' This is a generic function for setting an S4 object’s formulas.
#'
#' @param x The object to manipulate
#' @param ... Additional arguments passed on to the method
#' @param value The new formula
#'
#'
#' @export
setGeneric("formula<-", function(x, ..., value) UseMethod("formula<-", x))

formula.hypr <- function(x, ...) sapply(x@eqs, as.formula.expr_sum, simplify = FALSE)

`formula<-.hypr` <- function(x, ..., value) hypr(value)

#' @describeIn hypr Retrieve a \code{hypr} object’s null hypothesis equations.
#'
#' @return A \code{list} of null hypothesis equations
#'
#' @examples
#' h <- hypr(mu1~0, mu2~mu1)
#' formula(h)
#'
#' h2 <- hypr()
#' formula(h2) <- formula(h)
#' h2
#' formula(h2)
#'
#' # After updating, matrices should be equal
#' stopifnot(all.equal(hmat(h), hmat(h2)))
#' stopifnot(all.equal(cmat(h), cmat(h2)))
#'
#' @export
setMethod("formula", signature(x="hypr"), formula.hypr)

#' @describeIn hypr Modify a \code{hypr} object’s null hypothesis equations
#' @export
setMethod("formula<-", signature(x="hypr"), `formula<-.hypr`)


prepare_cmat <- function(value, add_intercept, remove_intercept) {
  intercept_col <- which_intercept(value)
  if(isTRUE(add_intercept) && isTRUE(remove_intercept)) {
    stop("Cannot add and remove intercept at the same time!")
  } else if(isTRUE(add_intercept) || (is.null(add_intercept) && length(intercept_col) == 0)) {
    if(length(intercept_col) > 0) {
      warning(sprintf("You are using add_intercept=TRUE but it seems your contrast matrix already includes an intercept at contrast #%d!", intercept_col))
    }
    if(is.null(colnames(value))) {
      value <- cbind(1, value)
    } else if(is.character(add_intercept)) {
      value <- cbind(1, value)
      colnames(value)[1] <- add_intercept
    } else {
      value <- cbind(`Intercept` = 1, value)
    }
  } else if(isTRUE(remove_intercept) || is.null(remove_intercept)) {
    if(length(intercept_col) == 0 && isTRUE(remove_intercept)) {
      stop("You are using remove_intercept=TRUE but your contrast matrix does not have an obvious intercept! Please set remove_intercept=FALSE or specify with a number corresponding to the column index you want to remove, e.g. remove_intercept=1 for removing the first column.")
    }
    if(length(intercept_col) > 0) {
      if(length(intercept_col) > 1) {
        stop("You are using remove_intercept=TRUE but your contrast matrix does not have an obvious single intercept! Please set remove_intercept=FALSE or specify with a number corresponding to the column index you want to remove, e.g. remove_intercept=1 for removing the first column.")
      }
      value <- value[,-intercept_col,drop=F]
    }
  } else if(is.numeric(remove_intercept)) {
    value <- value[,-remove_intercept,drop=F]
  }
  value
}

which_intercept <- function(mat) which(apply(mat, 2, function(x) all(x[1]==x[-1])))

has_intercept <- function(mat) length(which_intercept(mat) > 0)

#' Retrieve or set contrast matrix
#'
#' Use these functions to retrieve or set a \code{hypr} object’s contrast matrix. If used for updating, the hypothesis matrix and equations are derived automatically.
#'
#' Basic specification of contrasts in R is accomplished with basic R functions \code{\link[stats:contrasts]{stats::contrasts()}} and \code{\link[stats:C]{stats::C()}} (Chambers & Hastie, 1992). Other relevant packages for this topic are \code{multcomp} (Bretz et al., 2010), \code{contrast} (Kuhn et al., 2016), and, including also various vignettes, \code{emmeans} (Lenth, 2019).
#'
#' @references
#' Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical models}. Chapter 2 of \emph{Statistical Models} in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#'
#' Frank Bretz, Torsten Hothorn and Peter Westfall (2010), \emph{Multiple Comparisons Using R}, CRC Press, Boca Raton.
#'
#' Max Kuhn, contributions from Steve Weston, Jed Wing, James Forester and Thorn Thaler (2016). \emph{contrast: A Collection of Contrast Methods}. R package version 0.21. \url{https://CRAN.R-project.org/package=contrast}
#'
#' Lenth, R. (2019). \emph{emmeans: Estimated Marginal Means, aka Least-Squares Means}. R package version 1.4.1. \url{https://CRAN.R-project.org/package=emmeans}
#'
#'
#' @param x A hypr object
#' @param value contrast matrix
#' @param add_intercept Add additional intercept column to contrast matrix
#' @param remove_intercept If \code{TRUE}, tries to find an intercept column (all codes equal) and removes it from the matrix. If \code{NULL}, does the same but does not throw an exception if no intercept is found. \code{FALSE} explicitly disables this functionality. A numeric argument explicitly identifies the index of the column to be removed.
#' @param as_fractions Should the returned matrix be formatted as fractions (using \code{\link[MASS:as.fractions]{MASS::as.fractions()}})?
#' @param ... A list of hypothesis equations for which to retrieve a contrast matrix
#' @rdname cmat
#'
#' @seealso \code{\link[hypr]{hypr}}
#'
#' @return A \code{matrix} of contrast codes with contrasts as columns and levels as rows.
#'
#' @examples
#'
#' h <- hypr(mu1~0, mu2~mu1)
#' cmat(h) # retrieve the contrast matrix
#'
#' contr.hypothesis(h) # by default without intercept (removes first column)
#' contr.hypothesis(mu1~0, mu2~mu1)
#'
#' @export
cmat <- function(x, add_intercept = FALSE, remove_intercept = FALSE, as_fractions = TRUE) {
  check_argument(x, "hypr")
  check_argument(add_intercept, c("NULL","logical"), 1)
  check_argument(remove_intercept, c("NULL","logical"), 1)
  check_argument(as_fractions, "logical", 1)
  if(has_intercept(x@cmat) && missing(remove_intercept)) {
    warning("The contrast matrix you are retrieving appears to have an intercept column. If this is intentional, you can ignore this warning or suppress it by explictly calling cmat(..., remove_intercept=FALSE).")
  }
  value <- prepare_cmat(x@cmat, add_intercept, remove_intercept)
  if(isTRUE(as_fractions)) {
    value <- MASS::as.fractions(value)
  }
  class(value) <- union(class(value), "hypr_cmat")
  value
}

#' @describeIn cmat Set contrast matrix
#' @export
`cmat<-` <- function(x, add_intercept = FALSE, remove_intercept = FALSE, value) {
  check_argument(x, "hypr")
  check_argument(add_intercept, c("NULL","logical"), 1)
  check_argument(remove_intercept, c("NULL","logical"), 1)
  check_argument(value, "matrix", "numeric")
  if(!check_names(colnames(value))) {
    rownames(value) <- make.names(rownames(value))
  }
  if(!has_intercept(value) && missing(add_intercept)) {
    warning("The contrast matrix you are assigning to this hypr object does not appear to have an intercept column. If this is intentional, you can ignore this warning or suppress it by explictly calling cmat(..., add_intercept=FALSE) <- x.")
  }
  value <- prepare_cmat(value, add_intercept, remove_intercept)
  if(is.null(rownames(value))) {
    rownames(value) <- sprintf("mu%d", seq_len(nrow(value)))
  } else {
    rownames(value) <- vapply(rownames(value), function(s) if(grepl("^[^A-Za-z_]", s)) paste0("mu",s) else s, character(1), USE.NAMES = FALSE)
  }
  class(value) <- setdiff(class(value), "fractions")
  x@cmat <- value
  x@hmat <- cmat2hmat(value, as_fractions = FALSE)
  x@eqs <- hmat2expr(x@hmat)
  x
}


#' @describeIn cmat Retrieve contrast matrix with sensible intercept default to override factor contrasts
#' @export
contr.hypothesis <- function(..., add_intercept = FALSE, remove_intercept = NULL, as_fractions = FALSE) {
  args <- list(...)
  if(length(args) == 1 && is.numeric(args[[1]])) {
    stop("`contr.hypothesis` cannot be used with a numeric argument. Please specify explicit hypotheses or pass a hypr object!")
  }
  cmat(
    x = if(length(args) == 1 && is(args[[1]], "hypr")) args[[1]] else do.call(hypr, args),
    add_intercept = add_intercept,
    remove_intercept = remove_intercept,
    as_fractions = as_fractions
  )
}

`contrasts<-.hypr` <- function(x, how.many, value) {
  if(inherits(value, "hypr")) {
    cm <- contr.hypothesis(value)
  } else if(inherits(value, "hypr_cmat")) {
    cm <- value
  }
  if(!setequal(levels(x), rownames(cm))) {
    warning(sprintf("The levels of the hypr object (%s) do not match the levels of the factor (%s). Contrasts may have been incorrectly assigned. Please check results with contrasts(factor) or ensure that level names match!",paste(rownames(cm), collapse = ", "),paste(levels(x), collapse=", ")))
  }
  cm <- cm[match(levels(x), rownames(cm)), , drop = FALSE]
  if(missing(how.many))
    contrasts(x) <- cm
  else
    contrasts(x, how.many) <- cm
  x
}

#' @describeIn cmat Update factor contrasts
#' @param how.many see \code{\link[stats:contrasts]{stats::contrasts()}}
#' @export
setMethod("contrasts<-", c(x="ANY",how.many="ANY",value="hypr"), `contrasts<-.hypr`)

#' @describeIn cmat Update factor contrasts
#' @export
setMethod("contrasts<-", c(x="ANY",how.many="ANY",value="hypr_cmat"), `contrasts<-.hypr`)

#' @describeIn cmat Update contrast matrix with sensible intercept default
#' @export
`contr.hypothesis<-` <- function(x, add_intercept = NULL, remove_intercept = FALSE, as_fractions = FALSE, value) {
  check_argument(x, "hypr")
  check_argument(value, "matrix", "numeric")
  cmat(
    x,
    add_intercept = add_intercept,
    remove_intercept = remove_intercept
  ) <- value
  x
}

#' Enhanced generalized inverse function
#'
#' This function is a wrapper for \code{MASS::ginv} and calculates the generalized inverse of \code{x}.
#'
#' In addition to \code{MASS::ginv}, this function rounds values, formats the matrix as fractions and copies dimension names from the original matrix.
#'
#' @param x The original matrix
#' @param as_fractions Whether to format the matrix as fractions (MASS package)
#' @return Generalized inverse of \code{x}
#' @seealso \code{\link[MASS]{ginv}}
#'
#' @examples
#'
#' h <- hypr(mu1~0, mu2~mu1)
#' hmat(h)
#'
#' ginv2(hmat(h))
#' cmat(h)
#'
#' # cmat is effectively the generalized inverse of hmat
#' stopifnot(all.equal(ginv2(hmat(h)), cmat(h), check.attributes = FALSE))
#'
#' @export
ginv2 <- function(x, as_fractions = TRUE) {
  check_argument(x, "matrix", "numeric")
  check_argument(as_fractions, "logical", 1)
  y <- round(MASS::ginv(x), floor(-log10(.Machine$double.neg.eps) - 3))
  dimnames(y) <- dimnames(x)[2:1]
  if(isTRUE(as_fractions)) MASS::fractions(y) else y
}
