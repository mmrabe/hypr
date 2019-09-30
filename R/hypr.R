#' @include equations.R
#' @importFrom methods as is new show
NULL


#' An object containing null hypotheses, a hypothesis matrix, and a contrast matrix
#'
#' @param object a hypr object
#'
#' @slot eqs A list of null hypotheses
#' @slot hmat A hypothesis matrix
#' @slot cmat A contrast matrix
#'
setClass("hypr", slots=c(eqs = "list", hmat = "matrix", cmat = "matrix"))

#' @describeIn hypr Show summary of hypr object
#'
#' @export
setMethod(show, "hypr", function(object) {
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
      show(object@eqs[[i]])
      cat("\n")
    }
    cat("\nHypothesis matrix (transposed):\n")
    show(thmat(object))
    cat("\nContrast matrix:\n")
    show(cmat(object))
  }
})

parse_hypothesis <- function(expr, valid_terms = NULL, order_terms = FALSE) {
  if(!is(expr, "formula")) {
    stop("`expr` must be a formula")
  }
  ret <- simplify_expr_sum(simplify_expr(call("-",expr[[2]],expr[[3]])))
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
#' @name conversions
#' @param eqs A list() of equations
#' @param terms (optional) A character vector of variables to be expected (if not provided, automatically generated from all terms occurring in the equations list)
#' @param order_terms (optional) Whether to alphabetically order appearance of terms (rows in transposed hypothesis matrix or contrast matrix)
#' @param cmat A contrast matrix
#' @param hmat A hypothesis matrix
#' @param as_fractions Whether to output matrix using fractions formatting (MASS package)
#' @return A list of equations (hmat2eqs and cmat2eqs), a contrast matrix (hmat2cmat, eqs2cmat), or a hypothesis matrix (cmat2hmat, eqs2hmat)
#'
NULL

#' @describeIn conversions Convert null hypothesis equations to hypothesis matrix
#'
#' @export
eqs2hmat <- function(eqs, terms = NULL, order_terms = FALSE) {
  if(is.null(terms)) {
    terms <- unique(unlist(lapply(eqs, function(h) unlist(lapply(h, function(x) x@var)))))
  }
  if(order_terms) {
    terms <- sort(terms)
  }
  if(!is.list(eqs) || !all(vapply(eqs, function(x) is(x, "expr_sum"), logical(1)))) {
    stop("`eqs` must be a list of expr_sums!")
  }
  if(!check_names(names(eqs))) {
    stop("If equations are named, all must be named and names must be valid variable names in R!")
  }
  ret <- as.matrix(vapply(seq_along(eqs), function(i) {
    vapply(terms, function(j) {
      for(el in eqs[[i]]) {
        if(setequal_exact(el@var, j)) {
          return(as.fractions.expr_num(el@num))
        }
      }
      return(0)
    }, double(1))
  }, double(length(terms))))
  rownames(ret) <- terms
  colnames(ret) <- names(eqs)
  t(ret)
}

#' @describeIn conversions Convert null hypothesis equations to contrast matrix
#' @export
eqs2cmat <- function(eqs, as_fractions = TRUE) hmat2cmat(eqs2hmat(eqs), as_fractions = as_fractions)

#' @describeIn conversions Convert hypothesis matrix to contrast matrix
#' @export
hmat2cmat <- function(hmat, as_fractions = TRUE) {
  if(!check_names(rownames(hmat))) {
    stop("If hypothesis matrix columns are named, all must be named and names must be valid variable names in R!")
  }
  ginv2(hmat, as_fractions = as_fractions)
}

#' @describeIn conversions Convert contrast matrix to hypothesis matrix
#' @export
cmat2hmat <- function(cmat, as_fractions = TRUE) {
  if(!check_names(colnames(hmat))) {
    stop("If contrast matrix columns are named, all must be named and names must be valid variable names in R!")
  }
  ginv2(cmat, as_fractions = as_fractions)
}

#' @describeIn conversions Convert hypothesis matrix to null hypothesis equations
#' @export
hmat2eqs <- function(hmat, as_fractions = TRUE) {
  if(!check_names(rownames(hmat))) {
    stop("If hypothesis matrix columns are named, all must be named and names must be valid variable names in R!")
  }
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

#' Create a hypr object
#'
#' Use this function to create hypr objects from null hypothesis equations. Each argument should be one equation. For example, a null hypothesis for the grand mean (GM), often used as the intercept, is usually coded as mu~0.
#'
#' You may call the function without any arguments. In that case, an empty hypr object is returned. This is useful if you want to derive equations from a known hypothesis matrix or contrast matrix.
#'
#' @param ... A list of null hypothesis equations
#' @param terms (Optional) A list of terms to use. If supplied, matrix rows/columns will be in this order. An error will be thrown if an equation contains a term that is not in this vector.
#' @param order_terms (Optional) Whether to order the rows/columns of the hypothesis/contrast matrices alphabetically
#'
#' @examples
#'
#' # Create an empty hypr object (no hypotheses):
#' h <- hypr()
#'
#' # Treatment contrast:
#' h <- hypr(mu1~0, mu2~mu1, mu3~mu1)
#'
#' cmat(h, remove_intercept = TRUE)
#' contr.treatment(3)
#'
#' # This generates a similar hypr object:
#' h <- hypr()
#' cmat(h, add_intercept = TRUE) <- contr.treatment(c("mu1","mu2","mu3"))
#' h
#'
#' @export
hypr <- function(..., terms = NULL, order_terms = FALSE) {
  hyps = list(...)
  if(length(hyps) == 0) {
    return(new("hypr"))
  } else if(length(hyps) == 1 && is.list(hyps[[1]])) {
    hyps <- hyps[[1]]
  }
  if(!all(vapply(hyps, function(x) is(x, "formula"), logical(1)))) {
    stop("Arguments to hypr() must be formulas or a list() of those.")
  }
  parsed_hypotheses <- lapply(hyps, parse_hypothesis, valid_terms = terms, order_terms = order_terms)
  hmat <- eqs2hmat(parsed_hypotheses, terms = terms, order_terms = order_terms)
  cmat <- hmat2cmat(hmat, as_fractions = FALSE)
  new("hypr", eqs = parsed_hypotheses, hmat = hmat, cmat = cmat)
}

#' Retrieve and set hypothesis matrix
#'
#' Use these functions to retrieve or set a hypr object's hypothesis matrix. If used for updating, the contrast matrix and equations are derived automatically.
#'
#' @rdname hmat
#' @param x A hypr object
#' @param as_fractions Whether to format matrix as fractions (MASS package)
#' @param value Hypothesis matrix
#'
#' @export
hmat <- function(x, as_fractions = TRUE) if(as_fractions) MASS::as.fractions(x@hmat) else x@hmat

#' @describeIn hmat Retrieve transposed hypothesis matrix
#' @export
thmat <- function(x, as_fractions = TRUE) t(hmat(x, as_fractions = as_fractions))

#' @describeIn hmat Set hypothesis matrix
#' @export
`hmat<-` <- function(x, value) {
  class(value) <- setdiff(class(value), "fractions")
  x@hmat <- value
  x@eqs <- hmat2eqs(value)
  x@cmat <- hmat2cmat(value, as_fractions = FALSE)
  x
}


#' @describeIn hmat Set transposed hypothesis matrix
#' @export
`thmat<-` <- function(x, value) hmat(x) <- t(value)

#' Retrieve the terms (variables) used in a hypr object
#'
#' @param x A hypr object
#' @return A character vector of term names
#'
#' @export
setMethod("terms", signature(x="hypr"), function(x) rownames(hmat(x)))

#' Retrieve or set contrast matrix
#'
#' Use these functions to retrieve or set a hypr object's contrast matrix. If used for updating, the hypothesis matrix and equations are derived automatically.
#'
#' @param x A hypr object
#' @param value contrast matrix
#' @param add_intercept Add intercept column to contrast matrix
#' @param remove_intercept Remove intercept column from contrast matrix
#' @param ... A list of hypothesis equations for which to retrieve a contrast matrix
#' @rdname cmat
#'
#' @export
cmat <- function(x, add_intercept = FALSE, remove_intercept = FALSE) {
  if(add_intercept && remove_intercept) {
    stop("Cannot add and remove intercept at the same time!")
  } else if(add_intercept) {
    if(is.null(colnames(x@cmat))) {
      MASS::as.fractions(cbind(1, x@cmat))
    } else {
      MASS::as.fractions(cbind(Intercept=1, x@cmat))
    }
  } else if(remove_intercept) {
    MASS::as.fractions(x@cmat[,-1,drop=F])
  } else {
    MASS::as.fractions(x@cmat)
  }
}

#' @describeIn cmat Set contrast matrix
#' @export
`cmat<-` <- function(x, add_intercept = FALSE, remove_intercept = FALSE, value) {
  if(!is.matrix(value)) {
    stop("`value` must be a contrast matrix!")
  }
  if(add_intercept && remove_intercept) {
    stop("Cannot add and remove intercept at the same time!")
  } else if(add_intercept) {
    if(!is.matrix(value)) stop("add_intercept=TRUE can only be used with a matrix argument!")
    if(is.null(colnames(value))) {
      value <- cbind(1, value)
    } else {
      value <- cbind(`Intercept` = 1, value)
    }
  } else if(remove_intercept) {
    if(!is.matrix(value)) stop("remove_intercept=TRUE can only be used with a matrix argument!")
    value <- value[,-1,drop=F]
  }
  if(is.null(rownames(value))) {
    rownames(value) <- sprintf("mu%d", seq_len(nrow(value)))
  } else {
    rownames(value) <- vapply(rownames(value), function(s) if(grepl("^[^A-Za-z_]", s)) paste0("mu",s) else s, character(1))
  }
  class(value) <- setdiff(class(value), "fractions")
  x@cmat <- value
  x@hmat <- cmat2hmat(value, as_fractions = FALSE)
  x@eqs <- hmat2eqs(x@hmat)
  x
}


#' @describeIn cmat Retrieve contrast matrix to override factor contrasts
#' @export
contr.hypothesis <- function(..., add_intercept = FALSE, remove_intercept = FALSE) {
  args <- list(...)
  if(length(args) == 1 && is.numeric(args[[1]])) {
    stats::contr.treatment(args[[1]])
  } else if(length(args) == 1 && is(args[[1]], "hypr")) {
    cmat(x = args[[1]], add_intercept = add_intercept, remove_intercept = remove_intercept)
  } else {
    cmat(x = hypr(...), add_intercept = add_intercept, remove_intercept = remove_intercept)
  }
}

#' Nice generalized inverse
#'
#' This function calculates the generalized inverse of x, formats it as fractions and copies dimension names from the original matrix.
#'
#' @param x The original matrix
#' @param as_fractions Whether to format the matrix as fractions (MASS package)
#' @return Nice generalized inverse
#' @seealso MASS::ginv()
#'
#' @export
ginv2 <- function(x, as_fractions = TRUE) {
  y <- MASS::ginv(x)
  dimnames(y) <- dimnames(x)[2:1]
  if(as_fractions) MASS::fractions(y) else y
}
