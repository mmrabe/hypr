% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypr.R
\name{cmat}
\alias{cmat}
\alias{cmat<-}
\alias{contr.hypothesis}
\alias{contrasts<-,factor,ANY,hypr-method}
\alias{contrasts<-,factor,ANY,hypr_cmat-method}
\alias{contr.hypothesis<-}
\title{Retrieve or set contrast matrix}
\usage{
cmat(x, add_intercept = FALSE, remove_intercept = FALSE, as_fractions = TRUE)

cmat(x, add_intercept = FALSE, remove_intercept = FALSE) <- value

contr.hypothesis(
  ...,
  add_intercept = FALSE,
  remove_intercept = NULL,
  as_fractions = FALSE
)

\S4method{contrasts}{factor,ANY,hypr}(x, how.many = NULL) <- value

\S4method{contrasts}{factor,ANY,hypr_cmat}(x, how.many = NULL) <- value

contr.hypothesis(
  x,
  add_intercept = NULL,
  remove_intercept = FALSE,
  as_fractions = FALSE
) <- value
}
\arguments{
\item{x}{A hypr object}

\item{add_intercept}{Add additional intercept column to contrast matrix}

\item{remove_intercept}{If \code{TRUE}, tries to find an intercept column (all codes equal) and removes it from the matrix. If \code{NULL}, does the same but does not throw an exception if no intercept is found. \code{FALSE} explicitly disables this functionality. A numeric argument explicitly identifies the index of the column to be removed.}

\item{as_fractions}{Should the returned matrix be formatted as fractions (using \code{\link[MASS:as.fractions]{MASS::as.fractions()}})?}

\item{value}{contrast matrix}

\item{...}{A list of hypothesis equations for which to retrieve a contrast matrix}

\item{how.many}{see \code{\link[stats:contrasts]{stats::contrasts()}}}
}
\value{
A \code{matrix} of contrast codes with contrasts as columns and levels as rows.
}
\description{
Use these functions to retrieve or set a \code{hypr} object’s contrast matrix. If used for updating, the hypothesis matrix and equations are derived automatically.
}
\details{
Basic specification of contrasts in R is accomplished with basic R functions \code{\link[stats:contrasts]{stats::contrasts()}} and \code{\link[stats:C]{stats::C()}} (Chambers & Hastie, 1992). Other relevant packages for this topic are \code{multcomp} (Bretz et al., 2010), \code{contrast} (Kuhn et al., 2016), and, including also various vignettes, \code{emmeans} (Lenth, 2019).
}
\section{Functions}{
\itemize{
\item \code{cmat(x, add_intercept = FALSE, remove_intercept = FALSE) <- value}: Set contrast matrix

\item \code{contr.hypothesis()}: Retrieve contrast matrix with sensible intercept default to override factor contrasts

\item \code{contrasts(x = factor, how.many = ANY) <- value}: Update factor contrasts

\item \code{contrasts(x = factor, how.many = ANY) <- value}: Update factor contrasts

\item \code{contr.hypothesis(
  x,
  add_intercept = NULL,
  remove_intercept = FALSE,
  as_fractions = FALSE
) <- value}: Update contrast matrix with sensible intercept default

}}
\examples{

h <- hypr(mu1~0, mu2~mu1)
cmat(h) # retrieve the contrast matrix

contr.hypothesis(h) # by default without intercept (removes first column)
contr.hypothesis(mu1~0, mu2~mu1)

}
\references{
Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical models}. Chapter 2 of \emph{Statistical Models} in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

Frank Bretz, Torsten Hothorn and Peter Westfall (2010), \emph{Multiple Comparisons Using R}, CRC Press, Boca Raton.

Max Kuhn, contributions from Steve Weston, Jed Wing, James Forester and Thorn Thaler (2016). \emph{contrast: A Collection of Contrast Methods}. R package version 0.21. \url{https://CRAN.R-project.org/package=contrast}

Lenth, R. (2019). \emph{emmeans: Estimated Marginal Means, aka Least-Squares Means}. R package version 1.4.1. \url{https://CRAN.R-project.org/package=emmeans}
}
\seealso{
\code{\link[hypr]{hypr}}
}
