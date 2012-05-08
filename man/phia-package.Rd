\name{phia-package}
\alias{phia-package}
\alias{phia}
\docType{package}
\title{
Post-Hoc Interaction Analysis}
\description{
Analysis of terms in linear and generalized linear models, on the basis of
multiple comparisons of factor contrasts. Specially suited for the analysis of
interaction terms.}
\details{
\tabular{ll}{
Package: \tab phia\cr
Type: \tab Package\cr
Version: \tab 0.0-2\cr
Date: \tab 2012-05-08\cr
License: \tab GPL (>= 3)\cr
}
This package contains functions that may be used for the post-hoc analysis
of any term of linear models (univariate or multivariate), and generalized
linear models with factors or covariates.  The function \code{testFactors}
provides a flexible user interface for defining combinations of factor levels
and covariates, to evaluate and test the model, using the function
\code{linearHypothesis} from package \pkg{car}.  \code{testInteractions}
uses this function for multiple comparisons of simple effects, interaction
residuals, interaction contrasts, or user-defined contrasts.
\code{interactionMeans} may be used to explore \sQuote{cell means} of factorial
designs, and plot main effects or first-order interactions.
}
\author{
Helios De Rosario-Martinez

Maintainer: Helios De Rosario-Martinez <helios.derosario@ibv.upv.es>
}
