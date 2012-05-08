\name{testFactors}
\alias{testFactors}
\alias{testFactors.default}
\alias{testFactors.lm}
\alias{testFactors.glm}
\alias{testFactors.mlm}
\alias{summary.testFactors}
\title{
Evaluate and Test Combinations of Factor Levels
}
\description{
Calculates and tests the adjusted mean value of the response and other terms of a fitted model, for specific linear combinations of factor levels and specific values of the covariates.  This function is specially indicated for post-hoc analyses of models with factors, to test pairwise comparisons of factor levels, simple main effects, and interaction contrasts.  In multivariate models, it is possible to define and test contrasts of intra-subjects factors, as in a repeated-measures analysis.
}
\usage{
\method{testFactors}{default}(model, levels, covariates, terms.formula=~1,
  inherit.contrasts=FALSE, default.contrasts=c("contr.sum","contr.poly"),
  lht=TRUE, \dots)
\method{testFactors}{lm}(model, \dots)
\method{testFactors}{glm}(model, \dots, link=FALSE)
\method{testFactors}{mlm}(model, levels, covariates, terms.formula=~1,
  inherit.contrasts=FALSE, default.contrasts=c("contr.sum","contr.poly"),
  idata, icontrasts=default.contrasts, lht=TRUE, \dots)
\method{summary}{testFactors}(object, predictors=TRUE, matrices=TRUE, \dots)
}
\arguments{
  \item{model}{fitted model. Currently supported classes are \code{"lm"}, \code{"glm"}, and \code{"mlm"}.}
  \item{levels}{list that defines the factor levels that will be contrasted; see the \emph{Details} for more information.}
  \item{covariates}{optional numeric vector with specific values of the covariates (see \emph{Details}).}
  \item{terms.formula}{formula that defines the terms of the model that will be calculated and tested.  The default value is ~1, and stands for the adjusted mean value.  See the \emph{Details} for more information.}
  \item{inherit.contrasts}{logical value:  should the default contrasts of model factors be inherited from the model data frame?}
  \item{default.contrasts}{names of contrast-generating functions to be applied by default to factors and ordered factors, respectively, if \code{inherit.contrasts} is \code{FALSE} (the default); the contrasts must produce an intra-subjects model matrix in which different terms are orthogonal.  The default is \code{c("contr.sum", "contr.poly")}.}
  \item{lht}{logical indicating if the adjusted values are tested (via \code{\link{linearHypothesis}).}}
  \item{link}{for \code{glm} models, logical indicating if the adjusted mean values should represent the link function (\code{FALSE} by default, i.e. represent the adjusted means of the response variable).}
  \item{idata}{an optional data frame giving a factor or factors defining the intra-subjects model for multivariate repeated-measures data, as defined in \code{\link{Anova}} or \code{\link{linearHypothesis}}.}
  \item{icontrasts}{names of contrast-generating functions to be applied in the within-subject \dQuote{data}.  The default is the same as \code{default.contrasts}.}
  \item{object}{object returned by \code{\link{testFactors}}.}
  \item{predictors}{logical value: should \code{\link{summary}} return the values of the predictor values used in the calculations?}
  \item{matrices}{logical value: should \code{\link{summary}} return the matrices used for testing by \code{\link{linearHypothesis}}?}
  \item{\dots}{other arguments passed down to \code{\link{linearHypothesis}}.}
}
\details{
The only mandatory argument is \code{model}.  The fitted model may include any number of factor or numeric predictors, but no arithmetic expressions.  If any predictor has to be transformed, such transformation should be done explicitly to create (an)other variable(s) that would be included in the model as they are.  The simplest usage of this method, where no other argument is defined, calculates the adjusted mean of the model response variable, pooling all the levels of factor predictors, and setting the numeric predictors (covariates) to their average values in the model data frame.

The argument \code{levels} is a list whose elements must be named as the factors of the model --- or the factors of the intra-subjects design, if they exist --- that will be controlled in the calculations.  For any other factors, their levels will be pooled over. Elements whose name does not match any factor of the model will be ignored.  The content of these elements may take different forms:
\enumerate{
  \item{A character string of length 1 or 2, with the name of one or two factor levels.  In the former case, the calculations will be restricted to this level of the factor, as in a simple main effects analysis; in the latter, a pairwise contrast will be calculated between both factor levels.}
  \item{A numeric vector without names, of length equal to the number of levels in the factor.  This will create a linear combination of factor levels, with the elements of the vector as coefficients.  For instance, if the factor \code{f} has three levels, an element \code{f=c(0.5, 0.5, 0)} will average the two first levels, and \code{f=c(0.5, 0.5, -1)} will contrast the average of the two first levels against the third one.}
  \item{A numeric vector with names equal to some or all the levels of the factor.  This is a simplification of the previous option, where some levels can be omitted, and the coefficient of each level is determined by the names of the vector, which do not have to follow a specific order.  Omitted levels will automatically be set to zero.}
  \item{A numeric matrix, as an extension of the two previous options for calculating several combinations at a time.  Combinations are defined in columns, so if the matrix does not have row names, the number of rows must be equal to the number of levels in the factor, or if the matrix does have row names, they must coincide with the levels of the factor.}
}

The argument \code{covariates} may be used for setting specific values of the model numeric predictors (covariates).  It must be a vector of numeric values.  If the elements of this vector have names, their values will be assigned to the covariates that match them; covariates of the model with names not represented in this vector will be set to their default value (the average in the model data frame), and elements with names that do not match with covariates will be ignored.  On the other hand, if \code{covariates} has no names, and its length is equal to the number of covariates of the model, the values will be assigned to those covariates in the same order as they occur in the model.  If it has a different length, the vector will be trimmed or reclycled as needed to fit the number of covariates in the model; this feature may be used, for instance, to set all covariates to the same value, e.g. \code{covariates = 0}.

To analyse terms other than the adjusted mean value of the response, use the argument \code{terms.formula}.  For instance, if the model has the covariates \code{var1}, \code{var2}, \dots, the slopes of the response with respect to them may be added to the analysis by defining \code{terms.formula} as \code{~var1 + var2 \dots}.  This formula may be used more generally, for analysing interactions, omitting the mean response, adding the main effects of factors, etc.  A different analysis is done for each term of this formula, that must also be contained in the formula of \code{model}.  For instance, if \code{terms.formula} is equal to \code{~ var1*var2}, the function will analyse the adjusted intercept, plus the terms \code{var1}, \code{var2}, and \code{var1:var2}.  The intercept stands for the mean value of the response, and terms formed by one or more covariates stand por the slope of the response with respect to the product of those covariates.

If any of the variables in the term is a factor, the function analyses a full set of contrasts for that factor of the remaining part of the term; for instance if \code{var1} were a factor, the term \code{var1} would stand for the contrasts of the intercept, and \code{var1:var2} would stand for the contrasts of the slope \code{var2}, across the levels of \code{var1}.  The set of contrasts used in the analysis is normally defined by the argument \code{default.contrasts}: by default, if the factor is ordered it will be a set of \dQuote{polynomial contrasts}, and otherwise \dQuote{sum contrasts}; however, if \code{inherit.contrasts} is \code{TRUE} the contrasts will directly be copied from the ones used to define the model.  Factors that have explicit contrasts defined in the model data frame will use those contrasts, regardless of the values defined for \code{default.contrasts} and \code{inherit.contrasts}.  The analysis assumes that the contrasts are orthogonal to the intercept, which is the usual case if the default arguments are provided, and a warning will be issued if non-orthogonal contrasts are used; take special care of not using \dQuote{treatment contrats} if \code{inherit.contrasts} is set to \code{TRUE} or \code{default.contrasts} is changed. 

In generalized linear models, the adjusted means represent the expected values of the response by default, but the expected value of the link function may be shown by setting the argument \code{link=FALSE}.  On the other hand, slope values always refer to the link function.

For multivariate models, the arguments \code{idata}, and \code{icontrasts} may be used to define an intra-subjects model for multivariate repeated-measures data, as described for \code{\link{Anova}} or \code{\link{linearHypothesis}} in package \pkg{car}.  Note, however, that the combinations of intra-subjects factor levels are defined in \code{levels}, and other arguments defined in those functions like \code{idesign}, \code{imatrix} or \code{iterms} will have no effect in \code{testFactors}.

The significance of adjusted values is tested by a call to \code{\link{linearHypothesis}} for each term, unless \code{lht} is set to \code{FALSE}.  Extra arguments may be passed down to that function, for instance to specify the test statistic that will be evaluated.
}
\value{
An object of class \code{"testFactors")}, that contains the adjusted values of each term and the otuput of the test, plus other variables used in the calculations.  The \code{\link{summary}} method for this object will display those variables, unless they be omitted by setting the optional arguments \code{predictors} or \code{matrices} to \code{FALSE}.  The argument \code{predictors} refers to the coefficients of specified combinations of factor levels, the values of covariates, and the contrast matrices used for terms that include factors; \code{matrices} refers to the \dQuote{linear hypothesis matrix} used by \code{\link{linearHypothesis}}, and in multivariate linear models, to the \dQuote{response transformation matrix} as well --- if it exists.

Moreover, \code{\link{summary}} groups the results of the tests for all terms in one table.  By default this table shows the test statistics, their degrees of freedom, and the \emph{p}-values.  If the model is of class \code{"lm"}, it also shows the sums of squares; and if it is of class \code{"mlm"}, only the first type of test statistic returned by \code{\link{linearHypothesis}} (by default \dQuote{Pillai}) is shown.  This variable shape of the ANOVA table is controlled by additional classes assigned to the object (either \code{"testFactors.lm"} or \code{"testFactors.mlm"}, as suitable).
}
\author{
Helios De Rosario-Martinez, \email{helios.derosario@ibv.upv.es}
}
\seealso{
\code{\link{linearHypothesis}} in package \pkg{car}, \code{\link{interactionMeans}}, and \code{\link{testInteractions}} .
}
\examples{
# Example with factors and covariates
# Analyse prestige of Canadian occupations depending on
# education, income and type of occupation
# See ?Prestige for a description of the data set

Prestige$log.income <- log2(Prestige$income)
prestige.mod <- lm(prestige~(education+log.income)*type,data=Prestige)

# Pairwise comparisons for factor "type", to see how it influences
# the mean value of  prestige and interacts with log.income

# 1: "white collar" vs "blue collar"
wc.vs.bc <- list(type=c("wc", "bc"))
testFactors(prestige.mod, wc.vs.bc, terms.formula=~log.income)
# 2: "professional" vs. "blue collar"
prof.vs.bc <- list(type=c("prof", "bc"))
testFactors(prestige.mod, prof.vs.bc, terms.formula=~log.income)
# 3: "professional" vs. "white collar"
prof.vs.wc <- list(type=c("prof", "wc"))
testFactors(prestige.mod, prof.vs.wc, terms.formula=~log.income)


# Interaction contrasts in a repeated-measures experiment
# See ?OBrienKaiser for a description of the data set

mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5, 
                     post.1, post.2, post.3, post.4, post.5, 
                     fup.1, fup.2, fup.3, fup.4, fup.5) ~  treatment*gender, 
                data=OBrienKaiser)

# intra-subjects data:
phase <- factor(rep(c("pretest", "posttest", "followup"), each=5))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)
Anova(mod.ok, idata=idata, idesign=~phase*hour)

# Contrasts across "phase", for different contrasts of "treatment"

# 1: Treatment "A" vs. treatment "B"
A.vs.B <- list(treatment=c("A", "B"))
testFactors(mod.ok, A.vs.B, idata=idata, terms.formula=~0+phase)
# 2: Controls vs. treatments
control.vs.AB <- list(treatment=c(A=1, B=1, control=-2))
testFactors(mod.ok, control.vs.AB, idata=idata, terms.formula=~0+phase)

# Shortcut to get only the adjusted values and simplified ANOVA tables
contr <- list(A.vs.B=A.vs.B, control.vs.AB=control.vs.AB)
anovaTables <- function(contrast) summary(testFactors(mod.ok, contrast,
        idata=idata, terms.formula=~0+phase),
	predictors=FALSE, matrices=FALSE)
lapply(contr,anovaTables)
}
