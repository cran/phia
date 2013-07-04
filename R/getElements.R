# Common methods for different model classes

# Method analogous to model.frame.lm
getModelFrame.default <- function(model){model.frame(model)}
getModelFrame.lme <- function(model){
	mt <- model$terms
	mf <- model$data[names(attr(mt,"dataClasses"))]
	attr(mf,"terms") <- mt
	return(mf)
}
getModelFrame <- function(model){UseMethod("getModelFrame")}

# Method analogous to model.matrix.lm
getModelMatrix.default <- function(model){model.matrix(model)}
getModelMatrix.lme <- function(model){model.matrix(terms(model),getModelFrame(model))}
getModelMatrix <- function(model){UseMethod("getModelMatrix")}

# Method analogous to coef.lm
getCoef.default <- function(model){coef(model)}
getCoef.merMod <- getCoef.mer <- getCoef.lme <- function(model){fixef(model)}
getCoef <- function(model){UseMethod("getCoef")}

# Method for obtaning factor levels from model
getXLevels.default <- function(model){model$xlevels}
getXLevels.lme <- function(model){lapply(model$contrasts, rownames)}
getXLevels.merMod <- getXLevels.mer <- function(model){
	predictors <- all.vars(terms(model))[-1]
	mf <- model.frame(model)[predictors]
	are.factors <-sapply(mf, is.factor)
	lapply(mf[are.factors],"levels")
}
getXLevels <- function(model){UseMethod("getXLevels")}

# Method for obtaining contrasts
getContrasts <- function(model){attr(getModelMatrix(model),"contrasts")}

# Method for defining the error family (in glm or glmm)
getFamily.default <- function(model){NULL}
getFamily.merMod <- getFamily.glm <- function(model){family(model)}
getFamily.mer <- function(model){
	if (length(model@muEta > 0)){ # isGLMM
		# Get family from function call
		fam <- getCall(model)$family
		# If it was a character string, try get the appropriate function
		if (is.character(fam)){
			tryCatch(
				fam <- get(fam, mode = "function"),
				error=function(e) fam <- NULL
			)
		}
		return(eval(fam)())
	}else return(NULL)
}
getFamily <- function(model){UseMethod("getFamily")}
