### R code from vignette source 'phia.Rnw'

###################################################
### code chunk number 1: phia.Rnw:16-17
###################################################
options(useFancyQuotes = FALSE)


###################################################
### code chunk number 2: phia.Rnw:221-223
###################################################
library(phia)
some(Boik)


###################################################
### code chunk number 3: modboikresid
###################################################
mod.boik <- lm(edr ~ therapy*medication, data=Boik)
par(mfcol=c(1,2))
plot(mod.boik, 1:2) # Plot diagnostics for the model
Anova(mod.boik)


###################################################
### code chunk number 4: phia.Rnw:242-243
###################################################
mod.boik <- lm(edr ~ therapy*medication, data=Boik)
par(mfcol=c(1,2))
plot(mod.boik, 1:2) # Plot diagnostics for the model
Anova(mod.boik)


###################################################
### code chunk number 5: phia.Rnw:278-279
###################################################
(boik.means <- interactionMeans(mod.boik))


###################################################
### code chunk number 6: phia.Rnw:292-293
###################################################
interactionMeans(mod.boik, factors="therapy")


###################################################
### code chunk number 7: phia.Rnw:312-313
###################################################
plot(boik.means)


###################################################
### code chunk number 8: phia.Rnw:328-329
###################################################
plot(boik.means, multiple=FALSE) # Not printed in this paper


###################################################
### code chunk number 9: phia.Rnw:400-401
###################################################
testInteractions(mod.boik, fixed="therapy", across="medication")


###################################################
### code chunk number 10: phia.Rnw:457-460
###################################################
boik.mtable <- xtabs(boik.means$"adjusted mean" ~ therapy+medication, boik.means)
boik.mtable <- addmargins(boik.mtable, FUN=mean, quiet=TRUE)
print(boik.mtable, digits=4)


###################################################
### code chunk number 11: phia.Rnw:468-472
###################################################
boik.resid <- boik.mtable - boik.mtable[4,5] # Subtract the mean
boik.resid <- sweep(boik.resid, 1, boik.resid[,5]) # Subtract row means
boik.resid <- sweep(boik.resid, 2, boik.resid[4,]) # Subtract column means
print(boik.resid, digits=4)


###################################################
### code chunk number 12: phia.Rnw:479-480
###################################################
testInteractions(mod.boik,residual=c("therapy","medication"))


###################################################
### code chunk number 13: residualsplot
###################################################
matplot(t(boik.resid[-4,-5]), type="b", xaxt="n", ylab="Interaction residuals")
axis(1, at=1:4, labels=levels(Boik$medication))


###################################################
### code chunk number 14: phia.Rnw:496-497
###################################################
matplot(t(boik.resid[-4,-5]), type="b", xaxt="n", ylab="Interaction residuals")
axis(1, at=1:4, labels=levels(Boik$medication))


###################################################
### code chunk number 15: phia.Rnw:564-565
###################################################
testInteractions(mod.boik, pairwise="therapy", across="medication")


###################################################
### code chunk number 16: phia.Rnw:582-583
###################################################
testInteractions(mod.boik)


###################################################
### code chunk number 17: phia.Rnw:610-615
###################################################
cntrl.vs.all <- c(2, -1, -1)     # Control vs. both therapies
T1.vs.T2 <- c(0, 1, -1)          # Therapy T1 vs. T2
plcb.vs.all <- c(3, -1, -1, -1)  # Placebo vs. all doses
D1.vs.D3 <- c(0, 1, 0, -1)       # Min. dose vs. max.
D2.vs.avrg <- c(0, -1, 2, -1)    # Med. dose vs. average


###################################################
### code chunk number 18: phia.Rnw:630-640
###################################################
therapy.contr <- cbind(cntrl.vs.all, T1.vs.T2)
medication.contr <- cbind(plcb.vs.all, D1.vs.D3, D2.vs.avrg)
# Add row names
rownames(therapy.contr) <- levels(Boik$therapy)
rownames(medication.contr) <- levels(Boik$medication)
# Normalize columns, so that their norm is 1
normrows <- sqrt(colSums(therapy.contr^2))
therapy.contr <- sweep(therapy.contr, 2, normrows, "/")
normrows <- sqrt(colSums(medication.contr^2))
medication.contr <- sweep(medication.contr, 2, normrows, "/")


###################################################
### code chunk number 19: phia.Rnw:645-646
###################################################
(custom.contr <- list(therapy=therapy.contr, medication=medication.contr))


###################################################
### code chunk number 20: phia.Rnw:651-652
###################################################
testInteractions(mod.boik,custom=custom.contr)


###################################################
### code chunk number 21: phia.Rnw:690-691
###################################################
some(OBrienKaiser, 6)


###################################################
### code chunk number 22: phia.Rnw:703-704
###################################################
(idata <- expand.grid(hour=ordered(1:5), phase=c("pre", "post","fup")))


###################################################
### code chunk number 23: phia.Rnw:710-711
###################################################
addmargins(table(OBrienKaiser[c("gender","treatment")]))


###################################################
### code chunk number 24: phia.Rnw:717-721
###################################################
mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5,
    post.1, post.2, post.3, post.4, post.5,
    fup.1, fup.2, fup.3, fup.4, fup.5) ~ treatment*gender,
    data=OBrienKaiser)


###################################################
### code chunk number 25: phia.Rnw:730-731
###################################################
Anova(mod.ok, idata=idata, idesign=~phase*hour, type=3)


###################################################
### code chunk number 26: okplot
###################################################
ok.means <- interactionMeans(mod.ok, c("hour","treatment","phase"), idata=idata)
plot(ok.means, atx="phase", traces="treatment")


###################################################
### code chunk number 27: phia.Rnw:754-755
###################################################
ok.means <- interactionMeans(mod.ok, c("hour","treatment","phase"), idata=idata)
plot(ok.means, atx="phase", traces="treatment")


###################################################
### code chunk number 28: phia.Rnw:772-773
###################################################
testInteractions(mod.ok, pairwise=c("treatment", "phase"), idata=idata)


###################################################
### code chunk number 29: phia.Rnw:855-856
###################################################
str(Prestige)


###################################################
### code chunk number 30: phia.Rnw:869-870
###################################################
Prestige$log.income <- log2(Prestige$income)


###################################################
### code chunk number 31: phia.Rnw:879-881
###################################################
mod.prestige <- lm(prestige ~ (log.income+education)*type, data=Prestige)
Anova(mod.prestige)


###################################################
### code chunk number 32: prestigeplot
###################################################
plot(interactionMeans(mod.prestige, "type", slope="log.income"))
testInteractions(mod.prestige, pairwise="type", slope="log.income")


###################################################
### code chunk number 33: phia.Rnw:897-898
###################################################
plot(interactionMeans(mod.prestige, "type", slope="log.income"))
testInteractions(mod.prestige, pairwise="type", slope="log.income")


###################################################
### code chunk number 34: phia.Rnw:916-917
###################################################
table(Prestige$type) # Frequencies of occupation types


###################################################
### code chunk number 35: phia.Rnw:941-944
###################################################
mod.prestige2 <- update(mod.prestige, formula=.~.+(log.income:education)*type,
subset=(Prestige$type!="wc"))
Anova(mod.prestige2)


###################################################
### code chunk number 36: phia.Rnw:952-953
###################################################
testInteractions(mod.prestige2, pairwise="type", slope="log.income")


###################################################
### code chunk number 37: phia.Rnw:967-971
###################################################
# Look quantiles of the model frame (a subset of the original data)
quantile(model.frame(mod.prestige2)$education)
testInteractions(mod.prestige2, pairwise="type", slope="log.income",
covariates=c(education=14))


###################################################
### code chunk number 38: phia.Rnw:1032-1033
###################################################
str(AMSsurvey)


###################################################
### code chunk number 39: phia.Rnw:1056-1057
###################################################
ftable(xtabs(count ~ sex + citizen + type, AMSsurvey))


###################################################
### code chunk number 40: phia.Rnw:1067-1068
###################################################
mod.ams <- glm(count ~ type*(sex+citizen), family=poisson, data=AMSsurvey)


###################################################
### code chunk number 41: phia.Rnw:1074-1075
###################################################
Anova(mod.ams)


###################################################
### code chunk number 42: amsplot
###################################################
ams.means <- interactionMeans(mod.ams)
plot(ams.means, atx="type", traces=c("sex","citizen"))


###################################################
### code chunk number 43: phia.Rnw:1090-1091
###################################################
ams.means <- interactionMeans(mod.ams)
plot(ams.means, atx="type", traces=c("sex","citizen"))


###################################################
### code chunk number 44: phia.Rnw:1123-1125
###################################################
testInteractions(mod.ams, pairwise=c("type","sex")) # test type:sex
testInteractions(mod.ams, pairwise=c("type","citizen")) #test type:citizen


###################################################
### code chunk number 45: phia.Rnw:1171-1172 (eval = FALSE)
###################################################
## dm.de <- family(model)$mu.eta


###################################################
### code chunk number 46: phia.Rnw:1297-1303
###################################################
Snijders <- nlme::bdf[c("langPRET","langPOST",   # Outcomes
    "pupilNR", "IQ.ver.cen", "ses", "sex",       # Student-related variables
    "schoolNR", "schoolSES", "avg.IQ.ver.cen")]  # School-related variables
Snijders$sex <- factor(Snijders$sex, labels=c("F","M"))
names(Snijders) <-
    c("score.1","score.2","student","IQ","SES","sex","school","avgSES","avgIQ")


###################################################
### code chunk number 47: phia.Rnw:1310-1312
###################################################
Snijders$IQ2.pos <- with(Snijders, (IQ > 0)*IQ^2)
Snijders$IQ2.neg <- with(Snijders, (IQ < 0)*IQ^2)


###################################################
### code chunk number 48: phia.Rnw:1323-1324
###################################################
library(lme4)


###################################################
### code chunk number 49: phia.Rnw:1326-1330 (eval = FALSE)
###################################################
## form1 <- score.2 ~ 
##     IQ * SES + IQ2.pos + IQ2.neg + sex + avgIQ * avgSES +    # Fixed part
##     (IQ | school)                                            # Random part
## mod.snijders.1 <- lmer(form1, data=Snijders)


###################################################
### code chunk number 50: phia.Rnw:1342-1344 (eval = FALSE)
###################################################
## form2.1 <- update(form1, (score.1+score.2)/2~.)
## form2.2 <- update(form1, (score.2-score.1)~.)


###################################################
### code chunk number 51: phia.Rnw:1363-1369
###################################################
Snijders.long <- reshape(Snijders, direction="long", idvar="student",
    varying=list(c("score.1","score.2")), v.names="score", timevar="repetition")
# The within-subjects factor must be coded as a factor
Snijders.long$repetition <- as.factor(Snijders.long$repetition)
# See the variables of the long data frame
str(Snijders.long)


###################################################
### code chunk number 52: phia.Rnw:1385-1392
###################################################
form3 <- score ~ 
    repetition * (IQ * SES + IQ2.pos + IQ2.neg + sex) +   # Student-related
    avgIQ * avgSES +                                      # School-related
    (IQ | school) + (1 | student)                         # Random part
mod.snijders.3 <- lmer(form3, data=Snijders.long)
# See the main parameters of the model (ommit correlations table)
print(mod.snijders.3, correlation=FALSE)


###################################################
### code chunk number 53: phia.Rnw:1423-1424
###################################################
Anova(mod.snijders.3)


###################################################
### code chunk number 54: phia.Rnw:1437-1443
###################################################
# Cell means
interactionMeans(mod.snijders.3)
# Simple effect of sex at each repetition
testInteractions(mod.snijders.3, fixed="repetition", across="sex")
# Pairwise interactions (default)
testInteractions(mod.snijders.3)


###################################################
### code chunk number 55: phia.Rnw:1476-1477
###################################################
graphics.off()


