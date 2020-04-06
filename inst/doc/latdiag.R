### R code from vignette source 'latdiag.Rnw'

###################################################
### code chunk number 1: latdiag.Rnw:92-102
###################################################
library(latdiag)
if(!requireNamespace("ltm")) {
   warning("You need to install ltm to rebuild the vignette")
} else {
   data(LSAT, package = "ltm")
}
set.seed(12022020)
res <- draw.latent(LSAT)
#print(res, rootname = "lsat")
#plot(res, rootname = "lsat", graphtype = "pdf")


###################################################
### code chunk number 2: latdiag.Rnw:177-184
###################################################
score <- apply(LSAT, 1, mean)
LSAT$item6 <- sapply(score, function(x) sample(0:1, 1, prob = c(x, 1-x)))
LSAT$item7 <- sapply(score, function(x) sample(0:1, 1, prob = c(1-x, x)))
LSAT$item8 <- round(runif(nrow(LSAT)))
res <- draw.latent(LSAT)
#print(res, rootname = "simul")
#plot(res, rootname = "simul", graphtype = "pdf")


###################################################
### code chunk number 3: latdiag.Rnw:197-200
###################################################
res <- draw.latent(LSAT, which.npos = 5:6)
#print(res, rootname = "simul2")
#plot(res, rootname = "simul2", graphtype = "pdf")


