library(lavaan)
library(semPlot)
library(semTools)
library(regsem)

count <- 0
niters <- 200
nobs <- c(80,200,1000)
res.mat1 <- matrix(NA,niters*length(nobs),6)
res.mat2 <- matrix(NA,niters*length(nobs),6)

for(i in 1:niters){
  for(j in 1:length(nobs)){

count <- count + 1
res.mat1[count,1] <- res.mat2[count,1] <- nobs[j]

sim.mod <- "
i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
s =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
i ~ 1*c1 + .2*c2 + 0*c3 + 0*c4 + 0*c5 + 0*c6 + 0*c7 + 0*c8+ 0*c9+ 0*c10
s ~ 1*c1 + .2*c2 + 0*c3 + 0*c4 + 0*c5 + 0*c6 + 0*c7 + 0*c8+ 0*c9+ 0*c10"

dat <- simulateData(sim.mod,model.type="growth",sample.nobs=nobs[j],seed=i)

mod1 <- "
i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
s =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
i ~ c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8+ c9+ c10
s ~ c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8+ c9+ c10
"

out <- growth(mod1,dat,fixed.x=T)

res.mat1[count,2] <- sum(parameterestimates(out)[c(11:18,21:28),"pvalue"] < .05)
res.mat2[count,2] <- sum(parameterestimates(out)[c(9:10,19:20),"pvalue"] > .05)



reg.out <- cv_regsem(out,pars_pen=c(1:20),type="lasso",n.lambda=40,mult.start=FALSE,jump=.015)
reg.out


loc1 <- which(reg.out[[2]][,"BIC"] ==min(reg.out[[2]][,"BIC"]))[1]
res.mat1[count,3] <- sum(reg.out[[1]][loc1,c(3:10,13:20)] != 0)
res.mat2[count,3] <- sum(reg.out[[1]][loc1,c(1:2,11:12)] == 0)

reg.out2 <- cv_regsem(out,pars_pen=c(1:20),type="alasso",n.lambda=40,mult.start=FALSE,jump=.003)
reg.out2

loc2 <- which(reg.out2[[2]][,"BIC"] ==min(reg.out2[[2]][,"BIC"]))[1]
res.mat1[count,4] <- sum(reg.out2[[1]][loc2,c(3:10,13:20)] != 0)
res.mat2[count,4] <- sum(reg.out2[[1]][loc2,c(1:2,11:12)] == 0)

reg.out3 <- cv_regsem(out,pars_pen=c(1:20),type="scad",n.lambda=40,mult.start=FALSE,jump=.07)
reg.out3

loc3 <- which(reg.out3[[2]][,"BIC"] ==min(reg.out3[[2]][,"BIC"]))[1]
res.mat1[count,5] <- sum(reg.out3[[1]][loc3,c(3:10,13:20)] != 0)
res.mat2[count,5] <- sum(reg.out3[[1]][loc3,c(1:2,11:12)] == 0)

reg.out4 <- cv_regsem(out,pars_pen=c(1:20),type="mcp",n.lambda=40,mult.start=FALSE,jump=.1)
reg.out4

loc4 <- which(reg.out4[[2]][,"BIC"] ==min(reg.out4[[2]][,"BIC"]))[1]
res.mat1[count,6] <- sum(reg.out4[[1]][loc4,c(3:10,13:20)] != 0)
res.mat2[count,6] <- sum(reg.out4[[1]][loc4,c(1:2,11:12)] == 0)

  }
}

save.image("regsem_jss_sim.RData")


colMeans(res.mat1)
colMeans(res.mat2)

ret.mat <- matrix(NA,6,6)
colnames(ret.mat) <- c("N","ML","lasso","alasso","SCAD","MCP")
ret.mat[1,] <- colMeans(res.mat1[res.mat1[,1]==80,])/16
ret.mat[2,] <- colMeans(res.mat1[res.mat1[,1]==200,])/16
ret.mat[3,] <- colMeans(res.mat1[res.mat1[,1]==1000,])/16

ret.mat[4,] <- colMeans(res.mat2[res.mat2[,1]==80,])/4
ret.mat[5,] <- colMeans(res.mat2[res.mat2[,1]==200,])/4
ret.mat[6,] <- colMeans(res.mat2[res.mat2[,1]==1000,])/4
ret.mat[,1] <- c(80,200,1000,80,200,1000)

library(xtable)
xtable(ret.mat)