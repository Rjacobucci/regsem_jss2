

Xs <- seq(-2,2,.01)

plot(Xs,abs(Xs),"l",xlab="X",ylab="P(x)",ylim=c(0,1.2),lwd=2)

# see breheny

library(DiceKriging)
SCAD(.1,.1)

out <- rep(NA,length(Xs))
for(i in 1:length(Xs)){
  out[i] <- SCAD(Xs[i],.5)
}
lines(Xs,out,"l",lty=2,lwd=2)

mcp <- function(x,lambda){
  x <- abs(x)
  a <- 3.7
  if(x <= a*lambda){
    lambda*x - (x**2)/(2*a)
  }else{
    .5*a*(lambda**2)
  }
}

out2 <- rep(NA,length(Xs))
for(i in 1:length(Xs)){
  out2[i] <- mcp(Xs[i],.5)
}
lines(Xs,out2,"l",lty=3,lwd=2)
abline(a=0,b=0)
legend("bottomright", c("Lasso","SCAD","MCP"),lty=c(1,2,3), lwd=2,cex=.8)
