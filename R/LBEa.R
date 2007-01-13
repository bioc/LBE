`LBEa` <-
function(m,l=0.05,fig=TRUE,a.rng=NA){
  asearch <- function(a) {
    abs(sqrt(1/(gamma(a + 1))^2 * ((gamma(2 * a + 1) - (gamma(a + 1))^2)/m)) - l)
  }
  aopt <- max(1, optimize(asearch, c(0.3, 25))$minimum)
  if (fig){
    par(mfrow=c(1,1))
    if (is.na(a.rng[1])){
      asearch2 <- function(a) {
        abs(sqrt(1/(gamma(a + 1))^2 * ((gamma(2 * a + 1) - (gamma(a + 1))^2)/m)) - 0.5)
      }
      a.rng=c(1,max(1, optimize(asearch2, c(0.3, 30))$minimum))
    }
    a2<-seq(a.rng[1],a.rng[2],0.01)
    sd <- sqrt(1/(gamma(a2 + 1))^2 * ((gamma(2 * a2 + 1) - (gamma(a2 + 1))^2)/m))
    plot(a2,sd,type="l",lwd=3,col=4,ylab="standard deviation",xlab="a")
    abline(h=l,col="orange",lwd=3)
    abline(v=aopt,lty=2,lwd=2)
    legend(aopt,l,paste("a =",round(aopt,5)),bty="n",xjust=0,yjust=1,cex=1,col="white")
  }
  return(aopt)
}

