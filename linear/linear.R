#Cost Function
J = function(vBeta, vY, mX) {
  return(sum((vY - mX %*% vBeta)^2))
}

data=read.csv2(file = 'data.csv', header=TRUE,col.names = c('year','km','price'), sep=" ")
vY<-data$price
mX<-data$km
mX<-cbind(1,as.matrix(mX))
vBeta0=rep(0, ncol(mX))
sol=optim(vBeta0, J,mX = mX, vY = vY, method = 'BFGS')