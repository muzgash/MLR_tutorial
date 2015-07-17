#Cost Function
J = function(vBeta, vY, mX) {
  return(sum((vY - mX %*% vBeta)^2))
}
#REading the data
data=read.csv2(file = 'data.csv', header=TRUE,col.names = c('year','km','price'), sep=" ")
vY<-data$price

#We have to normalize since squaring the km variable we obtain NAs because of overflow
X<-as.numeric(data$km)/mean(as.numeric(data$km))

#Generating parameters for the linear regression
mX<-cbind(1,as.matrix(X))
vBeta_lin=rep(0, ncol(mX))
sol_lin=optim(vBeta_lin, J,mX = mX, vY = vY, method = 'BFGS')

#Generatig the parameter for a quadratic regression
mX<-cbind(1,as.matrix(X),as.matrix(X*X))
vBeta_quad=rep(0,ncol(mX))
sol_quad=optim(vBeta_quad, J,mX = mX, vY = vY, method = 'BFGS')

plot(X,data$price)
abline(a=sol_lin$par[1],b=sol_lin$par[2],col='red')
par(new=T)
x=seq(min(mX[,2]),max(mX[,2]),0.01)
plot(x,sol_quad$par[1]+sol_quad$par[2]*x+sol_quad$par[3]*x*x,col='blue',type='l')