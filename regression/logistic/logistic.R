J = function(vBeta, vY, mX) {
  return(-sum(vY*(mX %*% vBeta - log(1+exp(mX %*% vBeta)))
              + (1-vY)*(-log(1 + exp(mX %*% vBeta)))))
}

par(mfcol=c(1,2))

#Line logistic regression
data=read.table('data_line.txt')
names(data)=c('x1','x2','y')
vY=as.matrix(data$y)
mX=as.matrix(cbind(1,data[c('x1','x2')]))

vBeta0<-rep(0, ncol(mX))
sol<-optim(vBeta0, J,mX = mX, vY = vY)

valx0=data$x1[data$y==0]
valx1=data$x1[data$y==1]
valy0=data$x2[data$y==0]
valy1=data$x2[data$y==1]
plot(valx0,valy0,pch=20,xlim = c(0,4),ylim = c(0,4),xlab="X1",ylab = "X2")
par(new=T)
plot(valx1,valy1,pch=17,xlim = c(0,4),ylim = c(0,4),xlab="", ylab="")
par(new=T)
a=sol$par[1]
b=sol$par[2]
c=sol$par[3]
abline(a=-a/c,b=-b/c,col='red')
#Save plot

elipse=read.table('elipse.txt')
names(elipse)=c('x1','x2','y')
vY=as.matrix(elipse$y)
mX=as.matrix(cbind(1,elipse$x1*elipse$x1,elipse$x2*elipse$x2))
vBeta0<-c(-1,2/3,1.5)
sole<-optim(vBeta0, J,vY = vY,mX=mX)

valx0=elipse$x1[elipse$y==0]
valx1=elipse$x1[elipse$y==1]
valy0=elipse$x2[elipse$y==0]
valy1=elipse$x2[elipse$y==1]
plot(valx0,valy0,pch=17,xlim = c(-2,2),ylim = c(-2,2),xlab="X1",ylab = "X2")
par(new=T)
plot(valx1,valy1,pch=20,xlim = c(-2,2),ylim = c(-2,2),xlab="", ylab="")
par(new=T)
x<-seq(min(elipse$x1),max(elipse$x1),0.01)
a=sole$par[1]
b=sole$par[2]
c=sole$par[3]
y=-sqrt(-a/c-b*x*x/c)#Gives NaNs because the discriminant has negative values
plot(x,y,type='l',xlim = c(-2,2),ylim = c(-2,2),col='red',xlab="", ylab="")
par(new=T)
y=sqrt(-a/c-b*x*x/c)
plot(x,y,type='l',xlim = c(-2,2),ylim = c(-2,2),col='red',xlab="", ylab="")