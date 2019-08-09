library(ggplot2)

rm(list = ls())

df = read.csv("HW5/Homework_5.csv")
df[, c(5:10)] = df[, c(5:10)]/100
n <- length(df$cmt0.25)
alphay <- 0

AiT = function(sigmai, betai, alphai, t){
  return(exp((sigmai^2/(2*betai^2)-alphai/betai)*t+(alphai/(betai^2)-sigmai^2/betai^3)*
               (1-exp(-betai*t))+sigmai^2/(4*betai^3)*(1-exp(-2*betai*t))))
}
BiT = function(betai, t){
  return(1/betai*(1-exp(-betai*t)))
}
ytmT = function(AxT,BxT,AyT,ByT,t,x,y){
  return(-log(AxT)/t + BxT/t*x - log(AyT)/t + ByT/t * y)
}

fn = function(param){
  alphax = param[1]
  sigmax = param[2]
  sigmay = param[3]
  betax = param[4]
  betay = param[5]
  periods = c(0.25, 2,3,5,7,10)
  for (i in periods){
    assign(paste("Ax", i, sep = ""), AiT(sigmax, betax, alphax, i))
    assign(paste("Ay", i, sep = ""), AiT(sigmay, betay, alphay, i))
    assign(paste("Bx", i, sep = ""), BiT(betax, i))
    assign(paste("By", i, sep = ""), BiT(betay, i))
  }
  
  XY = matrix(0, nrow = n, ncol = 2)
  for (i in 1:n){
    mat = matrix(c(Bx0.25/0.25, Bx10/10,By0.25/0.25, By10/10), nrow = 2)
    b = c(df$cmt0.25[i]+ log(Ax0.25)/0.25 + log(Ay0.25)/0.25, 
          df$cmt10[i]+ log(Ax10)/10 + log(Ay10)/10)
    XY[i,] = solve(mat,b)
  }
  ytm2 = ytmT(Ax2,Bx2,Ay2,By2,2, XY[,1], XY[,2])
  ytm3 = ytmT(Ax3,Bx3,Ay3,By3,3, XY[,1], XY[,2])
  ytm5 = ytmT(Ax5,Bx5,Ay5,By5,5, XY[,1], XY[,2])
  ytm7 = ytmT(Ax7,Bx7,Ay7,By7,7, XY[,1], XY[,2])
  RMSE = sqrt((sum((ytm2 - df$cmt2)^2) + sum((ytm3 - df$cmt3)^2)+
                 sum((ytm5 - df$cmt5)^2) + sum((ytm7 - df$cmt7)^2))/(4*n))
  RMSE
}

out = optim(c(0.1,0.2,0.2,0.4,-0.1), fn)

alphax = out$par[1]
sigmax = out$par[2]
sigmay = out$par[3]
betax = out$par[4]
betay = out$par[5]

periods = c(0.25, 2,3,5,7,10)
for (i in periods){
  assign(paste("Ax", i, sep = ""), AiT(sigmax, betax, alphax, i))
  assign(paste("Ay", i, sep = ""), AiT(sigmay, betay, alphay, i))
  assign(paste("Bx", i, sep = ""), BiT(betax, i))
  assign(paste("By", i, sep = ""), BiT(betay, i))
}

XY = matrix(0, nrow = n, ncol = 2)

for (i in 1:n){
  mat = matrix(c(Bx0.25/0.25, Bx10/10,By0.25/0.25, By10/10), nrow = 2)
  b = c(df$cmt0.25[i]+ log(Ax0.25)/0.25 + log(Ay0.25)/0.25, 
        df$cmt10[i]+ log(Ax10)/10 + log(Ay10)/10)
  XY[i,] = solve(mat,b)
}
ytm0.25 = ytmT(Ax0.25, Bx0.25, Ay0.25, By0.25, 0.25, XY[,1], XY[,2])
ytm2 = ytmT(Ax2,Bx2,Ay2,By2,2, XY[,1], XY[,2])
ytm3 = ytmT(Ax3,Bx3,Ay3,By3,3, XY[,1], XY[,2])
ytm5 = ytmT(Ax5,Bx5,Ay5,By5,5, XY[,1], XY[,2])
ytm7 = ytmT(Ax7,Bx7,Ay7,By7,7, XY[,1], XY[,2])
ytm10 = ytmT(Ax10,Bx10,Ay10,By10,10, XY[,1], XY[,2])
XY = data.frame(XY)
colnames(XY) = c('X','Y')
ggplot(data = XY, aes(x = c(1:n))) + geom_line(aes(y = X, col = 'x')) + geom_line(aes(y = Y, col = 'y'))


ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt0.25, col = 'real cmt0.25')) + 
  geom_line(aes(y = ytm0.25, col = 'model cmt0.25'))
ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt2, col = 'real cmt2')) + 
  geom_line(aes(y = ytm2, col = 'model cmt2'))
ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt3, col = 'real cmt3')) + 
  geom_line(aes(y = ytm3, col = 'model cmt3'))
ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt5, col = 'real cmt5')) + 
  geom_line(aes(y = ytm5, col = 'model cmt5'))
ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt7, col = 'real cmt7')) + 
  geom_line(aes(y = ytm7, col = 'model cmt7'))
ggplot(data = df, aes(x = c(1:n))) + geom_line(aes(y = cmt10, col = 'real cmt10')) + 
  geom_line(aes(y = ytm10, col = 'model cmt10'))


