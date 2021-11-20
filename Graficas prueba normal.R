#Fucnión para generar la gráfica de la prueba de medias con sigma conocida.
#Genera las zonas de rechazo y no rechazo y posiciona el etadistico de prueba en su valor.
#Funciona para una y dos poblaciones. Solo distribución normal por ahora

plot.norm.test <- function (data, data2 = NULL, mu0 = NULL, gamma = 0.95, sigma = NULL, sigma2 = NULL, cola = "ambas", main = "", ylab ="", xlab ="", col.NR ="blue", col.R = "red", col.t = "black") {
  alfa = 1-gamma
  if(is.null(data2)) {
    est.prueb <- (mean(data)-mu0)/(sigma/sqrt(length(data)))
    if(cola == "izquierda"){
      ZR.Inf <- -5
      ZR.Sup <- qnorm(alfa)
      zona.NR=seq(ZR.Sup,5,length.out = 100)
      xNR <- c(ZR.Sup,zona.NR,5)
      yNR <- c(0,dnorm(zona.NR),0)
      xR <- c(-5,seq(-5,ZR.Sup,length.out = 100),ZR.Sup)
      yR <- c(0,dnorm(seq(-5,ZR.Sup,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR, yR, col= col.R)
      text(x = c(0,-3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
      if(est.prueb < -5){
        text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }
    }else if (cola == "derecha"){
      ZR.Inf <- qnorm(1-alfa)
      ZR.Sup <- 5
      zona.NR=seq(-5,ZR.Inf,length.out = 100)
      xNR <- c(-5,zona.NR,ZR.Inf)
      yNR <- c(0,dnorm(zona.NR),0)
      xR <- c(ZR.Inf,seq(ZR.Inf, 5,length.out = 100),5)
      yR <- c(0,dnorm(seq(ZR.Inf, 5,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR, yR, col= col.R)
      text(x = c(0,3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
      if(est.prueb > 5){
        text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      } else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }
    }else if (cola == "ambas"){
      ZR.Inf <- qnorm(alfa/2)
      ZR.Sup <- qnorm(1-alfa/2)
      zona.NR=seq(ZR.Inf,ZR.Sup,length.out = 100)
      xNR <- c(ZR.Inf,zona.NR,ZR.Sup)
      yNR <- c(0,dnorm(zona.NR),0)
      xR.Inf <- c(-5,seq(-5,ZR.Inf,length.out = 100),ZR.Inf)
      yR.Inf <- c(0,dnorm(seq(-5,ZR.Inf,length.out = 100)),0)
      xR.Sup <- c(ZR.Sup,seq(ZR.Sup, 5,length.out = 100),5)
      yR.Sup <- c(0,dnorm(seq(ZR.Sup, 5,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR.Inf, yR.Inf, col= col.R)
      polygon(xR.Sup, yR.Sup, col= col.R)
      text(x = c(0,-3.5,3.5), y = c(0.2,0.1,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo","Zona de rechazo"), col = c("white",1,1))
      if(est.prueb < -5){
        text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }else if(est.prueb > 5){
        text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }
    }
  } else {
    est.prueb <- (mean(data)-mean(data2))/sqrt((sigma/length(data))+(sigma2/length(data2)))
    if(cola == "izquierda"){
      ZR.Inf <- -5
      ZR.Sup <- qnorm(alfa)
      zona.NR=seq(ZR.Sup,5,length.out = 100)
      xNR <- c(ZR.Sup,zona.NR,5)
      yNR <- c(0,dnorm(zona.NR),0)
      xR <- c(-5,seq(-5,ZR.Sup,length.out = 100),ZR.Sup)
      yR <- c(0,dnorm(seq(-5,ZR.Sup,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR, yR, col= col.R)
      text(x = c(0,-3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
      if(est.prueb < -5){
        text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }
    }else if (cola == "derecha"){
      ZR.Inf <- qnorm(1-alfa)
      ZR.Sup <- 5
      zona.NR=seq(-5,ZR.Inf,length.out = 100)
      xNR <- c(-5,zona.NR,ZR.Inf)
      yNR <- c(0,dnorm(zona.NR),0)
      xR <- c(ZR.Inf,seq(ZR.Inf, 5,length.out = 100),5)
      yR <- c(0,dnorm(seq(ZR.Inf, 5,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR, yR, col= col.R)
      text(x = c(0,3.5), y = c(0.2,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo"), col = c("white",1))
      if(est.prueb > 5){
        text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      } else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = "black")
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = "black", srt = 90)
      }
    }else if (cola == "ambas"){
      ZR.Inf <- qnorm(alfa/2)
      ZR.Sup <- qnorm(1-alfa/2)
      zona.NR=seq(ZR.Inf,ZR.Sup,length.out = 100)
      xNR <- c(ZR.Inf,zona.NR,ZR.Sup)
      yNR <- c(0,dnorm(zona.NR),0)
      xR.Inf <- c(-5,seq(-5,ZR.Inf,length.out = 100),ZR.Inf)
      yR.Inf <- c(0,dnorm(seq(-5,ZR.Inf,length.out = 100)),0)
      xR.Sup <- c(ZR.Sup,seq(ZR.Sup, 5,length.out = 100),5)
      yR.Sup <- c(0,dnorm(seq(ZR.Sup, 5,length.out = 100)),0)
      plot(x= seq(-5,5,length.out= length(zona.NR)), y = dnorm(seq(-5,5,length.out = length(zona.NR))), type = "l", bty = "l", main = main, ylab = ylab, xlab = xlab)
      polygon(xNR, yNR, col= col.NR)
      polygon(xR.Inf, yR.Inf, col= col.R)
      polygon(xR.Sup, yR.Sup, col= col.R)
      text(x = c(0,-3.5,3.5), y = c(0.2,0.1,0.1), labels = c("Zona de \n no rechazo", "Zona de rechazo","Zona de rechazo"), col = c("white",1,1))
      if(est.prueb < -5){
        text(x = -4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = -4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }else if(est.prueb > 5){
        text(x = 4.5, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = 4.5, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }else {
        text(x = est.prueb, y = 0.06, labels ="t", cex = 2, col = col.t)
        text(x = est.prueb, y = 0.03, labels ="<-", cex = 2, col = col.t, srt = 90)
      }
    }
  }
}

plot.norm.test(iris$Sepal.Length, mu0 = 15)
