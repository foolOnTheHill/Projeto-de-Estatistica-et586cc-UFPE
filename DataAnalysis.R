#Arquivo
setwd("c:/")
data = read.table("Strikes.dat", header = TRUE)

#Variaveis

View(data)

countryCode = data$Country_Code
years = data$Year
strikes = data$Strike_Volume
unemp = data$Unemployment
inflation = data$Inflation
PR = data$Parlamentary_Representation
UC = data$Union_Centralization

#Histograma
histograma = function (data, metodo = "") {
  if (metodo == 'Sturges') {
    hist = hist(data, "Sturges")
  } else if (metodo == "Scott") {
    hist = hist(data, "Scott")
  } else {
    if (length(data) > 25) {
      k = sqrt(length(data))
      print(k)
    } else {
      k = 5
    }
    hist = hist(data, k)
  }
  
  lines(c(min(hist$breaks), hist$mids, max(hist$breaks)), c(0, hist$counts, 0), type = "l")
  
}

#Tabela de Frequencia
tabela = function(data, metodo = '') {
  r = max(data) - min(data) #Amplitude total
  
  #Numero de itervalos
  if (metodo == 'Sturges') {
    k = nclass.Sturges(x)
  } else if (metodo == "Scott") {
    k = nclass.scott(x)
  } else {
    if (length(data) > 25) {
      k = sqrt(length(data))
    } else {
      k = 5
    }
  }
  k = 3
  h = r/k #Aplitude de cada intervalo
  
  #
  lim = seq(min(data), max(data), by = h)
  Intervalo = cut(data, breaks = lim, include.lowest = TRUE)
  tabela = data.frame(table(Intervalo))
  #
  
  return(tabela)
}

#Intervalo de Confianca
student <- function(p, df) {
  return (abs(qt(0.5-p, df)))
}

distNorm <- function(data, media, desv, alfa) {
  x <- data
  df <- length(x)-1 #Graus de liberdade
  p <- (1-alfa)/2 #p em t-student
  Z <- student(p, df) #Z_alpha 
  E <- Z*(desv/sqrt(df+1)) #Erro
  var <- desv*desv
  
  #Limites do intervalo
  lowerBound <- media-E
  upperBound <- media+E
  
  #Grafico da Normal
  title <- paste("X~N(", media, ", ", var, ")", sep="")
  curve(dnorm(x, media, var), xlim=c(-max(x)+media, max(x)+media), main=title)
  
  #Destaque do intervalo de confianca no grafico
  par <- 0.01
  ox <- c(lowerBound, seq(lowerBound, upperBound, by=par), upperBound)
  oy <- c(0, dnorm(seq(lowerBound, upperBound, by=par), media, var), 0)
  polygon(ox, oy, col="skyblue")

}

#Teste de Hipoteses
teste <- function(data, Ho, mi) {
  df <- length(data)-1 #Graus de liberdade
  par = sd(data)/sqrt(length(data))
  T <- (mean(data)-mi)/par #Estatistica do teste
  acc <- TRUE  

  if (Ho == "=") { #Teste bilateral
    Tc <- student(0.475, df)
    if (T > Tc | T < (-1*Tc)) {
  acc <- FALSE
    }
  } else if (Ho == ">=") { #Teste unilateral a esquerda
    Tc <- (-1)*student(0.45, df)
    if (T < Tc) {
      acc <- FALSE
    }
  } else if (Ho == "<=") { #Teste unilateral a direita
    Tc <- student(0.45, df)
    if (T > Tc) {
      acc <- FALSE
    }
  }
  
  return(acc)
}

regCritica <- function(data, alfa) {
  x <- data
  df <- length(x)-1 #Graus de liberdade
  p <- (1-alfa)/2 #p em t-student
  Z <- student(p, df) #Z_alpha 
  
  #Grafico da Normal
  curve(dnorm(x, 0, 1), xlim=c(-3, 3))
  
  #Destaque da região critica
  par <- 0.001
  ox <- c(Z, seq(Z, 3, by=par), 3)
  oy <- c(0, dnorm(seq(Z, 3, by=par)), 0)
  polygon(ox, oy, col="red")
  
  ox = c(-3, seq(-3, -Z, par), -Z)
  oy = c(0,dnorm(seq(-3,-Z,par)), 0)
  polygon(ox,oy,col ='red')
  print(Z)
}


moda <- function(x) {
  t <- table(as.vector(x))
  k =  as(names(t)[t==max(t)], "numeric")
  return(k)
} 
