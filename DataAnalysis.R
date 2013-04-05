#Histograma
histograma = function (data) {
  hist = hist(data)
  lines(c(min(hist$breaks), hist$mids, max(hist$breaks)), c(0, hist$counts, 0), type = "l")
  hist
}

#Tabela de Frequências
tabela = function(data, metodo = '') {
  r = max(data) - min(data)
  if (metodo == 'sturge') {
    k = 1 + 3.22 * log (length(data), 10)
  } else {'sturn'
          if (length(data) > 25) {
            k = sqrt(length(data))
          } else {
            k = 5
          }
  }
  h = r/k
  
  lim = seq(min(data), max(data), by = h)
  intUnemp = cut(data, breaks = lim, include.lowest = TRUE)
  tabela = data.frame(table(data))
  return(tabela)
}

distNorm = function(data, media, desv, alfa) {
  df = length(data)-1
  Z = qt(((1-alfa)/2)-0.5, df)
  p = desv/sqrt(df+1)
  E = Z*p
  upperBound = media+E
  lowerBound = media-E
  x = data
  norm = curve(dnorm(x,media, desv * desv),datalim = c(min(data),max(data)), main = 'Normal padrão')
}

intervalo = 0.01
cord.x = c(2, seq(2,3, intervalo),3)
cord.y=c(0,dnorm(seq(2,3,intervalo)), 0)

polygon(cord.x,cord.y,col ='skyblue')
