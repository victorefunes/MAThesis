htrab<-function(x, y, psi){
  omega<-x/y
  if(omega<psi/2)
    htrab<-0
  if(omega>psi/2 & omega<2/psi)
    htrab<-(2/(2+psi))-(psi/(2+psi))*omega^{-1}
  if(omega>2/psi)
    htrab<- 2/(2+psi)
  
  htrab
}