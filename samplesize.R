## Funciones para el calculo de tamaño de muestra


# Calculo de tamaño de muestra para una porporción. Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss1p<-function(siglevel,power,alternative,dropout)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.2,0.5,0.8)
  for (i in 1:3)
  {
    res[i]<-ceiling(pwr.p.test(h = efsize[i], sig.level=siglevel,power=power,alternative=alternative)$n)
    dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
   resdf<-data.frame(n=as.character(res)
                    ,n_droput=as.character(dpres)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n","n (drop-out)")
  resdf
}

# Calculo de tamaño de muestra para dos porporciones con tamaños de muestra iguales. Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss2p<-function(siglevel,power,alternative,dropout)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.2,0.5,0.8)
  for (i in 1:3)
  {
    res[i]<-ceiling(pwr.2p.test(h = efsize[i], sig.level=siglevel,power=power,alternative=alternative)$n)
    dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
  resdf<-data.frame(n=as.character(res)
                    ,n_droput=as.character(dpres)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n para cada grupo","n para cada grupo (drop-out)")
  resdf
}

# Calculo de tamaño de muestra para dos porporciones con tamaños de muestra distintos. Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss2p2n<-function(siglevel,power,alternative,dropout)
{
  res1<-vector();res2<-vector()
  dpres1<-vector();dpres2<-vector()
  efsize<-c(0.2,0.5,0.8) 
  for (i in 1:3)
  {
    res1[i]<-ceiling(pwr.p.test(h = efsize[i], sig.level=siglevel,power=power,alternative=alternative)$n)
    dpres1[i]<-ceiling(res1[i]+res1[i]*dropout)
    res2[i]<-ceiling(pwr.2p2n.test(h = efsize[i], n1=res1[i],sig.level=siglevel,power=power,alternative=alternative)$n2)
    dpres2[i]<-ceiling(res2[i]+res2[i]*dropout)    
  }
  resdf<-data.frame(n1=as.character(res1)
                    ,n1_droput=as.character(dpres1)
                    ,n2=as.character(res2)
                    ,n2_droput=as.character(dpres2)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n grupo 1","n grupo 1 (drop-out)","n grupo 2","n grupo 2 (drop-out)")
  resdf
}

# Calculo de tamaño de muestra para una media. Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss1m<-function(siglevel,power,alternative,dropout)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.2,0.5,0.8)
  for (i in 1:3)
  {
    res[i]<-ceiling(pwr.t.test(d = efsize[i], sig.level=siglevel,power=power,alternative=alternative,type="one.sample")$n)
    dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
  resdf<-data.frame(n=as.character(res)
                    ,n_droput=as.character(dpres)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n","n (drop-out)")
  resdf
}

# Calculo de tamaño de muestra para dos medias (muestras independientes). Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss2mindep<-function(siglevel,power,alternative,dropout)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.2,0.5,0.8)
  for (i in 1:3)
  {
    res[i]<-ceiling(pwr.t.test(d = efsize[i], sig.level=siglevel,power=power,alternative=alternative,type="two.sample")$n)
    dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
  resdf<-data.frame(n1=as.character(res)
                    ,n1_droput=as.character(dpres)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n para cada grupo","n para cada grupo (drop-out)")
  resdf
}

# Calculo de tamaño de muestra para dos medias (muestras emparejadas). Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss2mempar<-function(siglevel,power,alternative,dropout)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.2,0.5,0.8)
  for (i in 1:3)
  {
    res[i]<-ceiling(pwr.t.test(d = efsize[i], sig.level=siglevel,power=power,alternative=alternative,type="paired")$n)
    dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
  resdf<-data.frame(n1=as.character(res)
                    ,n1_droput=as.character(dpres)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n para cada grupo","n para cada grupo (drop-out)")
  resdf  
}


# Calculo de tamaño de muestra para dos medias (tamaños distintos). Se fija nivel de significatividad, potencia, dropout.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)
ss2m2n<-function(siglevel,power,alternative,dropout)
{
  res1<-vector();res2<-vector()
  dpres1<-vector();dpres2<-vector()
  efsize<-c(0.2,0.5,0.8) 
  for (i in 1:3)
  {
    res1[i]<-ceiling(pwr.t.test(d = efsize[i], sig.level=siglevel,power=power,alternative=alternative,type="one.sample")$n)
    dpres1[i]<-ceiling(res1[i]+res1[i]*dropout)
    res2[i]<-ceiling(pwr.t2n.test(d = efsize[i], n1=res1[i],sig.level=siglevel,power=power,alternative=alternative)$n2)
    dpres2[i]<-ceiling(res2[i]+res2[i]*dropout)    
  }
  resdf<-data.frame(n1=as.character(res1)
                    ,n1_droput=as.character(dpres1)
                    ,n2=as.character(res2)
                    ,n2_droput=as.character(dpres2)
                    ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n grupo 1","n grupo 1 (drop-out)","n grupo 2","n grupo 2 (drop-out)")
  resdf  
}


# Calculo de tamaño de muestra para anova. Se fija nivel de significatividad, potencia, dropout y número de grupos.
# El resultado porporciona los tamaños para tres effects sizes (small,medium, large). Cohen (1988)

ssanova<-function(siglevel,power,dropout,grupos)
{
  res<-vector()
  dpres<-vector()
  efsize<-c(0.1,0.25,0.4)
  for (i in 1:3)
  {
      res[i]<-ceiling(pwr.anova.test(k=grupos,f = efsize[i], sig.level=siglevel,power=power)$n)
      dpres[i]<-ceiling(res[i]+res[i]*dropout)
  }
  resdf<-data.frame(n=as.character(res)
                    ,n_droput=as.character(dpres)
                      ,row.names=c("Small effect size","Medium effect size","Large effect size"))
  names(resdf)<-c("n para cada grupo","n para cada grupo (drop-out)")
  resdf
}

#Función para mostrar texto
texttoshow<-function(texto)
{
  texto
}

## Función para seleccinar y realizar el calculo de tamaño de muestra para una especidficación de test dada.

samplesize1<-function(siglevel,power,dropout,alternative,grupos,test)
{
  if(test=="1-p"){res<-ss1p(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)}
  if(test=="2-p.Tiguales"){res<-ss2p(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="2-p.Tdistintas"){res<-ss2p2n(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="1-m"){res<-ss1m(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="2-m.Mindependientes"){res<-ss2mindep(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="2-m.Memparejadas"){res<-ss2mempar(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="2-m.Tdistintas"){res<-ss2m2n(siglevel=siglevel,power=power,alternative=alternative,dropout=dropout)} 
  if(test=="ANOVA"){res<-ssanova(siglevel=siglevel,power=power,dropout=dropout,grupos=grupos)} 
  res
}



