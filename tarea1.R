print("Leyendo datos...")
mydata = read.table("NuAstro_4yr_IceCube_Data.txt",stringsAsFactors = F)
print("Borrando eventos con entradas incompletas...")
good <- complete.cases(mydata)
cleandata <-mydata[good, ]
#Sacamos los valores de resolucion
resolution<-cleandata[2:54,8]
resolution<-as.numeric(resolution)

#Calculamos la media por loop
sum <-0
count <-0
for (index in 1:length(resolution)){
  sum <- sum + resolution[index]
  count<-count+1
}

print("La media es por la funcion mean: ")
mean(resolution)
print("La media es por loop: ")
print(sum/count)

#Sacamos las fechas en MJD
mjd<-cleandata[2:54,5]
mjd<-as.numeric(mjd)

#Funcion para convertir, tiempo origen. 86400 es para convertir a segundos.
mjd2posix = function(x){as.POSIXct('1858-11-17',tz='UTC')+x*86400}

#Convirtiendo todas las fechas
#Aqui tenia que inicializar el GMT, y si agregaba otra cosa se cambiaba a num
#Intente usando list, pero en ese caso leia como num.
GMT <- mjd2posix(0)
for (j in 2:length(mjd)+1){
  GMT[j] <- mjd2posix(mjd[j])
}
#Añadiendo las fechas a la lista de datos principal
cleandata<- cbind(cleandata,GMT)

#Funcion para hallar el numero de eventos en un mes dado.
evt_month<-function(month){
  z=0
  if (month == 1) {p<-"enero"}
  if (month == 2) {p<-"febrero"}
  if (month == 3) {p<-"marzo"}
  if (month == 4) {p<-"abril"}
  if (month == 5) {p<-"mayo"}
  if (month == 6) {p<-"junio"}
  if (month == 7) {p<-"julio"}
  if (month == 8) {p<-"agosto"}
  if (month == 9) {p<-"septiembre"}
  if (month == 10) {p<-"octubre"}
  if (month == 11) {p<-"noviembre"}
  if (month == 12) {p<-"diciembre"}
  for (i in 3:53){
    c<-as.character(months.POSIXt(GMT[i]))
    if(p==c){z<-(z+1)}
  }
  return(z)
}
maximo<-0
#Loop para hallar el mes con mas eventos
for(m in 1:12){
  maximo[m] <- evt_month(m)
}

month<-which.max(maximo)

if (month == 1) {p<-"enero"}
if (month == 2) {p<-"febrero"}
if (month == 3) {p<-"marzo"}
if (month == 4) {p<-"abril"}
if (month == 5) {p<-"mayo"}
if (month == 6) {p<-"junio"}
if (month == 7) {p<-"julio"}
if (month == 8) {p<-"agosto"}
if (month == 9) {p<-"septiembre"}
if (month == 10) {p<-"octubre"}
if (month == 11) {p<-"noviembre"}
if (month == 12) {p<-"diciembre"}
print("El mes con mas eventos es: ")
print(p)
#Haciendo el plot en coordenadas ecuatoriales
plot(cleandata[,7],cleandata[,6],main="Eventos en coordenadas ecuatoriales",xlab="Angulo de declinacion",ylab="Ascension Recta")

#Esperaria que haya una zona más activa en el cielo que corresponderia con
#la via lactea,ya que ahi estan todos los objetos interesantes.
#Asi que divido el cielo en zonas en rangos de ascension recta y podemos
#ver el histograma.


Ascension_Recta <- 0
for (EvIndex in 3:54){
  value<-as.numeric(cleandata[EvIndex,6])
  #Esto hace como funcion floor.
  rangeValue = as.integer(value/10)
  Ascension_Recta[EvIndex-2]<-(rangeValue+0.5)*10
  hist(Ascension_Recta)
  
}


