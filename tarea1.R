mydata = read.table("NuAstro_4yr_IceCube_Data.txt",stringsAsFactors = F)
good <- complete.cases(mydata)
cleandata <-mydata[good, ]
resolution<-cleandata[2:54,8]
resolution<-as.numeric(resolution)
mean(resolution)
summary(resolution)
mjd<-cleandata[2:54,5]
mjd<-as.numeric(mjd)

mjd2posix = function(x){as.POSIXct('1858-11-17',tz='UTC')+x*86400}

GMT <- mjd2posix(0)
for (j in 2:length(mjd)+1){
  GMT[j] <- mjd2posix(mjd[j])
}
cleandata<- cbind(cleandata,GMT)

evt_month<-function(data,month){
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

for(m in 1:12){
  max[m] <- evt_month(cleandata,m)
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
  
plot(cleandata[,8],cleandata[,7])


