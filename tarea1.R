mydata = read.table("NuAstro_4yr_IceCube_Data.txt")
good <- complete.cases(mydata)
cleandata <-mydata[good, ]