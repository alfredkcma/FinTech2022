library(FinancialMath)
library(jrvFinance)
cf<-c()
r = 0

############Hazon's Criterion#############


IRRform <- function(x, r){
  npvX = npv(cf=x,rate=r,immediate.start=TRUE)
  n = length(x)-1
  res = 0
  res<- matrix(nrow=n+1, ncol=n+4)
  res[1,1:(n+1)]=x
  res[2:(n+1),1]<- -x[1]
  ki <- polyroot(rev(x))-1
  k = Re(ki)
  npvC = 0
  res[2:(n+1),(n+2)]=ki
  res[1,(n+3)]=npvX
  if(npvX>0){res[1,(n+4)]="invest"}
  else if(npvX<0){res[1,(n+4)]="not invest"}
  else{res[1,(n+4)]="indifferent"}
  rownamer = 0
  rownamer[1] = "x"
  for(i in (1:n)){
    rownamer[1+i] = i
    c = 0
    for(j in (2:n)){
      c[1] = -x[1]
      c[j] = -x[j]+(1+ki[i])*c[j-1]
      res[1+i,j]<-c[j]
    }
    npvC[i] = npv(cf=Re(c),rate=r,immediate.start=TRUE)
    res[1+i,(n+3)] = npvC[i]
    if(npvC[i]*(k[i]-r)>0){
      res[1+i,(n+4)]= "invest"
    }else if(npvC[i]*(k[i]-r)<0){res[1+i,(n+4)] = "not invest"}
    else if (npvC[i]*(k[i]-r)==0){res[1+i,(n+4)] = "indifferent"}
    else{res[1+i,(n+4)] = "error"}
  }
  j = seq(from = 0,to=length(x)-1,by = 1)
  colnamer = c(j,"IRR","NPV","Decision")
  res[is.na(res)]=" "
  data = data.frame(res)
  colnames(data) = colnamer
  rownames(data) = rownamer
  return(data)
}


############Calculate npv & npvc#############

NPVC<-function(cf, r){
  npv <- npv(cf=cf,rate=r,immediate.start=TRUE)
  irr <- polyroot(rev(cf))-1
  t <- length(cf)-1  
  c <- matrix(nrow=t,ncol=t)
  npvc <- 0
  c[,1] <- -cf[1]
  for(i in (1:t)){
    k<-irr[i]
    for(j in (2:t)){
      c[i,j] <- (1+k)*c[i,j-1]-cf[j]
      }
    npvc[i] <- npv(cf=Re(c[i,]),rate=r,immediate.start=TRUE)
  }
return(npvc)
}

#####################AIRR function############

AIRR <- function(x,c,r){
  i = 0
  k = 0
  n = length(x)-1
  for(j in (1:(n-1))){
    k[j] = (c[j+1]+x[j+1])/c[j]-1
    i[j] = k[j]*c[j]
  }
  k[n] = x[n+1]/c[n]-1
  i[n] = k[n]*c[n]
  ave = npv(cf=Re(i),rate=r)/npv(cf=Re(c),rate=r)
  return(ave)
}


#################generating aveK with C########################
AIRRctok <- function (x,c,r){
  i = 0
  k = 0
  n = length(x)-1
  for(j in (1:(n-1))){
    k[j] = (c[j+1]+x[j+1])/c[j]-1
    i[j] = k[j]*c[j]
  }
  k[n] = x[n+1]/c[n]-1
  i[n] = k[n]*c[n]
  ave = npv(cf=Re(i),rate=r)/npv(cf=Re(c),rate=r)
  return(k)
}


############################
#####Run from Here##########
cf.def = 0
freq.def = 0
n = 0
repeat{
  {
    cf.def[n+1] = as.numeric(readline("Please give a cashflow input: "))
    freq.def[n+1] = as.numeric(readline("Time for cf: "))
  }
  n = n+1
  if (readline(prompt = "Do you want to add more? Y/N: ") != "Y") {
    if (readline(prompt = "Confirm your cash flow input? Y/N: ") != "Y") {
      cf.def = 0
      freq.def = 0
      n = 0
    } else {break}
  }
}

c1.def = 0
freq1.def = 0
n = 0

repeat{
  {
    c1.def[n+1] = as.numeric(readline("Please give a investment stream input (please input 0 at the end): "))
    freq1.def[n+1] = as.numeric(readline("Time for c1: "))
  }
  n = n+1
  if (readline(prompt = "Do you want to add more? Y/N: ") != "Y") {
    if (readline(prompt = "Confirm your investment stream input? Y/N: ") != "Y") {
      c1.def = 0
      freq1.def = 0
      n = 0
    } else {break} 
  }}


r<-as.numeric(readline(prompt = "What is the interest rate: "))


cf = 0
freq = 0
for(i in 1:length(cf.def)){
  cf <- append(cf,rep(cf.def[i],freq.def[i]))
}
cf <- cf[-1]

freq <- seq(0,length(cf)-1,by=1)

c1 = 0
freq1 = 0
for(i in 1:length(c1.def)){
  c1 <- append(c1,rep(c1.def[i],freq.def[i]))
}
c1 <- c1[-1]

c1.k <- AIRRctok(cf.def,c1,0.05)

###AIRR&NPVC
AIRR <- AIRR(cf,c1,r=r)
npvC = npv(c1,rate=r)

###decision making
Decision <- if(npvC*(AIRR-r)>0){
  out = "invest"
} else {
  out = "not invest"
}

IRR <- IRRform(cf,r)
cat("The Hazon's (2003) Criterion gives","\n")
print(IRR)

cat("For Magni's (2010) AIRR, the investment stream input is ",c1,"\n")
cat("The corresponding kt is",c1.k,"\n")
cat("the present value of the investment stream is ",npvC,"\n")
cat("the AIRR result is: ",AIRR,"\n")

cat("The decision is: ",Decision,"\n")


