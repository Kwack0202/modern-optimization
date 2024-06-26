source("C:/Users/coden/OneDrive/바탕 화면/석사1학기/확률모형/Blind_Search/blind.R")

binint=function(x,D)
{ x=rev(intToBits(x)[1:D]) # get D bits
# remove extra 0s from raw type:
as.numeric(unlist(strsplit(as.character(x),""))[(1:D)*2])
}

intbin=function(x) sum(2^(which(rev(x==1))-1))
# sum a raw binary object x (evaluation function):
sumbin=function(x) sum(as.numeric(x))
# max sin of binary raw object x (evaluation function):
maxsin=function(x,Dim) sin(pi*(intbin(x))/(2^Dim))
D=8 # number of dimensions
x=0:(2^D-1) # integer search space
# set full search space in solutions x D:
search=t(sapply(x,binint,D=D))
# set the domain values (D binary variables):
domain=vector("list",D)
for(i in 1:D) domain[[i]]=c(0,1) # bits
# sum of bits, fsearch:
S1=fsearch(search,sumbin,"max") # full search
cat("fsearch best s:",S1$sol,"f:",S1$eval,"\n")
# sum of bits, dfsearch:
S2=dfsearch(domain=domain,fn=sumbin,type="max")
cat("dfsearch best s:",S2$sol,"f:",S2$eval,"\n")
# max sin, fsearch:
Dim=length(search[1,]) # set Dim argument of maxim
S3=fsearch(search,maxsin,"max",Dim=Dim) # Dim used by maxsin
cat("fsearch best s:",S3$sol,"f:",S3$eval,"\n")
# max sin, dfsearch: Dim is used by maxin
S4=dfsearch(domain=domain,fn=maxsin,type="max",Dim=Dim)
cat("dfsearch best s:",S4$sol,"f:",S4$eval,"\n")