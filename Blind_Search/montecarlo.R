mcsearch=function(fn,lower,upper,N,type="min",...)
{ D=length(lower)
s=matrix(nrow=N,ncol=D) # set the search space
for(i in 1:N) s[i,]=runif(D,lower,upper)
fsearch(s,fn,type,...) # best solution
}