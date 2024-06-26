### grid.R file ###
# standard grid search method (uses fsearch)
gsearch=function(fn,lower,upper,step,type="min",...)
{ D=length(step) # dimension
domain=vector("list",D) # domain values
L=vector(length=D) # auxiliary vector
for(i in 1:D)
{ domain[[i]]=seq(lower[i],upper[i],by=step[i])
L[i]=length(domain[[i]])
}
LS=prod(L)
s=matrix(ncol=D,nrow=LS) # set the search space
for(i in 1:D)
{
if(i==1) E=1 else E=E*L[i-1]
s[,i]=rep(domain[[i]],length.out=LS,each=E)
}
fsearch(s,fn,type,...) # best solution
}


# standard grid search method (uses dfsearch)
gsearch2=function(fn,lower,upper,step,type="min",...)
{ D=length(step) # dimension
domain=vector("list",D) # domain values
for(i in 1:D) domain[[i]]=seq(lower[i],upper[i],by=step[i])
dfsearch(domain=domain,fn=fn,type=type,...) # solution
}


# nested grid search method (uses fsearch)
ngsearch=function(fn,lower,upper,levels,step,type,...)
{ stop=FALSE;i=1 # auxiliary objects
bcur=switch(type,min=list(sol=NULL,eval=Inf),
max=list(sol=NULL,eval=-Inf))
while(!stop) # cycle while stopping criteria is not met
{
s=gsearch(fn,lower,upper,step,type,...)
# if needed, update best current solution:
if( (type=="min" && s$eval<bcur$eval)||
(type=="max" && s$eval>bcur$eval)) bcur=s
if(i<levels) # update step, lower and upper:
{ step=step/2
interval=(upper-lower)/4
lower=sapply(lower,max,s$sol-interval)
upper=sapply(upper,min,s$sol+interval)
}
if(i>=levels || sum((upper-lower)<=step)>0) stop=TRUE
else i=i+1
}
return(bcur) # best solution
}