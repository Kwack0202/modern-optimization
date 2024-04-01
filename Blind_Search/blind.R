fsearch=function(search,fn,type="min",...)
{
  x=apply(search,1,fn,...) # run fn over all search rows
  ib=switch(type,min=which.min(x),max=which.max(x))
  return(list(index=ib,sol=search[ib,],eval=x[ib]))
}

dfsearch=function(l=1,b=1,domain,fn,type="min",D=length(domain),
                  par=rep(NA,D),
                  bcur=switch(type,min=list(sol=NULL,eval=Inf),
                              max=list(sol=NULL,eval=-Inf)),
                  ...)
{ if((l-1)==D) # "leave" with solution par to be tested:
{ f=fn(par,...);fb=bcur$eval
ib=switch(type,min=which.min(c(fb,f)),
          max=which.max(c(fb,f)))
if(ib==1) return (bcur) else return(list(sol=par,eval=f))
}
  else # go through sub branches
  { for(j in 1:length(domain[[l]]))
  { par[l]=domain[[l]][j]
  bcur=dfsearch(l+1,j,domain,fn,type,D=D,
                par=par,bcur=bcur,...)
  }
    return(bcur)
  }
}