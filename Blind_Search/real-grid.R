source("C:/Users/coden/OneDrive/바탕 화면/석사1학기/확률모형/Blind_Search/blind.R") # load the blind search methods
source("C:/Users/coden/OneDrive/바탕 화면/석사1학기/확률모형/Blind_Search/grid.R") # load the grid search methods
# real value functions: sphere and rastrigin:
sphere=function(x) sum(x^2)
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
cat("sphere:\n") # D=2, easy task
S=gsearch(sphere,rep(-5.2,2),rep(5.2,2),rep(1.1,2),"min")
cat("gsearch s:",S$sol,"f:",S$eval,"\n")
S=ngsearch(sphere,rep(-5.2,2),rep(5.2,2),3,rep(3,2),"min")
cat("ngsearch s:",S$sol,"f:",S$eval,"\n")
cat("rastrigin:\n") # D=2, easy task
S=gsearch(rastrigin,rep(-5.2,2),rep(5.2,2),rep(1.1,2),"min")
cat("gsearch s:",S$sol,"f:",S$eval,"\n")
S=ngsearch(rastrigin,rep(-5.2,2),rep(5.2,2),3,rep(3,2),"min")
cat("ngsearch s:",S$sol,"f:",S$eval,"\n")