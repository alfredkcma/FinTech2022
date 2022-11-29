require(ggplot2)
# cash flow
#xflow = c(-1,6,-11,6)

# find all the internal rate of return

allirr <- function(xflow)
{
  return(polyroot(rev(xflow))-1)
}

# function to find the investment flow c from cash flow , given an internal return ir.
cflowFx <- function(xflow, ir)
{
  cfidx <- function(idx)
  {
    return(-sum(rev(OneRSeq[1:idx])*xflow[1:idx]))
    
  }
  Lxflow = length(xflow)
  OneRSeq = (1+ir)^seq(0,(Lxflow-1))
  cflow = rep(0, Lxflow-1)
  cflow = sapply(1:(Lxflow-1), cfidx)
  return(cflow)
}

# find the investment flow for the cash flow (-1,6,-11,6)
# given k = 0, 1, 2
#cflowFx(xflow, 0)
#cflowFx(xflow, 1)
#cflowFx(xflow, 2)

#function that calculate the pv of a given cash flow and rate ir.
pvfunc <- function(xflow,ir)
{
  Lxflow = length(xflow)
  OneRSeq = (1+ir)^seq(0,(Lxflow-1))
  disfactor = 1/OneRSeq
  pv = sum(xflow*disfactor)
  pv
}

PlotPVdf <- function(xflow)
{
  
  discrates = Re(polyroot(rev(xflow))-1)
  discrates
  R_range = seq(min(discrates),max(discrates),length.out = 1000)
  R_range = R_range[(R_range> (-0.9)) | (R_range < (-1.1))]
  Lxflow = length(xflow)
  
  pvfunc <- function(ir)
  {
    OneRSeq = (1+ir)^seq(0,(Lxflow-1))
    disfactor = 1/OneRSeq
    pv = sum(xflow*disfactor)
    pv
  }
  pv_s = sapply(R_range, pvfunc)
  pvdf = data.frame(r=R_range, pv = pv_s)
  return(pvdf)
  
}

#ggplot(plotdf,aes(x = r,y = pv))+geom_line()

#xflow = c(-5,1.2,1.5,1,-1.8,7,2)

#plotdf = PlotPVdf(xflow)

#ggplot(plotdf,aes(x = r,y = pv))+geom_line()
