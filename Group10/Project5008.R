generalIRR <- function(x,rate){
  
  # Function that generates investment cash stream given k=IRR, and CF=y
  cashstream <- function(k,y){
    c <- rep(0,length(y)-1)
    c[1] <- -y[1]
    for(i in 2:length(c)){
      c[i] <- (1+k)*c[i-1]-y[i]
    }
    return(c)
  }
  
  # Function that calculates the PV of any stream of cash flow
  PV <- function(y,r){
    return(sum(y/((1+r)^(0:(length(y)-1)))))
  }
  
  ########## STEP 1 FIND ROOTS ############
  
  # since PV is a polynomial in (1/(1+rate))
  invroots <- polyroot(x)
  
  # After obtaining solution, we find IRR
  roots <- 1/invroots-1
  
  
  ############# STEP 2 FIND investment cash stream ###################
  
  # Generate a list of investment streams for each IRR
  investmentstreams <- lapply(roots,function(j){cashstream(j,x)})
  
  
  ############## STEP 3 FIND PV of real part of cash stream ####################
  
  # Calculate the PV of the Real part of the investment streams at r = rate
  PresentV <- sapply(investmentstreams,function(j){PV(Re(j),rate)})
  
  ############ Step 4 check if investment stream is net-borrow/invest #######
  
  netborrow <- (PresentV < 0)
  
  ########## Step 5 Decide whether to take project #################
  
  feasible <- (Re(roots) < rate)*netborrow + (Re(roots) > rate)*(1-netborrow)
  
  ######## Ideally all roots should yield same conclusion #########
  
  ans <- list(IRRs = roots, CashFlows = investmentstreams, RealPV = PresentV, 
              TakeProject = ifelse(feasible==1,TRUE,FALSE), NetBorrow = netborrow,
              NPV=PV(x,rate))
  return(ans)
} 

#Test Cashflow

set.seed(17)
x <- runif(6,min=-1000,1000)
x
# Check IRR against oppurtunity cost of 10%
test2 <- generalIRR(x,0.1) 
test2$NPV
# Check to make sure only 1 unique conclusion from the multiple IRR and what 
# the outcome is
unique(test2$TakeProject)

# Get the IRRs
test2$IRRs

# Get the Investment Streams for each IRR
test2$CashFlows[[1]]

# Get the PV of the investment streams discounted at oppoturnity cost
test2$RealPV

###### Test with r cost of capital = 0.7
test3 <- generalIRR(x,0.7)
test3$NPV
unique(test3$TakeProject)



