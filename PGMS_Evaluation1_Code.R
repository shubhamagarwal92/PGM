# install.packages("bnlearn")
# install.packages("Rgraphviz")
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(bnlearn)
library(Rgraphviz)
library(dplyr)
# Seed for reproducability
set.seed(0)
# File path
plotPath = "path/to/dir"
assetFilePath = "path/to/dir/Returns.txt"
plotPath = "~/Documents/msiam/courses/Probabilistic GM/plot40.png"
assetFilePath = "~/Documents/msiam/courses/Probabilistic GM/Returns250d.txt"
# Part 2 - Question 1
set.seed(0)
size = 100000
# Using linear regression model
# Offset = 0 
# Residual standard deviation = 1
# Simulating multivariate Gaussian
X1= rnorm(size,mean = 0,sd=1)
X2= rnorm(size,mean = 0,sd=1)
X3 = -X1 + rnorm(size,mean = 0,sd=1) # Adding noise
X4 = -2*X3 + rnorm(size,mean = 0,sd=1)
X5 = -X2 + rnorm(size,mean = 0,sd=1)
X6 = X1 + 2*X3 -X2 + rnorm(size,mean = 0,sd=1)
# Create dataFrame of these variables
dataFrame = data.frame(X1,X2,X3,X4,X5,X6)
# Growth Shrink Algorithm
bn.gs <- gs(dataFrame)
# Hill Climbing Algorithm
bn.hc <- hc(dataFrame)
# True representation of graph
trueGraph = model2network("[X1][X2][X3|X1][X4|X3][X5|X2][X6|X1:X2:X3")
# plot(trueGraph)
## Boolean function to decide whether graph 
## is equal to true graph
compareWithTrueGraph = function(trueGraph,derivedGraph){
  stats = compare(trueGraph, derivedGraph)
  if(stats$fp !=0 | stats$fn !=0 |stats$tp ==0){
    return(FALSE)
  }
  else {return(TRUE)}
}
# all.equal: Also gives boolean or text
# all.equal(bn.hc, bn.gs)
isTrueGraphHC = compareWithTrueGraph(trueGraph,bn.hc)
isTrueGraphGS = compareWithTrueGraph(trueGraph,bn.gs)
# Plot both the graphs
# Save the file
# png(plotPath)
par(mfrow = c(1,2))
plot(bn.gs, main = "Growth Shrink")
plot(bn.hc, main = "Hill-Climbing")
# dev.off()
# Compare with true graph
gsCompare = as.data.frame(compare(trueGraph, bn.gs)) #Converting to dataframe
hcCompare = as.data.frame(compare(trueGraph, bn.hc))
# Another way to plot
# par(mfrow = c(1,2))
# graphviz.plot(bn.hc)
# graphviz.plot(bn.gs)

assetFrame = read.delim(assetFilePath,sep = " ")

## Part 2 - Section 2.2
assetFrame = assetFrame %>% select(AIR.FRANCE.KLM,ALCATEL.LUCENT,AXA,FAURECIA,
                                   GAUMONT, GEODIS, PPR,UNION.FINC.FRANC.)
assetFrame = na.omit(assetFrame)
# Suppressing Warnings
asset.gs <- suppressWarnings(gs(assetFrame))
asset.hc <- hc(assetFrame)
# Already defined plotting area
# par(mfrow = c(1,2))
plot(asset.gs, main = "Growth Shrinkw")
plot(asset.hc, main = "Hill-Climbing")

l = c("AIR.FRANCE.KLM","ALCATEL.LUCENT","AXA","FAURECIA","GAUMONT", "GEODIS", "PPR","UNION.FINC.FRANC.")
# To create dataframe of marginal independencies
getMI = function(x,y){
  c=ci.test(x,y,data=assetFrame)
  return(c$p.value)
}

df = expand.grid(x=l,y=l)
df = df %>% filter(df$x != df$y)
# df = as.data.frame(t(combn(l,2)))
df$p_value <- apply(df[,c('x','y')],1,function(z) getMI(z['x'],z['y']))
dfMI = df %>% filter(df$p_value>0.05)

# To create data frame of conditional independencies between 3 variables
getCI = function(x,y,z){
  c=ci.test(x,y,z,data=assetFrame)
  return(c$p.value)
}  
df2 = expand.grid(x=l,y=l,z=l)
df2 = df2 %>% filter(df2$x != df2$y) 
df2 = df2 %>% filter(df2$x != df2$z) 
df2 = df2 %>% filter(df2$y != df2$z)
df2$p_value <- apply(df2[,c('x','y','z')],1,function(a) 
  getCI(a['x'],a['y'],a['z']))
df2CI = df2 %>% filter(df2$p_value>0.05)

# GS states that GEODIS and UNION.FINC.FRANC should be independent but HC doesn't. 
# By ci.test we find that GEODIS and UNION.FINC.FRANC dependent.
ci.test("GEODIS", "UNION.FINC.FRANC.",data=na.omit(assetFrame))

# GS states that GEODIS and AIR.FRANCE.KLM should be independent but HC does not. 
# Conversely, we find that GEODIS and AIR.FRANCE.KLM are independent.
ci.test("GEODIS", "AIR.FRANCE.KLM",data=na.omit(assetFrame))

# HC says that GEODIS and GAUMONT should be independent given ALCATEL.LUCENT while GS doesn???t. 
# By ci.test we find GS to be true.
ci.test("GEODIS", "GAUMONT", "ALCATEL.LUCENT",data=na.omit(assetFrame))

# GS says that GEODIS and AIR.FRANCE.KLM should be independent given PPR while HC does not
# By ci.test we find GS to be true.
ci.test("GEODIS", "AIR.FRANCE.KLM", "PPR",data=na.omit(assetFrame))

# Both say ALCATEL.LUCENT and GAUMONT dependent which was found to be true by ci.test
ci.test("GAUMONT", "ALCATEL.LUCENT",data=na.omit(assetFrame))

# Both say GEODIS and UNION.FINC.FRANC conditionally dependent given GAUMONT
# It can be easily shown that they are conditional dependent by ci.test
ci.test("GEODIS", "UNION.FINC.FRANC.", "GAUMONT",data=na.omit(assetFrame))