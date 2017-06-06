#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("C:/Apps/projects/Capex/")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory

source("./code/tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "mlr", "corrplot", "plotly", "ModelMetrics", "lubridate", "fitdistrplus", "scales", "zoo", "CDFt", "goftest", "spatstat")
load_libs(reqPackages)



#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------
# d_path = "C:/Apps/projects/Capex/data/data.csv"
# 
# d <- read.csv(d_path, header=TRUE)
# 
# # correct names
# colnames(d) <- gsub("\\.+", ".", colnames(d))
# colnames(d) <- gsub("\\.$", "", colnames(d))
# 
# d <- d %>%
#       filter(Exclude=="") %>%
#       dplyr::select(Categories, Code.Name, Directorate:Remote.or.Industrial.Zone,
#                     Capex.Estimate.at.FID.100, As.Built.cost, Cost.Variance, ROUNDED.CV_PCT, ROUNDED.SV_PCT)
# 
# d$ROUNDED.SV_PCT <- gsub("%", "", d$ROUNDED.SV_PCT)
# 
# # correct var type
# d$Capex.Estimate.at.FID.100  <- as.numeric(as.character(d$Capex.Estimate.at.FID.100))
# d$As.Built.cost <- as.numeric(d$As.Built.cost)
# d$Cost.Variance <- as.numeric(d$Cost.Variance)
# d$ROUNDED.CV_PCT <- as.numeric(d$ROUNDED.CV_PCT)
# d$ROUNDED.SV_PCT <- as.numeric(d$ROUNDED.SV_PCT)
# 
# saveRDS(d, "./data/dat.rds") # only cost
# saveRDS(d, "./data/dat2.rds") # include cost and schedule


d <- readRDS("./data/dat.rds")
table(d$Categories)

oil <- d %>% filter(Oil.Gas=="Oil")
table(oil$Onshore.Offshore.Deep, oil$Categories)

cat <- d %>% filter(Oil.Gas=="Oil", Onshore.Offshore.Deep=="Onshore", Categories=="Below $1 bln")




# by_cat <- d %>% group_by(Categories) %>% 
#                 mutate(tot_cap=sum(Capex.Estimate.at.FID.100))


#d1to5 <- by_cat %>% filter(Categories=="$1-5 Bln") %>% filter(Cost.Variance==0.03)



# sim_sample <- function(cat, dat){
#   # cat: category name/string
#   # dat: category dataset
#   # simulate samples based on observed data
#   
#   d <- dat %>% filter(Categories==cat) %>% 
#                group_by(ROUNDED.CV_PCT) %>% 
#                mutate(cap_vargrp=sum(Capex.Estimate.at.FID.100)) %>% # tot capex at given var
#                mutate(w=cap_vargrp/tot_cap) %>% # weight for each var based on capex@var/tot capex
#                distinct(ROUNDED.CV_PCT, .keep_all=T)
#   
#   x <- d$ROUNDED.CV_PCT
#   min <- min(x)
#   max <- max(x)
#   
#   x.s <- (x-min)/(max-min)  # scale to 0~1
#   
#   return(list(sample(x=x.s, size=10000, replace=T, prob=d$w), min, max))
#   
# }


# cat = "$1-5 Bln" 
# cat = "Below $1 bln" 
# cat ="Greater $5 bln"

# sim <- sim_sample(cat, by_cat)
# d.sim = sim[[1]]
# min = sim[[2]]
# max = sim[[3]]

sample = cat$ROUNDED.CV_PCT
min = min(sample)
max = max(sample)
s <- (sample-min)/(max-min)  # scale sample to 0~1

# plot emp cdf
title = paste0(cat, " Empirical CDF")
plot(ecdf(s), xlab="Cost Variance", main=title)

fit.mme <- fitdist(d.sim, "beta", method="mme")
fit.mge <- fitdist(d.sim, "beta", method="mge")
fit.mge <- fitdist(d.sim, "beta", fix.arg=c(1.09, 1.68))
fit.mge2 <- fitdist(d.sim, "gamma", method="mge")
#fit.mle <- fitdist(d.sim, "beta", method="mle")
fit.qme <- fitdist(d.sim, "beta", method="qme", probs=c(0.1,0.5))

par(lwd=3)
cdfcomp(list(fit.mme, fit.mge, fit.qme), legendtext=c("Beta.MME", "Beta.MGE", "Beta.QME"))
qqcomp(list(fit.mme, fit.mge, fit.qme), legendtext=c("Beta.mme", "Beta.mge", "Beta.qme"))
gofstat(list(fit.mme, fit.mge, fit.qme), fitnames=c("Beta.mme", "Beta.mge", "Beta.qme"))

gofstat(fit.mme)


fit <- fit.mge
summary(fit)
plot(fit)

p10=qbeta(0.1, fit$estimate[1], fit$estimate[2] )
p50=qbeta(0.5, fit$estimate[1], fit$estimate[2] )
p90=qbeta(0.9, fit$estimate[1], fit$estimate[2] )

p10*(max-min)+min
p50*(max-min)+min
p90*(max-min)+min

y=function (x) dbeta(x,fit$estimate[1], fit$estimate[2])
curve(y, from=0, to=1, col="blue", yaxp = c(0, 3, 30))

# plot(fit, demp = TRUE)
# plot(fit, histo = FALSE, demp = TRUE)
# cdfcomp(fit, addlegend=FALSE)
# denscomp(fit, addlegend=FALSE)
# ppcomp(fit, addlegend=FALSE)
# qqcomp(fit, addlegend=FALSE)

# Chisq test
p <- hist(d.sim, breaks=20, include.lowest=FALSE, right=FALSE)
breaks_cdf <- pbeta(p$breaks, shape1=1.088349, shape2=1.684920)
null.probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])
a <- chisq.test(p$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)

# Cramér-von Mises criterion
cvm.test(d.sim, "pbeta", shape1=1.088349, shape2=1.684920)
cvm.test(d.sim, "pbeta", shape1=2.22, shape2=12.95)
cvm.test(d.sim, "pgamma", shape=2.55450, rate=17.02265)
#ad.test(dat, "pbeta", shape1=1.073824, shape2=1.582213)


# Kolmogorov-Smirnov test
num_of_samples = 100000
y <- rbeta(num_of_samples, shape1=1.088349, shape2=1.684920)
ks.test(d.sim, y)
