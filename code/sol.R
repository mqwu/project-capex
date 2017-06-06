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
d <- readRDS("./data/dat.rds")
table(d$Categories)

d <- d %>% mutate(cat1=case_when(.$Capex.Estimate.at.FID.100<=1000 ~ "0-1 bln",
                                 .$Capex.Estimate.at.FID.100>1000&.$Capex.Estimate.at.FID.100<=2000 ~ "1-2 bln",
                                 .$Capex.Estimate.at.FID.100>2000&.$Capex.Estimate.at.FID.100<=3000 ~ "2-3 bln",
                                 .$Capex.Estimate.at.FID.100>3000&.$Capex.Estimate.at.FID.100<=4000 ~ "3-4 bln",
                                 .$Capex.Estimate.at.FID.100>4000&.$Capex.Estimate.at.FID.100<=5000 ~ "4-5 bln",
                                 .$Capex.Estimate.at.FID.100>5000 ~ "above 5 bln"
                                 ))


oil <- d %>% filter(Oil.Gas=="Oil")
table(oil$Onshore.Offshore.Deep, oil$Categories)

table(oil$Onshore.Offshore.Deep, oil$cat1)

# 
# cat <- oil %>% filter(Onshore.Offshore.Deep=="Onshore", Categories=="Below $1 bln")
# cat <- oil %>% filter(Onshore.Offshore.Deep=="Shallow water", Categories=="Below $1 bln")
# cat <- oil %>% filter(Onshore.Offshore.Deep %in% c("Deepwater","DW"), Categories=="$1-5 Bln")


gas <- d %>% filter(Oil.Gas=="Gas")
table(gas$Onshore.Offshore.Deep, gas$Categories)

table(gas$Onshore.Offshore.Deep, gas$cat1)

cat <- gas %>% filter(Onshore.Offshore.Deep=="Shallow water", Categories=="Below $1 bln")
cat <- gas %>% filter(Onshore.Offshore.Deep=="Shallow water", Categories=="$1-5 Bln")

cat <- gas %>% filter(Onshore.Offshore.Deep=="LNG", Categories=="$1-5 Bln", Greenfield.GF.or.Brownfield.BF=="GF", Remote.or.Industrial.Zone=="RM")
cat <- gas %>% filter(Onshore.Offshore.Deep=="LNG", Categories=="Greater $5 bln", Greenfield.GF.or.Brownfield.BF=="GF", Remote.or.Industrial.Zone=="RM")

cat <- gas %>% filter(Onshore.Offshore.Deep=="LNG", cat1!="0-1 bln", Greenfield.GF.or.Brownfield.BF=="GF", Remote.or.Industrial.Zone=="RM")
cat <- gas %>% filter(Onshore.Offshore.Deep=="LNG", cat1!="0-1 bln", cat1!="above 5 bln", Greenfield.GF.or.Brownfield.BF=="GF", Remote.or.Industrial.Zone=="RM")
cat <- gas %>% filter(Onshore.Offshore.Deep=="LNG", cat1=="4-5 bln"|cat1=="above 5 bln", Greenfield.GF.or.Brownfield.BF=="GF", Remote.or.Industrial.Zone=="RM")

cat <- gas %>% filter(Onshore.Offshore.Deep=="Shallow water", cat1=="0-1 bln"|cat1=="1-2 bln")


oilprod <- d %>% filter(Oil.Gas=="Oil products")
cat <- oilprod %>% filter(Categories=="Below $1 bln")

table(oilprod$Onshore.Offshore.Deep, oilprod$cat1)

sample = cat$ROUNDED.CV_PCT
min = min(sample)
max = max(sample)
s <- (sample-min)/(max-min)  # scale sample to 0~1

# plot emp cdf
#title = paste0("Oil Products", "(less than $1 bln)", " Empirical CDF")
#title = paste0("Gas ", "LNG Greenfield Remote ", "(Greater $5 bln) ", "Empirical CDF")
title = paste0("Gas LNG Greenfield Remote ", "(greater than $4 bln)", " Empirical CDF")
#title = paste0("Gas Shallow water ", "(less than $2 bln)", " Empirical CDF")
 
png(paste0("./doc/fig/",title, ".png"), width=650, height=650)
par(lwd=2)
plot(ecdf(s), xlab="Cost Variance", main=title)
dev.off()

fit.mme <- fitdist(s, "beta", method="mme")
fit.mge <- fitdist(s, "beta", method="mge")
#fit.mge <- fitdist(s, "beta", fix.arg=c(1.09, 1.68))
fit.mge2 <- fitdist(s, "gamma", method="mge")
#fit.mle <- fitdist(d.sim, "beta", method="mle")
fit.qme <- fitdist(s, "beta", method="qme", probs=c(0.1,0.5))

par(lwd=3)
cdfcomp(list(fit.mme, fit.mge, fit.qme, fit.mge2), legendtext=c("Beta.MME", "Beta.MGE", "Beta.QME", "Gamma.MGE"))
qqcomp(list(fit.mme, fit.mge, fit.qme, fit.mge2), legendtext=c("Beta.mme", "Beta.mge", "Beta.qme", "Gamma.mge"))
gofstat(list(fit.mme, fit.mge, fit.qme, fit.mge2), fitnames=c("Beta.mme", "Beta.mge", "Beta.qme", "Gamma.mge"))
#gofstat(fit.mme)

fit <- fit.mge # beta
fit <- fit.mge2 # gamma
summary(fit)
png(paste0("./doc/fig/", title, " Diag.png"), width=650, height=650)
par(lwd=3)
plot(fit)
dev.off()

p10=qbeta(0.1, fit$estimate[1], fit$estimate[2] )
p50=qbeta(0.5, fit$estimate[1], fit$estimate[2] )
p90=qbeta(0.9, fit$estimate[1], fit$estimate[2] )


# p10=qgamma(0.1, shape=fit$estimate[1], rate=fit$estimate[2] )
# p50=qgamma(0.5, shape=fit$estimate[1], rate=fit$estimate[2] )
# p90=qgamma(0.9, shape=fit$estimate[1], rate=fit$estimate[2] )

out=data.frame(Catergory=title, n=length(s),
               P10=(p10*(max-min)+min)*100, P50=(p50*(max-min)+min)*100, P90=(p90*(max-min)+min)*100,
               alpha=fit$estimate[1], beta=fit$estimate[2])
write.table(out, "./doc/quantile.csv", row.names=F, append=T, quote=F, sep=",", col.names=F)



# plot(fit, demp = TRUE)
# plot(fit, histo = FALSE, demp = TRUE)
# cdfcomp(fit, addlegend=FALSE)
# denscomp(fit, addlegend=FALSE)
# ppcomp(fit, addlegend=FALSE)
# qqcomp(fit, addlegend=FALSE)

# Chisq test
# p <- hist(d.sim, breaks=20, include.lowest=FALSE, right=FALSE)
# breaks_cdf <- pbeta(p$breaks, shape1=1.088349, shape2=1.684920)
# null.probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])
# a <- chisq.test(p$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)

title
# # Cramér-von Mises criterion
cvm.test(s, "pbeta", shape1=fit$estimate[1], shape2=fit$estimate[2])
cvm.test(s, "pgamma", shape=fit$estimate[1], rate=fit$estimate[2])

cvm.test(x, "pbeta", shape1=fit$estimate[1], shape2=fit$estimate[2])

ad.test(x, "pbeta", shape1=fit$estimate[1], shape2=fit$estimate[2])



# 
# 
# # Kolmogorov-Smirnov test
num_of_samples = 100000
y <- rbeta(num_of_samples, shape1=fit$estimate[1], shape2=fit$estimate[2])
x <- rbeta(num_of_samples, shape1=0.797966134, shape2=2.322830201)
#y <- rgamma(num_of_samples, shape=fit$estimate[1], rate=fit$estimate[2])

ks.test(x, y)
ks.test(s, y)
