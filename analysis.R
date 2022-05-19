library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(Hmisc)
library(corrplot)

#read data files
hyips<-read.csv("total_analysis_hyip.csv")
uk_hyips_var_life<-read.csv("UK_hyip_variables_with_lifetime.csv")

#join both data files 
total<- hyips %>% left_join(uk_hyips_var_life)

# Survival function along with UK Address

total$uk <- !is.na(total$Registered.with.Companies.House)
km_fit_uk<-survfit(Surv(Lifetime.In.Days, Censored) ~ 1, data=total[total$UK==T,])

pdf("survival_plot.pdf",5,5)
plot(km_fit, xlim=c(1,xmax), xlab = " Lifetime (Days)", ylab = "Fraction of HYIPs still online after x Days", main = "Lifetimes of HYIPs (2018-2021)")
lines(km_fit_uk, xlim=c(1,xmax),  conf.int=FALSE, col="red")
legend(leg=c('Lifetime of all HYIPs (Observed)','Lifetime of all HYIPs (Observed) (95% CI)','Lifetime of all HYIPs with UK address (Observed)'),x='bottomleft',lty=c('solid','dashed','solid'), col=c('black','black','red'), cex = 0.7)
dev.off()

total$uk <- !is.na(total$Registered.with.Companies.House)
sf <- survfit(Surv(Lifetime.In.Days, Censored) ~ uk, data=total[!is.na(total$Lifetime.In.Days),])
survdiff(Surv(Lifetime.In.Days, Censored) ~ uk, data=total[!is.na(total$Lifetime.In.Days),]) 

#survival curve for UK addresses
ukadd <- survfit(Surv(Lifetime.In.Days, Censored) ~ Valid.Address, data = total[total$Same.Address==F,])

pdf("ukadd_survival_plot.pdf",5,5)
plot(ukadd[2],xlim=c(1,600), xlab = " Lifetime (Days)", ylab = "Fraction of HYIPs still online after x Days", main = "Lifetimes of HYIPs in the UK",conf.int=T, lwd=2)
lines(ukadd[1], xlim=c(1,600),col="blue", lty=3,conf.int=F, lwd=2)
lines(survfit(Surv(Lifetime.In.Days, Censored)~1, data=total[total$Same.Address==T,]),  xlim=c(1,600), col = "darkorange", lwd=2, conf.int=F, lty=4)
legend(leg=c('Valid UK address','Invalid UK address','Same UK address','Valid (95% CI)'),x='bottomleft',col=c('black','blue','darkorange','black'), lty=c(1,3,4,2), cex = 0.7, lwd=2)
dev.off()
survdiff(Surv(Lifetime.In.Days, Censored) ~ Valid.Address, data = total[total$Same.Address==F,])

#correlation matrix visualisation

cor_data<-total[,c(1,2,3,4,5,6,7,8,9,13,14,15,16,17,19,20,21)]
res <- rcorr(as.matrix(cor_data))
corrplot(res$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Cox Regression

cox_uk_var<-coxph(Surv(Lifetime.In.Days,Censored) ~ Valid.Address + Same.Address + Registered.with.Companies.House + Perfect.Money + Payeer + Bitcoin + Litecoin + Ethereum + Telegram + Facebook + Youtube + Twitter + Instagram + Goldcoders.license.check + Contact.Number, data = total)
summary(cox_uk_var)

cox_uk_nonbinary<-coxph(Surv(Lifetime.In.Days,Censored) ~ Valid.Address + Same.Address + Registered.with.Companies.House + Goldcoders.license.check + Contact.Number + Social.Media.Platforms + Payment.Processors, data = total)
summary(cox_uk_nonbinary)

cox_all<-coxph(Surv(Lifetime.In.Days,Censored) ~ Perfect.Money + Payeer + Bitcoin + Litecoin + Ethereum + Telegram + Facebook + Youtube + Twitter + Instagram + Goldcoders.license.check + Contact.Number, data = hyips)
summary(cox_all)

cox_all_with_ukaddress<-coxph(Surv(Lifetime.In.Days,Censored) ~ Perfect.Money + Payeer + Bitcoin + Litecoin + Ethereum + Telegram + Facebook + Youtube + Twitter + Instagram + Goldcoders.license.check + Contact.Number + UK.Address, data = hyips)
summary(cox_all_with_ukaddress)

cox_non_binary<-coxph(Surv(Lifetime.In.Days,Censored) ~ Goldcoders.license.check + Contact.Number + Social.Media.Platforms + Payment.Processors, data = hyips)
summary(cox_non_binary)