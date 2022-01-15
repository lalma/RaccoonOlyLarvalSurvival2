# example while loop

n_valid_rep <- 500
valid_rep_counter <- 0
while(valid_rep_counter < n_valid_rep) {
  # make sim data to test if decreasing
  if(decreasing == TRUE){ #test whether the data are decreaseing
    #add valid data to the dataframe
    valid_rep_counter <- valid_rep_counter + 1
  }
}



#####CI5 as ref, CI20 deleted
CI5ref = filter(bigcox, !(site %in% "CI20"))
write.csv(CI5ref, file = "CI5ref.csv")
View(CI5ref)
nrow(bigcox)
nrow(CI5ref)

#DB ref, CI 20 and 5 deleted
DB5ref = filter(bigcox, !(site %in% c("CI5","CI20")))
write.csv(CI5ref, file = "CI5ref.csv")
View(CI5ref)
nrow(bigcox)
nrow(DB5ref)

#PW ref, all other delted
PW5ref = filter(bigcox, !(site %in% c("CI5","DB","CI20")))
write.ci5(CI5ref, file = "CI5ref.ci5")
View(CI5ref)
nrow(bigcox)
nrow(DB5ref)
nrow(PW5ref)
head(PW5ref)

View(sf)
cox<-coxph(Surv(day[1],status[1])~site[k] +treatment[k], data=rep)

coxout = tidy(cox)
coxout
class(coxout)
coxout
write.csv("coxout.csv")
write.csv(coxout, file = "coxout.csv" )


################## PM : Question about the plot. Shouldnt we have much fatter looking confience intervals given we have all the replicates?
head(bigcox)

R1<-subset(bigcox, rep_id=="1")
KMsurv = Surv(time = R1$day, R1$status, type = "right")
sf <- survfit(KMsurv ~ site + treatment, data = R1)
coxR1<-coxph(KMsurv~treatment*site, data=R1)
S1<-summary(coxR1)
S1
out1 = tidy(coxR1)
out1
write.csv(here("out1.csv"))

View(sf)
cox<-coxph(Surv(day[1],status[1])~site[k] +treatment[k], data=rep)

coxout = tidy(cox)
coxout
class(coxout)
coxout
write.csv("coxout.csv")
write.csv(coxout, file = "coxout.csv" )


##notes
cox1<-coxph(KMsurv~Treatment+Location, data=OlyLarvaeKMforR)
cox<-coxph(KMsurv~site + treatment, data=rep)
coxph(Surv(day, status) ~ treatment+site+treatment:site, data = rep)

View(rep)
sf
write.csv(cox1, file = "cox1.csv")

d_expand <- data.frame(jar_id = rep(count$jar_id[1], times = count$sim_count[1]))  %>% #create a data frame of that jar
  
  mutate(rep_id = count$rep_id[1],  #create the number of rows that corresponds to the 
         #number of simulated larvae on day 1
         treatment = count$treatment[1], 
         site = count$site[1], 
         day = count$day[nrow(count)],
         status = 0)

