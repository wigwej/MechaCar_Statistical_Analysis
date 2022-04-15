MechaCar_Data<-read.csv("MechaCar_mpg.csv")
Mecha_Math<-lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, MechaCar_Data)
summary(Mecha_Math)


Suspension_Data<-read.csv("Suspension_Coil.csv")
all_lots<-Suspension_Data%>%summarise(Mean=mean(PSI), Median=median(PSI),
                            Variance=var(PSI), StandardDeviation=sd(PSI))

three_lots<-Suspension_Data%>%group_by(Manufacturing_Lot)%>%summarise(Mean=mean(PSI), Median=median(PSI),
                                      Variance=var(PSI), StandardDeviation=sd(PSI))


t.test(Suspension_Data$PSI,mu=1500)

t.test(subset(Suspension_Data,Manufacturing_Lot=="Lot1")$PSI,mu=1500)

t.test(subset(Suspension_Data,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(Suspension_Data,Manufacturing_Lot=="Lot3")$PSI,mu=1500)
