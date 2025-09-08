#Michael Neidlin 23.06.2025
#This code creates normalized curves from csv data

library("minpack.lm")
library("ggplot2")
library("tidyverse")
library("pracma")
library("readxl")
library("openxlsx")
library("ggpubr")

#Get all txt files
all.files<-list.files(pattern="\\.csv$",recursive=TRUE)

m <- matrix(0, ncol = 1, nrow = 501)
df.v<-as.data.frame(m)
df.dv<-as.data.frame(m)
colnames(df.v)<-c("time")
colnames(df.dv)<-c("time")


#FOR ALL LV FILES
for (actual.file in all.files){
filename<-actual.file

#Read csv and extract data (time, volume,timeRR)
df<-read.csv(filename)
t<-df$time #in ms
t<-t-min(t)
v<-df$volume #in ml
timeRR<-df$timeRR[1]
patient<-substring(filename,6,nchar(filename)-4)


#Add the timeRR and first value of v to the array
t<-c(t,timeRR)
v<-c(v,v[1])

#Have v and t with a length of 1000
v<-approx(t,v,n=501)$y
t<-seq(0,max(t),length.out=501)


#Perform circular shift to have max(v)=v[1]
shift<-which.max(v)
v<-circshift(v,-shift)


#Repeat v and t for periodic fitting. tt and vv are normalized
vv=rep(v,5)
tt=seq(0,5*max(t),length.out=2505)


#Now do the Fourier fitting (3rd order)


# Define frequency: assume 1 cycle per year → w = 2*pi / period
w <- 2 * pi / max(t)

# Fit Fourier series of order 3
fit <- nlsLM(
  vv ~ a0 +
    a1 * cos(w * tt) + b1 * sin(w * tt) +
    a2 * cos(2 * w * tt) + b2 * sin(2 * w * tt)+
    a3 * cos(3 * w * tt) + b3 * sin(3 * w * tt),
  start = list(a0 = mean(vv), a1 = 0, b1 = 0, a2 = 0, b2 = 0,a3=0,b3=0)
)


vv_pred<-predict(fit)


#This is an alternative way to create the derivative. Still to be decided (03.07.25)
#Extract parameters
#summ<-summary(fit)
#a0<-summ$parameters[1,1]
#a1<-summ$parameters[2,1]
#b1<-summ$parameters[3,1]
#a2<-summ$parameters[4,1]
#b2<-summ$parameters[5,1]
#a3<-summ$parameters[6,1]
#b3<-summ$parameters[7,1]


#Now create the analytical derivative
#dvv<- -a1*w*sin(w*tt)+b1*w*cos(w*tt)+
 # -2*a2*w*sin(2*w*tt)+2*b2*w*cos(2*w*tt)+
  #-3*a3*w*sin(3*w*tt)+3*b3*w*cos(3*w*tt)


#Determine gradient
dv<-gradient(vv_pred,tt)


#Extract one cycle
v<-vv_pred[1:length(t)]
dv<-dv[1:length(t)]

#Normalize
vmax<-max(v)

v<-v/vmax
dv<-dv/vmax
t<-t/timeRR


#Write to dataframe
df.v[,patient]<-v
df.dv[,patient]<-dv
}

df.v$time<-t
df.dv$time<-t

write.xlsx(df.v,"AllV_AP_250703.xlsx",sheetName="V")
write.xlsx(df.dv,"All_dV_AP_250703.xlsx",sheetName="dV")



writexl::write_xlsx(df.v,"AllV_250624.xlsx")
writexl::write_xlsx(df.dv,"AlldV_250624.xlsx")



#Create plots for control
df.v.norm.all.pat<- read_excel("AllV_250624.xlsx")
df.v.norm<- read_excel("AllV_250624.xlsx")
df.v.norm<-df.v.norm[,c("time","MEDIAN","PERZ_5","PERZ_95")]

colnames(df.v.norm.all.pat)[1]<-"time"
df.v.norm.all.pat<-df.v.norm.all.pat[,-c((ncol(df.v.norm.all.pat)-2):ncol(df.v.norm.all.pat))]

df.dv.norm.all.pat<- read_excel("AlldV_250624.xlsx")
df.dv.norm<- read_excel("AlldV_250624.xlsx")
df.dv.norm<-df.dv.norm[,c("time","MEDIAN","PERZ_5","PERZ_95")]
colnames(df.dv.norm.all.pat)[1]<-"time"
df.dv.norm.all.pat<-df.dv.norm.all.pat[,-c((ncol(df.dv.norm.all.pat)-2):ncol(df.dv.norm.all.pat))]


#Figure 1: All Norm (V and dv/dt)

#Define what to plot v or dv/dt
df.long<-gather(df.v.norm.all.pat,"patient","v",-time)

#Plot median, ranges and all normalized curves (v or dv)

a<-ggplot()+
  geom_line(data = df.long, aes(x = time, y = v, group = as.factor(patient)),
            color = "black", size = 0.8, alpha = 0.2) +
  geom_ribbon(data = df.v.norm,
              aes(x = time, ymin = PERZ_5, ymax = PERZ_95, fill = "5th/95th percentiles"),
              alpha = 0.5) +
  geom_line(data = df.v.norm,
            aes(x = time, y = MEDIAN, color = "Median"),
            size = 1.5) +
  scale_color_manual(name = "", values = c("Median" = "darkred")) +
  scale_fill_manual(name = "", values = c("5th/95th percentiles" = "lightblue")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)),
         color = guide_legend(override.aes = list(size = 1.5)))+ 
  theme_bw()+
  labs(
    title = paste("Normalized VTC for norm cohort"),
    x = "Time/timeRR",
    y = "Normalized volume [V/EDV]")+
  theme(axis.text = element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  theme(plot.title = element_text(hjust = 0.5,size=18))

#Define what to plot v or dv/dt
df.long<-gather(df.dv.norm.all.pat,"patient","v",-time)


b<-ggplot() +
  geom_line(data = df.long, aes(x = time, y = v, group = as.factor(patient)),
            color = "black", size = 0.8, alpha = 0.2) +
  geom_ribbon(data = df.dv.norm,
              aes(x = time, ymin = PERZ_5, ymax = PERZ_95, fill = "5th/95th percentiles"),
              alpha = 0.5) +
  geom_line(data = df.dv.norm,
            aes(x = time, y = MEDIAN, color = "Median"),
            size = 1.5) +
  scale_color_manual(name = "", values = c("Median" = "darkred")) +
  scale_fill_manual(name = "", values = c("5th/95th percentiles" = "lightblue")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)),
         color = guide_legend(override.aes = list(size = 1.5)))+
  theme_bw()+
  labs(
    title = paste("Normalized dV/dt curves of norm cohort"),
    x = "Time/timeRR",
    y = "Normalized dV/dt [dV/EDV]")+
  theme(axis.text = element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  theme(plot.title = element_text(hjust = 0.5,size=18))


arrange<-ggarrange(a, b, labels=c("A","B"),ncol = 2, nrow = 1,common.legend = TRUE, legend="right")
ggsave("VTC_DV_Norm.tiff",arrange,width=16,height=7,dpi=300,compression="lzw",bg="white")
