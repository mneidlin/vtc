#Michael Neidlin 08.09.2025
#This is the code from the Linden et al. 2025 publication
#Scripts extracts data from txt files and translates it into one csv with 3 columns - time, volume, timeRR

#Put all txt files (export from Tomtec Image Arena into one folder)

#Reads all txt files in working directory and sub-directories
all.files<-list.files(pattern="\\.txt$")


#Loops from the list of all txt files. Check that those are only the patients you want to analyze
#If there are other versions of TomTec, one might need to adjust the line numbers (in the code below)
#to extract the respective information (time, timeRR and volume)

for (actual.file in all.files){
filename<-actual.file

#reads entire txt file
df<-read.delim(filename)

#extracts the time. Row 791 might need adjustment. Check against full df
time<-df[791,]
time<-unlist(strsplit(time, ","))
time<-as.numeric(time[-1])


#extracts the total volume. Row 792 might need adjustment. Check against full df
V<-df[792,]
V<-unlist(strsplit(V, ","))
V<-as.numeric(V[-1])


#extracts the RR interval time. Row 12 might need adjustment. Check against full df
timeRR<-df[12,]
timeRR<-unlist(strsplit(timeRR, ","))
timeRR<-as.numeric(timeRR[2])
timeRR<-rep(timeRR,length(V))

#join the three variables and export to csv
df.export<-as.data.frame(cbind(time,V,timeRR))
colnames(df.export)<-c("time","volume","timeRR")

filename.csv<-gsub('.txt', '.csv', filename)

write.csv(df.export,filename.csv,row.names = FALSE)
}

