
#make a fake list of species
spp<-as.factor(c(1:8))

#Initiate and empty vector to store the number of spp. In
perm_null<-c()

#randomly sample the fake spp list 100000 times and each time count the number of species sampled and store it in the above vector. 
for(i in 1:100000){
  smpl<-sample(spp, 10, replace = T)
  unq<-length(unique(smpl))
  perm_null<-append(perm_null, unq)
}

hist(perm_null)

ToPlot<-data.frame(perm_null)

simresult<-data.frame(table(perm_null))

#Generate P values by dividing the number of observations by total samples 
simresult$pval<-simresult$Freq/100000
simresult$padj<-p.adjust(simresult$pval, method = "BH")
Pvals<-simresult

##Read in results
SimCommResults<-read.csv("~/Desktop/Simulated_Community_Analysis.csv", header = T, stringsAsFactors = F,
                        na.strings =c("", "NA"), strip.white = T)

#Divide dataset up by temperature 
ThirtyDegrees<-SimCommResults[which(SimCommResults$Temperature==30),1:6 ]
TenDegrees<-SimCommResults[which(SimCommResults$Temperature==10),1:6 ]
RoomTemp<-SimCommResults[which(SimCommResults$Temperature=="RT"),1:6 ]

#Pull out the number of species isolated for each replicate 
Uniq30<-data.frame(table(ThirtyDegrees[c(2,5,6)]))
Uniq30<-Uniq30[which(!Uniq30$Freq==0), ] 
TotalSpecies30<-data.frame(table(Uniq30$Sample.Number))

UniqRT<-data.frame(table(RoomTemp[c(2,5,6)]))
UniqRT<-UniqRT[which(!UniqRT$Freq==0), ]
TotalSpeciesRT<-data.frame(table(UniqRT$Sample.Number))

Uniq10<-data.frame(table(TenDegrees[c(2,5,6)]))
Uniq10<-Uniq10[which(!Uniq10$Freq==0), ]
TotalSpecies10<-data.frame(table(Uniq10$Sample.Number))


SimCommResults$SpNames<-paste(SimCommResults$Genus, SimCommResults$Species, sep=" ")

Plot<-SimCommResults[c(1,2,11)]
colnames(Plot)<-c("Temperature", "Sample.Number", "Species")
Plot$Species<-as.factor(Plot$Species)
Plot10<-Plot[which(Plot$Temperature==10), ]
Plot30<-Plot[which(Plot$Temperature==30), ]
PlotRT<-Plot[which(Plot$Temperature=="RT"), ]

require(ggplot2)

##Plot bargraphs for each temperature, coloring bars for each species


##10°C Graph
ggplot(Plot10, aes(x=Sample.Number, fill=Species))+
  geom_bar()+
  ggtitle("Species Recovered at 10°C")+
  xlab("Replicate")+ ylab("Number of Isolates")+
  scale_y_continuous(breaks = 0:10)+
  theme_minimal()+
  scale_fill_manual(values = c("dark blue", "blue"))

##RT Graph
ggplot(PlotRT, aes(x=Sample.Number, fill=Species))+
  geom_bar()+
  ggtitle("Species Recovered at Room Temperature")+
  xlab("Replicate")+ ylab("Number of Isolates")+
  scale_y_continuous(breaks = 0:10)+
  theme_minimal()+
  scale_fill_manual(values = c("#008080", "#A42A04","#000000", "#8E44AD"))

##30 Graph
ggplot(Plot30, aes(x=as.factor(Sample.Number), fill=Species))+
  geom_bar()+
  ggtitle("Species Recovered at 30°C")+
  xlab("Replicate")+ ylab("Number of Isolates")+
  scale_y_continuous(breaks = 0:10)+
  theme_minimal()+
  scale_fill_manual(values = c("#008080","#8E44AD"))



##Null model plot
  ggplot(ToPlot, aes(perm_null)) +
    geom_histogram(binwidth= 1,color="black", fill="#9F2B68")+
    theme(
      panel.background = element_rect(fill='transparent')
    ) +
    ggtitle("Expected Number of Species Recovered from 100,000 Samples")+
    xlab("Number of unique species recovered")+
    ylab("Frequency")+
    geom_vline(xintercept = 3, size = 2, color= "dark green", linetype = "dashed")+
    geom_vline(xintercept = 2, size = 2, color = "dark red", linetype = "dashed")+
    geom_vline(xintercept = 1, size = 2, color = "dark blue", linetype = "dashed")+
    geom_vline(xintercept = 0, size = 0.5)+
    scale_x_continuous(breaks = (0:8))

    


###### Adding in non-isolated species that were spiked in originally
  
speclist<-read.csv("~/Desktop/Simulated_Community_Project/speclist.csv", header = T, stringsAsFactors = F)  

NotFound<-speclist[which(!speclist$species%in%SimCommResults$Species), ]  
NotFound$Freq<-0
colnames(NotFound)<-c("Genus", "Species", "Freq")

TotalIsolates<-data.frame(table(SimCommResults[c(5,6)]))  
TotalIsolates<-TotalIsolates[which(!TotalIsolates$Freq==0), ]  
TotalIsolates<-rbind(TotalIsolates, NotFound)



