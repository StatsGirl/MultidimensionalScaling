#Author Breya Walker
#Date: 10.29.2017
#Purpose: This code assigns gene, location, and chromosome associated with significant CpG sites. The program 
#create a list of significant CpG sites, iterate through each element in the human methylation csv file, locate identical CpG sites
#and assign gene location and other attributes associated with that CpG to a new df and write to csv

file="/Users/breyawalker/Desktop/Desktop/DrZhangMeeting/HumanMethylation450_15017482_v1-2.csv"
headers=read.csv(file, skip = 7,header=FALSE,nrows=1, as.is=TRUE)
GeneFile<-read.csv(file,skip=8, header=FALSE, sep=",")
colnames(GeneFile)=headers
SigCpG<-c("cg08092318","cg09470754","cg14616345","cg15172966","cg18666454","cg18778933","cg26118358","cg00881378","cg01330670","cg05322916","cg18245083","cg00926502","cg01159515","cg02295678","cg04855216","cg05322916", "cg07041681","cg07161239","cg09004287","cg10372548","cg10561835","cg13167431","cg15943396","cg17790311","cg19269039","cg24842086","cg25053336","cg26950898")

#get indices of matches of significant CpG sites
Matches<-data.frame(match(SigCpG,GeneFile$Name))
#print(Matches)
#print(Matches[2,1])

#example
#print(GeneFile[(Matches[1,1]),1])

CPGSites<-data.frame(CpGSite=character(28), GeneName=character(28),Location=character(28),Chromosome=character(28)) #size=number of sig cpgs
#Store the element values from GeneFile to new df using indices above

print(GeneFile[(394501),])
for(i in Matches){
  #print(GeneFile[(i),])
  #print(GeneFile$Name[(i)])
  CPGSites$CpGSite=GeneFile$Name[i]
  CPGSites$GeneName=GeneFile$UCSC_RefGene_Name[i]
  CPGSites$Location=GeneFile$UCSC_RefGene_Group[i]
  CPGSites$Chromosome=GeneFile$Chromosome_36[i]

}
 
write.csv(CPGSites,"/Users/breyawalker/Desktop/Desktop/DrZhangMeeting/CPGAttributes.csv")
  

