#Breya Walker
# compute the 95% CI of the CpG sites and exponetiatate the coefficents to compute the probability
# 11.7.2017

#Store the coefficients into an array
est<-c(-3.271,
       -7.628,
       -6.632,
       -3.325,
       -2.862,
       -2.685,
       -5.589,
       -3.415,
        0.653,
       -3.319,
       -2.426,
       -3.339,
       -0.757,
       -0.983,
       -0.787,
       -3.707,
        5.131,
       -4.354,
       -3.775,
       -2.481,
       -2.882,
       -4.377,
       -3.278,
       -4.492,
       -3.24,
       -4.052,
       -2.714,
       -4.925
)

sd<-c(1.134,
      2.48,
      2.112,
      1.115,
      0.849,
      0.926,
      1.841,
      1.121,
      0.172,
      1.144,
      0.786,
      1.007,
      0.237,
      0.325,
      0.255,
      1.14,
      1.763,
      1.29,
      1.145,
      0.725,
      0.775,
      1.45,
      0.902,
      1.506,
      1.014,
      1.19,
      0.912,
      1.474
)
Array<-data.frame(est,sd)

print(Array[2,1])
CIInfo<-matrix(NA, nrow=28,ncol=2)
Exp<-matrix(NA,nrow=28,ncol=1)
ExpCI<-matrix(NA,nrow=28,ncol=2)
CIInfo
i=0
out=NULL

for (i in 1:28)
{

  CIl<-c(Array[i,1] - 1.96 * Array[i,2])
  CIh<-c(Array[i,1] + 1.96 * Array[i,2])
  CIInfo[i,]<-c(CIl,CIh)
  Expp<-c(exp(Array[i,1]))
  Exp[i,]<-c(Expp)
  ExppCIl<-c(exp(CIl))
  ExppCIh<-c(exp(CIh))
  ExpCI[i,]<-c(ExppCIl,ExppCIh)
  
}

write.csv(CIInfo,"/Users/breyawalker/Desktop/Desktop/DrZhangMeeting/CPG_CIintervals.csv")
write.csv(Exp,"/Users/breyawalker/Desktop/Desktop/DrZhangMeeting/CPG_CIintervalsExp.csv")
write.csv(ExpCI,"/Users/breyawalker/Desktop/Desktop/DrZhangMeeting/CPG_CIintervalsExpCI.csv")
