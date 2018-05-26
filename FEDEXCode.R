#-------------------------------------
# Version: 0.85
# Authors@R: person("Seongjin", "Kim", email = "skim22@memphis.edu", role = c("aut", "cre"))
#-------------------------------------

dir<-setwd("/Users/breyawalker/Desktop/Desktop/Naveen/BitBucket")

#------------------------------------
# Part1 a and b
#------------------------------------

# import data
# raw.data <- read.csv(file = "ticket_170807.csv", sep = ",", header = TRUE)
raw.data <- read.csv(file = "ticket_170807.csv", sep = ",", header = TRUE)
config.variable <- read.csv(file = "config_variable.csv", sep = ",", header = TRUE)

# convert the type of an integer to a character
typeof(config.variable$Variable_Name)
config.variable$Variable_Name <- as.character(config.variable$Variable_Name)
config.variable$Variable_Name <- trimws(config.variable$Variable_Name)
typeof(config.variable$Variable_Name)

# match the name of variables between raw.data and config.variable
match.data <- raw.data[,which(colnames(raw.data)%in%config.variable$Variable_Name)]

#------------------------------------
# Part2
#------------------------------------

# refine transpose data to remove missing values
refined.data <- match.data[complete.cases(match.data),]

#Author of parts3-4:person("Walker", "Breya", email = "breyawalker@gmail.com", role = c("aut", "cre"))

#----------------------
#Part 3a
#Choose 10% through 90% of records in 10% increments and determine the average revenue captured at each increment
#----------------------
#select combinations of the 8 columns
r<-c(1,2,3,4,5,6,7,8) #r=number of cols we want to choose. For now we will do 8 choose 2. 4.12.18 we will do 8 choose r
n=8 #change n to the number of columns we want to include
sample<-vector("list",n)

MaxRows=0
for(i in 1:n)
{
  sample[[i]]<-data.frame(combn(names(match.data[1:8]),r[i],simplify=FALSE)) #we save all possible unordered combinations of the n columns
  tem=MaxRows
  MaxRows<-sum(MaxRows+as.numeric(length(sample[[i]]))) #the maximum number of rows to include in the NewTable df
}

#Now we want to Choose 10% through 90% of the records
increment<-c(.10,.20,.30,.40,.50,.60,.70,.80,.90) #set list that contains increment values
length<-as.numeric(dim(refined.data)[1])

L<-as.numeric(length(sample)) #L= the length of the enitre number of columns to include
NewTable<-data.frame(matrix(ncol=1,nrow=MaxRows*9)) #We multiple the MaxRows by 9 to include each of the 9 increments of data records we want to include in list increment


count=1
for (r in 1:L) #we select L as the upper bound because the length of the sample is n
  
{
  ub<-as.numeric(length(sample[[r]]))#this is the length of each list in sample
  #test<-as.numeric(length(sample[[r]])) #this is the length of each list in sample
  cols<-sample[[r]]
  
  for (i in 1:ub)
    
  {
    testing<-c(cols[1:ub])
    temp<-levels(testing[[i]])
    
    temp2<-c(temp,colnames(refined.data["NET_CHRG_AMT"]))
    temp2
    
     for(t in 1:9) #10 through 90% increments
     {
       num<-length*increment[t]
       mydf<-sample(refined.data[1:num,temp2])
       AvgRev<-mean(mydf$NET_CHRG_AMT)
       #Table<-table(AvgRev)
       #myfile<-file.path(dir,sprintf('Part3a_%d_%d.csv',i,t))
       #write.csv(Table,file=myfile)
       NewTable[count,1]<-rbind(AvgRev)
        count=count+1
       }
  }
  
}

#----------------------
#Part 3b
#Choose large random sample of the data
#----------------------
L2<-as.numeric(length(sample)) #L= the length of the enitre number of columns to include
NewTable2<-data.frame(matrix(ncol=1,nrow=MaxRows*9)) #We multiple the MaxRows by 9 to include each of the 9 increments of data records we want to include in list increment


count2=1
for (r in 1:L2) #we select L as the upper bound because the length of the sample is n
  
{
  ub<-as.numeric(length(sample[[r]]))#this is the length of each list in sample
  #test<-as.numeric(length(sample[[r]])) #this is the length of each list in sample
  cols2<-sample[[r]]
  
  for (i in 1:ub)
    
  {
    testing<-c(cols2[1:ub])
    tem2<-levels(testing[[i]])
    
    temp3<-c(tem2,colnames(refined.data["NET_CHRG_AMT"]))
    temp3
    
    for(t in 1:9) #10 through 90% increments
    {
      num2<-length*increment[t]
      mydf2<-refined.data[sample(nrow(refined.data),size=num2,replace=FALSE),temp3]
      AvgRev2<-mean(mydf2$NET_CHRG_AMT)
      #Table<-table(AvgRev)
      #myfile<-file.path(dir,sprintf('Part3a_%d_%d.csv',i,t))
      #write.csv(Table,file=myfile)
      NewTable2[count2,1]<-rbind(AvgRev2)
      count2=count2+1
    }
  }
  
}

#---------------------
#Part 4
#output results in a csv file
#1st column: Num of vars
#2nd column:string of var names
#3rd column: % of records included
#4th column: Avg Revenue 
#--------------------



columnnames<-c("NumberOfVar","VarNames","% of records","Avg Revenue")
L3<-as.numeric(length(sample)) #L= the length of the enitre number of columns to include
NewTable3<-data.frame(matrix(ncol=4,nrow=MaxRows*9)) #We multiple the MaxRows by 9 to include each of the 9 increments of data records we want to include in list increment
colnames(NewTable3)<-columnnames
count3=1
for (r in 1:L3) #we select L as the upper bound because the length of the sample is n
  
{
  ub<-as.numeric(length(sample[[r]]))#this is the length of each list in sample
  #test<-as.numeric(length(sample[[r]])) #this is the length of each list in sample
  cols3<-sample[[r]]
  
  for (i in 1:ub)
    
  {
    testing<-c(cols3[1:ub])
    tem3<-levels(testing[[i]])
    
    temp4<-c(tem3,colnames(refined.data["NET_CHRG_AMT"]))
    temp4
    
    for(t in 1:9) #10 through 90% increments
    {
      num3<-length*increment[t]
      mydf3<-refined.data[sample(nrow(refined.data),size=num3,replace=FALSE),temp4]
      AvgRev3<-mean(mydf3$NET_CHRG_AMT)
      Numvars<-length(tem3)
      stringNames<-paste(tem3,collapse="_/_")
      #Table<-table(AvgRev)
      #myfile<-file.path(dir,sprintf('Part3a_%d_%d.csv',i,t))
      #write.csv(Table,file=myfile)
      NewTable3[count3,1]<-rbind(Numvars)
      NewTable3[count3,2]<-rbind(stringNames)
      NewTable3[count3,3]<-rbind(increment[t])
      NewTable3[count3,4]<-rbind(AvgRev3)
      count3=count3+1
    }
  }
  
}

myfile<-file.path(dir,sprintf('Part4final.csv'))
write.csv(NewTable3,file=myfile)
