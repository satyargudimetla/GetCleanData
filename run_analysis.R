getData <- function(path) {
 	x <- c()
	x[1] <- 17
	for (i in 2:561) { 
    		x[i]=16
	}
	#print(x)
	setPath <- paste(path,"\\UCIHARDataset\\train\\X_train.txt",sep="")
	getData_x_train <- read.fwf(setPath , widths=x)
	getData_x_train_df <- data.frame(  getData_x_train)
  rm(getData_x_train)

  #print(getData_x_train_df)
 
	setPath <- paste(path,"\\UCIHARDataset\\test\\X_test.txt",sep="")
	getData_x_test <-read.fwf(setPath  , widths=x)
	getData_x_test_df <- data.frame(getData_x_test)
  rm(getData_x_test)
	#print(getData_x_test_df)
  
	# bind x test and x train
	getXTotal<- rbind(getData_x_train_df, getData_x_test_df)
	#getXTotal
  #print(getXTotal)
  
	#READ Train Y files
	setPath <- paste(path,"\\UCIHARDataset\\train\\Y_train.txt",sep="")
  get_y_train <- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\train\\Y_train.txt" , width=1)
	get_y_train_df <- data.frame(get_y_train)
  rm(get_y_train)
	#get_y_train 

	#READ Test Y files
	setPath <- paste(path,"\\UCIHARDataset\\test\\Y_test.txt",sep="")

  get_y_test <- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\test\\Y_test.txt", width=1)
	get_y_test_df <- data.frame(get_y_test)
  rm(get_y_test)
	#get_y_test	

	# bind Y test and Y train
	getYTotal<- rbind(get_y_train_df,get_y_test_df	)
	#getYTotal
	
	# READ SUBJECT TRAIN
	getSubjectTrain<- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\train\\subject_train.txt", width=1)
	getSubjectTrain_df <-  data.frame(getSubjectTrain)
	rm(getSubjectTrain)
	
	# READ SUBJECT TEST
	setPath <- paste(path,"\\UCIHARDataset\\test\\subject_test.txt",sep="")
	getSubjectTest<- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\test\\subject_test.txt", width=1)
	getSubjectTest_df <- data.frame(getSubjectTest)
  rm(getSubjectTest)
  
	TotalSubject <- rbind(getSubjectTrain_df,getSubjectTest_df)
  #print(TotalSubject)
	# final data set
	total <- cbind(TotalSubject ,getYTotal, getXTotal)
	#print(total)
  
	# READ Column names
	getFeatureNames <- read.table("c:\\Rex\\projectfiles\\UCIHARDataset\\features.txt", sep=" ")
	#getFeatureNames 
	
	names(getFeatureNames) <- c("id", "value")  
  #addColumn <- c("1",  "tBodyAcc.mean-X")  
	#getFeatureNames <- rbind(addColumn, getFeatureNames)          
  #transpose the matrix
	#print(getFeatureNames)
  
	featureT <- t(getFeatureNames)

	#parse Just names
	featureTT <- featureT[2,]	
	#print(featureTT)
	colnames(total) <- c("Subject", "Activity", featureTT)
  #print(colnames(total))
	#REPLACE column names with tidy chars
	names(total) <- gsub("\\(\\)" ,"", names(total))
	names(total) <- gsub("\\,", "", names(total))
	names(total) <- gsub("\\(" ,"", names(total))
	names(total) <- gsub("\\)" ,"", names(total))
	names(total) <- gsub("\\-" ,"", names(total))
	
	#print(colnames(total))
  
	#RENAME ACTIVITY
	total$Activity[total$Activity == 1] <- "WALKING"
	total$Activity[total$Activity == 2] <- "WALKING_UPSTAIRS"
	total$Activity[total$Activity == 3] <- "WALKING_DOWNSTAIRS"
	total$Activity[total$Activity == 4] <- "SITTING"
	total$Activity[total$Activity == 5] <- "STANDING"
	total$Activity[total$Activity == 6] <- "LAYING"	

	#Remove NAs
	total <- total[,1:561]
	names(total) <- tolower(names(total))
  
	#print(total)
  
	#Select onlt columns that contain mean and Std

 
	stdDataT <- select (total, contains("std"))
	#print(stdDataT)
 
	meanDataT<- select(total, contains("mean"))
  
	getSubjectT <- select(total, subject)
	getActivityT <- select (total, activity)
  #print(intersect(getSubjectT ,getActivityT ,stdDataT , meanDataT))
  
	total <- cbind(getSubjectT ,getActivityT ,stdDataT , meanDataT)

#	print(length(total))
	#total <- total[complete.cases(total),]
	#print(length(total))

	#exclude and create colmeans for activity
	getWALKING_UPSTAIRS <- filter(total, activity=="WALKING_UPSTAIRS")
	getWALKING_UPSTAIRS <- colMeans(getWALKING_UPSTAIRS[3:length(getWALKING_UPSTAIRS)], na.rm=TRUE)
	names(getWALKING_UPSTAIRS) <- paste(names(getWALKING_UPSTAIRS) ,"Avg", sep="")
	
	getWALKING <- filter(total, activity=="WALKING")
	getWALKING <- colMeans(getWALKING[3:length(getWALKING)], na.rm=TRUE)
	names(getWALKING) <- paste(names(getWALKING) ,"Avg", sep="")

	getSTANDING <- filter(total, activity=="STANDING")
	getSTANDING <- colMeans(getSTANDING[3:length(getSTANDING)], na.rm=TRUE)
	names(getSTANDING) <- paste(names(getSTANDING) ,"Avg", sep="")

	getWALKING_DOWNSTAIRS <- filter(total, activity=="WALKING_DOWNSTAIRS")
	getWALKING_DOWNSTAIRS <- colMeans(getWALKING_DOWNSTAIRS[3:length(getWALKING_DOWNSTAIRS)], na.rm=TRUE)
	names(getWALKING_DOWNSTAIRS) <- paste(names(getWALKING_DOWNSTAIRS) ,"Avg", sep="")

	getSITTING <- filter(total, activity=="SITTING")
	getSITTING <- colMeans(getSITTING[3:length(getSITTING)], na.rm=TRUE)
	names(getSITTING) <- paste(names(getSITTING) ,"Avg", sep="")

	getLAYING <- filter(total, activity=="LAYING")
	getLAYING <- colMeans(getLAYING[3:length(getLAYING)], na.rm=TRUE)
	names(getLAYING) <- paste(names(getLAYING) ,"Avg", sep="")

	#join the vectors
	ActivityMeanData<- rbind(getWALKING,getWALKING_UPSTAIRS,getWALKING_DOWNSTAIRS,getSITTING,getSTANDING,getLAYING)
	#print(ActivityMeanData)
  write.table(ActivityMeanData, file = "c:\\Rex\\projectfiles\\ActivityMeansData2.txt", sep = ",", row.names = FALSE)

# remove from the memory
  rm(ActivityMeanData)
	#exclude and create colmeans for subject

	subjectMean  <- c()
	getUniqSubjects <- unique(TotalSubject )

  #print(getUniqSubjects)

	#for(j in getUniqSubjects$V1 ){
		#getV <- filter(total, subject==getUniqSubjects[j])

	get1 <- filter(total, subject==1)
	get1mean <- colMeans(get1 [3:length(get1 )], na.rm=TRUE)    

	get2 <- filter(total, subject==2)
	get2mean <- colMeans(get2 [3:length(get2 )], na.rm=TRUE)    

	get3 <- filter(total, subject==3)
	get3mean <- colMeans(get3 [3:length(get3 )], na.rm=TRUE)    

	get4 <- filter(total, subject==4)
	get4mean <- colMeans(get4 [3:length(get4 )], na.rm=TRUE)    

	get5 <- filter(total, subject==5)
	get5mean <- colMeans(get5 [3:length(get5 )], na.rm=TRUE)    

	get6 <- filter(total, subject==6)
	get6mean <- colMeans(get6 [3:length(get6 )], na.rm=TRUE)    

	get7 <- filter(total, subject==7)
	get7mean <- colMeans(get7 [3:length(get7 )], na.rm=TRUE)    

	get8 <- filter(total, subject==8)
	get8mean <- colMeans(get8 [3:length(get8 )], na.rm=TRUE)    

	get9 <- filter(total, subject==9)
	get9mean <- colMeans(get9 [3:length(get9 )], na.rm=TRUE)    

	get10 <- filter(total, subject==10)
	get10mean <- colMeans(get10 [3:length(get10 )], na.rm=TRUE)    

	get11 <- filter(total, subject==11)
	get11mean <- colMeans(get11 [3:length(get11 )], na.rm=TRUE)    

	get12 <- filter(total, subject==12)
	get12mean <- colMeans(get12 [3:length(get12 )], na.rm=TRUE)    

	get13 <- filter(total, subject==13)
	get13mean <- colMeans(get13 [3:length(get13 )], na.rm=TRUE)    

	get14 <- filter(total, subject==14)
	get14mean <- colMeans(get14 [3:length(get14 )], na.rm=TRUE)    

	get15 <- filter(total, subject==15)
	get15mean <- colMeans(get15 [3:length(get15 )], na.rm=TRUE)    

	get16 <- filter(total, subject==16)
	get16mean <- colMeans(get16 [3:length(get16 )], na.rm=TRUE)  

  get17 <- filter(total, subject==17)
  get17mean <- colMeans(get17 [3:length(get17 )], na.rm=TRUE)  

	get18 <- filter(total, subject==18)
	get18mean <- colMeans(get18 [3:length(get18 )], na.rm=TRUE)  

	get19 <- filter(total, subject==19)
	get19mean <- colMeans(get19 [3:length(get19 )], na.rm=TRUE)  

	get20 <- filter(total, subject==20)
	get20mean <- colMeans(get20 [3:length(get20 )], na.rm=TRUE)  

		get21 <- filter(total, subject==21)
	get21mean <- colMeans(get21 [3:length(get21 )], na.rm=TRUE)  
	
		get22 <- filter(total, subject==22)
	get22mean <- colMeans(get22 [3:length(get22 )], na.rm=TRUE)  
	
		get23 <- filter(total, subject==23)
	get23mean <- colMeans(get23 [3:length(get23 )], na.rm=TRUE)  
	
		get24 <- filter(total, subject==24)
	get24mean <- colMeans(get24 [3:length(get24 )], na.rm=TRUE)  
	
		get25 <- filter(total, subject==25)
	get25mean <- colMeans(get25 [3:length(get25 )], na.rm=TRUE)  
	
get26 <- filter(total, subject==26)
get26mean <- colMeans(get26 [3:length(get26 )], na.rm=TRUE)  

get27 <- filter(total, subject==27)
get27mean <- colMeans(get27 [3:length(get27 )], na.rm=TRUE)  

get28 <- filter(total, subject==28)
get28mean <- colMeans(get28 [3:length(get28 )], na.rm=TRUE)  

get29 <- filter(total, subject==29)
get29mean <- colMeans(get29 [3:length(get29 )], na.rm=TRUE)  

get30 <- filter(total, subject==30)
get30mean <- colMeans(get30 [3:length(get30 )], na.rm=TRUE)  

	subjectMean <- rbind(get1mean,get2mean,get3mean,get4mean,get5mean, get6mean
                       , get7mean, get8mean
                       , get9mean
          						, get10mean
                      , get11mean
                      ,get12mean
          						, get13mean
                      , get14mean
                      , get15mean
                      , get16mean
                      , get17mean
                      , get18mean
                      , get19mean
                      , get20mean
          						, get21mean
          						, get22mean
          						, get23mean
          						, get24mean
          						, get25mean
          						, get26mean
          						, get27mean
          						, get28mean
          						, get29mean
          						, get30mean)
  #print(get1mean)
	#	getSubjectMeanN <- colMeans(getV [3:length(getV )], na.rm=TRUE)		
	#	subjectMean <- rbind(subjectMean,getSubjectMeanN ) 		
		#rownames(subjectMean) <- paste(rownames(subjectMean),"_", j,sep="")
		#print(rownames(subjectMean))
#	}

	#print(subjectMean )

	#create a final means dataset with activity and subject
	#finalMeans <- rbind(subjectMean ,ActivityMeanData)
	#print(finalMeans)

	#RETURN DATA SET
	#total
	#finalMeans
	write.table(subjectMean, file = "c:\\Rex\\projectfiles\\SubjectMeansData2.txt", sep = ",", row.names = FALSE)
	
	#finalMeans
  rm(subjectMean)
}
