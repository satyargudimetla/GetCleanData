getData <- function(path) {
 	x <- c()
	x[1] <- 17
	for (i in 2:561) { 
    		x[i]=16
	}
	#print(x)
	setPath <- paste(path,"\\UCIHARDataset\\train\\X_train.txt",sep="")
	getData_x_train <- read.fwf(setPath ,n=200, widths=x)
	getData_x_train_df <- data.frame(  getData_x_train)
  rm(getData_x_train)

  #print(getData_x_train_df)
 
	setPath <- paste(path,"\\UCIHARDataset\\test\\X_test.txt",sep="")
	getData_x_test <-read.fwf(setPath , n=200, widths=x)
	getData_x_test_df <- data.frame(getData_x_test)
  rm(getData_x_test)
	#print(getData_x_test_df)
  
	# bind x test and x train
	getXTotal<- rbind(getData_x_train_df, getData_x_test_df)
	#getXTotal
  #print(getXTotal)
  
	#READ Train Y files
	setPath <- paste(path,"\\UCIHARDataset\\train\\Y_train.txt",sep="")
  get_y_train <- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\train\\Y_train.txt" ,n=200, width=1)
	get_y_train_df <- data.frame(get_y_train)
  rm(get_y_train)
	#get_y_train 

	#READ Test Y files
	setPath <- paste(path,"\\UCIHARDataset\\test\\Y_test.txt",sep="")

  get_y_test <- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\test\\Y_test.txt",n=200, width=1)
	get_y_test_df <- data.frame(get_y_test)
  rm(get_y_test)
	#get_y_test	

	# bind Y test and Y train
	getYTotal<- rbind(get_y_train_df,get_y_test_df	)
	#getYTotal
	
	# READ SUBJECT TRAIN
	getSubjectTrain<- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\train\\subject_train.txt",n=200, width=1)
	getSubjectTrain_df <-  data.frame(getSubjectTrain)
	rm(getSubjectTrain)
	
	# READ SUBJECT TEST
	setPath <- paste(path,"\\UCIHARDataset\\test\\subject_test.txt",sep="")
	getSubjectTest<- read.fwf("c:\\Rex\\projectfiles\\UCIHARDataset\\test\\subject_test.txt",n=200, width=1)
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

	#REPLACE column names with tidy chars
	names(total) <- gsub("\\(\\)" ,"", names(total))
	names(total) <- gsub("\\,", "", names(total))
	names(total) <- gsub("\\(" ,"", names(total))
	names(total) <- gsub("\\)" ,"", names(total))
	names(total) <- gsub("\\-" ,"", names(total))

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

	#Select onlt columns that contain mean and Std
	stdData <- select (total, contains("std"))
	meanData<- select(total, contains("mean"))
	getSubject <- select(total, subject)
	getActivity <- select (total, activity)
	total <- cbind(getSubject ,getActivity ,stdData , meanData)

	#print(length(total))
	total <- total[complete.cases(total),]
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

	#exclude and create colmeans for subject

	subjectMean  <- c()
	getUniqSubjects <- unique(TotalSubject )

	for(j in getUniqSubjects ){
		getV <- filter(total, subject==getUniqSubjects[j])
		getSubjectMeanN <- colMeans(getV [3:length(getV )], na.rm=TRUE)		
		subjectMean <- rbind(subjectMean,getSubjectMeanN ) 		
		rownames(subjectMean) <- paste(rownames(subjectMean),"_", j,sep="")
		#print(rownames(subjectMean))
	}

	#print(subjectMean )

	#create a final means dataset with activity and subject
	finalMeans <- rbind(subjectMean ,ActivityMeanData)
	#print(finalMeans)

	#RETURN DATA SET
	#total
	finalMeans
	write.table(finalMeans, file = "c:\\Rex\\projectfiles\\AllMeansData.txt", sep = ",", row.names = FALSE)
	
	finalMeans

}
