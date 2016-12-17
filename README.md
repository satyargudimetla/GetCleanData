GetCleanData
============
Satya
This anges added
Project is about using the R package all_run.R and the codebook provided to process the samsung Galaxy sensonr data.

To implement , please follow the give instructions.

Download the file "" and copy to a folder in the desktop under working directory for the R Studio.

Please make sure you have downloaded the project files from the following location and unzip.

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

After unzipping you will see the following project structure

path/projectfiles/all_run.R 

path/projectfiles/UCIHARDataset/

etc...

Now in the RStudio enter the following at the prompt

>setwd(path)

>library(dplyr)

>library(plyr)

>library(Hmisc)

> source("c:\\Rex\\projectfiles\\run_analysis.R")

> x<-getData("c:\\Rex\\projectfiles")

You should see a file create in your folder as "AllMeansData.txt".

Please note , inside the code, I have added n=20 in read.fwf command. This is basically to read just first 20 lines of 

each file .

If you have enough memory in your system, you can remove thos entry frm the code and run. You should see the entire 
output file.


