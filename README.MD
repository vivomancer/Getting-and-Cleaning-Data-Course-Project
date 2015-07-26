---
title: "README"
author: "David Dziak"
date: "July 25, 2015"
output: html_document
---

#Merges the training and the test sets to create one data set.

##Load all of the values into R

Here I read in all of the files into seperate data tables

##Merge all the loaded values
Here I combined the rows of the training and testing data sets using rbind

Then I gave them proper column headings using names()

Finally I merged the tables by columns using cbind

#Extracts only the measurements on the mean and standard deviation for each measurement. 

Here I used grep to get the column numbers for all columns that contained "mean" or "std" and pulled only those columns plus the first two from the data set

#Uses descriptive activity names to name the activities in the data set

Here I loaded the descriptive names for each activity and used plyr to rename each integer to its corrosponding string values

#Appropriately labels the data set with descriptive variable names. 

I actually did this when I cbind-ed the 3 merged tables togeather in the merge all loaded values step

  #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
 
 Here I used melt to put subject and activity each on their own row, then I used cast to find the mean of each variable for each combination of subject and activity