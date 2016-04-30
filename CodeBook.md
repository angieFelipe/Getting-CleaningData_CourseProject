**This dataset is based in the following open data**


Human Activity Recognition Using Smartphones Dataset
Version 1.0
"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit‡ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

##SUMMARY

The experiments have been carried out with a group of **30 volunteers** within an age bracket of 19-48 years. Each person performed **six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)** wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded **accelerometer and gyroscope**, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 



### For each record it is provided:


- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### The dataset includes the following files:


- 'README.md'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'HARdataset.txt'='HARdataset.zip' : data set for 10299 observations and 564 variables 
            (561 features + activity + subject + (test or training))

- 'HARsubset.txt': data set for 10299 observations and 89 variables 
            (86 features referred to mean and standard deviation + activity + subject + (test or training))

- 'HARaverage.txt': data set for 180 observations and 87 variables corresponding to the average of each variable of HARsubset.txt
    for each activity and each subject
      



### Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.



