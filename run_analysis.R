## Load train data and subject
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


## Load test data and subject
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Load activity_labels and features
activity<- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)

##  Combine train and test label
joinLabel <- rbind(y_train, y_test)
dim(joinLabel)

## Uses descriptive activity names to name the activities in the data set
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

## Combine train and test data
joinData <- rbind(x_train, x_test)
dim(joinData)
joinData <- joinData[, meanStdIndices]
dim(joinData)

## Appropriately labels the data set with descriptive activity names.
joinSubject <- rbind(subject_train, subject_test)
dim(joinSubject)
names(joinSubject) <- "subject"


names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names

## Create tidy_data
tidy_Data <- cbind(joinSubject, joinLabel, joinData)
dim(tidy_Data)

## Write tidt data into merged_data
write.table(tidy_Data, "merged_data.txt") 


## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(tidy_Data)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(tidy_Data)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == tidy_Data$subject
    bool2 <- activity[j, 2] == tidy_Data$activity
    result[row, 3:columnLen] <- colMeans(tidy_Data[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}


head(result)
subject          activity tBodyAccMeanX tBodyAccMeanY tBodyAccMeanZ tBodyAccStdX
1       1           walking     0.2773308  -0.017383819    -0.1111481  -0.28374026
2       1   walkingupstairs     0.2554617  -0.023953149    -0.0973020  -0.35470803
3       1 walkingdownstairs     0.2891883  -0.009918505    -0.1075662   0.03003534
4       1           sitting     0.2612376  -0.001308288    -0.1045442  -0.97722901
5       1          standing     0.2789176  -0.016137590    -0.1106018  -0.99575990
6       1            laying     0.2215982  -0.040513953    -0.1132036  -0.92805647
tBodyAccStdY tBodyAccStdZ tGravityAccMeanX tGravityAccMeanY tGravityAccMeanZ
1  0.114461337  -0.26002790        0.9352232       -0.2821650      -0.06810286
2 -0.002320265  -0.01947924        0.8933511       -0.3621534      -0.07540294
3 -0.031935943  -0.23043421        0.9318744       -0.2666103      -0.06211996
4 -0.922618642  -0.93958629        0.8315099        0.2044116       0.33204370
5 -0.973190056  -0.97977588        0.9429520       -0.2729838       0.01349058
6 -0.836827406  -0.82606140       -0.2488818        0.7055498       0.44581772
tGravityAccStdX tGravityAccStdY tGravityAccStdZ tBodyAccJerkMeanX tBodyAccJerkMeanY
1      -0.9766096      -0.9713060      -0.9477172        0.07404163      0.0282721096
2      -0.9563670      -0.9528492      -0.9123794        0.10137273      0.0194863076
3      -0.9505598      -0.9370187      -0.8959397        0.05415532      0.0296504490
4      -0.9684571      -0.9355171      -0.9490409        0.07748252     -0.0006191028
5      -0.9937630      -0.9812260      -0.9763241        0.07537665      0.0079757309
6      -0.8968300      -0.9077200      -0.8523663        0.08108653      0.0038382040
tBodyAccJerkMeanZ tBodyAccJerkStdX tBodyAccJerkStdY tBodyAccJerkStdZ tBodyGyroMeanX
1      -0.004168406      -0.11361560        0.0670025       -0.5026998    -0.04183096
2      -0.045562545      -0.44684389       -0.3782744       -0.7065935     0.05054938
3      -0.010971973      -0.01228386       -0.1016014       -0.3457350    -0.03507819
4      -0.003367792      -0.98643071       -0.9813720       -0.9879108    -0.04535006
5      -0.003685250      -0.99460454       -0.9856487       -0.9922512    -0.02398773
6       0.010834236      -0.95848211       -0.9241493       -0.9548551    -0.01655309
tBodyGyroMeanY tBodyGyroMeanZ tBodyGyroStdX tBodyGyroStdY tBodyGyroStdZ
1    -0.06953005     0.08494482    -0.4735355  -0.054607769    -0.3442666
2    -0.16617002     0.05835955    -0.5448711   0.004105184    -0.5071687
3    -0.09093713     0.09008501    -0.4580305  -0.126349195    -0.1247025
4    -0.09192415     0.06293138    -0.9772113  -0.966473895    -0.9414259
5    -0.05939722     0.07480075    -0.9871919  -0.987734440    -0.9806456
6    -0.06448612     0.14868944    -0.8735439  -0.951090440    -0.9082847
tBodyGyroJerkMeanX tBodyGyroJerkMeanY tBodyGyroJerkMeanZ tBodyGyroJerkStdX
1        -0.08999754        -0.03984287        -0.04613093        -0.2074219
2        -0.12223277        -0.04214859        -0.04071255        -0.6147865
3        -0.07395920        -0.04399028        -0.02704611        -0.4870273
4        -0.09367938        -0.04021181        -0.04670263        -0.9917316
5        -0.09960921        -0.04406279        -0.04895055        -0.9929451
6        -0.10727095        -0.04151729        -0.07405012        -0.9186085
tBodyGyroJerkStdY tBodyGyroJerkStdZ tBodyAccMagMean tBodyAccMagStd
1        -0.3044685        -0.4042555     -0.13697118    -0.21968865
2        -0.6016967        -0.6063320     -0.12992763    -0.32497093
3        -0.2388248        -0.2687615      0.02718829     0.01988435
4        -0.9895181        -0.9879358     -0.94853679    -0.92707842
5        -0.9951379        -0.9921085     -0.98427821    -0.98194293
6        -0.9679072        -0.9577902     -0.84192915    -0.79514486
tGravityAccMagMean tGravityAccMagStd tBodyAccJerkMagMean tBodyAccJerkMagStd
1        -0.13697118       -0.21968865         -0.14142881        -0.07447175
2        -0.12992763       -0.32497093         -0.46650345        -0.47899162
3         0.02718829        0.01988435         -0.08944748        -0.02578772
4        -0.94853679       -0.92707842         -0.98736420        -0.98412002
5        -0.98427821       -0.98194293         -0.99236779        -0.99309621
6        -0.84192915       -0.79514486         -0.95439626        -0.92824563
tBodyGyroMagMean tBodyGyroMagStd tBodyGyroJerkMagMean tBodyGyroJerkMagStd
1      -0.16097955      -0.1869784           -0.2987037          -0.3253249
2      -0.12673559      -0.1486193           -0.5948829          -0.6485530
3      -0.07574125      -0.2257244           -0.2954638          -0.3065106
4      -0.93089249      -0.9345318           -0.9919763          -0.9883087
5      -0.97649379      -0.9786900           -0.9949668          -0.9947332
6      -0.87475955      -0.8190102           -0.9634610          -0.9358410
fBodyAccMeanX fBodyAccMeanY fBodyAccMeanZ fBodyAccStdX fBodyAccStdY fBodyAccStdZ
1   -0.20279431   0.089712726    -0.3315601  -0.31913472   0.05604001  -0.27968675
2   -0.40432178  -0.190976721    -0.4333497  -0.33742819   0.02176951   0.08595655
3    0.03822918   0.001549908    -0.2255745   0.02433084  -0.11296374  -0.29792789
4   -0.97964124  -0.944084550    -0.9591849  -0.97641231  -0.91727501  -0.93446956
5   -0.99524993  -0.977070848    -0.9852971  -0.99602835  -0.97229310  -0.97793726
6   -0.93909905  -0.867065205    -0.8826669  -0.92443743  -0.83362556  -0.81289156
fBodyAccJerkMeanX fBodyAccJerkMeanY fBodyAccJerkMeanZ fBodyAccJerkStdX
1       -0.17054696       -0.03522552        -0.4689992       -0.1335866
2       -0.47987525       -0.41344459        -0.6854744       -0.4619070
3       -0.02766387       -0.12866716        -0.2883347       -0.0863279
4       -0.98659702       -0.98157947        -0.9860531       -0.9874930
5       -0.99463080       -0.98541870        -0.9907522       -0.9950738
6       -0.95707388       -0.92246261        -0.9480609       -0.9641607
fBodyAccJerkStdY fBodyAccJerkStdZ fBodyGyroMeanX fBodyGyroMeanY fBodyGyroMeanZ
1        0.1067399       -0.5347134     -0.3390322    -0.10305942    -0.25594094
2       -0.3817771       -0.7260402     -0.4926117    -0.31947461    -0.45359721
3       -0.1345800       -0.4017215     -0.3524496    -0.05570225    -0.03186943
4       -0.9825139       -0.9883392     -0.9761615    -0.97583859    -0.95131554
5       -0.9870182       -0.9923498     -0.9863868    -0.98898446    -0.98077312
6       -0.9322179       -0.9605870     -0.8502492    -0.95219149    -0.90930272
fBodyGyroStdX fBodyGyroStdY fBodyGyroStdZ fBodyAccMagMean fBodyAccMagStd
1    -0.5166919   -0.03350816    -0.4365622     -0.12862345     -0.3980326
2    -0.5658925    0.15153891    -0.5717078     -0.35239594     -0.4162601
3    -0.4954225   -0.18141473    -0.2384436      0.09658453     -0.1865303
4    -0.9779042   -0.96234504    -0.9439178     -0.94778292     -0.9284448
5    -0.9874971   -0.98710773    -0.9823453     -0.98535636     -0.9823138
6    -0.8822965   -0.95123205    -0.9165825     -0.86176765     -0.7983009
fBodyBodyAccJerkMagMean fBodyBodyAccJerkMagStd fBodyBodyGyroMagMean
1             -0.05711940             -0.1034924           -0.1992526
2             -0.44265216             -0.5330599           -0.3259615
3              0.02621849             -0.1040523           -0.1857203
4             -0.98526213             -0.9816062           -0.9584356
5             -0.99254248             -0.9925360           -0.9846176
6             -0.93330036             -0.9218040           -0.8621902
fBodyBodyGyroMagStd fBodyBodyGyroJerkMagMean fBodyBodyGyroJerkMagStd
1          -0.3210180               -0.3193086              -0.3816019
2          -0.1829855               -0.6346651              -0.6939305
3          -0.3983504               -0.2819634              -0.3919199
4          -0.9321984               -0.9897975              -0.9870496
5          -0.9784661               -0.9948154              -0.9946711
6          -0.8243194               -0.9423669              -0.9326607


## Write final data into viet.txt
write.table(result, file = "C:/Users/user/Desktop/RWD/viet.txt")