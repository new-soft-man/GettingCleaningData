run_analysis <- function() {
    
    require(dplyr)
    require(reshape2)
    
    wd=getwd()
    
    # Load activity labels 1 to 6
    dir=paste(wd,"/ProjectData/activity_labels.txt", sep="")
    x=read.table(dir, sep=" ", stringsAsFactors=FALSE)
    activities=data.frame(x)
    colnames(activities)=c("actNo", "activity")
    for (j in 1: nrow(activities)) {
        activities[j,"activity"] = as.character(activities[j,"activity"])
    }
    
    # Load Features to be used
    dir=paste(wd,"/ProjectData/features_selected.txt", sep="")
    features=read.table(dir,sep=",")
    colnames(features) = c("colNo", "varName")
    n=nrow(features)
    
    # Create a column # vector and related column variable name vector for Features
    cols=as.numeric(features[1,"colNo"])
    vars=as.character(features[1,"varName"])
    
    for (i in 2:n) {
        cols = c(cols, as.numeric(features[i,"colNo"]))
        vars = c(vars, as.character(features[i,"varName"]))
    }
    
    
    # Load Test subjects for all observations
    dir=paste(wd,"/ProjectData/test/subject_test.txt", sep="")
    subjects=data.frame(read.table(dir))
    
    # Load Test activities for all observations
    dir=paste(wd,"/ProjectData/test/y_test.txt", sep="")
    acts=data.frame(read.table(dir))
    
    # Load Test observations into a data frame
    dir=paste(wd,"/ProjectData/test/x_test.txt", sep="")
    x=read.table(dir)
    df=data.frame(x)
    # Select only mean and std-dev columns naming variables appropriately
    df=df[, cols]
    colnames(df)=vars
    
    # Add columns for Test activity and subject
    testdf=data.frame(Activity=NA_character_, Subject=0, df)
    testdf$Activity=as.character(testdf$Activity)
    testdf$Subject=as.numeric(testdf$Subject)
    
    # Update the Test data frame with activity tested, and subject #
    n=nrow(testdf)
    for (i in 1:n) {
        subno=subjects[i,1]
        actno=as.numeric(acts[i,1])
        act="???"
        for (j in 1: nrow(activities)) {
            if (actno == as.numeric(activities[j,"actNo"])) {
                act=as.character(activities[j,"activity"])
                break
            }
        }
        testdf[i,"Activity"] = act
        testdf[i,"Subject"] = subno      
    }
    
    
    # Load Train subjects for all observations
    dir=paste(wd,"/ProjectData/train/subject_train.txt", sep="")
    subjects=data.frame(read.table(dir))
    
    # Load Train activities for all observations
    dir=paste(wd,"/ProjectData/train/y_train.txt", sep="")
    acts=data.frame(read.table(dir))
    
    # Load Train observations into a data frame
    dir=paste(wd,"/ProjectData/train/x_train.txt", sep="")
    x=read.table(dir)
    df=data.frame(x)
    # Select only mean and std-dev columns naming variables appropriately
    df=df[, cols]
    colnames(df)=vars
    
    # Add columns for Train activity and subject
    traindf=data.frame(Activity=NA_character_, Subject=0, df)
    traindf$Activity=as.character(traindf$Activity)
    traindf$Subject=as.numeric(traindf$Subject)
    
    # Update the Train data frame with activity trained, and subject #
    n=nrow(traindf)
    for (i in 1:n) {
        subno=subjects[i,1]
        actno=as.numeric(acts[i,1])
        act="???"
        for (j in 1: nrow(activities)) {
            if (actno == as.numeric(activities[j,"actNo"])) {
                act=as.character(activities[j,"activity"])
                break
            }
        }
        traindf[i,"Activity"] = act
        traindf[i,"Subject"] = subno      
    }
    
    
    # Combine Test and Train data frames
    df = rbind(testdf, traindf)
    
    # Write the new combined file out
    dir=paste(wd,"/tidy_data.txt", sep="")
    write.table(df, file=dir, sep=",", row.names=FALSE)
    
    # Melt data to prepare for calculating averages
    molt=melt(df, id.vars=c("Activity", "Subject"), variable.name = "Variable", value.name = "Value")
    molt=arrange(molt, Activity, Subject, Variable)
    
    # Calculate averages by Activity, Subject and Variable
    avg=molt %>%
        group_by(Activity, Subject, Variable) %>%
        summarize(AvgVal = mean(Value) )
    
    # Write the new average value fileS out
    dir=paste(wd,"/avg_data.txt", sep="")
    write.table(avg, file=dir, sep=",", row.names=FALSE)
    
}
