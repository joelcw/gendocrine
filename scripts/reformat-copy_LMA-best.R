data = read.delim(file.choose())
head(data)

#make repeition into a factor (rather than an integer)
data$repetition <- as.factor(data$rep)

#get a list of all the words
allSpeakers <- levels(data$speaker)
#run a loop: for each speaker...
for(speakLoop in 1:length(allSpeakers)){
  #get a list of all the words that speaker uses
  words = unique(sort(data$vars[data$speaker==allSpeakers[speakLoop]]))
  #for each word
  for(wordLoop in 1:length(words)){
    #get a list of all the repetitions
    allReps <- sort(unique(data$repetition[data$vars == words[wordLoop] & data$speaker == allSpeakers[speakLoop]]))
    #for each repetition
    for(repLoop in 1:length(allReps)){
      #get a list of all the segments
      allSegs = unique(data$segment[data$vars == words[wordLoop] & data$repetition == allReps[repLoop] & data$speaker == allSpeakers[speakLoop]])
      if (length(allSegs)==0) {
        allSegs = c("NA")
      }
      else {allSegs = unique(data$segment[data$vars == words[wordLoop] & data$repetition == allReps[repLoop] & data$speaker == allSpeakers[speakLoop]])}
      #for each segment...
      for(segLoop in 1:length(allSegs)){
        #assign the value for word to a variable
        wd = as.character(words[wordLoop])
        #assign the value for repetition to a variable
        rep = allReps[repLoop]
        #assigned the value for speaker to a variable
        speaker = allSpeakers[speakLoop]
        #assign the value for segment to a variable
        seg = as.character(allSegs[segLoop])

        ####HERE IS WHERE YOU START IDENTIFYING FEATURES#####
        #breathiness: for the word, repetition, and segment you've defined, get the duration of breathiness
        br <- data$feature_dur[data$vars == wd & data$repetition == rep & data$speaker == speaker & data$segment == seg & data$feature=="br"]
        #something weird happens if that segment doesn't have breathiess; if that's the case, assign NA
        if(length(br)==0) {br <- "NA"}

        cr <- data$feature_dur[data$vars == wd & data$repetition == rep & data$speaker == speaker & data$segment == seg & data$feature=="cr"]
        if(length(cr)==0) {cr <- "NA"}

        pre <- data$feature_dur[data$vars == wd & data$repetition == rep & data$speaker == speaker & data$segment == seg & data$feature=="pre"]
        if(length(pre)==0) {pre <- "NA"}

        ######################################################

        #if this is the first time through this loop, create a data frame (assign each of the variables defined above to a column)
        if(wordLoop==1 & repLoop==1 & speakLoop==1 & segLoop==1){
          newdata <- data.frame(cbind(wd, rep, speaker, seg, br, cr, pre))
        }
        #if this isn't the first time, create a data frame with this iteration's values and append it to the previous data frame (as a row)
        else{
          new <- cbind(wd, rep, speaker, seg, br, cr, pre)
          newdata <- rbind(newdata, new)
          #close all your loops.
        }
        #{cat(paste(paste0(allSegs[segLoop],repLoop,"; ")))}#print(c('segLoop = ',segLoop))
      }
      #{cat(paste(paste0(allReps[repLoop])))}#print()
    }
    #print(paste0("speakLoop=",speakLoop,"; ",allSpeakers[speakLoop],": ",words[wordLoop]))
  }
  print(paste0("speakLoop=",speakLoop,"; ",allSpeakers[speakLoop]))
}; beepr::beep()


head(newdata)
tail(newdata)
getwd()
#setwd("//Users//misprdlina//Desktop//")
# NOTE: all columns are exported as FACTORS, will have to be reimported
# DO NOT TRY TO CHANGE FACTORS TO NUMERIC FOR <NA>s!!! IT WILL FUCK UP THE NUMBERS!!
write.table(newdata, file="genderimp_LMA-best.csv", sep=",", row.names=F)
