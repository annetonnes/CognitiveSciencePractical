#############                   ASSIGNMENT 5 - REINFORCEMENT LEARNING
#############   In this assignment, you are expected to produce two graphs based on the output of the model with the function (do-fbt-n 100). Therefore, you will have 100 output files, which have 100 lines in each file, representing 100 children repeated the task 100 times.
#############   The first graph's x-axis (from 1 to 100) should present the number of repetitions of the model. The y-axis (from 0 to 1) should show the proportion of reasoning strategies in which the model used at each repetition. This means that the graph should have 3 lines for each reasoning level (i.e., 1 line for zero-order, 1 line for first-order and 1 line for second-order reasoning strategy).
#############   The second graph's x-axis (from 1 to 100) should present the number of repetitions of the model. The y-axis should show the utility values of the production rules that you wrote in the model qith !safe-eval! functions. Thee graph should have 3 lines for each reasoning level (i.e., 1 line for zero-order reasoning strategy's utility, 1 line for first-order and 1 line for second-order reasoning strategy's utility).
#############   Based on the proportion of the reasoning strategies graph, you are expected to write one qualitative prediction for your model.
#############   The prediction will be related to children's wrong answers before they start to give correct second-order answers most of the time. 
#############   If you have more predictions, you can write them, too. 
#############   I will provide you the behavioral results after you submit this assignment.
#############   Below you can find some hints to write the code for the graphs but you don't have to use the hints and you can do it in your own way, too.

#setwd("C:/Users/jelle/Dropbox/uni/jaar3/CognitiveSciencePractical/ACTR_model_output")  #Jelle ## set your workspace to the path where your output files are.
#setwd("\Annet\...\ACTR_model_output")  #Jelle ## set your workspace to the path where your output files are.
#setwd("\Danielle\...\ACTR_model_output")  #Jelle ## set your workspace to the path where your output files are.
setwd("X:/My Documents/CSP/ACTR_model_output")  #Jelle ## set your workspace to the path where your output files are.

## You might need to use the following packages
require(ggplot2) #install.packages("name") if not installed yet
require(reshape2) 
require(tidyr)  
require(plyr)

## The below function will be used in order to print the two graphs together

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

##################################################################


### Before you make the graphs, you should read the output files into R and make merge them into one big data file that has 16 columns and 10,000 rows.

####You need to write a for loop in order to read the files.

#Before writing the for loop, do these two steps:

#1) create a blank matrix which has 17 columns in order to prepare to store the information in the outputs and assign it to "dat" below.
dat<- matrix(ncol=17) 
#2)produce a character vector of the names of txt output files in the named directory and assign it to "files" below.
files<- c("dat-0.txt", "dat-1.txt", "dat-2.txt", "dat-3.txt", "dat-4.txt", "dat-5.txt", "dat-6.txt", "dat-7.txt", "dat-8.txt", "dat-9.txt", "dat-10.txt", "dat-11.txt", 
"dat-12.txt", "dat-13.txt", "dat-14.txt", "dat-15.txt", "dat-16.txt", "dat-17.txt", "dat-18.txt", "dat-19.txt", "dat-20.txt", "dat-21.txt", "dat-22.txt", "dat-23.txt", "dat-24.txt", "dat-25.txt", "dat-26.txt", "dat-27.txt", "dat-28.txt",
"dat-29.txt", "dat-30.txt", "dat-31.txt", "dat-32.txt", "dat-33.txt", "dat-34.txt", "dat-35.txt", "dat-36.txt", "dat-37.txt", "dat-38.txt", "dat-39.txt", "dat-40.txt", "dat-41.txt", "dat-42.txt", "dat-43.txt", "dat-44.txt", "dat-45.txt", "dat-46.txt", "dat-47.txt", "dat-48.txt", "dat-49.txt", "dat-50.txt",
"dat-51.txt", "dat-52.txt", "dat-53.txt", "dat-54.txt", "dat-55.txt", "dat-56.txt", "dat-57.txt", "dat-58.txt", "dat-59.txt", "dat-60.txt", "dat-61.txt", "dat-62.txt", 
"dat-63.txt", "dat-64.txt", "dat-65.txt", "dat-66.txt", "dat-67.txt", "dat-68.txt", "dat-69.txt", "dat-70.txt", "dat-71.txt", "dat-72.txt", "dat-73.txt", "dat-74.txt", 
"dat-75.txt", "dat-76.txt", "dat-77.txt", "dat-78.txt", "dat-79.txt", "dat-80.txt", "dat-81.txt", "dat-82.txt", "dat-83.txt", "dat-84.txt", "dat-85.txt", "dat-86.txt", 
"dat-87.txt", "dat-88.txt", "dat-89.txt", "dat-90.txt", "dat-91.txt", "dat-92.txt", "dat-93.txt", "dat-94.txt", "dat-95.txt", "dat-96.txt", "dat-97.txt", "dat-98.txt", "dat-99.txt")

#Now write a for loop for each file 
for (i in files) {     
      # read each file in table format and create a dataframe (read.table function) and assign it to "X" below.
    X<- read.table(i) ;
   for (j in 1:100) {      # I provide you another for loop in order to add a new column (17th column) and add numbers to that column's rows from 1 to 100 for each file. You will use this new 17th column with numbers 1 to 100 for the x-axes in your graphs. 
       X$V17[j]<-j
       ##TODO: Fix this.
       #gsub("\\(|\\)","",X$V5[j]) #Trying to remove brackts but doesn't work.
   }
    #now, you should combine the X and dat matrices by rows and assign it to "dat" below.
    dat<- rbind(dat, X)
    #now, I provide you some manipulations in the data together with their explanations
    ds<-paste("",i, sep="")      #this adds " " to the name of file
    ds<-substr(ds, 1, nchar(ds)-4)#this removes the last 4 char (.txt)
    assign(ds, X)          # this assigns X to ds
}
dat = dat[-1,]      #this deletes the first row


## now, look at your dat matrix and if you see ERROR in some cells, change all of those ERRORs to the value 0.
        
####### In order to produce the proportion of reasoning strategies graph:

# Encode the matrix named dat 's V16 column as a factor below. V16 is the column that shows the reasoning levels that was pushed to *response* function in your models (i.e, 0,1,2).
dat$V16<- factor(dat$V16)
# Build a contingency table (the function is called table) of the counts at each combination of V16 and V17 columns and make that table a dataframe and assign it to "y.df" below. When you use the table function together with the as.data.frame function, the y.df will have 3 columns named Var1, Var2, and Freq and 300 rows. Var1 shows the reasoning levels; Var2 shows the number of simulations (from 1 to 100, meaning that over time) 
y.df<- as.data.frame(table(dat$V17, dat$V16))
# Divide the Freq column of y.df by 100 and assign to the new column called proportion below:    
y.df$proportion<- y.df$Freq/100
# Encode y.df$Var1 as factor and assign it to y.df$Var1 below:   
y.df$Var1<- factor(y.df$Var1)
# Encode y.df$Var2 as numeric and assign it to y.df$Var2 below:      
y.df$Var2<- factor(y.df$Var2)

# Below, write 3 lines of code in order to change the name of the levels of factor Var1 from 0 to "Zero-order"; from 1 to "First-order"; from 2 to "Second-order".          
levels(y.df$Var2)[levels(y.df$Var2)=="0"] <- "Zero-order"
levels(y.df$Var2)[levels(y.df$Var2)=="1"] <- "First-order"
levels(y.df$Var2)[levels(y.df$Var2)=="2"] <- "Second-order"
    
## now, you can make the first line graph showing the proportion of reasoning strategies over time (Var2).
y.zeroorder <- y.df[1:100,c(1,2,3,4)]
y.firstorder <- y.df[101:200,c(1,2,3,4)]
y.secondorder <- y.df[201:300,c(1,2,3,4)]

plot(0:100, (0:100)/100, type="n")
lines(y.zeroorder$Var1, y.zeroorder$proportion, col="green")
lines(y.firstorder$Var1, y.firstorder$proportion, col="red")
lines(y.secondorder$Var1, y.secondorder$proportion, col="blue")
    
####### In order to produce the utility values graph:   
        
# write a for loop that takes the average of V2 column for each time point (V17) and assign it to meanVectorZero
meanVectorZero<-c(1)
for (i in 1:100) {
  theseRows <- which( dat$V17 == i, arr.ind=TRUE)
  tot <- 0
  for (j in 1:100) {
    tot <- tot + dat[theseRows[j],2]
  }
  meanVectorZero[i]<-tot / 100    # write here if 17th column of dat is equal to 1,2,..100 get the mean of the second column
}

# write a for loop that takes the average of V7 column for each time point (V17) and assign it to meanVectorFirst

meanVectorFirst<-c(1)
for (i in 1:100) {
  theseRows <- which( dat$V17 == i, arr.ind=TRUE)
  tot <- 0
  for (j in 1:100) {
    tot <- tot + dat[theseRows[j],7]
  }
  meanVectorFirst[i]<-tot / 100    # write here if 17th column of dat is equal to 1,2,..100 get the mean of the second column
}

# write a for loop that takes the average of V12 column for each time point (V17) and assign it to meanVectorSecond

meanVectorSecond<-c(1)
for (i in 1:100) {
  theseRows <- which( dat$V17 == i, arr.ind=TRUE)
  tot <- 0
  for (j in 1:100) {
    tot <- tot + dat[theseRows[j],12]
  }
  meanVectorSecond[i]<-tot / 100    # write here if 17th column of dat is equal to 1,2,..100 get the mean of the second column
} 


# combine meanVectorZero,meanVectorFirst and meanVectorSecond by column and assign it to utility Values below
utilityValues<- cbind(meanVectorZero, cbind(meanVectorFirst, meanVectorSecond))

# create a vector called time that contains integers starting from 1 to 100 and assign to time below.
time<-c(1:100)

# combine time and utilityValues by column and assign it to utilityValues below
utilityValues<-cbind(time,utilityValues)
  
# make utilityValues a data frame and assign it to utilityValues below
utilityValues<-data.frame(utilityValues)


####### Make the utility values graph
  
# Before you make the graph, you need to convert the data to long data format and do couple of things. Below I provide you the code for these things.   

utilityValues_long <- gather(utilityValues, reasoning.level, utility.value, meanVectorZero:meanVectorSecond)

utilityValues_n <- utilityValues_long 
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorZero"] <- "Zero-order"
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorFirst"] <- "First-order"
levels(utilityValues_n$reasoning.level)[levels(utilityValues_n$reasoning.level)=="meanVectorSecond"] <- "Second-order"

names(utilityValues_n)[names(utilityValues_n)=="Reasoning level"]  <- "reasoning.level"

### Now, the data you will use for this graph is utilityValues_n. x is time and y is utility value. Similar to the first graph, second graph will have three lines indicating each reasoning strategies' utilities over time (1 to 100)
##HERE! TO DO
plot(0:100, (-5:20), type="n")
lines(y.zeroorder$Var1, y.zeroorder$proportion, col="green")
lines(y.firstorder$Var1, y.firstorder$proportion, col="red")
lines(y.secondorder$Var1, y.secondorder$proportion, col="blue")


## Now, use multiplot function in order to present the two graphs together in 2 columns

