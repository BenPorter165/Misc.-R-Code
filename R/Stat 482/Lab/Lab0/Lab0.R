#Lab 0 - Introduction to R

##################################################################
#In R, everytime you write a #, anything that follows it in that 
#line, R will ignore. A # is simply a place for you to write 
#comments and notes to yourself about what you are doing.
##################################################################

#In RStudio, the screen is split into 4 sections.

#Top left 
  #R Script - where you will write code that you want to save 
  #(and where you see these words)
#Bottom left
  #Console - where R shows you the code you ran and gives 
  #statistical output.
#Top right
  #Information area - where you will find your datasets, variables, 
  #commands, etc. for your session so far.
#Bottom right
  #Connections area - where you will find packages, plots, 
  #and help files for all functions in R. You can also see 
  #files on your computer.

###################################################################
##Importing a Data Set
###################################################################
#It's usually a good idea when dealing with data, to store your
#data set as a .csv file. Most software packages, like R, can easily
#handle this type of file. So, I will be giving you .csv files for
#our data sets this semester.

#In this lab, we will look at data collected from students in our
#introductory statistics courses here at Iowa State. The name of the 
#data file is HeightsData.csv and is located with Lab 0 in Canvas. 
#You will need to download the data set to your computer and make 
#note its location. The variables in this data set are:
  
#Gender: The student's self-identified gender. Options given were
  #Female, Male, or Other. The number of observations for Other did
  #not meet the minimum number of 5 for inclusion in shared data so 
  #those observations have been omitted from this data set.
#Height: The self-identified height of the student in inches.
#IdealHeight: The height of the student's ideal romantic partner.

#To import the data set to R, we will use the function read.csv. 
#Run the code below by placing your cursor somewhere in the line 
#and clicking the Run button at the top of this panel. Once you
#run this code, a window will open where you will select the file
#from its location of your computer. Navigate to the file's 
#location, select the file, and then click Open.

  heights<-read.csv(file.choose(), header=T) 

#A little explanation about the code: The first word (heights) is 
#a name I made for the dataset in R. You can use any name you 
#want, but it should have some meaning or connection to the data. 
#Try not to make it too long or too short.

#The argument header=T (T for true) is telling R that the first line
#in the .csv file has the names of the variables in it. If the .csv 
#file did not have variable names in the first row, you would type 
#header=F (F for false).
  
#The option file.choose() tells R to open the window for the user
#to select the location of the file on the computer. Alternatively,
#you can type in the location of the file on your computer (the path 
#name). For example, the path name on my computer for the data file 
#I just uploaded to R is: 
#C:/Users/amyf/Box/My Documents/Classes/Spring 2021 - STAT 482/Labs/Lab 0-Intro to R

#Now, we want to check to see if the data uploaded correctly.
#In the top right window, you will see a list of Data. Click on 
#heights. You should see a new tab in the R Script panel with a
#spreadsheet of the data. You can also use the commands head() 
#and tail() to view the first 6 or last 6 rows of the data set.
    head(heights)
    tail(heights)

#Go to the lab assignment to answer questions 1 and 2
  
###################################################################
##R Packages
###################################################################
#A package is a bundle of code that someone created to share with 
#other R users. Before you can use a package, you need to install it on 
#your computer. You need to do this ONLY ONCE.

#To install a package, go to the bottom right window, click on 
#Packages, Install, and type in the name of the package. We will 
#like to use a package called psych, so type this name in the prompt 
#and click install. You will see coding and information in the 
#Console panel. Once the package has been installed, you will see
#a command prompt (>).
    
#Once you install a package on your computer, it is available for
#you to use whenever you would like. However, you need to tell R to 
#open the package and make the functions in the package available
#for use in your current RStudio session. To do this, you use the
#function library(packagename), where packagename = name of the 
#package you wish to open. Here is the code to open the psych 
#package.
  library(psych)

###################################################################
##Descriptive Statistics
###################################################################
#Let's start with some descriptive statistics. R has many built-in
#functions for calculating statistics. To specify which variable 
#to use from a data table, use the $ between the name of the data
#set and the name of the variable. A few of the possible descriptive
#statistics for the variable Height in the data set heights are 
#listed below.
  mean(heights$Height)   #mean
  var(heights$Height)    #variance
  sd(heights$Height)     #standard deviation
  min(heights$Height)    #minimum value
  max(heights$Height)    #maximum value
  median(heights$Height) #median
  IQR(heights$Height)    #interquartile range
  range(heights$Height)  #range
    
#The describe() function from the psych package gives many different
#statistics for a quantitative variable. 
  describe(heights$Height)

#The output includes:
  #var: variable id
  #n: number of observations
  #mean: mean 
  #sd: standard deviation
  #median: median
  #trimmed: 10% trimmed mean
  #mad: median absolute deviation
  #min: minimum value
  #max: maximum value
  #range: maximum - minimum
  #skew: skew statistic
  #kurtosis: kurtosis statistic
  #se: standard error

#Go to the lab assignment to answer questions 3, 4 and 5
###################################################################
##Graphs
###################################################################

#R can create many interesting graphs, and there are usually several
#ways to make a particular graph. For this class, we will use a 
#package called ggplot2, originally developed here at Iowa State 
#University by one of our Ph.D. graduates, which allows us to create
#many types of graphs using the same grammar of graphics.

#To install the ggplot2 package, go back to the bottom right panel
#and click on Packages, Install, and type in ggplot2 for the name
#of the package. You will see coding and information in the 
#Console panel. Once the package has been installed, you will see
#a command prompt (>).

#Remember to use the functions in ggplot2, we must use the library()
#function to open this package. 
  library(ggplot2)

#Here we are going to make a histogram of the variable Height. The
#basic function to start building a graph is ggplot. In this function
#we tell ggplot which data set we will be using in our plot, which
#variable(s) we will be using, and where in the graph they will be
#located. In this example, the ggplot call will be
  
#ggplot(heights, aes(x = Height))+
  
#We have placed a plus sign at the end of the line to signify there
#is more code coming. Now, we need to specify what type of graph
#we wish to make. There are lot of options here, but since we want
#to look at the distribution of a quantitative variable, we will 
#use geom_histogram. With this graph type, you can specify a number
#of options to fine-tune the output. Here, we will choose to specify
#the number of bins to use in this histogram.

#geom_histogram(bins = 10)+
  
#We can also specify other aspects of the graph. The code below adds
#names for the x and y axes and gives the graph a title. 
  
#labs(x = "Height (in inches)",
#     y = "Number of students",
#     title = "Height of Students in STAT 101")+
  
#Finally, you can change some of the visual components to the graph,
#like the background, the font sizes, etc. Here is some code to 
#do just that.
  
#  theme_bw()+
#  theme(axis.title.y = element_text(size = rel(1.4)),
#        axis.title.x = element_text(size = rel(1.4)),
#        axis.text.x = element_text(size = rel(1.6)),
#        axis.text.y = element_text(size = rel(1.6)),
#        plot.title = element_text(hjust=0.5, size = rel(2)))

#Now run the code below to produce your histogram.

  ggplot(heights, aes(x = Height))+
    geom_histogram(bins = 10)+
    labs(x = "Height (in inches)",
         y = "Number of students",
         title = "Height of Students in STAT 101")+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.6)),
          axis.text.y = element_text(size = rel(1.6)),
          plot.title = element_text(hjust=0.5, size = rel(2)))

#You should see your graph in the bottom right panel of the window. 
#You can get a better look at the graph by clicking on the Zoom 
#button. In the window that appears, you can rescale the graph or
#go full screen. If you would like to save the graph, you can click
#the Export option and choose Save as Image. Save this graph to your 
#computer as an Image file (usually .png or .jpg are the best options)

#Go to the lab assignment to answer question 6. 
  
#Once you are finished, please submit your Lab 0 Assignment.