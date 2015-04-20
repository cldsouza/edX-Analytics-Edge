
#Load Songs data

Songs = read.csv("songs.csv")

MichaelJackson = subset(Songs, artistname == "Michael Jackson")

MJTop10 = subset(Songs, artistname == "Michael Jackson" & Top10 == 1)


MichaelJackson[c(“songtitle”, “Top10”)]

# The variable corresponding to the estimated time signature (timesignature) is discrete,
# meaning that it only takes integer values (0, 1, 2, 3, . . . ).
# What are the values of this variable that occur in our dataset?
table(Songs$timesignature)
hist(Songs$timesignature)

#the song with the highest tempo
which.max(songs$tempo) 

SongsTrain = subset(Songs, year <= 2009)
SongsTest = subset(Songs, year == 2010)


# Making model
# SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
# excluding some independent variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

#Construct a model without the variable loudness
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

#Construct a model without the variable energy
#this approach (subtracting the variable from the model formula) will 
#always work when you want to remove numeric variables.

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

# Predictions on the test set
predictTest = predict(SongsLog3, type="response", newdata=SongsTest)

# Confusion matrix with threshold of 0.45
table(SongsTest$Top10, predictTest > 0.45)




