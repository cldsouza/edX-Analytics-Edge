CPS = read.csv("CPSData.csv")

sort(table(CPS$State))
table(CPS$Region, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

mean(is.na(CPS$MetroAreaCode))

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)

#easier if output of above is sorted
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

# To merge in the metropolitan areas, we want to connect the field MetroAreaCode
# from the CPS data frame with the field Code in MetroAreaMap. 
# The following command merges the two data frames on these columns, 
# overwriting the CPS data frame with the result

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# Which metropolitan area has the highest proportion of interviewees of 
# Hispanic ethnicity? Hint: Use tapply() with mean, as in the previous 
# subproblem. Calling sort() on the output of tapply() could also be helpful 
# here

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean, na.rm=TRUE))

# Normally, we would look at the sorted proportion of interviewees from each
# metropolitan area who have not received a high school diploma with the command:
# sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))
# However, none of the interviewees aged 14 and younger have an education 
# value reported, so the mean value is reported as NA for each metropolitan 
# area. To get mean (and related functions, like sum) to ignore missing values,
# you can pass the parameter na.rm=TRUE. Passing na.rm=TRUE to the tapply function, 
# determine which metropolitan area has the smallest proportion of interviewees 
# who have received no high school diploma.

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))

# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether
# an interviewee is Asian, determine the number of metropolitan areas in the 
# United States from which at least 20% of interviewees are Asian

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# What proportion of the interviewees from the "New York-Northern 
# New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth
# that is not the United States? For this computation, don't include people
# from this metropolitan area who have a missing country of birth.
 
 table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

# Alternate method
 sort(tapply(CPS$Country != "United States", CPS$MetroArea == "New York-Northern New Jersey-Long Island, mean, na.rm=TRUE))
 



# To obtain the number of TRUE values in a vector of TRUE/FALSE values,
# you can use the sum() function. For instance, sum(c(TRUE, FALSE, TRUE, TRUE))
# is 3. Therefore, we can obtain counts of people born in a particular country
# living in a particular metropolitan area with:

 sort(tapply(CPS$Country == "India", CPS$MetroArea, mean, na.rm=TRUE))

# Alternate method

table(CPS$MetroArea == " Boston-Cambridge-Quincy, MA-NH", CPS$Country == "India")
