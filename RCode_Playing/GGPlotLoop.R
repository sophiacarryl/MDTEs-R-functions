library("ggplot2")
library("UsingR")
library("grid")
library("gridExtra")
library("reshape2")
library("dplyr")
library("tidyr")

df <- data.frame("Age" = c(21,15,15,15,16,21), 
                 "Sex" = c("Male", "Female", "Female", "Male", "Male","NA"),
                 "Height" = c(12,11,7,3,9,NA),
                 "Name" = c("John", "Dora", "Dora","Dora", "Anna", "John"), 
                 "Last" = c ("Henry", "Paul", "House", "Houze", "Henry", "Bill"), 
                 "Location" = c("Chicago", "Chicago", "Portland", "NYC", "NYC", "NYC"),
                 stringsAsFactors = FALSE)


ggplot(df, aes(x = Name)) +
  geom_bar(position = "identity")


output = vector("double", length(df), na.rm = TRUE)
    for (i in seq_along(df, na.rm = TRUE)) {
    output[[i]] <- mean(df[[i]])
}
output

Plotme = function(data){
  output = vector("double", length(data))
  for (i in seq_along(data)) {
    output[[i]] <- plot(mean(data[[i]]))
  }
  output  
}

Plotme(df)


class(df$Name)


df$Name = factor(df$Name)
df$Last = factor(df$Last )
df$Location = factor(df$Location)

str(df)

a = ggplot(df, aes(x=Name)) +
  geom_bar(aes(fill=Name)) +
  theme_classic()

b = ggplot(df, aes(x=Last)) +
  geom_bar(aes(fill=Last)) +
  theme_classic()

c = ggplot(df, aes(x=Location)) +
  geom_bar(aes(fill = Location)) +
  theme_classic()

grid.arrange(a, b, c)

# print(a, position = c(0, 0, 0.5, 1))
# print(b, position = c(0.5, 0, 1, 1))
# print(c, position = c(0.5, 0, 1, 1))

data.labor.long <- melt(df, id.vars=c("SN", "Age"), variable.name="category")


properties = c("Name", "Last", "Location")

for (property in properties) {
  p = ggplot(df, aes(x = properties[property])) +
    geom_bar(aes(fill = property)) +
    theme_classic()
  print (p)
}





plot <- 
  ggplot(subset(df, df$County==county_list[i]),
         aes(Year, value/1000, group = County, colour = category)) 
  


# properties = list("shipping_temperature", "blood_tube_type","procurement_temperature")
# 
# par(mfrow = c(2, 2)) 
# 
# for (property in properties) {
#   counts = table(df$properties)
#   barplot(counts, main="", 
#           xlab="",
#           ylab= "Counts",
#           las=3)
#   print (p)
# }


library(stringr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(pander)

# update this file path to point toward appropriate folders on your computer

# folder where you want the graphs to be saved:
results <- "/Users/majerus/Desktop/NJAIS/results/"  

# folder where the data is saved:
labor <- "/Users/majerus/Desktop/NJAIS/data/dept_labor/age_lvl/"

# create list of all .csv files in folder 
file_list <- list.files(path=labor, pattern="*.csv") 

# read in each .csv file in file_list and rbind them into a data frame called data.labor 
data.labor <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = as.numeric(str_sub(x, 1, 4)),
                         read.csv(paste(labor, x, sep=''), 
                                  stringsAsFactors = FALSE))))

# remove commas from numeric variables
data.labor[,c(3:12)] <- lapply(
  data.labor[,c(3:12)], 
  function(x) {as.numeric( 
    gsub(",", "", x))})

# drop 2010 from data then data and projections will occur in 5 year intervals 
data.labor <- subset(data.labor, data.labor$year!=2010)

colnames(data.labor) <- c("Year",   "County", "Total",  
                          "Under 5",  '5 to 9 years', '10 to 14 years', '15 to 19 years', 
                          "X20.24", "X25.29", "X30.34", "X35.39", "X40.44")

# select columns of interest
keep <- c("Year", "County", "Total", 'Under 5', 
          '5 to 9 years', '10 to 14 years', '15 to 19 years')

data.labor <- data.labor[keep]

# melt data to long format 
data.labor.long <- melt(data.labor, id.vars=c("County", "Year"), variable.name="category")

