# Part C1. Uploading Packages/ softwares----
library(tidyverse)
library(vegan)
library(iNEXT)
library(ape)
library(RSQLite)
library(BiocManager)
library(DECIPHER)
library(muscle)
library(Biostrings)

#PartC2. Obtaining data from BOLD----

#In India (country where i am from) Malaria is a very serious disease caused by a bite from a Mosquito infected with parasities. Malaria is rare in Canada and the United States. It is found in over 90 countries around the world, mainly in Africa, Asia, Oceania, and South and Central America. The risk of malaria is highest in parts of Oceania and in sub-Saharan Africa. Malaria is caused by a bite from a mosquito infected with parasites. Anopheles is a genus of Mosquito which cause malaria in humans in endemic areas. I am taking this assignment exercise to explore what information we have for this speccies. What is the species richness? How many specices have been collected from each country. 

# API tool was used to obtain data directly from BOLD database for our species 'Anopheles'. This data was accessed on Oct 1, 11.27 pm, and this is a public database.

Anopheles <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Anopheles&format=tsv")

# In environment window, we can see that we got an object named 'Anopheles' which has 11121 observations of 80 variables.

# Part C3. Data exploration----
# To start with, it is always important to check the class of the data object to see what type of data we are working with.

class (Anopheles)
# We have a tidyverse-style dataframe called as 'tibble'.

# It is also important to know what kind of data we have in each column or basically get a summary of the data contents.

str (Anopheles)
# This function gives us the details about our 80 variables but it is in complex format. We want a format that is easy to look at.

summary (Anopheles)
# This beautifully summarizes the length, class for character variables and other summary features (such as mean, median etc) for numeric variables.Everything is in order. For example, country, nucleotide, markercode is character variable, and lat lon is numeric.

# Next we want to have the names of all the variables.
names (Anopheles)

# This data object has too many variables that we don't need or are not interested in. So, we want to retain only the variables which we are interested in. Here, we are creating a new data frame called as Anopheles1 ( a subset of our main dataset, Anopheles). We are pulling out columns as numbered in the code. Square brackets are for indexing and comma is there is to include all the rows.
Anopheles1 <- Anopheles [, c(1,2,3,5,7,8,12,16,20,22,32,39,40,41,42,47,48,55,57,69,70,71,80)]
Anopheles1

# we want to check the names of columns, just to make sure we have all the columns that we need.
names (Anopheles1)

# By doing this, we caught that we don't have nucleotide variable in there. So, we are rewriting the code to include nucleotide (72) column.
Anopheles1 <- Anopheles [, c(1,2,3,5,7,8,12,16,20,22,32,39,40,41,42,47,48,55,57,69,70,71,72,80)]
Anopheles1

# checking names again.
names (Anopheles1)

#Lets focus on the latitude column for now. We want to know if we have any value outide the expected range. 
# First let's check the class of the variable latitide. we use '$' an indexing operator to take out lat column only.
class (Anopheles1)

#We are using hist funtion to build a histogram to visualize data. we use '$' an indexing operator to take out lat column only.

hist(Anopheles1$lat)

# So, most of our samples are between -10 to 10 latitude. It is fairly dispersed from -40 latitude to 60 latitude. No outliers in the data.

# Now, we are going to ask specific questions. 
#What is the northern-most sample? Here, we are taking out the lat column of Anopheles1 dataframe, and passing it to whihc.max fucntion which gives us the index position of the maximum value. Now, we can use this index position to look for processid of that record.
Anopheles1$processid[which.max(Anopheles1$lat)]

#"GBMIN55556-17" is the north-most sample.

# To check what is maximum latitude value, we use max function where we are removing the missing samples.
max(Anopheles1$lat, na.rm = TRUE)

# 66.87 is the maximum latidue where a sample was collected.

# If we want to know who collected the samples, then
Anopheles1$collectors[which.max(Anopheles1$lat)]

#NA, it is unknown who collected the sample.

# Now, I want to know about how many taxonomic species names are in the dataset? what is species richness? Unique fucntion is used to take out one copy of each unique species name and then length function is used to count the number of unique species i.e. length of this vector.
length(unique(Anopheles1$species_name))

#305, wow!
#Also, i want to know what is the species richness by BIN? Same style of coding.
length(unique(Anopheles1$bin_uri))

#288, not bad

#How many samples were collected north of the 40 degrees? We are using applying a condition that we want lat values which exceed 40 and we are passing this to sum fucntion which will give us the count of values exceeding 40.
sum (Anopheles1$lat > 40)

#NA
#Oops. I forgot to exclude the missing values. I should be aware of the default settings.

sum (Anopheles1$lat > 40, na.rm = TRUE)

# 404, there we go.

# Part C4. Exploring data on country basis.
# We want Summarize the count of records by country. Which country has the most barcode data? we are using table function.
table (Anopheles1$country)

# As we see here, data is organized in alphabetical order. We want to organize data decreasing order. 
sort(table(Anopheles1$country), decreasing = TRUE)

# Brazil, has the most barcode data.

# We can also do this by piping. We are staring with the dataframe we are wokring with. We use group_by function to group data from each country which is followed by summaring the data for each country and then counting the number of records in each country by length function. Finally, we are arranging data in decreasing roder so it will be easy for us to pick top countries if we want to work further.
Anopheles1 %>%
  group_by(country) %>%
  summarize(count = length(processid)) %>%
  arrange(desc(count))
Anopheles1
# Now, we want to eprform the diversity analysis. We want to know if we collect more samples from countries, do we add more new BINs. For that, we are going to use specaccum. Frist we need to transform data into that format. We are creating a new dataset 'Anopheles.by.country' grouped by country and bin_uri and we are counting the number of records per country per bin-uri.
Anopheles.by.country <- Anopheles1 %>%
  group_by(country, bin_uri) %>%
  count(bin_uri) 
Anopheles.by.country
# Next we need to remove the missing values from new dataset. Here we are using !is.na function to retain the records that are not NA means not missing. We are using 'filter' function to filter the data and creating a new data set 'Anopheles.by.country.na.rm' that does not have any missing values.
Anopheles.by.country.na.rm <- Anopheles.by.country %>%
filter(!is.na(country)) %>%
filter(!is.na(bin_uri))
Anopheles.by.country.na.rm
# Now we need to creat a comm object by using spread function. Our first argument is to "spread" our data Daphnia.by.country data. Second argument we want to spread by column. i.e. We want to take our column bin_uri and turn each BIN into its own column. Our third argument is we want the cells to contain counts.
Anopheles.spread.by.country <- spread(Anopheles.by.country.na.rm, bin_uri, n)
Anopheles.spread.by.country 
# checking class of the new dataset. It is important to check class to make sure we have created what we wanted.
class(Anopheles.spread.by.country)

# Here is the tidyverse type dataframe "tibble' which is what we wanted.

#Ror downstream analysis, we need to convert our NAs to 0. Zeroes in Anopheles.spread.by.country means that a given BIN hasn't been barcoded in that country.
Anopheles.spread.by.country[is.na(Anopheles.spread.by.country)] <- 0

# Perfect! we have replaced all NAs with zero. 
# We need our data all to be in numerical format to have a correct comm object type to pass to the function specaccum. we want all of our data to be in the columns and cells, not as a names attribute. We need to set the rownames as country, rather than having country as a data column.
Anopheles.spread.by.country <- Anopheles.spread.by.country %>%
  remove_rownames %>%
  column_to_rownames(var = "country")
Anopheles.spread.by.country

#Run a species accumulation curve analysis.
Anopheles.accum <- specaccum(Anopheles.spread.by.country)
Anopheles.accum

#Make a plot of the model.
plot(Anopheles.accum)

# Here, we can see, as samples are collected from countries, new species are being added.  
  
  
 



