#This is Arshdeep's Script, and I (Nadin Ibrahim) editted/added to the script.
#There are 4 major edits (marked with: ###Nadin's Addition #n)
#There are minor edits throughput: for grammar/spelling; adding package installations; changing commented output values (e.g. number of unique species) since the Anopheles data from BOLD contains more samples than during Arshdeep's initial analysis; and adding extra comments and a concluding paragraph
#The script is copied and repeated twice, at the top with Arshdeep's original script (without my edits), and at the bottom (with my edits). Please scroll to line 155 to view the edits.

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

####################################----Nadin's Edits to the Script---#############

  
# Part C1. Uploading Packages/ softwares----

#install.packages("tidyverse")
library(tidyverse)
#install.packages("vegan")
library(vegan)
#install.packages("iNEXT")
library(iNEXT)
#install.packages("ape")
library(ape)
#install.packages("RSQLite")
library(RSQLite)

#Code for installation of needed Bioconductor packages for older R versions (3.4 or earlier).
#source("https://bioconductor.org/biocLite.R")
#biocLite("Biostrings")
#biocLite("muscle")
#biocLite("DECIPHER")

#Code for installation for R version 3.5 or greater. You can install whichever Bioconductor packages you need.
#Instructions from: https://bioconductor.org/install/
#install.packages("BiocManager")
library(BiocManager)
#BiocManager::install(c("Biostrings", "DECIPHER", "muscle"))

library(DECIPHER)
library(muscle)
library(Biostrings)

#PartC2. Obtaining data from BOLD----

#In India (the country where I am from), Malaria is a very serious disease caused by the bite of a Mosquito infected with parasities. Malaria is rare in Canada and the United States. It is found in over 90 countries around the world, mainly in Africa, Asia, Oceania, and South and Central America. The risk of malaria is highest in parts of Oceania and in sub-Saharan Africa. Malaria is caused by a bite from a mosquito infected with parasites. Anopheles is a genus of Mosquito which cause malaria in humans in endemic areas. I am taking this assignment exercise to explore what information we have for this species. What is the species richness? How many specices have been collected from each country. 

# API tool was used to obtain data directly from BOLD database for our species 'Anopheles'. This data was accessed on November 11, 6:15 pm, and this is a public database.

Anopheles <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Anopheles&format=tsv")

# In environment window, we can see that we got an object named 'Anopheles' which has 11833 observations of 80 variables.

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

###Nadin's Addition #1: We can also visualize the latitude of records, among countries. If we want to show specific quantitative values, a dot plot can be a good option. Latitide information is good to analyze, to get a sense of what climate the genus Anopheles typically resides within. Latitude provides the location of a place north or south of the equator and is expressed by angular measurements ranging from 0? at the equator to 90? at the poles. With negative degrees below the equator and positive degrees above the equator. Different latitudes on Earth receive different amounts of sunlight, and are a key factor in determining a region's climate. 

#First let's filter out the samples without latitudes recorded
Anopheles_latitude_filtered <- Anopheles1 %>%
  filter(!is.na(lat))

#To create the dot plot we can use the ggplot2 package from the tidyverse package
ggplot(data = Anopheles_latitude_filtered) + 
  geom_point(mapping = aes(x = country, y = lat), stat = "identity", colour = "turquoise") +
  labs(title = "Latitides of Anopheles Records from BOLD by Country", x = "Countries", y = "Latitude") +
  coord_flip() +
  scale_x_discrete(limits = c(sort(x = unique(Anopheles_latitude_filtered$country), decreasing = T)))

# From the histogram we can see that most of our samples are between -10 to 10 latitude. It is fairly dispersed from -40 latitude to 60 latitude, with seeminly no outliers in the data.
# Looking at the dot plot, we can see that while the genus is present in various different countries, it tends to be most present in warmer countries like Brazil, Colombia, and Australia 

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

#320, wow!
#Also, i want to know what is the species richness by BIN? Same style of coding.
length(unique(Anopheles1$bin_uri))

#293, not bad

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

###Nadin's Addition 2: It would be interesting to create a bar graph visualization of the data to show the count of records among countries (ignoring the samples without countries recorded)!

#First let's filter out the samples without countries recorded 
Anopheles2 <- Anopheles1 %>%
  filter(!is.na(country))

#We can create a bar graph using the gglpot2 package from the tidyverse package. We will keep the Country names on the y-axis and the number of specimens on the x-axis and also fill in the colour of the barplots to give them a turquoise colour
ggplot(data = (Anopheles2)) + 
  geom_bar(mapping = aes(x = country), stat = "count", fill = "turquoise") +
  labs(title = "Number of Specimens (Anopheles) Sampled From Each Country", x = "Countries", y = "Number of Specimens") + coord_flip() + theme(text = element_text(size=9))

#Note: there are a lot of countries, so the font is rather small. If you would like to zoom in on the bar graph pictures (to have the county names larger and less squished among one another), simply click the "Zoom" button above the plot image in R studio.

# Now, we want to perform the diversity analysis. We want to know if we collect more samples from countries, do we add more new BINs. For that, we are going to use specaccum. Frist we need to transform data into that format. We are creating a new dataset 'Anopheles.by.country' grouped by country and bin_uri and we are counting the number of records per country per bin-uri.
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

#More downstream analysis, we need to convert our NAs to 0. Zeroes in Anopheles.spread.by.country means that a given BIN hasn't been barcoded in that country.
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
# Sampling does not seem complete yet based on this species accumulation curve as the curve has not begun to level off. More sampling should be performed to get a better representation of true species diversity.

###Nadin's Addition 3: Using the vegdist() function from the vegan package to look at the dissimilarity in community composition between countries and to create a cluster dendrogram by country

Anopheles_clustered <- vegdist(Anopheles.spread.by.country, method="bray") %>%
  hclust(method = "ward.D2")

plot(Anopheles_clustered, cex = 0.7)

#Here we create a cluster dendrogram by country using BIN URI. This is an interesting way to assess the similarty/dissimilarity among the different Anopheles species around the world. For example, on the far right, we can see that Djibouti and Ethiopia likely contain very similar species. 

###Nadin's Addition 4: What is the most common species in India and Canada?

#Canada
Anopheles.by.country.Canada <- Anopheles1 %>%
  group_by(country, species_name) %>%
  count(species_name) %>%
  filter(country == "Canada") %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species_name)) %>%
  arrange(desc(n)) %>%
  print()


#India
Anopheles.by.country.India <- Anopheles1 %>%
  group_by(country, species_name) %>%
  count(species_name) %>%
  filter(country == "India") %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species_name)) %>%
  arrange(desc(n)) %>%
  print()

#Canada: Anopheles punctipennis (24). This species is a vector of P. vivax, one of the protozoans that cause malaria.(https://en.wikipedia.org/wiki/Anopheles_punctipennis)
#India: Anopheles fluviatilis (60). This species is known to be a highly efficient malaria vector in hilly regions of India. (https://map.ox.ac.uk/bionomics/anopheles-fluviatilis/)

#Interentingly, the most common mosquito species in both Canada and India can both cause malaria (Note: while many Anopheles can transmit malaria, not all do). I was not expecting that the most common mosquito (Anopheles) species in Canada (based on the BOLD data) could cause Malaria. 
#Canada sees an average of 488 malaria cases per year spread across the country. Whereas India can get about 2-15 million cases a year. 
#References:
#https://www.canada.ca/en/public-health/services/diseases/malaria/surveillance-malaria.html
#https://www.ncbi.nlm.nih.gov/books/NBK1720/


###Nadin's Addition 5: Concluding Paragraph

#Based on the analyses above, the genus Anopheles is scattered around the world. However, it seems to be most commonly present near the equator (between latitides -10 and 10) and so seems to have a preference for warmer climates. This is a known fact, as mosquitos are cold-bolded and so require warm temperatures in order to flurish. It is an extremely diverse taxon, and based on the BOLD data, there were 320 classified species. After assessing the number of specimens sampled around the world, it was found that many of the samples were collected from Brazil, and much less from other countries. After creating a species accumulation curve, it seemed that sampling completeness has not been achieved based on the data available on BOLD, and so more sampling should be performed to gain a better understanding of genus diversity. Furthermore, creating a cluster dendrogram by country using BIN URI, showed that countries more close to each other had similar species (which is expected). And in a final analysis of the most common species of Anopheles in Canada and India, it was found that both species were known malaria vectors. I expected this for India but not for Canada, given the smaller rates of malaria per year in Canada vs. India. Overall, this was a very fascinating analysis of the genus Anopheles to assess species richness around the world.


