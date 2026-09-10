load("titanic_train.RDATA") # Here we download the data
head(titanic.train)
library(ggplot2) #This library is widely used to create charts and data visualizations.
library(dplyr) #It is a library used to perform manipulations and transformations of data.
library(ggmap) #This library is used to integrate maps into graphs generated with ggplot2.
install.packages("maps")
library(maps) #Offers basemaps and geospatial data from different geographic regions.

#-------------------------------KIDS and Women---------------------------------------------------
#We create a new variable called KIDS to differenciate people over and under 18
#And we compare the data with the survival information.
aux = titanic.train$Age < 18
sum(aux)
kid = rep("No",length(aux))
kid[aux] = "Yes"
titanic.train = cbind(titanic.train, kid)
prop.table(table(titanic.train$kid, titanic.train$Survived),1)
ggplot(data = titanic.train) + aes(x = kid, fill = Survived) +
  geom_bar()+ scale_fill_manual(values = c("palegreen", "forestgreen"))
#Now we compare again with the variable of sex.
prop.table(table(titanic.train$Sex, titanic.train$Survived),1)
ggplot(data = titanic.train) + aes(x = Sex, fill = Survived) +
  geom_bar()+ scale_fill_manual(values = c("palegreen", "forestgreen"))
#Now we create the variable BOYS which are under 18 and male.
aux2 = titanic.train$Age < 18 & titanic.train$Sex == 2
sum(aux2)
boys = rep("No",length(aux))
boys[aux] = "Yes"
titanic.train = cbind(titanic.train, boys)
prop.table(table(titanic.train$Sex, titanic.train$Survived),1)
prop.table(table(boys, titanic.train$Survived),1)

#-------------------geom_histogram() of Pclass depending of Fare--------------------------------------------------


ggplot(data = titanic.train) +
   geom_histogram(bins = 100, color = "black",aes(x = Fare, fill = Pclass))+
  xlab("Fare") +
  ylab("People") + scale_fill_discrete(name = "Type of class")
#Here we are comparing the people´s fare and their type of class. 
#Here is obvious that the fare affects the class,since the more fare the higher the class.
#There is a point in fare that there are only first class.
#With this data we can define rich people as the ones in first class.


#-----------------------geom_count() of people survived depending their type of class---------------------------------------------


prop.table(table(titanic.train$Pclass, titanic.train$Survived),1)
ggplot(titanic.train) + aes(x = Pclass, y = Survived,  fill = Sex ) +
  geom_count(color = "forestgreen")
#This shows that the most part of people were at third class but they survived more of first.


#----------------------------geom_boxplot() of port of embarkation depending the passengers fare----------------------------------------------------------------


# Create the graph
ggplot(data = titanic.train, aes(x = Embarked, y = Fare)) +
  geom_boxplot(fill = "white", color = "forestgreen") +
  labs(
    x = "Port of Embarkation",
    y = "Passenger fare",
    title = "Passenger Fare by Port of Embarkation"
  ) +  theme(plot.title = element_text(hjust = 0.5))


#------------MAP of UK and France showing shipping ports and survival rates---------------------------------------------------------


prop.table(table(titanic.train$Embarked, titanic.train$Survived),1) #Here we see the survival percentages of each port

# We build a DataFrame with the survival percentages for each port
porcentajes <- data.frame(
  Embarked = c("C", "Q", "S"),
  Porcentaje_Supervivencia = c(57.58, 42.11, 32.57), #We put the percentages in the table
  Longitud = c(-1.6, -7, -1.4),  # Approximate longitudes set by hand to roughly match your actual position
  Latitud = c(49.6, 54.5, 51.3))  # Approximate latitudes set by hand to roughly match your actual position
  
# We build a DataFrame with port positions
puertos_embarque <- data.frame(
    Embarked = c("C", "Q", "S"),  # Values of the variable 'Embarked'
    Latitud = c(49.6, 54.5, 51.3),  # Approximate latitudes set by hand to roughly match your actual position
    Longitud = c(-1.6, -7, -1.4))  #Approximate longitudes set by hand to roughly match your actual position
  
  
  # Obtain geographical information from United Kingdom and France
  
  UK_France_map <- map_data("world", region = c("UK", "France"))  #We take the maps out of library(maps)       
  # Create the map
  #geom_polygon() to render the map, geom_point() for the ports,
  #coord_fixed() to make sure the distances in the graph stay proportional,
  #theme_void so that the graph is clean and without labels.
  mapa= ggplot() +
    geom_polygon(data = UK_France_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = puertos_embarque, aes(x = Longitud, y = Latitud, color = Embarked, size = 5)) +
    coord_fixed(1.3) + theme_void() + ggtitle("UK and France MAP") + theme(plot.title = element_text(hjust = 0.5))
  
  
  #Add survival percentages to the map
  #geom_text to add survival percentages for each port
  mapa + geom_text(data = porcentajes, aes(x = Longitud, y = Latitud, label = paste("Survival:", Porcentaje_Supervivencia, "%")), size = 3)
  
  

#--------------------------geom_violin of the Class depending of the age-------------------------------------------------------------


  ggplot(data = titanic.train) +
  geom_violin(aes(x = Pclass, y = Age, fill = Pclass))+
  xlab("Type of class") +
  ylab("Age") + 
  scale_fill_discrete(name = "Type of class")



