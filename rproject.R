###### libraries --------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)
#install.packages("cluster")
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra)
#install.packages("plotly")
library(plotly)

library(rpart)
library(rpart.plot)
###Import-----
dataset<- read_csv("Ecommerce.csv")
dataset1 <- dataset %>%
  mutate(
    InvoiceDate = dmy(InvoiceDate),
    Country = as.factor(Country),
    CustomerID = as.character(CustomerID)
    
  ) %>%
  select("InvoiceNo","Description","Quantity","InvoiceDate","UnitPrice","CustomerID","Country")
# for this analysis, we will drop the X9 columns as It is filled with only NA's also we wont require the stock code variable


### Understanding and cleaning of the dataset----


      # let's have a look at the distribution of our dataset. 
5
      #  with this graph, we can see that most of our sales come from clients not registered. As this can bias our analysis, we will drop data of clients not registered.

notreg <- which(is.na(dataset1$CustomerID))
datasetref <- dataset1[-notreg,]
      # lets revome duplicated values
dupes <- which(duplicated(datasetref))
datasetref <- datasetref[-dupes,]
      # lets now remove NA's
data <- datasetref[complete.cases(datasetref),]
      
      # Lets now get more understanding about the distribution of our dataset
##### analysis of the data -----

# Filtering for top 10 countries by transaction
Transactions_per_Country <- data %>%
  group_by(Country) %>%
  summarise('Number of Transcations' = n()) %>%
  arrange(desc(`Number of Transcations`))%>%
  top_n(10)

names(Transactions_per_Country) <- c("Country", "Number of Transactions")

library(ggthemes)


Transaction_per_Country_Visz <- ggplot(head(Transactions_per_Country,5), aes(x=reorder(Country,-`Number of Transactions`), y=`Number of Transactions`)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = `Number of Transactions`), vjust = -0.5) +
  ggtitle('Top 5 Countries by Number of Transactions') +
  xlab('Countries') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_Country_Visz)
      
        # From this graph, we can see that most of our transactions are from United Kingdom
        # For the purpose of our analysis, as more than 80% of our data come from a single location, we will only focus on United Kindom data.

UK_OnlineRetail <- data %>%
  filter(Country == 'United Kingdom')


# Lets dive into the statistics of how long customer have been inactive. 

# Calculating the number of transaction per CustomerID
User_Frequency <- UK_OnlineRetail %>%
  group_by(CustomerID) %>%
  summarise(Frequency = n())

# Creating a dataframe with unique CustomersID
Users_Recency <- UK_OnlineRetail %>% 
  group_by(CustomerID) %>%
  summarise(Last_Customer_Activity = max(InvoiceDate)) %>%
  mutate(Last_Invoice = max(Last_Customer_Activity)) 
Users_Recency$Recency<- round(as.numeric(difftime(Users_Recency$Last_Invoice, Users_Recency$Last_Customer_Activity , units = c("days"))))

Users_Recency <- Users_Recency %>%
  select(CustomerID, Recency)

# Calculate Revenue per CustomerID
Users_Monetary_Value <- UK_OnlineRetail %>%
  mutate(Revenue = Quantity * UnitPrice) %>%
  group_by(CustomerID) %>%
  summarise(Monetary_Value=sum(Revenue))

Users_RFM <- merge(Users_Recency, User_Frequency) # Merging Recency and Frequency
Users_RFM <- merge(Users_RFM, Users_Monetary_Value) # Merging Monetary Value
DT::datatable((Users_RFM),
              rownames = FALSE,
              options = list(
                pageLength = 10))


##### Clusters -----
# finding optimal number of clusters

fviz_nbclust(Users_RFM, kmeans, method = "silhouette")

# we will use 3 clusters for this analysis

clusters <- kmeans(scale(Users_RFM[,2:4]), 3, nstart = 1) # Performing kmeans with RFM variables and creating 3 clusters. 

Users_RFM$Cluster <- as.factor(clusters$cluster) # Attaching the results to CustomersID to identify each customer's cluster

# lets now have a look to our clusters
KMeans_Results <- Users_RFM %>%
  group_by(Cluster) %>%
  summarise('Number of Users' = n(),
            'Recency Mean' = round(mean(Recency)),
            'Frequency Mean' = scales::comma(round(mean(Frequency))),
            'Monetary Value Mean' = scales::dollar(round(mean(Monetary_Value))),
            'Cluster Revenue' = scales::dollar(sum(Monetary_Value))
  )

DT::datatable((KMeans_Results),
              rownames = FALSE) # Display cluster means to identify their value to the business


Cluster_size_visz <- ggplot(KMeans_Results, aes(Cluster, `Number of Users`)) +
  geom_text(aes(label = `Number of Users`), vjust = -0.3) +
  geom_bar(aes(fill=Cluster), stat='identity') +
  ggtitle('Number of Users per Cluster') + 
  xlab("Cluster Number") +
  theme_classic()
print(Cluster_size_visz)


####Hiererchical Clustering-----

ClusterTree <- Users_RFM %>%
  select(Frequency, Monetary_Value, Recency)


fit_tree <-rpart(Monetary_Value ~ ., 
                 data=ClusterTree,
                 method = 'anova', 
                 control= rpart.control(cp=0.0127102))

rpart.plot(fit_tree, type=1,extra=1, box.palette=c("gray","lightblue"))
