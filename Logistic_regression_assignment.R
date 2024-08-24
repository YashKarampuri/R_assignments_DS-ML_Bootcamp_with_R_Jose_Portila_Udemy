# Logistic Regression assignment

# Getting the data

adult <- read.csv('adult_sal.csv')

head(adult)

# Dropping the repeated index column

library(dplyr)
adult <- select(adult,-X)

head(adult)

str(adult)

summary(adult)

# Cleaning the data

table(adult$type_employer)

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)

table(adult$type_employer)

# Marital Column

table(adult$marital)

# Reducing this to three groups:

# Married
# Not-Married
# Never-Married

group_marital <- function(mar){
  mar <- as.character(mar)

  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')

    # Never-Married
  }else if(mar=='Never-married'){
    return(mar)

    #Married
  }else{
    return('Married')
  }

  adult$marital <- sapply(adult$marital,group_marital)
  table(adult$marital)

  # Country Column

  table(adult$country)

  levels(adult$country)

  Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
            'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

  North.America <- c('Canada','United-States','Puerto-Rico' )

  Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
              'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

  Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                               'El-Salvador','Guatemala','Haiti','Honduras',
                               'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                               'Jamaica','Trinadad&Tobago')
  Other <- c('South')

  group_country <- function(ctry){
    if (ctry %in% Asia){
      return('Asia')
    }else if (ctry %in% North.America){
      return('North.America')
    }else if (ctry %in% Europe){
      return('Europe')
    }else if (ctry %in% Latin.and.South.America){
      return('Latin.and.South.America')
    }else{
      return('Other')
    }
  }

  adult$country <- sapply(adult$country,group_country)

  table(adult$country)

  str(adult)

  adult$type_employer <- sapply(adult$type_employer,factor)
  adult$country <- sapply(adult$country,factor)
  adult$marital <- sapply(adult$marital,factor)

  str(adult)

  # Missing Data

  install.packages('Amelia',repos = 'http://cran.us.r-project.org')
  library(Amelia)

  adult[adult == '?'] <- NA

  table(adult$type_employer)

  adult$type_employer <- sapply(adult$type_employer,factor)
  adult$country <- sapply(adult$country,factor)
  adult$marital <- sapply(adult$marital,factor)
  adult$occupation <- sapply(adult$occupation,factor)

  # Missingness Map

  missmap(adult)

  missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','black'))

  adult <- na.omit(adult)
  #str(adult)

  missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','black'))

  # Data analysis

  str(adult)

  library(ggplot2)
  library(dplyr)

  ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

  ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()

  names(adult)[names(adult)=="country"] <- "region"

  str(adult)

  ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Building a model to classify people into two groups: Above or Below 50k in Salary. (Logistic Regression)

  head(adult)

  # Train Test Split

  # Import Library
  library(caTools)

  # Set a random see so your "random" results are the same as this notebook
  set.seed(101)

  # Split up the sample, basically randomly assigns a booleans to a new column "sample"
  sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

  # Training Data
  train = subset(adult, sample == TRUE)

  # Testing Data
  test = subset(adult, sample == FALSE)

  help(glm)

  library(glmnet)

  # Assuming train is your training dataset
  x <- model.matrix(income ~ ., data = train)[, -1]  # excluding intercept column
  y <- train$income

  # Fit a logistic regression model with regularization
  model <- glmnet(x, y, family = "binomial")

  summary(model)

  help(step)

  new.step.model <- step(model)

  summary(new.step.model)

  # Confusion matrix:

  test$predicted.income = predict(model, newdata=test, type="response")

  table(test$income, test$predicted.income > 0.5)

  (6372+1423)/(6372+1423+548+872)

  #recall
  6732/(6372+548)

  #precision
  6732/(6372+872)




