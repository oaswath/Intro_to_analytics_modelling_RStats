library(kknn)
library(kernlab)

#Q3.1 (a)
data <- read.table("/~/credit_card_data-headers.txt",
                   stringsAsFactors = FALSE, header = TRUE)

set.seed(123)

#convert response variable to factors for classification
data$R1 <- as.factor(data$R1)

#use train.kknn function to perform leave one out cross validation to select 
model_cv_1 <- train.kknn(R1~.,data, kmax=20, kernel = "optimal", scale=TRUE)

1-model_cv_1$MISCLASS

#Results:
# > model_cv_1
# 
# Call:
#   train.kknn(formula = R1 ~ ., data = data, kmax = 20, kernel = "optimal",     scale = TRUE)
# 
# Type of response variable: nominal
# Minimal misclassification: 0.146789
# Best kernel: optimal
# Best k: 12

#use cv.kknn to perform k_fold cross validations 
set.seed(123)
acc <- rep(0,20)


for(i in 1:20){
model_cv_2 <- cv.kknn(R1~.,data, kcv=10, k=i, scale=TRUE) 

acc[i] <- mean(model_cv_2[[1]][,1] == model_cv_2[[1]][,2])

}

which.max(acc)


#Q3.1 (b)
set.seed(123)

n = nrow(data)
idx <- sample(n)

train_idx <- idx[1:floor(0.6 * n)]
val_idx   <- idx[(floor(0.6 * n) + 1):floor(0.8 * n)]
test_idx  <- idx[(floor(0.8 * n) + 1):n]

train <- data[train_idx, ]
val   <- data[val_idx, ]
test  <- data[test_idx, ]

max_k <- 20
val_acc  <- rep(0,20)

for (i in 1:max_k){
  
  model <- kknn(R1 ~ ., train = train, test = val, k = i, scale = TRUE)
  
 
  val_acc[i] <- mean(model$fitted.values == val[, "R1"]) 
}

which.max(val_acc)


model_final <- kknn(R1 ~ ., train = train, test = test, k =which.max(val_acc) , scale = TRUE)


test_acc <- mean(model_final$fitted.values == test[, "R1"])

val_acc[which.max(val_acc)]
test_acc

#Q4.2

library(dplyr)

set.seed(123)

iris_data <- read.table("/~/iris.txt",
                   stringsAsFactors = FALSE, header = TRUE)

iris_scaled_data <- scale(iris_data[,1:4])

within_cluster_var <- rep(0,10)

for (i in 1:10) {
  
  cluster <- kmeans(iris_scaled_data, centers = i, nstart = 25)
  within_cluster_var[i] <- cluster$tot.withinss
  
}

within_cluster_var

plot(1:10, within_cluster_var, type = "b",
     xlab = "Number of clusters",
     ylab = "Total within-cluster variance")


iris_data %>%
  group_by(Species) %>%
  summarise(across(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                   ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

# Species           mean_Sepal.Length mean_Sepal.Width mean_Petal.Length mean_Petal.Width
# <chr>                     <dbl>            <dbl>             <dbl>            <dbl>
# 1 setosa                  5.01             3.43              1.46            0.246
# 2 versicolor              5.94             2.77              4.26            1.33 
# 3 virginica               6.59             2.97              5.55            2.03 



# model with all predictors

cluster_all_predictors <- kmeans(iris_scaled_data, centers = 3, nstart = 25)

# model with just Petal.Length and Petal.Width

cluster_petal_only <- kmeans(iris_scaled_data[, c(3,4)], centers = 3, nstart = 25)


#compare cluster results against flower types

petal_only %>%
  count(Species, cluster_petal_only$cluster) %>%
  group_by(Species) %>%
  mutate(prop = n / sum(n))

petal_only %>%
  count(Species, cluster_all_predictors$cluster) %>%
  group_by(Species) %>%
  mutate(prop = n / sum(n))

