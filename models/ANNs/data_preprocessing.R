#We set the seed to a specfic number to ensure the reproducibility of the results we get

set.seed(1)

#Let's use the function that we created to generate the dataset
data <- planar_dataset()

#ggplot(data, aes(x = X1, y = X2, color = factor(Y))) + geom_point()

#Let's split the dataset into training and testing data
## We take 80% of the dataset for training and leave the rest for testing
train_test_split_idx <- 0.8 * nrow(data)
training_set <- data[1:train_test_split_idx, ]
testing_set <- data[(train_test_split_idx+1) : nrow(data), ]

head(testing_set)


# Feature scaling to make sure that the algorithm converges

X_train <- scale(training_set[, c(1:2)]) # we combine the two feature columns into 1 matrix
y_train <- training_set$Y
dim(y_train) <- c(length(y_train), 1) # 1 x 320

X_test <- scale(testing_set[, c(1:2)]) 
y_test <- testing_set$Y
dim(y_test) <- c(length(y_test), 1) # 1 x 80


X_train <- as.matrix(X_train, byrow=TRUE)
X_train <- t(X_train)
y_train <- as.matrix(y_train, byrow=TRUE)
y_train <- t(y_train)

X_test <- as.matrix(X_test, byrow=TRUE)
X_test <- t(X_test)
y_test <- as.matrix(y_test, byrow=TRUE)
y_test <- t(y_test)




