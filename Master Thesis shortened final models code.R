install.packages("pacman")
pacman::p_load(tidyverse,dslabs, caret, broom ) 
library(mice)
library(corrplot)

##loading dataset and call it commerce_data
path <- file.path(getwd(), "E Commerce Dataset.xlsx") 
commerce_data <- readxl::read_xlsx(path, sheet = 2)
commerce_data$CustomerID <- 1:nrow(commerce_data)

summary(commerce_data)
str(commerce_data)

####################visual representation of nas
missing_matrix <- is.na(commerce_data[,2:20])
missing_matrix <- as.data.frame(missing_matrix)
missing_matrix <- cbind(commerce_data[,1], missing_matrix)

# Convert the logical matrix to a long-format data frame
missing_data <- pivot_longer(missing_matrix, cols = 2:20, names_to = "Variable",
                             values_to = "Missing", values_drop_na = FALSE)
ggplot(data = missing_data, aes(x = Variable, y = CustomerID, fill = Missing)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "red"), na.value = "white") +
  labs(x = "Variables", y = "Observations", title = "Missing Value Heatmap")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################### handle missing values and call the dataset commerce_data_complt
set.seed(2)
imp <- mice(commerce_data)
commerce_data_complt <- complete(imp)
sum(is.na(commerce_data_complt))

################## get histograms/boxplots and barplots of variables
##visualize numeric variables
pred2 <- commerce_data_complt %>% select(c(3,6,14, 17,16,18,19,20)) %>% names()
plots_num <- lapply(pred2, function(x){
  commerce_data_complt %>% ggplot(aes_string(x))+
    geom_histogram()
})
gridExtra::grid.arrange(grobs = plots_num, ncol=4)

##visualize categorical variables
pred3 <- commerce_data_complt %>% select(c(4,5,7,8,9,10,11,12,13)) %>% names()
plots_cat <- lapply(pred3, function(x){
  commerce_data_complt %>% ggplot(aes_string(x))+
    geom_bar()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})
gridExtra::grid.arrange(grobs = plots_cat, ncol=5)

pred4 <- commerce_data_complt %>% select(c(3,6,14,17,16,18,19,20)) %>% names()

plots_box <- lapply(pred4, function(x){
  commerce_data_complt %>% 
    ggplot(aes(!!rlang::sym(x))) +
    geom_boxplot() +
    coord_flip() 
})

gridExtra::grid.arrange(grobs = plots_box, ncol = 4)

###################split dataset into train and test
set.seed(1991)
test <- createDataPartition(commerce_data_complt$Churn, times = 1, p = .2, list = FALSE)
train_commerce <- commerce_data_complt[-test,]
test_commerce <- commerce_data_complt[test,]


################ correlation analysis
num_var <- commerce_data_complt %>% select(where(is.numeric ), -c(1,2))
corrplot(cor(num_var, method = "spearman"), method = "color")


##################### logistic regression model

##training set

##undersample dataset to balance it for better training and results
set.seed(11)
undersampled_data <- downSample(x = train_commerce[,-which(names(train_commerce) == "Churn")], 
                                y = factor(train_commerce$Churn), yname = "Churn")

train_commerce_tr <- undersampled_data %>% select(-c(1,17,18,19,20)) %>% 
  mutate(across(c(CityTier, SatisfactionScore, MaritalStatus, HourSpendOnApp, NumberOfDeviceRegistered, NumberOfAddress,
                  Complain), factor),across(where(is.numeric),function(x) ifelse(x==0, log(.5),log(x))), 
         Gender = factor(ifelse(Gender == "Female", 0, 1)),
         PreferredLoginDevice = factor(ifelse(PreferredLoginDevice == "Computer", 0, 1)),
         PreferredPaymentMode = factor(case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode)),
         PreferedOrderCat = factor(ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                          PreferedOrderCat)))

Churn <- undersampled_data %>% .$Churn 
train_commerce_tr <- cbind(train_commerce_tr, Churn)


##train model
set.seed(1)
model_log <- glm(as.factor(Churn)~., data = train_commerce_tr, family = "binomial")

##testing set
test_commerce_tr <- test_commerce %>% select(-c(1,2,18,19,20)) %>% 
  mutate(across(c(CityTier, SatisfactionScore, MaritalStatus, HourSpendOnApp, NumberOfDeviceRegistered, NumberOfAddress,
                  Complain), factor),across(where(is.numeric),function(x) ifelse(x==0, log(.5),log(x))), 
         Gender = factor(ifelse(Gender == "Female", 0, 1)),
         PreferredLoginDevice = factor(ifelse(PreferredLoginDevice == "Computer", 0, 1)),
         PreferredPaymentMode = factor(case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode)),
         PreferedOrderCat = factor(ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                          PreferedOrderCat)))

Churn2 <- test_commerce %>% .$Churn
test_commerce_tr <- cbind(test_commerce_tr, Churn2)

##prediction
fit_log <- predict(model_log, newdata = test_commerce_tr, type = "response")

y_h <- ifelse(fit_log > 0.5, 1, 0) 
confusionMatrix(as.factor(y_h), as.factor(test_commerce_tr$Churn2))

######################## knn model fit

train_commerce_sc <- train_commerce %>% select(-c(1,2)) %>% 
  mutate(across(c(4,12,15,18), scale), 
         Gender = ifelse(Gender == "Female", 0, 1),
         PreferredLoginDevice = ifelse(PreferredLoginDevice == "Computer", 0, 1),
         PreferredPaymentMode = case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode),
         PreferedOrderCat = ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                   PreferedOrderCat))


##hot encode categorical variables
encoded_cat1 <- model.matrix(~PreferredPaymentMode-1, train_commerce_sc)
encoded_cat2 <- model.matrix(~PreferedOrderCat -1, train_commerce_sc)
encoded_cat3 <- model.matrix(~MaritalStatus -1, train_commerce_sc)
encoded_cat <- cbind(encoded_cat1, encoded_cat2, encoded_cat3)


rm(encoded_cat1, encoded_cat2, encoded_cat3)

train_commerce_sc <- train_commerce_sc %>% select(-c(PreferredPaymentMode, PreferedOrderCat, MaritalStatus))
train_commerce_sc <- cbind(train_commerce_sc, encoded_cat)
train_commerce_sc <- train_commerce_sc %>% mutate(Churn = train_commerce$Churn)

##train knn model
set.seed(2)
model_knn <- train(as.factor(Churn) ~., data = train_commerce_sc, method = "knn", tuneGrid = data.frame(k =seq(1,5,1)))


##test set preprocessing scale

test_commerce_sc <- test_commerce %>% select(-c(1,2)) %>% 
  mutate(across(c(4,12,15,18), scale), 
         Gender = ifelse(Gender == "Female", 0, 1),
         PreferredLoginDevice = ifelse(PreferredLoginDevice == "Computer", 0, 1),
         PreferredPaymentMode = case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode),
         PreferedOrderCat = ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                   PreferedOrderCat))


##one hot encoding categorical variables
encoded_cat1 <- model.matrix(~PreferredPaymentMode-1, test_commerce_sc)
encoded_cat2 <- model.matrix(~PreferedOrderCat -1, test_commerce_sc)
encoded_cat3 <- model.matrix(~MaritalStatus -1, test_commerce_sc)
encoded_cat <- cbind(encoded_cat1, encoded_cat2, encoded_cat3)


rm(encoded_cat1, encoded_cat2, encoded_cat3)

test_commerce_sc <- test_commerce_sc %>% select(-c(PreferredPaymentMode, PreferedOrderCat, MaritalStatus))
test_commerce_sc <- cbind(test_commerce_sc, encoded_cat)
test_commerce_sc <- test_commerce_sc %>% mutate(Churn = test_commerce$Churn)

##predictions 
fit_knn <- predict(model_knn, newdata = test_commerce_sc)
confusionMatrix(fit_knn, as.factor(test_commerce_sc$Churn))

################## Random Forest

##undersample dataset to balance it for more accurate training and predictions
set.seed(44)
undersampled_data1 <- downSample(x = train_commerce[,-which(names(train_commerce) == "Churn")], 
                                 y = factor(train_commerce$Churn), yname = "Churn")
##training set 
rm(train_commerce_sc, test_commerce_sc)
train_commerce_sc <- undersampled_data1 %>% select(-c(1,17,19,20)) %>% 
  mutate(across(c(4,12,15), function(x) ifelse(x==0, log(.5),log(x))), 
         Gender = ifelse(Gender == "Female", 0, 1),
         PreferredLoginDevice = ifelse(PreferredLoginDevice == "Computer", 0, 1),
         PreferredPaymentMode = case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode),
         PreferedOrderCat = ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                   PreferedOrderCat))


##one hot encoding categorical variables
encoded_cat1 <- model.matrix(~PreferredPaymentMode-1, train_commerce_sc)
encoded_cat2 <- model.matrix(~PreferedOrderCat -1, train_commerce_sc)
encoded_cat3 <- model.matrix(~MaritalStatus -1, train_commerce_sc)
encoded_cat <- cbind(encoded_cat1, encoded_cat2, encoded_cat3)


rm(encoded_cat1, encoded_cat2, encoded_cat3)

train_commerce_sc <- train_commerce_sc %>% select(-c(PreferredPaymentMode, PreferedOrderCat, MaritalStatus))
train_commerce_sc <- cbind(train_commerce_sc, encoded_cat)
train_commerce_sc <- train_commerce_sc %>% mutate(Churn = undersampled_data1$Churn)

##test set preprocessing scale

test_commerce_sc <- test_commerce %>% select(-c(1,2,18,20)) %>% 
  mutate(across(c(4,12,15), function(x) ifelse(x==0, log(.5),log(x))), 
         Gender = ifelse(Gender == "Female", 0, 1),
         PreferredLoginDevice = ifelse(PreferredLoginDevice == "Computer", 0, 1),
         PreferredPaymentMode = case_when(
           PreferredPaymentMode == "CC" ~ "Credit Card",
           PreferredPaymentMode == "COD" ~ "Cash on Delivery",
           TRUE ~ PreferredPaymentMode),
         PreferedOrderCat = ifelse(PreferedOrderCat == "Mobile", "Mobile Phone",
                                   PreferedOrderCat))


##one hot encoding catgorical variables
encoded_cat1 <- model.matrix(~PreferredPaymentMode-1, test_commerce_sc)
encoded_cat2 <- model.matrix(~PreferedOrderCat -1, test_commerce_sc)
encoded_cat3 <- model.matrix(~MaritalStatus -1, test_commerce_sc)
encoded_cat <- cbind(encoded_cat1, encoded_cat2, encoded_cat3)


rm(encoded_cat1, encoded_cat2, encoded_cat3)

test_commerce_sc <- test_commerce_sc %>% select(-c(PreferredPaymentMode, PreferedOrderCat, MaritalStatus))
test_commerce_sc <- cbind(test_commerce_sc, encoded_cat)
test_commerce_sc <- test_commerce_sc %>% mutate(Churn = test_commerce$Churn)

##train model
set.seed(33)
model_rf <- train(as.factor(Churn)~., data = train_commerce_sc, method = "rf")

##predictions
fit_rf <- predict(model_rf, newdata = test_commerce_sc)
confusionMatrix(fit_rf, as.factor(test_commerce_sc$Churn))
