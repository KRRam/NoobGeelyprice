

#Read file from specified directory
Geely <- read.csv(file.choose(), stringsAsFactors = F)

#Identify and install packages that are to be used
rqd_pkg <-
  c(
    "MASS",
    "car",
    "dplyr",
    "stringdist",
    "stringr",
    "forcats",
    "tidyr",
    "tidyverse",
    "lubridate",
    "ggplot2",
    "reshape2"
  )
pload <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pload(rqd_pkg)

#Understanding Data

dim(Geely) # shows that the data set has 205 rows and 26 columns
str(Geely) # shows the col names and the class of data in each column

x <-
  as.data.frame(sapply(Geely, class)) #Extracting class of variables for describing metdata
y <-
  as.data.frame(lengths(lapply(Geely, unique))) #Extracting no of unique values for describing metdata
GeelyMeta <- cbind(x, y[, 1])
colnames(GeelyMeta) <- c("Class", "Unique")
View(GeelyMeta)

#Loading data dictionary and preparing a Metadata Description
Data_dict <-
  readxl::read_excel(file.choose())#Imports data dictionary excel
View(Data_dict)
Data_dict <-
  Data_dict[, -c(1, 3:5, 7:8)] #Remove columns with NA values
Data_dict <- Data_dict[-c(1, 28, 29), ] #Remove rows with NA values
colnames(Data_dict) = c("Variable", "Description") #Give column names
Data_dict <-
  separate(
    data = Data_dict,
    col = Description,
    into = c("Desc"),
    sep = "\\("
  ) #Remove class descriptors in Data Dict
GeelyMeta$Desc <- Data_dict$Desc #Adds description to metadata
GeelyMeta$NAcount <-
  as.character(colSums(is.na(Geely))) #Shows that there are no  NA values
GeelyMeta$Blanks <-
  lapply(Geely, function(x)
    sum(nzchar(x, keepNA = FALSE) == 0))#Shows that there are no  missing values
View(GeelyMeta)

#Preparing five number summary

z <-
  as.data.frame(do.call(rbind, lapply(Geely, summary)))#Generates data summary for inclusion in metadata
colnames(z) <-
  c("Min", "Q1", "Med", "Mean", "Q3", "Max")#Renaming columns
z <-
  data.frame(apply(z, 2, function(x)
    as.numeric(as.character(x))), row.names = rownames(z))
GeelyMeta <- cbind(GeelyMeta, z[, 1:6])#Adds data summary to GeelyMeta
View(GeelyMeta)

#Identifying variables with outliers in original dataset

GeelyMeta$IQR <- GeelyMeta$Q3 - GeelyMeta$Q1
GeelyMeta$Ofenlo <- GeelyMeta$Q1 - 3 * (GeelyMeta$IQR)
GeelyMeta$Ofenhi <- GeelyMeta$Q3 + 3 * (GeelyMeta$IQR)
GeelyMeta$outlier <-
  abs((GeelyMeta$Min < GeelyMeta$Ofenlo) - (GeelyMeta$Max > GeelyMeta$Ofenhi))
Correct_outlier <-
  subset.data.frame(GeelyMeta, GeelyMeta$outlier == 1)
View(Correct_outlier)
rm(x, y, z, Data_dict)

# Goal of assignment model the price of cars with the available independent
# variables. It will be used by the management to understand how exactly the
# prices vary with the independent variables. They can accordingly manipulate
# the design of the cars, the business strategy etc. to meet certain price
# levels. Further, the model will be a good way for the management to understand
# the pricing dynamics of a new market.#

#CarName reduction to only Company name as required
Geelynew <- Geely %>% separate(CarName, into = "CoName", sep = " ")
Geelynew$CoName <- toupper(Geelynew$CoName)
unique(Geelynew$CoName)
Geelynew$CoName <-
  fct_collapse(
    Geelynew$CoName,
    ALFAROMEO = c("ALFA-ROMERO"),
    MAZDA = c("MAXDA", "MAZDA"),
    NISSAN = c("NISSAN", "NISSAN"),
    PORSCHE = c("PORCSHCE", "PORSCHE"),
    TOYOTA = c("TOYOTA", "TOYOUTA"),
    VOLKSWAGEN = c("VOKSWAGEN", "VOLKSWAGEN", "VW")
  )
View(Geelynew)
Geelynew <-
  Geelynew[, -1] #removing car_ID since it adds no meaningful data other than serial no
table(sapply(Geelynew, class))
# character    factor   integer   numeric
# 9         1         7         8

which((sapply(Geelynew, class)) == 'character') #shows charvar col names and index
# fueltype     aspiration     doornumber        carbody     drivewheel enginelocation     enginetype
# 3              4              5              6              7              8             14
# cylindernumber     fuelsystem
# 15             17

which((sapply(Geelynew, class)) == 'factor') #shows facvar col names and index

# CoName
# 2

which((sapply(Geelynew, class)) == 'integer') #shows intvar col names and index
# symboling curbweight enginesize horsepower    peakrpm    citympg highwaympg
# 1         13         16         21         22         23         24

#since symboling is actually a categorical variable with integer values as per the data dict, we change this to factor
Geelynew$symboling <- as.factor(Geelynew$symboling)
class(Geelynew$symboling)
# [1] "factor"

which((sapply(Geelynew, class)) == 'numeric') #shows numvar col names and index
# wheelbase        carlength         carwidth        carheight        boreratio           stroke
# 9               10               11               12               18               19
# compressionratio            price
# 20               25         25

# Univariate Analysis
p <- ggplot(Geelynew, aes(x = price))
p + geom_histogram(aes(fill = fueltype), color = "Red", position = "dodge")

geelynewplot <- gather(Geelynew, key = Category, value = Value)
ggplot(geelynewplot,  aes(y = Value, x = Category, col = Category)) + geom_jitter(position = position_jitter(height = 0, width =
                                                                                                               0.4))

# Bivariate Analysis

table(Geelynew$fuelsystem, Geelynew$fueltype)
#       diesel gas
# 1bbl      0  11
# 2bbl      0  66
# 4bbl      0   3
# idi      20   0
# mfi       0   1
# mpfi      0  94
# spdi      0   9
# spfi      0   1
#shows that all diesel vehicles are use idi fuelsystem
# suggesting that it can be combined without loss of information

Geelynew$fuel <- paste(Geelynew$fueltype, Geelynew$fuelsystem, sep = "-")
Geelynew <- Geelynew[, -c(3, 17)]

table(Geelynew$drivewheel, Geelynew$enginelocation)
#     front rear
# 4wd     9    0
# fwd   120    0
# rwd    73    3
#shows that only rwd drivewheel have rear enginelocation
# suggesting that it can be combined without loss of information

Geelynew$drivewheel <-
  paste(Geelynew$enginelocation, Geelynew$drivewheel, sep = "-")
Geelynew <- Geelynew[, -7]

Geelynew[sapply(Geelynew, is.character)] <-
  lapply(Geelynew[sapply(Geelynew, is.character)], as.factor) #takes a subset of Geelynew which is character and converts to factor
table(sapply(Geelynew, class))
#shows class of variables in Geelynew
# factor integer numeric
# 9       6       8


#extract numeric variables to a new DF to check corelations
Geelynewcor <- Geelynew[sapply(Geelynew, is.numeric)]
View(Geelynewcor)

cormat <- round(cor(Geelynewcor), 2)
hc <- hclust(as.dist(1 - cormat) / 2)
cormat.ord <- cormat[hc$order, hc$order]
cormat.ord[lower.tri(cormat.ord)] <- NA
require(reshape2)
melted_cormat <- melt(cormat.ord, na.rm = TRUE)
ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) + # Change gradient color
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    size = 12,
    hjust = 1
  )) +
  coord_fixed()

# Shows wheelbase, width and length corelate to curbweight
Geelymelt <- melt(Geelynewcor[, c(1:3, 5)], id = "curbweight")
ggplot(Geelymelt, aes(x = value, y = curbweight)) +
  facet_wrap( ~ variable, scales = "free") +
  geom_point() +
  geom_smooth(method = lm)

#Combining these columns will take away little from the data so we introduce curbweightratio = (curbweight/length+ curbweight/width+curbweight/wheelbase)
Geelynew$Cwtratio <-
  (
    Geelynew$curbweight / Geelynew$carlength + Geelynew$curbweight / Geelynew$wheelbase +
      Geelynew$curbweight / Geelynew$carwidth
  )
Geelynew <- Geelynew[, -c(7:9, 11)]

# Shows enginesize and horsepower have high correlation
Geelymelt <- melt(Geelynewcor[, c(6, 10)], id = "horsepower")
ggplot(Geelymelt, aes(x = value, y = horsepower)) +
  facet_wrap( ~ variable, scales = "free") +
  geom_point() +
  geom_smooth(method = lm)

#Combining these columns will take away little from the data so we introduce hpbysize = horsepower/enginesize
Geelynew$hpbysize <- (Geelynew$horsepower / Geelynew$enginesize)
Geelynew <- Geelynew[, -c(10, 14)]

#Corelation of price with stroke, compressionratio, peakrpm and carheight

Geelymelt <- melt(Geelynewcor[, c(4, 8, 9, 11, 14)], id = "price")
ggplot(Geelymelt, aes(x = value, y = price)) +
  facet_wrap( ~ variable, scales = "free") +
  geom_point() +
  geom_smooth(method = lm)

# convert factors with multiple levels to numerical variables

which((sapply(Geelynew, class)) == 'factor') #shows factorvar col names and index
# symboling     CoName     aspiration     doornumber        carbody
# 1              2              3              4              5
# drivewheel     enginetype cylindernumber    fuel
# 6              8              9             17

# Dummy-allfactors
dummy_all <-
  data.frame(
    model.matrix(
      ~ symboling + CoName + aspiration + doornumber + carbody + drivewheel +
        enginetype + cylindernumber + fuel,
      data = Geelynew
    )
  )
View(dummy_all)
dummy_all <- dummy_all[, -1]
Geelynewall <- cbind(Geelynew[, -c(1:6, 8:9, 17)], dummy_all)
table(sapply(Geelynewall, class)) #Reconfirming all variables are numeric
as.character(colSums(is.na(Geelynewall))) #Reconfirming no NA values

# Model Building

set.seed(100)
trainindices = sample(1:nrow(Geelynewall), 0.7 * nrow(Geelynewall))
train <- Geelynewall[trainindices, ]
test <- Geelynewall[-trainindices, ]


#M1
model_1 <- lm(train$price ~ ., data = train)
summary(model_1)

vif(model_1)

viferrorcause <- as.data.frame(alias(model_1)$Complete)
MCorrvars <- rownames(viferrorcause)
MCorrvars

#M2 removes MCorrvars

model_2 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi,
    data = train
  )
summary(model_2)

vif(model_2)

#M3
model_3 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi,
    data = train
  )
summary(model_3)

vif(model_3)

#M4
model_4 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1,
    data = train
  )
summary(model_4)
vif(model_4)

#M5
model_5 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg,
    data = train
  )
summary(model_5)
vif(model_5)

#M6
model_6 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE,
    data = train
  )
summary(model_6)
vif(model_6)

#M7
model_7 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan,
    data = train
  )
summary(model_7)
vif(model_7)

#M8
model_8 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd,
    data = train
  )
summary(model_8)
vif(model_8)

#M9
model_9 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc,
    data = train
  )
summary(model_9)
vif(model_9)

#M10
model_10 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi,
    data = train
  )
summary(model_10)
vif(model_10)

#M11
model_11 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi,
    data = train
  )
summary(model_11)
vif(model_11)

#M12
model_12 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi - fuelgas.2bbl,
    data = train
  )
summary(model_12)
vif(model_12)

#M13
model_13 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi - fuelgas.2bbl -
      compressionratio,
    data = train
  )
summary(model_13)
vif(model_13)

#M14
model_14 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi - fuelgas.2bbl -
      compressionratio - hpbysize,
    data = train
  )
summary(model_14)
vif(model_14)

#M15
model_15 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi - fuelgas.2bbl -
      compressionratio - hpbysize - peakrpm,
    data = train
  )
summary(model_15)
vif(model_15)

#M16
model_16 <-
  lm(
    price ~ . - enginetypedohcv - enginetypel - enginetypeohcf - cylindernumberthree -
      cylindernumbertwo - fuelgas.4bbl - cylindernumbertwelve - fuelgas.spfi -
      fuelgas.mpfi - symboling1 - citympg - highwaympg - CoNamePORSCHE - carbodysedan -
      drivewheelfront.rwd - enginetypeohc - CoNameAUDI - symboling.1 - symboling0 -
      carheight - stroke - symboling3 - CoNameHONDA - CoNameSAAB - CoNameVOLVO -
      doornumbertwo - carbodyhardtop - carbodyhatchback - enginetypeohcv - fuelgas.mfi -
      symboling2 - CoNameMERCURY - carbodywagon - fuelgas.spdi - fuelgas.2bbl -
      compressionratio - hpbysize - peakrpm - drivewheelfront.fwd,
    data = train
  )
summary(model_16)
vif(model_16)

#Price prediction
Predictprice <- predict(model_16, test[, -8])
test$predictedprice <- Predictprice

# Corelation bewteen predicted price and test price
r <- cor(test$price, test$predictedprice)
r
rsquared <- r ^ 2
rsquared
