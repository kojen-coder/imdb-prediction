
movie <- read.csv("/Users/apple/Desktop/Midterm/IMDB_data_Fall_2023.csv")
movie_final=movie
##############
#Country
#############
# Select countries to keep
countries_to_keep = c("USA", "UK", "Germany", "Australia", "Canada", "France")
# Convert 'country' to character
movie_final$country = as.character(movie_final$country)
# Modify the 'country' column
movie_final$country = ifelse(movie_final$country %in% countries_to_keep, movie_final$country, "Other")
#Check
table(movie_final$country)
#Change to a dummy variable for later use 
movie_final$country = as.factor(movie_final$country)

##############
#Distributor
#############
distributor_table = table(movie_final$distributor)
distributor_table
distributor_frequency_table=table(distributor_table)
distributor_frequency_table #threshond=36
movie_final$distributor = as.character(movie_final$distributor)
# Find distributors with a frequency less than 35
distributors_to_change = names(distributor_table[distributor_table < 35])
# Replace the names of these distributors with 'Other' in the original dataset
movie_final$distributor[movie_final$distributor %in% distributors_to_change] = 'Other'
#Change to a dummy varable for later use 
movie_final$distributor = as.factor(movie_final$distributor)

##############
#Director
#############
director_table = table(movie_final$director)
director_table
# Calculate the frequency of frequencies
director_freq_table = table(director_table)
director_freq_table #thershod= 7 
movie_final$director = as.character(movie_final$director)
# Identify directors with a frequency less than 7
directors_to_change = names(director_table[director_table < 7])
# Replace the names of these directors with 'Other' in the original dataset
movie_final$director[movie_final$director %in% directors_to_change] = 'Other'
#Check
new_director_table = table(movie_final$director)
new_director_table
# convert it back to a dummy
movie_final$director = as.factor(movie_final$director)

################
#Actor 1 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a1 = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a1 = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
#The cut() function in R automatically converts the resulting bins into a factor.
#Factors are R's data structure for categorical data
movie_final$actor1_ranking = cut(movie_final$actor1_star_meter, 
                                 breaks = breaks_a1, 
                                 labels = labels_a1, 
                                 include.lowest = TRUE, 
                                 right = FALSE)
movie_final$actor1_ranking = as.factor(movie_final$actor1_ranking)
#can drop and actor1 and actor1_star_meter later

################
#Actor 2 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a2 = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a2 = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final$actor2_ranking = cut(movie_final$actor2_star_meter, 
                                 breaks = breaks_a2, 
                                 labels = labels_a2, 
                                 include.lowest = TRUE, 
                                 right = FALSE)
movie_final$actor2_ranking = as.factor(movie_final$actor2_ranking)
#can drop and actor2 and actor2_star_meter later

################
#Actor 3 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a3 = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a3 = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final$actor3_ranking = cut(movie_final$actor3_star_meter, 
                                 breaks = breaks_a3, 
                                 labels = labels_a3, 
                                 include.lowest = TRUE, 
                                 right = FALSE)
movie_final$actor3_ranking = as.factor(movie_final$actor3_ranking)
#can drop and actor3 and actor3_star_meter later

################
#IMDBpro ranking
###############
#To categorize actors based on their IMDBpro ranking 
#into bins like "Top 100," "Top 101-200," ..
# Define the breaks for the bins
breaks_IMDB = c(-Inf,100, 200, 300, 400, 500, 600,700, 800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000, Inf)
# Define the labels for the bins
labels_IMDB = c("Top 1-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Top 501-600", "Top 601-700", "Top 701-800", "Top 801-900", "Top 901-1000", "Top 1001-1100", "Top 1101-1200", "Top 1201-1300", "Top 1301-1400", "Top 1401-1500", "Top 1501-1600", "Top 1601-1700", "Top 1701-1800", "Top 1801-1900", "Top 1901-2000", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final$IMDB_ranking = cut(movie_final$movie_meter_IMDBpro, 
                               breaks = breaks_IMDB, 
                               labels = labels_IMDB, 
                               include.lowest = TRUE, 
                               right = FALSE)
movie_final$IMDB_ranking = as.factor(movie_final$IMDB_ranking)
#can drop movie_meter_IMDBpro later

################
#Production company
###############
production_company_table = table(movie_final$production_company)
production_company_table
production_company_freq_table=table(production_company_table)
production_company_freq_table #threshod=20
# Convert 'production_company' to character 
movie_final$production_company = as.character(movie_final$production_company)
# Identify production companies with a frequency less than 30
companies_to_change = names(production_company_table[production_company_table < 30])
# Replace the names of these companies with 'Other' in the original dataset
movie_final$production_company[movie_final$production_company %in% companies_to_change] = 'Other'
# Convert 'production_company' back to a factor for later use in analysis and modeling
movie_final$production_company = as.factor(movie_final$production_company)

################
#Irrelevent Predictors
###############
movie_final$movie_title <- NULL
movie_final$movie_id <- NULL
movie_final$movie_link <- NULL
movie_final$imdb_link = NULL
# movie_final$release_day <- NULL
table(movie_final$language)
movie_final$language <- NULL #English dominance 1892
# movie_final$aspect_ratio <- NULL
movie_final$cinematographer<-NULL # maybe helpful but can't use in the model, can't identify important cinematographer as i did for directors
movie_final$plot_keywords <-NULL # helpful but can't use in the model
movie_final$genres  <-NULL # already included in the later columns
movie_final$actor1<-NULL
movie_final$actor1_star_meter<-NULL
movie_final$actor2<-NULL
movie_final$actor2_star_meter<-NULL
movie_final$actor3<-NULL
movie_final$actor3_star_meter<-NULL
movie_final$movie_meter_IMDBpro <-NULL

attach(movie_final)

dummy=names(movie_final)[sapply(movie_final, is.factor) | sapply(movie_final, is.character)]
dummy
continuous_vars = names(movie_final)[sapply(movie_final, is.numeric)]
continuous_vars

genre_dummies = c("action", "adventure", "scifi", "thriller", "musical", "romance", "western", 
                   "sport", "horror", "drama", "war", "animation", "crime")

# Identify continuous variables (numeric but not in the genre_dummies list)
continuous_vars = names(movie_final)[sapply(movie_final, is.numeric) & !names(movie_final) %in% genre_dummies]
continuous_vars
library(ggplot2)
# Plotting histograms for continuous variables
for (var in continuous_vars) {
  p = ggplot(movie_final, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") + 
    theme_minimal() +
    labs(title = paste("Distribution of", var), x = var, y = "Frequency")
  
  print(p)
}
# Plotting bar charts for categorical variables (genre_dummies are not included)
for (var in dummy) {
  p = ggplot(movie_final, aes_string(x = var)) +
    geom_bar(fill = "blue", color = "black") + 
    theme_minimal() +
    labs(title = paste("Distribution of", var), x = var, y = "Count")
  
  print(p)
}
#Examine the correlation coefficient
#  'imdb_score' is target variable Y
Y = movie_final$imdb_score

# Calculate correlation coefficients between "Y" and each continuous predictor
correlations = sapply(movie_final[continuous_vars], function(x) cor(x, Y, use = "complete.obs"))
correlations # weird, more budget leads to a lower score

# remove imbd_score from continuous_vars
continuous_vars=continuous_vars[-1]
require(psych)
# Detecting collinear numerical variables through correlation matrix
quantvars = movie_final[, continuous_vars]
corr_matrix = cor(quantvars)
round(corr_matrix, 2)
# correlations between continuous predictors are low, almost no collinearity

# Plotting scatter plots for each continuous predictor against Y
for (var in continuous_vars) {
  p <- ggplot(movie_final, aes_string(x = var, y = "imdb_score")) +
    geom_point(alpha = 0.5) +  # Adjust alpha for better visualization if points are overlapping
    geom_smooth(method = "lm", color = "blue") +  # Adds a linear regression fit line
    theme_minimal() +
    labs(title = paste("Scatter plot of", var, "vs IMDb Score"), x = var, y = "IMDb Score")
  
  print(p)
}
# based on the scatter plot, aspect_ratio should be treated as a categorical variable or just drop it.
table(aspect_ratio)

require(car)
########################Predictor: aspect_ratio
lm0=lm(imdb_score~aspect_ratio)
summary(lm0)
residualPlots(lm0, main="Residual Plots of imdb_score~aspect_ratio model")
# aspect_ratio is not significantly linear
##### Try different degrees of predictor
lm_ratio2 = lm(imdb_score~poly(aspect_ratio,2))
lm_ratio3 = lm(imdb_score~poly(aspect_ratio,3))
lm_ratio4 = lm(imdb_score~poly(aspect_ratio,4))
anova(lm0, lm_ratio2, lm_ratio3, lm_ratio4)
# aspect_ratio of degree 2 is better than degree 1 with prob 99.58%
summary(lm_ratio2)
# R-squared:  0.004373, still low

##### Try splines
library(splines)
library(boot)
a1=quantile(aspect_ratio, 0.25)
a2=quantile(aspect_ratio, 0.5)
a3=quantile(aspect_ratio, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(aspect_ratio, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree for spline: 1, spline didn't improve the fit of aspect_ratio, remove aspect_ratio

########################Predictor: movie_budget
lm1=lm(imdb_score~movie_budget)
summary(lm1)
# R^2: 0.006189
##### Try different degrees of predictor
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~poly(movie_budget,i), data = movie_final)
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 1
# mse: 1.204

##### Try splines
a1=quantile(movie_budget, 0.25)
a2=quantile(movie_budget, 0.5)
a3=quantile(movie_budget, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(movie_budget, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree:4
# mse: 1.205782
# no need for splines

########################Predictor: release_day
lm2=lm(imdb_score~release_day)
summary(lm2)
residualPlots(lm2, main="Residual Plots of imdb_score~release_day model")
# release day is not significantly linear
##### Try different degrees of predictor
lm_day2 = lm(imdb_score~poly(release_day,2))
lm_day3 = lm(imdb_score~poly(release_day,3))
lm_day4 = lm(imdb_score~poly(release_day,4))
anova(lm2, lm_day2, lm_day3, lm_day4)
# Higher degrees didn't improve the model significantly

##### Try splines
a1=quantile(release_day, 0.25)
a2=quantile(release_day, 0.5)
a3=quantile(release_day, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(release_day, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 2
# mse: 1.212146
lm2_s2 = lm(imdb_score~bs(release_day, knots=c(a1, a2, a3), degree=2))
summary(lm2_s2)
#R-squared:  0.00554, still low, remove release_day

########################Predictor: release_year
lm3=lm(imdb_score~release_year)
summary(lm3)
# R^2: 0.03796
##### Try different degrees of predictor
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~poly(release_year,i), data = movie_final)
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 4
# mse: 1.153663

##### Try splines
a1=quantile(release_year, 0.25)
a2=quantile(release_year, 0.5)
a3=quantile(release_year, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(release_year, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 3
# mse: 1.156078
# no need for splines

########################Predictor: duration
lm4=lm(imdb_score~duration)
summary(lm4)
# R^2: 0.1686
##### Try different degrees of predictor
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~poly(duration,i), data = movie_final)
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree:2
# mse: 0.9842

##### Try splines
a1=quantile(duration, 0.25)
a2=quantile(duration, 0.5)
a3=quantile(duration, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 2
# mse: 0.9725655
# splines have smaller mse, we can try splines of degree 2 for duration

########################Predictor: nb_news_articles
lm5=lm(imdb_score~nb_news_articles)
summary(lm5)
# R^2: 0.05083
##### Try different degrees of predictor
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~poly(nb_news_articles,i), data = movie_final)
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree:1
# mse: 1.2334

##### Try splines
a1=quantile(nb_news_articles, 0.25)
a2=quantile(nb_news_articles, 0.5)
a3=quantile(nb_news_articles, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(nb_news_articles, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 1
# mse: 1.072309
# splines have smaller mse, we can try splines of degree 1 for nb_news_articles

########################Predictor: nb_faces
lm6=lm(imdb_score~nb_faces)
summary(lm6)
# R^2: 0.007992
##### Try different degrees of predictor
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~poly(nb_faces,i), data = movie_final)
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree:2
# mse: 1.200

##### Try splines
a1=quantile(nb_faces, 0.25)
a2=quantile(nb_faces, 0.5)
a3=quantile(nb_faces, 0.75)
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score~bs(nb_faces, knots=c(a1, a2, a3), degree=i))
  mse[i] = cv.glm(movie_final, fit, K=5)$delta[1]
}
round(mse,3)
which.min(mse)
min(mse)
# best degree: 2
# mse: 1.201746
# no need for splines

##########################Conclusion###########################################
## Performance: duration>nb_news_articles>release_year>nb_faces>movie_budget, remove aspect_ratio, release_day
## Polynomial best degree:
# duration: 2
# nb_news_articles: 1
# release_year: 4
# nb_faces:2
# movie_budget:1
## Splines:
# no need for movie_budget, release_year, nb_faces
# we can try splines of degree 2 for duration, degree 1 for nb_news_articles

##################Try different combinations of continuous predictors
reg1=lm(imdb_score~duration+nb_news_articles)
summary(reg1)$adj.r.squared
# 0.2034407

reg2=lm(imdb_score~poly(duration,2)+nb_news_articles)
summary(reg2)$adj.r.squared
# 0.2187956

a1=quantile(duration, 0.25)
a2=quantile(duration, 0.5)
a3=quantile(duration, 0.75)
b1=quantile(nb_news_articles, 0.25)
b2=quantile(nb_news_articles, 0.5)
b3=quantile(nb_news_articles, 0.75)
reg3 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+bs(nb_news_articles, knots=c(b1, b2, b3), degree=1))
summary(reg3)$adj.r.squared
# 0.27393

reg4 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year)
summary(reg4)$adj.r.squared
# 0.3034084

reg5 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            poly(release_year,4))
summary(reg5)$adj.r.squared
# 0.3030238, poly(release_year,4) didn't improve the model, continue to use release_year

reg6 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year+nb_faces)
summary(reg6)$adj.r.squared
# 0.3072697
# nb_faces not so useful, maybe we can drop later

reg7 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year+poly(nb_faces,2))
summary(reg7)$adj.r.squared
# 0.3069114, poly(nb_faces,2) didn't improve the model, continue to use nb_faces

reg8 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year+nb_faces+movie_budget)
summary(reg8)$adj.r.squared
# 0.3349063

##################Add dummies
dummy
reg9 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year+nb_faces+movie_budget+
            release_month)
summary(reg9)$adj.r.squared
# 0.3393516

reg10 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
            bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
            release_year+nb_faces+movie_budget+
            release_month+country)
summary(reg10)$adj.r.squared
# 0.347398

reg11 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating)
summary(reg11)$adj.r.squared
# 0.3555714

reg12 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor)
summary(reg12)$adj.r.squared
# 0.3588983

reg13 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director)
summary(reg13)$adj.r.squared
# 0.3623009

reg14 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film)
summary(reg14)$adj.r.squared
# 0.3683884

reg15 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+production_company)
summary(reg15)$adj.r.squared
# 0.3679403, smaller adjusted r-squared
anova(reg14,reg15)
# p = 0.5717
# don't add production_company

reg16 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+actor1_ranking)
summary(reg16)$adj.r.squared
# 0.3709101

reg17 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+actor1_ranking+actor2_ranking)
summary(reg17)$adj.r.squared
# 0.3712469

reg18 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+
             actor1_ranking+actor2_ranking+actor3_ranking)
summary(reg18)$adj.r.squared
# 0.3736171

reg19 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+
             actor1_ranking+actor2_ranking+actor3_ranking+IMDB_ranking)
summary(reg19)$adj.r.squared
# 0.3915878
# save for test
# Create an empty list to store the regression models to test
reg_test = list()
reg_test[[1]] = reg19
## Useful dummy variables: IMDB_ranking, country, maturity_rating

reg20 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             IMDB_ranking+country+maturity_rating)
summary(reg20)$adj.r.squared
# 0.3713092
# save for test
reg_test[[2]] = reg20

# Use selected dummies and remove nb_faces
reg21 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+movie_budget+
             IMDB_ranking+country+maturity_rating)
summary(reg21)$adj.r.squared
# 0.3677676
# save for test
reg_test[[3]] = reg21

##################Add genres
# This is the model with most predictors
reg22 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+nb_faces+movie_budget+
             release_month+country+maturity_rating+distributor+director+colour_film+
             actor1_ranking+actor2_ranking+actor3_ranking+IMDB_ranking+
             action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg22)$adj.r.squared
# 0.4575101
# save for test
reg_test[[4]] = reg22

# Use only important predictors
reg23 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+movie_budget+
             IMDB_ranking+country+maturity_rating+ 
             action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg23)$adj.r.squared
# 0.4467829
# save for test
reg_test[[5]] = reg23

########### if we don't use splines
reg24 = lm(imdb_score~poly(duration,2)+nb_news_articles+release_year+movie_budget+
             IMDB_ranking+country+maturity_rating)
summary(reg24)$adj.r.squared
# 0.3157683


reg25 = lm(imdb_score~poly(duration,2)+nb_news_articles+release_year+movie_budget+
             IMDB_ranking+country+maturity_rating+
             action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg25)$adj.r.squared
# 0.3920647
# save for test
reg_test[[6]] = reg25

########## if we don't use polynomials
reg26 = lm(imdb_score~duration+nb_news_articles+release_year+movie_budget+
             IMDB_ranking+country+maturity_rating)
summary(reg26)$adj.r.squared
# 0.2988862

reg27 = lm(imdb_score~duration+nb_news_articles+release_year+movie_budget+
             IMDB_ranking+country+maturity_rating+
             action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg27)$adj.r.squared
# 0.3832939
# save for test
reg_test[[7]] = reg27

####################Validation Set Test
require(caTools)
mse = rep(NA, 7)
mse_iter = rep(NA, 10)
for (i in 1:7){
  for (j in 1:10){
    sample = sample.split(movie_final$imdb_score, SplitRatio = 0.7)
    train_set = subset(movie_final, sample == TRUE)
    test_set = subset(movie_final, sample == FALSE)
    actual = test_set$imdb_score
    prediction = predict(reg_test[[i]], test_set)
    mse_iter[j] = (actual-prediction)^2
  }
  mse[i] = mean(mse_iter)
}
round(mse,3)
which.min(mse)
min(mse)
# reg_test[[5]] = reg23
# mse = 0.05342894

########## Test collinearity
vif(reg23)
# no collinearity

########## Test Heteroskedasticity 
ncvTest(reg23) ##p-value<0.05, there is heteroskedasticity 

## correct heteroskedastic errors
#install.packages("lmtest")
#install.packages("plm")
require(lmtest)
require(plm)
coeftest(reg23, vcov=vcovHC(reg23, type="HC1"))


######### Test outliers
outlierTest(reg23)
# remove 395, 1581, 1806, 316, 1123, 989, 191, 1255
movie_final2=movie_final[-c(395, 1581, 1806, 316, 1123, 989, 191, 1255),]
reg28 = lm(imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
             bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
             release_year+movie_budget+
             IMDB_ranking+country+maturity_rating+
             action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime,
           data=movie_final2)
summary(reg28)$adj.r.squared
# 0.4624311
# adj.r.squared becomes greater after removing outliers

# Calculate mse
mse = rep(NA, 10)
for (i in 1:10){
  sample = sample.split(movie_final2$imdb_score, SplitRatio = 0.7)
  train_set = subset(movie_final2, sample == TRUE)
  test_set = subset(movie_final2, sample == FALSE)
  actual = test_set$imdb_score
  prediction = predict(reg28, test_set)
  mse[i] = (actual-prediction)^2
}
mean(mse)
# mse = 0.128175
# mse becomes greater after removing outliers
summary(reg23)
###########################Conclusion#########################
# Best model is reg23 (without removing outliers) with R-squared: 0.4643,	Adjusted R-squared: 0.4468, MSE: 0.05342894
# imdb_score~bs(duration, knots=c(a1, a2, a3), degree=2)+
# bs(nb_news_articles, knots=c(b1, b2, b3), degree=1)+
# release_year+movie_budget+
# IMDB_ranking+country+maturity_rating+
# action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime

######################################################################
########################Predict Test Dataset##########################
######################################################################
movie_test=read.csv("/Users/apple/Desktop/test_data_IMDB_Fall_2023.csv")
movie_final_test=movie_test 


##############
#Country
#############
country_table_test = table(movie_final_test$country)
country_table_test
#Change  United States to USA, United Kingdom to UK

# Convert 'country' to character
movie_final_test$country = as.character(movie_final_test$country)
# Modify the 'country' column
movie_final_test$country[movie_final_test$country == "United States"] = "USA"
movie_final_test$country[movie_final_test$country == "United Kingdom"] = "UK"
#Check
table(movie_final_test$country)
#Change to a dummy variable for later use 
movie_final_test$country = as.factor(movie_final_test$country)

##############
#Distributor
#############

## from movie_final,check the names of Distributor in the training set
training_distributor=table(movie_final$distributor) 
training_distributor
#names of Distributor in the test set
unique(movie_final_test$distributor)
movie_final_test$distributor = as.character(movie_final_test$distributor)
# Modify the 'distributor' column
movie_final_test$distributor[movie_final_test$distributor == "Lions Gate Films"] = "Lionsgate"
movie_final_test$distributor[movie_final_test$distributor == "Columbia Pictures"] = "Columbia Pictures Corporation"
# Rename distributors to 'Other' except for the specified ones
movie_final_test$distributor = ifelse(movie_final_test$distributor %in% c("Lionsgate", 
                                                                          "Columbia Pictures Corporation", 
                                                                          "Universal Pictures"), 
                                      movie_final_test$distributor, 
                                      "Other")

# Convert 'distributor' back to a factor 
movie_final_test$distributor = as.factor(movie_final_test$distributor)


##############
#Director
#############
#names of Director in the test set
director_table_test = table(movie_final_test$director)
director_table_test
#names of Director in the training set
director_in_training=table(movie_final$director) 
director_in_training

movie_final_test$director = as.character(movie_final_test$director)

# Replace the names of these directors with 'Other' if they are not present in the training set
movie_final_test$director[! movie_final_test$director %in% director_in_training] = 'Other'
#Check the test set
table(movie_final_test$director)
# convert it back to a dummy
movie_final_test$director = as.factor(movie_final_test$director)

################
#Actor 1 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a1_test = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a1_test = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
#The cut() function in R automatically converts the resulting bins into a factor.
#Factors are R's data structure for categorical data
movie_final_test$actor1_ranking = cut(movie_final_test$actor1_star_meter, 
                                           breaks = breaks_a1_test, 
                                           labels = labels_a1_test, 
                                           include.lowest = TRUE, 
                                           right = FALSE)
movie_final_test$actor1_ranking = as.factor(movie_final_test$actor1_ranking)
#can drop and actor1 and actor1_star_meter later

################
#Actor 2 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a2_test = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a2_test = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final_test$actor2_ranking = cut(movie_final_test$actor2_star_meter, 
                                           breaks = breaks_a2_test, 
                                           labels = labels_a2_test, 
                                           include.lowest = TRUE, 
                                           right = FALSE)
movie_final_test$actor2_ranking = as.factor(movie_final_test$actor2_ranking)
#can drop and actor2 and actor2_star_meter later

################
#Actor 3 ranking
###############
#To categorize actors based on their star meter rankings 
#into bins like "Top 50," "Top 51-100," ..
# Define the breaks for the bins
breaks_a3_test = c(-Inf, 50, 100, 200, 300, 400, 500, Inf)
# Define the labels for the bins
labels_a3_test = c("Top 1-50", "Top 51-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final_test$actor3_ranking = cut(movie_final_test$actor3_star_meter, 
                                           breaks = breaks_a3_test, 
                                           labels = labels_a3_test, 
                                           include.lowest = TRUE, 
                                           right = FALSE)
movie_final_test$actor3_ranking = as.factor(movie_final_test$actor3_ranking)
#can drop and actor3 and actor3_star_meter later

################
#IMDBpro ranking
###############
#To categorize movies based on their IMDBpro ranking 
#into bins like "Top 100," "Top 101-200," ..
# Define the breaks for the bins
breaks_IMDB_test = c(-Inf,100, 200, 300, 400, 500, 600,700, 800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000, Inf)
# Define the labels for the bins
labels_IMDB_test = c("Top 1-100", "Top 101-200", "Top 201-300", "Top 301-400", "Top 401-500", "Top 501-600", "Top 601-700", "Top 701-800", "Top 801-900", "Top 901-1000", "Top 1001-1100", "Top 1101-1200", "Top 1201-1300", "Top 1301-1400", "Top 1401-1500", "Top 1501-1600", "Top 1601-1700", "Top 1701-1800", "Top 1801-1900", "Top 1901-2000", "Other")
# Create a new cloumn 'actor1_ranking' with the bins
movie_final_test$IMDB_ranking = cut(movie_final_test$movie_meter_IMDBpro, 
                                         breaks = breaks_IMDB_test, 
                                         labels = labels_IMDB_test, 
                                         include.lowest = TRUE, 
                                         right = FALSE)
movie_final_test$IMDB_ranking = as.factor(movie_final_test$IMDB_ranking)
#can drop movie_meter_IMDBpro later

################
#Production company
###############
production_training = table(movie_final$production_company)
production_training 
production_test=table(movie_final_test$production_company)
production_test
movie_final_test$production_company = as.character(movie_final_test$production_company)
# Replace the names of these companies with 'Other' if they are not present i the training set
movie_final_test$production_company[!movie_final_test$production_company %in% production_training ] = 'Other'
#Check
movie_final_test$production_company
# Convert 'production_company' back to a factor for later use in analysis and modeling
movie_final_test$production_company = as.factor(movie_final_test$production_company)

################
#Irrelevent Predictors
###############
movie_final_test$movie_title <- NULL
movie_final_test$movie_id <- NULL
movie_final_test$movie_link <- NULL
movie_final_test$imdb_link = NULL
# movie_final$release_day <- NULL
movie_final_test$language <- NULL # All English movies
# movie_final$aspect_ratio <- NULL
movie_final_test$cinematographer<-NULL
movie_final_test$plot_keywords <-NULL 
movie_final_test$genres  <-NULL 
movie_final_test$actor1<-NULL   
movie_final_test$actor1_star_meter<-NULL 
movie_final_test$actor2<-NULL
movie_final_test$actor2_star_meter<-NULL
movie_final_test$actor3<-NULL
movie_final_test$actor3_star_meter<-NULL
movie_final_test$movie_meter_IMDBpro <-NULL

movie_final_test$movie_budget <- as.numeric(gsub(",", "", movie_final_test$movie_budget))

##################### Predict using our best model reg23
new_data = movie_final_test[,c("duration","nb_news_articles","release_year","movie_budget","IMDB_ranking","country","maturity_rating","action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime")]
predict(reg23, new_data)

##################
#Visualization 
#################

####################
#Continuous variable
####################

######## Duration
bin = c(0,2,4,6,8,10)
ggplot(movie_final, aes(x=duration, fill= cut(imdb_score,bin))) +
  geom_histogram(color="black")+
  geom_vline(data=movie_final, aes(xintercept=mean(duration)), color="red",linewidth=1.5,#mean line
             linetype="dashed")+
  labs(title="Duration - Frequency distribution",x="Duration", y = "Frequency")+
  theme_classic() +
  scale_fill_discrete(name = "imdbRating",
                      labels=c("<2", "2 to 4", "4 to 6", "6 to 8", "8-10"))+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")
######## Number of News Articles
bin = c(0,2,4,6,8,10)
ggplot(movie_final, aes(x=nb_news_articles, fill= cut(imdb_score,bin))) +
  geom_histogram(color="black")+
  geom_vline(data=movie_final, aes(xintercept=mean(nb_news_articles)), color="red",linewidth=1.5,#mean line
             linetype="dashed")+
  labs(title="Number of News Articles - Frequency distribution",x="Number of News Articles", y = "Frequency")+
  theme_classic() +
  scale_fill_discrete(name = "imdbRating",
                      labels=c("<2", "2 to 4", "4 to 6", "6 to 8", "8-10"))+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")
####### Release Year
bin = c(0,2,4,6,8,10)
ggplot(movie_final, aes(x=release_year, fill= cut(imdb_score,bin))) +
  geom_histogram(color="black")+
  geom_vline(data=movie_final, aes(xintercept=mean(release_year)), color="red",linewidth=1.5,#mean line
             linetype="dashed")+
  labs(title="Release Year - Frequency distribution",x="Release Year", y = "Frequency")+
  theme_classic() +
  scale_fill_discrete(name = "imdbRating",
                      labels=c("<2", "2 to 4", "4 to 6", "6 to 8", "8-10"))+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")
######## Budget
bin = c(0,2,4,6,8,10)
ggplot(movie_final, aes(x=movie_budget, fill= cut(imdb_score,bin))) +
  geom_histogram(color="black")+
  geom_vline(data=movie_final, aes(xintercept=mean(movie_budget)), color="red",linewidth=1.5,#mean line
             linetype="dashed")+
  labs(title="Budget  - Frequency distribution",x="Budget ", y = "Frequency")+
  theme_classic() +
  scale_fill_discrete(name = "imdbRating",
                      labels=c("<2", "2 to 4", "4 to 6", "6 to 8", "8-10"))+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")
####################
#Categorical Variable
####################
#####Country
ggplot(movie_final, aes(x=country, y=imdb_score)) +
  geom_boxplot(lwd=1.2) +geom_jitter(alpha=0.09, aes(color=cut(imdb_score,bin), ))+
  theme_gray()+
  labs(title="Rating vs Country",x="Country", y="imdBRating",
       colour="imdbRating") + scale_color_hue(l=40, c=35)+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")
####Maturity rating
ggplot(movie_final, aes(x=maturity_rating, y=imdb_score)) +
  geom_boxplot(lwd=1.2) +geom_jitter(alpha=0.09, aes(color=cut(imdb_score,bin), ))+
  theme_gray()+
  labs(title="Rating vs Maturity rating",x="Maturity rating", y="imdBRating",
       colour="imdbRating") + scale_color_hue(l=40, c=35)+
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

####IMDB Ranking
ggplot(movie_final, aes(x=IMDB_ranking, y=imdb_score)) +
  geom_boxplot(lwd=1.2) +
  geom_jitter(alpha=0.09, aes(color=cut(imdb_score, bin))) +
  theme_gray() +
  labs(title="Rating vs IMDB Ranking", x="IMDB Ranking", y="IMDB Rating", colour="IMDB Rating") + 
  scale_color_hue(l=40, c=35) +
  theme(
    plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
    legend.title = element_text(color = "black", size = 18),
    legend.text = element_text(color = "black", size = 14, face="bold"),
    axis.text.x = element_text(angle=45, hjust=1, size=10),  # Rotate x-axis labels and adjust size
    axis.text.y = element_text(size=12, face="bold"),
    axis.title = element_text(size=14, face="bold"),
    legend.position="bottom"
  )







