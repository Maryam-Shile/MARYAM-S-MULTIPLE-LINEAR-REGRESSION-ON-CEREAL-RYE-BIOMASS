

#FACTORS AFFECTING CEREAL RYE BIOMASS IN BELTSVILLE USING SIMPLE MULTIPLE LINEAR REGRESSION MODEL#



##########DATASET IMPORTATION
#1. Import dataset from system and save as G-data. 
#2. Understand dataset and check from missing values.
G_data<- read.csv ("C:/Users/user/Documents/R/prof_data.csv", header = TRUE)
View(G_data)
str(G_data)
summary(G_data)
class(G_data)
View(G_data)




######## DATA WRANGLING
#3. Using only the data with _25, slice your dataset
the_data<- G_data[, 1:14]
View(the_data)
#4. Filter out only data from Beltsville from the_data
new_data<- filter(the_data, location== "Beltsville")
View(new_data)


#4. Extract columns from new_data (BIOMASS AND ITS PREDICTOR VARIABLES)
new_data<- new_data[, c(9, 10, 12, 8, 5, 6, 7)]

#######EXPLORATORY DATA ANALYSIS FOR test_data
#5. View information on central tendency
summary(new_data)

#6. Check for Outliers
boxplot(new_data$ndvi_25)
boxplot(new_data$n_eff_25)
boxplot(new_data$tiller_eff_25)
boxplot(new_data$n_rate_fall)
boxplot(new_data$n_rate_spring)
boxplot(new_data$n_soil_base)
boxplot(new_data$biomass_25)


########UNDERSTANDING RELATIONSHIP BETWEEN THE SHOOT NITROGEN AND INDEPENDENT VARIABLES
#7. Plot relationships with scatter plot 
pairs(~biomass_25+ndvi_25+n_eff_25+n_rate_fall+tiller_eff_25+n_soil_base, data = new_data)

#8. Determine correlation with correlation matrix
correlation_1<- round(cor(new_data), 2)
correlation_1



#9. More information about the relationship using multiple linear regression
first_model<- lm(biomass_25~ndvi_25+n_eff_25+n_rate_fall+tiller_eff_25+n_soil_base, data = new_data)

summary(first_model)

#10. GIVE RESULTS
#1. Model percentage of fit is 85.05%. It indicates that this regression model is a good fit for predicting values of cereal rye biomass 
#2. Normalized difference vegetation index has a p value of 0.00018 and a highly negative effect or inverse relationship to cereal rye biomass .
#3. Nitrogen efficiency, nitrogen fertilizer application in fall and Tiller efficiency application in fall (with p values of < 2e-16, 3.44e-11 and 1.98e-09 respectively) all have a very high significant effect on biomass.
#4. Nitrogen soil base with Pvalue- 0.01163 shows that it does not affect cereal rye biomass.


######REFERENCES######
#Data source: GuilleMarc/CoverCrops_ML_Project (2020) https://raw.githubusercontent.com/GuilleMarc/CoverCrops_ML_Project/master/data_25.csv







