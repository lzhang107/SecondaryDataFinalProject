# Pranathi Ganni

#install.packages("ggplot2")
library(ggplot2)
# Import data and create variables for the data sets: infantMortality, GDPPC, manufacturing
# download crv for infant mortality: https://data.worldbank.org/indicator/SP.DYN.IMRT.IN?view=chart
# rename as zip file and csv file as InfantMortality
# open csv file and delete the top four rows (else R cannot read the data since columns are not at the top row) 
InfantMortality<-read.csv("/Users/PranathiG/Desktop/Secondary Data/Final Project/InfantMortality/InfantMortality.csv")

# download crv for GDPPC: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?view=chart
# rename as zip file and csv file as GDPPC
# open csv file and delete the top four rows (else R cannot read the data since columns are not at the top row) 
GDPPC<-read.csv("/Users/PranathiG/Desktop/Secondary Data/Final Project/GDPPC/GDPPC.csv")

# download crv for Manufacturing: https://data.worldbank.org/indicator/NV.IND.MANF.ZS?end=2016&start=1981&view=chart
# rename as zip file and csv file as Manufacturing
# open csv file and delete the top four rows (else R cannot read the data since columns are not at the top row) 
Manufacturing<-read.csv("/Users/PranathiG/Desktop/Secondary Data/Final Project/Manufacturing/Manufacturing.csv")

# Keep only variables you need
E_InfantMort_Egypt<-InfantMortality[66,c(1,3,25:60)]
T_InfantMort_Thailand<-InfantMortality[232,c(1,3,25:60)]
SK_InfantMort_SouthKorea<-InfantMortality[125,c(1,3,25:60)]
E_GDPPC_Egypt<-GDPPC[66,c(1,3,25:60)]
T_GDPPC_Thailand<-GDPPC[232,c(1,3,25:60)]
SK_GDPPC_SouthKorea<-GDPPC[125,c(1,3,25:60)]
E_Manu_Egypt<-Manufacturing[66,c(1,3,25:60)]
T_Manu_Thailand<-Manufacturing[232,c(1,3,25:60)]
SK_Manu_SouthKorea<-Manufacturing[125,c(1,3,25:60)]

# Combine all data sets for specific country into one data.frame
# This is mostly so we can use this to run correlation tests
Egypt <- data.frame()
for (i in 1:35) {
  Egypt[i, 1] <- E_InfantMort_Egypt[1, 3 + i]
  Egypt[i, 2] <- E_GDPPC_Egypt[1, 3 + i]
  Egypt[i, 3] <- E_Manu_Egypt[1, 3 + i]
}
colnames(Egypt) <- c("Infant Mortality", "GDPPC", "Manufacturing")

Thailand <- data.frame()
for (i in 1:35) {
  Thailand[i, 1] <- T_InfantMort_Thailand[1, 3 + i]
  Thailand[i, 2] <- T_GDPPC_Thailand[1, 3 + i]
  Thailand[i, 3] <- T_Manu_Thailand[1, 3 + i]
}
colnames(Thailand) <- c("Infant Mortality", "GDPPC", "Manufacturing")

SouthKorea <- data.frame()
for (i in 1:35) {
  SouthKorea[i, 1] <- SK_InfantMort_SouthKorea[1, 3 + i]
  SouthKorea[i, 2] <- SK_GDPPC_SouthKorea[1, 3 + i]
  SouthKorea[i, 3] <- SK_Manu_SouthKorea[1, 3 + i]
}
colnames(SouthKorea) <- c("Infant Mortality", "GDPPC", "Manufacturing")

# Count all missing values for each initial variables
titles <- c("Infant Mortality", "GDPPC", "Manufacturing")
row_a <- c(sum(is.na(E_InfantMort_Egypt))/nrow(E_InfantMort_Egypt), sum(is.na(E_GDPPC_Egypt))/nrow(GDPPC), sum(is.na(E_Manu_Egypt)/nrow(E_Manu_Egypt)))
row_b <- c(sum(is.na(T_InfantMort_Thailand))/nrow(T_InfantMort_Thailand), sum(is.na(T_GDPPC_Thailand))/nrow(T_GDPPC_Thailand), sum(is.na(T_Manu_Thailand))/nrow(T_Manu_Thailand))
row_c <- c(sum(is.na(SK_InfantMort_SouthKorea))/nrow(SK_InfantMort_SouthKorea), sum(is.na(SK_GDPPC_SouthKorea))/nrow(SK_GDPPC_SouthKorea), sum(is.na(SK_Manu_SouthKorea))/nrow(SK_Manu_SouthKorea))
NumberofMissingValues <- data.frame("Dataset" = titles, "% of Misisng Egypt-related Values"  = row_a, "% of Misisng Thailand-related Values"  = row_a, "% of Misisng South Korea-related Values"  = row_a)

# Look at countries that do not have data and see if it is related to other variables by running frequency
# Check to see if correlation between those with missing manufacturing and GDPPC
# Use pearson correlation
Country_NA <- data.frame()
for (i in 1:264) {
  Country_NA[i, 1] <- Manufacturing[i, 1]
  ManuNA <- sum(is.na(Manufacturing[i,c(1,3,25:60)]))
  GDPPCNA <- sum(is.na(GDPPC[i,c(1,3,25:60)]))
  Country_NA[i, 2] <- ManuNA
  Country_NA[i, 3] <- GDPPCNA
}
colnames(Country_NA) <- c("Country", "Manufacturing_NA", "GDPPC_NA")

cor(Country_NA$Manufacturing_NA, Country_NA$GDPPC_NA, method = "pearson")
# correlation coefficient is 0.6698955
# so there is a positive/uphill, a bit more moderate relationship

ggplot(Country_NA, aes(x=Manufacturing_NA, y=GDPPC_NA)) + geom_point() + geom_smooth(method="lm") +
     labs(title="Correlation between Missing Data in Manufacturing and GDPPC Datasets", subtitle="From World Bank, 1980-2015",
     y="Number of NA in GDPPC", x="Number of NA in Manufacturing")

NA_Table <- matrix(c(1261, 3050, 74, 6454), ncol = 2, byrow = TRUE)
colnames(NA_Table) <- c("Missing GDPPC Data", "Not Missing GDPPC Data")
rownames(NA_Table) <- c("Missing Manufacturing Data", "Not Missing Manufacturing Data")

# Create categorical variables from continuous variables to see semi-periphery, periphery, etc. types
# Look at amount missing in GDPPC values in 1980
sum(is.na(GDPPC[25])) * 100/ nrow(GDPPC[25])

# Look at amount missing in GDPPC values in 2015
sum(is.na(GDPPC[60])) * 100/ nrow(GDPPC[60])

WorldIncome1980 <- GDPPC[,c(1,25)]
WorldIncome1980$X1980 <- round(log10(WorldIncome1980$X1980), digits = 1)
colnames(WorldIncome1980) <- c("Country", "logGDPPC1980")
x <- table(WorldIncome1980$logGDPPC1980)
WorldIncome1980_ChosenCountries <- WorldIncome1980[c(66,232,125),]
cols <- ifelse(row.names(x) == toString(WorldIncome1980[66,2]) | row.names(x) == toString(WorldIncome1980[232,2]) | row.names(x) == toString(WorldIncome1980[125,2]), c("yellow", "green", "red", "orange", "pink"), "gray")
barplot(x, col = cols, main = "World Income in 1980", xlab = "logGDPPC", ylab = "Number of Countries")
#Explain missign data in a footnote "1980 GDPPC data from blank countries missing"

WorldIncome2015 <- GDPPC[,c(1,60)]
WorldIncome2015$X2015 <- round(log10(WorldIncome2015$X2015), digits = 1)
colnames(WorldIncome2015) <- c("Country", "logGDPPC2015")
x <- table(WorldIncome2015$logGDPPC2015)
WorldIncome2015_ChosenCountries <- WorldIncome2015[c(66,232,125),]
cols <- ifelse(row.names(x) == toString(WorldIncome2015[66,2]) | row.names(x) == toString(WorldIncome2015[232,2]) | row.names(x) == toString(WorldIncome2015[125,2]), c("yellow", "orange", "pink", "red"), "gray")
barplot(x, col = cols, main = "World Income in 2015", xlab = "logGDPPC", ylab = "Number of Countries")
#Explain missign data in a footnote "2015 GDPPC data from blank countries missing"

CategoricalTable <- merge(WorldIncome1980, WorldIncome2015, by = "Country")
CategoricalTable$Category1980 <- cut(CategoricalTable$logGDPPC1980, breaks = 3, labels=c("semi-periphery", "periphery", "core"))
CategoricalTable$Category2015 <- cut(CategoricalTable$logGDPPC2015, breaks = 3, labels=c("semi-periphery", "periphery", "core"))
CategoricalTable_ChosenCountries <- CategoricalTable[c(66,238,124),]

# Make line charts showing manufacturing, gdp data over years
# Look at correlation between manufacturing and gdp
x <- Egypt$Manufacturing
y <- Egypt$GDPPC
cor(Egypt$Manufacturing, Egypt$GDPPC, method = "pearson")
ggplot(Egypt, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
  labs(title="Correlation between Manufacturing and GDPPC Datasets in Egypt", subtitle="From World Bank, 1980-2015", 
       y="GDPPC Values", x="Manufacturing Values")
abline(lm(y ~ x, data = Egypt), col = "blue")

x <- Thailand$Manufacturing
y <- Thailand$GDPPC
cor(Thailand$Manufacturing, Thailand$GDPPC, method = "pearson")
ggplot(Thailand, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
  labs(title="Correlation between Manufacturing and GDPPC Datasets in Thailand", subtitle="From World Bank, 1980-2015", 
       y="GDPPC Values", x="Manufacturing Values")
abline(lm(y ~ x, data = Thailand), col = "blue")

x <- SouthKorea$Manufacturing
y <- SouthKorea$GDPPC
cor(SouthKorea$Manufacturing, SouthKorea$GDPPC, method = "pearson")
ggplot(SouthKorea, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
  labs(title="Correlation between Manufacturing and GDPPC Datasets in South Korea", subtitle="From World Bank, 1980-2015", 
       y="GDPPC Values", x="Manufacturing Values")
abline(lm(y ~ x, data = SouthKorea), col = "blue")

# Make line charts showing infant mortality, gdp data over years
# Look at correlation between infant mortality and gdp
x <- Egypt$`Infant Mortality`
y <- Egypt$GDPPC
cor(Egypt$`Infant Mortality`, Egypt$GDPPC, method = "pearson")
ggplot(Egypt, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
  labs(title="Correlation between Infant Mortality and GDPPC Datasets in Egypt", subtitle="From World Bank, 1980-2015", 
       y="GDPPC Values", x="Infant Mortality Values")
abline(lm(y ~ x, data = Egypt), col = "blue")

x <- Thailand$`Infant Mortality`
y <- Thailand$GDPPC
cor(Thailand$`Infant Mortality`, Thailand$GDPPC, method = "pearson")
ggplot(Thailand, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
  labs(title="Correlation between Infant Mortality and GDPPC Datasets in Thailand", subtitle="From World Bank, 1980-2015", 
       y="GDPPC Values", x="Infant Mortality Values")
abline(lm(y ~ x, data = Thailand), col = "blue")

x <- SouthKorea$`Infant Mortality`
y <- SouthKorea$GDPPC
cor(SouthKorea$`Infant Mortality`, SouthKorea$GDPPC, method = "pearson")
ggplot(SouthKorea, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm") +
    labs(title="Correlation between Infant Mortality and GDPPC Datasets in South Korea", subtitle="From World Bank, 1980-2015", 
    y="GDPPC Values", x="Infant Mortality Values")
abline(lm(y ~ x, data = SouthKorea), col = "blue")

#make table with all the coorelation coefficients
CorrelationCoefficient <- matrix(c(cor(Egypt$`Infant Mortality`, Egypt$GDPPC, method = "pearson"), 0, 0, 0, cor(Thailand$`Infant Mortality`, Thailand$GDPPC, method = "pearson"), 0, 0, 0, cor(SouthKorea$`Infant Mortality`, SouthKorea$GDPPC, method = "pearson")), ncol = 3, byrow = TRUE)
colnames(CorrelationCoefficient) <- c("Egypt Infant Mortality","Thailand Infant Mortality","South Korea Infant Mortality")
rownames(CorrelationCoefficient) <- c("Egypt GDPPC","Thailand GDPPC","South Korea GDPPC")
CorrelationCoefficient[CorrelationCoefficient == 0] <- NA
CorrelationCoefficient <- as.table(CorrelationCoefficient)

# multiple regression model
mr1 <- lm(formula = SouthKorea$GDPPC ~ SouthKorea$Manufacturing + SouthKorea$`Infant Mortality`, data = SouthKorea)
summary(mr1)
# aka GDPPC = -3.993*10^10 + 3.84*10^10*Manufacturing - 3.734*10^10*InfantMortalilty
lm(formula = SouthKorea$Manufacturing ~ SouthKorea$GDPPC + SouthKorea$`Infant Mortality`, data = SouthKorea)
# aka Manufacuting = 26.35 + 2.919*10^-12*GDPPC - 2.948*10^-2*InfantMortality

# test of significance: linearity tested by correlatoin, regression, multiple regression
# nice code!
