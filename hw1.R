library(dplyr)
library(ggplot2)
library(reshape)
library(viridis)
library(hrbrthemes)
setwd("D:/Statistiacl Principle of DataScience")

data <- read.csv('student-mat.csv')
head(data,5)

### Task 2.1
# Look up the data statistic description
summary(data)
st?(data)

## Through the summary, school, paid, activities, romantic have typos; freetime, famsize and G1 have missing values, G3 has wrong value for the grading.
# List the rows with typos
data %>% filter(school != 'GP' & school != 'MS') %>% nrow
data %>% f?lter(famsize != 'GT3' & famsize !='LE3')
data %>%  filter(paid != 'no' & paid != 'yes')
data %>% filter(activities != 'no' & activities != 'yes')
data %>% filter(romantic != 'no' & romantic != 'yes')
data %>% filter(G3 > 20)
# Get the rows which have an NA?value
data[is.na(data$freetime),]
data[is.na(data$G1),]

## Correct the typo
data <- data %>% mutate(school = replace(school , school == 'Gp', 'GP'))
data <- data %>% mutate(paid = replace(paid , paid == 'ye', 'yes'))  
data <- data %>% mutate(activities =?replace(activities , activities == 'No', 'no'))
data <- data %>% mutate(romantic = replace(romantic , romantic == 'YES', 'yes'))
data <- data %>% mutate(G3 = replace(G3 , G3 == 100, 10))
# Correct the factor level
data$school <- factor(data$school, levels ? c("GP", "MS"))
data$famsize <- factor(data$famsize, levels = c("LE3", "GT3"))
data$paid <- factor(data$paid, levels = c("no", "yes"))
data$activities <- factor(data$activities, levels = c("no", "yes"))
data$romantic <- factor(data$romantic, levels = c("no?, "yes"))
summary(data)

## Delete
data <- na.omit(data)
# Get the row's index(this should also belongs to missing value)
print(data[data$famsize != 'GT3' & data$famsize !='LE3',])
data <- data[-166,]
summary(data)
nrow(data)

## Save to new csv
write.csv(?ata, 'clean-student-mat.csv')


### Task 2.2
# Violin Plot
grades <- data[c(1,26:28)]
grades <- melt(grades, id='X')
colnames(grades) <- c('X','period','grade')

p <- ggplot (grades , aes(x = factor(period) , y = grade, fill = period))+
  geom_violin ()+
 ?geom_boxplot (width = 0.1)+
  stat_summary (fun ='mean',geom = 'point', shape =18, size =3, fill ='gray')

p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Bar Chart

# sorted <- data %>% arrange(desc(traveltime))
# g3 <- cut(sorted$G3, 4,?labels = c('0-5', '6-10', '11-15', '16-20'))
# bar_data <- data.frame(sorted, g3)
counts <- table(data$address, data$G3)
barplot(counts, main="Final Grade Distribution by Urban and Rural",
        xlab="Final Grade",ylab = "counts", col=c("darkblue","red")?
        legend = rownames(counts), beside=TRUE)

ggplot(data, aes(fill=address, x=traveltime, y=G3)) +
  geom_bar(position="stack",stat="identity") 

# Densities of Gmean
data$Gmean <- as.numeric(apply(data[,26:28], 1, mean))
head(data)

density <- ggplot?data, aes(Gmean, fill=school)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
density

# Scatter Plot
pairs(~G1+G2+G3,data = data,
      main = "Scatterplo? Matrix")

## Findings:
'
1. The garde median of three period are nearly the same.
2. G3 has the highest distribution, and more outliers than G1 and G2.
3. More students get high G3 grade.
4. In this dataset, more students are live in urban area.
5. Studen?s who live in urban area get the higher final grade and have less traveltime to school.
6. Two schools have the similar mean grade distribution. They are both normal distribution.
7. There are correlations between grades of different periods.
'

### Task 2?3
# Check the outliers
# Absences
ggplot(data, aes(y=as.factor(absences),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)
# Failures
ggplot(data, aes(y=as.factor(failures),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)
# Age
ggplot(data, aes(y=as.fac?or(age),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)
# Dalc
ggplot(data, aes(y=as.factor(Dalc),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)
# Walc
ggplot(data, aes(y=as.factor(Walc),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)
# Health
gg?lot(data, aes(y=as.factor(health),x=G3))+
  geom_boxplot(fill='slateblue',alpha=0.2)

health <- data %>% filter(health>2 & G3==0)

# Discover the correlations between all variables
df <- data
df$school <- as.numeric(df$school)
df$sex <- as.numeric(df$sex) ?1
df$address <- as.numeric(df$address)
df$school <- as.numeric(df$school)
df$famsize <- as.numeric(df$famsize)
df$Pstatus <- as.numeric(df$Pstatus) -1
df$schoolsup <- as.numeric(df$schoolsup) -1
df$famsup <- as.numeric(df$famsup) -1
df$paid <- as.numeric(d?$paid) -1
df$activities <- as.numeric(df$activities) -1
df$nursery <- as.numeric(df$nursery) -1
df$higher <- as.numeric(df$higher) -1
df$internet <- as.numeric(df$internet) -1
df$romantic <- as.numeric(df$romantic) -1
summary(df)

cormat <- round(cor(df[2:?9]),2)
head(cormat)
melted_cormat <- melt(cormat)
# Adjust the plot scale
limit <- max(abs(melted_cormat$value))*c(-1, 1)
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + 
  geom_tile()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1,?size = 10, hjust = 1))+
  coord_fixed()+
  scale_fill_distiller(palette = "BrBG", limit = limit)

# Discover with linear regression to figure out which variables are important to the final grade.
df2 <-  data.frame(df[2:25], df[28])
linear <- lm(G3 ~., dat?=df2)  # build linear regression model on full data
print(linear)

## According to the coefficients, sex, failures, schoolsup, higher are the most important variables, and the secondary importants are Pstatus, romantic and internet.
# High grade students
o?hers <- data %>% filter(G3<14)
high <- data %>% filter(G3>=14)
# Higher?
ggplot(data, aes(G3, fill=higher)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
?# Students who have no desire to get higher education tend to have lower final grade.

# sex
ggplot(data, aes(G3, fill=sex)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justi?ication=c(0,1))
## The distributions are similar, but more male students have grade >13

# School Support
ggplot(others, aes(G3, fill=schoolsup)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position?c(0,1), legend.justification=c(0,1))

ggplot(high, aes(G3, fill=schoolsup)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#e685b5')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
## School support help very litt?e propotion of students get a better grade

# Failures
ggplot(data, aes(fill=as.factor(failures), x=G3)) +
  geom_bar(position="dodge",stat="count") 
## There are less students who have higher final grade get failures.

# Pstatus
ggplot(data, aes(G3, fill=?status)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

summary(data %>% filter(Pstatus=="A"))
summary(data %>% filter(Pstatus=="T"))
## Students whos'par?nts are apart tend to have higher final grade.
# Romantic
ggplot(data, aes(G3, fill=romantic)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#e685b5')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

summary(data?%>% filter(romantic=="no"))
summary(data %>% filter(romantic=="yes"))
## More students who have no romantic relationships have high final grade.

# Internet
ggplot(data, aes(G3, fill=internet)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c(?#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Alcohol Consumption
summary(data %>% filter(Dalc>3) %>% select(Dalc,Walc,G3))
summary(data %>% filter(Dalc<3) %>% select(Dalc,Walc,G3))
ggplot(data, aes(x=Dalc, y=G3)) ? geom_point()
ggplot(data, aes(x=Walc, y=G3)) + geom_point()

ggplot(data, aes(x=G3, y=Gmean)) + geom_point()
ggplot(data, aes(x=G3, y=absences)) + geom_point()

### Conclusions:
'
According to the coefficients, sex, failures, schoolsup, higher are the mos? important variables, 
and the secondary important variables are Pstatus, romantic and internet. However, too much noisy data impacts 
a lot on the result. After exploring the correlations, alcohol consumption seems to be an important variable!
In the end,?the analysis shows that if student want to get better grades they should drink less alcohol,especially 
for the person who drink at the weekdays as well. The other variables only have impacts on a little part of the
students.
'
### Possible advices for imp?ove the grade:
'
Drink less alcohol, avoid to have a romance, and motivate self to get higher education.
'
