library(data.table)
library(MASS)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# read in data file from path
data = read.csv("multipleChoiceResponses.csv", header=TRUE, sep = ",")

# remove respondents who did not self identify as male or female due to small sample size
data2 = subset(data, GenderSelect == 'Male' | GenderSelect == 'Female')

# remove respondents below university or over retirement age
data3 = subset(data2, Age > 17 & Age <= 70)

table(data3$CurrentJobTitleSelect, data3$GenderSelect)
#                                    Female   Male 
#Unknown                                935   3636                                                 
#Business Analyst                       144    626                                                 
#Computer Scientist                      35    285                                                 
#Data Analyst                           267    912                                                 
#Data Miner                              14    100                                                 
#Data Scientist                          35   2015                                                 
#DBA/Database Engineer                   26    153                                                 
#Engineer                                51    485                                                 
#Machine Learning Engineer               70    533                                                 
#Operations Research Practitioner        10     42                                                 
#Other                                  181    998                                                 
#Predictive Modeler                      27    145                                                 
#Programmer                              52    391                                                
#Researcher                             156    436                                                 
#Scientist/Researcher                   161    774                                                 
#Software Developer/Software Engineer   157   1545                                                 
#Statistician                            59    221 
#TOTAL                                 2696  13297

# create data frame from table data
jobs = c('Unknown', 'Business Analyst', 'Computer Scientist', 'Data Analyst', 'Data Miner', 'Data Scientist', 'Database Engineer',
         'Engineer', 'Machine Learning Engineer', 'Operations Research Practitioner', 'Other', 'Predictive Modeler', 'Programmer',
         'Researcher', 'Scientist/Researcher', 'Software Developer', 'Statistician')

female_n = c(935,144,35,267,14,35,26,51,70,10,181,27,52,156,161,157,59)

male_n = c(3636,626,285,912,100,2015,153,485,533,42,998,145,391,436,774,1545,221)

job_by_sex = data.frame(jobs, female_n, male_n)
names(job_by_sex) = c('Job Title', 'Females', "Males")

# compute gender ratio
job_by_sex$ratio = job_by_sex$Females / job_by_sex$Males

# determine what percentage of the sample holds what job title
job_by_sex$Female_Perc = job_by_sex$Females / 2696
job_by_sex$Male_Perc = job_by_sex$Males / 13297

cor.test(job_by_sex$Female_Perc, job_by_sex$Male_Perc, alternative='two.sided', data=job_by_sex)
#Pearson's product-moment correlation
#
#data:  job_by_sex$Female_Perc and job_by_sex$Male_Perc
#t = 6.1683, df = 15, p-value = 1.798e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6176886 0.9434892
#sample estimates:
#      cor 
#0.8468987 

job_by_sex$Diff = job_by_sex$Male_Perc - job_by_sex$Female_Perc
# Biggest gap is in data science, which skews male
# A larger percentage of women are business or data analysts and researchers
# A larger percentage of women have "other" job titles which could mean less of them work in technology

# wrap labels for plotting
job_by_sex$Job_Labels = str_wrap(job_by_sex$`Job Title`, width=10)
# remove non-employed (students) respondents from plot
job_by_sex2 = job_by_sex[-1,]
MoreMales = '#513f9b'
MoreFemales = '#3f9b75'
cols = c('#3f9b75','#513f9b','#3f9b75','#513f9b','#513f9b','#513f9b','#513f9b','#513f9b','#3f9b75', '#513f9b','#513f9b','#513f9b','#3f9b75','#3f9b75','#513f9b','#3f9b75')
cols2 = c(MoreFemales, MoreMales, MoreFemales, MoreMales,MoreMales,MoreMales,MoreMales,MoreMales,MoreFemales,MoreMales,MoreMales,MoreMales,MoreFemales,MoreFemales,MoreMales,MoreFemales)

ggplot(job_by_sex2, aes(job_by_sex2$Job_Labels, job_by_sex2$Diff, fill=cols2)) +geom_bar(stat = "identity") + labs(x="Job Title", y="Percentage Difference (Male - Female)") + theme_minimal() 
