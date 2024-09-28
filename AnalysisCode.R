#Import required packages
library(readxl)
library(ggplot2)                     
library(GGally)
library(Hmisc)
library(rstatix)
library(writexl)
library(corrplot)
library(ggstatsplot)
library(FSA)
library(likert)
library(dplyr)

#Import file
survey <- read_excel("~/locationoffile/nameoffile.xlsx", 
                     sheet = "nameofsheet")

survey <- as.data.frame(survey)

#General statistical analysis.
str(survey)
distribution <- lapply(survey[11:46], shapiro.test)  #all fail normality

#Correlations using spearmans as most appropriate for Likert data

#Make corr plot
#questions only
sub_survey <- survey[,c(12:47)]
pcor <- cor.mtest(sub_survey, conf.level = 0.95, method = "spearman", na.action(na.exclude))
M = cor(sub_survey, method = "spearman", use = "pairwise.complete.obs" )

#Option to rename if desired
colnames(M) <- c("Technology and Education Q1", "Technology and Education Q2", "Technology and Education Q3", "Technology and Education Q4",
                 "Technology and Education Q5", "Technology and Education Q6", "Technology and Education Q7", "Technology and Education Q8",
                 "Technology and Education Q9", "Experience with AI and GPTs Q1", "Experience with AI and GPTs Q2", "Experience with AI and GPTs Q3",
                 "Experience with AI and GPTs Q4", "Experience with AI and GPTs Q5", "Experience with AI and GPTs Q6", "Experience with AI and GPTs Q7",
                 "Standardized Clients Q1", "Standardized Clients Q2", "Standardized Clients Q3", "Standardized Clients Q4", "Standardized Clients Q5",
                 "Standardized Clients Q6", "Standardized Clients Q7", "Standardized Clients Q8", "Standardized Clients Q9", "Standardized Clients Q10",
                 "Standardized Clients Q11", "Standardized Clients Q12", "AI Integration in Vet Ed Q1", "AI Integration in Vet Ed Q2", "AI Integration in Vet Ed Q3",
                 "AI Integration in Vet Ed Q4", "AI Integration in Vet Ed Q5", "AI Integration in Vet Ed Q6", "AI Integration in Vet Ed Q7", "AI Integration in Vet Ed Q8")
rownames(M) <- c("Technology and Education Q1", "Technology and Education Q2", "Technology and Education Q3", "Technology and Education Q4",
                 "Technology and Education Q5", "Technology and Education Q6", "Technology and Education Q7", "Technology and Education Q8",
                 "Technology and Education Q9", "Experience with AI and GPTs Q1", "Experience with AI and GPTs Q2", "Experience with AI and GPTs Q3",
                 "Experience with AI and GPTs Q4", "Experience with AI and GPTs Q5", "Experience with AI and GPTs Q6", "Experience with AI and GPTs Q7",
                 "Standardized Clients Q1", "Standardized Clients Q2", "Standardized Clients Q3", "Standardized Clients Q4", "Standardized Clients Q5",
                 "Standardized Clients Q6", "Standardized Clients Q7", "Standardized Clients Q8", "Standardized Clients Q9", "Standardized Clients Q10",
                 "Standardized Clients Q11", "Standardized Clients Q12", "AI Integration in Vet Ed Q1", "AI Integration in Vet Ed Q2", "AI Integration in Vet Ed Q3",
                 "AI Integration in Vet Ed Q4", "AI Integration in Vet Ed Q5", "AI Integration in Vet Ed Q6", "AI Integration in Vet Ed Q7", "AI Integration in Vet Ed Q8")
corrplot(M)

#Manuscript figure plot
corrplot(M, p.mat = pcor$p, method = 'circle', type = 'lower', insig='blank', col =COL2('PRGn'),
         diag=FALSE, tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)


#Kruskal-Wallis
#Semester.  Questions 
survey$Semester <-ordered(survey$Semester, levels = c("Second", "Fourth", "Sixth"))  #will not work for dunnTest
ggbetweenstats(data = survey,
               x = Semester,
               y = Q1,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q1 ~ Semester, data=survey)
dunnTest(Q1 ~ Semester, data=survey, method = "holm") #Can not have ggstatsplot loaded for this to work.  Keep in mind for rest of code.
dunnTest(Q1 ~ Semester, data=survey, method = "bonferroni")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q2,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q2 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q3,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q3 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q4,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q4 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q5,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q5 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q6,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q6 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q7,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q7 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q8,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q8 ~ Semester, data=survey)
dunnTest(Q8 ~ Semester, data=survey, method = "holm")
dunnTest(Q8 ~ Semester, data=survey, method = "bonferroni")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q9,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q9 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q10,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q10 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q11,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q11 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q12,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q12 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q13,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q13 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q14,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q14 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q15,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q15 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q16,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q16 ~ Semester, data=survey)
dunnTest(Q16 ~ Semester, data=survey, method = "holm")


ggbetweenstats(data = survey,
               x = Semester,
               y = Q17,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q17 ~ Semester, data=survey)
dunnTest(Q17 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q18,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q18 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q19,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q19 ~ Semester, data=survey)
dunnTest(Q19 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q20,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q20 ~ Semester, data=survey)
dunnTest(Q20 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q21,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q21 ~ Semester, data=survey)
dunnTest(Q21 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q22,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q22 ~ Semester, data=survey)
dunnTest(Q22 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q23,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q23 ~ Semester, data=survey)
dunnTest(Q23 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q24,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q24 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q25,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q25 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q26,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q26 ~ Semester, data=survey)
dunnTest(Q26 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q27,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q27 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q28,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q28 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q29,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q29 ~ Semester, data=survey)
dunnTest(Q29 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q30,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q30 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q31,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q31 ~ Semester, data=survey)

ggbetweenstats(data = survey,
               x = Semester,
               y = Q32,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q32 ~ Semester, data=survey)
dunnTest(Q32 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q33,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q33 ~ Semester, data=survey)
dunnTest(Q33 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q34,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q34 ~ Semester, data=survey)
dunnTest(Q34 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q35,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q35 ~ Semester, data=survey)
dunnTest(Q35 ~ Semester, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Semester,
               y = Q36,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q36 ~ Semester, data=survey)

#Kruskal wallis assessing age

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q1,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q1 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q2,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q2 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q3,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q3 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q4,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q4 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q5,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q5 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q6,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q6 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q7,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q7 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q8,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q8 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q9,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q9 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q10,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q10 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q11,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q11 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q12,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q12 ~ Age_range, data=survey)
dunnTest(Q12 ~ Age_range, data=survey, method="holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q13,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q13 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q14,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q14 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q15,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q15 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q16,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q16 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q17,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q17 ~ Age_range, data=survey)
dunnTest(Q17 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q18,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q18 ~ Age_range, data=survey)
dunnTest(Q18 ~ Age_range, data = survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q19,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q19 ~ Age_range, data=survey)
dunnTest(Q19 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q20,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q20 ~ Age_range, data=survey)
dunnTest(Q20 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q21,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q21 ~ Age_range, data=survey)
dunnTest(Q21 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q22,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q22 ~ Age_range, data=survey)
dunnTest(Q22 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q23,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q23 ~ Age_range, data=survey)
dunnTest(Q23 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q24,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q24 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q25,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q25 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q26,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q26 ~ Age_range, data=survey)
dunnTest(Q26 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q27,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q27 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q28,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q28 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q29,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q29 ~ Age_range, data=survey)
dunnTest(Q29 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q30,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q30 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q31,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q31 ~ Age_range, data=survey)

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q32,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q32 ~ Age_range, data=survey)
dunnTest(Q32 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q33,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q33 ~ Age_range, data=survey)
dunnTest(Q33 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q34,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q34 ~ Age_range, data=survey)
dunnTest(Q34 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q35,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q35 ~ Age_range, data=survey)
dunnTest(Q35 ~ Age_range, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Age_range,
               y = Q36,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q36 ~ Age_range, data=survey)

#Career Interest Kruskal-Wallis
ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q1,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q1 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q2,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q2 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q3,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q3 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q4,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q4 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q5,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q5 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q6,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q6 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q7,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q7 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q8,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q8 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q9,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q9 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q10,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q10 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q11,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q11 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q12,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q12 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q13,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q13 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q14,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q14 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q15,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q15 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q16,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q16 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q17,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q17 ~ Career_Interest, data=survey)
dunnTest(Q17 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q18,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q18 ~ Career_Interest, data=survey)
dunnTest(Q18 ~ Career_Interest, data = survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q19,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q19 ~ Career_Interest, data=survey)
dunnTest(Q19 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q20,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q20 ~ Career_Interest, data=survey)
dunnTest(Q20 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q21,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q21 ~ Career_Interest, data=survey)
dunnTest(Q21 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q22,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q22 ~ Career_Interest, data=survey)
dunnTest(Q22 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q23,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q23 ~ Career_Interest, data=survey)
dunnTest(Q23 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q24,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q24 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q25,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm",
               xlab = "Career Interest")
kruskal.test(Q25 ~ Career_Interest, data=survey)
dunnTest(Q25 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q26,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q26 ~ Career_Interest, data=survey)
dunnTest(Q26 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q27,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q27 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q28,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q28 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q29,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q29 ~ Career_Interest, data=survey)
dunnTest(Q29 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q30,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q30 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q31,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q31 ~ Career_Interest, data=survey)

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q32,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q32 ~ Career_Interest, data=survey)
dunnTest(Q32 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q33,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q33 ~ Career_Interest, data=survey)
dunnTest(Q33 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q34,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q34 ~ Career_Interest, data=survey)
dunnTest(Q34 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q35,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q35 ~ Career_Interest, data=survey)
dunnTest(Q35 ~ Career_Interest, data=survey, method = "holm")

ggbetweenstats(data = survey,
               x = Career_Interest,
               y = Q36,
               type = "nonparametric",
               plot.type = "violin",
               pairwise_comparisons = TRUE,
               pairwise.display = "significant",
               p.adjust.method = "holm")
kruskal.test(Q36 ~ Career_Interest, data=survey)
dunnTest(Q36 ~ Career_Interest, data=survey, method = "holm")

#Likert plots using Likert package
#Part 1 questions, 1-10
likertplots <- survey[,c(6,12:20)]
likertplots[] <- lapply(likertplots, as.factor)
likertplots$Semester <- factor(likertplots$Semester, levels = c("Second", "Fourth", "Sixth"))
str(likertplots)
likertplots  <- likertplots  %>% 
  rename("Q1 How much do you rely on online resources for your veterinary education?" = Q1,
         "Q2 How often do you find yourself turning to software or apps to help with your coursework?" = Q2,
         "Q3 When it comes to learning new tech tools for your classes, how confident do you feel?" = Q3,
         "Q4 How much do you enjoy using technology as part of your learning process?" = Q4,
         "Q5 How often do you independently explore new tech tools or apps beyond what is provided for your courses?" = Q5,
         "Q6 How easy is it for you to fix small issues with the digital tools you use?" = Q6,
         "Q7 How much have digital tools improved your learning in veterinary school?" = Q7,
         "Q8 How often do you feel overwhelmed by the technology skills required in your studies?" = Q8,
         "Q9 How likely are you to recommend the use of digital learning tools to your peers?" = Q9)

likertplots <- as.data.frame(likertplots)

Tech_ed <- reverse.levels(likertplots)

test <- likert(likertplots[,c(2:10)], group=Tech_ed$Semester)
plot(test, text.size=4) + theme(strip.text=element_text(size=12)) + theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c('#7b3294', '#c2a5cf','ivory2', '#a6dba0','#008837'), breaks = c("1","2","3","4","5"), name = "Likert Scale")

likertplots2 <- survey[,c(6,21:27)]
likertplots2[] <- lapply(likertplots2, as.factor)
likertplots2$Semester <- factor(likertplots2$Semester, levels = c("Second", "Fourth", "Sixth"))
str(likertplots2)
likertplots2  <- likertplots2  %>% 
  rename('Q10 How familiar are you with the term "Artificial Intelligence" (AI)?' = Q10,
         "Q11 How often do you use or interact with AI tools, in any context?" = Q11,
         "Q12 How often do you think AI is involved in the technology you use every day, like social media or online shopping?" = Q12,
         "Q13 Have you noticed features like auto-correct or predictive text on your phone?" = Q13,
         "Q14 How likely are you to explore AI tools on your own for personal or educational purposes?" = Q14,
         "Q15 If you've used AI-driven tools, how easy was it for you to understand and use them?" = Q15,
         "Q16 Rate your interest in learning more about AI and its applications in veterinary medicine." = Q16)

AI_exp <- reverse.levels(likertplots)

section2 <- likert(likertplots2[,c(2:8)], group=AI_exp$Semester)
plot(section2, text.size=4) + theme(strip.text=element_text(size=12)) + theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c('#7b3294', '#c2a5cf','ivory2', '#a6dba0','#008837'), breaks = c("1","2","3","4","5"), name = "Likert Scale")


likertplots3 <- survey[,c(6,28:33)]
likertplots3[] <- lapply(likertplots3, as.factor)
likertplots3$Semester <- factor(likertplots3$Semester, levels = c("Second", "Fourth", "Sixth"))
str(likertplots3)
likertplots3  <- likertplots3  %>% 
  rename('Q17 How effective are encounters with standardized clients (SC) as part of your veterinary training in communication skills?' = Q17,
         "Q18 What is your level of engagement during the SC encounters?" = Q18,
         "Q19 How realistic do you find the SC encounters?" = Q19,
         "Q20 How well do the SCs reflect the diversity of clients/owners that you might encounter in practice?" = Q20,
         "Q21 How well do the communication scenarios reflect the diversity of cases you might encounter in practice?" = Q21,
         "Q22 How effective are encounters with SCs in preparing you for real-world veterinary practice." = Q22)
         


scs <- reverse.levels(likertplots)

section3 <- likert(likertplots3[,c(2:7)], group=scs$Semester)
plot(section3, text.size=4) + theme(strip.text=element_text(size=12)) + theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c('#7b3294', '#c2a5cf','ivory2', '#a6dba0','#008837'), breaks = c("1","2","3","4","5"), name = "Likert Scale")

likertplots4 <- survey[,c(6,34:39)]
likertplots4[] <- lapply(likertplots4, as.factor)
likertplots4$Semester <- factor(likertplots4$Semester, levels = c("Second", "Fourth", "Sixth"))
str(likertplots4)
likertplots4  <- likertplots4  %>% 
  rename('Q23 After participating in SC encounters, how confident do you feel in handling similar situations in real life?' = Q23,
         "Q24 How likely are you to recommend the use of technologically advanced simulations in communication training?" = Q24,
         "Q25 Do you think technology can enhance the SC encounters?" = Q25,
         "Q26 Would you be interested in more technology-enhanced simulations, like virtual reality, in your communication training?" = Q26,
         "Q27 How beneficial do you find the feedback from instructors during the SC encounters?" = Q27,
         "Q28 How beneficial would you consider feedback from AI during the SC encounter?" = Q28)

scs1 <- reverse.levels(likertplots)

section3 <- likert(likertplots4[,c(2:7)], group=scs1$Semester)
plot(section3, text.size=4) + theme(strip.text=element_text(size=12)) + theme(axis.text.y = element_text(size = 12))+ 
  scale_fill_manual(values = c('#7b3294', '#c2a5cf','ivory2', '#a6dba0','#008837'), breaks = c("1","2","3","4","5"), name = "Likert Scale")

likertplots5 <- survey[,c(6,40:47)]
likertplots5[] <- lapply(likertplots5, as.factor)
likertplots5$Semester <- factor(likertplots5$Semester, levels = c("Second", "Fourth", "Sixth"))
str(likertplots5)
likertplots5  <- likertplots5  %>% 
  rename('Q29 Do you think AI can help personalize your veterinary learning experience?' = Q29,
         "Q30 What is your level of comfort with the idea of AI-assisted learning." = Q30,
         "Q31 How do you think AI might change the way you learn in veterinary school?" = Q31,
         "Q32 How willing are you to experiment with AI tools in your learning process?" = Q32,
         "Q33 Do you think AI could help improve your communication skills across veterinary contexts/disciplines?" = Q33,
         "Q34 How do you feel about the possibility of AI providing feedback on the use of your communication skills?" = Q34,
         "Q35 Do you believe AI will play an important role in the future of veterinary education?" = Q35,
         "Q36 Are you concerned about the privacy and/or security of your data when using AI tools?" = Q36)


ai_veted <- reverse.levels(likertplots5)

section4 <- likert(likertplots5[,c(2:9)], group=ai_veted$Semester)
plot(section4, text.size=4) + theme(strip.text=element_text(size=12)) + theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c('#7b3294', '#c2a5cf','ivory2', '#a6dba0','#008837'), breaks = c("1","2","3","4","5"), name = "Likert Scale")
