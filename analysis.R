install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

hiVotes_data <- read.csv(file = "csv/hiVotes.csv", header = TRUE)
scoreVote_data <- read.csv(file = "csv/scoreVotes.csv", header = TRUE)
scoreMeta_data <- read.csv(file = "csv/scoreMetadata.csv", header = TRUE)
companyMeta_data <- read.csv("csv/companyMetadata.csv")

wellbeing_questions <- subset(scoreMeta_data, name == "Wellbeing")
wellbeing_questionIds <- wellbeing_questions$questionId
wellbeing_votes <- scoreVote_data[scoreVote_data$questionId %in% wellbeing_questionIds,]

stress_questions <- subset(scoreMeta_data, question == "On a scale from 1 to 10, how would you rate the work-related stress?")
stress_questionId <- stress_questions$questionId
stress_votes <- subset(scoreVote_data, questionId %in% stress_questionId)

mean_hivotes <- mean(hiVotes_data$hiVote, na.rm = TRUE)
mean_scoreVotes <- mean(scoreVote_data$scoreVote, na.rm = TRUE)
mean_wellbeing_vote <- mean(wellbeing_votes$scoreVote, na.rm = TRUE)
mean_stress_vote <- mean(stress_votes$scoreVote, na.rm = TRUE)

# Compute the standard deviation of the 'hivotes' column
sd_hivotes <- sd(hiVotes_data$hiVote, na.rm = TRUE)
sd_scoreVotes <- sd(scoreVote_data$scoreVote, na.rm = TRUE)
sd_wellbeing_vote <- sd(wellbeing_votes$scoreVote, na.rm = TRUE)
sd_stress_vote <- sd(stress_votes$scoreVote, na.rm = TRUE)

# Plot the top 10 most frequent industries
top_industries <- companyMeta_data %>%
  filter(industry != "") %>% 
  group_by(industry) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(10, Count)

ggplot(top_industries, aes(x = reorder(industry, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top 10 Industries by Number of Companies",
       x = "Industry",
       y = "Number of Companies") +
  theme(axis.text.x = element_text(angle = 80, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14))

# Box plots showing distribution of scoreVote by the different score categories 
score_cate_data <- scoreVote_data %>%
  left_join(scoreMeta_data, by = "scoreId")

ggplot(score_cate_data, aes(x = name, y = scoreVote)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Score Votes by Score Category",
       x = "Score Category",
       y = "Score Vote") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14))

# Find company that has highest average "Wellbeing" score
wellbeing_scoreId <- subset(scoreMeta_data, name == "Wellbeing")$scoreId
wellbeing_scoreId_votes <- scoreVote_data %>%
  filter(scoreId %in% wellbeing_scoreId)

average_wellbeing <- wellbeing_scoreId_votes %>%
  left_join(companyMeta_data, by = "companyId") %>%
  group_by(companyId) %>%
  summarise(AverageWellbeingScore = mean(scoreVote, na.rm = TRUE)) %>%
  arrange(desc(AverageWellbeingScore))

top_company <- head(average_wellbeing, 1)
print(top_company)

# Compare Happiness Index (hiVote) between the two industries ARTS_ENTERTAINMENT_RECREATION and FINANCIAL_SERVICES_INSURANCE
mergedCompanyVote_data <- merge(companyMeta_data, hiVotes_data, by = "companyId")
artsEntertainment <- subset(mergedCompanyVote_data, industry == "ARTS_ENTERTAINMENT_RECREATION")$hiVote
financialServices <- subset(mergedCompanyVote_data, industry == "FINANCIAL_SERVICES_INSURANCE")$hiVote

# Conduct a t-test
tTestResult <- t.test(artsEntertainment, financialServices)

# Extract and print the means and p-value
meanArtsEntertainment <- mean(artsEntertainment, na.rm = TRUE)
meanFinancialServices <- mean(financialServices, na.rm = TRUE)
pValue <- tTestResult$p.value

print(paste("Mean hiVote for ARTS_ENTERTAINMENT_RECREATION:", meanArtsEntertainment))
print(paste("Mean hiVote for FINANCIAL_SERVICES_INSURANCE:", meanFinancialServices))
print(paste("p-value:", pValue))

# Find the company that has the "happiest" employees
votes_with_theme <- merge(scoreVote_data, scoreMeta_data, by = "scoreId")

full_data <- merge(votes_with_theme, companyMeta_data, by = "companyId")

mean_scores <- full_data %>%
  group_by(companyId, name) %>%
  summarise(meanScore = mean(scoreVote, na.rm = TRUE)) %>%
  pivot_wider(names_from = name, values_from = meanScore)

model_data <- mean_scores %>%
  mutate(happinessScore = (`Employee Net Promoter Score (eNPS).` + Relationships + Feedback + Alignment + Wellbeing + `Reward & Recognition`) / 7)

happiest_company <- model_data %>%
  arrange(desc(happinessScore)) %>%
  top_n(1, happinessScore) %>%
  select(companyId, happinessScore) %>%
  left_join(companyMeta_data, by = "companyId")  # Assuming companyMeta contains company names

print(happiest_company)

# whether significant differences in mean hiVote exist across industries
score_industry_merged <- merge(scoreVote_data, companyMeta_data, by = "companyId")
mean_score_by_industry <- score_industry_merged %>%
  group_by(industry) %>%
  summarise(MeanScoreVote = mean(scoreVote, na.rm = TRUE))
result_anova <- aov(scoreVote ~ industry, data = score_industry_merged)
summary_result <- summary(result_anova)
print(summary_result)

# Find the relationship between industry and hiVote
vote_companyData <- merge(companyMeta_data, hiVotes_data, by = "companyId")
vote_companyData$industry <- as.factor(vote_companyData$industry)
vote_companyData$hiVote <- as.numeric(vote_companyData$hiVote)
industrySummary <- vote_companyData %>%
  group_by(industry) %>%
  summarise(
    count = n(),
    mean_hiVotes = mean(hiVote, na.rm = TRUE),
    sd_hiVotes = sd(hiVote, na.rm = TRUE)
  )
print(industrySummary)
anovaResult <- aov(hiVote ~ industry, data = vote_companyData)
summary(anovaResult)

ggplot(vote_companyData, aes(x=industry, y=hiVote)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + # Rotate x labels for better readability
  labs(title = "Distribution of hiVotes by Industry", x = "Industry", y = "hiVotes")

full_data$timezone <- as.factor(full_data$timezone)

lm_model <- lm(scoreVote ~ timezone, data = full_data)
summary(lm_model)


