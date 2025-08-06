#Load Libraries
library(tidyverse)
library(BSDA)
library(entropy)
library(ggpubr)

#Load data
studentdata <- read.csv("https://raw.githubusercontent.com/mikegrand16/USLawSchools-GenderEthnicity/refs/heads/main/Data/AdmissionDataClean.csv")
facultydata <- read.csv("https://raw.githubusercontent.com/mikegrand16/USLawSchools-GenderEthnicity/refs/heads/main/Data/FacultyDataClean.csv")


#Calculate probabilities/percentages for student and faculty
stdata <- studentdata |>
  mutate(percent.STBIPOC = (ST.Total.Bipoc/ST.Total)*100,
         percent.STMen = (ST.Total.Men/ST.Total)*100, #(ST.Total.Men+ST.Total.Women)
         percent.STWomen = (ST.Total.Women/ST.Total)*100, #(ST.Total.Men+ST.Total.Women)
         percent.STAGI = (ST.Total.AGI/ST.Total)*100) |>
  arrange(School.List, Year) |>
  select(-X)

ftdata <- facultydata |>
  mutate(percent.FTBIPOC = (FT.BIPOC/FT.Total)*100,
         percent.FTMen = (FT.Male/FT.Total)*100,
         percent.FTWomen = (FT.Female/FT.Total)*100,
         percent.FTAGI = (FT.AGI/FT.Total)*100) |>
  arrange(School.List, Year) |>
  select(-X)

states <- stdata |>
  group_by(State, Year) |>
  summarise(TotalSchools = n()) |>
  arrange(State, Year)

schools <- stdata |>
  filter(Year == 2017) |>
  select(School.List, State)

fulldata <- stdata |>
  full_join(ftdata[,-9], by = c("School.List" = "School.List", "Year" = "Year"))

AGIdata <- fulldata |>
  filter(ST.Total.AGI > 0) |>
  filter(Year == 2023)
unique(AGIdata$School.List)

PR <- fulldata |>
  filter(State == "PUERTO RICO") |>
  arrange(percent.FTBIPOC, Year, School.List)

Howard <- fulldata |>
  filter(School.List == "HOWARD UNIVERSITY")

fulldata <- fulldata |>
  relocate(c(percent.STBIPOC, percent.FTBIPOC), .before = ST.Hispanic)

change_gender.ST <- read.csv("student_gen_yearly_change.csv")
change_gender.FT <- read.csv("fac_gen_yearly_change.csv")
change_div.ST <- read.csv("student_div_yearly_change.csv")
change_div.FT <- read.csv("fac_div_yearly_change.csv")

full_change <- change_gender.ST |>
  full_join(change_gender.FT, by = c("School.List"= "School.List",
                                     "State" = "State",
                                     "Year" = "Year"))

full_change <- full_change |> 
  full_join(change_div.ST, by = c("School.List"= "School.List",
                                  "State" = "State",
                                  "Year" = "Year",
                                  "ST.Total" = "ST.Total")) |>
  full_join(change_div.FT, by = c("School.List"= "School.List",
                                  "State" = "State",
                                  "Year" = "Year",
                                  "FT.Total" = "FT.Total"))

LC_change <- full_change |>
  filter(School.List == "LEWIS AND CLARK COLLEGE")
 
LC <- fulldata |>
  filter(School.List == "LEWIS AND CLARK COLLEGE") |>
  relocate(c(State, Year, c(19:22, 29:32)), .before = ST.Hispanic)

LC_Full <- LC |>
  full_join(LC_change[,c(1:3, 11:13, 21:23, 28, 33)], by = c("School.List"= "School.List",
                              "State" = "State",
                              "Year" = "Year"))

LC_Full <- LC_Full |>
  relocate(percent.FTBIPOC, .after = percent.STBIPOC)

LC_Full <- LC_Full |>
  relocate(c(39,40), .after = percent.FTBIPOC)
colnames(LC_Full)[c(6,7)] <- c("STBIPOC.year_change", "FTBIPOC.year_change")

LC_Full <- LC_Full |>
  relocate(c(35:40), .before = ST.Hispanic)
colnames(LC_Full)[c(14:19)] <- c("STMen.year_change", "STWomen.year_change", "STAGI.year_change",
                                 "FTMen.year_change", "FTWomen.year_change", "FTAGI.year_change")
#KLD of Lewis and Clark
LC_KLD <- KLD_df |>
  filter(School.List == "LEWIS AND CLARK COLLEGE")
LC_Full <- rbind(LC_Full, c("KL-Divergence Ethnicity", LC_KLD$KL.BIPOC[1], rep(NA, 38)))
LC_Full <- rbind(LC_Full, c("KL-Divergence Gender", LC_KLD$KL.Gender[1], rep(NA, 38)))
  
write.csv(LC_Full, "LewisandClarkData.csv", row.names = FALSE)

#### KLD function
# Smoothing function to deal with zero probabiliies
smooth_proportions <- function(p, epsilon = 1e-10) {
  p_smoothed <- p + epsilon
  p_smoothed / sum(p_smoothed)
}
# Define the KL divergence function
kl_divergence <- function(p, q) {
  p_smoothed <- smooth_proportions(p, epsilon)
  q_smoothed <- smooth_proportions(q, epsilon)
  kl <- KL.empirical(p_smoothed, q_smoothed)
  return(kl)
}
epsilon = 1e-10
fulldata <- fulldata |>
  mutate(pnew.STBIPOC = (percent.STBIPOC+epsilon)/(((1-percent.STBIPOC)+epsilon) + (percent.STBIPOC+epsilon)), 
         pnew.FTBIPOC = (percent.FTBIPOC+epsilon)/(((1-percent.FTBIPOC)+epsilon) + (percent.FTBIPOC+epsilon)))

 
KLD_df <- fulldata |>
  group_by(School.List) |>
  summarise(KL.BIPOC = KL.empirical(pnew.STBIPOC, pnew.FTBIPOC),
            KL.Gender = KL.empirical(c(ST.Total.Women/(ST.Total.Men+ST.Total.Women), ST.Total.Men/(ST.Total.Men+ST.Total.Women)),
                                         c(FT.Female/(FT.Male+FT.Female), FT.Male/(FT.Male+FT.Female))))
#KL.Gender.AGI = kl_divergence(c(percent.STWomen, percent.STMen, percent.STAGI), c(percent.FTWomen, percent.FTMen, percent.FTAGI))

KLD_df <- KLD_df |>
  full_join(schools, by = "School.List")

# Creates csv of KLD results
#write.csv(KLD_df, "Full_KLD_Results.csv", row.names = FALSE)

# Filters out schools from Puerto Rico
KLD_df <- KLD_df |>
  filter(State != "PUERTO RICO") |>
  filter(!(School.List %in% c("IDAHO, UNIVERSITY OF", "NEW HAMPSHIRE, UNIVERSITY OF")))

top_schools <- KLD_df |>
  top_n(10, KL.BIPOC)

s <- top_schools$School.List
c <- bottom_schools$School.List

# Means of gender and ethinicity distributions for top and bottom schools in terms of KLD
df <- fulldata |>
  filter(School.List %in% c(s, c)) |>
  group_by(School.List) |>
  summarise(ST.BIPOC = mean(percent.STBIPOC),
            FT.BIPOC = mean(percent.FTBIPOC),
            ST.Men = mean(percent.STMen),
            FT.Men = mean(percent.FTMen),
            ST.Women = mean(percent.STWomen),
            FT.Women = mean(percent.FTWomen))

bottom_schools <- KLD_df |>
  top_n(-10, KL.BIPOC)

# Function to plot top 10 and bottom 10 KLD results for ethnicity
create_plot <- function(data, color, title_label) {
  ggplot(data, aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
    geom_bar(stat = "identity", fill = color) +
    labs(x = "School", y = "KLD Distance", title = title_label) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
          axis.title = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0, size = 20), 
          legend.position = "none")
}
colors <- c("2017" = "#F8766D", "2018" = "#7CAE00", "2019" = "#00BFC4", "2020" = "#C77CFF", 
            "2021" = "#00BA38", "2022" = "#619CFF", "2023" = "#F564E3")

top_label <- "Top 10 Schools of KLD Distance for Student-Faculty\nEthnicity Profiles"
bottom_label <- "Bottom 10 Schools of KLD Distance for Student-Faculty\nEthnicity Profiles"

top_plot <- create_plot(top_schools, color = "red", top_label)
bottom_plot <- create_plot(bottom_schools, color = "blue", bottom_label)

ggarrange(top_plot + ylim(c(0,0.3)), bottom_plot + ylim(c(0,0.003)),
          widths = c(1, 1),
          nrow = 1, ncol = 2)
bottom_plot + ylim(c(0,0.003))


# KLD by State
KLD_State <- KLD_df |>
  group_by(State) |>
  summarise(AVG.KL.BIPOC = mean(KL.BIPOC),
            AVG.KL.Gender = mean(KL.Gender))

ggplot(KLD_State, aes(x = reorder(State, AVG.KL.BIPOC), y = AVG.KL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[1]) +
  labs(x = "State", y = "KLD Distance", title = "KLD Distance for Student-Faculty Ethnicity Profiles by State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  ylim(c(0,9.7))

ggplot(KLD_State, aes(x = reorder(State, AVG.KL.Gender), y = AVG.KL.Gender)) +
  geom_bar(stat = "identity", fill = colors[1]) +
  labs(x = "State", y = "KLD Distance", title = "KLD Distance for Student-Faculty Gender Profiles by State with AGI included") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none")

male <- ggplot(KLD_State, aes(x = reorder(State, AVG.KL.Male), y = AVG.KL.Male)) +
  geom_bar(stat = "identity", fill = colors[1]) +
  labs(x = "State", y = "KLD Distance", title = "KLD Distance for Male Student-Faculty Gender Profiles by State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none")

ggarrange(fem, male,
          widths = c(1, 1),
          nrow = 2, ncol = 1)

####
# Absoulte difference function
abs_diff <- function(p, q) {
  absdiff <- abs(p - q)
  return(absdiff)
}

# Initialize vectors for KL divergence results
ethnic.kl <- numeric(nrow(stdata))
BIPOC.kl <- numeric(nrow(stdata))
male.kl <- numeric(nrow(stdata))
female.kl <- numeric(nrow(stdata))
agi.kl <- numeric(nrow(stdata))

BIPOC.absdiff <- numeric(nrow(stdata))
female.absdiff <- numeric(nrow(stdata))
male.absdiff <- numeric(nrow(stdata))
agi.absdiff <- numeric(nrow(stdata))

# Iterate over each row to calculate KL divergence for each demographic
for (i in 1:nrow(stdata)) {
  # Minority demographic
  p_ethnic <- c(stdata[i, "percent.STBIPOC"], 1 - stdata[i, "percent.STBIPOC"])
  q_ethnic <- c(ftdata[i, "percent.FTBIPOC"], 1 - ftdata[i, "percent.FTBIPOC"])
  ethnic.kl[i] <- kl_divergence(p_ethnic, q_ethnic)
  BIPOC.kl[i] <- kl_divergence(q_ethnic, p_ethnic)
  
  BIPOC.absdiff[i] <- abs_diff(stdata[i, "percent.STBIPOC"], ftdata[i, "percent.FTBIPOC"])

  # Female
  p_female <- c(stdata[i, "percent.STWomen"], 1 - stdata[i, "percent.STWomen"])
  q_female <- c(ftdata[i, "percent.FTWomen"], 1 - ftdata[i, "percent.FTWomen"])
  female.kl[i] <- kl_divergence(p_female, q_female)
  
  female.absdiff[i] <- abs_diff(stdata[i, "percent.STWomen"], ftdata[i, "percent.FTWomen"])
  
  # Male
  p_male <- c(stdata[i, "percent.STMen"], 1 - stdata[i, "percent.STMen"])
  q_male <- c(ftdata[i, "percent.FTMen"], 1 - ftdata[i, "percent.FTMen"])
  male.kl[i] <- kl_divergence(p_male, q_male)
  
  male.absdiff[i] <- abs_diff(stdata[i, "percent.STMen"], ftdata[i, "percent.FTMen"])
  
  # AGI (Another Gender Identity)
  p_agi <- c(stdata[i, "percent.STAGI"], 1 - stdata[i, "percent.STAGI"])
  q_agi <- c(ftdata[i, "percent.FTAGI"], 1 - ftdata[i, "percent.FTAGI"])
  agi.kl[i] <- kl_divergence(p_agi, q_agi)
  
  agi.absdiff[i] <- abs_diff(stdata[i, "percent.STAGI"], ftdata[i, "percent.FTAGI"])
}

KL_data <- data.frame(School.List = stdata$School.List,
                      State = stdata$State,
                      Year = stdata$Year,
                      KL.BIPOC = ethnic.kl,
                      KL.FTST.BIPOC = BIPOC.kl,
                      AbsDiff.BIPOC = BIPOC.absdiff,
                      percent.STBIPOC = stdata$percent.STBIPOC,
                      percent.FTBIPOC = ftdata$percent.FTBIPOC,
                      KL.Female = female.kl,
                      AbsDiff.female <- female.absdiff,
                      percent.STWomen = stdata$percent.STWomen,
                      percent.FTWomen = ftdata$percent.FTWomen,
                      KL.Male = male.kl,
                      AbsDiff <- male.absdiff,
                      percent.STMen = stdata$percent.STMen,
                      percent.FTMen = ftdata$percent.FTMen,
                      KL.AGI = agi.kl,
                      AbsDiff <- agi.absdiff)
KL_data <- KL_data |>
  filter(State != "PUERTO RICO")
KL_StateData <- KL_data |>
  group_by(State, Year) |>
  summarise(AvgKL.BIPOC = mean(KL.BIPOC),
            AvgKL.FTST.BIPOC = mean(KL.FTST.BIPOC)) |>
  arrange(desc(AvgKL.BIPOC))

# KLD Plots by year
state_title <- "KL Divergence Distances for Ethnicity by State"

plotstate_2017 <- ggplot(subset(KL_StateData, Year == 2017), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[1]) +
  labs(x = "State", y = "KL-Distance", title = paste("2017", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2018 <- ggplot(subset(KL_StateData, Year == 2018), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[2]) +
  labs(x = "State", y = "KL-Distance", title = paste("2018", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2019 <- ggplot(subset(KL_StateData, Year == 2019), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[3]) +
  labs(x = "State", y = "KL-Distance", title = paste("2019", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2020 <- ggplot(subset(KL_StateData, Year == 2020), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[4]) +
  labs(x = "State", y = "KL-Distance", title = paste("2020", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2021 <- ggplot(subset(KL_StateData, Year == 2021), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[5]) +
  labs(x = "State", y = "KL-Distance", title = paste("2021", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2022 <- ggplot(subset(KL_StateData, Year == 2022), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[6]) +
  labs(x = "State", y = "KL-Distance", title = paste("2022", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

plotstate_2023 <- ggplot(subset(KL_StateData, Year == 2023), aes(x = reorder(State, AvgKL.BIPOC), y = AvgKL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[7]) +
  labs(x = "State", y = "KLD Distance", title = paste("2023", state_title)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") + 
  ylim(0,0.5)

ggarrange(plotstate_2021, plotstate_2022, plotstate_2023,
          nrow = 2, ncol = 2)

# KLD ethnicity by school
thresh <- median(KL_data$KL.BIPOC)
ggplot(subset(KL_data, Year == 2023 & KL.BIPOC <= thresh), aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[7]) +
  labs(x = "School", y = "KLD Distance", title = "2023 KL Divergence for Ethnicity. Filters for KLD <= median value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        plot.title = element_text(hjust = 0), 
        legend.position = "bottom")

top_schools <- KL_data |>
  group_by(Year) |>
  top_n(10, KL.BIPOC)

bottom_schools <- KL_data |>
  group_by(Year) |>
  top_n(-10, KL.BIPOC)

# Create plot function for KLD for top and bottom ranked schools by ethnicity
create_plot <- function(data, year, color, type) {
  ggplot(subset(data, Year == year), aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
    geom_bar(stat = "identity", fill = color) +
    labs(x = "School", y = "KLD Distance", title = paste(year, type, "10 Schools of KLD Distance for Student-Faculty\nEthnicity Profiles")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
          plot.title = element_text(hjust = 0), 
          legend.position = "none")
}
top_2017 <- create_plot(top_schools, 2017, colors["2017"], "Top")
bottom_2017 <- create_plot(bottom_schools, 2017, colors["2017"], "Bottom")
top_2018 <- create_plot(top_schools, 2018, colors["2018"], "Top")
bottom_2018 <- create_plot(bottom_schools, 2018, colors["2018"], "Bottom")
top_2019 <- create_plot(top_schools, 2019, colors["2019"], "Top")
bottom_2019 <- create_plot(bottom_schools, 2019, colors["2019"], "Bottom")
top_2020 <- create_plot(top_schools, 2020, colors["2020"], "Top")
bottom_2020 <- create_plot(bottom_schools, 2020, colors["2020"], "Bottom")
top_2021 <- create_plot(top_schools, 2021, colors["2021"], "Top")
bottom_2021 <- create_plot(bottom_schools, 2021, colors["2021"], "Bottom")
top_2022 <- create_plot(top_schools, 2022, colors["2022"], "Top")
bottom_2022 <- create_plot(bottom_schools, 2022, colors["2022"], "Bottom")
top_2023 <- create_plot(top_schools, 2023, colors["2023"], "Top")
bottom_2023 <- create_plot(bottom_schools, 2023, colors["2023"], "Bottom")

top_2017
bottom_2017

ggplot(subset(top_schools, Year == 2017), aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[1]) +
  labs(x = "School", y = "KLD Distance", title = "2017 Top 10 Schools of KLD Distance for Student-Faculty Ethnicity Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0))
top_2018

bottom_plot <- ggplot(subset(bottom_schools, Year == 2021), aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[5]) +
  labs(x = "School", y = "KLD Distance", title = "2021: Bottom 10 Schools of KLD Distance for Student-Faculty Ethnicity Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
        plot.title = element_text(hjust = 0)) +
  ylim(c(0,0.01))

ggarrange(bottom_2017+ylim(c(0,0.0045)), bottom_2018+ylim(c(0,0.0045)),
          widths = c(1, 1),
          nrow = 1, ncol = 2)

quants <- quantile(KL_data$KL.BIPOC, probs = c(.1, .5, .9))

ggplot(subset(KL_data, Year == 2023 & KL.BIPOC >= quants[3]), aes(x = reorder(School.List, KL.BIPOC), y = KL.BIPOC)) +
  geom_bar(stat = "identity", fill = colors[7]) +
  labs(x = "School", y = "KLD Distance", title = "All schools >= 90th percentile 2023. KL Divergence for Ethnicity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
        plot.title = element_text(hjust = 0))

##############
#percent.studentBIPOC vs percent.facultyBIPOC
ggplot(data = pvaldata, aes(x = percent.STBIPOC, percent.FTBIPOC)) +
  geom_point(aes(color = as.factor(Year)), size = 1.75) +
  labs(x = "Proportion of BIPOC students", y = "Proportion of BIPOC faculty",
       color = "Year",
       title = "Proportion of BIPOC faculty vs Proportion of BIPOC students") +
  theme(plot.title = element_text(hjust = 0))

percent_peryear <- pvaldata |>
  group_by(Year) |>
  summarise(Avg.percentSTBIPOC = mean(percent.STBIPOC),
            Avg.percentFTBIPOC = mean(percent.FTBIPOC),
            Avg.percentSTMale = mean(percent.STMen),
            Avg.percentSTMale = mean(percent.STMen),
            Avg.percentSTFemale = mean(percent.STWomen),
            Avg.percentFTFemale = mean(percent.FTWomen))
#plot
ggplot(data = percent_peryear, aes(x = Avg.percentSTBIPOC, y = Avg.percentFTBIPOC)) +
  geom_point(aes(color = as.factor(Year))) +
  labs(x = "Average proportion of student BIPOC", y = "Average proportion of faculty BIPOC",
       color = "Year",
       title = "Average proportion of faculty BIPOC vs average proportion of student BIPOC") +
  theme(plot.title = element_text(hjust = 0.5))


AGI_Schools <- pvaldata |> 
  filter((Significance.AGI == "Yes") | (Significance.AGI == "No"))

Interest <- pvaldata |> 
  filter((Significance.Male == "Yes") & (Significance.Female == "Yes") & (Significance.AGI == "Yes"))

top10 <- pvaldata |> filter(State != "PUERTO RICO") |>
  group_by(Year) |>
  slice_max(percent.STBIPOC, n = 10)


#####
threshold <- pvaldata |> 
  mutate(abs.diff = abs(percent.STBIPOC - percent.FTBIPOC))

ggplot(data = threshold, aes(x = abs.diff)) +
  geom_histogram(color = "black") 

median(threshold$abs.diff)



#Calculate Correlations
total_cor <- cor.test(pvaldata$percent.STBIPOC, pvaldata$percent.FTBIPOC)
year = 2023
cor.test(pvaldata$percent.STBIPOC[pvaldata$Year == year], pvaldata$percent.FTBIPOC[pvaldata$Year == year])

years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

#Initialize an empty list to store correlation matrices
correlation_list <- list()

for (i in 1:length(years)){
  year <- years[i]
  label <- paste0("cordata.", year)
  
  #Calculate the correlation matrix for the current year
  cor_matrix <- cor(fulldata[fulldata$Year == year, c(19,20,21, 29, 30, 31)])
  
  #Assign the correlation matrix to the list with the year as the name
  correlation_list[[as.character(year)]] <- cor_matrix
}

#Convert the list of correlation matrices to a data frame
#Flatten each correlation matrix and combine them into one data frame
correlation_df <- do.call(rbind, lapply(names(correlation_list), function(year) {
  data.frame(Year = year, as.data.frame(as.table(correlation_list[[year]])))
}))

#Function for confidence intervals
confinterval <- function(var1, var2, Year){
  col_one = case_when(var1 == "percent.STBIPOC" ~ 19,
                      var1 == "percent.FTBIPOC" ~ 29,
                      var1 == "percent.STMen" ~ 11,
                      var1 == "percent.FTMen" ~ 30,
                      var1 == "percent.STWomen" ~ 13,
                      var1 == "percent.FTWomen" ~ 31)
  col_two = case_when(var2 == "percent.STBIPOC" ~ 19,
                      var2 == "percent.FTBIPOC" ~ 29,
                      var2 == "percent.STMen" ~ 20,
                      var2 == "percent.FTMen" ~ 30,
                      var2 == "percent.STWomen" ~ 21,
                      var2 == "percent.FTWomen" ~ 31)
  
  test <- cor.test(fulldata[fulldata$Year == year, col_one], fulldata[fulldata$Year == year, col_two])
  confidence_interval <- test[["conf.int"]]
  
  return(confidence_interval)
}

interval1 <- numeric(nrow(correlation_df))
interval2 <- numeric(nrow(correlation_df))

for (i in 1:nrow(correlation_df)){
  var1 <- as.character(correlation_df[i,2])
  var2 <- as.character(correlation_df[i,3])
  year <- as.numeric(correlation_df[i,1])
  
  interval <- confinterval(var1, var2, year)
  
  interval1[i] <- interval[1]
  interval2[i] <- interval[2]
}

correlation_data <- data.frame(Year = correlation_df$Year,
                               Var1 = correlation_df$Var1,
                               Var2 = correlation_df$Var2,
                               Correlation.Estimate = correlation_df$Freq,
                               Confint.Lower = interval1,
                               Confint.Upper = interval2)

#write.csv(correlation_data, "correlation_data.csv", row.names = FALSE)

filter_cordata <- correlation_data |>
  filter(Var1 == "percent.FTMen" & Var2 == "percent.STMen")

total_cor <- cor.test(fulldata$percent.FTMen, fulldata$percent.STMen)

correlation_tableMen <- rbind(filter_cordata, c("Total", "percent.FTMen", "percent.STMen", total_cor$estimate, total_cor$conf.int[1], total_cor$conf.int[2]))

write.csv(correlation_table, "correlation_table_Gender.csv", row.names = FALSE)

#Ethnicity Linear Model
lm.FT <- lm(percent.FTBIPOC ~ percent.STBIPOC, data = pvaldata)

summary(lm.FT)

plot(lm.FT)

require(MASS)
boxcox(lm.FT, plotit=T, lambda = seq(-0.1,0.7, by=0.1))

lambda <- boxcox(lm.FT, plotit=F)$x[which.max(boxcox(lm.FT, plotit=F)$y)]

print(paste("lambda =", lambda))#Root 10 transformation

lm.box.FT <- lm(percent.FTBIPOC^0.1 ~ percent.STBIPOC, data = pvaldata)
summary(lm.box.FT)$adj.r.squared

plot(lm.box.FT)

ggplot(data = pvaldata, aes(x = percent.STBIPOC, percent.FTBIPOC)) +
  geom_point(aes(color = as.factor(Year)), size = 1.5) +
  labs(x = "Proportion of student BIPOC", y = "Proportion of faculty BIPOC",
       color = "Year",
       title = "Proportion of faculty BIPOC vs Proportion of student BIPOC",
       caption = paste0("Adjusted R-squared: ", round(summary(lm.FT)$adj.r.squared, digits = 4))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  geom_smooth(data = subset(pvaldata, percent.FTBIPOC > 0),
              aes(x = percent.STBIPOC, y = percent.FTBIPOC),
              method = "lm", color = "black", se = FALSE, linetype = "dashed", linewidth = 1)

#Gender Linear Models
lm.gender <- lm(percent.FTMen ~ percent.STMen + percent.STWomen, data = pvaldata)

summary(lm.gender)

#Extra
corrdata <- pvaldata |>
  group_by(Year) |>
  summarise(corr.STFT.BIPOC = cor(percent.STBIPOC, percent.FTBIPOC))

corrdata <- pvaldata |>
  group_by(Year) |>
  summarise(corr.STFT.BIPOC = cor(percent.STBIPOC, percent.FTBIPOC),
            corr.STFT.GenderM = cor(percent.STMen, percent.FTMen),
            corr.STFT.GenderF = cor(percent.STWomen, percent.FTWomen),
            corr.STFT.GMBIPOC = cor(percent.STBIPOC, percent.FTMen),
            corr.STFT.GFBIPOC = cor(percent.STBIPOC, percent.FTWomen))


years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

for (i in 1:length(years)){
  label <- paste0("cordata.", years[i])
  
  a <- cor(pvaldata[pvaldata$Year == years[i],8:13])
  
  assign(label, a)
}

library(openxlsx)

years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

# Create a new workbook
wb <- createWorkbook()

for (year in years){
  # Calculate the correlation matrix for the current year
  cor_matrix <- cor(pvaldata[pvaldata$Year == year, c(9,16:19)])
  
  # Add a worksheet for the current year
  addWorksheet(wb, sheetName = as.character(year))
  
  # Write the correlation matrix to the worksheet
  writeData(wb, sheet = as.character(year), cor_matrix, rowNames = TRUE)
}

# Save the workbook to a file
saveWorkbook(wb, "correlation_matrices_v2.xlsx", overwrite = TRUE)

#Number of NAs for AGI pval
sum(is.na(pvaldata$AGI.pval))

prop.test(x = c(1, 0),
          n = c(159, 63),
          alternative = "two.sided")
#Function for p-value of z-test
z_test <- function(ST.Proportion, FT.Proportion, ST.Total, FT.Total) {
  results <- prop.test(x = c(ST.Proportion, FT.Proportion),
                       n = c(ST.Total, FT.Total),
                       alternative = "two.sided")
  return(results$p.value)
}


#Initialize objects to store p-values
ethnic.pvals <- numeric(nrow(stdata))
male.pvals <- numeric(nrow(stdata))
female.pvals <- numeric(nrow(stdata))
agi.pvals <- numeric(nrow(stdata))

#Iterate over rows and calculate p-values
for (i in 1:nrow(stdata)) {
  ethnic.pvals[i] <- z_test(stdata[i, "ST.Total.Bipoc"], ftdata[i, "FT.BIPOC"], stdata[i, "ST.Total"], ftdata[i, "FT.Total"])
  male.pvals[i] <- z_test(stdata[i, "ST.Total.Men"], ftdata[i, "FT.Male"], stdata[i, "ST.Total"], ftdata[i, "FT.Total"])
  female.pvals[i] <- z_test(stdata[i, "ST.Total.Women"], ftdata[i, "FT.Female"], stdata[i, "ST.Total"], ftdata[i, "FT.Total"])
  agi.pvals[i] <- z_test(stdata[i, "ST.Total.AGI"], ftdata[i, "FT.AGI"], stdata[i, "ST.Total"], ftdata[i, "FT.Total"])
}

#AGI
AGIST_df <- stdata |>
  mutate(ST.NotAGI = ST.Total.Men + ST.Total.Women)

AGIFT_df <- ftdata |>
  mutate(FT.NotAGI = FT.Male + FT.Female)

notagi.pvals <- numeric(nrow(AGIST_df))

#Iterate over rows and calculate p-values
for (i in 1:nrow(AGIST_df)) {
  notagi.pvals[i] <- z_test(AGIST_df[i, "ST.NotAGI"], AGIFT_df[i, "FT.NotAGI"], AGIST_df[i, "ST.Total"], AGIFT_df[i, "FT.Total"])
}

#Create data frame for p-values
pvaldata <- data.frame(School.List = stdata$School.List,
                       State = stdata$State,
                       Year = stdata$Year,
                       Ethnicity.pvals = ethnic.pvals,
                       Male.pval = male.pvals,
                       Female.pval = female.pvals,
                       AGI.pval = agi.pvals,
                       NotAGI.pval = notagi.pvals,
                       percent.STBIPOC = stdata$percent.STBIPOC,
                       percent.FTBIPOC = ftdata$percent.FTBIPOC,
                       percent.STMen = stdata$percent.STMen,
                       percent.FTMen = ftdata$percent.FTMen,
                       percent.STWomen = stdata$percent.STWomen,
                       percent.FTWomen = ftdata$percent.FTWomen,
                       percent.STAGI = stdata$percent.STAGI,
                       percent.FTAGI = ftdata$percent.FTAGI,
                       percent.STHispanic = stdata$ST.Hispanic/stdata$ST.Total,
                       percent.STBlack = stdata$ST.Black/stdata$ST.Total,
                       percent.STNativeAmerican = stdata$ST.Native.American/stdata$ST.Total,
                       percent.STHawPacific = stdata$ST.Hawaiian.or.Other.Pacific.Islander/stdata$ST.Total)
#Assign variable showing whether significant p-val or not
pvaldata <- pvaldata |>
  mutate(Significance.Ethnicity = case_when(Ethnicity.pvals <= 0.05 ~ "Yes",
                                            Ethnicity.pvals > 0.05 ~ "No")) |>
  mutate(Significance.Male = case_when(Male.pval <= 0.05 ~ "Yes",
                                       Male.pval > 0.05 ~ "No"),
         Significance.Female = case_when(Female.pval <= 0.05 ~ "Yes",
                                         Female.pval > 0.05 ~ "No"),
         Significance.AGI = case_when(AGI.pval <= 0.05 ~ "Yes",
                                      AGI.pval > 0.05 ~ "No"),
         Significance.NotAGI = case_when(NotAGI.pval <= 0.05 ~ "Yes",
                                         NotAGI.pval > 0.05 ~ "No"))

#Schools with significant p-value for all 7 years
no_changeEth <- pvaldata |> 
  filter(percent.STBIPOC >= 0.5) |>
  group_by(School.List) %>%
  summarize(All_Yes = all(Significance.Ethnicity == "No" & Year %in% 2017:2023)) |>
  filter(All_Yes) |>
  left_join(pvaldata, by = c("School.List")) |>
  select(-All_Yes)

n_distinct(no_changeEth$School.List)

#write.csv(no_changeEth, "NoChange_PvalEthnicity.csv")

match <- pvaldata |>
  filter(State %in% c("DELAWARE", "WYOMING"))

n_distinct(match$School.List[match$Year == 2023])

no_changeMale <- pvaldata |> 
  group_by(School.List) %>%
  summarize(All_Yes = all(Significance.Male == "Yes" & Year %in% 2017:2023)) |>
  filter(All_Yes) |>
  left_join(pvaldata, by = c("School.List")) |>
  select(-All_Yes)

n_distinct(no_changeMale$School.List)

write.csv(no_changeMale, "NoChange_PvalMale.csv")

no_changeFemale <- pvaldata |> 
  group_by(School.List) %>%
  summarize(All_Yes = all(Significance.Female == "Yes" & Year %in% 2017:2023)) |>
  filter(All_Yes) |>
  left_join(pvaldata, by = c("School.List")) |>
  select(-All_Yes)

write.csv(no_changeFemale, "NoChange_PvalFemale.csv")

#Schools with significant ethnicity p-val
Counts <- pvaldata |> 
  group_by(Year, State) |>
  summarise(Ethnicity = sum(Significance.Ethnicity == "No"),
            Male = sum(Significance.Male == "No"),
            Female = sum(Significance.Female == "No"),
            AGI = sum(Significance.NotAGI == "No")) |>
  mutate_at(vars(3:6), ~replace(., is.na(.), 0)) |>
  arrange(State, Year)

#Join Method 1
Counts_Pval <- Counts |>
  full_join(states, by = c("State", "Year"))
#Join Method 2
Counts_Pval <- cbind(Counts, states$TotalSchools)
names(Counts_Pval)[7] <- "TotalSchools"

total_school <- Counts_Pval |>
  group_by(Year) |>
  summarise(Total.Ethnicity = (sum(Ethnicity)/sum(TotalSchools)),
            Total.Male = (sum(Male)/sum(TotalSchools)),
            Total.Female = (sum(Female)/sum(TotalSchools)),
            Total.AGI = (sum(AGI)/sum(TotalSchools)))
#Plots
ggplot(Counts_Pval, aes(x = State, y = (Ethnicity/TotalSchools)*100, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  facet_grid(Year ~ ., scales = "free") +  # Create separate plots for each year
  labs(x = "State", y = "Percent", title = "Percentage of schools in each state with a matching Student-Faculty Ethnicity Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  scale_fill_discrete(name = "Year")

ggplot(Counts_Pval, aes(x = State, y = (Male/TotalSchools)*100, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  facet_grid(Year ~ ., scales = "free") +  # Create separate plots for each year
  labs(x = "State", y = "Percent", title = "Percentage of schools in each state with a signficant p-value for Student-Faculty Male Gender Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  scale_fill_discrete(name = "Year")

ggplot(Counts_Pval, aes(x = State, y = (Female/TotalSchools)*100, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  facet_grid(Year ~ ., scales = "free") +  # Create separate plots for each year
  labs(x = "State", y = "Percent", title = "Percentage of schools in each state with a matching Student-Faculty Female Gender Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  scale_fill_discrete(name = "Year")

#AGI Plot
ggplot(Counts_Pval, aes(x = State, y = (AGI/TotalSchools)*100, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  facet_grid(Year ~ ., scales = "free") +  # Create separate plots for each year
  labs(x = "State", y = "Percent", title = "Percentage of schools in each state with a signficant p-value for Student-Faculty AGI Gender Profiles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  scale_fill_discrete(name = "Year") + 
  ylim(c(0,100))

#Using total schools
#Ethnicity
ggplot(total_school, aes(x = as.factor(Year), y = Total.Ethnicity, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Percentage of US Law Schools with Matching Student-Faculty Ethnicity Profiles") +
  theme(plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  ylim(c(0,1))
#Female
ggplot(total_school, aes(x = as.factor(Year), y = Total.Female, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Percentage of US Law Schools with Matching Student-Faculty Female Gender Profiles") +
  theme(plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  ylim(c(0,1))
#Male
ggplot(total_school, aes(x = as.factor(Year), y = Total.Male, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Percentage of US Law Schools with Matching Student-Faculty Male Gender Profiles") +
  theme(plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  ylim(c(0,1))
#AGI
ggplot(total_school, aes(x = as.factor(Year), y = Total.AGI, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Percentage of US Law Schools with Matching Student-Faculty AGI Gender Profiles") +
  theme(plot.title = element_text(hjust = 0), 
        legend.position = "none") +
  ylim(c(0,1))

#Lag 1 year, FTBIPOC{Year - 1}:STBIPOC{Year}
lagdata <- stdata |>
  filter(Year > 2017) |>
  arrange(School.List, Year) |>
  mutate(YearST = Year) |>
  select(-c(Year, State))
ID <- seq(1:nrow(lagdata))
lagdata <- cbind(ID, lagdata)

df <- ftdata |>
  filter(Year < 2023) |>
  arrange(School.List, Year)
df <- cbind(ID, df)

lagdata <- lagdata |>
  full_join(df, by = c("ID", "School.List"))

lagdata <- lagdata |>
  mutate(Lag.Interval = paste(Year, "-",YearST)) |>
  relocate(Lag.Interval, .after = School.List)

lagdata$Lag.Interval <- as.factor(lagdata$Lag.Interval)

write.csv(lagdata, "lagdata.csv", row.names = FALSE)

cor.test(lagdata$percent.STBIPOC, lagdata$percent.FTBIPOC)

ggplot(data = lagdata, aes(x = percent.STBIPOC, percent.FTBIPOC)) +
  geom_point(aes(color = Lag.Interval), size = 1.75) +
  labs(x = "Proportion of BIPOC students", y = "Proportion of BIPOC faculty",
       color = "Year",
       title = "Proportion of BIPOC faculty vs Proportion of BIPOC students") +
  theme(plot.title = element_text(hjust = 0))
