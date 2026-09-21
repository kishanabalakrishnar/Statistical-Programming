#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

yrbss$Grade <- factor(yrbss$grade, levels = c("9", "10", "11", "12", "other"), 
labels = c("9", "10", "11", "12", "Other"))
yrbss$Gender <- factor(yrbss$gender, levels = c("female", "male"),
labels = c("Female", "Male"))

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

aggregate(physically_active_7d ~ Gender + Grade, data = yrbss, FUN = mean) |> 
  ggplot(aes(x = Grade, y = physically_active_7d, group = Gender, color = Gender)) + 
  geom_line() + labs(x = "Grade", y = "Mean Number of Days Physically Active",
title = "Average Number of Physically Active Days By Grade and Gender", color = "Gender")
...


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss |> mutate(bmi = weight / (height^2)) |> filter(Gender == "Female" & Grade == "12") |> 
  ggplot(aes(x = factor(physically_active_7d), y = bmi)) + geom_boxplot() +
  labs(x = "Number of Physically Active Days", y = "Body Mass Index (BMI)",
title = "Relationship Between Physical Active Days and BMI\nAmong Female Students in Grade 12")

# Push your completed code to your GitHub repository
