
# Step 1: Load the dataset
student_data <- read.csv("Data/student_data.csv.csv")

# Step 2: Check the dataset
print("Dataset loaded successfully!")
print(dim(student_data))

# Step 3: Display column names
print(names(student_data))

# Step 4: Display the first 6 records
head(student_data)


# Step 5: Calculate average marks
student_data$Average_Marks <- (
  student_data$Series1_Marks +
    student_data$Series2_Marks
) / 2

# Step 6: Calculate average percentage
student_data$Average_Percentage <-
  (student_data$Average_Marks / 40) * 100

# Step 7: Classify students
student_data$Performance <- ifelse(
  student_data$Average_Percentage >= 80,
  "Top Student",
  ifelse(
    student_data$Average_Percentage >= 50,
    "Average Student",
    "Weak Student"
  )
)

# Step 8: Display the results
head(student_data)

# Step 9: Count students in each category
print(table(student_data$Performance))


# Step 10: Save the processed dataset
write.csv(
  student_data,
  "Data/student_performance_results.csv",
  row.names = FALSE
)

print("Updated dataset saved successfully!")


# Step 11: Create a performance graph

performance_count <- table(student_data$Performance)

png("Graphs/performance_graph.png",
    width = 1000, height = 700)
# PIE CHART: Students in Each Department

department_count <- table(student_data$Department)

png("Graphs/students_in_each_department.png",
    width = 1000, height = 700)

pie(
  department_count,
  main = "Students in Each Department",
  col = rainbow(length(department_count)),
  labels = paste0(
    names(department_count),
    "\n", department_count, " students\n",
    round(100 * department_count / sum(department_count), 1), "%"
  ),
  cex = 0.8
)

dev.off()

# BAR CHART: Student Performance Categories

performance_count <- table(
  factor(
    student_data$Performance,
    levels = c("Top Student", "Average Student", "Weak Student")
  )
)

png("Graphs/student_performance_bar_chart.png",
    width = 1000, height = 700)

barplot(
  performance_count,
  main = "Number of Students by Performance",
  xlab = "Performance Category",
  ylab = "Number of Students",
  col = c("lightgreen", "skyblue", "lightcoral"),
  space = 0.4
)

dev.off()
# LINE GRAPH: Student Performance by Department

department_performance <- table(
  student_data$Department,
  student_data$Performance
)

png("Graphs/performance_by_department_line.png",
    width = 1000, height = 700)

matplot(
  department_performance,
  type = "o",
  pch = c(16, 17, 15),
  lty = 1,
  col = c("green", "blue", "red"),
  xaxt = "n",
  main = "Student Performance Across Departments",
  xlab = "Department",
  ylab = "Number of Students"
)

axis(
  1,
  at = 1:nrow(department_performance),
  labels = rownames(department_performance),
  las = 2
)

legend(
  "topleft",
  legend = colnames(department_performance),
  col = c("green", "blue", "red"),
  pch = c(16, 17, 15),
  lty = 1
)

dev.off()