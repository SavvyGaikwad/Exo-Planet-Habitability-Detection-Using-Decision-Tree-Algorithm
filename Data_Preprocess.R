# Step 1: Read the CSV file
data <- read.csv("C:\\Users\\hp\\OneDrive\\Desktop\\Study Material\\DS\\CP_DATASET.csv")

# Step 2: Remove prefix symbols from numeric columns and convert to numeric
numeric_cols <- c("Mass", "Radius", "Flux", "Tsurf", "Period", "Distance", "Age", "ESI")

for (col in numeric_cols) {
  data[[col]] <- as.numeric(gsub("[^0-9.]", "", data[[col]]))
}

# Step 3: Handle missing values using mean
for (col in numeric_cols) {
  if (any(is.na(data[[col]]))) {
    if (is.numeric(data[[col]])) {
      # Replace missing values with mean
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
  }
}

# Step 4: Convert "ESI" column into proper numeric format
data$ESI <- as.numeric(gsub("[^0-9.]", "", data$ESI))

# Step 5: Calculate the "Habitable" column
data$Habitable <- ifelse(data$ESI >= 0.29, "Yes", "No")

# Step 6: Write the modified data to a new CSV file
write.csv(data,"C:\\Users\\hp\\OneDrive\\Desktop\\Study Material\\DS\\new_data.csv", row.names = FALSE)
