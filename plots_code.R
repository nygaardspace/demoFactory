# Create a group-means data set
gd <- ALL %>% 
  group_by(`Country of Exchange`) %>% 
  summarise(
    VAR1 = mean(VAR1),
    VAR2 = mean(VAR2),
    ...
  )

# Plot
g <- ggplot(ALL, aes(x =`Country of Exchange`,y= `Full-Time Employees`, group = `Country of Exchange`))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Full time employees plot", 
       subtitle="Full time employees per country",
       x="Country",
       y="Full-time employees")

# Plot
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")