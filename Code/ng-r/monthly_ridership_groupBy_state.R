library(readxl)
library(tidyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

# Monthly ridership dataset
Monthly_Ridership <- read_excel("Data/raw/April 2023 Complete Monthly Ridership (with adjustments and estimates)_0.xlsx", sheet='Calendar Year UPT')
View(Monthly_Ridership)
str(Monthly_Ridership)
summary(Monthly_Ridership)

# change columns name
df <- Monthly_Ridership %>% 
  rename("UZA_Name" = "UZA Name",
         "NTD_ID" = "NTD ID")

# clean data
df <- df %>% drop_na(NTD_ID)

# split column to get the first State
df[c("UZA_CityName", "UZA_State")] <- str_split_fixed(df$UZA_Name, ',',2)

df_ridership <- df %>%
  separate("UZA_State", c("State_1", "State_2"))

df_ridership <- filter(df_ridership, State_2 != "Non-UZA")

# validate
unique(df_ridership$State_2)

# drop columns that will not be used
df_final <- df_ridership[ , !names(df_ridership) %in% 
      c("NTD_ID","Legacy NTD ID","Agency", "Status", "Reporter Type", "UZA", "UZA_Name", "Mode", "TOS", "UZA_CityName", "State_1", "2023" )]

str(df_final)

# Group by sum 
# Sum on all columns except group by columns
df_State <- df_final %>% group_by(State_2) %>% 
  summarise(across(everything(), sum),
            .groups = 'drop')  %>%
  as.data.frame()
View(df_State)

# plot graph by state
data_long <- melt(df_State, id = "State_2")
data_long_2 <- filter(data_long, value != 0)
data_long_2 <- filter(data_long, State_2 != "NY")

data_long_2_short <- data_long_2[ , !names(data_long_2) %in% 
                            c("2002","2003")]

data_long_2_short %>%
  ggplot( aes(x=variable, y=value, group=State_2, color=State_2)) +
  geom_line()

# read state file
usa_state_code <- read_excel("Data/USA_State.xlsx")
view(usa_state_code)

# join tables
df_State_2022 <- df_State[,-2:-21]
usa_state_code_simplified <- usa_state_code[,-1]

usa_state_code_simplified <- usa_state_code_simplified %>% 
  rename("State_2" = "Code")

df_State_2022 <- df_State_2022 %>% 
  rename("ridership_2022" = "2022")

# Perform a left join
combined_tbl <- merge(x = df_State_2022, y = usa_state_code_simplified, by = "State_2", all.x = T)

combined_tbl_2 <- combined_tbl %>% drop_na(Population)
combined_tbl_2 <- filter(combined_tbl_2, ridership_2022 != 0)

combined_tbl_2$percentage_ridership <- combined_tbl_2$ridership_2022 / combined_tbl_2$Population

combined_tbl_2 <- combined_tbl_2[order(combined_tbl_2$percentage_ridership,decreasing=TRUE),]
head(combined_tbl_2,10)

