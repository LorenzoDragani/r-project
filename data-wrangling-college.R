library(tidyverse)
library(janitor)
library(ggrepel)


college_data <- read_csv("College.csv") |> 
  clean_names()

college_data |> 
  glimpse()

college_data |> 
  mutate(name = x1) |> 
  select(private:last_col()) |>
  glimpse()

colleges <- college_data |> 
  mutate(accept_rate = round((accept/apps)*100)) |> 
  select(private:top10perc, personal, s_f_ratio, perc_alumni:accept_rate) |> 
  group_by(private) |> 
  summarize(avg_apps = mean(apps), avg_accept = mean(accept), 
            avg_accept_rate = mean(accept_rate),avg_expenditure = mean(expend), 
            avg_grad_rate = mean(grad_rate),avg_top10 = mean(top10perc), 
            avg_num_apps = mean(apps), avg_s_f_ratio = mean(s_f_ratio),
            n = n())

college_data <- college_data |> 
  mutate(most_expensive = case_when(
  expend > 55000 & private == "Yes" ~ "Most expensive private college\n (Johns Hopkins University)",
  expend == 16527 & private == "No" ~ "Most expensive public college\n (University of Washington)",
  .default = " "
    )
)

## Data Visualization 

ggplot(data = college_data,
       mapping = aes(x = expend,
                     y = grad_rate,
                     color = private)) +
  geom_point() +
  scale_color_manual(values = c("blue", "orange")) +
  labs(title = "U.S. News and World Report’s College Data from 1995",
       x = "Instructional expenditure per student",
       y = "Graduation Rate",
       color = "Private") +
  scale_x_continuous(limits = c(0,60000),
                     breaks = seq(0,60000,5000),
                     labels = scales::dollar) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  geom_text(aes(label=most_expensive)) +
  theme_light() +
  theme(panel.border = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.position="top",
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=10)), 
        legend.text = element_text(margin=margin(0,10,0,0)),
        plot.title = element_text(face="bold"))

# expensive_public <- college_data |> filter(private=="No" & expend > 16000 ) |> select(x1, expend)
# expensive_private <- college_data |> filter(private=="Yes" & expend > 55000 ) |> select(x1, expend)