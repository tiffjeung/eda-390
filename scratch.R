
## load packages
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

## read-in data -- names below

two_three <- read_csv("data/2012-13.csv")
three_four <- read_csv("data/2013-14.csv")
four_five <- read_csv("data/2014-15.csv")
five_six <- read_csv("data/2015-16.csv")
six_seven <- read_csv("data/2016-17.csv")
seven_eight <- read_csv("data/2017-18.csv")

# this gives the NUMBER of unique values in each. probably want to apply this to the merged set.
f_or_f <- two_three %>% summarise_all(funs(n_distinct(.)))

#### ----- ####

## merge datasets
# 2,378,008 observations
# how many NA dates are there? none. 
total_print_test <- bind_rows(two_three, three_four, four_five, five_six, six_seven, seven_eight) %>% 
  clean_names(case = "snake")

# parse dates as dates
total_print_test <- total_print_test %>% 
  mutate(print_date = mdy_hm(date)) %>% 
  select(-date)

# check for distinct values
# total_print_test %>% 
#   select(use_type) %>% distinct()

#### --- ####

## what to keep, what to throw away

##### USE TYPE #####

# Copies make up 26920, while printing jobs are 2351088, which is about 1.1 percent of the jobs. 
# Will discard because we're concerned about printing.
# total_print_test %>% 
#   group_by(use_type) %>% count()

total_print_test <- total_print_test %>% 
  filter(use_type == "PRINT") 

##### USER OFFICE #####

## who is doing the printing? 

# total_print_test %>% 
#   group_by(user_office) %>% count()
 
## there's a lot of NA's. let's remove those first 
#(while noting that there are 60113 of them)
# leaves us with 2290975 observations
total_print_test <- total_print_test %>% 
  filter(!is.na(user_office))

# total_print_test %>% group_by(user_office) %>% count()

## What's the comparison of students versus not?
## students make up the vast majority of the counts. 

## Let's see a plot

# okay. should go back and add labels.
user_office_plot <- total_print_test %>% 
  group_by(user_office) %>%
  mutate(n = n()) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(user_office, n))) +
  labs(title = "Type of Users Printing at NU",
        x = "User Office",
        y = "Count of Print Jobs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# remove non-undergrads.
total_print_test <- total_print_test %>% 
  ## kept faculty
  filter(user_office %in% c("faculty", "student"))

faculty_students_plot <- total_print_test %>% 
  group_by(user_office) %>% 
  mutate(n = n()) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(user_office, n))) +
  labs(title = "Faculty vs Student Print Jobs",
       x = "User Office",
       y = "Count of Print Jobs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)
  

##### PRINTER SERVER #####

# care about undergarduate population
# the evanston campus makes up the vast majority of the printing population.
# note that we cannot guarantee that it's undergraduates, but we're likely not at the chicago campus, so taking those out.

## bar graph of printer servers
# total_print_test %>% group_by(printer_server) %>% 
#   ggplot() +
#   geom_bar(mapping = aes(x = printer_server))

# select only locations where undergrads may print
# leaves us with 1793658 observations.
total_print_test <- total_print_test %>% 
  filter(printer_server %in% c("ev-print", "saf-print"))

##### PRINTER NAME #####
## looking at the most popular printers
all_top_printers <- total_print_test %>%
  group_by(printer_name) %>%
  count() %>%
  arrange(desc(n))

##### SIZE KB #####
# don't really care about file size to answer question. let's remove it.
total_print_test <- total_print_test %>% 
  select(-size_kb)

## at this point, have removed the non-undergrad servers and the non-undergrad printers. 
## the remaining conclusions should apply to the undergrad population (ish)




##### TOTAL PRINTED PAGES #####
# note that total printed pages doesn't mean literal sheets: the digital equivalent. 
# actual length + sheets is dependent on duplex/simplex. 
# make a variable called sheets_used?

total_print_test <- total_print_test %>% 
  mutate(sheets_used = ceiling(duplex_pages / 2) + simplex_pages)

# total pages: 6447623

##### DUPLEX PAGES #####

pages_only <- total_print_test %>% 
  select(total_printed_pages, duplex_pages, simplex_pages, sheets_used, print_date)

## summary stats of printing jobs by year
sum_stat_pages <- pages_only %>% 
  mutate(year_printed = year(print_date)) %>% 
  group_by(year_printed) %>% 
  summarise(mean_sheets = mean(sheets_used),
            min_sheets = min(sheets_used),
            # so the maximum print job == 606
            max_sheets = max(sheets_used),
            median_sheets = median(sheets_used),
            sd_sheets = sd(sheets_used)) 

# ggplot(sum_stat_pages, aes(x = as.factor(year_printed))) +
#   geom_boxplot(aes(
#     lower = mean_sheets - sd_sheets, 
#     upper = mean_sheets + sd_sheets, 
#     middle = mean_sheets, 
#     ymin = mean_sheets - 3*sd_sheets, 
#     ymax = mean_sheets + 3*sd_sheets),
#     stat = "identity") 

# this as a box plot? sum_stat_pages

# bar graph (facet wrap by year) of avg printing distribution

# this is too large of a range. look at the 25 and 75th quartiles?
complete_density_plot <- pages_only %>% 
  ggplot() +
  geom_density(aes(x = sheets_used)) +
  geom_vline(aes(xintercept = mean(sheets_used)), color = "blue", linetype = "dashed")

density_by_yer <- pages_only %>% 
  mutate(yer = as.factor(year(print_date))) %>% 
  ggplot() +
  geom_density(aes(x = sheets_used)) +
  geom_vline(aes(xintercept = mean(sheets_used)), color = "blue", linetype = "dashed") +
  coord_cartesian(xlim = c(0, 6)) +
  facet_wrap(~yer, nrow = 2) +
  labs(title = "Average Sheets Used per Job by Year",
       x = "Number of Sheets",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# five num stats. 
# minimum 0, lower hinger 1, median 2, upper hinge 4, max 606
basic_sum_stats <- fivenum(pages_only$sheets_used, na.rm = TRUE)

## graph with trend line of mean sheets
# bar chart with a line?
sum_stat_sheets <- pages_only %>% 
  mutate(year_printed = factor(year(print_date))) %>% 
  group_by(year_printed) %>% 
  summarise(mean_sheets = mean(sheets_used),
            min_sheets = min(sheets_used),
            # so the maximum print job == 606
            max_sheets = max(sheets_used),
            median_sheets = median(sheets_used)) %>% 
  ggplot() +
  geom_bar(aes(year_printed, mean_sheets), stat = "identity") +
  geom_point(aes(year_printed, max_sheets)) +
  labs(title = "Sheets Used per Job by Year",
       x = "Year",
       y = "Average Number of Pages") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
  #annotate("text", x = 2014, y = 550, label = "max")
sum_stat_sheets

mean_sheets_year <- pages_only %>% 
  mutate(year_printed = factor(year(print_date))) %>% 
  group_by(year_printed) %>% 
  summarise(mean_sheets = mean(sheets_used),
            min_sheets = min(sheets_used),
            # so the maximum print job == 606
            max_sheets = max(sheets_used),
            median_sheets = median(sheets_used)) %>% 
  ggplot() +
  geom_bar(aes(year_printed, mean_sheets), stat = "identity") +
  labs(title = "Average Sheets per Job by Year",
       x = "Year",
       y = "Average Number of Pages") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
mean_sheets_year

total_sheets_count <- pages_only %>% 
  mutate(yr_printed = factor(year(print_date))) %>% 
  group_by(yr_printed) %>% 
  summarise(total_sheets = sum(sheets_used)) %>% 
  ggplot() +
  geom_bar(aes(yr_printed, total_sheets), stat = "identity") +
  labs(title = "Total Sheets Used per Year",
       x = "Number of Sheets",
       y = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(labels = comma)




# sheets_year <- pages_only %>% 
#   mutate(year_printed = factor(year(print_date))) %>% 
#   group_by(year_printed) %>% 
#   ggplot(aes(year_printed, n)) +
#   geom_boxplot() +
#   labs(title = "Average Printing Jobs by Year",
#        x = "Year",
#        y = "Average Number of Pages") + 
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# sheets_year

##### SIMPLEX PAGES #####

##### COLOR PAGES ESTIMATED #####
## Binary, hard to really say how much ink, or how many color pages were used.
## Does the school have printer ink data?

# Graph of color versus not, of the student populus. 
yes_color_pts <- total_print_test %>% 
  filter(color_pages_estimated %in% "Y")

color_bar_plot <- total_print_test %>%
  ggplot() +
  geom_bar(mapping = aes(x = color_pages_estimated)) + 
  geom_bar(data = yes_color_pts, aes(x = color_pages_estimated), fill = '#b6acee', alpha = 0.025) +
  labs(title = "Color Printing",
       x = "Color",
       y = "Number of Print Jobs") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Actual percentage?
percent_color_ct <- total_print_test %>% group_by(color_pages_estimated) %>%
  summarise(n = n()) %>% 
  # this should work. idk why it's not.
  mutate(percent = (n / sum(n)) * 100)

color_none <- total_print_test %>% 
  filter(color_pages_estimated %in% "N") %>% 
  mutate(color_year = year(print_date)) %>% 
  group_by(color_year) %>% 
  summarise(n = n())

color_yay <- total_print_test %>% 
  filter(color_pages_estimated %in% "Y") %>% 
  mutate(color_year = year(print_date)) %>% 
  group_by(color_year) %>% 
  summarise(n = n())

color_year <- ggplot() +
  geom_point(data = color_none, aes(color_year, n), color = "red") +
  geom_point(data = color_yay, aes(color_year, n), color = "blue") +
  labs(title = "Color Printing by Year",
       x = "Color",
       y = "Number of Print Jobs") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

color_year_full <- total_print_test %>% 
  mutate(color_yr = year(print_date),
         color_pages_estimated = factor(color_pages_estimated)) %>% 
  group_by(color_yr, color_pages_estimated) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_point(aes(color_yr, n)) +
  labs(title = "Color Printing by Year",
       x = "Color",
       y = "Number of Print Jobs") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

##### PAPER SIZE #####
# This includes the plotter
# 
# total_print_test %>% 
#   group_by(paper_size) %>% count()

##### PRINT DATE #####

time_test <- total_print_test

# Parse out specific time measures
# Note that the quarter function follows fiscal quarters. Label accordingly
# fall: october, november, decmeber.
# winter: january, feb, march
# spring: april, may, mid june
# summer: (will kind of go through august but not completely)
time_test <- time_test %>%
  mutate(yr = year(print_date),
         mnth = month(print_date, label = TRUE),
         dy = day(print_date),
         day_of_week = wday(print_date, label = TRUE),
         hur = hour(print_date),
         mnte = minute(print_date),
         ## note: Q1 is winter quarter, Q2 is spring quarter, Q4 is fall quarter 
         # SO need to adjust 1 as 2, 2 as 3, 4 as 1
         qtr = quarter(print_date))

# We're actually increasing. Are we on pace to increase this year?
annual_print_plot <- time_test %>% 
  mutate(yr = factor(yr)) %>% 
  group_by(yr) %>%
  ggplot() +
  geom_bar(mapping = aes(x = yr)) + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, family = "sans"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  labs(title = "Printing Jobs By Year",
       x = "Year",
       y = "Total Print Jobs")


## Day of the week
# Monday has the most (probably from student work on the weekends).
week_day_pts <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  group_by(day_of_week)

dow_all_year_totals <- time_test %>% 
  group_by(day_of_week) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = day_of_week)) + 
  geom_bar(data = week_day_pts, mapping = aes(x = day_of_week), fill = '#5b3b8c') +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  labs(title = "Printing by Day of the Week",
       x = "Day of Week",
       y = "Total Print Jobs")
dow_all_year_totals




## Per year, per quarter
time_qtrs <- time_test

time_qtrs <- time_qtrs %>% 
  group_by(yr, qtr) %>% 
  summarise(n = n()) %>% 
  # create yr_qtr variable for the sake of graphing by quarter
  unite("yr_qtr", c(yr, qtr), sep = "-", remove = FALSE)

# graph of printed pages per quarter
# time_qtrs %>% 
#   ggplot(aes(yr_qtr, n)) +
#   geom_point()

all_qtr_printing <- time_qtrs %>% 
  ggplot(aes(x = yr_qtr, y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Printing by Quarter (2012-2018)",
       x = "Quarter",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 16))

# overall time of day, based on 2012-2018.
by_hour_plot <- time_test %>% 
  group_by(hur) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hur, y = n)) +
  geom_rect(xmin = 9, xmax = 17, ymin = -Inf, ymax = Inf, fill = '#b6acee', alpha = 0.025) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0:23)) +
  theme_minimal() +
  labs(title = "Printing by the Hour (2012-2018)",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))

# Time of day, but by year. Make year a factor to graph properly.
# General trend aross the years is that people print most commonly around noon.
# This is without considering day of the week.

day_split_by_year_plot <- time_test %>% 
  mutate(yr = as.factor(yr)) %>% 
  group_by(hur, yr) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_line(aes(x = hur, y = n, color = yr)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by Time of Day, Separated by Year",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_discrete(name = "Year")


## Business days only
business_days_hour_plot <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  mutate(yr = as.factor(yr)) %>% 
  group_by(hur, yr) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_line(aes(x = hur, y = n, color = yr)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing on Weekdays",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 14)) + 
  scale_color_discrete(name = "Year")



# highlight 20-29, then 50-59 due to class periods
all_year_min <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  mutate(mnte = as.factor(mnte)) %>% 
  group_by(mnte) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_rect(xmin = 20, xmax = 29, ymin = -Inf, ymax = Inf, fill = '#b6acee', alpha = 0.025) +
  geom_rect(xmin = 50, xmax = 59, ymin = -Inf, ymax = Inf, fill = '#b6acee', alpha = 0.025) +
  geom_point(aes(mnte, n)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute (weekdays)",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12))


## Is this different on different days of the week?
## days of the week -- on the same graph.
# this is interesting but also confusing? so let's facet it out to see the 
# best time PER DAY to print
minute_print_by_day <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  mutate(minmin = as.factor(mnte)) %>% 
  group_by(minmin, day_of_week) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_point(aes(minmin, n, color = day_of_week)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Day of the Week")



dow_faceted_minutes <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  # removed factor because the graphs are illegible if you put them all together
  #mutate(minminmin = as.factor(mnte)) %>% 
  group_by(mnte, day_of_week) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnte, n)) +
  geom_point() +
  facet_wrap(~ day_of_week) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

dow_faceted_minutes

## Individual monday minutes
monday_minutes <- time_test %>% 
  filter(day_of_week %in% c("Mon")) %>% 
  # removed factor because the graphs are illegible if you put them all together
  mutate(minminmin = as.factor(mnte)) %>% 
  group_by(minminmin) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(minminmin, n)) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Individual friday minutes
friday_minutes <- time_test %>% 
  filter(day_of_week %in% c("Fri")) %>% 
  # removed factor because the graphs are illegible if you put them all together
  mutate(minminmin = as.factor(mnte)) %>% 
  group_by(minminmin) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(minminmin, n)) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Compare monday tuesday wednesday thursday (bc square) printing.
four_day_min <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu")) %>% 
  # removed factor because the graphs are illegible if you put them all together
  #mutate(minminmin = as.factor(mnte)) %>% 
  group_by(mnte, day_of_week) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnte, n)) +
  geom_point() +
  facet_wrap(~ day_of_week, nrow = 2) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

thurs_fri_minutes <- time_test %>% 
  filter(day_of_week %in% c("Thu", "Fri")) %>% 
  # removed factor because the graphs are illegible if you put them all together
  #mutate(minminmin = as.factor(mnte)) %>% 
  group_by(mnte, day_of_week) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnte, n)) +
  geom_point() +
  facet_wrap(~ day_of_week, nrow = 1) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing by the Minute",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Peak hours are 10 - 3. let's look at minutes for those hours.
# Want to maintain hours and minutes, maybe day of week.
# See separate hour by hour.
# Closer look at peak times by hour
peak_hour_boxplots <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
         hur %in% c(10, 11, 12, 13, 14, 15)) %>% 
  mutate(hur = as.factor(hur)) %>% 
  group_by(mnte, hur) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_boxplot(aes(hur, n)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Peak Printing Hours",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Morning boxplot
# At 8 am, the traffic is still relatively low, but does pick up by 9.
morning_hour_boxplots <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
         hur %in% c(7, 8, 9)) %>% 
  mutate(hur = as.factor(hur)) %>% 
  group_by(mnte, hur) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_boxplot(aes(hur, n)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Morning Printing Hours",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Try printing around 8pm. 
afternoon_evening_boxplots <- time_test %>% 
  filter(day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
         hur %in% c(16, 17, 18, 19, 20, 21, 22, 23)) %>% 
  mutate(hur = as.factor(hur)) %>% 
  group_by(mnte, hur) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_boxplot(aes(hur, n)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Afternoon and Evening Printing Hours",
       x = "Hour",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# let's look at the minute by minute specifically of those, now that we've looked at general minute to minute trends?
# 8 am -- basically get there when the library starts. 
eight_am_printing_minutes <- time_test %>% 
  filter(hur %in% c(8)) %>% 
  mutate(mnte = as.factor(mnte)) %>% 
  group_by(mnte) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnte, n)) + 
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing from 8-9 a.m.",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 14))

# okay no real pattern
seven_pm_printing_minutes <- time_test %>% 
  filter(hur %in% c(19)) %>% 
  mutate(mnte = as.factor(mnte)) %>% 
  group_by(mnte) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnte, n)) + 
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Printing between 7-8 p.m.",
       x = "Minute",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 13))


## Weekends only? little bit different, less clear patterns.
# time_test %>% 
#   filter(day_of_week %in% c("Sat", "Sun")) %>% 
#   mutate(yr = as.factor(yr)) %>% 
#   group_by(hr, yr) %>%
#   summarise(n = n()) %>%
#   ggplot() +
#   geom_line(aes(x = hr, y = n, color = yr))



# april paperLESS analysis.
# Does april as a month have any less printing?
# If it has an effect, maybe the months AFTER will have fewer printing. but hard to say because we only have two months? may and then june (ish)
apr_pts <- time_test %>% 
  filter(mnth %in% "Apr") %>% 
  group_by(mnth, yr) %>% 
  summarise(n = n())

# Average across months
# Highlight april.
avg_by_month_april_red <- time_test %>% 
  group_by(mnth, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnth, n)) + 
  geom_point() +
  geom_point(data = apr_pts, aes(mnth, n), color = "red") +
  facet_wrap(~ yr, ncol = 1) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Printing by Month",
       x = "Month",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14))

# The difference between april and may specifically? 
mar_apr_may <- time_test %>% 
  filter(mnth %in% c("Mar", "Apr", "May")) %>% 
  group_by(mnth, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mnth, n)) +
  geom_point() +
  facet_wrap(~ yr, ncol = 1) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(title = "March, April, May",
       x = "Month",
       y = "Total Print Jobs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

mar_apr_may


