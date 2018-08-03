# # Install packages 
# install.packages("readr")
# install.packages("dplyr")
# install.packages("janitor")
# install.packages("psychometric")
# install.packages("ggplot2")
# install.packages("lme4")
# install.packages("broom")
# install.packages("lmerTest")

# Analysis start
library(readr)
library(dplyr)
library(janitor)
df1 <-  read_csv("baseline.csv") %>% 
  clean_names() %>% 
  mutate(id = toupper(id),    # consistently case IDs
         item1r = 11 - item1, # reverse scoring
         scale1 = (item1r + item2 + item3 + item4) / 4) # creating a summary scale
          
head(df1)

## ----longitudinal, echo = TRUE, warning = FALSE, message = FALSE---------
df2 <-  read_csv("esm.csv") %>% 
  mutate(id=toupper(id)) %>%  # consistently case IDs
  group_by(id) %>%            # groups by ID (also possible to group by day here)
  arrange(id, surv_time) %>%  # sort by ID and time of survey 
  mutate(ximean = mean(xi),   # obtain the group mean of IV xi (groupmean because of grouping above)
         xic = xi - ximean,   # group mean centre scores on xi
         xiclag = lag(xic))   # lag centred xi by 1 observation

head(df2)

## ----combining, echo = TRUE----------------------------------------------
full <-  inner_join(df2, df1, by = "id")
head(full)

## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
library(ggplot2)
df <-  tribble(
  ~grp, ~x, ~y,
     1,   0,   0,  
     1,   10, 10,
     2,   0,  2, 
     2,   10,  12,
     3,   0,  0,  
     3,   10,  12 
)

plot1 <- df %>% 
  filter(grp == 1) %>% 
  ggplot(aes(x, y)) + 
  theme_classic() +
  ggtitle("Linear Regression Model") + 
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
plot1

## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
plot2 <- df %>% 
  filter(grp == 1 | grp == 2) %>% 
  ggplot(aes(x, y, colour = as.factor(grp))) + 
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("Random Intercept Model") + 
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
plot2


## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
plot3 <- df %>% 
  filter(grp == 1 | grp == 3) %>% 
  ggplot(aes(x, y, colour = as.factor(grp))) + 
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("Random Slope Model") + 
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
plot3

## ----icc, echo = TRUE, message=FALSE, warning = FALSE--------------------
library(psychometric)

# Share of variance between subjects in dependent y
ICC2.lme(y, id, data = full) 


## ----mlm, echo = TRUE, message=FALSE, warning = FALSE--------------------
library(lme4)
library(broom)
#library(lmerTest)

model1 <-  lmer(y ~ (1|id), data = full)
glance(model1)

## ----mlm1, echo = TRUE, message=FALSE, warning = FALSE-------------------
tidy(model1)

## ----mlm2, echo = TRUE, message=FALSE, warning = FALSE-------------------
model2 <-  lmer(y ~ xic + scale1 + (1|id), data = full)
glance(model2)
anova(model1, model2)

## ----mlm3, echo = TRUE, message=FALSE, warning = FALSE-------------------
summary(model2)

