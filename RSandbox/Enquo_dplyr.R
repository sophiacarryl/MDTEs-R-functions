library("dplyr")
library("glue")
library("ggplot2")
df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

greet <- function(name) {
  paste("How do you do, ", name, "?")
}

greet("Hadley")

greet <- function(name) {
  glue("How do you do, {name}?")
}
greet("Hadley")


df <- tibble(
  g1 = c("Abby", "Jack", "Jack", "Abby", "Abby"),
  g2 = c("Ryan", "Ryan", "Mik", "Mik", "Mik"),
  a = sample(5),
  b = sample(5),
  c = sample(5)
)

df %>%
  group_by(g1) %>%
  summarise(a = mean(a))

# In dplyr (and in tidyeval in general) you use !! to say that you want to 
# unquote an input so that it’s evaluated, not quoted. 

my_summarise <- function(df, group_var) {
  df %>%
    group_by(!! group_var) %>%
    summarise(a = mean(a))
}

my_summarise(df, quo(g1))
#---

my_summarise <- function(df, group_var) {
  quo_group_var <- enquo(group_var)
  print(quo_group_var)
  
  df %>%
    group_by(!! quo_group_var) %>%
    summarise(a = mean(a))
}

my_summarise(df, g1)
#--------------------------------

plot_by_two_groups <- function(comments_df, # should be a a dataframe
                               group_var_y, #variable to be plotted on y axis
                               group_var_x,
                               get_all_word_counts = F,
                               ...){
  
  group_var_y <- enquo(group_var_y)
  group_var_x <- enquo(group_var_x)
  
  
  total_comments_by_group_var_y <- comments_df%>% 
    count(!! group_var_y) %>% rename(total_comments_by_group_var_y = n)
  
  comments_df%>%
    count(!! group_var_x,!! group_var_y)%>%
    inner_join(total_comments_by_group_var_y)%>%
    mutate(perc_of_comments_by_group_var_y = n/total_comments_by_group_var_y)%>%
    ggplot(aes(x=!! group_var_y,y=perc_of_comments_by_group_var_y,fill=!! group_var_x))+
    geom_bar(stat = "identity")+
    theme_classic()+
    coord_flip()+
    ylab(ylab)+
    #  theme(axis.title.y = element_blank()) +
    labs(title = paste("Rating Distibution by", quo_name(group_var_y)),
         #  y = "% of customers",
         subtitle =paste("% of customers by", quo_name(group_var_y),
                         "Total verbatim = ",nrow(comments_df)),
         fill = NULL)
  
}

plot_by_two_groups(df, g1, g2)


