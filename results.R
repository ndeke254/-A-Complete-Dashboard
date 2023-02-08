library(rhandsontable)
library(tidyverse)
library(stringr)
rhandsontable(user_results)

#Grade allocation
x <- user_results$Science
user_results<- user_results |> 
  mutate( Maths_grade = 
            case_when(
              x >= 70 ~'A',
              x >= 60 ~'B',
              x >= 50 ~'C',
              x >= 40 ~'D',
              TRUE ~ 'E'
            )
          )
#combine columns
user_results$Science <- paste(user_results$Science,
                              user_results$Maths_grade)
user_results <- user_results[,-7]

#show grade alone
grade <- user_results |> subset(user=='Jefferson') 
grade <- grade[,-1]
r_score <- as.character(grade[1,])
name <- grade |> colnames()
grade_summary <- data.frame(subject= name,
                             score = r_score
                            )
final_grade <- str_sub(grade_summary$score,-1)
show_table <- data.frame(subject= name,
           score = final_grade
           )














