library('ggplot2')
library('glue')
library('dplyr')

loan_data <- read.csv('LoanApplicant_Dataset.csv', stringsAsFactors = FALSE)

cols <- c("Gender", "Married", "Education", "Self_Employed",
              "Credit_History", "Property_Area", "Loan_Status")
n = length(cols)

# transforming values to be more clear
loan_data[loan_data$Married == 'Yes', 'Married'] <- 'Married'
loan_data[loan_data$Married == 'No', 'Married'] <- 'Not Married'

loan_data[loan_data$Self_Employed == 'Yes', 'Self_Employed'] <- 'Self Employed'
loan_data[loan_data$Self_Employed == 'No', 'Self_Employed'] <- 'Not Self Employed'

loan_data[loan_data$Credit_History == 1, 'Credit_History'] <- 'With Credit History'
loan_data[loan_data$Credit_History == 0, 'Credit_History'] <- 'Without Credit History'

loan_data[loan_data$Loan_Status == 'Y', 'Loan_Status'] <- 'With Loan Status'
loan_data[loan_data$Loan_Status == 'N', 'Loan_Status'] <- 'Without Loan Status'

# iterating through all the columns of data frame
index = 1
for (col1 in cols[index:n]){
  for (col2 in cols[index:n]){
    if (col1 != col2){
      
      # creating marginal distribution
      marg_dist <- data.frame(prop.table(table(loan_data[[col1]]))*100)
      names(marg_dist)[names(marg_dist) == 'Var1'] <- col1
      names(marg_dist)[names(marg_dist) == 'Freq'] <- 'MarginalDistribution'

      # creating contingency table with percents
      pt <- as.data.frame.matrix(prop.table(
        table(loan_data[[col1]], loan_data[[col2]]), margin = 1)*100)
      pt[col1] <- rownames(pt)
      
      # joining marginal dist with contingency table
      merged_df1 <- merge(pt,marg_dist,by=col1)
      rownames(merged_df1) <- merged_df1[[col1]]
      merged_df1[col1] <- NULL
      print(merged_df1)
      
      # checking if values match with marginal distribution
      matches = 0
      df_cols <- names(merged_df1)
      for (var in df_cols){
        if (var != 'MarginalDistribution'){
          match <- sum(merged_df1$MarginalDistribution == merged_df1[[var]])
          matches = matches + match
        }
      }
      if (matches > 0){
        print(glue("{col1} and {col2} have no or weak relationship"))
      } else {
        print(glue("{col1} and {col2} have some relationship"))
      }
      
      # plotting bar chart
      barplot(as.matrix(merged_df1), main=glue("{col1} vs {col2}"),
              legend=rownames(merged_df1))
    }
  }
  index <- index + 1
}