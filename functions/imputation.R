Imputation <- function(set, impute_table, vars){
  if (class(set[, vars[1]]) == "factor"){
    newset <- data.frame(matrix("", ncol = length(vars), nrow = nrow(set)))  
    for (i in 1:length(vars)){                       #Iterate over specific columns
      index <- match(vars, names(set)) 
      set[,index[i]] = factor(set[,index[i]], levels = levels(set[,index[i]])) 
      set[,index[i]][is.na(set[,index[i]])] = impute_table[1,i]  #convert all NA's to impute_table
      newset[,i] <- set[,index[i]]
    }
    newset <- newset %>% setNames(vars) #Set back the names of features
    index <- match(vars, names(set))    #Find index from input data set
    newset <- cbind(newset, set[,-index]) #Add column not used in imputation to new data set
    return(newset) #return the data.frame with imputation
  } else {
    newset <- data.frame(matrix("", ncol = length(vars), nrow = nrow(set)))  
    for (i in 1:length(vars)){                       #Iterate over specific columns
      index <- match(vars, names(set))               #Identify columns
      set[,index[i]] <- ifelse(is.na(set[,index[i]]), impute_table[1,i], set[,index[i]]) #Replace NA for impute_table values
      newset[,i] <- set[,index[i]]
    }
    newset <- newset %>% setNames(vars) #Set back the names of features
    index <- match(vars, names(set))    #Find index from input data set
    newset <- cbind(newset, set[,-index]) #Add column not used in imputation to new data set
    return(newset) #return the data.frame with imputation
  }
}  
