##################################################
# An RScript of all the functions that           #  
# I created and regularly use.                   #
##################################################
# Last updated: April 17, 2019                   #
# Notes: Created file                            #
##################################################

## A function for breaking up long xtables into
## multiple xtables so that they fit on paper
## more nicely.
make_multiple_tables <- function(y, threshold = 30) {
  for (i in 1:ceiling(length(names(y)) / threshold)) {
    df <- data.frame(x = seq((i - 1) * threshold + 1, min(c(length(names(y)), i * threshold)), 1),
                     names = names(y[((i - 1) * threshold + 1):(min(c(length(names(y)), i * threshold)))]))
    names(df) <- c("#", "Variable")
    print(xtable(df, 
                 caption = paste0("Part ", i)),
          table.placement = "H",
          caption.placement = "top",
          include.rownames = FALSE,
          include.colnames = TRUE,
          comment = FALSE)  
  }
}

## A function used for passing a vector in R
## to become a string in SQL.
## Mostly used for passing a long list of EMPLID's
## from a data frame in R.
R_vector_to_SQL_vector <- function(v) {
  my_str <- paste0("('", v[1], "'")
  
  for (i in v[-1]) {
    my_str <- paste0(my_str, ",'", i, "'")
  }
  my_str <- paste0(my_str, ")")
  
  return(my_str)
}

## A function for reading a SQL file into R
## and also calling the query and
## returning the data.
get_query_from_file <- function(path = "X:/Groups/Decision Support/Exchange/Shiloh/SQL/", file_name, ext = ".sql") {
  con <- file(paste0(path, file_name, ext), "r")
  query <- readLines(con, -1)
  close(con)
  query <- gsub("\n", "", query)
  query <- gsub("[;]", "", query)
  my_newq <- ""
  for (elem in query) {
    my_newq <- paste(my_newq, elem)
  }
  
  df <- dbGetQuery(db_con, my_newq)
  
  return(df)
}

## A function for getting all the enrollments
## for each student based on ID.
## Replaces a place holder in the query with
## the student ID's.
get_chunked_enrollments <- function(threshold = 500, emplid = id, query = enrolled_query) {
  full_results <- NULL
  n <- ceiling(length(id)/threshold)
  
  for (i in 1:n) {
    i_id <- emplid[(threshold * (i - 1) + 1):min(c(i * threshold, length(emplid)))]
    i_query <- gsub("[<]XXX[>]", R_vector_to_SQL_vector(i_id), query)
    i_r <- dbGetQuery(db_con, i_query) 
    
    if (nrow(i_r) > 0) {
      if (!is.null(full_results)) {
        full_results <- rbind(full_results, i_r)
      } else {
        full_results <- i_r
      }
    }
    print(i)
  }
  
  return(full_results)
}

## A function for de-identifying student ID's.
quasi_deidentify <- function(emplid, my_factor) {
  emplid <- as.numeric(emplid) + my_factor
  
  return(as.character(emplid))
}

## A function for adding 0's in front of a student ID
## that has lost 0's due to Excel formatting.
slaponzeros <- function(howmanytotaldigits = 9, ids) {
  newid <- rep(NA, length(ids))
  for (i in 1:length(ids)) {
    i_nchar <- nchar(ids[i])
    newid[i] <- ids[i]
    for (j in (i_nchar + 1):howmanytotaldigits) {
      newid[i] <- paste0("0", newid[i])
    }
  }
  
  return(newid)
}