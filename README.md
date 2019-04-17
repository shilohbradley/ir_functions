# R Functions for Institutional Researchers
R functions and templates for everyday Institutional Research tasks, including reading SQL files into R, de-identifying student ID's, passing large batches of student ID's or UNITIDS (IPEDS) to SQL, and so much more!

## Examples
De-identify study ID's
```
> quasi_deidentify(emplid = 123456789, my_factor = 444444443)
[1] "567901232"
```

Reading a query from a file 
  - ```path``` and ```ext``` have default values so normally only ```file_name``` is needed unless the path is different
```
> get_query_from_file(path = "X:/Groups/Decision Support/Exchange/Shiloh/SQL/", file_name = "example_query", ext = ".sql")
  STRM
1 2108
2 2118
3 2128
4 2138
```
