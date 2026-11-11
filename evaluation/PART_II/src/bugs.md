# Bugs encountered during Dast homework and their solution
## Type errors
### Lengths differs among variables when defining a linear model
The error message: 
```
Error in model.frame.default(formula = biomass ~ dat$num_trees + dat$mean_dbh,  : 
  variable lengths differ (found for 'dat$num_trees')
```
I added a rename of given variable like varName <- dat$varName and it stopped.

### Apparent wrong type in column
```
Error in model.frame.default(formula = dat$biom ~ dat$num_trees + dat$mean_dbh +  : 
  invalid type (NULL) for variable 'dat$dem100_altitude'
```
It is 1000 instead of 100

### Undefined column
```
Error in `[.data.frame`(NFI_SURVEY_NUM, , i) : undefined columns selected
```
Better to use lapply rather than manually counting the index of each integer
column

### X must be a numeric matrix
```
Error in heatmap(NFI_SURVEY_NUM_cor, col = col, symm = TRUE) : 
  'x' must be a numeric matrix
```
First convert the correlation df into a matrix using as.matrix,
then specify its rownames using `rownames` and finally `heatmap()`.

### Cannot save my heatmap
```
Error in UseMethod("grid.draw") : 
  no applicable method for 'grid.draw' applied to an object of class "c('integer', 'numeric')"
```

### Error in RowSideColors length
```
Error in heatmap(hm_nfi_survey, scale = "none", col = col, RowSideColors = rep(c("blue",  : 
  'RowSideColors' must be a character vector of length nrow(x)
```
`RowSideColors` must be of the half of the length of the correlation matrix (nbOfVariables)