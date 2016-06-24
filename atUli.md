# @Uli: Notes on how to do R code
Michael David Gill  
June 24, 2016  

### RStudio Setup

1. Setup a new GitHub repository.
2. Create a "New Project"; follow the "Version Control" options.
3. Set up the package skeleton (Directory structure (R/,man/), DESCRIPTION file, NAMESPACE file, documentation files).


```r
package.skeleton("econometrics")
```
4. Delete unncessary file "?-internal.R" in R directory.

### Script Setup
1. Create "R Script" file in R directory.
2. Add description header like what will be placed in the DESCRIPTION file.
3. Copy the original psuedo-psuedoscript.
4. Add comments beginning with #' that will later be used to generate the documentation.
5. Add skeleton code and more psuedoscript.
