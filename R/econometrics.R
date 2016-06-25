################################################################################
# Package: econometrics                                                        #
# Type: Package                                                                #
# Title: Regressions / Econometric analyses in R with data from MySQL database #
# Author: Michael David Gill                                                   #
# Maintainer: Michael David Gill <michaelgill1969@gmail.com>                   #
# URL: https://github.com/michaelgill1969/econometrics                         #
# Description: This package, developed for the Upwork Contract ID 16581251,    #
# provides functions for importing data from SQL database and econometric      #
# analyses on that data.  The regressions cover random effects, fixed effects, #
# and instrument variable models. The script is written in R and the code is   #
# structured with explanatory comments on average every 2 code lines (in       #
# English). The script is flexible so the user can chose file paths, set the   #
# parameters for the regression like Y and X’s, chose the type of model        #
# (single or multiple), chose tests (single or multiple). After the setup, the #
# code runs automatically without user interaction, displays and saves the     #
# results.                                                                     #
# License: The MIT License (MIT)                                               #
################################################################################

# 1.	Set all parameters at the start of the script for the further steps
# (file path, predefine which regressions, tests and trimming should be
# performed, if SQL Script is performed or not, level of trimming, set y, x’s,
# fixed effects, instrument variables, …)
#' Main Function
#'
#' Description
#'
#' @param file_path path to SQL database.
#' @param regression_type type of regression(s?) to be used.
#' @param test_type type of test(s?) to be performed.
#' @param left_trim
#' outlier percentage to be trimmed from a distributions lower end.
#' @param right_trim
#' outlier percentage to be trimmed from a distributions upper end.
#' @param etc et cetera.
#' @author Michael David Gill
#' @details
#' description
#' @export
#' @import package
#' what packages to open to install, check and load necessary libraries.

econometrics <- function(
    file_path,
    regression_type,
    test_type,
    left_trim,
    right_trim
) {

    # load sql database
    mysql_db <- load_sql_database(file_path)

    # run sql script and import resulting table
    data_frame <- run_sql_script(file_path)

    # shutdown sql database
    # sleep 600s
    Sys.sleep(600)
    # Shut down database
    dbSendQuery(mysql_db, "SHUTDOWN;")

    # write other function calls
}


# 3.	Connect to MySQL database (e.g. ‘RMySQL’)
# - Credentials are available from a text file or likewise (e.g. D:\Project\Database.txt)
# - User: root / password: test / host: localhost / port:3306
# 4.	Run SQL Script from file and wait until finished (e.g. D:\Project\Prepare_Data.sql)
# 5.	Import all data from database to R (schema: test / table: data)
# 6.	Wait 10 min and then shutdown MySQL database (e.g. SQL command: SHUTDOWN;)
#' Load SQL Database
#'
#' Description
#'
#' @param file_path path to SQL database.
#' @return mysql_db a SQL database connection.
#' @author Michael David Gill
#' @details
#' description
#' @export
#' @import RMySQL
#' what packages to open to install, check and load necessary libraries.

load_sql_database <- function(file_path) {

    # load libraries
    # install.packages("RMySQL")
    library(RMySQL)

    # set working directory to the SQL file_path
    old_working_directory <- getwd()[1]
    setwd(file_path)

    # load configuration file
    cat("Connecting to the MySQL database ...", "\n")
    cat("Please enter the name of the MySQL configuration file: ")
    configuration_file <- readLines(n = 1)

    if (exists("configuration_file")) {

        configuration_df <- read.delim(configuration_file, sep = "=")

        dbname <- as.character(configuration_df["database", 1])
        username <- as.character(configuration_df["user", 1])
        password <- as.character(configuration_df["password", 1])
        host <- as.character(configuration_df["host", 1])
        port <- as.integer(configuration_df["port", 1])

    } else {

        cat("That configuration file does not exist.", "\n")
        cat("Please enter the database name: ")
        dbname <- readLines(n = 1)
        cat("Please enter the username: ")
        username <- readLines(n = 1)
        cat("Please enter the password: ")
        password <- readLines(n = 1)
        cat("Please enter the host name: ")
        host <- readLines(n = 1)
        cat("Please enter the port number: ")
        port <- readLines(n = 1)

    }

    # open MySQL connection
    mysql_db <- dbConnect(
        MySQL(),
        dbname = dbname,
        username = username,
        password = password,
        host = host,
        port = port
    )

    # return SQL database
    return(mysql_db)

    # reset working directory
    setwd(old_working_directory)

}


#' Run SQL Script
#'
#' Description
#'
#' @param file_path path to SQL database.
#' @return data_frame
#' resulting table from SQL statement formatted as an R data frame.
#' @author Michael David Gill
#' @details
#' description
#' @export
#' @import RMySQL
#' what packages to open to install, check and load necessary libraries.

run_sql_script <- function(file_path) {

    # load libraries
    # install.packages("RMySQL")
    library(RMySQL)

    # set working directory to the SQL file_path
    old_working_directory <- getwd()[1]
    setwd(file_path)

    # load SQL script file
    cat("Loading the MySQL script ...", "\n")
    cat("Please enter the name of the MySQL script file: ")
    sql_file_name <- readLines(n = 1)

    if (exists("sql_file_name")) {

        sql_statement <- read.fwf(sql_file_name, widths = c(1000))

    } else {

        cat("That script file does not exist.", "\n")
        cat("Please enter your SQL code: ")
        sql_statement <- readLines(n = 1000) # @MICHAEL: DON"T FORGET TO FIX THIS

    }

    # run the statements from the SQL script
    sql_table <- dbSendQuery(mysql_db, sql_statement)
    # import the SQL table to an R data frame
    data_frame <- fetch(sql_table, n = -1)

    # return SQL table
    return(data_frame)

    # reset working directory
    setwd(old_working_directory)

}


# 7.	If defined, trim data based on left and right trim level / interval
# - the trim level should be based on the chosen types (quantile, mean ± standard deviation, median ± standard deviation)


# 8.	Summarize data and save (mean, standard deviation, min, max, correlation)


# 9.	Test for multicollinearity by calculating variance inflation factors (VIF) for each variable


# 10.	Run a pooled OLS and save the estimates with ‘plm’ function (-->pooling)


# 11.	Run a random regression and save the estimates with ‘plm’ function (-->random)


# 12.	Run a fixed effects regression  and save the estimates with ‘plm’ function (-->fixed)


# 13.	Run regression with instrumental variables (-->IV)


# 14.	Run regression with instrumental variables and fixed effects (--> fixed_IV)


# 15.	Display within, between and overall variation


# 16.	Perform tests
# - Pooled OLS vs. random effects: Breusch-Pagan Lagrange multiplier: ‘plmtest’
# - Random vs. fixed effects: Hausman test: ‘phtest’
# - Testing for time-fixed effects: ‘pFtest’, ‘plmtest
# - Testing for heteroskedasticity: ‘bptest’ / White’s test
# - Testing for weak instrument: F-test / Wald test


# 17.	Display all results from data summary, performed regressions and tests


# 18.	Save results to file


# Try to implement a multicore approach.
# As my regressions take a lot of time, the R code should take advantage of
# multicore CPUs in case of performing the regressions. I couldn’t find anything
# if this is possible “within” a single regression. Otherwise parallelize the
# computation of multiple regression with e.g. ‘foreach’ package (--> each
# regression is computed on a single core, but regressions are computed
# parallel).
#
# Based on the "external" SQL code the user knows the variable names, so he can
# define the regression variables to be used n advance.