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
#' R SQL Econometrics
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
#' @export r_sql_econometrics
#' @import RMySQL

r_sql_econometrics <- function() {

    cat("Welcome to R Econometrics for MySQL Databases.", "\n")

    # load sql database
    mysql_db <- load_sql_database()


    # run sql script and import resulting table
    data_frame <- run_sql_script(file_path, mysql_db)


    # enter parameters
    cat("Please enter the left and right trim levels (from 0 to 0.5):", "\n")
    left_trim <- as.numeric(readline(prompt = "left > "))
    right_trim <- as.numeric(readline(prompt = "right > "))


    # display descriptive statistics
    descriptive_statistics <- summarize_data(data_frame, left_trim, right_trim)
    descriptive_statistics


    # define outcome variable, predictor variables, instrumental variables, and
    # fixed effects

    cat(
        "Please enter the column number containing the outcome variable: ",
        "\n"
    )
    y <- as.integer(readline(prompt = "y > "))

    cat(
        paste(
            "Please enter the column numbers containin the covariates, ",
            "one per line, and then hit enter/return to end: "
        ),
        "\n"
    )
    x <- scan(stdin())

    cat(
        paste(
            "Please enter the column numbers containing the instrumental ",
            "variables, one per line, and then hit enter/return to end: "
        ),
        "\n"
    )
    x_iv <- scan(stdin())

    cat(
        paste(
            "Please enter the column numbers containing the fixed-effect ",
            "variables, one per line, and then hit enter/return to end: "
        ),
        "\n"
    )
    x_fe <- scan(stdin())


    # choose model to run
    model_choice <- choose_model()


    # choose tests to run


    # shutdown sql database
    # sleep 600s
    Sys.sleep(600)
    # Shut down database
    dbSendQuery(mysql_db, "SHUTDOWN;")


    # run model
    model <- multi_plm(y, x, x_fe, x_iv, data_frame, model_choice)


    # run tests


}


# 3.	Connect to MySQL database (e.g. ‘RMySQL’)
# - Credentials are available from a text file or likewise (e.g. D:\Project\Database.txt)
# - User: root / password: test / host: localhost / port:3306
# 4.	Run SQL Script from file and wait until finished (e.g. D:\Project\Prepare_Data.sql)
# 5.	Import all data from database to R (schema: test / table: data)
# 6.	Wait 10 min and then shutdown MySQL database (e.g. SQL command: SHUTDOWN;)

#' Construct Configuration Data Frame
#'
#' Description
#'
#' @return configuration_df a data frame of MySQL configuration parameters.
#' @author Michael David Gill
#' @details
#' description
#' @import stringi

construct_configuration_df <- function() {

    cat("Would you like to use a MySQL configuration file?", "\n")

    repeat {

        use_configuration_file <- readline(prompt = "Yes (Y) or No (N) > ")

        if (stri_startswith_fixed(use_configuration_file, tolower("y"))
        ) {

            cat("Please enter the path to the MySQL configuration file:", "\n")
            file_path <- readline(prompt = "path > ")

            cat("Please enter the name of the MySQL configuration file:", "\n")
            configuration_file <-
                paste(file_path, readline(prompt = "file > "), sep = "/")

            configuration_df <-
                as.data.frame(t(read.delim(configuration_file, sep = "=")))

            return(configuration_df)

            break

        } else if (
            stri_startswith_fixed(use_configuration_file, tolower("n"))
        ) {

            cat("Please enter the username: ", "\n")
            user <- readline(prompt = "username > ")
            cat("Please enter the password: ", "\n")
            password <- readline(prompt = "password > ")
            cat("Please enter the database name: ", "\n")
            database <- readline(prompt = "database > ")
            cat("Please enter the host name: ", "\n")
            host <- readline(prompt = "host > ")
            cat("Please enter the port number: ", "\n")
            port <- as.integer(readline(prompt = "port > "))

            configuration_df <- data.frame(user, password, database, host, port)

            return(configuration_df)

            break

        }

    }

}


#' Load SQL Database
#'
#' Description
#'
#' @param file_path path to SQL database.
#' @param configuration_file_name name of a MySQL configuration file.
#' @return mysql_db a SQL database connection.
#' @author Michael David Gill
#' @details
#' description
#' @import RMySQL

load_sql_database <- function() {

    # load configuration file
    cat("Connecting to the MySQL database ...", "\n")

    configuration_df <- construct_configuration_df()

    # open MySQL connection

    mysql_db <- dbConnect(
        MySQL(),
        dbname = as.character(configuration_df$database),
        username = as.character(configuration_df$user),
        password = as.character(configuration_df$password),
        host = as.character(configuration_df$host),
        port = as.integer(configuration_df$port)
    )

    # return SQL database
    return(mysql_db)

}


#' Run SQL Script
#'
#' Description
#'
#' @param mysql_db name of database.
#' @return data_frame
#' resulting table from SQL script formatted as an R data frame.
#' @author Michael David Gill
#' @details
#' description
#' @import RMySQL
#' @import stringi

run_sql_script <- function(mysql_db) {

    # load SQL script file
    cat("Loading the MySQL script ...", "\n")

    repeat {

        cat("Please enter the path to the MySQL script file:", "\n")
        file_path <- readline(prompt = "path > ")
        cat("Please enter the name of the MySQL script file: ", "\n")
        sql_file_name <-
            paste(file_path, readline(prompt = "SQL > "), sep = "/")

        if (exists("sql_file_name")) {

            sql_script <- readLines(sql_file_name)
            sql_script <- gsub("\t", " ", sql_script)

            break

        } else {

            cat("That script file does not exist.", "\n")

        }

    }

    # run the statements from the SQL script
    sql_statement <- ""
    for (i in 1:length(sql_script)) {
        if (
            !stri_startswith_fixed(sql_script[i], "/*")
            &&
            !is.null(sql_script[i])
            &&
            (sql_script[i] != "")
        ) {
            sql_statement <- paste(sql_statement, sql_script[i])
            if (stri_endswith_fixed(sql_statement, ";")) {
                sql_table <- dbSendQuery(mysql_db, sql_statement)
            }
        }
    }
    # import the SQL table to an R data frame
    data_frame <- fetch(sql_table, n = -1)

    # return SQL table
    return(data_frame)

}


# 7.	If defined, trim data based on left and right trim level / interval
# - the trim level should be based on the chosen types (quantile, mean ± standard deviation, median ± standard deviation)

#' Trim Variable
#'
#' Description
#'
#' @param x a vector to be trimmed.
#' @param left_trim
#' percentage of observations to trim from right of the variable's distribution.
#' @param right_trim
#' percentage of observations to trim from left of the variable's distribution.
#' @return vector
#' @author Michael David Gill
#' @details
#' description

trim_variable <- function(x, left_trim, right_trim) {

    return(
        x[
            ceiling(length(x) * left_trim)
            :
            floor(length(x) - length(x) * right_trim)
        ]
    )

}


# 8.	Summarize data and save (mean, standard deviation, min, max, correlation)

#' Summarize Data
#'
#' Description
#'
#' @param data_frame data frame to be summarized.
#' @param left_trim
#' percentage of observations to trim from right of the variable's distribution.
#' @param right_trim
#' percentage of observations to trim from left of the variable's distribution.
#' @return summary
#' data frame including mean, median, standard deviation, minimum, and maximum
#' for each variable.
#' @author Michael David Gill
#' @details
#' description

summarize_data <- function(data_frame, left_trim, right_trim) {

    means <-
        sapply(
            data_frame,
            function(x) {
                if(is.numeric(x)) mean(trim_variable(x, left_trim, right_trim))
            }
        )
    standard_deviations <-
        sapply(
            data_frame,
            function(x) {
                if(is.numeric(x)) sd(trim_variable(x, left_trim, right_trim))
            }
        )
    minimums <-
        sapply(
            data_frame,
            function(x) {
                if(is.numeric(x)) min(trim_variable(x, left_trim, right_trim))
            }
        )
    maximums <-
        sapply(
            data_frame,
            function(x) {
                if(is.numeric(x)) max(trim_variable(x, left_trim, right_trim))
            }
        )

    return(
        data.frame(rbind(means, standard_deviations, minimums, maximums))
    )

}


# 9.	Test for multicollinearity by calculating variance inflation factors (VIF) for each variable

#' Variance Inflation Factors for Multiple Models
#'
#' Description
#'
#' @param all_models a list of panel data estimator models.
#' @return a list of test results.
#' @author Michael David Gill
#' @details
#' description
#' @import plm
#' @import car
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

multi_vif <- function(all_models) {

    test_results <- list(NULL, NULL, NULL)
    for (i in 1:length(all_models)) {
        test_results[[i]] <- vif(all_models[[i]])
    }

    return(test_results)

}


# 10.	Run a pooled OLS and save the estimates with ‘plm’ function (-->pooling)
# 11.	Run a random regression and save the estimates with ‘plm’ function (-->random)
# 12.	Run a fixed effects regression  and save the estimates with ‘plm’ function (-->fixed)

# 13.	Run regression with instrumental variables (-->IV) ivreg {AER}
# 14.	Run regression with instrumental variables and fixed effects (--> fixed_IV) felm {lfe}

#' Choose model
#'
#' Description
#'
#' @return model_choice
#' @author Michael David Gill
#' @details
#' description

choose_model <- function() {

    # prompt the user to choose the type(s) of model(s) to be run

    cat("Please choose the models to be run: ", "\n")
    cat("1. pooled OLS", "\n")
    cat("2. fixed effects", "\n")
    cat("3. random effects", "\n")
    cat("4. regression with instrumental variables", "\n")
    cat("5. regression with fixed effects and instrumental variables", "\n")

    model_choice <- readline(prompt = "model number > ")

    if (
        !is.integer(model_choice)
        &&
        (!((model_choice >= 1) && (model_choice <= 5)))
    ) {
        cat("Please enter an integer between 1 and 5.")
        model_choice <- readline(prompt = "model number > ")
    }

    return(model_choice)

}

#' Multiple Panel Data Estimators
#'
#' Description
#'
#' @param y column numbers of outcome variable to be predicted.
#' @param x a vector of column numbers of predictor/covariate variable(s).
#' @param x_fe optional vector of column numbers of fixed effects.
#' @param x_iv optional vector of column numbers of instrumental variables.
#' @param data data frame to be modeled.
#' @param model_choice
#' type of model to be run, either (1) pooled OLS, (2) fixed effects, (3) random
#' effects, (4) regression with instrumental variables, or (5) regression with
#' fixed effects and instrumental variables.
#' @return model
#' @author Michael David Gill
#' @details
#' description
#' @import plm
#' @import AER
#' @import lfe
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

multi_plm <- function(y, x, x_fe, x_iv, data, model_choice) {

    # assemble covariate argument
    covariate_argument <- colnames(data_frame[x[1]])
    for (i in 2:length(x)) {
        covariate_argument <-
            paste(covariate_argument, colnames(data_frame[x[i]]), sep = " + ")
    }
    formula <- paste(colnames(data_frame[y]), covariate_argument, sep = " ~ ")

    if (!missing(x_fe) && !missing(x_iv)) {
        fixed_effects_argument <- colnames(data_frame[x_fe[1]])
        for (i in 2:length(x_fe)) {
            fixed_effects_argument <-
                paste(
                    fixed_effects_argument,
                      colnames(data_frame[x_fe[i]]),
                      sep = " + "
                )
        }
        instrumental_variables_argument <- colnames(data_frame[x_iv[1]])
        for (i in 2:length(x_iv)) {
            instrumental_variables_argument <-
                paste(
                    instrumental_variables_argument,
                    colnames(data_frame[x_iv[i]]),
                    sep = " + "
                )
        }
        formula <-
            paste(
                formula,
                fixed_effects_argument,
                instrumental_variables_argument,
                sep = " | "
            )
    } else if (!missing(x_iv)) {
        instrumental_variables_argument <- colnames(data_frame[x_iv[1]])
        for (i in 2:length(x_iv)) {
            instrumental_variables_argument <-
                paste(
                    instrumental_variables_argument,
                    colnames(data_frame[x_iv[i]]),
                    sep = " + "
                )
        }
        formula <-
            paste(
                formula,
                instrumental_variables_argument,
                sep = " | "
            )
    }

    # run chosen model
    if (model_choice == 1) {
        pooled_model <- plm(as.formula(formula), data_frame, model = "pooling")
    } else if (model_choice == 2) {
        fixed_model <- plm(as.formula(formula), data_frame, model = "within")
    } else if (model_choice == 3) {
        random_model <- plm(as.formula(formula), data_frame, model = "random")
    } else if (model_choice == 4) {
        iv_model <- ivreg(as.formula(formula), data_frame)
    } else if (model_choice == 5) {
        feiv_model <- felm(as.formula(formula), data_frame)
    }

    # return output
    if (model_choice == 1) {
        return(pooled_model)
    } else if (model_choice == 2) {
        return(fixed_model)
    } else if (model_choice == 3) {
        return(random_model)
    } else if (model_choice == 4) {
        return(iv_model)
    } else if (model_choice == 5) {
        return(feiv_model)
    }

}


# 15.	Display within, between and overall variation


# 16.	Perform tests
# - Pooled OLS vs. random effects: Breusch-Pagan Lagrange multiplier: ‘plmtest’
plmtest(all_models[[1]], type = "bp")

# - Random vs. fixed effects: Hausman test: ‘phtest’

#' Multiple Hausman Tests
#'
#' Description
#'
#' @param all_models a list of panel data estimator models.
#' @return a list of test results.
#' @author Michael David Gill
#' @details
#' description
#' @import plm
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

multi_phtest <- function(all_models) {

    combination <- combn(all_models, 2)
    test_results <- list(NULL, NULL, NULL)
    for (i in 1:ncol(combination)) {
        test_results[[i]] <- phtest(combination[1,i][[1]], combination[2,i][[1]])
    }

    return(test_results)

}


# - Testing for time-fixed effects: ‘pFtest’, ‘plmtest
plmtest(all_models[[1]], effect = "time", type = "bp")

#' Multiple F Tests for Individual and/or Time Effects
#'
#' Description
#'
#' @param all_models a list of panel data estimator models.
#' @return a list of test results.
#' @author Michael David Gill
#' @details
#' description
#' @import plm
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

multi_pFtest <- function(all_models) {

    combination <- combn(all_models, 2)
    test_results <- list(NULL, NULL, NULL, NULL, NULL, NULL)
    for (i in 1:ncol(combination)) {
        test_results[[i]] <- pFtest(combination[1,i][[1]], combination[2,i][[1]])
        test_results[[i]] <- pFtest(combination[2,i][[1]], combination[1,i][[1]])
    }

    return(test_results)

}

# - Testing for heteroskedasticity: ‘bptest’ / White’s test

#' Multiple Breusch-Pagan Tests
#'
#' Description
#'
#' @param all_models a list of panel data estimator models.
#' @return a list of test results.
#' @author Michael David Gill
#' @details
#' description
#' @import plm
#' @import lmtest
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

multi_bptest <- function(all_models) {

    test_results <- list(NULL, NULL, NULL)
    for (i in 1:length(all_models)) {
        test_results[[i]] <- bptest(all_models[[i]])
    }

    return(test_results)

}

# - Testing for weak instrument: F-test / Wald test
# waldtest(all_models[[1]], test = "F")


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