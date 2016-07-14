# library(AER)
# library(RMySQL)
# library(car)
# library(lfe)
# library(lmtest)
# library(plm)
# library(stringi)

#' R SQL Econometrics
#'
#' Runs various pooled linear models and tests for econometric analysis using
#' data imported from a MySQL server.
#'
#' @author Michael David Gill
#' @details
#' Sets all parameters at the start of the script for the further steps (file
#' path, predefine which regressions, tests and trimming should be performed, if
#' SQL Script is performed or not, level of trimming, set y, x’s, fixed effects,
#' instrument variables, …)
#'
#' Connects to MySQL database (e.g. ‘RMySQL’)
#' - Credentials are available from a text file or likewise
#' - User: root / password: test / host: localhost / port:3306
#'
#' Runs SQL Script from file and wait until finished
#'
#' Imports all data from database to R (schema: test / table: data)
#'
#' Waits 10 min and then shutdown MySQL database (e.g. SQL command: SHUTDOWN;)

r_sql_econometrics <- function() {

    cat("Welcome to R Econometrics for MySQL Databases.", "\n")

    # load sql database
    mysql_db <- load_sql_database()


    # run sql script and import resulting table
    data_frame <- run_sql_script(mysql_db)


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
            "Please enter the column numbers containing the covariates, ",
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


    # shutdown sql database
    # sleep 600s
    Sys.sleep(600)
    # Shut down database
    dbSendQuery(mysql_db, "SHUTDOWN;")


    # run models
    model_list <-
        run_models(
            y = y,
            x = x,
            x_fe = x_fe,
            x_iv = x_iv,
            data = data_frame,
            model_choice =  model_choice
        )




    # 15.	Display within, between and overall variation

    # 16.	Perform tests
    # run tests
    test_list <- run_tests(model_list)


    # 17.	Display all results from data summary, performed regressions and tests

    # 18.	Save results to file

    # As my regressions take a lot of time, the R code should take advantage of
    # multicore CPUs in case of performing the regressions. I couldn’t find anything
    # if this is possible “within” a single regression. Otherwise parallelize the
    # computation of multiple regression with e.g. ‘foreach’ package (--> each
    # regression is computed on a single core, but regressions are computed
    # parallel).
    #
    # Based on the "external" SQL code the user knows the variable names, so he can
    # define the regression variables to be used n advance.


}


#' Construct Configuration Data Frame
#'
#' A helper function that constructs a data frame of configuration data for a
#' MySQL server from a file or user input for use by the load_sql_database
#' function.
#'
#' @return configuration_df a data frame of MySQL configuration parameters.
#' @author Michael David Gill
#' @import stringi

construct_configuration_df <- function() {

    cat("Would you like to use a MySQL configuration file?", "\n")

    repeat {

        use_configuration_file <- readline(prompt = "Yes (Y) or No (N) > ")

        if (stri_startswith_fixed(tolower(use_configuration_file), "y")
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
            stri_startswith_fixed(tolower(use_configuration_file), "n")
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
#' Loads a MySQL database.
#'
#' @return mysql_db a SQL database connection.
#' @author Michael David Gill
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
#' Runs a SQL query imported from an SQL query file.
#'
#' @param mysql_db name of database.
#' @return data_frame
#' resulting table from SQL script formatted as an R data frame.
#' @author Michael David Gill
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


#' Trim Variable
#'
#' If defined, trims data based on left and right trim level / interval for use
#' by the summarize_data function.
#'
#' @param x a vector to be trimmed.
#' @param left_trim
#' percentage of observations to trim from right of the variable's distribution.
#' @param right_trim
#' percentage of observations to trim from left of the variable's distribution.
#' @return vector
#' @author Michael David Gill

trim_variable <- function(x, left_trim, right_trim) {

    return(
        x[
            ceiling(length(x) * left_trim)
            :
            floor(length(x) - length(x) * right_trim)
        ]
    )

}


#' Summarize Data
#'
#' Calculates summary of descriptive statisitics (mean, standard deviation, min,
#' max, correlation).
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


#' Choose model
#'
#' Allows the user to choose one or all of several econometric models to be
#' calculated by run_models.
#'
#' @return model_choice
#' @author Michael David Gill

choose_model <- function() {

    # prompt the user to choose the type(s) of model(s) to be run

    cat("Please choose the models to be run: ", "\n")
    cat("1. pooled OLS", "\n")
    cat("2. fixed effects", "\n")
    cat("3. random effects", "\n")
    cat("4. regression with instrumental variables", "\n")
    cat("5. regression with fixed effects and instrumental variables", "\n")
    cat("6. all models", "\n")

    model_choice <- readline(prompt = "model number > ")

    if (
        !is.integer(model_choice)
        &&
        (!((model_choice >= 1) && (model_choice <= 6)))
    ) {
        cat("Please enter an integer between 1 and 6.")
        model_choice <- readline(prompt = "model number > ")
    }

    return(model_choice)

}


#' Run Models
#'
#' Calculates one or several econometric models.
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
#' Runs a pooled OLS and save the estimates with ‘plm’ function (-->pooling), a
#' random regression and save the estimates with ‘plm’ function (-->random), a
#' fixed effects regression  and save the estimates with ‘plm’ function
#' (-->fixed), a regression with instrumental variables (-->IV) ivreg {AER}, and
#' a regression with instrumental variables and fixed effects (--> fixed_IV)
#' @import plm
#' @import AER
#' @import lfe
#' @references Croissant, Y., & Millo, G. (2008). Panel data econometrics in R:
#' The plm package. \emph{Journal of Statistical Software, 27}(2), 1–43.
#' http://doi.org/10.18637/jss.v027.i02

run_models <- function(y, x, x_fe, x_iv, data, model_choice) {

    # assemble covariate argument
    covariate_argument <- colnames(data[x[1]])
    for (i in 2:length(x)) {
        covariate_argument <-
            paste(covariate_argument, colnames(data[x[i]]), sep = " + ")
    }
    formula <- paste(colnames(data[y]), covariate_argument, sep = " ~ ")

    if (!missing(x_fe) && !missing(x_iv)) {
        fixed_effects_argument <- colnames(data[x_fe[1]])
        for (i in 2:length(x_fe)) {
            fixed_effects_argument <-
                paste(
                    fixed_effects_argument,
                      colnames(data[x_fe[i]]),
                      sep = " + "
                )
        }
        instrumental_variables_argument <- colnames(data[x_iv[1]])
        for (i in 2:length(x_iv)) {
            instrumental_variables_argument <-
                paste(
                    instrumental_variables_argument,
                    colnames(data[x_iv[i]]),
                    sep = " + "
                )
        }
        formula_fe_iv <-
            paste(
                formula,
                fixed_effects_argument,
                instrumental_variables_argument,
                sep = " | "
            )
    } else if (!missing(x_iv)) {
        instrumental_variables_argument <- colnames(data[x_iv[1]])
        for (i in 2:length(x_iv)) {
            instrumental_variables_argument <-
                paste(
                    instrumental_variables_argument,
                    colnames(data[x_iv[i]]),
                    sep = " + "
                )
        }
        formula_iv <-
            paste(
                formula,
                instrumental_variables_argument,
                sep = " | "
            )
    }

    # run chosen model
    model_list <- list()
    if (model_choice == 1) {
        try(
            pooled_model <-
                plm(as.formula(formula), data, model = "pooling"),
            silent = TRUE
        )
        if (exists("pooled_model")) {
            model_list[[length(model_list)+1]] <- pooled_model
        }
    } else if (model_choice == 2) {
        try(
            fixed_model <-
                plm(as.formula(formula), data, model = "within"),
            silent = TRUE
        )
        if (exists("fixed_model")) {
            model_list[[length(model_list)+1]] <- fixed_model
        }
    } else if (model_choice == 3) {
        try(
            random_model <-
                plm(as.formula(formula), data, model = "random"),
            silent = TRUE
        )
        if (exists("random_model")) {
            model_list[[length(model_list)+1]] <- random_model
        }
    } else if (model_choice == 4) {
        try(
            iv_model <- ivreg(as.formula(formula_iv), data),
            silent = TRUE
        )
        if (exists("iv_model")) {
            model_list[[length(model_list)+1]] <- iv_model
        }
    } else if (model_choice == 5) {
        try(
            feiv_model <- felm(as.formula(formula_fe_iv), data),
            silent = TRUE
        )
        if (exists("feiv_model")) {
            model_list[[length(model_list)+1]] <- feiv_model
        }
    } else if (model_choice == 6) {
        try(
            pooled_model <-
                plm(as.formula(formula), data, model = "pooling"),
            silent = TRUE
        )
        if (exists("pooled_model")) {
            model_list[[length(model_list)+1]] <- pooled_model
        }
        try(
            fixed_model <-
                plm(as.formula(formula), data, model = "within"),
            silent = TRUE
        )
        if (exists("fixed_model")) {
            model_list[[length(model_list)+1]] <- fixed_model
        }
        try(
            random_model <-
                plm(as.formula(formula), data, model = "random"),
            silent = TRUE
        )
        if (exists("random_model")) {
            model_list[[length(model_list)+1]] <- random_model
        }
        try(
            iv_model <- ivreg(as.formula(formula_iv), data),
            silent = TRUE
        )
        if (exists("iv_model")) {
            model_list[[length(model_list)+1]] <- iv_model
        }
        try(
            feiv_model <- felm(as.formula(formula_fe_iv), data),
            silent = TRUE
        )
        if (exists("feiv_model")) {
            model_list[[length(model_list)+1]] <- feiv_model
        }
    }

    # return output
    return(model_list)

}


#' Run Tests
#'
#' Runs several test on models generated by run_models.
#'
#' @param model_list a list of panel data estimator models.
#' @return a list of test results.
#' @author Michael David Gill
#' @details
#' Tests for multicollinearity by calculating variance inflation factors (VIF)
#' for each variable, pooled OLS vs. random effects with Breusch-Pagan Lagrange
#' multiplier (plmtest), time-fixed effects: (plmtest), heteroskedasticity
#' (bptest / White’s test), Random vs. fixed effects: Hausman test (phtest),
#' time-fixed effects (pFtest), weak instrument (F-test / Wald test).
#' @import car
#' @import plm
#' @import lmtest

run_tests <- function(model_list) {

    test_list <- list()

    # 9.	Test for multicollinearity by calculating variance inflation factors (VIF) for each variable
    for (i in 1:length(model_list)) {
        try(
            test_list[[length(test_list)+1]] <- vif(model_list[[i]]),
            silent = TRUE
        )
    }

    # - Pooled OLS vs. random effects: Breusch-Pagan Lagrange multiplier: ‘plmtest’
    for (i in 1:length(model_list)) {
        try(
            test_list[[length(test_list)+1]] <-
                plmtest(model_list[[i]], type = "bp"),
            silent = TRUE
        )
    }

    # - Testing for time-fixed effects: ‘plmtest
    for (i in 1:length(model_list)) {
        try(
            test_list[[length(test_list)+1]] <-
                plmtest(model_list[[i]], effect = "time", type = "bp"),
            silent = TRUE
        )
    }

    # - Testing for heteroskedasticity: ‘bptest’ / White’s test

    # Multiple Breusch-Pagan Tests
    for (i in 1:length(model_list)) {
        try(
            test_list[[length(test_list)+1]] <- bptest(model_list[[i]]),
            silent = TRUE
        )
    }

    # - Random vs. fixed effects: Hausman test: ‘phtest’
    for (i in 1:length(model_list)) {
        for (j in (i + 1):length(model_list)) {
            try(
                test_list[[length(test_list)+1]] <-
                    phtest(model_list[[i]], model_list[[j]]),
                silent = TRUE
            )
        }
    }

    # - Testing for time-fixed effects: ‘pFtest’
    for (i in 1:length(model_list)) {
        for (j in (i + 1):length(model_list)) {
            try(
                test_list[[length(test_list)+1]] <-
                    pFtest(model_list[[i]], model_list[[j]]),
                silent = TRUE
            )
        }
    }

    # - Testing for weak instrument: F-test / Wald test
    for (i in 1:length(model_list)) {
        try(
            test_list[[length(test_list)+1]] <-
                waldtest(model_list[[i]], test = "F"),
            silent = TRUE
        )
    }

    return(test_list)

}

