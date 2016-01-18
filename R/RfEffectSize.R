


RfEffectSize <- function(forest, training_data, varlist = NULL, ...){
    ### Initial Setup of Things ------------------------------------------------

    # check validity of inputs
    # (Or I'll do it later)
    # 1. varlist vars are in data
    # 2. acceptable classes of data
    #    logical, factor, character, integer, numeric
    # 3. Make sure forest$y lines up with nrow(training_data)
    # 4. No NA values in training_data

    # if varlist is NULL, get it from forest
    if(is.null(varlist)) varlist <- names(forest$forest$xlevels)

    # Get the proper parameters for re-training
    cat_classes <- c("factor", "character", "logical")

    #### Declare a bunch of functions that we're going to need -----------------
    DoComparison <- function(p1, p2, level_name){
        if("matrix" %in% class(p1)){
            result <- lapply(1:ncol(p1), function(j){
                effect_size <- mean(p1[ , j ] - p2[ , j ])

                test <- t.test(p1[ , j ], p2[ , j ])

                data.frame(level=level_name,
                           effect_size=effect_size,
                           p_value=test$p.value,
                           conf_95_low=test$conf.int[ 1 ],
                           conf_95_high=test$conf.int[ 2 ],
                           stringsAsFactors=FALSE)
            })

            names(result) <- colnames(p1)
        }else{
            effect_size <- mean(p1 - p2)

            test <- t.test(p1, p2)

            result <- list(response=data.frame(level=level_name,
                                               effect_size=effect_size,
                                               p_values=test$p.value,
                                               conf_95_low=test$conf.int[ 1 ],
                                               conf_95_high=test$conf.int[ 2 ],
                                               stringsAsFactors=FALSE))
        }

        return(result)
    }


    ExecuteContinuous <- function(variable, training_data, varlist, forest, cat_classes){
        # split data above and below the mean
        var_mean <- mean(training_data[[ variable ]])

        level_name <- paste(variable, ">=", var_mean)

        sub <- training_data[[ variable ]] >= var_mean

        d1 <- training_data[ sub , ]
        d1 <- cbind(d1, y=forest$y[ sub ])
        d1 <- d1[ , c("y", setdiff(varlist, variable)) ]


        d2 <- training_data[ ! sub , ]
        d2 <- cbind(d1, y=forest$y[ ! sub ])
        d2 <- d2[ , c("y", setdiff(varlist, variable)) ]

        # Make a formula
        f <- paste("y ~ ",
                   paste(setdiff(varlist, variable), collapse = " + "),
                   sep="")
        f <- as.formula(f)

        # fit models for both levels
        m1 <- randomForest::randomForest(f, data = d1,
                                         ntree = forest$ntree,
                                         mtry = forest$mtry)

        m2 <- randomForest::randomForest(f, data = d2,
                                         ntree = forest$ntree,
                                         mtry = forest$mtry)

        # make predictions
        p1 <- predict(m1, newdata = training_data,
                      type = if(forest$type == "classification"){"prob"}else{"response"})
        p2 <- predict(p2, newdata = training_data,
                      type = if(forest$type == "classification"){"prob"}else{"response"})

        # compare
        result <- list(DoComparison(p1, p2, level_name))

        names(result) <- level_name

        result
    }

    ExecuteBinary <- function(variable, training_data, varlist, forest, cat_classes){
        # get levels and split data for each level
        var_levels <- as.character(sort(unique(training_data[[ variable ]])))

        sub <- training_data[[ variable ]] == var_levels[ 1 ]

        d1 <- training_data[ sub , ]
        d1 <- cbind(d1, y=forest$y[ sub ])
        d1 <- d1[ , c("y", setdiff(varlist, variable)) ]


        d2 <- training_data[ ! sub , ]
        d2 <- cbind(d2, y=forest$y[ ! sub ])
        d2 <- d2[ , c("y", setdiff(varlist, variable)) ]

        # Make a formula
        f <- paste("y ~ ",
                   paste(setdiff(varlist, variable), collapse = " + "),
                   sep="")
        f <- as.formula(f)

        # fit models for both levels
        m1 <- randomForest::randomForest(f, data = d1, ntree = forest$ntree,
                                         mtry = forest$mtry)

        m2 <- randomForest::randomForest(f, data = d2, ntree = forest$ntree,
                                         mtry = forest$mtry)

        # make predictions
        p1 <- predict(m1, newdata = training_data,
                      type = if(forest$type == "classification"){"prob"}else{"response"})
        p2 <- predict(m2, newdata = training_data,
                      type = if(forest$type == "classification"){"prob"}else{"response"})

        # Do the comparison
        result <- list(DoComparison(p1, p2, level_name = var_levels[ 1 ]),
                       DoComparison(p2, p1, level_name = var_levels[ 2 ]))

        names(result) <- var_levels

        result

    }

    ExecuteMulti <- function(variable, training_data, varlist, forest, cat_classes){
        var_levels <- as.character(sort(unique(training_data[[ variable ]])))

        result <- lapply(var_levels, function(x){
          sub <- training_data[[ variable ]] == x

          d1 <- training_data[ sub , ]
          d1 <- cbind(d1, y=forest$y[ sub ])
          d1 <- d1[ , c("y", setdiff(varlist, variable)) ]


          d2 <- training_data[ ! sub , ]
          d2 <- cbind(d2, y=forest$y[ ! sub ])
          d2 <- d2[ , c("y", setdiff(varlist, variable)) ]

          # Make a formula
          f <- paste("y ~ ",
                     paste(setdiff(varlist, variable), collapse = " + "),
                     sep="")
          f <- as.formula(f)

          # fit models for both levels
          m1 <- randomForest::randomForest(f, data = d1, ntree = forest$ntree,
                                           mtry = forest$mtry)

          m2 <- randomForest::randomForest(f, data = d2, ntree = forest$ntree,
                                           mtry = forest$mtry)

          # make predictions
          p1 <- predict(m1, newdata = training_data,
                        type = if(forest$type == "classification"){"prob"}else{"response"})
          p2 <- predict(m2, newdata = training_data,
                        type = if(forest$type == "classification"){"prob"}else{"response"})

          # Do the comparison
          DoComparison(p1, p2, level_name = x)
        })
        names(result) <- var_levels

        result
    }

    Execute <- function(variable, training_data, varlist, forest, cat_classes){
        var_class <- class(training_data[[ variable ]])

        if(var_class %in% cat_classes){
            num_levels <- length(unique(training_data[[ variable ]]))

            if(num_levels > 2){
                result <- ExecuteMulti(variable = variable,
                                       training_data = training_data,
                                       varlist = varlist,
                                       forest = forest,
                                       cat_classes = cat_classes)
            }else{
                result <- ExecuteBinary(variable = variable,
                                        training_data = training_data,
                                        varlist = varlist,
                                        forest = forest,
                                        cat_classes = cat_classes)
            }
        }else{
            result <- ExecuteContinuous(variable = variable,
                                        training_data = training_data,
                                        varlist = varlist,
                                        forest = forest,
                                        cat_classes = cat_classes)
        }

        result
    }

    #### Get effect size for variables -----------------------------------------
    effect_sizes <- textmineR::TmParallelApply(X = varlist,
                                               export = c("training_data",
                                                          "varlist", "forest",
                                                          "cat_classes"),
                                               FUN = function(variable){
                                                 Execute(variable = variable,
                                                         training_data = training_data,
                                                         varlist = varlist,
                                                         forest = forest,
                                                         cat_classes = cat_classes)
                                               })

#     effect_sizes <- parallel::mclapply(varlist, function(variable){
#         Execute(variable = variable,
#                 training_data = training_data,
#                 varlist = varlist,
#                 forest = forest,
#                 cat_classes = cat_classes)
#     }, mc.cores = parallel::detectCores())

#     effect_sizes <- lapply(varlist[1:5], function(variable){
#       Execute(variable = variable,
#               training_data = training_data,
#               varlist = varlist,
#               forest = forest,
#               cat_classes = cat_classes)
#     })

    names(effect_sizes) <- varlist

    #### Reformat result for return --------------------------------------------
    if(class(forest$y) %in% cat_classes){

      y_levels <- as.character(sort(unique(forest$y)))

      names(y_levels) <- y_levels

      sorted_result <- lapply(y_levels, function(l){
        lapply(effect_sizes, function(x){
          do.call(rbind, lapply(x, function(y) y[[ l ]]))
        })
      })

    }else{
      sorted_result <- lapply(effect_sizes, function(x){
        do.call(rbind, lapply(x, function(y) y$response))
      })
    }
    sorted_result
}