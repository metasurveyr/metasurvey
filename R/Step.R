Step <- R6Class("Step",
    public = list(
        name = NULL,
        edition = NULL,
        survey_type = NULL,
        type = NULL,
        new_var = NULL,
        exprs = NULL,
        call = NULL,
        svy_before = NULL,
        default_engine = NULL,
        depends_on = list(),
        initialize = function(name, edition, survey_type, type, new_var, exprs, call, svy_before, default_engine, depends_on) {
            self$name <- name
            self$edition <- edition
            self$survey_type <- survey_type
            self$type <- type
            self$new_var <- new_var
            self$exprs <- exprs
            self$call <- call
            self$svy_before <- svy_before
            self$default_engine <- default_engine
            self$depends_on <- depends_on
        }
    )
)