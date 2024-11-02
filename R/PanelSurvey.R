RotativePanelSurvey <- R6Class(
  "RotativePanelSurvey",
  public = list(
    implantation = NULL,
    follow_up = NULL,
    type = NULL,
    default_engine = NULL,
    steps = NULL,
    recipes = NULL,
    workflows = NULL,
    design = NULL,
    initialize = function(implantation, follow_up, type, default_engine, steps, recipes, workflows, design) {
      self$implantation <- implantation
      self$follow_up <- follow_up
      self$type <- type
      self$default_engine <- default_engine
      self$steps <- steps
      self$recipes <- recipes
      self$workflows <- workflows
      self$design <- design
    },
    get_implantation = function() {
      return(self$implantation)
    },
    get_follow_up = function(index = length(self$follow_up), monthly = NULL, quarterly = NULL, semiannual = NULL, annual = NULL) {
      return(self$follow_up[index])
    },
    get_type = function() {
      return(self$type)
    },
    get_default_engine = function() {
      return(self$default_engine)
    },
    get_steps = function() {
      return(self$steps)
    },
    get_recipes = function() {
      return(self$recipes)
    },
    get_workflows = function() {
      return(self$workflows)
    },
    get_design = function() {
      return(self$design)
    }
  )
)
