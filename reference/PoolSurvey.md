# PoolSurvey Class

This class represents a collection of surveys grouped by specific
periods (e.g., monthly, quarterly, annual). It provides methods to
access and manipulate the grouped surveys.

## Value

An object of class `PoolSurvey`.

## See also

Other panel-surveys:
[`RotativePanelSurvey`](https://metasurveyr.github.io/metasurvey/reference/RotativePanelSurvey.md),
[`extract_surveys()`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md),
[`get_follow_up()`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md),
[`get_implantation()`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)

## Public fields

- `surveys`:

  A list containing the grouped surveys.

## Methods

### Public methods

- [`PoolSurvey$new()`](#method-PoolSurvey-new)

- [`PoolSurvey$get_surveys()`](#method-PoolSurvey-get_surveys)

- [`PoolSurvey$print()`](#method-PoolSurvey-print)

- [`PoolSurvey$clone()`](#method-PoolSurvey-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a new instance of the PoolSurvey class.

#### Usage

    PoolSurvey$new(surveys)

#### Arguments

- `surveys`:

  A list containing the grouped surveys.

------------------------------------------------------------------------

### Method `get_surveys()`

Retrieves surveys for a specific period.

#### Usage

    PoolSurvey$get_surveys(period = NULL)

#### Arguments

- `period`:

  A string specifying the period to retrieve (e.g., "monthly",
  "quarterly").

#### Returns

A list of surveys for the specified period.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints metadata about the PoolSurvey object.

#### Usage

    PoolSurvey$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PoolSurvey$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
pool <- PoolSurvey$new(list(annual = list("group1" = list(s1, s2))))
} # }
```
