# RecipeCertification

Quality certification for recipes with three tiers: community (default),
reviewed (peer-reviewed by institutional member), and official
(certified by institution).

## Public fields

- `level`:

  Character. Certification level.

- `certified_by`:

  RecipeUser or NULL. The certifying user.

- `certified_at`:

  POSIXct. Timestamp of certification.

- `notes`:

  Character or NULL. Additional notes.

## Methods

### Public methods

- [`RecipeCertification$new()`](#method-RecipeCertification-new)

- [`RecipeCertification$numeric_level()`](#method-RecipeCertification-numeric_level)

- [`RecipeCertification$is_at_least()`](#method-RecipeCertification-is_at_least)

- [`RecipeCertification$to_list()`](#method-RecipeCertification-to_list)

- [`RecipeCertification$print()`](#method-RecipeCertification-print)

- [`RecipeCertification$clone()`](#method-RecipeCertification-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RecipeCertification

#### Usage

    RecipeCertification$new(
      level,
      certified_by = NULL,
      notes = NULL,
      certified_at = NULL
    )

#### Arguments

- `level`:

  Character. One of "community", "reviewed", "official".

- `certified_by`:

  RecipeUser or NULL. Required for reviewed/official.

- `notes`:

  Character or NULL. Additional notes.

- `certified_at`:

  POSIXct or NULL. Auto-set if NULL.

------------------------------------------------------------------------

### Method `numeric_level()`

Get numeric level for ordering (1=community, 2=reviewed, 3=official)

#### Usage

    RecipeCertification$numeric_level()

#### Returns

Integer

------------------------------------------------------------------------

### Method `is_at_least()`

Check if certification is at least a given level

#### Usage

    RecipeCertification$is_at_least(level)

#### Arguments

- `level`:

  Character. Level to compare against.

#### Returns

Logical

------------------------------------------------------------------------

### Method `to_list()`

Serialize to list for JSON

#### Usage

    RecipeCertification$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print certification badge

#### Usage

    RecipeCertification$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RecipeCertification$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Use recipe_certification() for the public API:
cert <- recipe_certification()
inst <- recipe_user("IECON", type = "institution")
official <- recipe_certification("official", certified_by = inst)
```
