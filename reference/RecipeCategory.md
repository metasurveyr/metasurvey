# RecipeCategory

Standardized taxonomy for classifying recipes by domain. Supports
hierarchical categories with parent-child relationships.

## Value

An object of class `RecipeCategory`.

## Methods

- \$new(name, description, parent):

  Constructor for creating a new category

- \$is_subcategory_of(ancestor_name):

  Check if this category is a subcategory of another

- \$get_path():

  Get full hierarchical path

- \$equals(other):

  Check equality by name

- \$to_list():

  Serialize to list for JSON

- \$print(...):

  Print category information

- \$from_list(lst):

  Class method to reconstruct from list (see details)

## Public fields

- `name`:

  Character. Category identifier.

- `description`:

  Character. Human-readable description.

- `parent`:

  RecipeCategory or NULL. Parent category for hierarchy.

## Methods

### Public methods

- [`RecipeCategory$new()`](#method-RecipeCategory-new)

- [`RecipeCategory$is_subcategory_of()`](#method-RecipeCategory-is_subcategory_of)

- [`RecipeCategory$get_path()`](#method-RecipeCategory-get_path)

- [`RecipeCategory$equals()`](#method-RecipeCategory-equals)

- [`RecipeCategory$to_list()`](#method-RecipeCategory-to_list)

- [`RecipeCategory$print()`](#method-RecipeCategory-print)

- [`RecipeCategory$from_list()`](#method-RecipeCategory-from_list)

- [`RecipeCategory$clone()`](#method-RecipeCategory-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RecipeCategory

#### Usage

    RecipeCategory$new(name, description, parent = NULL)

#### Arguments

- `name`:

  Character. Category identifier (non-empty string).

- `description`:

  Character. Description of the category.

- `parent`:

  RecipeCategory or NULL. Parent category.

------------------------------------------------------------------------

### Method `is_subcategory_of()`

Check if this category is a subcategory of another

#### Usage

    RecipeCategory$is_subcategory_of(ancestor_name)

#### Arguments

- `ancestor_name`:

  Character. Name of the potential ancestor category.

#### Returns

Logical

------------------------------------------------------------------------

### Method `get_path()`

Get full hierarchical path

#### Usage

    RecipeCategory$get_path()

#### Returns

Character string with slash-separated path

------------------------------------------------------------------------

### Method [`equals()`](https://magrittr.tidyverse.org/reference/aliases.html)

Check equality by name

#### Usage

    RecipeCategory$equals(other)

#### Arguments

- `other`:

  RecipeCategory to compare with.

#### Returns

Logical

------------------------------------------------------------------------

### Method `to_list()`

Serialize to list for JSON

#### Usage

    RecipeCategory$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print category

#### Usage

    RecipeCategory$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

------------------------------------------------------------------------

### Method `from_list()`

Deserialize a RecipeCategory from a list

#### Usage

    RecipeCategory$from_list(lst)

#### Arguments

- `lst`:

  List with name, description, parent fields, or NULL

#### Returns

RecipeCategory object or NULL

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RecipeCategory$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Use recipe_category() for the public API:
cat <- recipe_category("economics", "Economic indicators")
sub <- recipe_category("labor_market", "Labor market", parent = "economics")
```
