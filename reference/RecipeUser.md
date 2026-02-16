# RecipeUser

User identity for the recipe ecosystem. Supports three account types:
individual, institutional_member, and institution.

## Public fields

- `name`:

  Character. User or institution name.

- `email`:

  Character or NULL. Email address.

- `user_type`:

  Character. One of "individual", "institutional_member", "institution".

- `affiliation`:

  Character or NULL. Organizational affiliation.

- `institution`:

  RecipeUser or NULL. Parent institution (for institutional_member).

- `url`:

  Character or NULL. Institution URL.

- `verified`:

  Logical. Whether the account is verified.

- `review_status`:

  Character. One of "approved", "pending", "rejected".

## Methods

### Public methods

- [`RecipeUser$new()`](#method-RecipeUser-new)

- [`RecipeUser$trust_level()`](#method-RecipeUser-trust_level)

- [`RecipeUser$can_certify()`](#method-RecipeUser-can_certify)

- [`RecipeUser$to_list()`](#method-RecipeUser-to_list)

- [`RecipeUser$print()`](#method-RecipeUser-print)

- [`RecipeUser$clone()`](#method-RecipeUser-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RecipeUser

#### Usage

    RecipeUser$new(
      name,
      user_type,
      email = NULL,
      affiliation = NULL,
      institution = NULL,
      url = NULL,
      verified = FALSE,
      review_status = "approved"
    )

#### Arguments

- `name`:

  Character. User or institution name.

- `user_type`:

  Character. One of "individual", "institutional_member", "institution".

- `email`:

  Character or NULL. Email address.

- `affiliation`:

  Character or NULL. Organizational affiliation.

- `institution`:

  RecipeUser or NULL. Parent institution for institutional_member.

- `url`:

  Character or NULL. Institution URL.

- `verified`:

  Logical. Whether account is verified.

- `review_status`:

  Character. "approved", "pending", or "rejected".

------------------------------------------------------------------------

### Method `trust_level()`

Get trust level (1=individual, 2=member, 3=institution)

#### Usage

    RecipeUser$trust_level()

#### Returns

Integer trust level

------------------------------------------------------------------------

### Method `can_certify()`

Check if user can certify at a given level

#### Usage

    RecipeUser$can_certify(level)

#### Arguments

- `level`:

  Character. Certification level ("reviewed" or "official").

#### Returns

Logical

------------------------------------------------------------------------

### Method `to_list()`

Serialize to list for JSON

#### Usage

    RecipeUser$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print user card

#### Usage

    RecipeUser$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RecipeUser$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Use recipe_user() for the public API:
user <- recipe_user("Juan Perez", email = "juan@example.com")
inst <- recipe_user("IECON", type = "institution")
member <- recipe_user("Maria", type = "institutional_member", institution = inst)
```
