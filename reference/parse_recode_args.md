# Parse a STATA recode command

Handles patterns like:

- recode var (old=new) (old=new)

- recode var (old=new), gen(newvar)

- recode var .=0

- recode var 23/38=22

## Usage

``` r
parse_recode_args(args, options = NULL)
```

## Arguments

- args:

  Arguments string after "recode"

- options:

  Options string (may contain gen())

## Value

List with var_name, gen_var (or NULL), and mappings list
