# Parse variables from a DDI XML file

Reads a DDI Codebook XML file and extracts variable-level metadata:
name, label, type (discrete/continuous), and value labels.

## Usage

``` r
anda_parse_variables(ddi_xml_path)
```

## Arguments

- ddi_xml_path:

  Character path to the DDI XML file

## Value

A list of variable metadata, each with: name, label, type, value_labels
(named list or NULL), description
