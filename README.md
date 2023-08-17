# Standardized Ecological Analysis
**S**tandardized **E**cological **A**na**l**ysis, or SEAL in short, is a standardized method to store, manipulate, and analyze multivariate data. SEAL data is easily readable and manipulable within `R` or any other program that reads `.xlsx` files.

## SEAL data in a nutshell
A common multivariate data structure looks as follows where within an observation (represented by `#`) consists of many factors and items, which those items have said properties too.

| # |          |          |          |         |          |          |            |
| - | -------- | -------- | -------- | ------- | -------- | -------- | ---------- |
|   |          |          |          | a       | b        | c        | Property 1 |
|   |          |          |          | d       | e        | f        | Property 2 |
|   |          |          |          | g       | h        | i        | Property 3 |
|   |          |          |          |         |          |          |            |
| # | Factor 1 | Factor 2 | Factor 3 | Item 1  | Item 2   | Item 3   |            |
| 1 | a        | d        | g        |         |          |          |            |
| 2 | b        | e        | h        |         |          |          |            |
| 3 | c        | f        | i        |         |          |          |            |

This package separates the above structure into the following, what we call `matrix`.
Matrix data
| # | Factor 1 | Factor 2 | Factor 3 | Item 1  | Item 2   | Item 3   |
| - | -------- | -------- | -------- | ------- | -------- | -------- |
| 1 | a        | d        | g        |         |          |          |
| 2 | b        | e        | h        |         |          |          |
| 3 | c        | f        | i        |         |          |          |

Matrix item
| # | Item | Property 1 | Property 2 | Property 3 |
| - | ---- | ---------- | ---------- | ---------- |
| 1 | 1    | a          | d          | g          |
| 2 | 2    | b          | e          | h          |
| 3 | 3    | c          | f          | i          |

Matrix factor
| # | Factor | Label      |
| - | ----   | ---------- |
| 1 | 1      | a          |
| 2 | 1      | b          |
| 3 | 1      | c          |
| 4 | 2      | d          |
| 5 | 2      | e          |
| 6 | 2      | f          |
| 7 | 3      | g          |
| 8 | 3      | h          |
| 9 | 3      | i          |
