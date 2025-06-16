# CSV XForm Example

This project demonstrates how to easily handle and transform "vertical" CSV files into a more familiar "horizontal" table format using F#.

## What is a Vertical CSV?

A vertical CSV is a format where each row represents a field name and its values for multiple records, rather than each row representing a single record. For example:

| Field                  | Order1    | Order2    |
|------------------------|-----------|-----------|
| OrderID                | ORD001    | ORD002    |
| Customer.Name          | John Doe  | Jane Smith|
| Item[0].SKU            | SKU001    | SKU003    |
| ...                    | ...       | ...       |

This is in contrast to the more common "horizontal" CSV, where each row is a record and each column is a field.

An example Horizontal CSV would look like this:

| OrderID | Customer.Name | Item[0].SKU | Item[1].SKU |
|---------|----------------|--------------|--------------|
| ORD001  | John Doe       | SKU001      | SKU002      |
| ORD002  | Jane Smith     | SKU003      | SKU004      |

## Purpose

The main purpose of this project is to serve as an example of how F# can be used to:

- Parse vertical CSV files
- Dynamically map fields to structured data types
- Output the data in a horizontal, tabular format

## Why F#?

F# makes this kind of data transformation easy due to:

- Powerful pattern matching
- Flexible record and list types
- Concise and expressive syntax for data manipulation

## How It Works

- The program reads a vertical CSV file.
- It dynamically constructs records for each order, mapping fields and handling nested structures (like items and taxes).
- It then prints the data as a horizontal table, with each order as a column and each field as a row.

## Usage

Build and run the project, passing the path to a vertical CSV file as an argument:

```sh
cd CsvXForm 
dotnet run $(pwd)/vertical_orders.csv 
```

