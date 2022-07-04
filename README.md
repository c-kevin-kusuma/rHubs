# rHubs

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## Overview

Building an R package to deal with retrieving, analyzing, and updating data from HubSpot. This package is designed to pull and push data between R and HubSpot via API calls using generated API Key. In order to use this package, a valid HubSpot instance and an account with sufficient access are required. All inputs and outputs in this package will be in dataframes. Functions in this package will automatically perform translations between JSON and dataframe to match the required input and output of HubSpot objects.

<br>

## How to Install
Currently, this package is only available through GitHub. You can install this package using devtools github method:
```r
devtools::install_github(repo='c-kevin-kusuma/rHubs', ref='main')
```
