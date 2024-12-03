# phsuR

phsuR is an R package for geocoding and spatial analysis, designed to support the Population Health Surveillance Unit (PHSU). It simplifies retrieving geographic data, performing spatial joins, and attaching regional boundary information to datasets.
Features

* Geocode addresses using the BC Address Geocoder API.
* Attach regional data to geographic coordinates.
* Built-in dataset for Community Health Service Areas (CHSAs) in British Columbia.

# Installation

Install the development version from GitHub:

```R
install.packages("remotes")
remotes::install_github("your-username/phsuR")
```

# Usage

Geocode Addresses:

```R
library(phsuR)
addresses <- c("601 W Broadway, Vancouver, BC", "1081 Burrard St, Vancouver, BC")
result <- get_geocode_data(addresses)
```

Spatial Join:

```R
example_data <- data.frame(longitude = c(-123.115, -123.121), latitude = c(49.262, 49.282))
result <- spatial_join_with_boundaries(example_data)
```

Combined Workflow:

```R
geo_data <- get_geocode_with_region(addresses)
```

# Data

CHSA Boundaries

Access the built-in dataset for CHSA boundaries in British Columbia:

```R
data("chsa_boundaries")
```

# Future Work

The package is currently tailored to the PHSU's needs, but future enhancements aim to make it more general-purpose for spatial and health data analysis. Planned updates include:

* Supporting additional requests to the BC Address Geocoder API
* Expanded utility for non-spatial health data analysis

Suggestions for new features are welcome! Open an issue or submit a pull request.
