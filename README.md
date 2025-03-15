# Canadian Census Data Retrieval using Cancensus API

## Project Overview

This repository contains R code that utilizes the Cancensus API to retrieve census data for the top 50 most populated cities in Canada. The specific census vectors collected are detailed in a separate file within the repository. Data retrieval covers the census years 2011, 2016, and 2021, with the resulting datasets stored as CSV files for further analysis and use.

## Data Source

The primary data source for this project is the **Cancensus API**, which provides comprehensive Canadian census data.

- **API Documentation:** [Cancensus API Documentation](https://mountainmath.github.io/cancensus/)

## Installation & Setup

To use the scripts in this repository, ensure that you have R installed along with the following dependencies:

- `{tidyverse}`
- `{readr}`
- `{cancensus}`
- `{rjson}`
- `{jsonlite}`
- `{httr}`
- `{stringr}`

Install these libraries in your R environment using:

```R
install.packages(c("tidyverse", "readr", "cancensus", "rjson", "jsonlite", "httr", "stringr"))
```

### Cancensus API Setup

To access the Cancensus API, you'll need an API key:

1. Sign up for a Cancensus API key [here](https://censusmapper.ca/api).
2. Configure your API key in R using the following command:

```R
library(cancensus)
options(cancensus.api_key = "your_cancensus_api_key")
```

Replace `"your_cancensus_api_key"` with your actual API key.

## Usage

Follow these steps to execute the R scripts and fetch census data:

1. **Clone this repository:**

```bash
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
```

2. **Identify census vectors:**

- Review the file containing census vector IDs (e.g., `vectors_list.csv`) provided within the repository to confirm or adjust the vectors you wish to retrieve.

3. **Run the R script:**

Open your R environment and open the provided script:

The script will fetch data for the top 50 Canadian cities across the specified years (2011, 2016, 2021) and store CSV outputs accordingly.


## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

