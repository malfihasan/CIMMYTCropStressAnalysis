# 🌾 CIMMYTCropStressAnalysis 🌾

[![License: Custom](https://img.shields.io/badge/License-CIMMYT%20Bangladesh%20and%20Md%20Alfi%20Hasan,%20PhD-brightgreen)](https://github.com/malfihasan/CIMMYTCropStressAnalysis/LICENSE)
[![R](https://img.shields.io/badge/Made%20with-R-blue)](https://www.r-project.org/)
[![Contributions Welcome](https://img.shields.io/badge/Contributions-Welcome-brightgreen)](https://github.com/malfihasan/CIMMYTCropStressAnalysis/issues)

A collaborative Git project for CIMMYT Bangladesh and M. Alfi Hasan, PhD.

## Overview

This project conducts a comprehensive analysis of crop suitability in Bangladesh. The analysis involves calculating the cumulative distribution function for various crops based on their optimum temperature thresholds. The probability of exceeding these thresholds is used to generate interpolated surfaces using the best-fitted kriging method. Some suitability maps are masked to identify risk zones for different crops. Additionally, the project compares different climate periods to understand changes in crop suitability over time.

## Features

- 📊 **Data Processing**: Utilizes historical temperature data from 1984 to 2016.
- 🌡️ **Threshold Analysis**: Determines suitability based on maximum and minimum temperature thresholds.
- 🗺️ **Spatial Analysis**: Generates raster maps using kriging interpolation.
- 🚀 **Automated Pipeline**: Streamlines data splitting, threshold calculation, and map generation.

## Requirements

- R (version 3.6 or higher)
- R libraries: `raster`, `rgdal`, `maptools`, `sp`, `automap`, `colorspace`, `RColorBrewer`

## Installation

1. Clone the repository:
    ```bash
    git clone https://github.com/malfihasan/CIMMYTCropStressAnalysis.git
    ```
2. Install the required R packages:
    ```r
    install.packages(c('raster', 'rgdal', 'maptools', 'sp', 'automap', 'colorspace', 'RColorBrewer'))
    ```

## Contributing

We welcome contributions! Please check the [issues](https://github.com/malfihasan/CIMMYTCropStressAnalysis/issues) and submit a pull request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under CIMMYT Bangladesh and Md Alfi Hasan, PhD. See the [LICENSE](LICENSE) file for details.

