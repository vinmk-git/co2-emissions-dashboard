# CO₂ Emissions Explorer 🌍

An interactive Shiny dashboard for exploring global carbon dioxide emissions from 1970-2023, with breakdowns by country, sector, and per-capita metrics.

**Live demo:** [https://vinmk-shiny.shinyapps.io/co2_viz_0/](https://vinmk-shiny.shinyapps.io/co2_viz_0/)

## Features

- **Interactive time slider** - Explore emissions trends across 50+ years
- **Sector breakdown** - Visualize emissions by power, industry, transport, agriculture, and more
- **Country comparison** - Select specific countries or view all nations simultaneously
- **Per capita toggle** - Switch between total emissions and per-person footprints
- **Multiple visualizations**:
  - Line charts showing emission trends over time
  - Pie chart of sectoral contributions
  - Stacked area charts (absolute and proportional)
- **Data export** - Download filtered datasets for further analysis

## Dataset

This project uses data from the **EDGAR (Emissions Database for Global Atmospheric Research) 2024 GHG Report**.

**Citation:**
> Crippa M., Guizzardi D., Pagani F., Banja M., Muntean M., Schaaf E., Becker, W., Monforti-Ferrario F., Quadrelli, R., Risquez Martin, A., Taghavi-Moharamli, P., Grassi, G., Rossi, S., Melo, J., Oom, D., Branco, A., San-Miguel, J., Manca, G., Pisoni, E., Vignati, E., Pekar, F., *GHG emissions of all world countries – JRC/IEA 2024 Report*, Luxembourg, 2024, https://data.europa.eu/doi/10.2760/4002897, JRC138862.

**Source:** https://edgar.jrc.ec.europa.eu/report_2024  
**Contact:** JRC-EDGAR@ec.europa.eu

## Installation

```r
# Install required packages
install.packages(c(
  "shiny", "shinyWidgets", "shinycssloaders", "bslib", "shinyBS", "thematic",
  "tidyverse", "readxl", "ggplot2", "plotly", "viridis"
))

# Run the app
shiny::runApp()
```

## Technologies Used

- **R Shiny** - Interactive web application framework
- **bslib** - Bootstrap theming
- **ggplot2 & plotly** - Data visualization
- **tidyverse** - Data manipulation
- **viridis** - Colorblind-friendly color palettes

## Project Structure

```
├── app.R                 # Main Shiny application
├── Downloads/            # EDGAR dataset (Excel files)
└── README.md
```

## Acknowledgments

- **Data source:** European Commission Joint Research Centre (JRC) EDGAR database
- **Development assistance:** Claude AI (Anthropic) was used for debugging, code optimization, and cleaning up reactive expressions
- **Color palettes:** viridis package for accessible, perceptually-uniform colors

## License

Data usage is subject to EDGAR terms and conditions. See the dataset's "citation and references" sheet for details.

---

*Built with R Shiny • Deployed on shinyapps.io*
