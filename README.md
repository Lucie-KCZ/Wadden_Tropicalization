# Community Tropicalisation Explorer 🌍 

Welcome to the **Community Tropicalisation Explorer**!  
This project provides a complete workflow to analyse, visualise, and understand how marine communities respond to climate change, focusing on **Community Temperature Index (CTI)** dynamics and their ecological drivers.

It includes:
- An easy-to-use **Shiny web application**.
- A full set of **R scripts for manual analysis**.
- Supporting tools for **sea surface temperature (SST) data extraction and processing**.

---

## Ecological background and purpose

Marine communities are shifting under climate change.  
Community responses can include:

- **Tropicalisation** (gain of warm-affinity species)
- **Borealisation** (gain of cold-affinity species)
- **Deborealisation** (loss of cold-affinity species)
- **Detropicalisation** (loss of warm-affinity species)

Tracking CTI over time, and decomposing the processes behind it, is essential for monitoring biodiversity resilience, identifying hotspots of change, and informing management strategies.

This project aims to **make CTI analysis accessible and reproducible** for ecologists, conservation practitioners, and stakeholders.

---

## 🚀 How to use this project

### 1. Quick Start — Launch the Shiny App

1. Clone or download the repository:

```bash
git clone https://github.com/Lucie-KCZ/Wadden_Tropicalization.git
cd Wadden_Tropicalization/App
```

2. Install required R packages:

```r
install.packages(c(
  "shiny", "leaflet", "shinyFiles", "ggplot2", "terra",
  "nlme", "ggeffects", "scales", "dplyr", "leaflet.minicharts"
))
```

3. Run the app:

```r
shiny::runApp("app.R")
```

or open `App/app.R` in RStudio and click **Run App**,
or access directly the [app online](https://lucie-kcz.shinyapps.io/tropicalisation/).

---

### 2. Advanced Use — Run Full Analysis Manually

For users who want full control outside of the app:

- Navigate to the `analysis/` folder.
- Scripts are modular:
  - `compute_cti.R`: compute Community Temperature Index.
  - `compute_sti.r`: compute Species Thermal Indices.
  - `extract_temperatures.r`: link samples to SST.
  - `full_analysis.R`: run the entire CTI analysis pipeline from input data.
- SST handling scripts (`temperatures/R/`) are available if you want to recompute temperature rasters or extract SST from raw data.

---

## 📂 Project Structure

```
App/
├── app.R
├── Documentation.docx
├── example_no_temp.csv
├── example_with_temp.csv

analysis/
├── compute_cti.R
├── compute_sti.r
├── extract_temperatures.r
├── full_analysis.R

temperatures/
├── R/
│   ├── avg_sst.R
│   └── extract_sst.R

data/processed/
├── AquaMODIS_part1.zip
├── AquaMODIS_part2.zip
├── average_sst.tif
├── TerraModis_STT_url.txt

Other files:
├── download.py
├── terminal.txt
├── README.md
├── compare_temperatures.html
├── tutorial.html
```

---

## 📊 Data Requirements

Your CSV file must include:

| Column      | Description                                  |
|-------------|----------------------------------------------|
| StationID   | Site or station identifier (text)            |
| year        | Year of sampling (numeric)                   |
| lat         | Latitude (decimal degrees)                   |
| long        | Longitude (decimal degrees)                  |
| temperature | Sample temperature (°C) *(optional)*         |
| [Species]   | One column per species (abundance or presence)|

---

## 📜 How the Code Works

1. **Species Thermal Index (STI)**: compute mean temperature across species occurrences.
2. **Community Temperature Index (CTI)**: weighted mean of species’ STIs per community sample.
3. **Trend Analysis**: model CTI changes over time.
4. **Process Decomposition**: classify species trends into ecological processes.
5. **Outputs**: plots, maps, and trend tables.

---

## 📜 License

This project is distributed under the terms of the **GNU General Public License v3.0**:

> This program is free software: you can redistribute it and/or modify  
> it under the terms of the GNU General Public License as published by  
> the Free Software Foundation, either version 3 of the License, or  
> (at your option) any later version.  
> 
> This program is distributed in the hope that it will be useful,  
> but WITHOUT ANY WARRANTY; without even the implied warranty of  
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the  
> GNU General Public License for more details.  
> 
> For the full text, see: [https://www.gnu.org/licenses/](https://www.gnu.org/licenses/)

---

## 🤝 Contributing

Pull requests, bug reports, and feature suggestions are welcome!  
Please open an Issue or submit a PR if you have ideas to improve the project.

---

Developed to bridge the gap between ecological research methods and operational biodiversity monitoring, promoting **open science**, **data accessibility**, and **community empowerment**.
