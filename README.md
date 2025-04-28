# Community Tropicalisation Explorer 🌍 

Welcome to the **Community Tropicalisation Explorer**!  
This R Shiny application allows users to analyse, visualise, and understand how marine communities respond to climate change, focusing on **Community Temperature Index (CTI)** dynamics and their ecological drivers. It is already available to use as it is on the following link: [Shinny App - Tropicalisation Explorer](https://lucie-kcz.shinyapps.io/tropicalisation/)

The app is designed to make **scientific ecological analyses accessible**, without requiring R scripting skills, while remaining fully open-source for customisation and extension.

---

## Ecological background and purpose

As sea surface temperatures rise, marine communities often undergo **tropicalisation** — a shift toward greater dominance of species adapted to warmer climates.

However, climate change can affect communities through multiple pathways:

- Gains of warm-affinity species (**Tropicalisation**)
- Gains of cold-affinity species (**Borealisation**)
- Losses of cold-affinity species (**Deborealisation**)
- Losses of warm-affinity species (**Detropicalisation**)

Each of these processes reflects **different ecological dynamics** and **different conservation concerns**.

The Community Tropicalisation Explorer enables users to:

- **Compute CTI** trends per station and globally
- **Decompose CTI changes** into ecological processes
- **Visualise spatial and temporal dynamics**
- **Export publication-ready plots and tables**

This supports biodiversity monitoring, climate adaptation planning, stakeholder reporting, and ecological education.

---

## 🚀 Quick Start

### 1. Clone this repository

```bash
git clone https://github.com/your_username/community-tropicalisation-explorer.git
cd community-tropicalisation-explorer
```

### 2. Install R packages

```r
install.packages(c(
  "shiny", "leaflet", "shinyFiles", "ggplot2", "terra",
  "nlme", "ggeffects", "scales", "dplyr", "leaflet.minicharts"
))
```

### 3. Launch the app

```r
shiny::runApp("path_to_folder_where_you_cloned_the_app")
```

Or open `app.R` and click **Run App**.

---

## 📂 Project Structure

```
community-tropicalisation-explorer/
├── app.R            # Main Shiny app with UI, server, and core computation scripts
├── example_data/    # (optional) Example CSVs for testing
├── LICENSE          # GNU General Public License v3.0
├── README.md        # This file
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
| [Species]   | One column per species (abundance or presence) |

✅ If `temperature` is missing, the app will assign MODIS SST temperatures automatically.  
✅ You can work with **abundance** or **presence-absence** data (selectable inside the app).

---

## 📜 How the App Works (Ecological and Technical Logic)

### 1. Species Thermal Index (STI)

Each species’ thermal preference is computed as the **mean temperature across all samples where it occurs**.

### 2. Community Temperature Index (CTI)

CTI for each sample is calculated as the **weighted average** of species' STIs, using their relative abundances (or presences).

### 3. Temporal Trends

The app fits:

- **Per-station linear models**: slope of CTI over time
- **Global mixed-effects model**: overall CTI trend across stations

### 4. Decomposition into Ecological Processes

Species trends are classified into processes based on:

- **Trend direction** (increase or decrease)
- **Thermal affinity** (warmer or colder than site average)

Each sample is summarised into **process strengths (%)**, highlighting whether CTI changes are driven more by tropicalisation, borealisation, detropicalisation, or deborealisation.

---

## 🛠️ Code Organisation (for Developers)

The entire logic is contained within `app.R` and structured clearly:

- **User Interface (UI)**: defines page layout, tabs, inputs
- **Server**: handles user inputs, runs analysis, generates outputs
- **Computation functions**:
  - `compute_STI(df)`
  - `get_CTI(community_matrix, STI, infos_col, occu)`
  - `get_processes(community_matrix, STI, CTI, infos_col)`
  - `plot_trend(CTI)`
  - `plot_species(process_df)`
  - `make_cti_trends_table(CTI, proc)`

📜 All scripts are **open**, **readable**, and **modular** to allow fast understanding and customisation.

---

## ⚡ How to Use or Modify

- If you just want to use the app: **no modification needed** — just run `app.R`.
- If you want to modify it (e.g., for different traits, different visual styles):
  - Edit the embedded functions in `app.R`.
  - UI elements can be modified easily inside `fluidPage()` and `sidebarLayout()`.

If you adapt the app, please consider sharing improvements back with the community!

---

## 📜 License

This project is licensed under the **GNU General Public License v3.0**.

**GNU GENERAL PUBLIC LICENSE**
Version 3, 29 June 2007

Copyright (C) 2025 Lucie Kuczynski

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see [https://www.gnu.org/licenses/](https://www.gnu.org/licenses/).

---

## 🤝 Contributing

Pull requests, bug reports, or feature suggestions are very welcome!  
You can open an Issue or fork the project on GitHub.

---

## 🌊 Acknowledgements

This app was developed to help bridge the gap between research methods and operational biodiversity monitoring, under the principles of **open science**, **accessible tools**, and **community empowerment**.
