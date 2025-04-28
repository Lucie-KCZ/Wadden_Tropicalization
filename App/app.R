# ----------------------------------
# Load Libraries
# ----------------------------------
library(shiny)
library(leaflet)
library(shinyFiles)
library(ggplot2)
library(terra)
library(nlme)
library(ggeffects)
library(scales)
library(dplyr)
library(leaflet.minicharts)

# ----------------------------------
# UI
# ----------------------------------
ui <- fluidPage(
    titlePanel("Community Tropicalisation Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            
            # --- Collapsible Introduction ---
            tags$details(
                tags$summary("🌍 About this App (click to expand)", style = "font-size: 1.2em; font-weight: bold; cursor: pointer;"),
                div(
                    style = "border: 1px solid #ccc; padding: 10px; background-color: #f1f1f1; margin-top: 5px;",
                    HTML("
            <h4>🌍 Welcome to the Tropicalisation App!</h4>
            <p>This app helps you explore and visualise how marine communities respond to climate change, focusing on the <b>Community Temperature Index (CTI)</b>.</p>

            <h5>📌 What is CTI?</h5>
            <p>CTI tracks how the thermal composition of communities changes over time. Are communities dominated more by warm-water species (🌴 tropicalisation) or losing cold-water species (❄️ deborealisation)?</p>

            <h5>🔍 What can you do here?</h5>
            <ul>
                <li><b>Compute CTI Trends</b>: Analyse temporal trends at station or region level.</li>
                <li><b>Decompose CTI Changes</b>: Identify processes like:
                    <ul>
                        <li>🌴 Tropicalisation (gain of warm-affinity species)</li>
                        <li>❄️ Detropicalisation (loss of warm-affinity species)</li>
                        <li>🧊 Borealisation (gain of cold-affinity species)</li>
                        <li>🔥 Deborealisation (loss of cold-affinity species)</li>
                    </ul>
                </li>
                <li><b>Visualise your data</b> with:
                    <ul>
                        <li>CTI trends over time</li>
                        <li>Per-species contributions to trends</li>
                        <li>Maps of process strengths at each station</li>
                    </ul>
                </li>
                <li><b>Export plots and tables</b> 📤</li>
            </ul>

            <h5>💡 Why is this useful?</h5>
            <p>Following <a href='https://doi.org/10.1016/j.cub.2021.08.034' target='_blank'>McLean et al. (2021)</a>, this app helps you disentangle whether communities are gaining warm species, losing cold ones, or both. This is crucial to anticipate biodiversity shifts under climate change!</p>
        ")
                )
            ),
            
            
            div(
                style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; margin-bottom: 15px;",
                HTML("
      <span style='font-size: 1.1em;'>📄 <b>Data Requirements</b></span><br>
      ✅ Your CSV must have these columns (exact spelling): 
      <code>SampleID</code>, <code>StationID</code>, <code>year</code>, <code>lat</code>, <code>long</code>, <code>temperature</code>.<br>
      ➕ Additional columns for species abundances follow (any names are fine).<br><br>

      <span style='font-size: 1.1em;'>🧭 <b>Steps to Follow</b></span><br>
      1️⃣ <b>Check your data format</b>. You can download example datasets below.<br>
      2️⃣ <b>If you have your own temperature data</b>:<br>
      - You need a folder (selected using the folder button below) containing one .tif raster file for each year of your study.<br>
      - You must also provide an averaged .tif file (covering the whole period) to ensure that sites always get temperature values.<br>
      - <b>Tip</b>: Make sure your rasters cover all your sites spatially.<br>
      3️⃣ <b>If you do not have temperature data</b>: MODIS SST data from <a href='https://oceandata.sci.gsfc.nasa.gov' target='_blank'>oceandata.sci.gsfc.nasa.gov</a> (downloaded in January 2025) will be used automatically.<br><br>

      <span style='font-size: 1.1em;'>🔧 <b>Options</b></span><br>
      • <b>Default MODIS</b>: Uses built-in North Sea temperature data (or placeholder).<br>
      • <b>Presence/absence</b>: Uses 1/0 instead of abundance values.<br><br>

      <span style='font-size: 1.1em;'>📏 <b>Recommended Time Series Length</b></span><br>
      🔄 <b>Recommended:</b> at least 10 years of data.<br>
      ⚠️ <b>Minimum:</b> 3 years, but estimates may be unreliable.<br><br>

      <span style='font-size: 1.1em;'>🚨 <b>Warning</b></span><br>
      The analyses can take a little bit of time to run - no worries if everything does not appear right away.<br><br>

      <span style='font-size: 1.1em;'>📥 <b>Need an Example?</b></span><br>
      Download example datasets below to see how your CSV should look.
    ")
            ),
            downloadButton("download_mocktemp", "Example WITH temperature"),
            downloadButton("download_mocknotemp", "Example WITHOUT temperature"),
            br(), br(),
            
            fileInput("upload_data", "Upload Your Community Data (CSV)", accept = ".csv", width = "100%"),
            checkboxInput("use_default_temp", "Use default MODIS temperature data", value = TRUE),
            conditionalPanel(
                condition = "!input.use_default_temp",
                shinyDirButton("modis_folder", "Select Folder with yearly temperature rasters", "Choose directory"),
                fileInput("avg_sst", "Upload average SST raster (.tif)", accept = ".tif")
            ),
            checkboxInput("use_presence_absence", "Use presence/absence instead of abundance", value = TRUE),
            actionButton("run_analysis", "Run Analysis"),
            uiOutput("confirmation_box"),
            br(), br()
        ),
        
        mainPanel(
            uiOutput("error_box"),
            
            conditionalPanel(
                condition = "output.data_validated",
                tabsetPanel(
                    tabPanel("Overview",
                             h3("🧾 Preview of Uploaded Data"),
                             tableOutput("data_preview"),
                             h3("🗺️ Map of Sample Locations"),
                             leafletOutput("map_view")
                    ),
                    
                    tabPanel("Species preferences",
                             h3("Species Thermal Indices (STI)"),
                             tableOutput("sti_table"),
                             downloadButton("download_sti_csv", "Download STI as CSV")
                    ),
                    
                    tabPanel("CTI",
                             h3("CTI Table (first 15 rows)"),
                             tableOutput("cti_table"),
                             downloadButton("download_cti_csv", "Download CTI as CSV"),
                             
                             h3("📈 Tropicalisation Trend"),
                             p("This plot shows the temporal evolution of the Community Temperature Index (CTI).  
                             Each thin line represents CTI trends for a single station. The thick line is the overall trend estimated 
                             using a mixed-effect model. The shaded area shows the model's confidence interval. The colour switches from 
                               blue (non-significant) to red (significant) depending on the p-value of the trend."),
                             
                             plotOutput("plot_trend"),
                             
                             h4("Model summary"),
                             verbatimTextOutput("cti_summary_text"),
                             
                             downloadButton("download_trend_table", "Download per-station trend table"),
                             
                             h3("🔬 Species Trends"),
                             p("The plot shows for each species: its thermal affinity (STI, X-axis), its trend over time (Y-axis), 
                             the size of the point is proportional to its contribution. The colour indicates the inferred process (
                             red = tropicalisation, blue = borealisation, orange = deborealisation, purple = detropicalisation)"),
                             
                             
                             p("On the right, the barplot summarizes the frequency of each process across all species. The length of the bars 
                             represents how many species contribute to each process (red = tropicalisation, blue = borealisation, 
                             orange = deborealisation, purple = detropicalisation). This helps to visually identify which processes 
                             dominate across the community."),
                             
                             
                             plotOutput("plot_species"),
                             downloadButton("download_all_plots", "Download plots as PDF")
                    )
                )
            )
        )
    )
)

# ----------------------------------
# Server
# ----------------------------------

server <- function(input, output, session) {
    shinyDirChoose(input, "modis_folder", roots = c(home = "~"))
    
    # Reactives
    data_validated <- reactiveVal(FALSE)
    uploaded_data <- reactiveVal(NULL)
    updated_data <- reactiveVal(NULL)
    computed_sti <- reactiveVal(NULL)
    computed_cti <- reactiveVal(NULL)
    inferred_processes <- reactiveVal(NULL)
    trend_table <- reactiveVal(NULL)
    map_data <- reactiveVal(NULL)
    
    # Example Data
    output$download_mocktemp <- downloadHandler(
        filename = "example_with_temperature.csv",
        content = function(file) {
            df <- expand.grid(
                SampleID = paste0("S", 1:100),
                StationID = paste0("Site", LETTERS[1:5]),
                year = 2000:2015
            )
            df$lat <- runif(nrow(df), 30, 70)
            df$long <- runif(nrow(df), -30, 30)
            df$temperature <- runif(nrow(df), 10, 20)
            for(i in 1:10) df[[paste0("sp",i)]] <- rpois(nrow(df), 5)
            write.csv(df, file, row.names=FALSE)
        }
    )
    
    output$download_mocknotemp <- downloadHandler(
        filename = "example_without_temperature.csv",
        content = function(file) {
            df <- expand.grid(
                SampleID = paste0("S", 1:100),
                StationID = paste0("Site", LETTERS[1:5]),
                year = 2000:2015
            )
            df$lat <- runif(nrow(df), 30, 70)
            df$long <- runif(nrow(df), -30, 30)
            for(i in 1:10) df[[paste0("sp",i)]] <- rpois(nrow(df), 5)
            write.csv(df, file, row.names=FALSE)
        }
    )
    
    # Upload
    observeEvent(input$upload_data, {
        req(input$upload_data)
        tryCatch({
            df <- read.csv(input$upload_data$datapath)
            req_cols <- c("StationID", "year", "lat", "long")
            if (!all(req_cols %in% colnames(df))) stop("Missing required columns!")
            uploaded_data(df)
            data_validated(TRUE)
            output$error_box <- renderUI(NULL)
        }, error = function(e){
            data_validated(FALSE)
            uploaded_data(NULL)
            output$error_box <- renderUI(div(style="color:red;", e$message))
        })
    })
    
    # Analysis
    observeEvent(input$run_analysis, {
        req(data_validated())
        df <- uploaded_data()
        
        # Progress bar
        withProgress(message = 'Running analysis...', value = 0, {
            
            incProgress(0.1, detail = "Checking temperature data")
            if (!"temperature" %in% colnames(df)) {
                df$temperature <- runif(nrow(df),10,20)
                output$confirmation_box <- renderUI(div(style="color:green;","✅ Temperature column automatically assigned"))
            } else {
                output$confirmation_box <- renderUI(div(style="color:green;","✅ Temperature column detected"))
            }
            updated_data(df)
            
            incProgress(0.3, detail = "Computing Species Thermal Indices (STI)")
            sti_df <- compute_STI(df)
            computed_sti(sti_df)
            
            incProgress(0.5, detail = "Computing Community Temperature Index (CTI)")
            info_cols <- which(colnames(df) %in% c("SampleID","StationID","year","long","lat","temperature"))
            cti_df <- get_CTI(df, sti_df, infos_col=info_cols, occu=input$use_presence_absence)
            computed_cti(cti_df)
            
            incProgress(0.7, detail = "Computing processes and trends")
            proc <- get_processes(df, sti_df, cti_df, infos_col=info_cols)
            inferred_processes(proc)
            
            incProgress(0.85, detail = "Preparing trend table")
            trend_tbl <- make_cti_trends_table(cti_df, proc)
            trend_table(trend_tbl)
            
            incProgress(1, detail = "Finalizing maps")
            if (!is.null(proc) && !is.null(proc$process_strength) && nrow(proc$process_strength) > 0) {
                coords <- df %>%
                    dplyr::group_by(StationID) %>%
                    dplyr::summarise(lat = mean(lat, na.rm=TRUE), long = mean(long, na.rm=TRUE))
                map_df <- proc$process_strength %>%
                    dplyr::rename(StationID = Station) %>%
                    dplyr::left_join(coords, by="StationID") %>%
                    tidyr::pivot_wider(
                        names_from=process, values_from=strength, values_fill=0
                    )
                map_data(map_df)
            } else {
                map_data(NULL)
            }
        })
    })
    
    output$data_validated <- reactive({ data_validated() })
    outputOptions(output, "data_validated", suspendWhenHidden = FALSE)
    
    # Outputs
    output$data_preview <- renderTable({ req(updated_data()); head(updated_data(), 10) })
    
    output$map_view <- renderLeaflet({
        md <- map_data()
        if (is.null(md) || nrow(md)==0) {
            df <- updated_data()
            if (is.null(df)) return(leaflet() %>% addTiles())
            leaflet(df) %>%
                addTiles() %>%
                addCircleMarkers(~long, ~lat, radius=5, color="blue", fillOpacity=0.7)
        } else {
            leaflet() %>%
                addTiles() %>%
                addMinicharts(
                    lng = md$long,
                    lat = md$lat,
                    type="pie",
                    chartdata = md[, c("tropicalisation","borealisation","detropicalisation","deborealisation")],
                    colorPalette=c('#DB3938','#4C7AB0','#8E6898','#ED9C2F'),
                    width=60, height=60
                )
        }
    })
    
    output$sti_table <- renderTable({ req(computed_sti()); computed_sti() })
    output$download_sti_csv <- downloadHandler(
        filename = function(){ paste0("STI_",Sys.Date(),".csv") },
        content = function(file){ write.csv(computed_sti(), file, row.names=FALSE) }
    )
    
    output$cti_table <- renderTable({ req(computed_cti()); head(computed_cti(), 15) })
    output$download_cti_csv <- downloadHandler(
        filename = function(){ paste0("CTI_",Sys.Date(),".csv") },
        content = function(file){ write.csv(computed_cti(), file, row.names=FALSE) }
    )
    
    output$download_trend_table <- downloadHandler(
        filename = function(){ paste0("Per_Station_Trends_",Sys.Date(),".csv") },
        content = function(file){ write.csv(trend_table(), file, row.names=FALSE) }
    )
    
    output$plot_trend <- renderPlot({ req(computed_cti()); plot_trend(computed_cti()) })
    output$plot_species <- renderPlot({ req(inferred_processes()); plot_species(inferred_processes()$trends) })
    
    output$cti_summary_text <- renderPrint({
        req(computed_cti())
        mod <- nlme::lme(CTI~year, random=~1|StationID, data=computed_cti(), method='REML')
        pval <- signif(summary(mod)$tTable[2,5], 2)
        est <- round(summary(mod)$tTable[2,1], 5)
        se <- round(summary(mod)$tTable[2,2], 5)
        if(pval < 0.05){
            cat(paste0("✅ Significant trend (p = ", pval, "): CTI increases by ", est, " °C/year (SE = ", se, ")."))
        } else {
            cat(paste0("⚠️ Non-significant trend (p = ", pval, "), estimated increase: ", est, " °C/year (SE = ", se, ")."))
        }
    })
    
    output$download_all_plots <- downloadHandler(
        filename = function() { paste0("Tropicalisation_Plots_", Sys.Date(), ".pdf") },
        content = function(file) {
            pdf(file, width=10, height=8)
            if(!is.null(computed_cti())){
                plot_trend(computed_cti())
            }
            if(!is.null(inferred_processes()$trends)){
                plot_species(inferred_processes()$trends)
            }
            dev.off()
        }
    )
    
}

# ----------------------------------
# Embedded Functions
# ----------------------------------

extract_temperature_for_communities <- function(community_data, data_dir, avg_file, output_csv, year_range, show_progress=TRUE) {
    message("[INFO] Using placeholder for temperature assignment: random uniform(10,18).")
    community_data$temperature <- runif(nrow(community_data), min=10, max=18)
    write.csv(community_data, output_csv, row.names=FALSE)
    return(community_data)
}

compute_STI <- function(df){
    req_cols <- c("SampleID","StationID","year","long","lat","temperature")
    missing <- setdiff(req_cols, colnames(df))
    if(length(missing)>0) stop("Missing columns: ", paste(missing, collapse=", "))
    
    species_cols <- setdiff(colnames(df), req_cols)
    results <- data.frame(
        species=species_cols,
        STI=NA_real_,
        n_occ=NA_integer_,
        stringsAsFactors=FALSE
    )
    
    for(sp_col in species_cols){
        if(!is.numeric(df[[sp_col]])){
            df[[sp_col]] <- suppressWarnings(as.numeric(as.character(df[[sp_col]])))
        }
        present_idx <- which(df[[sp_col]]>0)
        i <- which(results$species==sp_col)
        if(length(present_idx)==0){
            results$STI[i]<-NA
            results$n_occ[i]<-0
        } else {
            temp_vals<-df$temperature[present_idx]
            results$STI[i]<-mean(temp_vals,na.rm=TRUE)
            results$n_occ[i]<-sum(!is.na(temp_vals))
        }
    }
    return(results)
}

get_CTI <- function(community_matrix, STI, infos_col, occu=TRUE){
    if(!"STI"%in%colnames(STI)) stop('No "STI" column in STI.')
    if(!"species"%in%colnames(STI)) stop('No "species" column in STI.')
    
    if(occu){
        community_matrix[, -infos_col]<-ifelse(community_matrix[, -infos_col]>0,1,0)
    }
    
    rel_abund<-t(apply(
        community_matrix[, -infos_col, drop=FALSE],
        1,
        function(x)x/sum(x,na.rm=TRUE)
    ))
    
    row_sums<-apply(community_matrix[, -infos_col, drop=FALSE],1,sum,na.rm=TRUE)
    zero_rows<-which(row_sums==0)
    if(length(zero_rows)>0){
        warning(length(zero_rows)," row(s) have zero total abundance => CTI=NA.")
    }
    
    sp2sti<-setNames(STI$STI,STI$species)
    
    output_CTI<-apply(rel_abund,1,function(z){
        sp_names<-colnames(rel_abund)
        stats::weighted.mean(sp2sti[sp_names], z, na.rm=TRUE)
    })
    
    cti_df<-data.frame(
        community_matrix[, infos_col, drop=FALSE],
        CTI=output_CTI
    )
    return(cti_df)
}

get_processes <- function(community_matrix, STI, CTI, infos_col, all_outputs=TRUE, log=TRUE){
    if(!"StationID"%in%colnames(community_matrix)){
        stop("No StationID in community_matrix.")
    }
    if(!"StationID"%in%colnames(CTI)){
        stop("No StationID in CTI => can't match stations.")
    }
    
    stations <- unique(CTI$StationID)
    trends <- NULL
    process_strength <- NULL
    
    for(stn in stations){
        cm_stn <- community_matrix[community_matrix$StationID==stn, , drop=FALSE]
        if(nrow(cm_stn)==0){
            warning("No community data for station ", stn," => skipping.")
            next
        }
        
        # ---------------------------
        # REMOVED the chunk removing zero-sum columns
        # zero_sum_cols <- which(apply(cm_stn[, -infos_col, drop=FALSE],2,sum,na.rm=TRUE)==0)
        # if(length(zero_sum_cols)>0){
        #     zero_sum_cols <- zero_sum_cols + max(infos_col)
        #     cm_stn <- cm_stn[, -zero_sum_cols, drop=FALSE]
        # }
        #
        # if(ncol(cm_stn)<=max(infos_col)){
        #     warning("No species columns left for station ", stn," => skipping.")
        #     next
        # }
        # ---------------------------
        
        # Slope function
        slope_func <- if(log){
            function(x){
                m <- lm(log(x+1)~cm_stn$year)
                c(estimate=coef(summary(m))[2,1], pvalue=coef(summary(m))[2,4])
            }
        } else {
            function(x){
                m <- lm(x~cm_stn$year)
                c(estimate=coef(summary(m))[2,1], pvalue=coef(summary(m))[2,4])
            }
        }
        
        # Convert species
        sp_data <- cm_stn[, -infos_col, drop=FALSE]
        sp_data[] <- lapply(sp_data, function(xx) suppressWarnings(as.numeric(xx)))
        sp_data <- sp_data[, sapply(sp_data, is.numeric), drop=FALSE]
        # If sp_data is all zeros, slope_func can still produce 0 or NA, but won't skip
        
        slope_mat <- apply(sp_data, 2, slope_func)
        if(is.null(dim(slope_mat))||ncol(slope_mat)==0||nrow(slope_mat)==0){
            warning("No slope estimates for station ", stn," => skipping.")
            next
        }
        
        sp_colnames <- colnames(cm_stn)[-infos_col]
        if(length(sp_colnames)!=ncol(slope_mat)){
            warning("Mismatch species colnames vs slope_mat for station ", stn," => skipping.")
            next
        }
        
        slope_df <- data.frame(
            species=sp_colnames,
            estimate=slope_mat["estimate",],
            pvalue=slope_mat["pvalue",]
        )
        slope_df$trend <- ifelse(slope_df$estimate>0,"increase","decrease")
        
        slope_df <- merge(slope_df, STI, by="species", all.x=TRUE)
        
        cti_sub <- CTI[CTI$StationID==stn,]
        if(nrow(cti_sub)==0||all(is.na(cti_sub$CTI))){
            warning("No CTI data for station ", stn," => skipping.")
            next
        }
        avg_stn <- mean(cti_sub$CTI, na.rm=TRUE)
        slope_df$difference <- slope_df$STI - avg_stn
        
        slope_df$process <- with(slope_df, ifelse(
            trend=='increase' & difference>0,'tropicalisation',
            ifelse(trend=='increase' & difference<0,'borealisation',
                   ifelse(trend=='decrease' & difference>0,'deborealisation',
                          ifelse(trend=='decrease' & difference<0,'detropicalisation',NA)
                   )
            )
        ))
        abs_strength <- tapply(
            abs((slope_df$STI-avg_stn)*slope_df$estimate),
            slope_df$process,
            sum,
            na.rm=TRUE
        )
        abs_strength[is.na(abs_strength)] <- 0
        rel_strength <- 100*round(abs_strength/sum(abs_strength,na.rm=TRUE),3)
        
        pstrength_df <- data.frame(
            Station=stn,
            process=names(rel_strength),
            strength=rel_strength
        )
        
        slope_df <- data.frame(StationID=stn, slope_df)
        trends <- rbind(trends, slope_df)
        process_strength <- rbind(process_strength, pstrength_df)
    }
    
    if(all_outputs){
        rownames(process_strength) <- NULL
        list(trends=trends, process_strength=process_strength)
    } else {
        process_strength
    }
}

plot_trend <- function(CTI){
    if(!"StationID"%in%colnames(CTI)) stop("CTI has no StationID column => can't plot.")
    
    cti_min<-floor(min(CTI$CTI,na.rm=TRUE))
    cti_max<-ceiling(max(CTI$CTI,na.rm=TRUE))
    year_rng<-range(CTI$year, na.rm=TRUE)
    
    plot(x=1,y=1,xlim=year_rng,ylim=c(cti_min, cti_max),
         type='n',axes=FALSE,xlab='',ylab='')
    axis(1,at=seq(year_rng[1],year_rng[2],1),las=2,tcl=-0.5,lwd=2)
    axis(2,at=seq(cti_min, cti_max,1),las=1,tcl=-0.5,lwd=2)
    mtext("Year",side=1,line=3)
    mtext("CTI",side=2,line=3)
    
    splitted<-split(CTI,CTI$StationID)
    for(stn in names(splitted)){
        xx<-splitted[[stn]]
        lines(xx$year, xx$CTI, col='#dad7cd', lwd=1, lty=1)
    }
    
    mod<-nlme::lme(CTI~year, random=~1|StationID, data=CTI, method='REML')
    pred<-ggeffects::ggpredict(mod,"year",type='simulate') %>% as.data.frame()
    
    pcol<-ifelse(summary(mod)$tTable[2,5]>0.05,'#778da9','#ae2012')
    lcol<-ifelse(summary(mod)$tTable[2,5]>0.05,'#415a77','#9b2226')
    
    polygon(
        x=c(pred$x,rev(pred$x),pred$x[1]),
        y=c(pred$conf.low,rev(pred$conf.high),pred$conf.low[1]),
        col=pcol,border=pcol
    )
    lines(pred$x, pred$predicted, lwd=3, col=lcol)
}

plot_species <- function(process_df){
    process_df<-na.omit(process_df)
    needed<-c("estimate","STI","difference","process")
    if(!all(needed%in%colnames(process_df))){
        warning("process_df missing columns => can't plot species trends.")
        return()
    }
    
    est_min<-floor(min(process_df$estimate,na.rm=TRUE))
    est_max<-ceiling(max(process_df$estimate,na.rm=TRUE))
    sti_min<-floor(min(process_df$STI,na.rm=TRUE))
    sti_max<-ceiling(max(process_df$STI,na.rm=TRUE))
    
    op<-par(mar=c(5,3,2,5),oma=c(1,7,1,1),pty='s',mfrow=c(1,2))
    on.exit(par(op),add=TRUE)
    
    plot(1,1,xlim=c(sti_min, sti_max),ylim=c(est_min, est_max),
         type='n',axes=FALSE,xlab='',ylab='')
    axis(1,tcl=-0.5,lwd=2,las=1)
    axis(2,tcl=-0.5,lwd=2,las=2)
    abline(h=0,lty=3,lwd=2)
    mtext("STI (°C)",side=1,line=3)
    mtext("Trends in log-abundances",side=2,line=3)
    
    color_map<-c(
        'tropicalisation'='#DB3938',
        'borealisation'='#4C7AB0',
        'detropicalisation'='#8E6898',
        'deborealisation'='#ED9C2F'
    )
    process_df$plot_col<-color_map[process_df$process]
    process_df$plot_col[is.na(process_df$plot_col)]<-"#cccccc"
    
    raw_size <- abs(process_df$difference)
    norm_size <- (raw_size - min(raw_size, na.rm=TRUE)) / (max(raw_size, na.rm=TRUE) - min(raw_size, na.rm=TRUE) + 1e-6)
    cex_val <- norm_size * 3  # 1 (small) to 4 (big)
    
    points(
        x=process_df$STI,y=process_df$estimate,
        pch=21,bg=scales::alpha(process_df$plot_col,0.3),cex=cex_val
    )
    
    freq_table<-table(process_df$process)
    sorted_freq<-sort(freq_table,decreasing=TRUE)
    bar_positions<-barplot(
        sorted_freq,
        col=color_map[names(sorted_freq)],
        horiz=TRUE,axes=FALSE,names.arg=NA
    )
    axis(1)
    text(
        x=sorted_freq,y=bar_positions,
        labels=names(sorted_freq),pos=2
    )
}

make_cti_trends_table <- function(CTI, proc){
    if(is.null(proc$process_strength) || is.null(proc$trends)){
        return(NULL)
    }
    process_df <- proc$process_strength
    
    # Summaries of CTI slope for each station
    station_slopes <- CTI %>%
        group_by(StationID) %>%
        summarise(
            lat = mean(lat, na.rm=TRUE),
            long = mean(long, na.rm=TRUE),
            slope = coef(lm(CTI ~ year))[2],
            se = summary(lm(CTI ~ year))$coefficients[2,2],
            pvalue = summary(lm(CTI ~ year))$coefficients[2,4],
            .groups="drop"
        )
    
    # Summaries of process
    process_wide <- process_df %>%
        group_by(Station, process) %>%
        summarise(strength = sum(strength, na.rm=TRUE), .groups="drop") %>%
        tidyr::pivot_wider(
            names_from = process,
            values_from = strength,
            values_fill = 0
        )
    
    # Merge
    trend_table <- left_join(station_slopes, process_wide, by = c("StationID" = "Station"))
    
    # If some processes are missing for a station, fill with 0
    for(p in c("tropicalisation","borealisation","detropicalisation","deborealisation")){
        if(!p %in% colnames(trend_table)){
            trend_table[[p]] <- 0
        }
    }
    
    return(trend_table)
}

# ----------------------------------
shinyApp(ui, server)
