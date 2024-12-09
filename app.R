#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    https://shiny.posit.co/

# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(cluster)
library(factoextra)

# Load dataset
UMKBinaan <- read.csv("dataset/UMKBinaanJSMR.csv")

# Data preprocessing
UMKBinaan$Saldo <- as.numeric(gsub("[^0-9.]", "", UMKBinaan$Saldo))
UMKBinaan$Tunggakan <- as.numeric(gsub("[^0-9.]", "", UMKBinaan$Tunggakan))

# Normalisasi data numerik untuk clustering
UMKBinaan_scaled <- UMKBinaan %>%
  select(Saldo, Tunggakan) %>%
  scale()

# UI
ui <- fluidPage(
  titlePanel("Analisis Risiko Keuangan UMK Binaan dengan Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Pengaturan Clustering"),
      p("Gunakan clustering untuk menganalisis risiko keuangan."),
      numericInput("numClusters", "Jumlah Klaster (K):", value = 4, min = 2, max = 10),
      actionButton("runClustering", "Lakukan Clustering"),
      hr(),
      h4("Top Sektor"),
      p("Analisis sektor dengan tunggakan terbesar."),
      sliderInput("topNSectors", "Jumlah Sektor:", min = 1, max = 10, value = 5)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Evaluasi Clustering",
                 h3("Elbow Method"),
                 plotOutput("elbowPlot"),
                 h3("Silhouette Score"),
                 plotOutput("silhouettePlot")),
        tabPanel("Hasil Clustering",
                 h3("Tabel Hasil Clustering"),
                 dataTableOutput("clusterTable")),
        tabPanel("Ringkasan Hasil Clustering",
                 h3("Ringkasan Hasil Clustering dan Rekomendasi"),
                 dataTableOutput("clusterSummary")),
        tabPanel("Visualisasi Distribusi Klaster",
                 h3("Visualisasi Distribusi Klaster"),
                 plotOutput("clusterBarPlot")),
        tabPanel("Top Sektor Tunggakan",
                 h3("Tabel Top Sektor"),
                 dataTableOutput("topSectorTable"),
                 h3("Diagram Batang Top Sektor"),
                 plotOutput("topSectorBarPlot")),
        tabPanel("EDA",
                 h3("Eksplorasi Data Awal"),
                 h4("Statistik Deskriptif"),
                 dataTableOutput("summaryStats"),
                 h4("Distribusi Saldo"),
                 plotOutput("saldoDistribution"),
                 h4("Distribusi Tunggakan"),
                 plotOutput("tunggakanDistribution"),
                 h4("Hubungan Saldo dan Tunggakan"),
                 plotOutput("scatterPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Clustering Process
  clustering_result <- eventReactive(input$runClustering, {
    kmeans(UMKBinaan_scaled, centers = input$numClusters, nstart = 25)
  })
  
  # Elbow Method
  output$elbowPlot <- renderPlot({
    fviz_nbclust(UMKBinaan_scaled, kmeans, method = "wss") +
      labs(title = "Elbow Method untuk Menentukan Jumlah Klaster") +
      theme_minimal()
  })
  
  # Silhouette Plot
  output$silhouettePlot <- renderPlot({
    clusters <- clustering_result()
    silhouette_res <- silhouette(clusters$cluster, dist(UMKBinaan_scaled))
    fviz_silhouette(silhouette_res) +
      labs(title = paste("Silhouette Plot untuk", input$numClusters, "Klaster")) +
      theme_minimal()
  })
  
  # Tampilkan hasil clustering
  output$clusterTable <- renderDataTable({
    clusters <- clustering_result()
    clustered_data <- UMKBinaan %>%
      mutate(Cluster = as.factor(clusters$cluster))
    
    datatable(clustered_data, 
              options = list(pageLength = 10, searching = FALSE),
              caption = "Hasil clustering untuk UMK berdasarkan Saldo dan Tunggakan.")
  })
  
  # Ringkasan hasil clustering dengan rekomendasi lebih rinci
  output$clusterSummary <- renderDataTable({
    clusters <- clustering_result()
    clustered_data <- UMKBinaan %>%
      mutate(Cluster = as.factor(clusters$cluster))
    
    # Ringkasan per klaster
    cluster_summary <- clustered_data %>%
      group_by(Cluster) %>%
      summarise(
        Jumlah_Anggota = n(),
        Rata_rata_Saldo = mean(Saldo, na.rm = TRUE),
        Total_Saldo = sum(Saldo, na.rm = TRUE),
        Rata_rata_Tunggakan = mean(Tunggakan, na.rm = TRUE),
        Total_Tunggakan = sum(Tunggakan, na.rm = TRUE)
      ) %>%
      mutate(Rekomendasi = case_when(
        Rata_rata_Tunggakan == 0 ~ "UMK dalam klaster ini memiliki catatan pembayaran yang sempurna. Pertahankan hubungan baik dan tawarkan insentif untuk memperluas pinjaman dengan risiko rendah.",
        Rata_rata_Tunggakan < mean(clustered_data$Tunggakan) ~ "UMK dalam klaster ini memiliki risiko rendah. Dorong pembayaran rutin dengan memberikan edukasi keuangan untuk mempertahankan kinerja.",
        Rata_rata_Tunggakan >= mean(clustered_data$Tunggakan) & Rata_rata_Tunggakan < (1.5 * mean(clustered_data$Tunggakan)) ~ "Risiko sedang terdeteksi. Prioritaskan pemantauan terhadap UMK ini dan siapkan program mitigasi risiko, seperti restrukturisasi pinjaman.",
        Rata_rata_Tunggakan >= (1.5 * mean(clustered_data$Tunggakan)) ~ "UMK dalam klaster ini memiliki risiko tinggi. Lakukan evaluasi mendalam terhadap penyebab tunggakan dan pertimbangkan untuk menunda pemberian pinjaman baru hingga situasi membaik."
      ))
    
    datatable(cluster_summary, 
              options = list(pageLength = input$numClusters, searching = FALSE),
              caption = "Ringkasan hasil clustering berdasarkan Saldo dan Tunggakan.")
  })
  
  
  # Bar Chart untuk distribusi klaster
  output$clusterBarPlot <- renderPlot({
    clusters <- clustering_result()
    clustered_data <- UMKBinaan %>%
      mutate(Cluster = as.factor(clusters$cluster))
    
    cluster_distribution <- clustered_data %>%
      count(Cluster) %>%
      arrange(desc(n))
    
    ggplot(cluster_distribution, aes(x = Cluster, y = n, fill = Cluster)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Distribusi Jumlah Anggota di Setiap Klaster",
           x = "Klaster", y = "Jumlah Anggota") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal()
  })
  
  # Analisis Top Sektor
  output$topSectorTable <- renderDataTable({
    top_sectors <- UMKBinaan %>%
      group_by(Sektor) %>%
      summarise(Total_Tunggakan = sum(Tunggakan, na.rm = TRUE)) %>%
      arrange(desc(Total_Tunggakan)) %>%
      head(input$topNSectors)
    
    datatable(top_sectors, 
              options = list(pageLength = input$topNSectors, searching = FALSE),
              caption = "Top sektor dengan total tunggakan tertinggi.")
  })
  
  # Diagram batang untuk top sektor
  output$topSectorBarPlot <- renderPlot({
    top_sectors <- UMKBinaan %>%
      group_by(Sektor) %>%
      summarise(Total_Tunggakan = sum(Tunggakan, na.rm = TRUE)) %>%
      arrange(desc(Total_Tunggakan)) %>%
      head(input$topNSectors)
    
    ggplot(top_sectors, aes(x = reorder(Sektor, -Total_Tunggakan), y = Total_Tunggakan)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Sektor dengan Total Tunggakan Tertinggi",
           x = "Sektor", y = "Total Tunggakan") +
      theme_minimal()
  })
  # Statistik Deskriptif
  output$summaryStats <- renderDataTable({
    summary_data <- UMKBinaan %>%
      summarise(
        Total_UMK = n(),
        Saldo_Min = min(Saldo, na.rm = TRUE),
        Saldo_Maks = max(Saldo, na.rm = TRUE),
        Saldo_Rata2 = mean(Saldo, na.rm = TRUE),
        Tunggakan_Min = min(Tunggakan, na.rm = TRUE),
        Tunggakan_Maks = max(Tunggakan, na.rm = TRUE),
        Tunggakan_Rata2 = mean(Tunggakan, na.rm = TRUE)
      )
    datatable(summary_data, 
              caption = "Statistik deskriptif untuk Saldo dan Tunggakan")
  })
  
  # Distribusi Saldo (Bar Chart)
  output$saldoDistribution <- renderPlot({
    # Mengelompokkan data berdasarkan sektor dan menghitung total saldo per sektor
    sector_saldo <- UMKBinaan %>%
      group_by(Sektor) %>%
      summarise(Total_Saldo = sum(Saldo, na.rm = TRUE)) %>%
      arrange(desc(Total_Saldo))
    
    # Membuat diagram batang
    ggplot(sector_saldo, aes(x = reorder(Sektor, Total_Saldo), y = Total_Saldo, fill = Sektor)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Distribusi Saldo Berdasarkan Sektor",
           x = "Sektor", 
           y = "Total Saldo (dalam satuan rupiah)") +
      theme_minimal(base_size = 14) +
      coord_flip() +  # Membalikkan koordinat agar lebih mudah dibaca
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16))
  })
  
  
  # Distribusi Tunggakan (Pie Chart)
  output$tunggakanDistribution <- renderPlot({
    # Mengelompokkan data berdasarkan sektor dan menghitung total tunggakan per sektor
    sector_tunggakan <- UMKBinaan %>%
      group_by(Sektor) %>%
      summarise(Total_Tunggakan = sum(Tunggakan, na.rm = TRUE)) %>%
      arrange(desc(Total_Tunggakan))
    
    # Menambahkan kolom proporsi untuk persentase
    sector_tunggakan <- sector_tunggakan %>%
      mutate(Proporsi = Total_Tunggakan / sum(Total_Tunggakan) * 100,
             Label = paste(Sektor, sprintf("(%.1f%%)", Proporsi)))
    
    # Membuat diagram pie
    ggplot(sector_tunggakan, aes(x = "", y = Total_Tunggakan, fill = Label)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      labs(title = "Distribusi Tunggakan Berdasarkan Sektor",
           fill = "Sektor",
           x = NULL,
           y = NULL) +
      theme_void(base_size = 14) +
      theme(legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, size = 16)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Hubungan antara Saldo dan Tunggakan (Bubble Chart)
  output$scatterPlot <- renderPlot({
    ggplot(UMKBinaan, aes(x = Saldo, y = Tunggakan, size = Saldo, color = Sektor)) +
      geom_point(alpha = 0.6) +
      scale_size_continuous(range = c(3, 12)) +  # Ukuran gelembung berdasarkan saldo
      labs(title = "Hubungan antara Saldo dan Tunggakan Berdasarkan Sektor",
           x = "Saldo (dalam satuan rupiah)", 
           y = "Tunggakan (dalam satuan rupiah)",
           subtitle = "Ukuran gelembung menunjukkan saldo, warna mewakili sektor") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right") +
      scale_color_brewer(palette = "Set3")  # Warna untuk sektor
  })
}

# Run the application
shinyApp(ui = ui, server = server)