

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(readr)
library(ggplot2)

# -------------------- 2. LOAD DATA ----------------------------
# Put sales_data.csv in the same folder as app.R
data <- read_csv("C:\\Users\\Administrator\\Downloads\\archive (4)\\sales_data.csv", show_col_types = FALSE)

# IMPORTANT: Safely convert sale_date
data$sale_date <- as.Date(data$sale_date)

# Clean numeric fields
data$sales_amount <- as.numeric(data$sales_amount)
data$customer_id <- as.integer(data$customer_id)

# Remove incomplete rows only where required
data <- data %>%
  filter(!is.na(sale_date), !is.na(sales_amount))

# -------------------- 3. UI ----------------------------------
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "Sales Performance",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "tabs",
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Sales Analysis",
        tabName = "analysis",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Data Explorer",
        tabName = "data",
        icon = icon("table")
      )
    ),
    
    br(),
    
    h4("FILTERS", style = "color:white; padding-left:15px;"),
    
    # Product filter
    selectInput(
      "product",
      "Product",
      choices = NULL,
      multiple = TRUE,
      selected = NULL
    ),
    
    # Customer filter
    selectizeInput(
      "customer",
      "Customer ID",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All customers"
      )
    ),
    
    # Date range
    dateRangeInput(
      "date_range",
      "Sales Date",
      start = min(data$sale_date, na.rm = TRUE),
      end = max(data$sale_date, na.rm = TRUE),
      min = min(data$sale_date, na.rm = TRUE),
      max = max(data$sale_date, na.rm = TRUE),
      format = "yyyy-mm-dd"
    ),
    
    br(),
    
    actionButton(
      "reset",
      "Reset Filters",
      icon = icon("refresh"),
      class = "btn-warning",
      style = "margin-left:15px; width:250px;"
    )
  ),
  
  # Body
  dashboardBody(
    
    # ---------------- CUSTOM CSS ----------------
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #16213E;
          font-weight: bold;
        }

        .skin-blue .main-header .navbar {
          background-color: #16213E;
        }

        .skin-blue .main-sidebar {
          background-color: #111827;
        }

        .content-wrapper {
          background-color: #F4F6F9;
        }

        .small-box {
          border-radius: 12px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.10);
        }

        .box {
          border-radius: 12px;
          border-top: 3px solid #3C8DBC;
          box-shadow: 0 3px 10px rgba(0,0,0,0.08);
        }

        .box-title {
          font-weight: bold;
        }

        .dataTables_wrapper {
          font-size: 13px;
        }

        .dashboard-title {
          font-size: 28px;
          font-weight: 700;
          margin-bottom: 5px;
          color: #1F2937;
        }

        .dashboard-subtitle {
          color: #6B7280;
          margin-bottom: 20px;
        }
      "))
    ),
    
    tabItems(
      
      # ========================================================
      # DASHBOARD TAB
      # ========================================================
      tabItem(
        tabName = "dashboard",
        
        div(
          class = "dashboard-title",
          "Sales Performance Dashboard"
        ),
        
        div(
          class = "dashboard-subtitle",
          "Interactive overview of revenue, customers, products and sales trends"
        ),
        
        # KPI ROW
        fluidRow(
          
          valueBoxOutput("total_sales", width = 3),
          
          valueBoxOutput("total_orders", width = 3),
          
          valueBoxOutput("customers", width = 3),
          
          valueBoxOutput("avg_order", width = 3)
        ),
        
        # SECOND KPI ROW
        fluidRow(
          
          valueBoxOutput("top_product", width = 3),
          
          valueBoxOutput("max_sale", width = 3),
          
          valueBoxOutput("min_sale", width = 3),
          
          valueBoxOutput("growth", width = 3)
        ),
        
        # CHARTS
        fluidRow(
          
          box(
            title = "Sales Trend Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("sales_trend", height = "380px")
          ),
          
          box(
            title = "Sales by Product",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("product_chart", height = "380px")
          )
        ),
        
        fluidRow(
          
          box(
            title = "Monthly Sales Performance",
            status = "info",
            solidHeader = TRUE,
            width = 7,
            plotlyOutput("monthly_chart", height = "350px")
          ),
          
          box(
            title = "Top Customers",
            status = "info",
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("customer_chart", height = "350px")
          )
        )
      ),
      
      # ========================================================
      # ANALYSIS TAB
      # ========================================================
      tabItem(
        tabName = "analysis",
        
        h2("Advanced Sales Analysis"),
        
        fluidRow(
          
          box(
            title = "Product Performance",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("product_analysis", height = "400px")
          ),
          
          box(
            title = "Sales Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("distribution", height = "400px")
          )
        ),
        
        fluidRow(
          
          box(
            title = "Daily Sales",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("daily_sales", height = "400px")
          )
        )
      ),
      
      # ========================================================
      # DATA TAB
      # ========================================================
      tabItem(
        tabName = "data",
        
        h2("Sales Data Explorer"),
        
        box(
          title = "Filtered Sales Records",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          downloadButton(
            "download_data",
            "Download Filtered CSV",
            class = "btn-success"
          ),
          
          br(),
          br(),
          
          DTOutput("sales_table")
        )
      )
    )
  )
)

# -------------------- 4. SERVER -------------------------------
server <- function(input, output, session) {
  
  # Initialize filters after data loads
  updateSelectInput(
    session,
    "product",
    choices = sort(unique(data$product)),
    selected = sort(unique(data$product))
  )
  
  updateSelectizeInput(
    session,
    "customer",
    choices = sort(unique(data$customer_id)),
    selected = NULL,
    server = TRUE
  )
  
  # ------------------------------------------------------------
  # FILTERED DATA
  # ------------------------------------------------------------
  filtered_data <- reactive({
    
    req(input$date_range)
    
    result <- data %>%
      filter(
        sale_date >= input$date_range[1],
        sale_date <= input$date_range[2]
      )
    
    if (!is.null(input$product) && length(input$product) > 0) {
      result <- result %>%
        filter(product %in% input$product)
    }
    
    if (!is.null(input$customer) && length(input$customer) > 0) {
      result <- result %>%
        filter(customer_id %in% as.integer(input$customer))
    }
    
    result
  })
  
  # ------------------------------------------------------------
  # RESET FILTERS
  # ------------------------------------------------------------
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "product",
      selected = sort(unique(data$product))
    )
    
    updateSelectizeInput(
      session,
      "customer",
      selected = character(0)
    )
    
    updateDateRangeInput(
      session,
      "date_range",
      start = min(data$sale_date, na.rm = TRUE),
      end = max(data$sale_date, na.rm = TRUE)
    )
  })
  
  # ============================================================
  # KPI CARDS
  # ============================================================
  
  output$total_sales <- renderValueBox({
    
    d <- filtered_data()
    
    valueBox(
      value = dollar(sum(d$sales_amount, na.rm = TRUE), prefix = "₹"),
      subtitle = "Total Sales",
      icon = icon("indian-rupee-sign"),
      color = "aqua"
    )
  })
  
  output$total_orders <- renderValueBox({
    
    d <- filtered_data()
    
    valueBox(
      value = comma(nrow(d)),
      subtitle = "Total Orders",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$customers <- renderValueBox({
    
    d <- filtered_data()
    
    valueBox(
      value = comma(n_distinct(d$customer_id)),
      subtitle = "Unique Customers",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$avg_order <- renderValueBox({
    
    d <- filtered_data()
    
    avg <- ifelse(
      nrow(d) == 0,
      0,
      mean(d$sales_amount, na.rm = TRUE)
    )
    
    valueBox(
      value = dollar(avg, prefix = "₹"),
      subtitle = "Average Order Value",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$top_product <- renderValueBox({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      best <- "N/A"
    } else {
      best <- d %>%
        group_by(product) %>%
        summarise(total = sum(sales_amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice(1) %>%
        pull(product)
    }
    
    valueBox(
      value = best,
      subtitle = "Top Product",
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  output$max_sale <- renderValueBox({
    
    d <- filtered_data()
    
    valueBox(
      value = dollar(
        ifelse(nrow(d) == 0, 0, max(d$sales_amount, na.rm = TRUE)),
        prefix = "₹"
      ),
      subtitle = "Highest Sale",
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$min_sale <- renderValueBox({
    
    d <- filtered_data()
    
    valueBox(
      value = dollar(
        ifelse(nrow(d) == 0, 0, min(d$sales_amount, na.rm = TRUE)),
        prefix = "₹"
      ),
      subtitle = "Lowest Sale",
      icon = icon("arrow-down"),
      color = "light-blue"
    )
  })
  
  # Simple first-vs-last-period change indicator
  output$growth <- renderValueBox({
    
    d <- filtered_data()
    
    if (nrow(d) < 2) {
      pct <- 0
    } else {
      
      monthly <- d %>%
        mutate(month = floor_date(sale_date, "month")) %>%
        group_by(month) %>%
        summarise(total = sum(sales_amount), .groups = "drop") %>%
        arrange(month)
      
      if (nrow(monthly) >= 2 && monthly$total[1] != 0) {
        pct <- ((monthly$total[nrow(monthly)] -
                   monthly$total[1]) /
                  monthly$total[1]) * 100
      } else {
        pct <- 0
      }
    }
    
    valueBox(
      value = paste0(round(pct, 1), "%"),
      subtitle = "First-to-Last Month Change",
      icon = icon("chart-line"),
      color = "olive"
    )
  })
  
  # ============================================================
  # SALES TREND
  # ============================================================
  
  output$sales_trend <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(sale_date) %>%
      summarise(
        total_sales = sum(sales_amount, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(
      d,
      aes(
        x = sale_date,
        y = total_sales,
        text = paste0(
          "Date: ", sale_date,
          "<br>Sales: ₹", round(total_sales, 2)
        )
      )
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      labs(x = NULL, y = "Sales Amount") +
      scale_y_continuous(labels = label_dollar(prefix = "₹")) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # ============================================================
  # PRODUCT CHART
  # ============================================================
  
  output$product_chart <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(product) %>%
      summarise(
        sales = sum(sales_amount, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(sales))
    
    p <- ggplot(
      d,
      aes(
        x = reorder(product, sales),
        y = sales,
        text = paste0(
          "Product: ", product,
          "<br>Sales: ₹", round(sales, 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Product", y = "Sales") +
      scale_y_continuous(labels = label_dollar(prefix = "₹")) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # MONTHLY CHART
  # ============================================================
  
  output$monthly_chart <- renderPlotly({
    
    d <- filtered_data() %>%
      mutate(month = floor_date(sale_date, "month")) %>%
      group_by(month) %>%
      summarise(
        sales = sum(sales_amount, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(
      d,
      aes(
        x = month,
        y = sales,
        text = paste0(
          "Month: ", format(month, "%b %Y"),
          "<br>Sales: ₹", round(sales, 2)
        )
      )
    ) +
      geom_col() +
      labs(x = NULL, y = "Sales") +
      scale_y_continuous(labels = label_dollar(prefix = "₹")) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # CUSTOMER CHART
  # ============================================================
  
  output$customer_chart <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(customer_id) %>%
      summarise(
        sales = sum(sales_amount, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(sales)) %>%
      slice_head(n = 10)
    
    p <- ggplot(
      d,
      aes(
        x = reorder(as.factor(customer_id), sales),
        y = sales,
        text = paste0(
          "Customer: ", customer_id,
          "<br>Sales: ₹", round(sales, 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Customer ID", y = "Sales") +
      scale_y_continuous(labels = label_dollar(prefix = "₹")) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # ADVANCED PRODUCT ANALYSIS
  # ============================================================
  
  output$product_analysis <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(product) %>%
      summarise(
        total_sales = sum(sales_amount, na.rm = TRUE),
        avg_sale = mean(sales_amount, na.rm = TRUE),
        orders = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      d,
      aes(
        x = orders,
        y = total_sales,
        size = avg_sale,
        text = paste0(
          "Product: ", product,
          "<br>Orders: ", orders,
          "<br>Total Sales: ₹", round(total_sales, 2),
          "<br>Average Sale: ₹", round(avg_sale, 2)
        )
      )
    ) +
      geom_point(alpha = 0.8) +
      geom_text(
        aes(label = product),
        vjust = -1.2,
        size = 4
      ) +
      labs(
        x = "Number of Orders",
        y = "Total Sales",
        size = "Average Sale"
      ) +
      scale_y_continuous(labels = label_dollar(prefix = "₹")) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # SALES DISTRIBUTION
  # ============================================================
  
  output$distribution <- renderPlotly({
    
    d <- filtered_data()
    
    p <- ggplot(
      d,
      aes(
        x = sales_amount,
        text = paste0(
          "Sale Amount: ₹",
          round(sales_amount, 2)
        )
      )
    ) +
      geom_histogram(
        bins = 30,
        alpha = 0.85
      ) +
      labs(
        x = "Sales Amount",
        y = "Frequency"
      ) +
      scale_x_continuous(
        labels = label_dollar(prefix = "₹")
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # DAILY SALES
  # ============================================================
  
  output$daily_sales <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(sale_date) %>%
      summarise(
        total_sales = sum(sales_amount, na.rm = TRUE),
        orders = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      d,
      aes(
        x = sale_date,
        y = total_sales,
        text = paste0(
          "Date: ", sale_date,
          "<br>Sales: ₹", round(total_sales, 2),
          "<br>Orders: ", orders
        )
      )
    ) +
      geom_area(alpha = 0.35) +
      geom_line(linewidth = 1) +
      labs(
        x = "Date",
        y = "Total Sales"
      ) +
      scale_y_continuous(
        labels = label_dollar(prefix = "₹")
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # ============================================================
  # DATA TABLE
  # ============================================================
  
  output$sales_table <- renderDT({
    
    filtered_data() %>%
      arrange(desc(sale_date)) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      ) %>%
      formatCurrency(
        "sales_amount",
        currency = "₹",
        digits = 2
      )
  })
  
  # ============================================================
  # DOWNLOAD
  # ============================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "sales_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# -------------------- 5. RUN APP ------------------------------
shinyApp(ui = ui, server = server)
