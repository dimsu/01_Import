# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)




# 2.0 Importing Files ----
#hint when you are looking for help type: ?function()

bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel(path="00_data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel(path="00_data/bike_sales/data_raw/orderlines.xlsx")




# 3.0 Examining Data ----
#function --glimpse--  to have a quick look on data 
glimpse(bikes_tbl)
bikes_tbl




 # 4.0 Joining Data ----

left_join(orderlines_tbl, bikes_tbl, by=c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by=c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by=c("customer.id" = "bikeshop.id"))


# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
    
    #separate description column into multiple one,
    separate(description, 
             into = c("category1", "category2", "frame.material"), 
             sep = " - ", 
             remove = TRUE) %>%
    
    #separate location column into city and state 
    separate(location, 
             into = c("city", "state"), 
             sep = ",", 
             remove = FALSE) %>%
    
    #Add a calculated column total.price column - use the function MUTATE
    # MUTATE is used to create/add a new column in the tibble (data structure)
    mutate(total.price = price*quantity) %>%
    
    #Remove unnecessary column - use the function SELECT
    #the sign (-) at the beginning is necessary = Removal column 
    select(-...1, -location) %>%
    select(-ends_with(".id")) %>%
    
    #After removing all the column end with .id but we need 
    #to get the column order.id - use the function BIND_COLS
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
    #Reorder column 
    select(contains("date"), contains("id"), 
            contains(c("order", "id")), price, quantity,
           total.price, everything()) %>%
    
    #Rename column 
    #names() = get/set a name of an object
    #set_names() == str_replace_all
    rename(order_date = order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_"))
    

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

    #selecting columns to focus on and adding a year column 
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    
    #Selecting columns to focus on and adding a year column
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    
    #Grouping by year and summarizing sales
    #function summarise is used to create a new data structure according to 
    #the grouping_by made upfront
    group_by(year) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    #Text formatting introduce de $ sign 
    mutate(sales_text = scales::dollar(sales))
    
sales_by_year_tbl

# Step 2 - Visualize

sales_by_year_tbl %>%
    
    #Setup canvas with year (X-axis) and sales (Y-axis)
    ggplot(aes(x = year, y = sales)) + 
    
    # Geometrics - geom_smooth = trend curve
    geom_col(fill = "#2c3e50") + 
    geom_label(aes(label = sales_text)) + 
    geom_smooth(method = "lm", se = FALSE) + 
    
    #Formatting 
    # theme_made_in_tidyquant_library
    theme_tq() + 
    scale_y_continuous(labels = scales::dollar) + 
    labs(
        title = "Revenue by year", 
        subtitle = "Upward trend",
        x = "", 
        y="Revenue"
        ) 
    
# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
    
    #selecting concerned columns and add a year
    select(order_date, total_price, category2) %>%
    mutate(year = year(order_date)) %>%
    glimpse() %>%
    
    #Group_by and summarize year and category2
    #function summarize allows to create a new dataframe function 
    #of the regrouping that you made with the group_by function
    #when you use the group_by function, you need to ungroup() at the end
    group_by(year, category2) %>% glimpse() %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    #Format $ text
    mutate(sales_text = scales::dollar(sales))

sales_by_year_cat_2_tbl


# Step 2 - Visualize

sales_by_year_cat_2_tbl %>%
    
    # set up x, y, fill --> empty figure, just the repere
    ggplot(aes(x = year, y = sales, fill = category2)) + 
    
    # Geometrics --> create the column/bar on the plot link to the geometry
    # make the line trend on the plot
    geom_col() + 
    geom_smooth(method = "lm", se = FALSE) +
    
    # Facet 
    #the function facet_wrap split plots into multiple plots by a
    #categorical feature -- facet_wrap with scales = "free_y" to 
    #show trend. Use without scales argument to show magnitude
    facet_wrap(~ category2, ncol = 3) +
    
    # Formatting 
    theme_tq() + 
    scale_fill_tq() + 
    scale_y_continuous(labels = scales::dollar) + 
    labs(
        title = "Revenue by year and Category 2", 
        subtitle = "Each product category has an upward trend", 
        x = "", 
        y = "Revenue", 
        fill = "product Secondary Category"
    )


# 7.0 Writing Files ----

fs::dir_create("00_data/bike_sales/data_wrangled_student")


# 7.1 Excel ----

bike_orderlines_wrangled_tbl %>%
    write_xlsx("00_data/bike_sales/data_ wrangled_student/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>%
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----
  #Format specific to R 
bike_orderlines_wrangled_tbl %>%
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")