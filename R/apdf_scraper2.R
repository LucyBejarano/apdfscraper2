#' A pdf scraper function v2
#'
#' Pdf to csv converter
#
#' This function allows you to scrap one or more pdfs with one or several pages.
#' The scraping method results into a .csv file per each pdf processed.
#' To do so, please follow the following example.
#'
#' Copy step 1 and 2 and replace with your folder locations.
#' Copy step 3 and press ctrl+enter
#
#' Example:
#'
#'
#' # Step 1.
#' pdf_location <-"C:/the name of the folder where is my PDF/"
#'
#' # Step 2.
#' csv_location <-"C:/the name of the folder where is my PDF/"
#'
#' # Step 3.
#' a_pdf_scraper(pdf_location, csv_location)
#'
#' Finally, check the .csv folder to see the pdf converted.
#
#' @export



a_pdf_scraper_2 <- function(pdf_location, csv_location) {



  # load packages

  library(pdftools)
  library(stringr)
  library(stringi)
  library(dplyr)


  # Set working environment ######################################################

  # Set the path where to read the data
  setwd(pdf_location)



  # Load data ####################################################################


  # Extract names of all ".pdf" files ##
  list_file <-
    dir(pattern = "\\.pdf", recursive = FALSE)
  list_file # list of all files


  # Load data
  temp <- lapply(list_file, pdf_text)
  names(temp) <- list_file

  # Tidy all pdfs listed

  # non stop
  error_result <-tryCatch({
    print(list_file[n_pdf])

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  for (n_pdf in seq_along(1:length(temp))) {
    # Data splitting and cleaning ################################################

      data <- temp[[n_pdf]] %>%
      strsplit("\n")

    data <- unlist(data) %>%
      trimws()

    # Extract employee data ######################################################


    # Select the rows that have digits between "/" (we want to look the dates)

    list_rows <- grep(pattern = "[[:digit:]]/[[:digit:]]", x = data)
    employee_info <- data[list_rows]

    # Remove leading and/or trailing white space from character strings.

    employee_info <- trimws(employee_info)


    # Split in columns
    employee_info <-
      str_split_fixed(employee_info, " {3,}", 6) # We need at least 3 spaces in the names and 6 columns

    employee_info <-
      data.frame(employee_info, stringsAsFactors = FALSE)

    # Extract the number of ID from the first column
    employee_info$ID <-
      stri_extract_first_regex(employee_info$X1, "[0-9]+")

    # Set the number ID as first column
    employee_info <- employee_info %>%
      relocate(ID)

    # Leave the CI without the ID
    employee_info <- employee_info %>%
      mutate(X1 = if_else(
        grepl(" ", X1),
        stri_extract_last_regex(employee_info$X1, "[0-9]+"),
        ""
      ))
    # Include name of column
    head_title <-c("ID","CI","Apellidos y Nombres(s)","Nacionalidad","Fecha de Nacimiento","Ocupación que desempeña","Fecha de Ingreso")

    names(employee_info) <- head_title


    ## Extract Gender in a new column (gender is attached to other column)
    employee_info <- employee_info %>%
      mutate(Genero = if_else(
        grepl("^M |^F ", `Ocupación que desempeña`),
        substr(employee_info$`Ocupación que desempeña`, 1, 2),
        ""
      ))

    # Remove gender attached to another column
    employee_info <-
      employee_info %>% mutate(`Ocupación que desempeña` = if_else(
        grepl(pattern = "[[:digit:]]/[[:digit:]]", `Ocupación que desempeña`),
        `Ocupación que desempeña`,
        substr(
          `Ocupación que desempeña`,
          2,
          nchar(`Ocupación que desempeña`)
        )
      ))

    # Set the column gender after "fecha de nacimiento"
    employee_info <- employee_info %>%
      relocate(Genero, .after = `Fecha de Nacimiento`)

    # Filter empty rows
    employee_info <- employee_info %>%
      filter(`Apellidos y Nombres(s)`!=""|Nacionalidad!="")



    # Extract data amounts ############################################################

    # Delete dots and spaces to later convert to numeric
    data_amount_detect <- data %>%
      str_replace_all(pattern = "\\.", repl = "") %>%
      str_replace_all(pattern = " ", repl = "") %>%
      as.numeric()

    # Keep all the columns that are not "na" so they all have numbers
    data_amount <- data[!is.na(data_amount_detect)]

    # Reduces repeated whitespace inside a string
    data_amount <- str_squish(data_amount)

    # Split all numbers considering an space
    data_amount <- str_split_fixed(data_amount, " ", 13)
    data_amount <- data.frame(data_amount, stringsAsFactors = FALSE)

    data_amount <- as.data.frame(data_amount)

    # Add title
    amount_title <-
      c(
        "Días Hábiles Pagados Mes",
        "Hrs/Día Pagadas",
        "Haber Básico",
        "Bono Antiguedad",
        "Total Horas Extras",
        "Otros Bonos",
        "Total Salarios Dominicales",
        "Total Pagado",
        "AFP",
        "RC-IVA",
        "Otros",
        "Total Descuentos",
        "Líquido Pagable"
      )

    names(data_amount) <- amount_title

    # Create one table ###########################################################


    data_final <- cbind(employee_info, data_amount)


    #Save in CSV #################################################################

    # Define name for csv
    name_csv = gsub(
      list_file[n_pdf],
      pattern = ".pdf",
      replacement = "",
      fixed = TRUE
    )

    # Save .csv
    write.csv2(
      data_final,
      file = paste0(csv_location, name_csv, ".csv") ,
      row.names = FALSE
    )

  }


  print(error_result)
  print("Scrapping completed")
}
