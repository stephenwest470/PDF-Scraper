# Load necessary libraries
library(rvest)
library(pdftools)

# Set base URL and search term
base_url <- 'https://sitereports.nabunken.go.jp/ja/search/p/' #Currently set to: sitereports.nabunken.go.jp
search_term <- "" #Insert search term here

# Initialize skip list and page number
skip_pdf <- character()  # Initialize skip list
page <- 1

# Attempt to read previous state from save.txt
if (file.exists("save.txt")) {
  saved_data <- readLines("save.txt")
  page <- as.numeric(saved_data[1])  # Read saved page number
  skip_pdf <- saved_data[-1]           # Read skipped PDF names
} else {
  # Create a new timestamped directory for output files
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H.%M")
  dir.create(timestamp)
}

# Begin PDF search
repeat {
  current_url <- paste0(base_url, page, "?all=", URLencode(search_term))
  all_links <- read_html(current_url) %>% html_nodes("a") %>% html_attr("href")
  pdf_links <- url_absolute(all_links, base = current_url)[grepl("\\.pdf$", all_links)]
  
  # Stop the loop if no PDF links are found
  if (length(pdf_links) == 0) {
    message("No more PDF links found.")
    break
  }
  
  # Process each PDF link
  for (pdf_url in pdf_links) {
    pdf_name <- URLdecode(basename(pdf_url))  # Get PDF name
    
    # Skip already processed PDFs
    if (pdf_name %in% skip_pdf) {
      message("Skipping PDF: ", pdf_name)
      next
    }
    
    # Load the PDF text
    pdf_text <- suppressMessages(tryCatch(pdftools::pdf_text(pdf_url), error = function(e) NULL))
    
    # Check if PDF text is valid
    if (is.null(pdf_text) || all(nchar(pdf_text) == 0)) {
      message("Ignoring PDF with no searchable text: ", pdf_name)
      skip_pdf <- c(skip_pdf, pdf_name)  # Add to skip list
      next
    }
    
    # Search for the term in each page of the PDF
    found <- FALSE
    for (i in seq_along(pdf_text)) {
      if (grepl(search_term, pdf_text[[i]], ignore.case = TRUE)) {
        # Save the found page as a new PDF
        pdf_page_name <- paste0(gsub(".pdf$", "", pdf_name), " (", i, ").pdf")
        output_path <- file.path(timestamp, pdf_page_name)
        
        tryCatch({
          pdf_subset(pdf_url, pages = i, output = output_path)  # Save page as PDF
          message("Found in PDF: ", pdf_name, " | Page: ", i)
          found <- TRUE  # Update found flag
        }, error = function(e) {
          message("Error converting PDF: ", pdf_name, " | Page: ", i, " - ", e$message)
        })
      }
    }
    
    # Add PDF to skip list if it was processed
    if (found) {
      skip_pdf <- c(skip_pdf, pdf_name)
    }
  }
  
  # Write current page and skip list to save.txt
  writeLines(c(as.character(page), skip_pdf), "save.txt")
  
  # Increment the page for the next loop
  page <- page + 1
}

# Delete save.txt if the loop finished successfully
if (file.exists("save.txt")) {
  file.remove("save.txt")
}

# Completion message
message("PDF search complete. All files saved in directory: ", timestamp)
