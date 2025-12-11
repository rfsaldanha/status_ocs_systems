# Packages
suppressMessages({
  library(cli)
  library(lubridate)
  library(glue)
  library(httr2)
  library(ntfy)
})

# Setup
ntfy_topic <- "ocs_status_systems"

# Check function (by Google Gemini)
url_exists_httr2 <- function(url) {
  # 1. Create a request object
  req <- request(url) |>
    # 2. Use the HEAD method for efficiency (only headers are retrieved)
    req_method("HEAD") |>
    # 3. Suppress automatic error throwing for 4xx/5xx status codes
    req_error(is_error = function(resp) FALSE) |>
    # 4. Set a timeout for robustness against slow servers
    req_timeout(240)

  # 5. Perform the request
  resp <- tryCatch(
    {
      req_perform(req)
    },
    error = function(e) {
      # If a network error occurs (e.g., DNS failure, connection timeout), return FALSE
      return(NULL)
    }
  )

  # 6. Check the status code if a response was received
  if (is.null(resp)) {
    return(FALSE)
  } else {
    status_code <- resp_status(resp)
    # Check if the status is a success code (200-299)
    return(status_code >= 200 && status_code < 300)
  }
}

# Systems urls
systems_list <- list(
  list(
    name = "Site",
    url = "https://climaesaude.icict.fiocruz.br"
  ),
  list(
    name = "Mapas",
    url = "https://mapas.climaesaude.icict.fiocruz.br"
  ),
  list(
    name = "MonitorAr Saúde",
    url = "https://shiny.icict.fiocruz.br/monitorarsaude"
  ),
  list(
    name = "FluxSUS",
    url = "https://shiny.icict.fiocruz.br/fluxsus"
  ),
  list(
    name = "Seca e Saúde",
    url = "https://shiny.icict.fiocruz.br/sentseca2"
  ),
  list(
    name = "Malária Transfronteiriça",
    url = "https://shiny.icict.fiocruz.br/publicirdmalaria"
  ),
  list(
    name = "MalariaScan",
    url = "https://www.malariascanapp-demo.online"
  ),
  list(
    name = "Desastres Climáticos",
    url = "https://mapas.climaesaude.icict.fiocruz.br/extremos"
  ),
  list(
    name = "Baixo Tocantins Clima e Saúde Local",
    url = "https://shiny.icict.fiocruz.br/sent_micr_harmonize2"
  )
)

for (i in 1:length(systems_list)) {
  name = systems_list[[i]]$name
  url = systems_list[[i]]$url

  datasus_ftp_connection <- url_exists_httr2(url = url)

  if (datasus_ftp_connection) {
    ntfy_send(
      message = glue("OCS '{name}' system is up and reachable."),
      tags = tags$white_check_mark,
      topic = ntfy_topic
    )
  } else {
    ntfy_send(
      message = glue("OCS '{name}' system is not reachable."),
      tags = tags$rotating_light,
      topic = ntfy_topic
    )
  }
}
