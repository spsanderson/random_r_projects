# Define the function to get GitHub profile information
get_github_profile_info <- function(username) {
  # Define the URLs for the GitHub API requests
  user_url <- paste0("https://api.github.com/users/", username)
  repos_url <- paste0("https://api.github.com/users/", username, "/repos")
  events_url <- paste0("https://api.github.com/users/", username, "/events")
  
  # Make the GET request to get user information
  user_response <- httr::GET(user_url)
  user_data <- jsonlite::fromJSON(httr::content(user_response, "text"))
  
  # Extract the required information from the user data
  num_repos <- user_data$public_repos
  num_followers <- user_data$followers
  
  # Make the GET request to get repositories information
  repos_response <- httr::GET(repos_url)
  repos_data <- jsonlite::fromJSON(httr::content(repos_response, "text"))
  
  # Calculate the total number of stars for all repositories
  num_stars <- sum(repos_data$stargazers_count)
  
  # Make the GET request to get user events (for contributions)
  events_response <- httr::GET(events_url)
  events_data <- jsonlite::fromJSON(httr::content(events_response, "text"))
  
  # Calculate the number of contributions (events)
  num_contributions <- length(events_data)
  
  # Print the retrieved information
  cat(paste0("GitHub Profile Information for ", username, ":\n",
             "Total number of repositories: ", num_repos, "\n",
             "Total number of followers: ", num_followers, "\n",
             "Total number of stars: ", num_stars, "\n",
             "Total number of contributions: ", num_contributions, "\n"))
}

# Example of calling the function
get_github_profile_info("spsanderson")
