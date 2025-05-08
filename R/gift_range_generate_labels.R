#' Helper function to dynamically generate labels (Internal)
#' @keywords internal


generate_gift_range_labels <- function(upper_limits, lower_bound, prefixes = NULL) {
  if (is.null(upper_limits) || length(upper_limits) == 0) {
    stop("upper_limits cannot be empty.")
  }

  breaks <- c(lower_bound, upper_limits)
  num_labels <- length(breaks) - 1
  labels <- character(num_labels)

  # Ensure prefixes vector is of correct length or type if provided
  if (!is.null(prefixes)) {
    if (!is.character(prefixes)) {
        stop("label_prefixes must be a character vector or NULL.")
    }
    if (length(prefixes) < num_labels) {
      warning("Number of prefixes is less than number of labels. Recycling or truncating.")
      prefixes <- rep(prefixes, length.out = num_labels)
    } else if (length(prefixes) > num_labels) {
      prefixes <- prefixes[1:num_labels]
    }
    prefixes <- paste0(prefixes, ": ")
  } else {
    prefixes <- rep("", num_labels) # Empty string if no prefixes
  }

  format_val <- function(val, force_cents = FALSE) {
    if (is.infinite(val) && val > 0) return("Inf")
    if (is.infinite(val) && val < 0) return("-Inf")
    
    digits_to_show <- if (force_cents || (val %% 1 != 0 && round((val %% 1) * 100) != 0)) 2 else 0
    
    if (!force_cents && abs((val %% 1) - 0.99) < 1e-9 && val %% 1 != 0) { # Check for numbers very close to .99
        val <- floor(val) + 0.99
        digits_to_show <- 2
    }
     # Format with comma for thousands, respecting specified digits
    formatted_num <- formatC(val, format="f", big.mark=",", digits=digits_to_show)
    return(formatted_num)
  }


  for (i in 1:num_labels) {
    lower <- breaks[i]
    upper <- breaks[i+1] # This is the actual break point for cut()

    if (i == 1 && lower == -Inf) { # First bin starting from -Inf
      labels[i] <- paste0(prefixes[i], "< $", format_val(upper))
    } else if (i == 1 && (lower == 0 && upper == upper_limits[1])) { # First bin, often like "< $X"
      labels[i] <- paste0(prefixes[i], "< $", format_val(upper))
    } else if (is.infinite(upper)) { # Last bin
      labels[i] <- paste0(prefixes[i], "$", format_val(lower), " and up")
    } else { # Intermediate bins
      label_upper_val <- upper - 0.01
      labels[i] <- paste0(prefixes[i], "$", format_val(lower), " - $", format_val(label_upper_val, force_cents = TRUE))
    }
  }
  return(labels)
}