#' @import magick
NULL
#'
#'
#' Greyscale filter for RGB vector
#'
#' @param rgb_vector A raw vector of 3 RGB values
#'
#' @return Function returns numeric vector of greyscale values
#' @examples greyscale_filter(as.raw(c(210, 55, 22)))
#' @export
  greyscale_filter <- function(rgb_vector) { #function takes RGB vector with three values

      R <- as.integer(rgb_vector[1]) #first value is R, transformed to decimal

      G <- as.integer(rgb_vector[2]) #second values is G, tansformed to decimal

      B <- as.integer(rgb_vector[3]) #third value is B, transformed to decimal

      grey_val <- round(R * 0.299 + G * 0.587 + B * 0.114) #formula to transform color to grey

      return(c(grey_val, grey_val, grey_val)) #function returns vector with three numeric grey values
   }


#' Cutoff filter for RGB vector
#'
#' @param rgb_vector A raw vector of 3 RGB values
#' @param threshold An integer from 0 to 255 representing the grayscale cut-off value.
#'                  Default is set to 127 (approximately 50%).
#'
#' @return A vector with numeric values. If grayscale value is bigger than threshold, returns black (0, 0, 0),
#'         otherwise returns magenta (255, 0, 255).
#' @export
#'
#' @examples cutoff_filter(as.raw(c(200, 100, 150)), threshold = 100) #returns black

  cutoff_filter <- function(rgb_vector, threshold = 127) { #function takes RGB vector with three values, threshold is set as default to 127.

    R <- as.integer(rgb_vector[1]) #first value is R, transformed to decimal

    G <- as.integer(rgb_vector[2]) #second values is G, tansformed to decimal

    B <- as.integer(rgb_vector[3]) #third value is B, transformed to decimal

    grey_val <- round(R * 0.299 + G * 0.587 + B * 0.114) #formula to transform color to grey

    if (grey_val > threshold) { #conditional for grey values more than threshold
      return(c(0, 0, 0)) #returns black color

    } else { #if less
      return(c(255, 0, 255)) #returns magenta colour
    }
  }


#' Immage filter - apply a filter function to each pixel af a bitmap image
#'
#' @param bitmap_name A 3D raw array representing a bitmap image (from image_read()[[1]] subsetting the first element)
#' @param filter_name A function that takes an RGB vector (length 3) and returns a transformed RGB vector
#'
#' @return A 3D raw array representing the new filtered bitmap image
#' @export
#'
  image_filter <- function(bitmap_name, filter_name) {

    dimension <- dim(bitmap_name)  # Dimensions of our bitmap

    new_bitmap <- array(as.raw(0), dim = c(3, dimension [2], dimension [3])) #creating an empty array for a new bitmap

    for (x in 1:dimension [2]) { #looping through all x and y coordinates for each pixel in the picture
      for (y in 1:dimension [3]) {

        rgb_vector <- bitmap_name[, x, y] #extracting RGB values for each pixel to a vector

        new_pixels <- filter_name(rgb_vector) #applying a filter to a raw RGB vector

        new_bitmap[, x, y] <- as.raw(new_pixels) #as filters transform raw to decimal RGB values, here as.raw transforms them back to hexadecimal and populates an empty array for a new bitmap
      }
    }
    return(new_bitmap) #function returns the new bitmap with the a filer applied to pixels
  }



