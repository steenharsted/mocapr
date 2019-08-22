#Document mocapr_data
#' @title Motion Capture Data of 6 Different Movements
#' @description `mocapr_data` contains motion catpure data of 6 movements.The format of the data is wide
#'  and contains frame by frame joint angles and global joint center positions.
#'  Each joint is typically represented by 6 columns (3 angles and 3 positions).
#'  All joint related variables are abbreviated according to their side (L|R),
#'  joint(A|K|H|S|E|W), and angle/position.\cr
#'  Please see GitHub README.md for more information.
#' @format A \code{tibble}
#' \describe{
#'   \item{movement_nr}{A variable for easy reference of the 6 movements in the dataset}
#'   \item{movement_description}{A short description of the movement being performed}
#'   \item{frame}{Frame number}
#'   \item{time_seconds}{Passed time in seconds from begining of the recording}
#'   \item{Please_see_above_for_the_remaining_68_columns}{The data set contains 68 columns more.\cr Please read the abbreviation guide above
#'   or the GitHub [README.md](https://github.com/steenharsted/mocapr) for more information.}
#'   }
#' @name mocapr_data
#' @docType data
#' @author Steen Harsted \email{steenharsted@gmail.com}
#' @references See video footage of the movements [here](https://www.youtube.com/playlist?list=PLMjrjny4Ymmd1nSGHU0A6dWfEWjBxc-VQ).\cr
#' Thanks to Archi Montañez for allowing the data of him danceing Capoeira to be included in this package.\cr
#' The remaining movements are recordings of the package author.
#' @keywords data
"mocapr_data"
