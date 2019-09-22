\name{fire_data}
\alias{fire_data}
\docType{data}
\title{Fire Emission Data
%%   ~~ data name/kind ... ~~
}
\description{This data is from DOI and USFS and is used by the histogram function.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("fire_data")}
\format{
  A data frame with 6435 observations on the following 56 variables.
  \describe{
    \item{\code{unique_ID}}{a numeric vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{Date}}{a factor with 1731 levels}
    \item{\code{Year}}{a numeric vector}
    \item{\code{State}}{a factor with levels \code{AZ} \code{CA} \code{CO} \code{ID} \code{MT} \code{NM} \code{NV} \code{OR} \code{UT} \code{WA} \code{WY}}
    \item{\code{Source}}{a factor with levels \code{DOI} \code{USFS}}
    \item{\code{CO}}{a numeric vector}
    \item{\code{CO_dist}}{a numeric vector}
    \item{\code{CO_nobs}}{a numeric vector}
    \item{\code{NO2}}{a numeric vector}
    \item{\code{NO2_dist}}{a numeric vector}
    \item{\code{NO2_nobs}}{a numeric vector}
    \item{\code{O3}}{a numeric vector}
    \item{\code{O3_dist}}{a numeric vector}
    \item{\code{O3_nobs}}{a numeric vector}
    \item{\code{SO2}}{a numeric vector}
    \item{\code{SO2_dist}}{a numeric vector}
    \item{\code{SO2_nobs}}{a numeric vector}
    \item{\code{PM2.5}}{a numeric vector}
    \item{\code{PM2.5_dist}}{a numeric vector}
    \item{\code{PM2.5_daydiff}}{a numeric vector}
    \item{\code{PM10}}{a numeric vector}
    \item{\code{PM10_dist}}{a numeric vector}
    \item{\code{PM10_daydiff}}{a numeric vector}
    \item{\code{Lead}}{a numeric vector}
    \item{\code{Lead_dist}}{a numeric vector}
    \item{\code{Lead_daydiff}}{a numeric vector}
    \item{\code{wind}}{a numeric vector}
    \item{\code{wind_dist}}{a numeric vector}
    \item{\code{tavg}}{a numeric vector}
    \item{\code{tavg_dist}}{a numeric vector}
    \item{\code{tmax}}{a numeric vector}
    \item{\code{tmax_dist}}{a numeric vector}
    \item{\code{tmin}}{a numeric vector}
    \item{\code{tmin_dist}}{a numeric vector}
    \item{\code{precip}}{a numeric vector}
    \item{\code{precip_dist}}{a numeric vector}
    \item{\code{PDSI}}{a numeric vector}
    \item{\code{PM2.5_exc_val}}{a numeric vector}
    \item{\code{PM2.5_exc_dist}}{a numeric vector}
    \item{\code{PM2.5_exc_daydiff}}{a numeric vector}
    \item{\code{PM10_exc_val}}{a numeric vector}
    \item{\code{PM10_exc_dist}}{a numeric vector}
    \item{\code{PM10_exc_daydiff}}{a numeric vector}
    \item{\code{Lead_exc_val}}{a numeric vector}
    \item{\code{Lead_exc_dist}}{a numeric vector}
    \item{\code{Lead_exc_daydiff}}{a numeric vector}
    \item{\code{CO_exc_val}}{a logical vector}
    \item{\code{CO_exc_dist}}{a logical vector}
    \item{\code{NO2_exc_val}}{a numeric vector}
    \item{\code{NO2_exc_dist}}{a numeric vector}
    \item{\code{O3_exc_val}}{a numeric vector}
    \item{\code{O3_exc_dist}}{a numeric vector}
    \item{\code{SO2_exc_val}}{a numeric vector}
    \item{\code{SO2_exc_dist}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fire_data)
## maybe str(fire_data) ; plot(fire_data) ...
}
\keyword{datasets}
