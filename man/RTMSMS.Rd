\name{RTMSMS}
\alias{RTMSMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RTMSMS
%%  ~~function to do ... ~~
}
\description{This function is used to get RT-MS/MS level
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RTMSMS(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
## The function is currently defined as
RTMSMS <- function(dat){
  # a <- choose.files()
  # dat <- import(a)
  dat = dat %>% mutate(
    RTMSMS = case_when(
      deltaRT <= 2 & Score >= 40 ~ 1,
      deltaRT <= 2 & Score >= 35 ~ 2,
      deltaRT <= 2 & Score >= 30 ~ 3,
      deltaRT >2 & deltaRT <= 4 & Score >= 30 ~3,
      deltaRT > 4~ 4,
      Score < 30 ~ 4,
      TRUE~5
    )
  )
  # dat %>% export(paste0(a,'RT-MSMSLEVEL.csv'))
  return(dat)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
