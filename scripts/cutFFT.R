# Function which return FFT until the frequency param.
# This FFT must be a matrix with two columns, where the first column is the frequency
# and the second column is the amplitude.
cutFFT <- function(fft, frequency)
{
  for (freq in 1:nrow(fft))
  {
    if (fft[freq,1] >= frequency)
    {
      return(fft[1:freq,])
    }
  }
}