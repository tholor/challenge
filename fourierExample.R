library(pracma)
#example fourier transformation from: http://www.mathworks.de/de/help/matlab/ref/fft.html?refresh=true

#create signal with two sin curves and a random noise
Fs = 1000;                   # % Sampling frequency
T = 1/Fs;                    # % Sample time
L = 1000;                    # % Length of signal
t = c(0:(L-1))*T               #% Time vector
#Sum of a 50 Hz sinusoid and a 120 Hz sinusoid; amplitudes of 0.7 and 1
x = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t); 
y = x + 2*rnorm(length(t));     #% Sinusoids plus some gaussian noise
plot(Fs*t[1:50],y[1:50], type = "l")
title('Signal Corrupted with Zero-Mean Random Noise')
xlabel('time (milliseconds)')



#apply fourier transformation to detect the original sinus signals
#NFFT etc for scaling and extracting only the positive values from fft
NFFT = 2^nextpow2(L); #Next power of 2 from length of y

Y = fft(y)/L;
#take only one half of the measures and scale them
Y_pos = 2*abs(Y[1:(NFFT/2+1)])
f = (Fs/2)*seq(0,1,length = NFFT/2+1)

#Plot single-sided amplitude spectrum.
plot(f,Y_pos,type="l") 
title('Single-Sided Amplitude Spectrum of y(t)')
#get frequencies with high amplitudes
which(Y_pos>0.4)
