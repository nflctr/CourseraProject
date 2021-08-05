
# R PROGRAMMING : Membuat FIR Low Pass
# diketahui n = 11 dan wc = pi/8

# Menentukan bobot filter
wc <- pi/8
n <- 0:10
h <- (sin((wc)*(n-5)))/(pi*(n-5))
h[6] <- 0.125
h
plot(h)

# Menentukan Hamming Window (w)
w <- 0.54-0.46*cos(2*pi*n/10)
w
plot(w)

# Menentukan tanggapan impuls (hd)
hd <- h*w
hd
plot(hd)

# Simulasi Tanggapan Frekuensi
freqz(hd)


