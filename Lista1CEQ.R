#Dados da tabela (média, amplitude, desvio padrão)
x1 = c(413, 419, 411, 421); xb1 = mean(x1); amp1 = diff(range(x1)); sd1 = sd(x1); 
x2 = c(395, 413, 413, 417); xb2 = mean(x2); amp2 = diff(range(x2)); sd2 = sd(x2);
x3 = c(425, 407, 423, 417); xb3 = mean(x3); amp3 = diff(range(x3)); sd3 = sd(x3);
x4 = c(403, 399, 401, 397); xb4 = mean(x4); amp4 = diff(range(x4)); sd4 = sd(x4);
x5 = c(419, 409, 429, 409); xb5 = mean(x5); amp5 = diff(range(x5)); sd5 = sd(x5);
x6 = c(407, 407, 407, 409); xb6 = mean(x6); amp6 = diff(range(x6)); sd6 = sd(x6); 
x7 = c(397, 401, 407, 393); xb7 = mean(x7); amp7 = diff(range(x7)); sd7 = sd(x7);
x8 = c(405, 417, 407, 407); xb8 = mean(x8); amp8 = diff(range(x8)); sd8 = sd(x8);
x9 = c(411, 415, 409, 405); xb9 = mean(x9); amp9 = diff(range(x9)); sd9 = sd(x9);
x10 = c(409, 413, 405, 409); xb10 = mean(x10); amp10 = diff(range(x10)); sd10 = sd(x10);
x11 = c(407, 403, 409, 405); xb11 = mean(x11); amp11 = diff(range(x11)); sd11 = sd(x11);
x12 = c(407, 411, 415, 415); xb12 = mean(x12); amp12 = diff(range(x12)); sd12 = sd(x12);
x13 = c(403, 409, 407, 411); xb13 = mean(x13); amp13 = diff(range(x13)); sd13 = sd(x13); 
x14 = c(409, 405, 393, 397); xb14 = mean(x14); amp14 = diff(range(x14)); sd14 = sd(x14);
x15 = c(419, 415, 401, 395); xb15 = mean(x15); amp15 = diff(range(x15)); sd15 = sd(x15);
x16 = c(407, 399, 413, 403); xb16 = mean(x16); amp16 = diff(range(x16)); sd16 = sd(x16);
x17 = c(409, 407, 410, 411); xb17 = mean(x17); amp17 = diff(range(x17)); sd17 = sd(x17);
x18 = c(427, 417, 409, 407); xb18 = mean(x18); amp18 = diff(range(x18)); sd18 = sd(x18);
x19 = c(411, 415, 419, 425); xb19 = mean(x19); amp19 = diff(range(x19)); sd19 = sd(x19);
x20 = c(405, 417, 409, 419); xb20 = mean(x20); amp20 = diff(range(x20)); sd20 = sd(x20);
x21 = c(397, 407, 417, 405); xb21 = mean(x21); amp21 = diff(range(x21)); sd21 = sd(x21);
x22 = c(413, 413, 405, 395); xb22 = mean(x22); amp22 = diff(range(x22)); sd22 = sd(x22);
x23 = c(409, 405, 409, 413); xb23 = mean(x23); amp23 = diff(range(x23)); sd23 = sd(x23);
x24 = c(403, 399, 399, 405); xb24 = mean(x24); amp24 = diff(range(x24)); sd24 = sd(x24);
x25 = c(395, 389, 401, 393); xb25 = mean(x25); amp25 = diff(range(x25)); sd25 = sd(x25);

#Combinação da média, amplitude e desvio padrão
xb = c(xb1, xb2, xb3, xb4, xb5, xb6, xb7, xb8, xb9, xb10, xb11, xb12, 
      xb13, xb14, xb15, xb16, xb17, xb18, xb19, xb20, xb21, xb22, xb23, xb24, xb25)
amp = c(amp1, amp2, amp3, amp4, amp5, amp6, amp7, amp8, amp9, amp10, amp11, amp12, 
        amp13, amp14, amp15, amp16, amp17, amp18, amp19, amp20, amp21, amp22, amp23,
        amp24, amp25)
sd = c(sd1, sd2, sd3, sd4, sd5, sd6, sd7, sd8, sd9, sd10, sd11, sd12, sd13, sd14, 
       sd15, sd16, sd17, sd18, sd19, sd20, sd21, sd22, sd23, sd24, sd25)

#Cálculo do x-bb e os limites 
xbb = mean(xb)
lscx = xbb + 0.729*rb;lscx
lcx = xbb;lcx
licx = xbb - 0.729*rb;licx

#Cálculo do R-b e os limites
rb = mean(amp)
lscr = rb*2.282;lscr
lcr = rb;lcr
licr = 0

#Cálculo do S-b e os limites
sb = mean(sd)
lscs = sb*2.266;lscs
lcs = sb;lcs
lics = 0

#Gráfico de x-b
plot(xb, xlab = "amostra", ylab = "média", type='o')
abline(h=lscx, col="red")
abline(h=lcx, col="red")
abline(h=licx, col="red")

#Gráfico de R-b
plot(amp, xlab = "amostra", ylab = "amplitude", type='o')
abline(h=lscr, col="blue")
abline(h=lcr, col="blue")
abline(h=licr, col="blue")

#Gráfico de S-b
plot(sd, xlab = "amostra", ylab = "desvio padrão", type='o')
abline(h=lscs, col="green")
abline(h=lcs, col="green")
abline(h=lics, col="green")

#Nova média/amplitude/desvio padrão após retirada dos elementos acima/abaixo dos limites
xbal = c(xb1, xb2, xb4, xb5, xb6, xb7, xb8, xb9, xb10, xb11, xb12, 
         xb13, xb14, xb15, xb16, xb17, xb18, xb20, xb21, xb22, xb23, xb24)
ampal = c(amp1, amp2, amp4, amp5, amp6, amp7, amp8, amp9, amp10, amp11, amp12, 
          amp13, amp14, amp15, amp16, amp17, amp18, amp20, amp21, amp22, amp23,
          amp24)
sdal = c(sd1, sd2, sd4, sd5, sd6, sd7, sd8, sd9, sd10, sd11, sd12, sd13, sd14, 
         sd15, sd16, sd17, sd18, sd20, sd21, sd22, sd23, sd24)

#Cálculo do novo x-bb e dos limites
xbbal = mean(xbal)
lscxal = xbbal + 0.729*rbal;lscxal
lcxal = xbbal;lcxal
licxal = xbbal - 0.729*rbal;licxal

#Novo gráfico de x-b
plot(xbal, xlab = "amostra", ylab = "média", type='o')
abline(h=lscxal, col="red")
abline(h=lcxal, col="red")
abline(h=licxal, col="red")

#Cálculo do novo R-b e dos limites
rbal = mean(ampal)
lscral = rbal*2.282;lscral
lcral = rbal;lcral
licral = 0

#Novo gráfico de R-b 
plot(ampal, xlab = "amostra", ylab = "amplitude", type='o')
abline(h=lscral, col="blue")
abline(h=lcral, col="blue")
abline(h=licral, col="blue")

#Cálculo do novo S-b e dos limites
sbal = mean(sdal)
lscsal = sbal*2.266;lscsal
lcsal = sbal;lcsal
licsal = 0

#Novo gráfico de S-b
plot(sdal, xlab = "amostra", ylab = "desvio padrão", type='o')
abline(h=lscsal, col="green")
abline(h=lcsal, col="green")
abline(h=licsal, col="green")