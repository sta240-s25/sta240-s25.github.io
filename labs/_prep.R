r1 = .5
r2 = 2
s1 = 1
s2 = 1
v1 = s1^2
v2 = s2^2
r = 0

rp = seq(m2, m1, length.out = 500)
w = seq(0, 1, length.out = 500)#(rp - r2) / (r1 - r2)

Er = w * r1 + (1 - w) * r2
Vr = w^2 * v1 + (1-w)^2 * v2 + 2 * w * (1-w) * r * s1 * s2
Sr = sqrt(Vr)

plot(Sr, Er, type = "l", xlim = c(0, 2), ylim = c(0, 10))

#https://www.jstor.org/stable/2975974?seq=15