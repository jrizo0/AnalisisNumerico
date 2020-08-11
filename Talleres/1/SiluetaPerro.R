#####################
# Silueta del Perro #
#####################

x = c(00.50, 01.01, 05.85, 07.46, 11.28, 15.20, 18.46, 21.25, 24.15, 25.80, 28.00, 30.80, 30.81)
y = c(02.40, 02.95, 03.86, 05.41, 07.45, 06.30, 04.49, 07.15, 07.05, 05.80, 05.85, 04.50, 03.00)


plot(x,y,main = "Interpolación")
vx1 = c(x[1:4])
vy1 = c(y[1:4])
splines = splinefun(vx1,vy1, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx1[1], to = vx1[length(vx1)])
vx2 = c(x[4:7])
vy2 = c(y[4:7])
splines = splinefun(vx2,vy2, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx2[1], to = vx2[length(vx2)])
vx3 = c(x[7:12])
vy3 = c(y[7:12])
splines = splinefun(vx3,vy3, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx3[1], to = vx3[length(vx3)])
vx4 = c(x[12:13])
vy4 = c(y[12:13])
splines = splinefun(vx4,vy4, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx4[1], to = vx4[length(vx4)])
vx5 = c(x[13:18])
vy5 = c(y[13:18])
splines = splinefun(vx5,vy5, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx5[1], to = vx5[length(vx5)])
