

library(ggplot2)
g = matrix(rep(0, 360*10), nrow = 360, ncol = 10, byrow = T)
for (i in 1:360){
  g[i,1] = cos(i*pi/180);
  g[i,2] = sin(i*pi/180);
  g[i,3] = 0.5*cos(i*pi/180)
  g[i,4] = 0.5*sin(i*pi/180) + 0.5
  g[i,5] = 0.5*sin(i*pi/180) - 0.5
  g[i,6] = 0.15*cos(i*pi/180)
  g[i,7] = 0.15*sin(i*pi/180) + 0.5
  g[i,8] = 0.15*sin(i*pi/180) - 0.5
}
for (i in 90:270){
  g[i,9] = cos(i*pi/180);
  g[i,10] = sin(i*pi/180);
}
g1 = as.data.frame(g)
ggplot(data = g1) + 
  geom_polygon(aes(x = V1, y = V2), fill = "black") + 
  geom_polygon(aes(x = V9, y = V10), fill = "white") +
  geom_polygon(aes(x = V3, y = V4), fill = "white") +
  geom_polygon(aes(x = V3, y = V5), fill = "black") +
  geom_polygon(aes(x = V6, y = V7), fill = "black") +
  geom_polygon(aes(x = V6, y = V8), fill = "white")

# polygon() -----
YinYang <- function (ang) {
  angle_yin = seq(-pi/2 - ang,pi/2 - ang,pi/180)
  angle_yang = seq(-pi/2 - ang - pi,pi/2 - ang - pi,pi/180)
  angle2 = seq(0,2*pi,pi/180)
  x_yin = c(cos(angle_yin),0.5*cos(rev(angle_yin)) + 0.5*sin(ang),0.5*cos(angle_yin - pi) - 0.5*sin(ang), NA, 0.15*cos(angle2) - 0.5*sin(ang) )
  y_yin = c(sin(angle_yin),0.5*sin(rev(angle_yin)) + 0.5*cos(ang),0.5*sin(angle_yin - pi) - 0.5*cos(ang), NA, 0.15*sin(angle2) - 0.5*cos(ang) )
  x_yang = c(cos(angle_yang),0.5*cos(rev(angle_yang)) + 0.5*sin(ang - pi),0.5*cos(angle_yang - pi) - 0.5*sin(ang - pi), NA, 0.15*cos(angle2) - 0.5*sin(ang - pi) )
  y_yang = c(sin(angle_yang),0.5*sin(rev(angle_yang)) + 0.5*cos(ang - pi),0.5*sin(angle_yang - pi) - 0.5*cos(ang - pi), NA, 0.15*sin(angle2) - 0.5*cos(ang - pi) )
  polygon(c(x_yin, NA, x_yang), c(y_yin, NA, y_yang), col=c("black", "white", "white", "black"))
}
plot(-1:1, -1:1, type="n")
YinYang(0)
# animation ----
library(animation)
oopt = ani.options(interval = 0.2, nmax = 20)

for (i in 0:ani.options("nmax")){
  plot(-1:1, -1:1, type="n")
  YinYang(i*pi/10)
  ani.pause()  ## pause for a while ('interval')
}


YinYang <- function (ang) {
  angle_yin = seq(-pi/2 - ang,pi/2 - ang,pi/180)
  angle_yang = seq(-pi/2 - ang - pi,pi/2 - ang - pi,pi/180)
  angle2 = seq(0,2*pi,pi/180)
  x_yin = c(cos(angle_yin),0.5*cos(rev(angle_yin)) + 0.5*sin(ang),0.5*cos(angle_yin - pi) - 0.5*sin(ang), NA, 0.15*cos(angle2) - 0.5*sin(ang) )
  y_yin = c(sin(angle_yin),0.5*sin(rev(angle_yin)) + 0.5*cos(ang),0.5*sin(angle_yin - pi) - 0.5*cos(ang), NA, 0.15*sin(angle2) - 0.5*cos(ang) )
  x_yang = c(cos(angle_yang),0.5*cos(rev(angle_yang)) + 0.5*sin(ang - pi),0.5*cos(angle_yang - pi) - 0.5*sin(ang - pi), NA, 0.15*cos(angle2) - 0.5*sin(ang - pi) )
  y_yang = c(sin(angle_yang),0.5*sin(rev(angle_yang)) + 0.5*cos(ang - pi),0.5*sin(angle_yang - pi) - 0.5*cos(ang - pi), NA, 0.15*sin(angle2) - 0.5*cos(ang - pi) )
  polygon(c(x_yin, NA, x_yang), c(y_yin, NA, y_yang), col=c("black", "white", "white", "black"))
}

# save as gif
ani.options(convert = shQuote('D:/Software/ImageMagick-6.8.1-Q16/convert.exe'))
saveGIF({
  ani.options(interval = 0.2, nmax = 20)
  for (i in 0:ani.options("nmax")){
    plot(-1:1, -1:1, type="n")
    YinYang(i*pi/10)
    ani.pause()  ## pause for a while ('interval')
  }
})


