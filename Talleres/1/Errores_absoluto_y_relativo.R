###############################
# Errores absoluto y relativo #
###############################

v = 4 ;    #Valor de la velocidad 
t = 5 ;    #Valor del tiempo
Ev =0.1;   #Error de la velocidad
Et =0.1;   #Error del tiempo
d = v*t ;
ErrorA = (v*Et) + (t*Ev);
ErrorR = ((Ev/v)+(Et/t))*100;
drestada = d - ErrorA ; #Distancia restada por el error absoluto
dsumada = d+ ErrorA;    #Distancia sumada por el error absoluto
cat ("Este es el valor del error absoluto",ErrorA);
cat ("Este es el valor del error relativo",ErrorR,"%");
cat ("La distancia va de", drestado ,"<=",d,"<=", dsumado);
