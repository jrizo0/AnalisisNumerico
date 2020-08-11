################################################
# Aproximación de e^x  ,  suponiendo que x=0.5 #
################################################

n=0;
Experimental=0;
tolerancia=0.00000001;
teorico=1.648721271;

#Ciclo hasta que el error sea valido
repeat{
  Experimental=Experimental+(0.5^n)/factorial(n);
  n=n+1;
  if((abs(Experimental-teorico))<tolerancia) break;
}
cat ("El valor de e^0.5 teorico es ",teorico);
cat ("El valor de e^0.5 experimental es ",Experimental);
