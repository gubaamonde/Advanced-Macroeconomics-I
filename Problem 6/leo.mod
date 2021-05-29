var y, c, k, i, l, a, r, w;
varexo e;

parameters beta, rlo, alpla, delta,b;

beta=0.9;
rlo=0.95;
alpla=0.4;
delta=0.025;
b=1;

model; 
1/exp(c)=beta*(1/exp(c(+1)))*(1-delta+exp(r(+1)));
b/(1-exp(l))=exp(w)/exp(c);
exp(y)=exp(a)*(exp(k)^alpla)*(exp(l)^(1-alpla));
exp(y)=exp(c)+exp(i);
exp(k)=(1-delta)*exp(k(-1))+exp(i);
exp(w)=(1-alpla)*exp(a)*((exp(k)/exp(l))^alpla);
exp(r)=alpla*exp(a)*((exp(l)/exp(k))^(1-alpla));
a=rlo*a(-1)+e;
end;

initval;
y=log(1);
c=log(0.9);
k=log(4);
i=log(0.1);
l=log(0.4);
a=0;
r=log(0.13);
w=log(1.5);
end;	

shocks;
var e=0.01;
end;

steady;

stoch_simul(order=1,irf=100);

