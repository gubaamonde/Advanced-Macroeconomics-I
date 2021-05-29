var y, c, k, i, h, a, r, w;
varexo e;

parameters beta, rho, alpha, delta, sigma, b;

beta=0.9;
rho=0.95;
alpha=0.4;
delta=0.025;
sigma=1;
b=1;

model; %See Remark 17 in Pfeifer (2013)
exp(c)^(-sigma)=beta*(exp(c(+1))^(-sigma))*(1+delta+exp(r(+1)));
b/(1-exp(h))=exp(w)/exp(c);
exp(y)=exp(a)*(exp(k(-1))^alpha)*(exp(h)^(1-alpha));
exp(y)=exp(c)+exp(i);
exp(k)=(1-delta)*exp(k(-1))+exp(i);
exp(w)=(1-alpha)*exp(a)*((exp(k(-1))/exp(h))^alpha);
exp(r)=alpha*exp(a)*((exp(h)/exp(k(-1)))^(1-alpha));
a=rho*a(-1)+e;
end;

initval;
y=log(1);
c=log(0.9);
k=log(4);
i=log(0.1);
h=log(0.4);
a=0;
r=log(0.13);
w=log(1.5);
end;

shocks;
var e=0.01;
end;

steady;

stoch_simul(order=1,irf=100);

